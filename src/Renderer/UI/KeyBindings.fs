/// Turns a key press into an action, using the table in KeyTypes and the context the user is in.
///
/// This is the only place in Issie that should listen for keys. It replaces five mechanisms that
/// grew independently and never agreed with each other: the Electron menu accelerators, the
/// document.onkeydown reassigned on every render in SheetDisplay, the addEventListener
/// subscription in Renderer, the one React handler in SelectedComponentView, and the arrow-key
/// intercept in Update.
///
/// KeyTypes says what the shortcuts are; this module says what they do and delivers them.
module KeyBindings

open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open ModelType
open ModelHelpers
open DrawModelType
open Optics
open Optics.Operators
open KeyTypes

let isMac = Bridge.isMac

/// The chord table for this platform, built once.
let private platformTable = KeyTypes.table isMac

//-------------------------------------------------------------------------------------------------//
//---------------------------------------CONTEXT---------------------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// Which context the model puts the user in, ignoring DOM focus (which is read separately, at key
/// time, because it changes without the model changing).
///
/// First match wins, so the order is the priority order.
let contextOfModel (m: Model) : KeyContext =
    // The code editor lives inside a popup and both fields are cleared together by ClosePopup, so
    // requiring both is the accurate test rather than a defence against one of them latching.
    if m.CodeEditorState.IsSome && m.PopupViewFunc.IsSome then
        CodeEditor
    // before Popup, since the browser IS a popup and wants arrows and Enter that no other one does
    elif m.ProjectBrowser.IsSome && m.PopupViewFunc.IsSome then
        ProjectBrowser
    elif m.PopupViewFunc.IsSome || m.SpinnerPayload.IsSome || m.PopupDialogData.Progress.IsSome then
        Popup
    elif
        m.RightPaneTabVisible = RightTab.Simulation
        && m.SimSubTabVisible = SimSubTab.WaveSim
        && (getWSModel m).State = WaveSimState.Success
        && m.KeyFocusPane = RightPane
    then
        WaveSim
    else
        match m.Sheet.Action with
        // CurrentAction carries a function in one case, so it cannot be compared with <>
        | SheetT.CurrentAction.Idle ->
            if m.CurrentProj.IsSome then SheetIdle else Global
        | _ -> SheetBusy

/// The context as of the last update.
///
/// A cache of a pure function of the model, not state in its own right: it exists because a DOM
/// event handler has no way to see the model, and preventDefault has to be decided synchronously
/// inside the handler so the answer cannot be fetched by dispatching a message. This is the same
/// reason Update.evilUIState existed, generalised from a three-case hack into the real thing, and
/// it is written from exactly the same place - the top-level update wrapper in Renderer.fs.
let mutable private modelContext: KeyContext = Global

let setContextFromModel (m: Model) = modelContext <- contextOfModel m

/// True when DOM focus is somewhere that must receive the key itself.
///
/// Nothing in Issie sets tabIndex or contentEditable, so the tag name is a complete test and none
/// of the 54 hand-rolled input boxes needs to know this module exists.
let private isTextEntryFocused () =
    match Browser.Dom.document.activeElement with
    | null -> false
    | el ->
        match el.tagName with
        | "INPUT"
        | "TEXTAREA"
        | "SELECT" -> true
        | _ -> false

/// The context a key press should be resolved in.
let currentContext () =
    if isTextEntryFocused () then TextEntry else modelContext

//-------------------------------------------------------------------------------------------------//
//------------------------------------EVENT NORMALISATION------------------------------------------//
//-------------------------------------------------------------------------------------------------//

let private namedKeys =
    [ "ArrowLeft", Names.arrowLeft
      "ArrowRight", Names.arrowRight
      "ArrowUp", Names.arrowUp
      "ArrowDown", Names.arrowDown
      "Escape", Names.escape
      "Enter", Names.enter
      "Tab", Names.tab
      " ", Names.space
      "Delete", Names.delete
      "Backspace", Names.backspace
      "-", Names.minus
      "=", Names.equal
      "+", Names.equal ]
    |> Map.ofList

/// Physical keys, used when ev.key cannot be trusted.
let private namedCodes =
    [ "Minus", Names.minus
      "Equal", Names.equal
      "NumpadSubtract", Names.minus
      "NumpadAdd", Names.equal
      "Space", Names.space ]
    |> Map.ofList

let private isAsciiLetter (c: char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let private isDigit (c: char) = c >= '0' && c <= '9'

/// Resolve a key event to a KeyName.
///
/// ev.key alone will not do: on macOS Option rewrites it, so Cmd+Option+A arrives as "a" with a
/// ring over it. ev.code alone will not do either, being physical and so wrong on any layout that
/// is not QWERTY - Ctrl+A on AZERTY is physically KeyQ. So use ev.key wherever it is meaningful
/// and fall back to the physical key only where it is not.
let keyNameOf (ev: Browser.Types.KeyboardEvent) : KeyName =
    let key = ev.key
    let code: string = ev?code
    match key with
    | k when k <> null && k.Length = 1 && isAsciiLetter k[0] -> KLetter(System.Char.ToUpper k[0])
    | k when k <> null && k.Length = 1 && isDigit k[0] -> KDigit k[0]
    | k when k <> null && Map.containsKey k namedKeys -> KNamed namedKeys[k]
    | k when k <> null && k.Length > 1 && k[0] = 'F' && isDigit k[1] ->
        match System.Int32.TryParse k[1..] with
        | true, n -> KFn n
        | _ -> KNamed k
    | _ ->
        // ev.key was mangled or is a symbol: fall back to the physical key
        match code with
        | c when c <> null && c.StartsWith "Key" && c.Length = 4 -> KLetter c[3]
        | c when c <> null && c.StartsWith "Digit" && c.Length = 6 -> KDigit c[5]
        | c when c <> null && Map.containsKey c namedCodes -> KNamed namedCodes[c]
        | c when c <> null -> KNamed c
        | _ -> KNamed(if key = null then "" else key)

let modsOf (ev: Browser.Types.KeyboardEvent) : Mods =
    { Primary = (if isMac then ev.metaKey else ev.ctrlKey)
      Secondary = (if isMac then ev.ctrlKey else ev.metaKey)
      Alt = ev.altKey
      Shift = ev.shiftKey }

let chordOf (ev: Browser.Types.KeyboardEvent) : Chord =
    { Mods = modsOf ev; Key = keyNameOf ev }

//-------------------------------------------------------------------------------------------------//
//-----------------------------------------ACTIONS-------------------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// What each shortcut does.
///
/// Deliberately has no wildcard case, so adding a ShortcutId to the table without giving it an
/// action is a compile error rather than a shortcut that quietly does nothing.

/// Which of the two zoomable things a zoom key means.
type private ZoomKey =
    | ZoomIn
    | ZoomOut
    | ZoomToFit

/// Whether the waveform viewer is the thing the user is looking at.
///
/// Three facts, all of them about what is on the screen and what was last touched, and none about
/// DOM focus: the viewer keeps the zoom keys while the user is typing a clock cycle into its own
/// input box, which is the case that made the old context-based split fail.
///
/// KeyFocusPane is the "last touched side" - set by a mousedown in either pane - so clicking the
/// schematic hands the keys back even while the viewer is still on screen.
let private waveSimHasTheKeyboard (m: Model) =
    m.RightPaneTabVisible = RightTab.Simulation
    && m.SimSubTabVisible = SimSubTab.WaveSim
    && (getWSModel m).State = WaveSimState.Success
    && m.KeyFocusPane = RightPane

/// Zoom whatever is being looked at. Needs the model, which the dispatcher has no access to, so it
/// goes round through ExecFuncInMessage as the other model-dependent actions here do.
let private zoom (dispatch: Msg -> unit) (key: ZoomKey) =
    let act (m: Model) (d: Msg -> unit) =
        match waveSimHasTheKeyboard m, key with
        | true, ZoomIn -> WaveSimNavigation.changeZoom (getWSModel m) true d
        | true, ZoomOut -> WaveSimNavigation.changeZoom (getWSModel m) false d
        // no "fit" for waveforms - the viewer has no such idea - so Ctrl+0 always fits the diagram
        | true, ZoomToFit
        | false, ZoomToFit -> d (Sheet(SheetT.KeyPress SheetT.KeyboardMsg.CtrlW))
        | false, ZoomIn -> d (Sheet(SheetT.KeyPress SheetT.KeyboardMsg.ZoomIn))
        | false, ZoomOut -> d (Sheet(SheetT.KeyPress SheetT.KeyboardMsg.ZoomOut))
    dispatch <| ExecFuncInMessage(act, dispatch)

/// Put the caret in the Properties pane's Name box, with the existing name selected so that typing
/// replaces it - which is what F2 means everywhere else.
///
/// Run after the render rather than at once: the box exists only when the Properties pane is
/// showing, and the key that asks for it is also what switches the pane to it.
let private focusComponentNameBox (_: Msg -> unit) (model: Model) =
    match Browser.Dom.document.getElementById "labelInputElement" with
    | null -> ()
    | el ->
        el.focus ()
        // select(), so the first character typed replaces the name instead of extending it
        el?select ()
    model

let actionOf (id: ShortcutId) (dispatch: Msg -> unit) : unit =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let keyDispatch (k: SheetT.KeyboardMsg) = sheetDispatch (SheetT.KeyPress k)
    let busWireDispatch (bMsg: BusWireT.Msg) = sheetDispatch (SheetT.Msg.Wire bMsg)
    let symbolDispatch msg = busWireDispatch (BusWireT.Msg.Symbol msg)
    let setTheme theme =
        dispatch <| SetThemeUserData theme
        symbolDispatch (SymbolT.Msg.SetTheme theme)
    let wholeSheet (f: BusWireT.Model -> BusWireT.Model) =
        dispatch <| UpdateModel(fun m -> m |> Optic.map (sheet_ >-> SheetT.wire_) f)
    let stepAppZoom delta =
        Bridge.setZoomLevel (max -9.0 (min 9.0 (Bridge.getZoomLevel () + delta)))
    /// An editing shortcut does nothing on a library component's sheet, which can only be looked
    /// at. Decided inside a message because a key handler has no way to see the model - the same
    /// reason zoom above goes through one.
    let ifEditable (act: unit -> unit) =
        dispatch <| ExecFuncInMessage((fun m _ -> if not (openSheetIsReadOnly m) then act ()), dispatch)

    match id with
    // ---- sheet editing ----
    | ScCopy -> ifEditable (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlC)
    | ScPaste -> ifEditable (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlV)
    | ScSelectAll -> keyDispatch SheetT.KeyboardMsg.CtrlA
    | ScDelete -> ifEditable (fun () -> keyDispatch SheetT.KeyboardMsg.DEL)
    | ScRotateClockwise -> ifEditable (fun () -> sheetDispatch (SheetT.Rotate CommonTypes.Degree90))
    | ScRotateAnticlockwise -> ifEditable (fun () -> sheetDispatch (SheetT.Rotate CommonTypes.Degree270))
    | ScFlipVertical -> ifEditable (fun () -> sheetDispatch (SheetT.Flip SymbolT.FlipVertical))
    | ScFlipHorizontal -> ifEditable (fun () -> sheetDispatch (SheetT.Flip SymbolT.FlipHorizontal))
    | ScAlign -> ifEditable (fun () -> sheetDispatch (SheetT.Arrangement SheetT.AlignSymbols))
    | ScDistribute -> ifEditable (fun () -> sheetDispatch (SheetT.Arrangement SheetT.DistributeSymbols))
    | ScRotateLabel -> ifEditable (fun () -> sheetDispatch SheetT.RotateLabels)
    // Only with exactly one component selected: the Properties pane shows a name box for one
    // component and nothing to rename for none or several, so on any other selection the key
    // would switch the pane away from what the user was looking at and do nothing.
    | ScRenameComponent ->
        dispatch
        <| UpdateModel(fun m ->
            match m.Sheet.SelectedComponents, openSheetIsReadOnly m with
            | [ _ ], false ->
                m
                |> Optic.set rightPaneTabVisible_ Properties
                |> Optic.set runAfterRender_ (Some { FnToRun = focusComponentNameBox; ButtonSpinnerOn = false })
            | _ -> m)
    | ScUndo -> ifEditable (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlZ)
    | ScRedo -> ifEditable (fun () -> keyDispatch SheetT.KeyboardMsg.CtrlY)
    | ScRedrawFloatingWires -> ifEditable (fun () -> wholeSheet BusWireSeparate.redrawFloatingWires)
    | ScRedrawAllWires -> ifEditable (fun () -> wholeSheet BusWireSeparate.redrawAllWires)
    | ScMovePortsHelp ->
        dispatch
        <| ShowStaticInfoPopup(
            "How to move component ports",
            SymbolPortHelpers.moveCustomPortsPopup (),
            dispatch)

    // ---- escape, one action per context ----
    | ScCancelGesture -> keyDispatch SheetT.KeyboardMsg.ESC
    | ScClosePopup -> dispatch ClosePopup
    // ---- project browser ----
    | ScBrowserPrev -> dispatch (MoveProjectBrowserSelection -1)
    | ScBrowserNext -> dispatch (MoveProjectBrowserSelection 1)
    | ScBrowserOpen -> dispatch (OpenProjectBrowserSelection dispatch)
    | ScBrowserParent -> dispatch GoToProjectBrowserParent
    | ScDeselect -> sheetDispatch SheetT.ResetSelection
    | ScLeaveTextBox ->
        // hand the keyboard back to the schematic without reaching for the mouse
        match Browser.Dom.document.activeElement with
        | null -> ()
        | el -> el?blur ()

    // ---- view ----
    | ScZoomIn -> zoom dispatch ZoomIn
    | ScZoomOut -> zoom dispatch ZoomOut
    | ScZoomToFit -> zoom dispatch ZoomToFit
    // the Electron roles these replace stepped the zoom *level* by 0.5 rather than scaling a
    // factor, so match that or the steps feel different from what users had
    | ScAppZoomIn -> stepAppZoom 0.5
    | ScAppZoomOut -> stepAppZoom -0.5
    | ScAppZoomReset -> Bridge.setZoomLevel 0.0
    | ScFullScreen -> Bridge.toggleFullScreen ()
    | ScToggleGrid -> sheetDispatch SheetT.Msg.ToggleGrid
    | ScToggleWireArrows -> busWireDispatch BusWireT.Msg.ToggleArrowDisplay
    | ScWireTypeJump -> sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Jump)
    | ScWireTypeRadiussed -> sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Radiussed)
    | ScWireTypeModern -> sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Modern)
    | ScThemeDefault -> setTheme SymbolT.ThemeType.Colourful
    | ScThemeLight -> setTheme SymbolT.ThemeType.Light
    | ScThemeGrayscale -> setTheme SymbolT.ThemeType.White
    | ScToggleBuildTab -> dispatch ChangeBuildTabVisibility

    // ---- file ----
    | ScNewSheet -> dispatch <| MenuAction(MenuNewFile, dispatch)
    | ScSaveSheet -> dispatch <| MenuAction(MenuSaveFile, dispatch)
    | ScAbout -> UIPopups.viewInfoPopup dispatch
    | ScQuit -> dispatch <| MenuAction(MenuExit, dispatch)

    // ---- waveform simulator ----
    | ScWaveStepBack -> dispatch (WaveSimKeyPress "ArrowLeft")
    | ScWaveStepForward -> dispatch (WaveSimKeyPress "ArrowRight")

    // ---- text entry: macOS only, where these keys are otherwise delivered by the application
    // menu that no longer exists. Elsewhere they are unbound and Chromium handles them.
    | ScTextCopy -> Bridge.editCommand "copy"
    | ScTextCut -> Bridge.editCommand "cut"
    | ScTextPaste -> Bridge.editCommand "paste"
    | ScTextSelectAll -> Bridge.editCommand "selectAll"
    | ScTextUndo -> Bridge.editCommand "undo"
    | ScTextRedo -> Bridge.editCommand "redo"

    // ---- infrastructure ----
    // preventDefault is the whole point of these two: space would otherwise scroll the canvas,
    // and Ctrl+W might be read as close-window by the host
    | ScSuppressScroll -> ()
    | ScSwallowCloseWindow -> ()
    | ScDevTools -> Bridge.toggleDevTools ()

    // ---- gestures have no chord, so can never be resolved to ----
    | GsCtrlWheelZoom
    | GsShiftDragPan
    | GsSpaceDragPan
    | GsCtrlHoldPorts
    | GsTabBetweenBoxes -> ()

//-------------------------------------------------------------------------------------------------//
//----------------------------------------DISPATCHER-----------------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// One resolution, for the shadow log.
type Resolution =
    { Chord: string
      Context: string
      Resolved: string }

/// The last few resolutions, newest first, so what the dispatcher decided can be read from
/// outside the app while it runs. Debug builds only; see publishKeyLog.
let mutable private keyLog: Resolution list = []
let private keyLogLength = 40

let private record (chord: Chord) (ctx: KeyContext) (resolved: ShortcutId option) =
    let entry =
        { Chord = chordLabel isMac chord
          Context = string ctx
          Resolved =
            match resolved with
            | Some id -> string id
            | None -> "(unbound - falls through)" }
    keyLog <- entry :: List.truncate (keyLogLength - 1) keyLog

/// Expose the shadow log to the outside, next to window.issie. Debug builds only.
let publishKeyLog () =
    if JSHelpers.debugLevel > 0 then
        Browser.Dom.window?issieKeys <- (fun () -> keyLog |> List.toArray)

/// Whether the space bar is currently held, which puts the canvas in pan mode.
///
/// A held key rather than a chord, so it is tracked here beside the physical modifier state rather
/// than in the shortcut table - which resolves one press to one action and has nowhere to say
/// "while down".
let mutable private spaceHeld = false

let private isModifierKey (key: string) =
    key = "Control" || key = "Meta" || key = "Alt" || key = "Shift"

/// The one key handler in Issie.
let onKeyDown (dispatch: Msg -> unit) (e: Browser.Types.Event) =
    let ev: Browser.Types.KeyboardEvent = unbox e
    // an IME composing a character sends keydown with keyCode 229; those keys belong to the input
    if not (jsToBool ev?isComposing) && ev?keyCode <> 229 then
        if not (isModifierKey ev.key) then
            let ctx = currentContext ()
            let chord = chordOf ev
            let resolved = lookup platformTable ctx chord
            if JSHelpers.debugLevel > 0 then
                record chord ctx (resolved |> Option.map (fun s -> s.Id))
            match resolved with
            | Some s when s.DevOnly && JSHelpers.debugLevel = 0 -> ()
            | Some s when jsToBool ev?repeat && not s.AllowRepeat ->
                // swallow the repeat rather than letting it through: a held Delete should not
                // start deleting whatever the browser thinks is focused
                if s.PreventDefault then ev.preventDefault ()
            | Some s ->
                if s.PreventDefault then ev.preventDefault ()
                actionOf s.Id dispatch
            | None when ctx = CodeEditor ->
                // The code editor renders its own text and reconstructs typing from the raw key,
                // so it wants every key that is not a shortcut rather than the focused element.
                dispatch
                <| AnyKeyPress
                    { KeyString = ev.key
                      AltKey = ev.altKey
                      ControlKey = ev.ctrlKey
                      MetaKey = ev.metaKey
                      ShiftKey = ev.shiftKey }
            | None ->
                // Unbound: let it reach the focused element. This is what makes typing, caret
                // movement and native text editing work without any per-input code.
                ()

        // Ctrl held is a state, not a chord, so it is tracked separately from the table
        if (ev.key = "Control" || ev.key = "Meta") && not (SheetDisplay.isPhysicalModifierHeld ()) then
            SheetDisplay.setPhysicalModifierHeld true
            dispatch <| Sheet SheetT.PortMovementStart

        // Space held is the same kind of thing. Only on the canvas: in a text box space is a
        // space, and the chord table already declines to bind it there for the same reason.
        if ev.key = " " && not spaceHeld then
            match currentContext () with
            | SheetIdle
            | SheetBusy ->
                spaceHeld <- true
                dispatch <| Sheet SheetT.PanModeStart
            | _ -> ()

let onKeyUp (dispatch: Msg -> unit) (e: Browser.Types.Event) =
    let ev: Browser.Types.KeyboardEvent = unbox e
    // only Ctrl/Meta going up ends the mode. This used to fire on *any* keyup, so releasing some
    // other key while Ctrl was held left the model thinking Ctrl was up when it was not.
    if (ev.key = "Control" || ev.key = "Meta") && SheetDisplay.isPhysicalModifierHeld () then
        SheetDisplay.setPhysicalModifierHeld false
        dispatch <| Sheet SheetT.PortMovementEnd

    if ev.key = " " && spaceHeld then
        spaceHeld <- false
        dispatch <| Sheet SheetT.PanModeEnd

/// Ctrl released while the window is not focused produces no keyup at all.
let onWindowBlur (dispatch: Msg -> unit) (_: Browser.Types.Event) =
    if SheetDisplay.isPhysicalModifierHeld () then
        SheetDisplay.setPhysicalModifierHeld false
        dispatch <| Sheet SheetT.PortMovementEnd

    if spaceHeld then
        spaceHeld <- false
        dispatch <| Sheet SheetT.PanModeEnd
