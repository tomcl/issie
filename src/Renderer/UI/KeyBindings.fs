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

let isMac = global.Node.Api.``process``.platform = global.Node.Base.Darwin

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
    // CodeEditorState is only ever set, never cleared, so on its own it latches on for the rest of
    // the session. Pairing it with PopupViewFunc is the only sound test until the editor's close
    // clears it - which it should.
    if m.CodeEditorState.IsSome && m.PopupViewFunc.IsSome then
        CodeEditor
    elif m.PopupViewFunc.IsSome || m.SpinnerPayload.IsSome || m.PopupDialogData.Progress.IsSome then
        Popup
    elif
        m.RightPaneTabVisible = RightTab.Simulation
        && m.SimSubTabVisible = SimSubTab.WaveSim
        && (getWSModel m).State = WaveSimState.Success
        // TODO: MousePointerIsOnRightSection is a mouse-position proxy, stale whenever the pointer
        // has not moved and measured against the wrong edge for non-wavesim tabs. It is replaced
        // by a click-driven KeyFocusPane when the old mechanisms are removed.
        && m.MousePointerIsOnRightSection
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
let actionOf (id: ShortcutId) (dispatch: Msg -> unit) : unit =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let keyDispatch (k: SheetT.KeyboardMsg) = sheetDispatch (SheetT.KeyPress k)
    let busWireDispatch (bMsg: BusWireT.Msg) = sheetDispatch (SheetT.Msg.Wire bMsg)
    let symbolDispatch msg = busWireDispatch (BusWireT.Msg.Symbol msg)
    let setTheme theme =
        dispatch <| SetThemeUserData theme
        symbolDispatch (SymbolT.Msg.SetTheme theme)
    let wireOf f =
        dispatch
        <| UpdateModel(fun m -> m |> Optic.map (sheet_ >-> SheetT.wire_) (f m.Sheet.SelectedComponents))
    let webContents () = JSHelpers.electronRemote.getCurrentWebContents ()
    let stepAppZoom delta =
        let wc = webContents ()
        wc.setZoomLevel (max -9.0 (min 9.0 (wc.getZoomLevel () + delta)))

    match id with
    // ---- sheet editing ----
    | ScCopy -> keyDispatch SheetT.KeyboardMsg.CtrlC
    | ScPaste -> keyDispatch SheetT.KeyboardMsg.CtrlV
    | ScSelectAll -> keyDispatch SheetT.KeyboardMsg.CtrlA
    | ScDelete -> keyDispatch SheetT.KeyboardMsg.DEL
    | ScRotateClockwise -> sheetDispatch (SheetT.Rotate CommonTypes.Degree90)
    | ScRotateAnticlockwise -> sheetDispatch (SheetT.Rotate CommonTypes.Degree270)
    | ScFlipVertical -> sheetDispatch (SheetT.Flip SymbolT.FlipVertical)
    | ScFlipHorizontal -> sheetDispatch (SheetT.Flip SymbolT.FlipHorizontal)
    | ScAlign -> sheetDispatch (SheetT.Arrangement SheetT.AlignSymbols)
    | ScDistribute -> sheetDispatch (SheetT.Arrangement SheetT.DistributeSymbols)
    | ScRotateLabel -> sheetDispatch SheetT.RotateLabels
    | ScUndo -> keyDispatch SheetT.KeyboardMsg.CtrlZ
    | ScRedo -> keyDispatch SheetT.KeyboardMsg.CtrlY
    | ScSeparateWires -> wireOf BusWireSeparate.reSeparateWiresFrom
    | ScRerouteWires -> wireOf BusWireSeparate.reRouteWiresFrom
    | ScMovePortsHelp ->
        dispatch
        <| ShowStaticInfoPopup(
            "How to move component ports",
            SymbolPortHelpers.moveCustomPortsPopup (),
            dispatch)

    // ---- escape, one action per context ----
    | ScCancelGesture -> keyDispatch SheetT.KeyboardMsg.ESC
    | ScClosePopup -> dispatch ClosePopup
    | ScDeselect -> sheetDispatch SheetT.ResetSelection
    | ScLeaveTextBox ->
        // hand the keyboard back to the schematic without reaching for the mouse
        match Browser.Dom.document.activeElement with
        | null -> ()
        | el -> el?blur ()

    // ---- view ----
    | ScDiagramZoomIn -> keyDispatch SheetT.KeyboardMsg.ZoomIn
    | ScDiagramZoomOut -> keyDispatch SheetT.KeyboardMsg.ZoomOut
    | ScDiagramZoomToFit -> keyDispatch SheetT.KeyboardMsg.CtrlW
    // the Electron roles these replace stepped the zoom *level* by 0.5 rather than scaling a
    // factor, so match that or the steps feel different from what users had
    | ScAppZoomIn -> stepAppZoom 0.5
    | ScAppZoomOut -> stepAppZoom -0.5
    | ScAppZoomReset -> webContents().setZoomLevel 0.0
    | ScFullScreen ->
        let w = JSHelpers.electronRemote.getCurrentWindow ()
        w.setFullScreen (not (w.isFullScreen ()))
    | ScToggleGrid -> sheetDispatch SheetT.Msg.ToggleGrid
    | ScToggleWireArrows -> busWireDispatch BusWireT.Msg.ToggleArrowDisplay
    | ScWireTypeJump -> sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Jump)
    | ScWireTypeRadiussed -> sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Radiussed)
    | ScWireTypeModern -> sheetDispatch (SheetT.WireType SheetT.WireTypeMsg.Modern)
    | ScThemeDefault -> setTheme SymbolT.ThemeType.Colourful
    | ScThemeLight -> setTheme SymbolT.ThemeType.Light
    | ScThemeGrayscale -> setTheme SymbolT.ThemeType.White
    | ScToggleBuildTab -> dispatch ChangeBuildTabVisibility
    | ScToggleMemoryDisplay ->
        JSHelpers.loggingMemory <- not JSHelpers.loggingMemory
        printfn $"""Memory display is now {if JSHelpers.loggingMemory then "on" else "off"}."""

    // ---- file ----
    | ScNewSheet -> dispatch <| MenuAction(MenuNewFile, dispatch)
    | ScSaveSheet -> dispatch <| MenuAction(MenuSaveFile, dispatch)
    | ScSaveProjectNewFormat -> dispatch <| MenuAction(MenuSaveProjectInNewFormat, dispatch)
    | ScWriteVerilog -> dispatch <| MenuAction(MenuVerilogOutput, dispatch)
    | ScAbout -> UIPopups.viewInfoPopup dispatch
    | ScQuit -> dispatch <| MenuAction(MenuExit, dispatch)

    // ---- waveform simulator ----
    | ScWaveStepBack -> dispatch (WaveSimKeyPress "ArrowLeft")
    | ScWaveStepForward -> dispatch (WaveSimKeyPress "ArrowRight")

    // ---- text entry: macOS only, where these keys are otherwise delivered by the application
    // menu that no longer exists. Elsewhere they are unbound and Chromium handles them.
    | ScTextCopy -> webContents().copy ()
    | ScTextCut -> webContents().cut ()
    | ScTextPaste -> webContents().paste ()
    | ScTextSelectAll -> webContents().selectAll ()
    | ScTextUndo -> webContents().undo ()
    | ScTextRedo -> webContents().redo ()

    // ---- infrastructure ----
    // preventDefault is the whole point of this one: space would otherwise scroll the canvas
    | ScSuppressScroll -> ()
    | ScDevTools -> renderer.ipcRenderer.send ("toggle-dev-tools", [||]) |> ignore

    // ---- gestures have no chord, so can never be resolved to ----
    | GsCtrlWheelZoom
    | GsShiftDragPan
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

/// Resolve a key event without acting on it: no preventDefault, no action. Used while the old
/// mechanisms are still in place, so that every context can be walked through and the resolutions
/// read back before anything depends on them.
let shadowKeyDown (_dispatch: Msg -> unit) (e: Browser.Types.Event) =
    let ev: Browser.Types.KeyboardEvent = unbox e
    // a modifier pressed on its own is not a chord
    match ev.key with
    | "Control"
    | "Meta"
    | "Alt"
    | "Shift" -> ()
    | _ ->
        let ctx = currentContext ()
        let chord = chordOf ev
        let resolved = lookup platformTable ctx chord |> Option.map (fun s -> s.Id)
        record chord ctx resolved
