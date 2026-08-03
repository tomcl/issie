/// The one place every keyboard shortcut in Issie is described.
///
/// This module is deliberately pure: no Fable.React, no ElectronAPI, no ModelType, no Browser.Dom.
/// It says *what* the shortcuts are, not what they do and not how they are delivered. The binding
/// from an identity to an action lives in UI/KeyBindings.fs, which is the only module that needs
/// the model. Keeping the two apart is what lets the help table, the renderer menus and (later)
/// the context menus all be generated from one list instead of drifting apart, which is what
/// happened to the hand-written table in UIPopups.fs.
///
/// Platform is always a parameter, never read here, so this module has no dependency on how the
/// host discovers it.
module KeyTypes

/// Modifier state, normalised so one table serves both platforms.
///
/// Primary is the "command" modifier: Ctrl on Windows and Linux, Cmd on macOS. That is the one
/// almost every shortcut wants, and normalising it here is what removes the isMac conditionals
/// that used to be written out by hand at every binding site.
///
/// Secondary is whichever of the two is left over. It exists for exactly one shortcut - macOS
/// full screen is Cmd+Ctrl+F - and should stay that rare.
type Mods =
    { Primary: bool
      Secondary: bool
      Alt: bool
      Shift: bool }

module Mods =
    let none = { Primary = false; Secondary = false; Alt = false; Shift = false }
    let prim = { none with Primary = true }
    let primShift = { prim with Shift = true }
    let primAlt = { prim with Alt = true }
    let primSecondary = { prim with Secondary = true }
    let alt = { none with Alt = true }
    let shift = { none with Shift = true }

/// A key identity, normalised away from the browser's raw strings.
///
/// Raw `ev.key` cannot be used directly: on macOS Option mangles it, so Cmd+Option+A arrives as
/// "å". Raw `ev.code` cannot be used either, since it is physical and so wrong on non-QWERTY
/// layouts. KeyBindings.keyNameOf resolves an event into one of these using `ev.key` where it is
/// meaningful and falling back to `ev.code` where it is not.
type KeyName =
    /// 'A' to 'Z', always upper case
    | KLetter of char
    /// '0' to '9'
    | KDigit of char
    /// F1 to F12
    | KFn of int
    /// One of the names in the Names module below - never an arbitrary string
    | KNamed of string

/// The named keys the table is allowed to use. A closed set, so a typo is a compile error
/// rather than a shortcut that silently never fires.
module Names =
    let arrowLeft = "ArrowLeft"
    let arrowRight = "ArrowRight"
    let arrowUp = "ArrowUp"
    let arrowDown = "ArrowDown"
    let escape = "Escape"
    let enter = "Enter"
    let tab = "Tab"
    let space = "Space"
    let delete = "Delete"
    let backspace = "Backspace"
    /// the physical - key, for zoom out
    let minus = "Minus"
    /// the physical = key, which carries + ; used for zoom in
    let equal = "Equal"

type Chord = { Mods: Mods; Key: KeyName }

/// Where the keyboard is logically pointing. Exactly one holds at any moment, and
/// KeyBindings.contextOfModel decides which by a fixed priority.
///
/// TextEntry and CodeEditor are *opaque*: a chord not bound in them does nothing rather than
/// falling back to a more general context. That single rule is what stops Ctrl+A in a properties
/// box selecting the whole schematic.
type KeyContext =
    /// DOM focus is in an input, textarea or select
    | TextEntry
    /// the hand-rolled code editor is up
    | CodeEditor
    /// a modal popup is showing and focus is not in one of its boxes
    | Popup
    /// the waveform simulator has the keyboard
    | WaveSim
    /// a draw block gesture is in progress
    | SheetBusy
    /// the schematic editor, nothing in progress
    | SheetIdle
    /// no project open, or nothing more specific applies
    | Global

/// Every shortcut identity in Issie.
///
/// Adding a case here is a compile error until KeyBindings.actionOf binds it, which is the point:
/// there is no way to describe a shortcut and forget to implement it. Several identities share a
/// chord and are told apart only by context - Escape is four different actions.
type ShortcutId =
    // ---- sheet editing ----
    | ScCopy
    | ScPaste
    | ScSelectAll
    | ScDelete
    | ScRotateClockwise
    | ScRotateAnticlockwise
    | ScFlipVertical
    | ScFlipHorizontal
    | ScAlign
    | ScDistribute
    | ScRotateLabel
    | ScUndo
    | ScRedo
    | ScSeparateWires
    | ScRerouteWires
    | ScMovePortsHelp
    // ---- escape, one identity per context ----
    | ScCancelGesture
    | ScClosePopup
    | ScDeselect
    | ScLeaveTextBox
    // ---- view ----
    | ScDiagramZoomIn
    | ScDiagramZoomOut
    | ScDiagramZoomToFit
    | ScAppZoomIn
    | ScAppZoomOut
    | ScAppZoomReset
    | ScFullScreen
    | ScToggleGrid
    | ScToggleWireArrows
    | ScWireTypeJump
    | ScWireTypeRadiussed
    | ScWireTypeModern
    | ScThemeDefault
    | ScThemeLight
    | ScThemeGrayscale
    | ScToggleBuildTab
    | ScToggleMemoryDisplay
    // ---- file ----
    | ScNewSheet
    | ScSaveSheet
    | ScSaveProjectNewFormat
    | ScWriteVerilog
    | ScAbout
    | ScQuit
    // ---- waveform simulator ----
    | ScWaveStepBack
    | ScWaveStepForward
    // ---- text entry (macOS needs these explicitly - see KeyBindings) ----
    | ScTextCopy
    | ScTextCut
    | ScTextPaste
    | ScTextSelectAll
    | ScTextUndo
    | ScTextRedo
    // ---- infrastructure ----
    | ScSuppressScroll
    | ScDevTools
    // ---- gestures: no chord, documented only ----
    | GsCtrlWheelZoom
    | GsShiftDragPan
    | GsCtrlHoldPorts
    | GsTabBetweenBoxes

type Trigger =
    /// Chords for Windows/Linux and for macOS. Several chords for one platform are alternatives
    /// for the same action; only the first is shown in help and menus.
    | Chords of win: Chord list * mac: Chord list
    /// Not a key at all - a mouse or modifier gesture that exists only so the help table can
    /// describe it. The help table was missing all of these.
    | Gesture of win: string * mac: string

type Category =
    | CatFile
    | CatEdit
    | CatView
    | CatWaveSim
    | CatTextEntry
    | CatGesture
    | CatDev

type ShortcutSpec =
    { Id: ShortcutId
      Trigger: Trigger
      /// Contexts in which this shortcut is live. Order is irrelevant.
      Contexts: KeyContext list
      /// Call preventDefault when it fires. Almost always true; false only when the browser's own
      /// handling is also wanted.
      PreventDefault: bool
      /// Fire on auto-repeat. True for zoom and cursor stepping, false for anything destructive.
      AllowRepeat: bool
      /// Help text. "" omits the row from the help table.
      Doc: string
      Category: Category
      DevOnly: bool }

/// Defaults, so each row in the table below states only what is interesting about it.
let private spec id trigger contexts doc category =
    { Id = id
      Trigger = trigger
      Contexts = contexts
      PreventDefault = true
      AllowRepeat = false
      Doc = doc
      Category = category
      DevOnly = false }

/// Wraps a row that should keep firing while the key is held: zooming and stepping the waveform
/// cursor, and nothing destructive.
let private repeating (s: ShortcutSpec) = { s with AllowRepeat = true }

/// Wraps a row that exists only in debug builds.
let private devOnly (s: ShortcutSpec) = { s with DevOnly = true }

// ---------------------------------------------------------------------------------------------
// shorthand for building the table
// ---------------------------------------------------------------------------------------------

let private ch mods key = { Mods = mods; Key = key }
let private letter c = KLetter c
let private named n = KNamed n

/// same chord on both platforms
let private both (chords: Chord list) = Chords(chords, chords)
/// Windows/Linux only - inert on macOS
let private winOnly (chords: Chord list) = Chords(chords, [])
/// macOS only - inert elsewhere
let private macOnly (chords: Chord list) = Chords([], chords)

/// The canvas, whether or not a gesture is in progress.
let private sheet = [ SheetIdle; SheetBusy ]
/// Anywhere the user is driving the app rather than typing into it.
let private appWide = [ SheetIdle; SheetBusy; WaveSim; Global ]

// ---------------------------------------------------------------------------------------------
// THE TABLE
// ---------------------------------------------------------------------------------------------

/// Every shortcut in Issie. The single source of truth for the dispatcher, the help table and the
/// shortcut labels shown on menus.
///
/// Chords are unchanged from the Electron menus they replace, so nothing has to be relearned.
/// They are safe to keep because context does the separating: Ctrl+Left rotates on the canvas and
/// still does word-navigation inside a text box, since rotate is simply not bound in TextEntry.
let shortcuts: ShortcutSpec list =
    [
      // ------------------------------------------------------------------ sheet editing
      spec ScCopy (both [ ch Mods.prim (letter 'C') ]) [ SheetIdle ] "Copy selected items" CatEdit
      spec ScPaste (both [ ch Mods.prim (letter 'V') ]) [ SheetIdle ] "Paste items" CatEdit
      spec ScSelectAll (both [ ch Mods.prim (letter 'A') ]) [ SheetIdle ] "Select all items" CatEdit
      spec ScDelete
          (Chords(win = [ ch Mods.none (named Names.delete) ],
                  mac = [ ch Mods.none (named Names.backspace); ch Mods.none (named Names.delete) ]))
          [ SheetIdle ] "Delete items" CatEdit

      spec ScRotateClockwise
          (Chords(win = [ ch Mods.prim (named Names.arrowRight) ],
                  mac = [ ch Mods.primAlt (named Names.arrowRight) ]))
          [ SheetIdle ] "Rotate items clockwise" CatEdit
      spec ScRotateAnticlockwise
          (Chords(win = [ ch Mods.prim (named Names.arrowLeft) ],
                  mac = [ ch Mods.primAlt (named Names.arrowLeft) ]))
          [ SheetIdle ] "Rotate items anticlockwise" CatEdit
      spec ScFlipVertical
          (Chords(win = [ ch Mods.prim (named Names.arrowUp) ],
                  mac = [ ch Mods.primAlt (named Names.arrowUp) ]))
          [ SheetIdle ] "Flip items vertically" CatEdit
      spec ScFlipHorizontal
          (Chords(win = [ ch Mods.prim (named Names.arrowDown) ],
                  mac = [ ch Mods.primAlt (named Names.arrowDown) ]))
          [ SheetIdle ] "Flip items horizontally" CatEdit

      spec ScAlign
          (Chords(win = [ ch Mods.primShift (letter 'A') ], mac = [ ch Mods.primAlt (letter 'A') ]))
          [ SheetIdle ] "Align items" CatEdit
      spec ScDistribute
          (Chords(win = [ ch Mods.primShift (letter 'D') ], mac = [ ch Mods.primAlt (letter 'D') ]))
          [ SheetIdle ] "Distribute items" CatEdit
      spec ScRotateLabel
          (Chords(win = [ ch Mods.primShift (named Names.arrowRight) ],
                  mac = [ ch Mods.primAlt (letter 'R') ]))
          [ SheetIdle ] "Rotate label of item" CatEdit

      spec ScUndo (both [ ch Mods.prim (letter 'Z') ]) [ SheetIdle ] "Undo diagram action" CatEdit
      spec ScRedo
          (Chords(win = [ ch Mods.prim (letter 'Y') ], mac = [ ch Mods.primShift (letter 'Z') ]))
          [ SheetIdle ] "Redo diagram action" CatEdit

      // menu-only: reachable from the Edit dropdown, no chord
      spec ScSeparateWires (both []) [ SheetIdle ] "Separate wires from selected components" CatEdit
      spec ScRerouteWires (both []) [ SheetIdle ] "Reroute wires from selected components" CatEdit
      spec ScMovePortsHelp (both []) [ SheetIdle ] "" CatEdit

      // ------------------------------------------------------------------ escape, by context
      spec ScCancelGesture (both [ ch Mods.none (named Names.escape) ]) [ SheetBusy ]
          "Cancel the action in progress" CatEdit
      spec ScClosePopup (both [ ch Mods.none (named Names.escape) ]) [ Popup ]
          "Close the open dialogue" CatEdit
      spec ScDeselect (both [ ch Mods.none (named Names.escape) ]) [ SheetIdle ]
          "Deselect everything" CatEdit
      spec ScLeaveTextBox
          (both [ ch Mods.none (named Names.enter); ch Mods.none (named Names.escape) ])
          [ TextEntry ] "Leave an input box, returning the keyboard to the schematic" CatTextEntry

      // ------------------------------------------------------------------ view
      repeating (
          spec ScDiagramZoomIn
              (Chords(win = [ ch Mods.alt (named Names.arrowUp) ],
                      mac = [ ch Mods.primAlt (named Names.equal) ]))
              sheet "Zoom diagram in" CatView)
      repeating (
          spec ScDiagramZoomOut
              (Chords(win = [ ch Mods.alt (named Names.arrowDown) ],
                      mac = [ ch Mods.primAlt (named Names.minus) ]))
              sheet "Zoom diagram out" CatView)
      spec ScDiagramZoomToFit
          (Chords(win = [ ch Mods.prim (letter 'W') ], mac = [ ch Mods.primAlt (KDigit '0') ]))
          sheet "Zoom circuit to fit in screen" CatView

      repeating (
          spec ScAppZoomIn
              (both [ ch Mods.prim (named Names.equal); ch Mods.primShift (named Names.equal) ])
              appWide "Zoom application in" CatView)
      repeating (
          spec ScAppZoomOut (both [ ch Mods.prim (named Names.minus) ]) appWide
              "Zoom application out" CatView)
      spec ScAppZoomReset (both [ ch Mods.prim (KDigit '0') ]) appWide
          "Zoom application reset" CatView
      spec ScFullScreen
          (Chords(win = [ ch Mods.none (KFn 11) ], mac = [ ch Mods.primSecondary (letter 'F') ]))
          appWide "Enter/exit fullscreen" CatView

      spec ScToggleGrid (macOnly [ ch Mods.primAlt (letter 'G') ]) sheet
          "Show/hide grid lines" CatView
      spec ScToggleWireArrows (macOnly [ ch Mods.primAlt (letter 'W') ]) sheet
          "Show/hide wire arrows" CatView

      // menu-only
      spec ScWireTypeJump (both []) sheet "" CatView
      spec ScWireTypeRadiussed (both []) sheet "" CatView
      spec ScWireTypeModern (both []) sheet "" CatView
      spec ScThemeDefault (both []) sheet "" CatView
      spec ScThemeLight (both []) sheet "" CatView
      spec ScThemeGrayscale (both []) sheet "" CatView
      spec ScToggleBuildTab (both []) appWide "" CatView
      spec ScToggleMemoryDisplay (both []) appWide "" CatView

      // ------------------------------------------------------------------ file
      spec ScNewSheet (both [ ch Mods.prim (letter 'N') ]) appWide "Create new sheet" CatFile
      spec ScSaveSheet (both [ ch Mods.prim (letter 'S') ]) appWide "Save current sheet" CatFile
      spec ScSaveProjectNewFormat (macOnly [ ch Mods.primShift (letter 'S') ]) appWide
          "Save project in new format" CatFile
      spec ScWriteVerilog (both []) appWide "" CatFile
      spec ScAbout (macOnly [ ch Mods.prim (letter 'H') ]) appWide
          "Open about/help window" CatFile
      spec ScQuit (macOnly [ ch Mods.prim (letter 'Q') ]) appWide "Quit application" CatFile

      // ------------------------------------------------------------------ waveform simulator
      repeating (
          spec ScWaveStepBack (both [ ch Mods.none (named Names.arrowLeft) ]) [ WaveSim ]
              "Move the waveform cursor back one clock cycle" CatWaveSim)
      repeating (
          spec ScWaveStepForward (both [ ch Mods.none (named Names.arrowRight) ]) [ WaveSim ]
              "Move the waveform cursor forward one clock cycle" CatWaveSim)

      // ------------------------------------------------------------------ text entry
      // macOS routes clipboard keys in text fields through the application menu, so with no Edit
      // menu they must be performed explicitly. Windows and Linux need nothing here: leaving them
      // unbound lets Chromium handle them, which it does correctly.
      spec ScTextCopy (macOnly [ ch Mods.prim (letter 'C') ]) [ TextEntry ] "" CatTextEntry
      spec ScTextCut (macOnly [ ch Mods.prim (letter 'X') ]) [ TextEntry ] "" CatTextEntry
      spec ScTextPaste (macOnly [ ch Mods.prim (letter 'V') ]) [ TextEntry ] "" CatTextEntry
      spec ScTextSelectAll (macOnly [ ch Mods.prim (letter 'A') ]) [ TextEntry ] "" CatTextEntry
      spec ScTextUndo (macOnly [ ch Mods.prim (letter 'Z') ]) [ TextEntry ] "" CatTextEntry
      spec ScTextRedo (macOnly [ ch Mods.primShift (letter 'Z') ]) [ TextEntry ] "" CatTextEntry

      // ------------------------------------------------------------------ infrastructure
      // Space scrolls the page by default, which on the canvas is never wanted. It is left unbound
      // in TextEntry so that typing a space works, which the old evilUIState hack got wrong.
      repeating (
          spec ScSuppressScroll (both [ ch Mods.none (named Names.space) ])
              [ SheetIdle; SheetBusy; WaveSim; Popup; Global ] "" CatView)

      devOnly (
          spec ScDevTools
              (Chords(win = [ ch Mods.primShift (letter 'I') ],
                      mac = [ ch Mods.primAlt (letter 'I') ]))
              appWide "Show/hide browser developer tools" CatDev)

      // ------------------------------------------------------------------ gestures (help only)
      spec GsCtrlWheelZoom (Gesture("Control + mouse wheel", "Command-mouse wheel")) []
          "Zoom the diagram" CatGesture
      spec GsShiftDragPan (Gesture("Shift + drag on canvas", "Shift-drag on canvas")) []
          "Scroll the diagram" CatGesture
      spec GsCtrlHoldPorts (Gesture("Hold Control over a custom component", "Hold Command over a custom component")) []
          "Show the ports and resize corners that can be dragged" CatGesture
      spec GsTabBetweenBoxes (Gesture("Tab / Shift + Tab", "Tab / Shift-Tab")) []
          "Move between input boxes in the properties pane" CatGesture ]

// ---------------------------------------------------------------------------------------------
// lookup
// ---------------------------------------------------------------------------------------------

/// Chords for one platform.
let chordsFor (isMac: bool) (spec: ShortcutSpec) : Chord list =
    match spec.Trigger with
    | Chords(win, mac) -> if isMac then mac else win
    | Gesture _ -> []

/// (context, chord) -> spec, for one platform. Built once per platform by the caller.
let table (isMac: bool) : Map<KeyContext * Chord, ShortcutSpec> =
    shortcuts
    |> List.collect (fun s -> chordsFor isMac s |> List.collect (fun c -> s.Contexts |> List.map (fun ctx -> (ctx, c), s)))
    |> Map.ofList

/// Resolve a chord in a context.
///
/// There is deliberately no fallback to a more general context. A chord that means nothing in
/// TextEntry must reach the focused box rather than doing something to the schematic, and the same
/// argument applies to every other context - so every shortcut states all the contexts it works in.
let lookup (tbl: Map<KeyContext * Chord, ShortcutSpec>) (ctx: KeyContext) (chord: Chord) =
    Map.tryFind (ctx, chord) tbl

// ---------------------------------------------------------------------------------------------
// display
// ---------------------------------------------------------------------------------------------

let private keyDisplayName (key: KeyName) =
    match key with
    | KLetter c -> string c
    | KDigit c -> string c
    | KFn n -> $"F{n}"
    | KNamed n when n = Names.arrowLeft -> "Left arrow"
    | KNamed n when n = Names.arrowRight -> "Right arrow"
    | KNamed n when n = Names.arrowUp -> "Up arrow"
    | KNamed n when n = Names.arrowDown -> "Down arrow"
    | KNamed n when n = Names.equal -> "Plus (+)"
    | KNamed n when n = Names.minus -> "Minus (-)"
    | KNamed n when n = Names.enter -> "Return"
    | KNamed n -> n

/// The parts of a chord, in the order they should be shown. Returned as a list so that the
/// existing help-table renderer in UIPopups can join them with its own separator unchanged.
let chordParts (isMac: bool) (chord: Chord) : string list =
    let m = chord.Mods
    [ if m.Primary then yield (if isMac then "Command" else "Control")
      if m.Secondary then yield (if isMac then "Control" else "Meta")
      if m.Alt then yield (if isMac then "Option" else "Alt")
      if m.Shift then yield "Shift"
      yield keyDisplayName chord.Key ]

let private keyShortName (key: KeyName) =
    match key with
    | KLetter c -> string c
    | KDigit c -> string c
    | KFn n -> $"F{n}"
    | KNamed n when n = Names.arrowLeft -> "Left"
    | KNamed n when n = Names.arrowRight -> "Right"
    | KNamed n when n = Names.arrowUp -> "Up"
    | KNamed n when n = Names.arrowDown -> "Down"
    | KNamed n when n = Names.equal -> "+"
    | KNamed n when n = Names.minus -> "-"
    | KNamed n when n = Names.escape -> "Esc"
    | KNamed n when n = Names.delete -> "Del"
    | KNamed n when n = Names.enter -> "Return"
    | KNamed n -> n

/// Compact form for a menu label, e.g. "Ctrl+Right" or "Cmd+Opt+Right".
let chordLabel (isMac: bool) (chord: Chord) : string =
    let m = chord.Mods
    [ if m.Primary then yield (if isMac then "Cmd" else "Ctrl")
      if m.Secondary then yield (if isMac then "Ctrl" else "Meta")
      if m.Alt then yield (if isMac then "Opt" else "Alt")
      if m.Shift then yield "Shift"
      yield keyShortName chord.Key ]
    |> String.concat "+"

/// The label to show beside a menu item, or "" when the shortcut has no chord on this platform.
let idLabel (isMac: bool) (id: ShortcutId) : string =
    shortcuts
    |> List.tryFind (fun s -> s.Id = id)
    |> Option.bind (fun s -> chordsFor isMac s |> List.tryHead)
    |> Option.map (chordLabel isMac)
    |> Option.defaultValue ""

// ---------------------------------------------------------------------------------------------
// self-check
// ---------------------------------------------------------------------------------------------

/// Problems with the table, as human-readable strings. Empty means the table is sound.
/// Called at startup in debug builds: a shortcut that can never fire because another one shadows
/// it is invisible at runtime and very hard to notice by hand.
let validate () : string list =
    let duplicateIds =
        shortcuts
        |> List.countBy (fun s -> s.Id)
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map (fun (id, n) -> $"ShortcutId {id} appears {n} times")

    let clashesOn isMac name =
        shortcuts
        |> List.collect (fun s ->
            chordsFor isMac s |> List.collect (fun c -> s.Contexts |> List.map (fun ctx -> (ctx, c), s.Id)))
        |> List.groupBy fst
        |> List.filter (fun (_, g) -> List.length g > 1)
        |> List.map (fun ((ctx, c), g) ->
            let ids = g |> List.map (snd >> string) |> String.concat ", "
            $"{name}: {chordLabel isMac c} in {ctx} is claimed by {ids}")

    let gesturesHaveNoContexts =
        shortcuts
        |> List.filter (fun s ->
            match s.Trigger with
            | Gesture _ -> not (List.isEmpty s.Contexts)
            | Chords _ -> false)
        |> List.map (fun s -> $"{s.Id} is a Gesture but declares contexts, which can never match")

    duplicateIds @ clashesOn false "windows" @ clashesOn true "macos" @ gesturesHaveNoContexts
