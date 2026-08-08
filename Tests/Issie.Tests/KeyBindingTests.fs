/// The shortcut table: a shortcut that can never fire, or that quietly shadows another, is
/// invisible at runtime and very hard to spot by reading. These tests are what makes the table
/// safe to edit.
module KeyBindingTests

open Expecto
open KeyTypes

/// Every case of the ShortcutId union, via reflection. Under .NET this is exact, so a case added
/// to the union but never given a row in the table is caught here rather than by never working.
let private allShortcutIds () =
    Reflection.FSharpType.GetUnionCases typeof<ShortcutId>
    |> Array.map (fun c -> c.Name)
    |> Set.ofArray

let private idName (id: ShortcutId) =
    let case, _ = Reflection.FSharpValue.GetUnionFields(id, typeof<ShortcutId>)
    case.Name

let tests =
    testList "KeyBindings" [

        test "the table validates" {
            // duplicate ids, and any (context, chord) claimed twice on either platform
            Expect.equal (KeyTypes.validate ()) [] "KeyTypes.validate reported problems"
        }

        test "every ShortcutId has exactly one row" {
            let inTable = shortcuts |> List.map (fun s -> idName s.Id) |> Set.ofList
            let declared = allShortcutIds ()
            Expect.equal (Set.difference declared inTable) Set.empty
                "ShortcutId cases with no row in the table - they can never fire"
            Expect.equal (Set.difference inTable declared) Set.empty
                "rows naming an id that is not a ShortcutId case - impossible, but check anyway"
        }

        test "gestures carry no chords and no contexts" {
            // A Gesture exists only so the help table can describe it. Giving one a context would
            // suggest it is dispatchable, which it is not.
            shortcuts
            |> List.filter (fun s -> match s.Trigger with Gesture _ -> true | Chords _ -> false)
            |> List.iter (fun s ->
                Expect.isEmpty s.Contexts $"{idName s.Id} is a gesture but declares contexts"
                Expect.isEmpty (chordsFor false s) $"{idName s.Id} is a gesture but has windows chords"
                Expect.isEmpty (chordsFor true s) $"{idName s.Id} is a gesture but has macOS chords")
        }

        test "every chord-bearing shortcut names at least one context" {
            // A chord with no context can never be looked up, so it silently does nothing.
            shortcuts
            |> List.filter (fun s -> not (List.isEmpty (chordsFor false s) && List.isEmpty (chordsFor true s)))
            |> List.iter (fun s ->
                Expect.isNonEmpty s.Contexts $"{idName s.Id} has chords but no context to fire in")
        }

        test "text entry is not shadowed by canvas shortcuts" {
            // The whole point of the context system: a chord bound on the canvas must not also be
            // bound in TextEntry unless it is there on purpose. This is what stops Ctrl+A in a
            // properties box selecting the schematic.
            let textActions =
                set [ "ScTextCopy"; "ScTextCut"; "ScTextPaste"; "ScTextSelectAll"
                      "ScTextUndo"; "ScTextRedo"; "ScLeaveTextBox" ]
            // Keys an input box has no use for, so letting them through costs typing nothing and
            // saves the user from losing zoom the moment a caret lands somewhere. The test that
            // asks for them in TextEntry is below; both must be changed together, which is the
            // point of naming them twice.
            let harmlessInATextBox = set [ "ScZoomIn"; "ScZoomOut"; "ScZoomToFit" ]
            shortcuts
            |> List.filter (fun s -> List.contains TextEntry s.Contexts)
            |> List.iter (fun s ->
                Expect.isTrue
                    (Set.contains (idName s.Id) textActions
                     || Set.contains (idName s.Id) harmlessInATextBox)
                    $"{idName s.Id} is bound in TextEntry but is neither a text action nor a key a text box has no use for")
        }

        test "the chords are the ones intended" {
            // Written out here independently of the table, so that a slip while editing it is
            // caught rather than silently changing a key people use. This replaces a list that
            // pinned the accelerators of the Electron menus: those are gone, have been through a
            // release, and several have deliberately changed since.
            let expected =
                [ ScCopy,                 "Ctrl+C",            "Cmd+C"
                  ScPaste,                "Ctrl+V",            "Cmd+V"
                  ScSelectAll,            "Ctrl+A",            "Cmd+A"
                  // chordLabel is the compact menu form, so Del not Delete; the help table uses
                  // chordParts, which spells it out
                  ScDelete,               "Del",               "Backspace"
                  ScRotateAnticlockwise,  "Ctrl+Left",         "Cmd+Left"
                  ScRotateClockwise,      "Ctrl+Right",        "Cmd+Right"
                  ScFlipVertical,         "Ctrl+Up",           "Cmd+Up"
                  ScFlipHorizontal,       "Ctrl+Down",         "Cmd+Down"
                  ScAlign,                "Ctrl+Shift+A",      "Cmd+Shift+A"
                  ScDistribute,           "Ctrl+Shift+D",      "Cmd+Shift+D"
                  ScRotateLabel,          "Ctrl+Shift+R",      "Cmd+Shift+R"
                  ScUndo,                 "Ctrl+Z",            "Cmd+Z"
                  ScRedo,                 "Ctrl+Y",            "Cmd+Shift+Z"
                  ScNewSheet,             "Ctrl+N",            "Cmd+N"
                  ScSaveSheet,            "Ctrl+S",            "Cmd+S"
                  ScAbout,                "F1",                "F1"
                  ScQuit,                 "",                  "Cmd+Q"
                  ScFullScreen,           "F11",               "Cmd+Ctrl+F"
                  // Primary with + - 0 zooms what the user is looking at ...
                  ScZoomIn,               "Ctrl++",            "Cmd++"
                  ScZoomOut,              "Ctrl+-",            "Cmd+-"
                  ScZoomToFit,            "Ctrl+0",            "Cmd+0"
                  // ... and adding Alt zooms the whole application
                  ScAppZoomIn,            "Ctrl+Alt++",        "Cmd+Opt++"
                  ScAppZoomOut,           "Ctrl+Alt+-",        "Cmd+Opt+-"
                  ScAppZoomReset,         "Ctrl+Alt+0",        "Cmd+Opt+0"
                  ScToggleGrid,           "Ctrl+Alt+G",        "Cmd+Opt+G"
                  ScToggleWireArrows,     "Ctrl+Alt+W",        "Cmd+Opt+W"
                  // bound to nothing, so that the host cannot read it as close-window
                  ScSwallowCloseWindow,   "Ctrl+W",            ""
                  ScDevTools,             "Ctrl+Shift+I",      "Cmd+Opt+I" ]

            expected
            |> List.iter (fun (id, win, mac) ->
                Expect.equal (idLabel false id) win $"{idName id} on windows"
                Expect.equal (idLabel true id) mac $"{idName id} on macOS")
        }

        test "a shortcut has the same chord on both platforms unless it has a reason not to" {
            // Mods.Primary already absorbs Ctrl-against-Cmd, so "the same" here means literally
            // the same chords. Anything else is a shortcut a user cannot carry from one machine to
            // another, and every one of these was an accident rather than a decision before.
            let deliberatelyDifferent =
                set [
                    // platform conventions, which it would be wrong to make uniform
                    "ScDelete"      // Backspace deletes on macOS
                    "ScRedo"        // Cmd+Shift+Z on macOS, Ctrl+Y on Windows
                    "ScFullScreen"  // F11 against Cmd+Ctrl+F
                    "ScQuit"        // macOS quits from the keyboard; Windows does not
                    "ScDevTools"    // follows the browser convention on each platform
                    // exists only on the platform whose host might act on the key
                    "ScSwallowCloseWindow"
                ]
            // The text-entry actions are macOS-only by construction: elsewhere Chromium handles
            // them and binding them would take them away from it.
            let isTextAction (s: ShortcutSpec) = s.Category = CatTextEntry

            shortcuts
            |> List.filter (fun s -> not (Set.contains (idName s.Id) deliberatelyDifferent))
            |> List.filter (fun s -> not (isTextAction s))
            |> List.iter (fun s ->
                Expect.equal (chordsFor false s) (chordsFor true s)
                    $"{idName s.Id} differs between platforms with no reason recorded")
        }

        test "zoom works wherever nothing else wants the key" {
            // The bug this pins: zoom used to be bound only in the sheet contexts, so it stopped
            // the moment a properties box took DOM focus or the right-hand pane was clicked - in
            // all of which nothing else wants Ctrl and + - 0. Which thing is zoomed is decided in
            // the action, not by the context, so the context list can be as wide as it likes.
            let mustZoomIn = [ SheetIdle; SheetBusy; WaveSim; TextEntry ]
            [ ScZoomIn; ScZoomOut; ScZoomToFit ]
            |> List.iter (fun id ->
                let spec = shortcuts |> List.find (fun s -> s.Id = id)
                mustZoomIn
                |> List.iter (fun ctx ->
                    Expect.isTrue (List.contains ctx spec.Contexts)
                        $"{idName id} is not bound in {ctx}, so zoom dies there"))
        }

        test "the waveform cursor keys yield to a focused input box" {
            // Left and Right step the cursor, and must not do so while someone is typing. That is
            // TextEntry's opacity doing the work: the arrows are simply not bound there.
            [ ScWaveStepBack; ScWaveStepForward ]
            |> List.iter (fun id ->
                let spec = shortcuts |> List.find (fun s -> s.Id = id)
                Expect.isFalse (List.contains TextEntry spec.Contexts)
                    $"{idName id} is bound in TextEntry, so it would fire while typing"
                Expect.isTrue (List.contains WaveSim spec.Contexts)
                    $"{idName id} must work when the wave simulator has the keyboard")
        }

        test "cancel is Escape in every context that has one" {
            // Escape is four different actions told apart only by context. If any of them drifts
            // onto another key the others become unreachable-looking.
            [ ScCancelGesture; ScClosePopup; ScDeselect ]
            |> List.iter (fun id ->
                Expect.equal (idLabel false id) "Esc" $"{idName id} on windows"
                Expect.equal (idLabel true id) "Esc" $"{idName id} on macOS")
        }
    ]
