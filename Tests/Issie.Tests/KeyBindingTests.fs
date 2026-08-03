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
            // bound in TextEntry unless it is deliberately a text action. This is what stops
            // Ctrl+A in a properties box selecting the schematic.
            let textActions =
                set [ "ScTextCopy"; "ScTextCut"; "ScTextPaste"; "ScTextSelectAll"
                      "ScTextUndo"; "ScTextRedo"; "ScLeaveTextBox" ]
            shortcuts
            |> List.filter (fun s -> List.contains TextEntry s.Contexts)
            |> List.iter (fun s ->
                Expect.isTrue (Set.contains (idName s.Id) textActions)
                    $"{idName s.Id} is bound in TextEntry but is not a text action")
        }

        test "chords the Electron menus used are preserved" {
            // Transcribed independently from the accelerator strings in Renderer.fs, so that a
            // slip in the table is caught rather than silently changing a binding users know.
            // Deletable once the menus are gone and these have been through a release.
            let expected =
                [ ScCopy,                 "Ctrl+C",            "Cmd+C"
                  ScPaste,                "Ctrl+V",            "Cmd+V"
                  ScSelectAll,            "Ctrl+A",            "Cmd+A"
                  // chordLabel is the compact menu form, so Del not Delete; the help table uses
                  // chordParts, which spells it out
                  ScDelete,               "Del",               "Backspace"
                  ScRotateAnticlockwise,  "Ctrl+Left",         "Cmd+Opt+Left"
                  ScRotateClockwise,      "Ctrl+Right",        "Cmd+Opt+Right"
                  ScFlipVertical,         "Ctrl+Up",           "Cmd+Opt+Up"
                  ScFlipHorizontal,       "Ctrl+Down",         "Cmd+Opt+Down"
                  ScAlign,                "Ctrl+Shift+A",      "Cmd+Opt+A"
                  ScDistribute,           "Ctrl+Shift+D",      "Cmd+Opt+D"
                  ScRotateLabel,          "Ctrl+Shift+Right",  "Cmd+Opt+R"
                  ScUndo,                 "Ctrl+Z",            "Cmd+Z"
                  ScRedo,                 "Ctrl+Y",            "Cmd+Shift+Z"
                  ScNewSheet,             "Ctrl+N",            "Cmd+N"
                  ScSaveSheet,            "Ctrl+S",            "Cmd+S"
                  ScSaveProjectNewFormat, "",                  "Cmd+Shift+S"
                  ScAbout,                "",                  "Cmd+H"
                  ScQuit,                 "",                  "Cmd+Q"
                  ScFullScreen,           "F11",               "Cmd+Ctrl+F"
                  ScAppZoomIn,            "Ctrl++",            "Cmd++"
                  ScAppZoomOut,           "Ctrl+-",            "Cmd+-"
                  ScAppZoomReset,         "Ctrl+0",            "Cmd+0"
                  ScDiagramZoomIn,        "Alt+Up",            "Cmd+Opt++"
                  ScDiagramZoomOut,       "Alt+Down",          "Cmd+Opt+-"
                  ScDiagramZoomToFit,     "Ctrl+W",            "Cmd+Opt+0"
                  ScToggleGrid,           "",                  "Cmd+Opt+G"
                  ScToggleWireArrows,     "",                  "Cmd+Opt+W"
                  ScDevTools,             "Ctrl+Shift+I",      "Cmd+Opt+I" ]

            expected
            |> List.iter (fun (id, win, mac) ->
                Expect.equal (idLabel false id) win $"{idName id} on windows"
                Expect.equal (idLabel true id) mac $"{idName id} on macOS")
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
