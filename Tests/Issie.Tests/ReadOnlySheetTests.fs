/// Holding a library component's sheet at what it loaded with.
///
/// The whole of read-only enforcement is ModelHelpers.pinSheet, which writes the saved half of the
/// draw block back over the live one after every message. It restores rather than refuses because
/// there are far too many ways to edit a sheet to block them one at a time and be sure: dozens of
/// mutating cases across the three draw block Msg types, several places that write model.Sheet
/// directly without reaching any update function, and UpdateModel, which carries an arbitrary
/// Model -> Model.
///
/// So the thing worth testing is not any particular edit but the invariant: whatever the draw
/// block has become, what the sheet would be SAVED as is unchanged - while the display-only
/// fields, which selection and hover use and which no .dgm ever holds, are left alone.
module ReadOnlySheetTests

open Expecto
open CommonTypes
open DrawModelType
open ModelType
open ModelHelpers
open SheetDescription
open SheetDescription.Operators

/// two 4-bit inputs into an adder, one output
let private adderSheet =
    describeSheet "adder" [
        comp "A" (Input1(4, None))
        comp "B" (Input1(4, None))
        comp "ADD" (NbitsAdderNoCinCout 4)
        comp "S" (Output 4)
    ] [ "A" ==> "ADD/P"; "B" ==> "ADD/Q"; "ADD/SUM" ==> "S" ]

/// A sheet model holding every component of the design as a symbol, as the app has after a load.
let private loadedSheet () =
    let comps, conns =
        match SheetLayout.toCanvasState adderSheet with
        | Ok canvas -> canvas
        | Error e -> failtest $"sheet would not build: {e}"
    let sheet, _ = SheetUpdate.init ()
    let symbols = SymbolUpdate.loadComponents [] sheet.Wire.Symbol comps
    let wireOf (conn: Connection) : BusWireT.Wire =
        { WId = conn.Id
          InputPort = InputPortId conn.Target.Id
          OutputPort = OutputPortId conn.Source.Id
          Color = HighLightColor.Red
          Width = 1
          Segments = []
          StartPos = { X = 0.; Y = 0. }
          InitialOrientation = BusWireT.Horizontal }
    { sheet with
        Wire =
            { sheet.Wire with
                Symbol = symbols
                Wires = conns |> List.map (fun c -> c.Id, wireOf c) |> Map.ofList } }

/// What this sheet would be written to its .dgm as: the one thing the pin exists to keep fixed.
let private savedState (sheet: SheetT.Model) : CanvasState =
    // the pair the app itself saves - see BusWireUpdate, which builds canvas state exactly so
    SymbolUpdate.extractComponents sheet.Wire.Symbol |> List.sortBy (fun c -> c.Id),
    BusWire.extractConnections sheet.Wire |> List.sortBy (fun c -> c.Id)

let private someSymbolId (sheet: SheetT.Model) =
    sheet.Wire.Symbol.Symbols |> Map.toList |> List.head |> fst

let tests =
    testList "ReadOnlySheet" [

        test "moving a symbol leaves the saved state alone" {
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let cid = someSymbolId loaded
            let moved =
                { loaded with
                    Wire =
                        { loaded.Wire with
                            Symbol = SymbolUpdate.moveSymbols loaded.Wire.Symbol [cid] { X = 137.; Y = -42. } } }
            Expect.notEqual (savedState moved) (savedState loaded) "the move did reach the saved state"
            Expect.equal (savedState (pinSheet pinned moved)) (savedState loaded)
                "and the pin put every component back where it was"
        }

        test "deleting symbols and wires leaves the saved state alone" {
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let gutted =
                { loaded with
                    Wire =
                        { loaded.Wire with
                            Symbol = { loaded.Wire.Symbol with Symbols = Map.empty }
                            Wires = Map.empty } }
            Expect.equal (savedState (pinSheet pinned gutted)) (savedState loaded)
                "a symbol the sheet has deleted comes back, and so do the wires"
        }

        test "an added symbol does not survive" {
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let cid = someSymbolId loaded
            let extra = ComponentId 999 // an id that was not there
            let added =
                { loaded with
                    Wire =
                        { loaded.Wire with
                            Symbol =
                                { loaded.Wire.Symbol with
                                    Symbols =
                                        loaded.Wire.Symbol.Symbols
                                        |> Map.add extra loaded.Wire.Symbol.Symbols[cid] } } }
            let pinnedBack = pinSheet pinned added
            Expect.isFalse (Map.containsKey extra pinnedBack.Wire.Symbol.Symbols)
                "the pin maps over the symbols it holds, not over the ones the sheet now has"
            Expect.equal (savedState pinnedBack) (savedState loaded) "so the saved state is unchanged"
        }

        test "changing a component's type - what the properties pane does - does not stick" {
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let cid = someSymbolId loaded
            let widened =
                loaded.Wire.Symbol.Symbols
                |> Map.map (fun _ sym ->
                    { sym with Component = { sym.Component with Type = Input1(17, None) } })
            let changed =
                { loaded with
                    Wire = { loaded.Wire with Symbol = { loaded.Wire.Symbol with Symbols = widened } } }
            Expect.equal (savedState (pinSheet pinned changed)) (savedState loaded)
                "widths and types come from the pin, since they are saved"
        }

        test "selection and hover still work, because they are not saved" {
            // SymbolUpdate.extractComponent ignores Symbol.Appearance, so the pin can leave it
            // alone. That is what makes a viewed sheet worth looking at rather than inert: the
            // user can still select a component and read its properties.
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let cid = someSymbolId loaded
            let selected =
                { loaded with
                    Wire =
                        { loaded.Wire with
                            Symbol =
                                { loaded.Wire.Symbol with
                                    Symbols =
                                        loaded.Wire.Symbol.Symbols
                                        |> Map.map (fun _ sym ->
                                            { sym with
                                                Appearance =
                                                    { sym.Appearance with
                                                        Colour = "lightgreen"; ShowPorts = SymbolT.ShowBoth } }) } } }
            let pinnedBack = pinSheet pinned selected
            Expect.equal pinnedBack.Wire.Symbol.Symbols[cid].Appearance.Colour "lightgreen"
                "the highlight survives the pin"
            Expect.equal pinnedBack.Wire.Symbol.Symbols[cid].Appearance.ShowPorts SymbolT.ShowBoth
                "and so does showing the ports"
            Expect.equal (savedState pinnedBack) (savedState loaded) "while nothing saved has moved"
        }

        test "nothing can be copied out, and no undo history builds up" {
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let copiedAndEdited =
                { loaded with
                    Wire =
                        { loaded.Wire with
                            CopiedWires = loaded.Wire.Wires
                            Symbol = { loaded.Wire.Symbol with CopiedSymbols = loaded.Wire.Symbol.Symbols } }
                    UndoList = [loaded]
                    RedoList = [loaded]
                    TmpModel = Some loaded }
            let pinnedBack = pinSheet pinned copiedAndEdited
            Expect.isEmpty pinnedBack.Wire.Symbol.CopiedSymbols
                "a copy from a library component puts nothing on the clipboard"
            Expect.isEmpty pinnedBack.Wire.CopiedWires "nor do its wires"
            Expect.isEmpty pinnedBack.UndoList "there is no history to step back through"
            Expect.isEmpty pinnedBack.RedoList "nor forward"
            Expect.isNone pinnedBack.TmpModel "and no half-finished gesture is kept"
        }

        test "an untouched sheet is returned as the very same object" {
            // The pin runs after every message, including every mouse move. Maps are persistent,
            // so a message that touched nothing pinned is caught by reference equality and costs
            // no allocation at all.
            let loaded = loadedSheet ()
            let pinned = pinnedCanvasOf loaded
            let scrolled = { loaded with ScreenScrollPos = { X = 25.; Y = 60. }; Zoom = 1.75 }
            Expect.isTrue
                (LanguagePrimitives.PhysicalEquality (pinSheet pinned scrolled) scrolled)
                "scrolling and zooming go through untouched"
        }
    ]
