/// Persistence tests: the .dgm save path must produce JSON that the load path can
/// read back, and a serialisation failure must surface as an Error rather than as
/// placeholder text that would overwrite the sheet file.
module PersistenceTests

open Expecto
open CommonTypes
open Helpers.JsonHelpers
open CanvasBuilder

let private sheetInfo: SheetInfo =
    { Form = Some User; Description = None; ParameterDefinitions = None }

let tests =
    testList "Persistence" [
        test "stateToJsonString round-trips a canvas through jsonStringToState" {
            let inComp = makeComp "in0" 0 1 (Input1(3, None)) "I0"
            let dut = makeComp "not" 1 1 (NbitsNot 3) "N1"
            let outComp = makeComp "out0" 1 0 (Output 3) "O0"
            let canvas: CanvasState =
                [ inComp; dut; outComp ], [ conn inComp 0 dut 0; conn dut 0 outComp 0 ]
            match stateToJsonString (canvas, None, Some sheetInfo) with
            | Error e -> failtest $"serialisation failed: {e}"
            | Ok json ->
                match jsonStringToState json with
                | Error e -> failtest $"parse of freshly saved JSON failed: {e}"
                | Ok saved ->
                    let jsonComps, conns = saved.getCanvas
                    let comps = List.map convertFromJSONComponent jsonComps
                    Expect.equal (List.length conns) 2 "connection count"
                    Expect.equal
                        (comps |> List.map (fun c -> c.Id, c.Type) |> List.sort)
                        ([ "in0", Input1(3, None); "not", NbitsNot 3; "out0", Output 3 ] |> List.sort)
                        "component ids and types survive the round trip"
        }
    ]
