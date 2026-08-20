/// Memories whose address and data widths are parameters.
///
/// A memory is the one component with two widths and with contents that have to fit them, and the
/// two facts meet badly: parameterise a width and one component becomes several memories, one per
/// set of bindings its sheet is used at, all sharing one map of initial data. So "does the data fit"
/// is a question with one answer per instance, and the three places that ask it - the editor, the
/// data source, and the simulator - are what these tests cover.
module MemoryParameters

open Expecto
open CommonTypes
open ParameterTypes
open CanvasBuilder

let private mem addressWidth wordWidth data =
    { Init = FromData
      AddressWidth = addressWidth
      WordWidth = wordWidth
      Data = Map.ofList data
      Comments = None }

/// A parameter declaration. Descriptions are compulsory but say nothing these tests depend on.
let private declares (name: string) (expr: ParamExpression) =
    ParamName name, { Expression = expr; Description = $"test parameter {name}" }

let tests =
    testList "MemoryParameters" [

        // --- the slots ---

        test "both widths of every memory component can be set from a parameter" {
            let widths (compType: ComponentType) =
                match compType with
                | ROM1 m | RAM1 m | AsyncROM1 m | AsyncRAM1 m -> m.AddressWidth, m.WordWidth
                | c -> failtest $"not a memory: {c}"
            [ AsyncROM1 (mem 4 8 []); ROM1 (mem 4 8 []); RAM1 (mem 4 8 []); AsyncRAM1 (mem 4 8 []) ]
            |> List.iter (fun compType ->
                let addressed = ComponentSlots.trySetSlotValue MemoryAddressWidth 6I compType
                let worded = ComponentSlots.trySetSlotValue MemoryWordWidth 16I compType
                Expect.equal (Option.map widths addressed) (Some (6, 8)) $"address width of {compType}"
                Expect.equal (Option.map widths worded) (Some (4, 16)) $"data width of {compType}"
                Expect.isTrue (ComponentSlots.slotApplies MemoryAddressWidth compType) "the slot applies"
                Expect.isTrue (ComponentSlots.slotApplies MemoryWordWidth compType) "as does the other")
        }

        test "a memory width slot means nothing on a component that is not a memory" {
            Expect.isNone (ComponentSlots.trySetSlotValue MemoryAddressWidth 6I (Register 8))
                "a register has one width, and it is Buswidth"
            Expect.isNone (ComponentSlots.trySetSlotValue MemoryWordWidth 6I (Input1 (8, None)))
                "nor is an input's width a memory's"
            Expect.isFalse (ComponentSlots.slotApplies MemoryWordWidth (Register 8)) "so the slot does not apply"
        }

        test "setting a width leaves the contents exactly as they were" {
            // the deliberate choice: a memory passing through a size that does not hold its data is
            // a step on the way to one that does, and the data is the only copy of what was typed
            let full = RAM1 (mem 4 8 [ 15I, 200I ])
            match ComponentSlots.trySetSlotValue MemoryAddressWidth 2I full with
            | Some (RAM1 m) ->
                Expect.equal m.AddressWidth 2 "the width is the new one"
                Expect.equal m.Data (Map [ 15I, 200I ]) "and the contents are untouched, though they no longer fit"
            | other -> failtest $"expected a RAM, got {other}"
        }

        test "both widths are bounded like any other bus width, and say which is which" {
            let bounds (constraints: ParamConstraint list) =
                constraints
                |> List.choose (function
                    | MinVal (PInt v, _) -> Some ("min", v)
                    | MaxVal (PInt v, _) -> Some ("max", v)
                    | _ -> None)
            let messages (constraints: ParamConstraint list) =
                constraints |> List.map (function MinVal (_, m) | MaxVal (_, m) -> m)
            let maxWidth = bigint CommonTypes.Constants.maxIssieBusWidth
            let addressBounds = ComponentSlots.constraintsFor MemoryAddressWidth (RAM1 (mem 4 8 []))
            Expect.equal (bounds addressBounds) [ "min", 1I; "max", maxWidth ] "an address is a bus"
            Expect.isTrue (messages addressBounds |> List.forall (fun m -> m.Contains "Address width"))
                "and a message names which of the two widths it is about"
            let wordBounds = ComponentSlots.constraintsFor MemoryWordWidth (RAM1 (mem 4 8 []))
            Expect.isTrue (messages wordBounds |> List.forall (fun m -> m.Contains "Word width"))
                "as does the other"
        }

        // --- what fits ---

        test "contents fit a memory when every address and every word is inside it" {
            Expect.isNone (MemoryData.memoryProblem (mem 4 8 [ 15I, 255I; 0I, 0I ]))
                "the largest address and the largest word of a 4x8 memory"
            Expect.isSome (MemoryData.memoryProblem (mem 4 8 [ 16I, 0I ]))
                "one past the last location"
            Expect.isSome (MemoryData.memoryProblem (mem 4 8 [ 0I, 256I ]))
                "one past the largest word"
            Expect.isNone (MemoryData.memoryProblem (mem 40 40 [ (1I <<< 39), (1I <<< 39) ]))
                "widths past 32 bits, where a uint32 mask would have wrapped"
        }

        test "the problem names the location and says how many others there are" {
            match MemoryData.dataProblem 4 8 (Map [ 20I, 0I; 30I, 0I ]) with
            | None -> failtest "expected a problem"
            | Some problem ->
                Expect.stringContains problem "20" "the lowest offending address is the one named"
                Expect.stringContains problem "1 other" "and the rest are counted"
        }

        test "data has to fit every shape the memory has, not just the one it is drawn at" {
            let data = Map [ 12I, 200I ]
            Expect.isNone (MemoryData.dataProblemAtWidths [ 4, 8 ] data) "fits a 4x8 memory"
            Expect.isSome (MemoryData.dataProblemAtWidths [ 4, 8; 3, 8 ] data)
                "and does not fit the same memory where an instance makes it 3 address bits"
            Expect.isSome (MemoryData.dataProblemAtWidths [ 4, 8; 4, 4 ] data)
                "nor where an instance makes the words narrower"
            match MemoryData.dataProblemAtWidths [ 4, 8; 3, 8 ] data with
            | Some problem -> Expect.stringContains problem "instance" "the message says an instance is at fault"
            | None -> failtest "expected a problem"
        }

        // --- the shapes one memory has across a design ---

        test "a memory on a sheet used at two sizes has two shapes" {
            // child: a ROM whose address width is the sheet's parameter W
            let rom = makeComp 1 1 1 (ROM1 (mem 4 8 [])) "ROM"
            let addr = makeComp 2 0 1 (Input1 (4, None)) "ADDR"
            let dout = makeComp 3 1 0 (Output 8) "DOUT"
            let canvas = [ rom; addr; dout ], [ conn addr 0 rom 0; conn rom 0 dout 0 ]
            let wExpr = { Expression = PParameter (ParamName "W"); Constraints = [] }
            let paramDefs =
                { DefaultBindings = Map [ declares "W" (PInt 4I) ]
                  ParamSlots =
                    Map [ { CompId = 1; CompSlot = MemoryAddressWidth }, wExpr
                          { CompId = 2; CompSlot = IO "ADDR" }, wExpr ] }
            let child = makeLdc "mchild" (Some paramDefs) canvas
            let instance (id: int) (w: int) =
                makeComp id 1 1
                    (customOf child [ "ADDR", w ] [ "DOUT", 8 ]
                        (Some (Map [ ParamName "W", PInt (bigint w) ])))
                    $"I{id}"
            let parentOf (instances: Component list) =
                makeLdc "mparent" None (instances, [])

            let widthsUnder (parent: LoadedComponent) =
                ParameterAnalysis.memoryWidthsInDesign [ parent; child ] "mchild" 1 (mem 4 8 [])

            Expect.equal (widthsUnder (parentOf [ instance 1 4 ])) [ 4, 8 ]
                "one instance, one shape - the data width is not parameterised, so it is the stored one"
            Expect.equal (widthsUnder (parentOf [ instance 1 4; instance 2 3 ]) |> List.sort)
                [ 3, 8; 4, 8 ]
                "two instances at different values give the memory two shapes"
            Expect.equal (widthsUnder (parentOf [ instance 1 4; instance 2 4 ])) [ 4, 8 ]
                "two instances at the same value give one"
            Expect.equal (ParameterAnalysis.memoryWidthsInDesign [ child ] "mchild" 1 (mem 4 8 []))
                [ 4, 8 ]
                "and a sheet nothing instantiates has the shape its own declared values give it"
        }

        test "a memory whose widths are not parameters has one shape wherever it is used" {
            let rom = makeComp 1 1 1 (ROM1 (mem 5 16 [])) "ROM"
            let sheet = makeLdc "plain" None ([ rom ], [])
            Expect.equal (ParameterAnalysis.memoryWidthsInDesign [ sheet ] "plain" 1 (mem 5 16 []))
                [ 5, 16 ]
                "the widths the component carries, which is the only answer there is"
        }
    ]
