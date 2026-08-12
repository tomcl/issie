/// What BusWidthInferer says when a wire is the wrong width.
///
/// Every component type's happy path is already driven, by ComponentSemantics building one of each
/// through the whole pipeline - a wrong inference there shows up as a wrong output. What nothing
/// drove is the other half of that file: the rule each component type states about the widths it
/// will accept, and the error it gives when a wire breaks it. Those rules are what stands between a
/// mis-wired sheet and a simulator that assumes its inputs are the width they claim.
///
/// Each case below is one specific mis-wiring, with the rule it breaks named beside it. They assert
/// only that the answer is an Error - what the message says is the view's business - and the
/// correct wiring of the same component is asserted to infer, so that a case cannot pass by
/// everything failing.
module WidthInferenceTests

open Expecto
open CommonTypes
open CanvasBuilder

/// One component under test, fed by an Input per input width and feeding an Output per output
/// width - the same shape ComponentSemantics uses, so a case here reads like a case there.
let private canvasOf (compType: ComponentType) (inWidths: int list) (outWidths: int list) : CanvasState =
    let dut = makeComp "dut" (List.length inWidths) (List.length outWidths) compType "DUT"
    let ins = inWidths |> List.mapi (fun i w -> makeComp $"in{i}" 0 1 (Input1(w, None)) $"I{i}")
    let outs = outWidths |> List.mapi (fun i w -> makeComp $"out{i}" 1 0 (Output w) $"O{i}")

    let conns =
        (ins |> List.mapi (fun i c -> conn c 0 dut i))
        @ (outs |> List.mapi (fun i c -> conn dut i c 0))

    dut :: ins @ outs, conns

let private infer compType inWidths outWidths =
    BusWidthInferer.inferConnectionsWidth (canvasOf compType inWidths outWidths)

let private rom =
    { Init = FromData; AddressWidth = 4; WordWidth = 8; Data = Map [ 1I, 7I ]; Comments = None }

/// (name, component, input widths, output widths) - each a mis-wiring that must be refused
let private badWidths: (string * ComponentType * int list * int list) list =
    [ // a gate and its inputs are one bit
      "GateN with a 2-bit input", GateN(And, 2), [ 2; 1 ], [ 1 ]
      "Not with a 2-bit input", Not, [ 2 ], [ 1 ]
      // the n-bit components take their own width on every data port
      "NbitsAnd with mismatched operands", NbitsAnd 4, [ 4; 5 ], [ 4 ]
      "NbitsOr with mismatched operands", NbitsOr 4, [ 4; 5 ], [ 4 ]
      "NbitsXor with mismatched operands", NbitsXor(4, None), [ 4; 5 ], [ 4 ]
      "NbitsNot with the wrong width", NbitsNot 4, [ 5 ], [ 4 ]
      // an adder's carry in is one bit, its operands its own width
      "NbitsAdder with a wide carry in", NbitsAdder 4, [ 2; 4; 4 ], [ 4; 1 ]
      "NbitsAdder with a mismatched operand", NbitsAdder 4, [ 1; 4; 5 ], [ 4; 1 ]
      // a spreader takes one bit
      "NbitSpreader with a wide input", NbitSpreader 4, [ 2 ], [ 4 ]
      // multiplexer data inputs match each other, and the select is as wide as the choice needs
      "Mux2 with mismatched data", Mux2, [ 4; 5; 1 ], [ 4 ]
      "Mux2 with a wide select", Mux2, [ 4; 4; 2 ], [ 4 ]
      "Mux4 with a narrow select", Mux4, [ 4; 4; 4; 4; 1 ], [ 4 ]
      "Demux2 with a wide select", Demux2, [ 4; 2 ], [ 4; 4 ]
      // a split must have something to split: at least one bit above the split point
      "SplitWire with nothing above the split", SplitWire 2, [ 2 ], [ 2; 1 ]
      // a selection must lie inside its input
      "BusSelection reaching past the input", BusSelection(2, 1), [ 2 ], [ 2 ]
      // the shift amount input is the width the component says it is
      "Shift with the wrong shifter width", Shift(4, 2, LSL), [ 4; 3 ], [ 4 ]
      "Shift with a mismatched data input", Shift(4, 2, LSL), [ 5; 2 ], [ 4 ]
      // a comparison is against a bus of the width it names
      "BusCompare with the wrong width", BusCompare1(4, 5I, "5"), [ 5 ], [ 1 ]
      // clocked components take their own width, and their control inputs one bit
      "Register with the wrong width", Register 4, [ 5 ], [ 4 ]
      "RegisterE with a wide enable", RegisterE 4, [ 4; 2 ], [ 4 ]
      "DFF with a wide input", DFF, [ 2 ], [ 1 ]
      "Counter with a wide load input", Counter 4, [ 4; 2; 1 ], [ 4 ]
      // a memory's address bus is as wide as it was given
      "ROM with the wrong address width", AsyncROM1 rom, [ 5 ], [ 8 ] ]

/// The same components wired correctly, so that a refusal above means the rule and not the harness
let private goodWidths: (string * ComponentType * int list * int list) list =
    [ "GateN", GateN(And, 2), [ 1; 1 ], [ 1 ]
      "Not", Not, [ 1 ], [ 1 ]
      "NbitsAnd", NbitsAnd 4, [ 4; 4 ], [ 4 ]
      "NbitsNot", NbitsNot 4, [ 4 ], [ 4 ]
      "NbitsAdder", NbitsAdder 4, [ 1; 4; 4 ], [ 4; 1 ]
      "NbitSpreader", NbitSpreader 4, [ 1 ], [ 4 ]
      "Mux2", Mux2, [ 4; 4; 1 ], [ 4 ]
      "Mux4", Mux4, [ 4; 4; 4; 4; 2 ], [ 4 ]
      "Demux2", Demux2, [ 4; 1 ], [ 4; 4 ]
      "SplitWire", SplitWire 2, [ 4 ], [ 2; 2 ]
      "BusSelection", BusSelection(2, 1), [ 4 ], [ 2 ]
      "Shift", Shift(4, 2, LSL), [ 4; 2 ], [ 4 ]
      "BusCompare", BusCompare1(4, 5I, "5"), [ 4 ], [ 1 ]
      "Register", Register 4, [ 4 ], [ 4 ]
      "RegisterE", RegisterE 4, [ 4; 1 ], [ 4 ]
      "DFF", DFF, [ 1 ], [ 1 ]
      "Counter", Counter 4, [ 4; 1; 1 ], [ 4 ]
      "ROM", AsyncROM1 rom, [ 4 ], [ 8 ] ]

let tests =
    testList "WidthInference" [

        test "a correctly wired component infers its widths" {
            for name, compType, inWidths, outWidths in goodWidths do
                match infer compType inWidths outWidths with
                | Ok _ -> ()
                | Error e -> failtest $"{name} should infer, but was refused: {e.Msg}"
        }

        test "a wire of the wrong width is refused rather than assumed" {
            let wrong =
                badWidths
                |> List.choose (fun (name, compType, inWidths, outWidths) ->
                    match infer compType inWidths outWidths with
                    | Error _ -> None
                    | Ok _ -> Some name)
            Expect.isEmpty wrong "these mis-wirings were accepted"
        }

        test "a refusal says something, and says which connection is at fault" {
            // the message reaches the user and the connection ids are what gets highlighted, so an
            // empty one of either is a refusal that cannot be acted on
            for name, compType, inWidths, outWidths in badWidths do
                match infer compType inWidths outWidths with
                | Ok _ -> () // reported by the test above
                | Error e ->
                    Expect.isGreaterThan e.Msg.Length 20 $"{name} gave a message too short to explain anything"
                    Expect.isNonEmpty e.ConnectionsAffected $"{name} named no connection to highlight"
        }
    ]
