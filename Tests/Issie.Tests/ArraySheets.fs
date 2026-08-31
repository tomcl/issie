/// Array design sheets: how the copies of such a sheet join to each other, and what ports the
/// sheet therefore has. See CommonTypes.ArrayInfo and ArrayExpand.
///
/// These are pure tests over a canvas plus its array settings - no simulation, no draw block - so
/// they pin the two questions that everything else about the feature is built on: which JoinOut
/// meets which JoinIn, and what that leaves loose.
module ArraySheets

open Expecto
open CommonTypes
open ParameterTypes
open CanvasBuilder

/// A sheet with `copies` copies, whose loop variable is `i`.
let private arrayInfo copies muxes =
    { LoopParam = ParamName "i"; EndValue = copies - 1; Muxes = muxes }

/// Parameter definitions holding one JoinNum expression per named component.
/// The loop variable is deliberately NOT in DefaultBindings: it is not a declared property of the
/// sheet, it is named by the array settings and has a value only inside a copy.
let private joinNums (entries: (int * string) list) : ParameterDefs =
    { DefaultBindings = Map.empty
      ParamSlots =
        entries
        |> List.map (fun (compId, exprText) ->
            match parseExpression exprText with
            | Ok expr -> {CompId = ComponentId compId; CompSlot = JoinNum}, {Expression = expr; Constraints = []}
            | Error msg -> failwithf $"test expression '{exprText}' does not parse: {msg}")
        |> Map.ofList }

/// Lay components out down the sheet in the order given, since the (Y, X) order of the components
/// is the order of the ports they generate.
let private stacked (comps: Component list) =
    comps |> List.mapi (fun i comp -> { comp with Y = float (i * 100) })

/// The port names an outline has, in order.
let private names (ports: (string * int) list) = ports |> List.map fst

let private outline info defs canvas = ArrayExpand.arrayOutlineOf info defs canvas |> fst
let private problems info defs canvas = ArrayExpand.arrayOutlineOf info defs canvas |> snd

//-------------------------------------------------------------------------------------------//
//------------------------------------JOIN MATCHING------------------------------------------//
//-------------------------------------------------------------------------------------------//

/// A carry chain: copy a publishes on channel a+1, copy b takes channel b, so they meet when
/// b = a+1. Copy 0's Join in is on channel 0, which nothing publishes, and copy n-1's Join out is
/// on channel n, which nothing takes: those two become the array's carry in and carry out.
let private carryChain copies =
    let jOut = makeComp 1 1 0 (JoinOut(1, 1)) "C"
    let jIn = makeComp 2 0 1 (JoinIn(1, 0)) "C"
    let canvas: CanvasState = stacked [ jIn; jOut ], []
    arrayInfo copies [], Some (joinNums [ 1, "i+1"; 2, "i" ]), canvas

let private joinTests =
    testList "joins" [
        test "a forward chain joins each copy to the next and leaves the two ends loose" {
            for copies in [ 1; 2; 5; 8 ] do
                let info, defs, canvas = carryChain copies
                let w = ArrayExpand.joinsOf info defs canvas
                Expect.isEmpty w.Problems $"{copies} copies: nothing should be wrong with a carry chain"
                Expect.equal (List.length w.Matched) (copies - 1)
                    $"{copies} copies: every copy but the last drives the next"
                // each match is copy a's Join out driving copy a+1's Join in
                for (o, i) in w.Matched do
                    Expect.equal i.Copy (o.Copy + 1) "a carry goes to the NEXT copy"
                Expect.equal (w.UnmatchedIn |> List.map (fun e -> e.Copy, e.Num)) [ 0, 0 ]
                    $"{copies} copies: only copy 0's Join in is unsupplied"
                Expect.equal (w.UnmatchedOut |> List.map (fun e -> e.Copy, e.Num)) [ copies - 1, copies ]
                    $"{copies} copies: only the last copy's Join out is untaken"
        }

        test "a backward chain is the same chain with the numbers shifted up" {
            // copy a publishes on channel a and copy b takes channel b+1, so a = b+1: copy a
            // drives copy a-1. Written this way round rather than as i-1 because a channel number
            // may never be negative - that is what keeps every derived port name a valid label.
            let jOut = makeComp 1 1 0 (JoinOut(1, 0)) "B"
            let jIn = makeComp 2 0 1 (JoinIn(1, 1)) "B"
            let canvas: CanvasState = stacked [ jIn; jOut ], []
            let info = arrayInfo 4 []
            let defs = Some (joinNums [ 1, "i"; 2, "i+1" ])
            let w = ArrayExpand.joinsOf info defs canvas
            Expect.isEmpty w.Problems "a backward chain is an ordinary chain"
            for (o, i) in w.Matched do
                Expect.equal i.Copy (o.Copy - 1) "a backward chain goes to the PREVIOUS copy"
            Expect.equal (w.UnmatchedOut |> List.map (fun e -> e.Copy)) [ 0 ]
                "copy 0 has nowhere to send to, so its Join out is the array's output"
            Expect.equal (w.UnmatchedIn |> List.map (fun e -> e.Copy)) [ 3 ]
                "the last copy has nothing to take from, so its Join in is the array's input"
        }

        test "a skip chain leaves two loose ends at each end" {
            // copy a publishes on a+2 and copy b takes b, so copy a drives copy a+2: two separate
            // chains through the array, and so two loose ends at each end of it
            let jOut = makeComp 1 1 0 (JoinOut(1, 2)) "S"
            let jIn = makeComp 2 0 1 (JoinIn(1, 0)) "S"
            let canvas: CanvasState = stacked [ jIn; jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 6 []) (Some (joinNums [ 1, "i+2"; 2, "i" ])) canvas
            Expect.isEmpty w.Problems "a skip chain is an ordinary chain"
            Expect.equal (List.length w.Matched) 4 "six copies, skipping two, leaves four joins"
            Expect.equal (w.UnmatchedIn |> List.map (fun e -> e.Copy)) [ 0; 1 ]
                "the first two copies have nothing to take from"
            Expect.equal (w.UnmatchedOut |> List.map (fun e -> e.Copy)) [ 4; 5 ]
                "and the last two have nowhere to send to"
        }

        test "two channels of the same shape do not join to each other" {
            // both chains are numbered i+1 against i; only the LABEL keeps them apart, which is
            // what makes a join's label a channel name rather than a component name
            let cOut = makeComp 1 1 0 (JoinOut(1, 1)) "C"
            let cIn = makeComp 2 0 1 (JoinIn(1, 0)) "C"
            let dOut = makeComp 3 4 0 (JoinOut(4, 1)) "D"
            let dIn = makeComp 4 0 1 (JoinIn(4, 0)) "D"
            let canvas: CanvasState = stacked [ cIn; dIn; cOut; dOut ], []
            let defs = Some (joinNums [ 1, "i+1"; 2, "i"; 3, "i+1"; 4, "i" ])
            let w = ArrayExpand.joinsOf (arrayInfo 3 []) defs canvas
            Expect.isEmpty w.Problems "two independent chains are not a mistake"
            Expect.equal (List.length w.Matched) 4 "each chain joins two pairs of copies"
            for (o, i) in w.Matched do
                Expect.equal o.Comp.Label i.Comp.Label "a channel never joins one label to another"
        }

        test "a constant channel number makes every copy drive one wire, and is refused" {
            // no expression, so the stored number is the channel in EVERY copy: n drivers of one
            // wire, which is the mistake this check exists for
            let jOut = makeComp 1 1 0 (JoinOut(1, 5)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 3 []) None canvas
            Expect.isNonEmpty w.Problems "three copies on one channel must be reported"
            Expect.stringContains (List.head w.Problems) "more than one copy"
                "and the message must say what is wrong"
        }

        test "a negative channel number is refused wherever it occurs" {
            // i-1 is fine in every copy but the first, and the first is what makes the port name
            // C_out_-1 - which is not a name a label may have
            let jOut = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 4 []) (Some (joinNums [ 1, "i-1" ])) canvas
            Expect.isNonEmpty w.Problems "a channel number that goes negative must be reported"
            Expect.stringContains (List.head w.Problems) "negative" "and say so"
        }

        test "a channel number naming anything but the loop variable is refused" {
            // it must not: which joins are loose decides the sheet's PORTS, so a number that
            // depended on a property would make the port list depend on who instantiated the sheet
            let jOut = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 2 []) (Some (joinNums [ 1, "WIDTH+1" ])) canvas
            Expect.isNonEmpty w.Problems "a property in a channel number must be reported"
        }

        test "two joins on one side sharing a label are refused" {
            // a JoinOut and a JoinIn share a label - that is the channel - but two of a kind are
            // two ports of the copy with one name
            let a = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let b = makeComp 2 1 0 (JoinOut(1, 1)) "C"
            let canvas: CanvasState = stacked [ a; b ], []
            let w = ArrayExpand.joinsOf (arrayInfo 2 []) None canvas
            Expect.isNonEmpty w.Problems "two Join outs called C must be reported"
            Expect.stringContains (List.head w.Problems) "different names" "and say what to do"
        }
    ]

//-------------------------------------------------------------------------------------------//
//---------------------------------------THE OUTLINE-----------------------------------------//
//-------------------------------------------------------------------------------------------//

let private outlineTests =
    testList "outline" [
        test "an ordinary Input goes to every copy and an ordinary Output gives one port per copy" {
            let inp = makeComp 1 0 1 (Input1(4, None)) "A"
            let outp = makeComp 2 1 0 (Output 4) "S"
            let canvas: CanvasState = stacked [ inp; outp ], []
            for copies in [ 1; 2; 5; 8 ] do
                let ins, outs = outline (arrayInfo copies []) None canvas
                Expect.equal ins [ "A", 4 ] $"{copies} copies: one input however many copies there are"
                Expect.equal outs [ for i in 0 .. copies - 1 -> $"S_{i}", 4 ]
                    $"{copies} copies: one output port per copy, numbered"
        }

        test "a BusOut is one output as wide as all the copies together" {
            let b = makeComp 1 1 0 (BusOut 3) "SUM"
            let canvas: CanvasState = stacked [ b ], []
            for copies in [ 1; 2; 5; 8 ] do
                let _, outs = outline (arrayInfo copies []) None canvas
                Expect.equal outs [ "SUM", 3 * copies ] $"{copies} copies of 3 bits is one {3 * copies}-bit bus"
        }

        test "a declared multiplexer adds a select input and an output, and an ArrayOut adds none" {
            let a = makeComp 1 1 0 (ArrayOut 6) "V"
            let canvas: CanvasState = stacked [ a ], []
            // no multiplexer: the ArrayOut contributes nothing at all
            let ins, outs = outline (arrayInfo 5 []) None canvas
            Expect.isEmpty ins "an Array out is not an input"
            Expect.isEmpty outs "and contributes no output of its own"
            // one multiplexer over it, and then a second, independent one
            let muxes = [ {MuxSource = "V"; MuxName = "PICK"}; {MuxSource = "V"; MuxName = "ALSO"} ]
            let ins, outs = outline (arrayInfo 5 muxes) None canvas
            Expect.equal ins [ "PICK_sel", 3; "ALSO_sel", 3 ]
                "five copies need three select bits, and each multiplexer has its own select"
            Expect.equal outs [ "PICK", 6; "ALSO", 6 ]
                "each multiplexer's output is as wide as the values it selects between"
        }

        test "a multiplexer's select is never zero bits wide" {
            let a = makeComp 1 1 0 (ArrayOut 2) "V"
            let canvas: CanvasState = stacked [ a ], []
            let muxes = [ {MuxSource = "V"; MuxName = "P"} ]
            for copies, expected in [ 1, 1; 2, 1; 3, 2; 4, 2; 5, 3; 8, 3; 9, 4 ] do
                let ins, _ = outline (arrayInfo copies muxes) None canvas
                Expect.equal ins [ "P_sel", expected ]
                    $"{copies} copies need {expected} select bits - and one copy still needs a select"
        }

        test "a multiplexer over an Array out that is not there is reported" {
            let canvas: CanvasState = stacked [ makeComp 1 0 1 (Input1(1, None)) "A" ], []
            let muxes = [ {MuxSource = "GONE"; MuxName = "P"} ]
            let probs = problems (arrayInfo 4 muxes) None canvas
            Expect.isNonEmpty probs "a multiplexer must say what it selects between"
            Expect.stringContains (List.head probs) "GONE" "and the message must name it"
        }

        test "loose join ends are the array's own inputs and outputs" {
            let info, defs, canvas = carryChain 8
            let ins, outs = outline info defs canvas
            Expect.equal ins [ "C_in_0", 1 ] "copy 0's carry in is the array's"
            Expect.equal outs [ "C_out_8", 1 ] "and the last copy's carry out is the array's"
        }

        test "the ports of a whole array sheet come in the order the components are laid out" {
            // a ripple-carry adder's outline: two operand busses in, a carry in, the sum bus out,
            // a carry out, and a multiplexer over the per-copy carries
            let cIn = makeComp 1 0 1 (JoinIn(1, 0)) "C"
            let a = makeComp 2 0 1 (Input1(8, None)) "A"
            let b = makeComp 3 0 1 (Input1(8, None)) "B"
            let carry = makeComp 4 1 0 (ArrayOut 1) "CARRY"
            let sum = makeComp 5 1 0 (BusOut 1) "SUM"
            let cOut = makeComp 6 1 0 (JoinOut(1, 1)) "C"
            let canvas: CanvasState = stacked [ cIn; a; b; carry; sum; cOut ], []
            let muxes = [ {MuxSource = "CARRY"; MuxName = "CBIT"} ]
            let defs = Some (joinNums [ 6, "i+1"; 1, "i" ])
            let ins, outs = outline (arrayInfo 8 muxes) defs canvas
            Expect.equal (names ins) [ "C_in_0"; "A"; "B"; "CBIT_sel" ]
                "inputs follow the components down the sheet, with the multiplexer selects last"
            Expect.equal (names outs) [ "SUM"; "C_out_8"; "CBIT" ]
                "outputs likewise - the Array out gives none, and the multiplexer's comes last"
            Expect.equal outs [ "SUM", 8; "C_out_8", 1; "CBIT", 1 ]
                "the sum bus is one bit per copy; a carry is one bit wherever it appears"
        }

        test "two components deriving one port name are reported" {
            // S on a three-copy sheet gives S_0 S_1 S_2, and a BusOut called S_1 gives S_1
            let outp = makeComp 1 1 0 (Output 2) "S"
            let clash = makeComp 2 1 0 (BusOut 2) "S_1"
            let canvas: CanvasState = stacked [ outp; clash ], []
            let probs = problems (arrayInfo 3 []) None canvas
            Expect.isNonEmpty probs "a name derived twice must be reported"
            Expect.stringContains (List.head probs) "S_1" "and the message must name it"
        }

        test "a sheet whose joins are wrong still has ports" {
            // nothing here throws: a sheet is edited through states that do not work out, and its
            // instances still have to be drawn while that is true
            let jOut = makeComp 1 1 0 (JoinOut(1, 5)) "C"
            let inp = makeComp 2 0 1 (Input1(2, None)) "A"
            let canvas: CanvasState = stacked [ jOut; inp ], []
            let ins, outs = outline (arrayInfo 3 []) None canvas
            Expect.isNonEmpty (problems (arrayInfo 3 []) None canvas) "the sheet is wrong"
            Expect.equal ins [ "A", 2 ] "and its ordinary input is still a port"
            Expect.equal outs [ "C_out_5", 1 ] "and the join still reads as a loose end"
        }
    ]

let tests = testList "ArraySheets" [ joinTests; outlineTests ]
