/// Array design sheets: how the copies of such a sheet join to each other, and what ports the
/// sheet therefore has. See CommonTypes.ArrayInfo and ArrayExpand.
///
/// These are pure tests over a canvas plus its array settings - no simulation, no draw block - so
/// they pin the two questions that everything else about the feature is built on: which JoinOut
/// meets which JoinIn, and what that leaves loose.
module ArraySheets

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open ParameterTypes
open CanvasBuilder
open Optics
open DrawModelType
open Optics.Operators

/// A sheet with `copies` copies, whose loop variable is `i`.
let private arrayInfo copies =
    { LoopParam = ParamName "i"; Copies = copies }

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
/// The messages a sheet's problems carry. What each problem POINTS AT is checked separately -
/// see the highlighting test - so that the tests reading text do not all have to know about it.
let private problems info defs canvas =
    ArrayExpand.arrayOutlineOf info defs canvas |> snd |> List.map (fun p -> p.Message)

/// The problems whole, for the tests that care which components a message highlights.
let private problemsFull info defs canvas = ArrayExpand.arrayOutlineOf info defs canvas |> snd

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
    arrayInfo copies, Some (joinNums [ 1, "i+1"; 2, "i" ]), canvas

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
            let info = arrayInfo 4
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
            let w = ArrayExpand.joinsOf (arrayInfo 6) (Some (joinNums [ 1, "i+2"; 2, "i" ])) canvas
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
            let w = ArrayExpand.joinsOf (arrayInfo 3) defs canvas
            Expect.isEmpty w.Problems "two independent chains are not a mistake"
            Expect.equal (List.length w.Matched) 4 "each chain joins two pairs of copies"
            for (o, i) in w.Matched do
                Expect.equal o.Comp.Label i.Comp.Label "a channel never joins one label to another"
        }

        test "one mistake is one message, and it points at the components" {
            // Two Join outs on one channel used to arrive as SEVEN problems: the clash, then one
            // "driven in more than one copy" per copy (each printing the same copy twice), then one
            // derived-name collision per copy. One fault, said four ways, none of them the whole
            // truth. The clash itself is what is reported, and the consequences are not.
            let a = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let b = makeComp 2 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ a; b ], []
            let probs = problemsFull (arrayInfo 3) (Some (joinNums [ 1, "i"; 2, "i" ])) canvas
            Expect.hasLength probs 1 "one mistake, one message"
            Expect.equal (List.head probs).Message "2 Join out components are on channel 0 of 'C'"
                "which says what is wrong and stops"
            Expect.equal (List.sort (List.head probs).Components) (List.sort [ a.Id; b.Id ])
                "and points at both of the components it is about"
        }

        test "a channel that one component drives in several copies says which copies" {
            // the other shape of the same fault, and the one outClashes is now alone in reporting
            let solo = makeComp 1 1 0 (JoinOut(1, 5)) "D"
            let canvas: CanvasState = stacked [ solo ], []
            let probs = problemsFull (arrayInfo 3) None canvas
            Expect.hasLength probs 1 "one component on one channel in three copies is one message"
            Expect.equal (List.head probs).Message "Join out 'D' drives channel 5 in copies 0, 1, 2"
                "naming the copies, which is the whole of what is wrong"
            Expect.equal (List.head probs).Components [ solo.Id ] "and pointing at the component"
        }

        test "a channel number naming anything but the loop variable says so, and names it" {
            // The evaluator's own message reports what is in scope as "properties of this sheet",
            // and the one name in scope for a channel number is the LOOP VARIABLE - which is
            // deliberately not a property. Reported here instead, so the message names the right
            // word and calls it the right thing.
            let jOut = makeComp 1 1 0 (JoinOut(1, 1)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let info = arrayInfo 3
            let defs = Some (joinNums [ 1, "W+1" ])
            match problems info defs canvas with
            | [] -> failtest "a channel number that names a property must be refused"
            | msgs ->
                let msg = List.head msgs
                Expect.stringContains msg "'W'" "the message names what was written"
                Expect.stringContains msg "loop variable 'i'" "and what may be written instead"
                Expect.isFalse (msg.Contains "properties of this sheet")
                    "and does not call the loop variable a property of the sheet, which it is not"
        }

        test "a constant channel number makes every copy drive one wire, and is refused" {
            // no expression, so the stored number is the channel in EVERY copy: n drivers of one
            // wire, which is the mistake this check exists for
            let jOut = makeComp 1 1 0 (JoinOut(1, 5)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 3) None canvas
            Expect.isNonEmpty w.Problems "three copies on one channel must be reported"
            Expect.stringContains (List.head w.Problems).Message "drives channel 5 in copies"
                "and the message must say what is wrong"
        }

        test "a negative channel number is refused wherever it occurs" {
            // i-1 is fine in every copy but the first, and the first is what makes the port name
            // C_out_-1 - which is not a name a label may have
            let jOut = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 4) (Some (joinNums [ 1, "i-1" ])) canvas
            Expect.isNonEmpty w.Problems "a channel number that goes negative must be reported"
            Expect.stringContains (List.head w.Problems).Message "negative" "and say so"
        }

        test "a channel number naming anything but the loop variable is refused" {
            // it must not: which joins are loose decides the sheet's PORTS, so a number that
            // depended on a property would make the port list depend on who instantiated the sheet
            let jOut = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 2) (Some (joinNums [ 1, "WIDTH+1" ])) canvas
            Expect.isNonEmpty w.Problems "a property in a channel number must be reported"
        }

        test "two joins on one side sharing a channel AND a number are refused" {
            // sharing a channel is fine - that is what a channel is - and two joins facing the same
            // way at different numbers are two ports of the copy. Two at the SAME number are one
            // wire driven twice.
            let a = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let b = makeComp 2 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ a; b ], []
            let w = ArrayExpand.joinsOf (arrayInfo 2) None canvas
            Expect.isNonEmpty w.Problems "two Join outs on channel 0 of C must be reported"
            Expect.stringContains (List.head w.Problems).Message "channel 0" "and the message must say which"
        }

        test "two joins on one side sharing only a channel are allowed" {
            // a copy reading two of its neighbours: one channel, two numbers, two ports
            let a = makeComp 1 0 1 (JoinIn(1, 0)) "C"
            let b = makeComp 2 0 1 (JoinIn(1, 1)) "C"
            let canvas: CanvasState = stacked [ a; b ], []
            let defs = Some (joinNums [ 1, "i"; 2, "i+1" ])
            let w = ArrayExpand.joinsOf (arrayInfo 3) defs canvas
            Expect.isEmpty w.Problems "two Join ins at different numbers are two ports, not a clash"
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
                let ins, outs = outline (arrayInfo copies) None canvas
                Expect.equal ins [ "A", 4 ] $"{copies} copies: one input however many copies there are"
                Expect.equal outs [ for i in 0 .. copies - 1 -> $"S_{i}", 4 ]
                    $"{copies} copies: one output port per copy, numbered"
        }

        test "a BusOut is one output as wide as all the copies together" {
            let b = makeComp 1 1 0 (BusOut 3) "SUM"
            let canvas: CanvasState = stacked [ b ], []
            for copies in [ 1; 2; 5; 8 ] do
                let _, outs = outline (arrayInfo copies) None canvas
                Expect.equal outs [ "SUM", 3 * copies ] $"{copies} copies of 3 bits is one {3 * copies}-bit bus"
        }

        test "a MuxOut makes its own multiplexer: a select input and an output" {
            // no declaration list any more - a MuxOut IS a multiplexer, and two of them are two
            let v = makeComp 1 1 0 (MuxOut 6) "PICK"
            let w = makeComp 2 1 0 (MuxOut 6) "ALSO"
            let canvas: CanvasState = stacked [ v; w ], []
            let ins, outs = outline (arrayInfo 5) None canvas
            Expect.equal ins [ "PICK_sel", 3; "ALSO_sel", 3 ]
                "five copies need three select bits, and each MuxOut has its own select"
            Expect.equal outs [ "PICK", 6; "ALSO", 6 ]
                "and each output is as wide as the values it selects between"
        }

        test "a multiplexer select is never zero bits wide" {
            let a = makeComp 1 1 0 (MuxOut 2) "P"
            let canvas: CanvasState = stacked [ a ], []
            for copies, expected in [ 1, 1; 2, 1; 3, 2; 4, 2; 5, 3; 8, 3; 9, 4 ] do
                let ins, _ = outline (arrayInfo copies) None canvas
                Expect.equal ins [ "P_sel", expected ]
                    $"{copies} copies need {expected} select bits - and one copy still needs a select"
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
            let carry = makeComp 4 1 0 (MuxOut 1) "CARRY"
            let sum = makeComp 5 1 0 (BusOut 1) "SUM"
            let cOut = makeComp 6 1 0 (JoinOut(1, 1)) "C"
            let canvas: CanvasState = stacked [ cIn; a; b; carry; sum; cOut ], []
            let defs = Some (joinNums [ 6, "i+1"; 1, "i" ])
            let ins, outs = outline (arrayInfo 8) defs canvas
            Expect.equal (names ins) [ "C_in_0"; "A"; "B"; "CARRY_sel" ]
                "inputs follow the components down the sheet, the MuxOut select among them"
            Expect.equal (names outs) [ "CARRY"; "SUM"; "C_out_8" ]
                "and so do the outputs, each component in the place it is drawn"
            Expect.equal outs [ "CARRY", 1; "SUM", 8; "C_out_8", 1 ]
                "the sum bus is one bit per copy; a carry is one bit wherever it appears"
        }

        test "two components deriving one port name are reported" {
            // S on a three-copy sheet gives S_0 S_1 S_2, and a BusOut called S_1 gives S_1
            let outp = makeComp 1 1 0 (Output 2) "S"
            let clash = makeComp 2 1 0 (BusOut 2) "S_1"
            let canvas: CanvasState = stacked [ outp; clash ], []
            let probs = problems (arrayInfo 3) None canvas
            Expect.isNonEmpty probs "a name derived twice must be reported"
            Expect.stringContains (List.head probs) "S_1" "and the message must name it"
        }

        test "a sheet whose joins are wrong still has ports" {
            // nothing here throws: a sheet is edited through states that do not work out, and its
            // instances still have to be drawn while that is true
            let jOut = makeComp 1 1 0 (JoinOut(1, 5)) "C"
            let inp = makeComp 2 0 1 (Input1(2, None)) "A"
            let canvas: CanvasState = stacked [ jOut; inp ], []
            let ins, outs = outline (arrayInfo 3) None canvas
            Expect.isNonEmpty (problems (arrayInfo 3) None canvas) "the sheet is wrong"
            Expect.equal ins [ "A", 2 ] "and its ordinary input is still a port"
            Expect.equal outs [ "C_out_5", 1 ] "and the join still reads as a loose end"
        }
    ]

//-------------------------------------------------------------------------------------------//
//-------------------------------------THE EXPANSION-----------------------------------------//
//-------------------------------------------------------------------------------------------//

let private maxArraySize = 40

/// A one-bit full adder drawn as an ARRAY DESIGN SHEET, so that n copies of it are an n-bit
/// ripple-carry adder. This is the design the whole feature exists for.
///
///   A, B          8-bit operands, driven to EVERY copy
///   ABIT, BBIT    copy i's operand bits, selected at LSB i - which is the loop variable doing
///                 the one thing that makes one copy differ from the next
///   C             the carry channel: copy i publishes on i+1 and takes i, so it goes to i+1
///   SUM           one bit per copy, concatenated into the array's sum bus
let private rippleSheet (copies: int) =
    let a = makeComp 1 0 1 (Input1(8, None)) "A"
    let b = makeComp 2 0 1 (Input1(8, None)) "B"
    let cIn = makeComp 3 0 1 (JoinIn(1, 0)) "C"
    let aBit = makeComp 4 1 1 (BusSelection(1, 0)) "ABIT"
    let bBit = makeComp 5 1 1 (BusSelection(1, 0)) "BBIT"
    // one bit of sum and a carry out: an adder over one-bit busses is exactly a full adder
    let add = makeComp 6 3 2 (NbitsAdder 1) "FA"
    let sum = makeComp 7 1 0 (BusOut 1) "SUM"
    let cOut = makeComp 8 1 0 (JoinOut(1, 1)) "C"
    let canvas: CanvasState =
        stacked [ a; b; cIn; aBit; bBit; add; sum; cOut ],
        [ conn a 0 aBit 0; conn b 0 bBit 0
          conn cIn 0 add 0; conn aBit 0 add 1; conn bBit 0 add 2
          conn add 0 sum 0; conn add 1 cOut 0 ]
    // the bit each copy takes is its own index; the carry channel numbering makes the chain
    let defs =
        { DefaultBindings = Map.empty
          ParamSlots =
            [ 4, IO "ABIT", "i"; 5, IO "BBIT", "i"; 8, JoinNum, "i+1"; 3, JoinNum, "i" ]
            |> List.map (fun (compId, slot, exprText) ->
                match parseExpression exprText with
                | Ok expr -> {CompId = ComponentId compId; CompSlot = slot}, {Expression = expr; Constraints = []}
                | Error msg -> failwithf $"test expression '{exprText}' does not parse: {msg}")
            |> Map.ofList }
    { makeLdc "ripple" (Some defs) canvas with ArrayInfo = Some (arrayInfo copies) }

/// A parent sheet holding one instance of the array sheet, with the outline ports it derives.
let private rippleParent (copies: int) =
    let sheet = rippleSheet copies
    let ins, outs = outline (arrayInfo copies) sheet.LCParameterSlots sheet.CanvasState
    let a = makeComp 11 0 1 (Input1(8, None)) "X"
    let b = makeComp 12 0 1 (Input1(8, None)) "Y"
    let cin = makeComp 13 0 1 (Input1(1, None)) "CIN"
    let arr = makeComp 14 (List.length ins) (List.length outs) (customOf sheet ins outs None) "ARR"
    let sum = makeComp 15 1 0 (Output copies) "S"
    let cout = makeComp 16 1 0 (Output 1) "COUT"
    // the outline is C_in_0, A, B in and SUM, C_out_n out - the order the components are laid out
    let inIx name = ins |> List.findIndex (fun (l, _) -> l = name)
    let outIx name = outs |> List.findIndex (fun (l, _) -> l = name)
    let canvas: CanvasState =
        stacked [ a; b; cin; arr; sum; cout ],
        [ conn a 0 arr (inIx "A"); conn b 0 arr (inIx "B"); conn cin 0 arr (inIx "C_in_0")
          conn arr (outIx "SUM") sum 0; conn arr (outIx $"C_out_{copies}") cout 0 ]
    makeLdc "top" None canvas, sheet

/// Drive the parent's inputs and read its outputs by label.
let private runParent (parent: LoadedComponent) (deps: LoadedComponent list) (inputVals: Map<string, bigint>) =
    match Simulator.startCircuitSimulation maxArraySize parent.Name parent.CanvasState (parent :: deps) with
    | Error e -> failtestf "%A" e
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun (cid, ComponentLabel label, width) ->
            let fd = NumberHelpers.convertBigintToFastData width inputVals[label]
            FastExtract.changeInput cid (IData fd) 0 simData.FastSim)
        simData.Outputs
        |> List.map (fun (cid, ComponentLabel label, _) ->
            match FastExtract.extractFastSimulationOutput simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
            | IData fd -> label, fd.GetBigInt
            | IAlg _ -> failwith "algebraic output")
        |> Map.ofList

let private expansionTests =
    testList "expansion" [
        test "one array sheet becomes a wrapper and a body, and the body is one copy" {
            let sheet = rippleSheet 4
            let expanded, problems, _ = ArrayElaborate.expandArraySheets [ sheet ]
            Expect.isEmpty problems "a ripple-carry array sheet is a correct design"
            Expect.equal (expanded |> List.map (fun l -> l.Name)) [ "ripple"; "ripple/instance" ]
                "the sheet keeps its name and place, and its body follows it"
            let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
            let body = expanded |> List.find (fun l -> l.Name = "ripple/instance")
            Expect.isNone wrapper.ArrayInfo "after expansion the wrapper is an ordinary sheet"
            Expect.isNone body.ArrayInfo "and so is the body"
            // the body is one copy: the array IO has become ordinary IO, keeping the connections
            Expect.equal (names body.InputLabels) [ "A"; "B"; "C_in_0" ]
                "a Join in becomes an input of the copy, named for its direction"
            Expect.equal (names body.OutputLabels) [ "SUM"; "C_out_1" ]
                "and a BusOut and a Join out become outputs of it"
            Expect.equal (fst body.CanvasState |> List.length) 8 "the body holds what was drawn"
            Expect.equal (snd body.CanvasState) (snd sheet.CanvasState) "with its wires untouched"
        }

        test "the wrapper's own ports are the outline its instances were given" {
            for copies in [ 1; 2; 5; 8 ] do
                let sheet = rippleSheet copies
                let expected = outline (arrayInfo copies) sheet.LCParameterSlots sheet.CanvasState
                let expanded, _, _ = ArrayElaborate.expandArraySheets [ sheet ]
                let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
                // read the ordinary way, off the wrapper's Input1 and Output components in (Y, X)
                // order - which is what everything downstream will do
                Expect.equal (CanvasExtractor.parseDiagramSignature wrapper.CanvasState) expected
                    $"{copies} copies: the wrapper's signature must be the outline, in order"
        }

        test "the wrapper holds one numbered instance of the body per copy" {
            let expanded, _, _ = ArrayElaborate.expandArraySheets [ rippleSheet 5 ]
            let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
            let instances =
                fst wrapper.CanvasState
                |> List.choose (fun c -> match c.Type with | Custom cc -> Some (c.Label, cc.Name) | _ -> None)
            Expect.equal instances [for i in 0 .. 4 -> $"ripple{i}", "ripple/instance"]
                "five numbered instances of the body, which is what the wave selector will show"
        }

        test "one loose channel read by several copies is one port, wired to all of them" {
            // A channel number need not vary copy by copy. `i/2` gives the array one input per PAIR
            // of copies, and a number that does not name the loop variable at all is the extreme of
            // the same thing: every copy loose on one channel. Both are allowed, so the port that
            // channel becomes has to drive EVERY copy on it - keeping one end per channel wired the
            // first copy and left the others' body ports dangling, which surfaced as an unconnected
            // port on the generated sheet, naming components the user cannot open.
            let jIn = makeComp 1 0 1 (JoinIn(1, 0)) "C"
            let outp = makeComp 2 1 0 (Output 1) "R"
            let canvas: CanvasState = stacked [ jIn; outp ], [ conn jIn 0 outp 0 ]
            let info = arrayInfo 4
            let sheet =
                { makeLdc "pairs" (Some (joinNums [ 1, "i/2" ])) canvas with ArrayInfo = Some info }

            let ins, _ = outline info sheet.LCParameterSlots canvas
            Expect.equal (names ins) [ "C_in_0"; "C_in_1" ]
                "four copies reading i/2 are loose on two channels, so the array has two inputs"
            Expect.isEmpty (problems info sheet.LCParameterSlots canvas)
                "and nothing about that is an error: a channel may be read by several copies"

            let expanded, expandProblems, _ = ArrayElaborate.expandArraySheets [ sheet ]
            Expect.isEmpty expandProblems "so the expansion has nothing to complain about either"
            let wrapper = expanded |> List.find (fun l -> l.Name = "pairs")
            let wComps, wConns = wrapper.CanvasState
            let unconnected =
                wComps
                |> List.collect (fun c -> c.InputPorts |> List.map (fun p -> c.Label, p.Id))
                |> List.filter (fun (_, pid) -> wConns |> List.forall (fun cn -> cn.Target.Id <> pid))
            Expect.isEmpty unconnected "every copy on a loose channel is wired to the port it became"

            // and it is the RIGHT copies: C_in_0 feeds copies 0 and 1, C_in_1 copies 2 and 3
            let fedBy (portLabel: string) =
                let src = wComps |> List.find (fun c -> c.Label = portLabel)
                let srcPort = (List.head src.OutputPorts).Id
                wConns
                |> List.filter (fun cn -> cn.Source.Id = srcPort)
                |> List.map (fun cn ->
                    wComps |> List.find (fun c -> c.InputPorts |> List.exists (fun p -> p.Id = cn.Target.Id))
                    |> fun c -> c.Label)
                |> List.sort
            Expect.equal (fedBy "C_in_0") [ "pairs0"; "pairs1" ] "channel 0 is what copies 0 and 1 read"
            Expect.equal (fedBy "C_in_1") [ "pairs2"; "pairs3" ] "and channel 1 what copies 2 and 3 read"
        }

        test "generated ids are above every id the design already uses" {
            // FastCreate indexes arrays by the raw integer id, so a generated one that collided
            // with a real one would make two components one, and a negative one would corrupt the
            // build under Fable without saying so
            let sheet = rippleSheet 4
            let existing =
                fst sheet.CanvasState |> List.map (fun c -> cToInt c.Id) |> List.max
            let expanded, _, _ = ArrayElaborate.expandArraySheets [ sheet ]
            let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
            let ids = fst wrapper.CanvasState |> List.map (fun c -> cToInt c.Id)
            Expect.all ids (fun id -> id > existing) "every generated id is above the design's"
            Expect.equal (List.length (List.distinct ids)) (List.length ids) "and they are distinct"
        }

        test "an array ripple-carry adder adds, at every width" {
            // the test the feature exists for: n copies of a full adder, chained by their carries,
            // against Issie's own n-bit adder over every pair of operands
            for copies in [ 1; 2; 4 ] do
                let parent, sheet = rippleParent copies
                let limit = (1 <<< copies) - 1
                for x in 0 .. limit do
                    for y in 0 .. limit do
                        for cin in 0 .. 1 do
                            let got =
                                runParent parent [ sheet ]
                                    (Map [ "X", bigint x; "Y", bigint y; "CIN", bigint cin ])
                            let total = x + y + cin
                            Expect.equal got["S"] (bigint (total % (1 <<< copies)))
                                $"{copies}-bit array adder: {x} + {y} + {cin} sum"
                            Expect.equal got["COUT"] (bigint (total / (1 <<< copies)))
                                $"{copies}-bit array adder: {x} + {y} + {cin} carry out"
        }

        test "a multiplexer over an Array out selects the copy the index names" {
            // every copy emits its own index, and the multiplexer reads one of them back
            let idx = makeComp 1 0 1 (Constant1(3, 0I, "0")) "I"
            let v = makeComp 2 1 0 (MuxOut 3) "V"
            let canvas: CanvasState = stacked [ idx; v ], [ conn idx 0 v 0 ]
            let defs =
                { DefaultBindings = Map.empty
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "I"},
                          {Expression = (match parseExpression "i" with | Ok e -> e | Error m -> failwith m)
                           Constraints = []} ] }
            let sheet =
                { makeLdc "vals" (Some defs) canvas with ArrayInfo = Some (arrayInfo 5) }
            let sel = makeComp 11 0 1 (Input1(3, None)) "SEL"
            let ins, outs = outline (arrayInfo 5) (Some defs) canvas
            let arr = makeComp 12 (List.length ins) (List.length outs) (customOf sheet ins outs None) "A"
            let out = makeComp 13 1 0 (Output 3) "O"
            let parent =
                makeLdc "top" None
                    (stacked [ sel; arr; out ], [ conn sel 0 arr 0; conn arr 0 out 0 ])
            for i in 0 .. 4 do
                let got = runParent parent [ sheet ] (Map [ "SEL", bigint i ])
                Expect.equal got["O"] (bigint i) $"copy {i} emits {i}, so selecting it reads {i} back"
            // and out of range, which five copies makes reachable with a three-bit select
            for i in 5 .. 7 do
                let got = runParent parent [ sheet ] (Map [ "SEL", bigint i ])
                Expect.equal got["O"] 0I $"select {i} names no copy, so the output is 0"
        }

        test "an array sheet with a parameterised width follows what its instance binds" {
            // the array feature and the property system meeting: the copies' width is a property of
            // the array sheet, so the array's own bus output must widen with it
            let inp = makeComp 1 0 1 (Input1(2, None)) "D"
            let outp = makeComp 2 1 0 (BusOut 2) "Q"
            let canvas: CanvasState = stacked [ inp; outp ], [ conn inp 0 outp 0 ]
            let wExpr = { Expression = PParameter (ParamName "W"); Constraints = [] }
            let defs =
                { DefaultBindings =
                    Map [ ParamName "W", {Expression = PInt 2I; Description = "the width of one copy"} ]
                  ParamSlots =
                    Map [ {CompId = ComponentId 1; CompSlot = IO "D"}, wExpr
                          {CompId = ComponentId 2; CompSlot = IO "Q"}, wExpr ] }
            let sheet = { makeLdc "wide" (Some defs) canvas with ArrayInfo = Some (arrayInfo 3) }
            for w in [ 2; 5 ] do
                let bindings = Map [ ParamName "W", PInt (bigint w) ]
                let resolved =
                    ComponentSlots.resolveCanvasAtBindings bindings defs.ParamSlots canvas
                let ins, outs = outline (arrayInfo 3) (Some defs) resolved
                Expect.equal ins [ "D", w ] $"W={w}: the broadcast input is one copy wide"
                Expect.equal outs [ "Q", 3 * w ] $"W={w}: the bus output is three copies wide"
                let src = makeComp 11 0 1 (Input1(w, None)) "IN"
                let arr = makeComp 12 1 1 (customOf sheet ins outs (Some bindings)) "A"
                let out = makeComp 13 1 0 (Output (3 * w)) "O"
                let parent =
                    makeLdc "top" None
                        (stacked [ src; arr; out ], [ conn src 0 arr 0; conn arr 0 out 0 ])
                // every copy sees the same value, so the bus is that value repeated three times
                let value = bigint ((1 <<< w) - 1)
                let got = runParent parent [ sheet ] (Map [ "IN", value ])
                let expected = value + (value <<< w) + (value <<< (2 * w))
                Expect.equal got["O"] expected $"W={w}: three copies of a broadcast value"
        }

        test "what crosses to the sidecar is the expanded design, and no array component" {
            // The boundary. The renderer has already turned the project into the circuit it
            // simulates, and hands the sidecar THAT - so the far end never sees an array component
            // and needs to know nothing about them.
            //
            // It also settles a hazard that had nothing to do with any of the bugs: expansion MINTS
            // component ids, and SimSetInputs names a component by id across the wire. Two
            // expansions agreeing was luck. Only one of the two expands now, so what the step panel
            // sets an input on is the very component the sidecar holds.
            let parent, sheet = rippleParent 3
            match Simulator.startCircuitSimulation maxArraySize parent.Name parent.CanvasState [ parent; sheet ] with
            | Error e -> failtestf "%A" e.ErrType
            | Ok sd ->
                let design = Simulator.designForSidecar sd.FastSim
                let onTheWire = design.Sheets |> List.map (fun s -> s.SheetName)
                Expect.contains onTheWire (ArrayElaborate.bodyNameOf sheet.Name)
                    "the body sheet crosses, which is what says the design was expanded first"
                for s in design.Sheets do
                    for c in s.Components do
                        match c.TypeS with
                        | BusOut _ | MuxOut _ | JoinOut _ | JoinIn _ ->
                            failtestf $"'{s.SheetName}' put %A{c.TypeS} on the wire: the far end has                                         no way to read it, and must never need one"
                        | _ -> ()
                // and it still simulates, to the same answers, at the far end
                let shimmed = SimpleDesignShim.designToLoadedComponents design
                let top = shimmed |> List.find (fun l -> l.Name = design.TopSheet)
                let inputs = Map [ "X", 5I; "Y", 3I; "CIN", 0I ]
                let want = runParent parent [ sheet ] inputs
                let got = runParent top (shimmed |> List.filter (fun l -> l.Name <> top.Name)) inputs
                Expect.equal got["S"] want["S"] "the sidecar's copy must give the answers the renderer's does"
                Expect.equal got["COUT"] want["COUT"] "carry out too"
        }

        test "a simulation of an array design does not report itself edited the moment it starts" {
            // What put the Refresh button up and left it there. The question "is the design still
            // what the project holds" was asked of the sheets as SIMULATED - and for an array
            // design those are a wrapper and a body, neither of which the project has - so the
            // answer was no, always, for every simulation of every design containing an array.
            //
            // Asked for both tops, because the two go wrong differently: with the parent on top the
            // array is a dependency whose canvas is not the project's, and with the array itself on
            // top it is also the OPEN sheet, whose ports are read a second way.
            let parent, sheet = rippleParent 3
            for top in [ parent; sheet ] do
                let project: CommonTypes.Project =
                    { ProjectPath = ""
                      OpenFileName = top.Name
                      WorkingFileName = Some top.Name
                      LoadedComponents = [ parent; sheet ] }
                match Simulator.startCircuitSimulation maxArraySize top.Name top.CanvasState [ parent; sheet ] with
                | Error e -> failtestf "%A" e.ErrType
                | Ok sd ->
                    Expect.isTrue
                        (FastExtract.compareLoadedStates sd.FastSim top.CanvasState (Some project))
                        $"simulating '{top.Name}' must not look like an edit to it"
        }

        test "each copy is shown with the channel numbers it is really joined by" {
            // A join's body port carries the channel the SHEET is drawn at, which is copy 0's - so
            // every copy, being an instance of that one sheet, would read C_in_0. That says nothing
            // about which copy's output feeds which copy's input, and it is the numbers that say
            // it: copy k takes channel k and gives channel k+1, so the name of a copy's carry in
            // is the name of the previous copy's carry out.
            let _, sheet = rippleParent 4
            let _, _, copyNames = ArrayElaborate.expandArraySheets [ sheet ]
            let byCopy =
                copyNames
                |> Map.toList
                |> List.sortBy fst
                |> List.map (fun (_, (ins, outs)) -> names ins, names outs)
            Expect.equal (List.length byCopy) 4 "one set of names per copy"
            for copy, (ins, outs) in List.indexed byCopy do
                Expect.contains ins $"C_in_{copy}" $"copy {copy} takes channel {copy}"
                Expect.contains outs $"C_out_{copy + 1}" $"and gives channel {copy + 1}"
                // the ports that are not joins are the same on every copy
                Expect.contains ins "A" "a broadcast input is the same port in every copy"
            // and the labels on the CANVAS are untouched, because they are what wires a copy to
            // its body - FastCreate.indexOf finds a subsheet's IO by label and width
            let expanded, _, _ = ArrayElaborate.expandArraySheets [ sheet ]
            let wrapper = expanded |> List.find (fun l -> l.Name = sheet.Name)
            let copyOnCanvas =
                fst wrapper.CanvasState
                |> List.pick (fun c -> match c.Type with | Custom ct -> Some ct | _ -> None)
            Expect.equal (names copyOnCanvas.InputLabels) [ "A"; "B"; "C_in_0" ]
                "the canvas keeps the body's labels, whichever copy it is"
        }

        test "a big array offers its copies' ports inside them, not all at once" {
            // Below the threshold the copies' ports are listed flat on the array, which is the
            // quickest way to compare one copy with another. Above it that list is the wrong shape
            // - it grows with the copies, and the bound on those is 1024 - so the ports move inside
            // the copy, reachable one at a time through the combo box that chooses which.
            //
            // The two are one rule read from its two ends, and what is checked is that exactly one
            // end offers them: not both, which is the same signal twice, and not neither.
            //
            // A broadcast rather than the ripple chain, because this needs to run at 65 copies and
            // a ripple's operand bus would have to be 65 bits wide to be sliced that far.
            let arrayOf copies =
                let inp = makeComp 1 0 1 (Input1(1, None)) "D"
                let outp = makeComp 2 1 0 (BusOut 1) "Q"
                { makeLdc "wide" None (stacked [ inp; outp ], [ conn inp 0 outp 0 ]) with
                    ArrayInfo = Some (arrayInfo copies) }
            let portsOf (fs: FastSimulation) instance = (PortView.ofInstance fs instance).ViewPorts |> List.length
            let below = ArrayElaborate.Constants.copiesShownFlattened
            for copies in [ below; below + 1 ] do
                let sheet = arrayOf copies
                match Simulator.startCircuitSimulation maxArraySize sheet.Name sheet.CanvasState [ sheet ] with
                | Error e -> failtestf "%i copies: %A" copies e.ErrType
                | Ok sd ->
                    let fs = sd.FastSim
                    let copyInstances =
                        fs.Design.InstancesInside (InstancePath [], ArrayElaborate.bodyNameOf sheet.Name)
                    Expect.equal (List.length copyInstances) copies $"{copies} copies of the body"
                    let onTheArray = portsOf fs (InstancePath [])
                    let insideACopy = portsOf fs (List.head copyInstances)
                    if copies <= below then
                        Expect.equal insideACopy 0
                            $"{copies} copies: a copy's own IO is offered on the array, so not here as well"
                        Expect.isGreaterThan onTheArray copies
                            $"{copies} copies: and the array offers a port for each of them"
                    else
                        Expect.isGreaterThan insideACopy 0
                            $"{copies} copies: too many to list, so a copy's ports are offered inside it"
                        Expect.isLessThan onTheArray copies
                            $"{copies} copies: and no longer all at once on the array"
        }

        test "a wave on a copy leads back to the component drawn on the array's sheet" {
            // The waveform viewer's "show me this" button goes to the component a wave comes from.
            // For a wave on one of the copies that component is one the EXPANSION made - a custom
            // component on a sheet nobody drew - so there was no symbol to go to and the button did
            // nothing at all. What the user means by it is the join that port is, which is drawn on
            // the array component's own sheet.
            let parent, sheet = rippleParent 3
            match Simulator.startCircuitSimulation maxArraySize parent.Name parent.CanvasState [ parent; sheet ] with
            | Error e -> failtestf "%A" e.ErrType
            | Ok sd ->
                let fs = sd.FastSim
                let arrayInstance =
                    fs.Design.InstancesInside (InstancePath [], sheet.Name) |> List.exactlyOne
                // every port the array itself offers - which at three copies is the copies' ports
                let ports = (PortView.ofInstance fs arrayInstance).ViewPorts
                let onCopies =
                    ports
                    |> List.filter (fun p ->
                        match fs.Design.ComponentOfInstance (p.PortComp, (let (InstancePath ap) = arrayInstance in ap)) with
                        | Some { Type = Custom _ } -> true
                        | _ -> false)
                Expect.isNonEmpty onCopies "three copies, so the array offers their ports"

                let drawnOn = fst sheet.CanvasState |> List.map (fun c -> c.Id) |> Set.ofList
                for p in onCopies do
                    let waveId = PortView.waveIndexOf arrayInstance p
                    let drawn = PortView.drawnComponentOf fs.Design waveId
                    Expect.isTrue (Set.contains drawn drawnOn)
                        $"a wave on a copy's port must lead to a component drawn on '{sheet.Name}',                           or the button has nowhere to go"
                    Expect.notEqual drawn p.PortComp
                        "and not to the copy itself, which is on a sheet nobody drew"
        }

        test "an array component simulates when it is itself the sheet being simulated" {
            // What someone does first: draw the array component, then press simulate while looking
            // at it, before ever placing an instance. Everything above drives an array through a
            // PARENT sheet, so nothing held the top-sheet case to producing right ANSWERS - only,
            // since the previous commit, to building at all.
            let _, sheet = rippleParent 3
            let got = runParent sheet [] (Map [ "A", 5I; "B", 3I; "C_in_0", 0I ])
            Expect.equal got["SUM"] 0I "5 + 3 is 8: bit 0 of each sum is 0, and three of those is 0"
            Expect.equal got["C_out_3"] 1I "and the top carry is 1"
        }
    ]

//-------------------------------------------------------------------------------------------//
//--------------------------------------THE REFUSALS-----------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Simulate a design and give back whatever it complained about.
let private simError (top: LoadedComponent) (deps: LoadedComponent list) =
    match Simulator.startCircuitSimulation maxArraySize top.Name top.CanvasState (top :: deps) with
    | Ok _ -> None
    | Error e -> Some (SimGraphTypes.errMsg e.ErrType)

let private refusalTests =
    testList "refusals" [
        test "array IO on a sheet that is not an array sheet is refused, and is named" {
            // the state a paste onto an ordinary sheet leaves, and the state a sheet is in when its
            // array settings are taken away while its array components are still on it
            let inp = makeComp 1 0 1 (Input1(1, None)) "A"
            let bad = makeComp 2 1 0 (BusOut 1) "B"
            let top = makeLdc "plain" None (stacked [ inp; bad ], [ conn inp 0 bad 0 ])
            match simError top [] with
            | None -> failtest "array IO on an ordinary sheet must be refused"
            | Some msg ->
                Expect.stringContains msg "'B'" "the message names the component"
                Expect.stringContains msg "array component" "and what it needs to be on"
        }

        test "the refusal names every component that is wrong, and points at them" {
            // several at once is the ordinary case: a paste, or a sheet losing its array settings,
            // brings all of them together. The message says WHICH, and the ids are what the
            // simulator highlights in red - a message about 'B' and 'C' is no use if the sheet does
            // not show which two those are.
            let inp = makeComp 1 0 1 (Input1(1, None)) "A"
            let one = makeComp 2 1 0 (BusOut 1) "B"
            let two = makeComp 3 1 0 (JoinOut (1, 0)) "C"
            let top = makeLdc "plain" None (stacked [ inp; one; two ], [ conn inp 0 one 0; conn inp 0 two 0 ])
            match Simulator.startCircuitSimulation maxArraySize top.Name top.CanvasState [ top ] with
            | Ok _ -> failtest "array IO on an ordinary sheet must be refused"
            | Error e ->
                let msg = SimGraphTypes.errMsg e.ErrType
                Expect.stringContains msg "'B'" "both components are named"
                Expect.stringContains msg "'C'" "both components are named"
                Expect.equal (List.sort e.ComponentsAffected) (List.sort [ one.Id; two.Id ])
                    "and both are highlighted, so the message can be read beside them"
        }

        test "the same components are fine on an array sheet, which is the point" {
            let parent, sheet = rippleParent 2
            Expect.isNone (simError parent [ sheet ]) "an array sheet's own IO is not an error on it"
        }

        test "an array component can be checked while it is the sheet being looked at" {
            // Through validateSheetOfDesign, which is what the Simulation tab's verdict actually
            // calls, and THAT is what this pins. getStateAndDependencies takes the sheet being
            // checked out of the list it hands back, so the check used to pass that shortened list
            // on - leaving nothing able to say the open sheet was an array component. The checks
            // then ran on its unexpanded canvas and refused its own IO as being on a sheet that is
            // not an array component, advising the user to make it the thing it already was.
            //
            // Asked the way the caller asks it, because the earlier version of this test passed the
            // sheet INSIDE the dependency list and so passed while the app was still doing that.
            let _, sheet = rippleParent 2
            let err =
                Simulator.validateSheetOfDesign sheet.Name [ sheet ]
                |> function
                   | Ok _ -> None
                   | Error e -> Some (SimGraphTypes.errMsg e.ErrType)
            Expect.isNone err "checking an array component must expand it, as simulating it does"
        }

        test "a broken array sheet is refused when the design uses it" {
            let parent, sheet = rippleParent 2
            let broken = { sheet with ArrayInfo = Some (arrayInfo 0) }
            match simError parent [ broken ] with
            | None -> failtest "a copy count that makes no sense must be refused"
            | Some msg -> Expect.stringContains msg "at least 1" "and the message must say the bound"
        }

        test "a broken array sheet elsewhere in the project does not stop an unrelated design" {
            // the rule every other kind of error in a sheet already follows: only the sheets the
            // design reaches are checked, so one being worked on cannot block another being run
            let inp = makeComp 1 0 1 (Input1(1, None)) "A"
            let outp = makeComp 2 1 0 (Output 1) "B"
            let plain = makeLdc "plain" None (stacked [ inp; outp ], [ conn inp 0 outp 0 ])
            let broken = { rippleSheet 2 with ArrayInfo = Some (arrayInfo 0) }
            Expect.isNone (simError plain [ broken ])
                "a design that instantiates nothing is not affected by a sheet it does not use"
        }

        test "an array sheet can be simulated while it is open" {
            // the first thing anyone will do with one, so it must give the array's own hardware
            // rather than an error about the sheet not being a design
            let sheet = rippleSheet 4
            match Simulator.startCircuitSimulation maxArraySize sheet.Name sheet.CanvasState [ sheet ] with
            | Error e -> failtestf "simulating an array sheet directly: %A" (SimGraphTypes.errMsg e.ErrType)
            | Ok simData ->
                // as sets: getSimulationIOs builds its lists with cons, so what it gives back is
                // the canvas order reversed rather than the sheet's port order. That the wrapper's
                // PORTS come out in outline order is pinned in the expansion tests, which read them
                // the way every sheet's are read.
                let ins = simData.Inputs |> List.map (fun (_, ComponentLabel l, _) -> l) |> Set.ofList
                let outs = simData.Outputs |> List.map (fun (_, ComponentLabel l, _) -> l) |> Set.ofList
                Expect.equal ins (Set [ "A"; "B"; "C_in_0" ]) "its inputs are the array's own"
                Expect.equal outs (Set [ "SUM"; "C_out_4" ]) "and so are its outputs"
        }
    ]

//-------------------------------------------------------------------------------------------//
//----------------------------------INSTANCES ELSEWHERE--------------------------------------//
//-------------------------------------------------------------------------------------------//

let private instanceTests =
    testList "instances" [
        // Changing the copy count changes how many ports the array sheet has, so every component
        // made from it elsewhere is then out of date. That is the one edit in the feature reaching
        // beyond its own sheet, and it must go through the machinery a sheet's ports changing
        // already has rather than needing one of its own.
        test "changing the copy count makes instances of the array sheet out of date" {
            let parent, sheet = rippleParent 4
            let instance = fst parent.CanvasState |> List.find (fun c -> c.Label = "ARR")
            let cc = match instance.Type with | Custom cc -> cc | t -> failtestf "%A" t

            /// The instance record CustomCompPorts works on: what the instance stores against what
            /// its own bindings give it, which is the whole definition of out of date.
            let against (sheets: LoadedComponent list) =
                let expected =
                    CanvasExtractor.signatureOfInstance sheets Map.empty cc.Name Map.empty
                    |> Option.defaultWith (fun () -> failtest "no signature for the array instance")
                ({ Sheet = parent.Name
                   CompId = instance.Id
                   Label = instance.Label
                   Old = (cc.InputLabels, cc.OutputLabels)
                   Expected = expected }: CustomCompPorts.Instance)

            Expect.isFalse (CustomCompPorts.instanceIsOutOfDate (against [ parent; sheet ]))
                "an instance placed at the sheet's own copy count is up to date"

            // eight copies rather than four: the sum bus doubles and the carry out is on a
            // different channel, so the instance's ports are no longer the sheet's
            let widened = { sheet with ArrayInfo = Some (arrayInfo 8) }
            Expect.isTrue (CustomCompPorts.instanceIsOutOfDate (against [ parent; widened ]))
                "and is out of date once the sheet has a different number of copies"
        }

        test "an array sheet's stored ports are its outline, so the project agrees about them" {
            // LoadedComponent.InputLabels is what the rest of Issie reads; on an array sheet it
            // must be the derived outline and not the sheet's Input1 and Output components
            let sheet = rippleSheet 4
            let expected = outline (arrayInfo 4) sheet.LCParameterSlots sheet.CanvasState
            let stored =
                CanvasExtractor.parseDiagramSignatureFor
                    sheet.ArrayInfo sheet.LCParameterSlots sheet.CanvasState
            Expect.equal stored expected "the sheet's ports are its outline"
            Expect.notEqual stored (CanvasExtractor.parseDiagramSignature sheet.CanvasState)
                "which is not what reading its Input1 and Output components would give"
        }
    ]

//-------------------------------------------------------------------------------------------//
//---------------------------------------THE SYMBOLS-----------------------------------------//
//-------------------------------------------------------------------------------------------//

let private symbolTests =
    testList "symbols" [
        // The bug this pins: changeNumberOfBitsf ended in a wildcard, so a width typed into
        // Properties for one of these was accepted and silently thrown away. Every component whose
        // width the pane can edit has to be in that match, and a wildcard cannot say so.
        test "the width of every array IO component can be changed" {
            let cases =
                [ BusOut 1, BusOut 7
                  MuxOut 1, MuxOut 7
                  JoinOut (1, 3), JoinOut (7, 3)
                  JoinIn (1, 3), JoinIn (7, 3) ]
            for before, after in cases do
                Expect.equal (SymbolReplaceHelpers.withNumberOfBits 7 before) after
                    $"a width change must reach %A{before}, and must leave its channel alone"
        }

        test "a join says its channel and its width, and only once" {
            // both were drawn before: the legend and the separate bus-width text sat on top of one
            // another. The width is in the legend now, and the direction is not - the port side
            // already says which way it goes.
            Expect.equal (Symbol.getComponentLegend None (JoinIn (1, 3)) Degree0) "Join[3]"
                "a one-bit join needs no bit range, as no other one-bit port does"
            Expect.equal (Symbol.getComponentLegend None (JoinOut (8, 12)) Degree0) "Join[12] (7:0)"
                "and a wider one says its range"
            Expect.equal (Symbol.getComponentLegend None (JoinIn (8, 12)) Degree0)
                (Symbol.getComponentLegend None (JoinOut (8, 12)) Degree0)
                "the two directions read the same: which side the port is on says which it is"
            Expect.equal (Symbol.getComponentLegend None (BusOut 4) Degree0) "BusOut (3:0)"
                "a bus output says its per-copy width"
        }

        test "a join draws the channel EXPRESSION it is on, and an Output its loop variable" {
            // What a copy is joined by is the expression, not what that expression comes to in copy
            // 0: `Join[1]` is copy 0's answer and says nothing about which copy feeds which, while
            // `Join[i+1]` against `Join[i]` is the chain itself. The text is on the SYMBOL because a
            // Component carries no expression - a join's channel is a parameter slot of the sheet.
            let jOut = makeComp 1 1 0 (JoinOut(1, 1)) "C"
            let jIn = makeComp 2 0 1 (JoinIn(1, 0)) "C"
            let plain = makeComp 3 0 1 (JoinIn(1, 7)) "D"
            let outp = makeComp 4 1 0 (Output 8) "SUM"
            let info = arrayInfo 4
            let slots = (joinNums [ 1, "i+1"; 2, "i" ]).ParamSlots
            let synced =
                makeSymbolModel [ jOut; jIn; plain; outp ]
                |> SymbolUpdate.syncArrayText (Some info) slots
            let textOf (comp: Component) = synced.Symbols[comp.Id].ArrayText

            Expect.equal (textOf jOut) (Some "i+1") "a Join out draws the expression its slot holds"
            Expect.equal (textOf jIn) (Some "i") "and so does the Join in it meets"
            Expect.equal (textOf plain) None
                "a join with no slot has only the number it is drawn at, and draws that"
            Expect.equal (textOf outp) (Some "i")
                "an Output is one port per copy, so it draws the loop variable it belongs to"

            // what those come out as on the symbol
            Expect.equal (Symbol.getComponentLegend (textOf jOut) jOut.Type Degree0) "Join[i+1]"
                "the chain reads as the chain, not as what it comes to in copy 0"
            Expect.equal (Symbol.getComponentLegend (textOf plain) plain.Type Degree0) "Join[7]"
                "and a channel that is a plain number still draws that number"

            // taking the array settings away takes the annotations with them
            let plainSheet = synced |> SymbolUpdate.syncArrayText None slots
            Expect.all (plainSheet.Symbols |> Map.toList |> List.map (fun (_, sym) -> sym.ArrayText))
                Option.isNone
                "on a sheet that is not an array component nothing draws a loop variable"
        }

        test "an array IO symbol is wide enough for its legend" {
            // the legend grows with the channel number and the width, so the symbol has to
            for compType in [ JoinIn (1, 0); JoinIn (8, 1234); JoinOut (64, 999); BusOut 128; MuxOut 1 ] do
                let _, _, _, w = Symbol.getComponentProperties compType "X"
                // ONE line, and measured in the style SymbolView draws it in
                let text =
                    DrawHelpers.getTextWidthInPixels Symbol.arrayLegendStyle
                        (Symbol.getComponentLegend None compType Degree0)
                // the chevron is the last fifth and carries no text
                Expect.isGreaterThan (w * 0.8) text $"%A{compType}: the legend must fit on one line"
        }

        test "a join is one size at every width anyone is likely to type" {
            // The width is in the legend, so it changes how much text there is to fit. A symbol
            // that resized under every keystroke in the properties box would jump about while
            // being read, so joins are floored at the widest legend expected of one.
            let widthOf w = let _, _, _, sw = Symbol.getComponentProperties (JoinIn (w, 3)) "X" in sw
            let common = [ 1; 2; 4; 8; 16 ] |> List.map widthOf
            Expect.equal (List.distinct common) [ List.head common ]
                "the everyday widths must all give one size, or the symbol moves as a width is typed"
            Expect.isGreaterThan (widthOf 1 |> int) (3 * Symbol.Constants.gridSize)
                "and that size is not the bare minimum a component may be"
        }

        test "a join too long for that size does grow" {
            // the other half of the floor: it is a floor and not a fixed size, so a legend that
            // really is longer than the expected worst case still fits
            let widthOf ct = let _, _, _, w = Symbol.getComponentProperties ct "X" in w
            Expect.isGreaterThan (widthOf (JoinIn (1024, 123456))) (widthOf (JoinIn (1, 3)))
                "a symbol whose text has outgrown it is what nobody notices until they see it drawn"
        }

        test "changing an array IO width resizes the symbol that holds it" {
            // the whole chain the properties box runs: type in, symbol out. Reported as broken
            // because the legend used to be split over two lines, so the symbol was sized by its
            // FIRST line and the bit range - the only part that changes - never reached the size.
            let narrow = Symbol.createNewSymbol [] {X=0.;Y=0.} (JoinIn (1, 3)) "J1" DrawModelType.SymbolT.ThemeType.Colourful
            let wide =
                narrow
                |> Optic.set (DrawModelType.SymbolT.component_ >-> CommonTypes.type_) (JoinIn (1024, 123456))
                |> SymbolReplaceHelpers.resizedForLegend
            Expect.isGreaterThan wide.Component.W narrow.Component.W
                "a wider join needs a longer symbol, and nothing else will give it one"
        }

        test "array IO is not drawn in the colour of ordinary IO" {
            // it IS io, but it is io that means something different from the port beside it - a
            // BusOut is n ports WIDE where the Output next to it is n ports - so the colour should
            // not say they are the same thing
            let colourOf ct = Symbol.getSymbolColour ct false SymbolT.ThemeType.Colourful
            let ordinary = [ Output 1; Input1 (1, None); Constant1 (1, 0I, "0") ] |> List.map colourOf
            for ct in [ BusOut 1; MuxOut 1; JoinOut (1, 0); JoinIn (1, 0) ] do
                Expect.isFalse (List.contains (colourOf ct) ordinary)
                    $"%A{ct} must not be the colour of an ordinary IO component"
                Expect.equal (colourOf ct) (colourOf (BusOut 1))
                    $"%A{ct} is one of a family and is coloured as one"
        }

        test "a select is as many bits as it takes to index the copies, and never none" {
            for copies, expected in [ 1, 1; 2, 1; 3, 2; 4, 2; 5, 3; 8, 3; 9, 4; 1024, 10 ] do
                Expect.equal (ArrayExpand.arraySelectWidth copies) expected
                    $"{copies} copies need {expected} select bits"
        }
    ]

let tests =
    testList "ArraySheets" [ joinTests; outlineTests; expansionTests; refusalTests; instanceTests; symbolTests ]
