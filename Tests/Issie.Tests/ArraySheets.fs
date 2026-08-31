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

        test "a constant channel number makes every copy drive one wire, and is refused" {
            // no expression, so the stored number is the channel in EVERY copy: n drivers of one
            // wire, which is the mistake this check exists for
            let jOut = makeComp 1 1 0 (JoinOut(1, 5)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 3) None canvas
            Expect.isNonEmpty w.Problems "three copies on one channel must be reported"
            Expect.stringContains (List.head w.Problems) "more than one copy"
                "and the message must say what is wrong"
        }

        test "a negative channel number is refused wherever it occurs" {
            // i-1 is fine in every copy but the first, and the first is what makes the port name
            // C_out_-1 - which is not a name a label may have
            let jOut = makeComp 1 1 0 (JoinOut(1, 0)) "C"
            let canvas: CanvasState = stacked [ jOut ], []
            let w = ArrayExpand.joinsOf (arrayInfo 4) (Some (joinNums [ 1, "i-1" ])) canvas
            Expect.isNonEmpty w.Problems "a channel number that goes negative must be reported"
            Expect.stringContains (List.head w.Problems) "negative" "and say so"
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
            Expect.stringContains (List.head w.Problems) "channel 0" "and the message must say which"
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
            let expanded, problems = ArrayElaborate.expandArraySheets [ sheet ]
            Expect.isEmpty problems "a ripple-carry array sheet is a correct design"
            Expect.equal (expanded |> List.map (fun l -> l.Name)) [ "ripple"; "ripple/copy" ]
                "the sheet keeps its name and place, and its body follows it"
            let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
            let body = expanded |> List.find (fun l -> l.Name = "ripple/copy")
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
                let expanded, _ = ArrayElaborate.expandArraySheets [ sheet ]
                let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
                // read the ordinary way, off the wrapper's Input1 and Output components in (Y, X)
                // order - which is what everything downstream will do
                Expect.equal (CanvasExtractor.parseDiagramSignature wrapper.CanvasState) expected
                    $"{copies} copies: the wrapper's signature must be the outline, in order"
        }

        test "the wrapper holds one numbered instance of the body per copy" {
            let expanded, _ = ArrayElaborate.expandArraySheets [ rippleSheet 5 ]
            let wrapper = expanded |> List.find (fun l -> l.Name = "ripple")
            let instances =
                fst wrapper.CanvasState
                |> List.choose (fun c -> match c.Type with | Custom cc -> Some (c.Label, cc.Name) | _ -> None)
            Expect.equal instances [for i in 0 .. 4 -> $"ripple{i}", "ripple/copy"]
                "five numbered instances of the body, which is what the wave selector will show"
        }

        test "generated ids are above every id the design already uses" {
            // FastCreate indexes arrays by the raw integer id, so a generated one that collided
            // with a real one would make two components one, and a negative one would corrupt the
            // build under Fable without saying so
            let sheet = rippleSheet 4
            let existing =
                fst sheet.CanvasState |> List.map (fun c -> cToInt c.Id) |> List.max
            let expanded, _ = ArrayElaborate.expandArraySheets [ sheet ]
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
        test "array IO on a sheet that is not an array sheet is refused, and says what to do" {
            // the state a paste onto an ordinary sheet leaves, and the state a sheet is in when its
            // array settings are taken away while its array components are still on it
            let inp = makeComp 1 0 1 (Input1(1, None)) "A"
            let bad = makeComp 2 1 0 (BusOut 1) "B"
            let top = makeLdc "plain" None (stacked [ inp; bad ], [ conn inp 0 bad 0 ])
            match simError top [] with
            | None -> failtest "array IO on an ordinary sheet must be refused"
            | Some msg ->
                Expect.stringContains msg "'B'" "the message names the component"
                Expect.stringContains msg "ARRAY DESIGN SHEET" "and says what kind of sheet it needs"
                Expect.stringContains msg "right-click" "and where to make one"
        }

        test "the same components are fine on an array sheet, which is the point" {
            let parent, sheet = rippleParent 2
            Expect.isNone (simError parent [ sheet ]) "an array sheet's own IO is not an error on it"
        }

        test "a broken array sheet is refused when the design uses it" {
            let parent, sheet = rippleParent 2
            let broken = { sheet with ArrayInfo = Some (arrayInfo 0) }
            match simError parent [ broken ] with
            | None -> failtest "a copy count that makes no sense must be refused"
            | Some msg -> Expect.stringContains msg "at least one" "and the message must say why"
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
            Expect.equal (Symbol.getComponentLegend (JoinIn (1, 3)) Degree0) "Join[3]"
                "a one-bit join needs no bit range, as no other one-bit port does"
            Expect.equal (Symbol.getComponentLegend (JoinOut (8, 12)) Degree0) "Join[12].(7:0)"
                "and a wider one says its range"
            Expect.equal (Symbol.getComponentLegend (JoinIn (8, 12)) Degree0)
                (Symbol.getComponentLegend (JoinOut (8, 12)) Degree0)
                "the two directions read the same: which side the port is on says which it is"
            Expect.equal (Symbol.getComponentLegend (BusOut 4) Degree0) "BusOut.(3:0)"
                "a bus output says its per-copy width"
        }

        test "an array IO symbol is wide enough for its legend" {
            // the legend grows with the channel number and the width, so the symbol has to
            for compType in [ JoinIn (1, 0); JoinIn (8, 1234); JoinOut (64, 999); BusOut 128; MuxOut 1 ] do
                let _, _, _, w = Symbol.getComponentProperties compType "X"
                // per LINE: a legend with a . in it is drawn over two, and bold
                let text =
                    (Symbol.getComponentLegend compType Degree0).Split '.'
                    |> Array.map (DrawHelpers.getTextWidthInPixels
                                    {Symbol.Constants.componentLabelStyle with FontSize = "14px"; FontWeight = "bold"})
                    |> Array.max
                // the chevron is the last fifth and carries no text
                Expect.isGreaterThan (w * 0.8) text $"%A{compType}: the legend must fit on one line"
        }

        test "a select is as many bits as it takes to index the copies, and never none" {
            for copies, expected in [ 1, 1; 2, 1; 3, 2; 4, 2; 5, 3; 8, 3; 9, 4; 1024, 10 ] do
                Expect.equal (ArrayExpand.arraySelectWidth copies) expected
                    $"{copies} copies need {expected} select bits"
        }
    ]

let tests =
    testList "ArraySheets" [ joinTests; outlineTests; expansionTests; refusalTests; instanceTests; symbolTests ]
