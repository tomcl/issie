﻿module TruthTableCreate
open CommonTypes
open TimeHelpers
open SimGraphTypes
open SimTypes
open TruthTableTypes
open SynchronousUtils
open NumberHelpers
open FastExtract
open Helpers
open Fable.Core.JsInterop
importSideEffects "katex/dist/katex.min.css"
let katex: obj = importDefault "katex"

/// Wraps SimulationIO and Viewer types in the CellIO DU Type
let toCellIO simIOs viewers =
    (List.map (fun io -> SimIO io) simIOs
    @
    List.map (fun ((l,f),w,_) -> Viewer ((l,f),w)) viewers)

/// Converts a Truth Table from a Mapping to a list of its rows.
/// Note that separation of inputs and outputs is lost.
let tableAsList tMap =
    tMap
    |> Map.toList
    |> List.map (fun (lhs,rhs) -> List.append lhs rhs)

let product (seq1:'a seq) (seq2:'a seq seq) =
    seq { for item1 in seq1 do
              for item2 in seq2 do
                  yield item1 |> Seq.singleton |> Seq.append item2 }

/// Find the Cartesian product of n sets, implemented with Sequences
let productn (s:seq<#seq<'a>>) =
    s |> Seq.fold (fun r s -> r |> product s) (seq { yield Seq.empty })

let getConstraintsOnIO io constraints =
    let newEqu = 
            constraints.Equalities
            |> List.filter (fun e -> e.IO = io)
    let newIneq =
        constraints.Inequalities
        |> List.filter (fun i -> i.IO = io)
    {Equalities = newEqu; Inequalities = newIneq}

let constrainedValuesandLength {Equalities = equ; Inequalities = ineq} =
    let equValues =
        equ
        |> Seq.map (fun con -> con.Value)
    let ineqValues, ineqLength =
        ((Seq.empty,0), ineq)
        ||> List.fold (fun (seqAcc,count) con ->
            let values = Seq.init con.Range (fun x -> x + con.LowerBound)
            Seq.append values seqAcc, count+con.Range)
    Seq.append equValues ineqValues, equ.Length + ineqLength

/// Find all LHS rows of the Truth Table, limited by input constraints and bit limit
let tableLHS (inputs: SimulationIO list) (ttt: ModelType.TTType) :
    TruthTableRow list * int =

    // Maximum number of rows on LHS of Truth Table.
    // Limited for speed and memory consumption reasons.
    let rowLimit = int(2.0**ttt.BitLimit)
    
    // Find all input values for a given input.
    // Implemented using Sequences, which are lazily evaluated.
    let inputValuesSeq count (io: SimulationIO) =
        let (_,_,w) = io
        let seqLength = int (2.0**w)
        match getConstraintsOnIO (SimIO io) ttt.InputConstraints with
        | {Equalities = []; Inequalities = []} ->
            Seq.init seqLength id, count*seqLength
        | conSet->
            constrainedValuesandLength conSet
            |> fun (vals,seqLength) -> vals,count*seqLength

    // Partition the inputs into algebraic and numeric
    let algebraInputs, numericInputs =
        inputs
        |> List.partition (fun io -> List.contains io ttt.AlgebraIns)

    let numericVals, constrainedRowCount =
        (1,numericInputs)
        ||> List.mapFold inputValuesSeq

    let numericRows =
        numericVals
        |> productn
        |> Seq.truncate rowLimit
        |> Seq.map (fun l -> 
            l 
            |> Seq.mapi (fun i n ->
                let (_,_,w) = numericInputs[i]
                {IO = SimIO (numericInputs[i]); Data = Bits (convertIntToWireData w (bigint n))})
            |> Seq.toList)
        |> Seq.toList
    
    let algebraRow =
        algebraInputs
        |> List.map (fun io -> 
            let (_,label,_) = io
            {IO = SimIO io; Data = Algebra (SingleTerm io)})
    
    if numericInputs.IsEmpty then
        // All inputs are algebraic, so only one row in table with all algebra
        [algebraRow],1
    else
        // Append algebra to each numeric row to get full LHS
        numericRows
        |> (List.map (fun r -> algebraRow@r)), constrainedRowCount

    

/// Feeds the given input combination (LHS Row) to the table's Fast Simulation and extracts the
/// simulated output combination (RHS Row).
// NOTE: This function mutates the table's Fast Simulation data structure.
let simulateInputCombination 
        (rowLHS: TruthTableRow) 
        (outputs: SimulationIO list)  
        (simData: SimulationData)
        : TruthTableRow =
    
    // Change input values in the table's FastSimulation (THESE ARE MUTABLE!)
    rowLHS
    |> List.map (fun cell ->
        match cell.IO, cell.Data with
        | SimIO (cid,_,_), Bits wd ->
            (cid,IData (convertWireDataToFastData wd))
        | SimIO io, Algebra exp ->
            let (cid,_,_) = io
            (cid,IAlg (SingleTerm io))
        | x, y ->
            failwithf "what? CellData from input rows has IO: %A, and Data: %A." x y)
    |> FastExtract.changeInputBatch simData.ClockTickNumber simData.FastSim

    // Extract Output and Viewer values from the Fast Simulation
    let outputRow =
        (outputs,simData)
        ||> FastExtract.extractFastSimulationIOsFData
        |> List.map (fun (comp,out) -> 
            match out with
            | IData wd -> {IO = SimIO comp; Data = Bits (convertFastDataToWireData wd)}
            | IAlg exp -> {IO = SimIO comp; Data = Algebra exp})
    let viewerRow =
        FastExtract.extractViewers simData
        |> List.map (fun ((l,f),w,fs) ->
            match fs with
            | IData wd -> {IO = Viewer ((l,f),w); Data = Bits (convertFastDataToWireData wd)}
            | IAlg exp -> {IO = Viewer ((l,f),w); Data = Algebra exp})
    outputRow @ viewerRow

/// Create a Truth Table from Simulation Data, taking into account 
/// algebraic inputs and input constriants.
let truthTable 
    (simData: SimulationData) // Simulation Data for sheet
    (ttType: ModelType.TTType) // style data for truth table
    (isRegeneration: bool) // Is this function call regeneration (true) or first time (false)
    : TruthTable =
    let start = TimeHelpers.getTimeMs()

    // Create a new SimData separate from the one used by the Step Simulator
    let tableSimData =
        if isRegeneration then 
            simData
        else
            match FastRun.buildFastSimulationFData 2 ""  simData.Graph with
            | Ok fs -> 
                {simData with FastSim = fs}
            | _ -> 
                failwithf "Error in building fast simulation for Truth Table evaluation"
    let inputs = List.map fst (FastExtract.extractFastSimulationIOsFData simData.Inputs tableSimData)
    let outputs = List.map fst (FastExtract.extractFastSimulationIOsFData simData.Outputs tableSimData)
    let viewers = FastExtract.extractViewers simData
    let lhs,tCRC = tableLHS inputs ttType
    let rhs = List.map (fun i -> simulateInputCombination i outputs tableSimData) lhs

    List.zip lhs rhs
    |> Map.ofList
    |> (fun tableMap ->
        let listRep = tableAsList tableMap
        {
            TableMap = tableMap
            FilteredMap = tableMap
            DCMap = None
            SortedListRep = listRep
            IsTruncated = (tableMap.Count <> tCRC)
            HasRedundancies = false
            MaxRowsWithConstraints = tCRC
            TableSimData = tableSimData
            IOOrder = toCellIO (inputs@outputs) viewers
        })
    |> fun table ->
        if table.IsTruncated || ttType.AlgebraIns.Length > 0 then
            table
        else
            let hasRed = TruthTableReduce.hasRedundancies table
            {table with HasRedundancies = hasRed}
    |> TimeHelpers.instrumentInterval "truthTableGeneration" start

