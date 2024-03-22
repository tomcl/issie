module SheetBeautifyD2

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open BusWireRoute
open System
open System.Diagnostics
let symbolModel_ = SheetT.symbol_

let inline mapValues (map:Map<'a,'b>) = map |> Map.toArray |> Array.map snd 

let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2

/// constants used by SheetBeautify
module Constants =
    ()
// D2. sheetOrderFlip 
// Adjust on sheet: Port order on custom components, flip components, flip MUX input order
// Primary Optimisation: Reduce wire crossings 
// Test using random sample inputsReduction in wire crossings, other quality measures

// Build Function: 
//      - try through all possible combinations of flipping components
//      - find the one with the least wire crossings
//      - apply that combination to the model and return the new model

// script to describe a symbol
type symbolScript = {
    Flipped: SymbolT.FlipType option
    ReversedInput: bool
    PortOrder: Map<Edge, string list>
}

// script to describe a model
type modelScript = list<ComponentId * symbolScript>



let rec combinations list =
    match list with
    | [] -> [[]] // The only permutation of an empty list is a list containing an empty list
    | _ -> 
        list |> List.collect (fun head ->
            let tail = list |> List.filter (fun x -> x <> head)
            combinations tail |> List.map (fun perm -> head :: perm))

let rec cartesianProduct lists =
        match lists with
        | [] -> [[]]
        | head::tail ->
            let rec combineWithAll x list =
                match list with
                | [] -> []
                | head::tail -> (x::head) :: combineWithAll x tail
            let tailProduct = cartesianProduct tail
            head |> List.collect (fun x -> combineWithAll x tailProduct)

let generatePortOrderCombs (symbol: SymbolT.Symbol) =
    match symbol.Component.Type with
    | Custom _ -> 
        let edgePermutations = 
            symbol.PortMaps.Order
            |> Map.map (fun _ orderList -> combinations orderList)
        
        let portOrderCombs = 
            edgePermutations[Edge.Left] |> List.collect (fun leftConfig ->
                edgePermutations[Edge.Right] |> List.collect (fun rightConfig ->
                    edgePermutations[Edge.Top] |> List.collect (fun topConfig ->
                        edgePermutations[Edge.Bottom] |> List.map (fun bottomConfig ->
                            Map.ofList [
                                (Edge.Left, leftConfig);
                                (Edge.Right, rightConfig);
                                (Edge.Top, topConfig);
                                (Edge.Bottom, bottomConfig)
                            ]
                        )
                    )
                )
            )
        portOrderCombs
    
    | _ ->
        [symbol.PortMaps.Order]


/// <summary> generate all possible symbolScripts for a symbol </summary>
/// <param name="symbol"> The symbol to generate scripts for </param>
/// <returns> A list of all possible symbolScripts for the symbol </returns>
let generateSymbolScript (symbol: SymbolT.Symbol) =
    let flips = [Some FlipHorizontal; Some FlipVertical; None]
    let reversedInputs = [true; false]
    let portOrderCombs = generatePortOrderCombs symbol

    let allCombs =
        List.collect (fun flipped ->
            List.collect (fun reversedInput ->
                List.map (fun portOrder ->
                    {
                        Flipped = flipped
                        ReversedInput = reversedInput
                        PortOrder = portOrder
                    }
                ) portOrderCombs
            ) reversedInputs
        ) flips
    allCombs

let generateModelScript (model: SheetT.Model): list<modelScript> =
    let symbols = 
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    
    let Ids = 
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map fst
    
    let symbolScriptLists = symbols |> List.map generateSymbolScript
            
    // Generating all possible combinations
    symbolScriptLists
    |> cartesianProduct
    |> List.map (fun scripts -> List.zip Ids scripts)



// update a symbol with a symbolScript
let applyScriptToSymbol (script: symbolScript) =
    
    let a_, updateMuxFlip = reversedInputPorts_

    let handleFlip (flip: SymbolT.FlipType option) =
        match flip with
        | Some SymbolT.FlipType.FlipHorizontal -> 
            SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal
        | Some SymbolT.FlipType.FlipVertical ->
            SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal 
            >> SymbolResizeHelpers.rotateAntiClockByAng Degree180 
        | None -> id
    
    let handlePortOrder (symbol: SymbolT.Symbol): SymbolT.Symbol =
        match symbol.Component.Type with
        | Custom _ -> 
            symbol
            |> putPortOrder Edge.Left script.PortOrder.[Edge.Left]
            |> putPortOrder Edge.Right script.PortOrder.[Edge.Right]
            |> putPortOrder Edge.Top script.PortOrder.[Edge.Top]
            |> putPortOrder Edge.Bottom script.PortOrder.[Edge.Bottom]
        | _ -> 
            symbol     

    let handleMux2InputOrder(symbol: SymbolT.Symbol): SymbolT.Symbol = 
        match symbol.Component.Type with
        | Mux2 | Mux4 | Mux8 -> updateMuxFlip (Some script.ReversedInput) symbol
        | _ -> symbol

    let updateSymbol (symbol: SymbolT.Symbol): SymbolT.Symbol = 
        symbol
        |> handleFlip script.Flipped
        |> handleMux2InputOrder
        |> handlePortOrder

    updateSymbol

// TODO: Apply the script to each symbol
let applyScriptToModel (model: SheetT.Model) (modelScript: modelScript): SheetT.Model =
    
    let updateOneSymbol (symScript: symbolScript)(symId: ComponentId)(model: SheetT.Model): SheetT.Model = 
        let applyScriptToSymbol' = applyScriptToSymbol symScript
        
        let newSymModel: SymbolT.Model = 
            SymbolUpdate.updateSymbol applyScriptToSymbol' symId model.Wire.Symbol

        let newModel = 
            model
            |> Optic.set symbol_ newSymModel
            |> updateBoundingBoxes
        
        {newModel with Wire =  (updateWires newModel.Wire [symId] {X = 0.0; Y = 0.0})}
        
    modelScript
    |> List.fold (fun model (symId, symScript) -> updateOneSymbol symScript symId model) model


// Count the number of right angle intersections
let evaluateModel (model: SheetT.Model) =
    numOfWireRightAngleCrossings model


let randomSample list =
    // Check if the list has at least 10,000 elements
        if List.length list >= 10000 then
            let rnd = System.Random()
            let sampleNum = 
                List.length list
                |> System.Math.Sqrt
                |> System.Math.Floor
                |> int
            let shuffledList = List.sortBy (fun _ -> rnd.Next()) list
            shuffledList |> List.take sampleNum
        else
            list

// Get optimized model
let optimizeModelExhaustive (model: SheetT.Model): SheetT.Model =
        
    let scripts = 
        model
        |> generateModelScript
        |> randomSample

    let bestModel, bestScore = 
        scripts
        |> List.map (applyScriptToModel model)
        |> List.map (fun model -> (model, evaluateModel model))
        |> List.minBy snd
    
    // printfn "Best score: %A" bestScore
    bestModel


// Iterated local search-----------------------------------------------

let findAllMuxAndGate (model: SheetT.Model) = 
    let symbols = 
        model.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    
    let muxAndGateSymbols = 
        symbols
        |> List.filter (fun sym -> 
            match sym.Component.Type with
            | Mux2 | Mux4 | Mux8 | GateN _ | Not | NbitsXor _| NbitsAnd _| NbitsNot _ | NbitsOr _| NbitsXor _-> true
            | _ -> false)
    
    muxAndGateSymbols


let optimizeOneSymbol (model: SheetT.Model) (symbol: SymbolT.Symbol) = 
    let symbolScripts = generateSymbolScript symbol
    let modelScripts = 
        symbolScripts
        |> List.map (fun symbolScript -> [(symbol.Id, symbolScript)])
    
    let bestModel, bestScore = 
        modelScripts
        |> List.map (applyScriptToModel model)
        |> List.map (fun model -> (model, evaluateModel model))
        |> List.minBy snd
    
    (bestModel, bestScore)

let optimizeModelOnce (model: SheetT.Model) = 
    let bestModel, score = 
        model
        |> findAllMuxAndGate
        |> List.fold (fun (currentModel, _) symbol ->
                optimizeOneSymbol currentModel symbol
            ) (model, 999)
    
    bestModel, score


let optimizeModelILS (model: SheetT.Model) =

    let mutable currentModel = model
    let mutable currentScore = 999
    let mutable continueOptimization = true

    while continueOptimization do
        let newModel, newScore = optimizeModelOnce currentModel

        // Check if the score has improved (decreased)
        if newScore < currentScore then
            currentModel <- newModel
            currentScore <- newScore
        else
            // If no improvement, stop the optimization loop
            continueOptimization <- false
    
    // 
    //     printfn "current score: %A" currentScore
    // printfn "Final score: %A" currentScore
    currentModel


    

// Unused code

// let splitComponents (sym1: Symbol) (model: SheetT.Model) = 
//     let connections = 
//         model.Wire.Wires
//         |> Map.toList
//         |> List.map snd
    
//     let components = 
//         model.Wire.Symbol.Symbols
//         |> Map.toList
//         |> List.map snd

//     let idList = 
//         components
//         |> List.map (fun sym -> sym.Id)
    
//     let portList = 
//         components
//         |> List.map (fun sym -> sym.PortMaps.Order)
//         |> List.map (Map.toList)
//         |> List.map (List.collect snd)
    
//     // componentId -> portId
//     let compIdToPortIdMap = 
//         List.zip idList portList
//         |> Map.ofList
    
//     let invertMap map =
//         Map.fold (fun acc key valueList ->
//             List.fold (fun accInner value -> 
//                 Map.add value key accInner) acc valueList) Map.empty map

//     let portIdToCompIdMap = invertMap compIdToPortIdMap

//     let mapOutputToInput (connections: List<Wire>) =
//         let connectionsList = 
//             connections
//             |> List.collect (fun wire -> [(wire.InputPort, wire.OutputPort); (wire.InputPort, wire.OutputPort)])
//         let groupedConnections = 
//             connectionsList
//             |> Seq.groupBy fst
//             |> Seq.map (fun (port, connectedPorts) -> port, connectedPorts |> Seq.map snd |> Seq.toList)
//         Map.ofSeq groupedConnections


//     let mapIO = mapOutputToInput connections


//     let outputPortIds = 
//         sym1.Component.OutputPorts
//         |> List.map (fun port -> port.Id)
    
//     let getConnectedCompId (symId: ComponentId) =  
        
//         let portIdList = compIdToPortIdMap.[symbol.Id]

//         let symbolPortIdList = 
//             portIdList
//             |> List.map (fun portId -> mapIO.[portId])
//             |> List.collect id
//             |> List.distinct
        
//         symbolPortIdList
//         |> List.map (fun ele -> portIdToCompIdMap[ele])
//         |> List.distinct
    
//     let connectionListMap = 
//         idList
//         |> List.map (fun id -> (id, getConnectedCompId id))
//         |> Map.ofList

//     let groupComponents connectionListMap idList =

//         let rec isConnected comp1 comp2 =
//             match Map.tryFind comp1 connectionListMap with
//             | Some connections -> List.contains comp2 connections || List.exists (fun c -> isConnected c comp2) connections
//             | None -> false

//         let rec group remainingIds grouped =
//             match remainingIds with
//             | [] -> grouped
//             | head::tail ->
//                 let related, unrelated = List.partition (fun id -> isConnected head id || isConnected id head) tail
//                 group unrelated ((head::related)::grouped)

//         group idList []
    
// groupComponents connectionListMap idList 

//------------------------------------test function---------------------------------------//
// inrevelent to the main code, used for testing

// let printSymbolScript (script: symbolScript) =
//     let flippedMessage = match script.Flipped with
//                          | Some flipType -> sprintf "Some(%A)" flipType
//                          | None -> "None"
//     let portOrderStr = 
//         script.PortOrder 
//         |> Map.toList 
//         |> List.map (fun (edge, orders) ->
//             sprintf "%A: [%s]" edge (String.concat "; " orders)) // Adjusted to use String.concat
//         |> String.concat ", "
//     let message = 
//         sprintf "Flipped: %s, ReversedInput: %b, PortOrder: {%s}" 
//             flippedMessage script.ReversedInput portOrderStr
//     printfn "%s" message
//     1



// let printModelScript (script: modelScript) =
//     let a = 
//         script
//         |> List.map snd
//         |> List.map printSymbolScript
//     1


// let randomPossibleModels (model: SheetT.Model) = 
//     let scripts = generateModelScript model
    
//     printModelScript scripts.[0]
//     |> ignore


//     let models = 
//         scripts
//         |> List.map (applyScriptToModel model)
    

//     let random = System.Random()
//     let randomModel = models.[random.Next(models.Length)]
    
//     printfn "Best score: %A" (evaluateModel randomModel)

//     randomModel


// let certainModel (model: SheetT.Model) = 

//     let scripts = generateModelScript model
    
//     printModelScript scripts.[0]
//     |> ignore


//     let models = 
//         scripts
//         |> List.map (applyScriptToModel model)
    
//     models[0]

