module SheetBeautify

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
    // PortEdge: Edge
    // PortOrder: list<string>
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


let generateSymbolScript (symbol: SymbolT.Symbol) =
    let flips = [Some FlipHorizontal; Some FlipVertical; None]
    let reversedInputs = [true; false]
    // let portEdges = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]

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

// Get optimized model
let getOptimizedModel (model: SheetT.Model): SheetT.Model =
    // Convert symbol map to list
    let scripts = generateModelScript model

    let modelsWithScores = 
        scripts
        |> List.map (applyScriptToModel model)
        |> List.map (fun model -> (model, evaluateModel model))

    let bestModel = 
        modelsWithScores
        |> List.minBy snd
        |> fst
    
    let bestScore = 
        modelsWithScores
        |> List.minBy snd
        |> snd
    
    printfn "Best score: %A" bestScore

    bestModel


// test function

let printSymbolScript (script: symbolScript) =
    let flippedMessage = match script.Flipped with
                         | Some flipType -> sprintf "Some(%A)" flipType
                         | None -> "None"
    let portOrderStr = 
        script.PortOrder 
        |> Map.toList 
        |> List.map (fun (edge, orders) ->
            sprintf "%A: [%s]" edge (String.concat "; " orders)) // Adjusted to use String.concat
        |> String.concat ", "
    let message = 
        sprintf "Flipped: %s, ReversedInput: %b, PortOrder: {%s}" 
            flippedMessage script.ReversedInput portOrderStr
    printfn "%s" message
    1



let printModelScript (script: modelScript) =
    let a = 
        script
        |> List.map snd
        |> List.map printSymbolScript
    1


let randomPossibleModels (model: SheetT.Model) = 
    let scripts = generateModelScript model
    
    printModelScript scripts.[0]
    |> ignore


    let models = 
        scripts
        |> List.map (applyScriptToModel model)
    

    let random = System.Random()
    let randomModel = models.[random.Next(models.Length)]
    
    printfn "Best score: %A" (evaluateModel randomModel)

    randomModel


let certainModel (model: SheetT.Model) = 

    let scripts = generateModelScript model
    
    printModelScript scripts.[0]
    |> ignore


    let models = 
        scripts
        |> List.map (applyScriptToModel model)
    
    models[0]


// Heuristic Algorithm
// Use a heuristic to partition components into independent connected groups
// TODO
