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
    | [] -> [[]] // The only combination of an empty list is a list with an empty list
    | head :: tail ->
        let recSubsets = combinations tail
        recSubsets @ (recSubsets |> List.map (fun subset -> head :: subset))

let generateSymbolScript (symbol: SymbolT.Symbol) =
    let flips = [Some FlipHorizontal; Some FlipVertical; None]
    let reversedInputs = [true; false]
    let portEdges = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
    
    let portOrderCombs = 
        portEdges
        |> List.map (fun edge -> (edge, symbol.PortMaps.Order[edge]))
        |> combinations
    
    let edgePermutations = 
        symbol.PortMaps.Order
        |> Map.map (fun _ orderList -> combinations orderList)
    
    let portOrderCombs2 = 
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
    


    let allCombs =
        List.collect (fun flipped ->
            List.collect (fun reversedInput ->
                List.map (fun portOrder ->
                    {
                        Flipped = flipped
                        ReversedInput = reversedInput
                        PortOrder = portOrder
                    }
                ) portOrderCombs2
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





// let getSymId (symLabel: string) (symModel: SymbolT.Model) =
//     mapValues symModel.Symbols
//     |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
//     |> function | Some x -> x.Id | None -> failwith "Can't find symbol with label '{symPort.Label}'"


// let flipSymbol (symId) (model: SheetT.Model): (SheetT.Model) =
//     // let symId = getSymId symLabel model.Wire.Symbol
//     let flipver (model: SheetT.Model) = 
//         let flipSymbol' = SymbolResizeHelpers.flipSymbol SymbolT.FlipVertical
//         let symModel: SymbolT.Model = 
//             SymbolUpdate.updateSymbol flipSymbol' symId model.Wire.Symbol

//         model
//         |> Optic.set symbolModel_ symModel
//         |> SheetUpdateHelpers.updateBoundingBoxes
    
//     model
//     |> flipver
            

// update a symbol with a symbolScript
let applyScriptToSymbol (script: symbolScript) =
    // let _, updatePortOrder = symPortOrder_ script.PortEdge
    // let _, updateMux2InputOrder = reverseMux2Input_
    // let _, updateFlip = symbolFlipped_

    // symbol
    // // |> updatePortOrder (Some script.PortOrder) 
    // // |> updateMux2InputOrder script.ReversedInput
    // |> updateFlip script.Flipped

    
    let a_, updatePortOrder = reversedInputPorts_


    let handleFlip (flip: SymbolT.FlipType option) =

        match flip with
        | Some SymbolT.FlipType.FlipHorizontal -> 
            SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal
        | Some SymbolT.FlipType.FlipVertical ->
            SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal 
            >> SymbolResizeHelpers.rotateAntiClockByAng Degree180 
        | None -> id
    
    
    let handlePortOrder (symbol: SymbolT.Symbol): SymbolT.Symbol =
        symbol
        |> putPortOrder Edge.Left script.PortOrder.[Edge.Left]
        |> putPortOrder Edge.Right script.PortOrder.[Edge.Right]
        |> putPortOrder Edge.Top script.PortOrder.[Edge.Top]
        |> putPortOrder Edge.Bottom script.PortOrder.[Edge.Bottom]

        // [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
        // |> putPortOrder script.PortEdge script.PortOrder

    let handleMux2InputOrder = 
        updatePortOrder (Some script.ReversedInput)

    let updateSymbol (symbol: SymbolT.Symbol): SymbolT.Symbol = 
        symbol
        |> handleFlip script.Flipped
        // |> handlePortOrder
        |> handleMux2InputOrder

        
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
        
        newModel
    
    modelScript
    |> List.fold (fun model (symId, symScript) -> updateOneSymbol symScript symId model) model


// Count the number of right angle intersections
let evaluateModel (model: SheetT.Model) =
    countTotalRightAngleIntersect model

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
    let randomElement = models.[random.Next(models.Length)]

    randomElement


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
