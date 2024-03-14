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
    Flipped: bool
    ReversedInput: bool
    PortEdge: Edge
    PortOrder: list<string>
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
    let flips = [true; false]
    let reversedInputs = [true; false]
    let portEdges = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
    
    let portOrderCombs = 
        portEdges
        |> List.map (fun edge -> symbol.PortMaps.Order[edge])
        |> combinations
    
    let allCombs =
        List.collect (fun flipped ->
            List.collect (fun reversedInput ->
                List.map (fun (portEdge, portOrder) ->
                    {
                        Flipped = flipped
                        ReversedInput = reversedInput
                        PortEdge = portEdge
                        PortOrder = []
                    }
                ) (List.zip portEdges portOrderCombs)
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
let applyScriptToSymbol (script: symbolScript) (symbol: SymbolT.Symbol) =
    let _, updatePortOrder = symPortOrder_ script.PortEdge
    let _, updateMux2InputOrder = reverseMux2Input_
    let _, updateFlip = symbolFlipped_

    symbol
    |> updatePortOrder (Some script.PortOrder) 
    |> updateMux2InputOrder script.ReversedInput
    |> updateFlip script.Flipped

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
let optimizeFlipForComponents (model: SheetT.Model): SheetT.Model =
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
    
    bestModel
    

// Heuristic Algorithm
// Use a heuristic to partition components into independent connected groups
// TODO
