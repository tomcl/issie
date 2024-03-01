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

/// constants used by SheetBeautify
module Constants =
    // D2. sheetOrderFlip 
    // Adjust on sheet: Port order on custom components, flip components, flip MUX input order
    // Primary Optimisation: Reduce wire crossings 
    // Test using random sample inputsReduction in wire crossings, other quality measures
    


    // Test Function: 
    //      - try through all possible combinations of flipping components
    //      - find the one with the least wire crossings
    //      - apply that combination to the model and return the new model

    //  this can be used in the team deliverable to find the best combination of flipping components
    //  to reduce wire crossings
    //  Test on Port order of custom components, MUX input order were not tested in this function, and is needed to be implemented in the future
    

    // generate all possible flip combinations
    
    
    type symbolScript = {
        Flipped: bool
        ReversedInput: bool
        PortEdge: Edge
        PortOrder: list<string>
    }

    type modelScript = list<symbolScript>

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
        ()
        //TODO: Implement this
        

    // update a symbol with a script
    let applyScript (script: symbolScript) (symbol: SymbolT.Symbol) =
        let _, updatePortOrder = symPortOrder_ script.PortEdge
        let _, updateMux2InputOrder = reverseMux2Input_
        let _, updateFlip = symbolFlipped_

        symbol
        |> updatePortOrder (Some script.PortOrder) 
        |> updateMux2InputOrder script.ReversedInput
        |> updateFlip script.Flipped
    
    let applyScriptToModel (model: SheetT.Model) (script: modelScript): SheetT.Model =
        let symbols = 
            model.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map snd
        ()
        // Apply the script to each symbol

    let evaluateModel (model: SheetT.Model) =
        // Count the number of right angle intersections
        countTotalRightAngleIntersect model

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
        

        
        
        
        
        // Iterate over all combinations, apply them, and find the one with the least intersections
        // let bestModel =
        //     flipCombs
        //     |> List.collect (fun flipComb ->
        //         reversedInputCombs
        //         |> List.collect (fun reversedInputComb ->
        //             portOrderCombs
        //             |> List.map (fun portOrderComb ->
        //                 let newModel = applyCombination flipComb reversedInputComb portOrderComb model
        //                 let metric = countTotalRightAngleIntersect newModel
        //                 (newModel, metric)
        //             )
        //         )
        //     )
        //     |> List.minBy snd
        //     |> fst

        // bestModel