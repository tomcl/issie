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
    let optimizeFlipForComponents (model: SheetT.Model) =
        // Convert symbol map to list
        let symList = 
            model.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map snd

        // Generate all possible flip combinations
        let rec generateCombinations symbols =
            match symbols with
            | [] -> [[]]
            | x::xs ->
                let recs = generateCombinations xs
                List.collect (fun recComb -> [true::recComb; false::recComb]) recs

        let flipCombinations = generateCombinations symList

        // Function to apply a flip combination to the model and return a new model
        let applyFlipCombination (combination: bool list) (model: SheetT.Model) =

            let symbols = 
                model.Wire.Symbol.Symbols
                |> Map.toList
                |> List.map snd

            // Ensure the combination list matches the number of symbols
            if List.length combination <> List.length symbols then
                failwith "Combination length does not match the number of symbols."

            // Pair each symbol with its intended flip status
            let symbolsWithFlipStatus = List.zip symbols combination

            // Apply each flip status to its corresponding symbol, updating the model iteratively
            let updatedModel =
                symbolsWithFlipStatus
                |> List.fold (fun accModel (symbol, flipStatus) ->
                    let setFlip = snd (SymbolFlippedLens symbol)
                    setFlip flipStatus accModel
                ) model

            updatedModel

        // Iterate over all combinations, apply them, and find the one with the least intersections
        let bestCombination, minIntersections =
            flipCombinations
            |> List.map (fun comb -> 
                let newModel = applyFlipCombination comb model
                (comb, countTotalRightAngleIntersect newModel))
            |> List.minBy snd

        // Apply the best combination to the model
        let optimizedModel = applyFlipCombination bestCombination model
        // Return the optimized model
        optimizedModel
