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
    let rec generateFlipComb symbols =
        match symbols with
        | [] -> [[]]
        | x::xs ->
            let recs = generateFlipComb xs
            List.collect (fun recComb -> [true::recComb; false::recComb]) recs

    // generate all possible mux input combinations
    let rec generateMuxInputComb symbols =
        match symbols with
        | [] -> [[]]
        | x::xs ->
            let recs = generateMuxInputComb xs
            List.collect (fun recComb -> [true::recComb; false::recComb]) recs
    
    let rec insertAt i x list =
        match list with
        | [] -> [x] // If the list is empty, return a list with the element x.
        | h::t when i = 0 -> x::list // If i is 0, insert x at the beginning of the list.
        | h::t -> h :: insertAt (i-1) x t // Otherwise, recursively insert x into the tail of the list.

    let rec permutations list = 
        match list with
        | [] -> [[]] // The permutation of an empty list is a list containing an empty list.
        | x::xs -> 
            permutations xs |> List.collect (fun perm -> 
                List.mapi (fun i _ -> insertAt i x perm) (x::perm)
            )
    let generateAllPortOrderCombs symbols =
        symbols |> List.map (fun symbol ->
            let generatePermutationsForSide side =
                match Map.tryFind side symbol.PortMaps.Order with
                | Some ports -> permutations ports
                | None -> [[]] // If no ports on this side, return a list with an empty list.
            let sides = [Edge.Left; Edge.Right; Edge.Top; Edge.Bottom]
            sides |> List.map generatePermutationsForSide
        )

    let optimizeFlipForComponents (model: SheetT.Model) =
        // Convert symbol map to list
        let symList = 
            model.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map snd

        // Generate all possible flip combinations
        let flipCombs = generateFlipComb symList
        let reversedInputCombs = generateMuxInputComb symList
        let portOrderCombs = generateAllPortOrderCombs symList
        
        let allCombinations = 
            flipCombs
            |> List.collect (fun flipComb ->
                reversedInputCombs
                |> List.collect (fun reversedInputComb ->
                    portOrderCombs
                    |> List.map (fun portOrderComb -> (flipComb, reversedInputComb, portOrderComb))
                )
            )
        // Function to apply a flip combination to the model and return a new model

        
        let applyCombination (flipState: list<bool>) (reversedInputState: list<bool>) (portOrderState) model =
            let symbols = model.Wire.Symbol.Symbols |> Map.toList |> List.map snd

            // Ensure the zip operations pair each symbol with its states correctly

            let combinedStates = List.zip3 flipState reversedInputState portOrderState
            let symbolStates = List.zip symbols combinedStates

            // Define a function to apply states to a symbol and update the model
            let applyStates (model: SheetT.Model) (symbol, (flip, reversedInput, portOrder)) =
                let modelAfterFlip = (snd (SymbolFlippedLens symbol) flip model)
                let modelAfterReversedInput = (snd (reverseMuxInputLens symbol) reversedInput modelAfterFlip)
                let modelAfterPortOrder = (snd (portOrderLens symbol (determineEdgeForSymbol symbol)) portOrder modelAfterReversedInput)
                modelAfterPortOrder

            // Apply the states to each symbol in the model
            List.fold applyStates model symbolStates

        
        
        // Iterate over all combinations, apply them, and find the one with the least intersections
        let bestModel =
            flipCombs
            |> List.collect (fun flipComb ->
                reversedInputCombs
                |> List.collect (fun reversedInputComb ->
                    portOrderCombs
                    |> List.map (fun portOrderComb ->
                        let newModel = applyCombination flipComb reversedInputComb portOrderComb model
                        let metric = countTotalRightAngleIntersect newModel
                        (newModel, metric)
                    )
                )
            )
            |> List.minBy snd
            |> fst

        bestModel