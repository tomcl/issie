module SheetBeautifyFlip
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
open Operators
open BusWireRoute
open Symbol

/// <summary>AUTHOR ec1221 - Adjusts port order on custom components, flip components,
/// flip MUX input order to reduce wire crossings.</summary>
/// <param name="model">  sheet model with components to optimize</param>
/// <returns>New sheet model with adjust ports/flip.</returns>
let rec optimizePortOrder (model: SheetT.Model) : SheetT.Model =
    // Other notes: current implementation works well on unrotated blocks which the majority are,
    // where their inputs/outputs are on left or right side. However, possible improvement could be to test rotating
    // blocks that are in close proximity, as these are mainly the cases that could reduce some wire crossings.

    let filterComponents (symbols: (ComponentId * Symbol) list) =
        symbols |> List.filter (fun (compId,symbol) -> match symbol.Component.Type with
                                                            | Custom _ -> true
                                                            | (Mux2 | Mux4 | Mux8) -> true
                                                            | _ -> false)

    // update wiring of model and evaluate against the best case so far by comparing wire crossings
    let evaluateUpdateChanges (crossings: int) (changedSymbols: SheetT.Model) (compcompId: ComponentId): bool * SheetT.Model * int =
        let updatedWires = updateWires changedSymbols.Wire [compcompId] {X= 0.0; Y= 0.0}
        let updatedModel = {changedSymbols with Wire = updatedWires}
        let updatedCrossings = numOfWireRightAngleCrossings updatedModel
        //printfn $"after crossings: {updatedCrossings}, before: {crossings}"
        (updatedCrossings < crossings),updatedModel, updatedCrossings


    // Swap port pairs and returns new model if reduction in wire crossings - For ports on a given edge of symbol
    let swapPortsOnEdge ((model,compId,symbol): SheetT.Model * ComponentId * Symbol) (edge: Edge) =

        // Function to generate all possible combinations of port swaps
        let generateSwaps (order: string list) : (string list list) =

            let swapElementsAtIndexes index1 index2 list =
                    list
                    |> List.mapi (fun i x ->
                        if i = index1 then List.item index2 list
                        elif i = index2 then List.item index1 list
                        else x
                    )

            let possibleOrders = order.Length

            if (possibleOrders > 1) then
                order
                |> List.replicate possibleOrders
                |> List.mapi (fun i order ->
                    if i < possibleOrders-1 then
                        swapElementsAtIndexes i (i+1) order
                    else
                        swapElementsAtIndexes i 0 order
                )
            else []

        // create model with new port layout for specific edge on a symbol and evaluate
        let evaluatePortOrder ((model,crossings): SheetT.Model * int) (order: string list) =
            let newSymbol =
                model
                |> Optic.set symbols_ (Map.add compId (putPortOrder  edge order model.Wire.Symbol.Symbols[compId]) model.Wire.Symbol.Symbols)

            let improvement,newModel,updatedCrossings = evaluateUpdateChanges crossings newSymbol compId

            if improvement then
                printfn "Swapped a MUX / custom component port order"
                newModel,updatedCrossings
            else
                model,crossings

        // create a model with swapped MUX sel port and evaluate, if given a SEL port on a MUX
        // Although the function later that tries to flip symbols also has the functionality to
        // do the same thing, this is run first to prioritise keeping the MUX inputs and outputs
        // on the same side
        let tryMuxSelSwap  symbol edge (model,crossings) =
            if (isMuxSel symbol edge) then
                let flipDirection =
                    match edge with
                    | (Left | Right) -> FlipHorizontal
                    | (Top | Bottom) -> FlipVertical

                let newSymbol = flipSymbol symbol.Component.Label flipDirection model
                let improvement,newModel,updatedCrossings = evaluateUpdateChanges crossings newSymbol compId

                if improvement then
                    printfn "Flipped MUX Sel port edge"
                    newModel
                else
                    model
            else
                model

        let swappedPortsOrderList =
            symbol.PortMaps.Order[edge]
            |> generateSwaps

        let initialCrossings = numOfWireRightAngleCrossings model

        let bestModel =
            ((model,initialCrossings), swappedPortsOrderList)
            ||> List.fold evaluatePortOrder
            |> tryMuxSelSwap symbol edge

        (bestModel,compId,symbol)

    // Flip symbol horizontally, vertically, or both based on improvement
    let optimizeSymbolFlip ((model,crossings): SheetT.Model * int) ((compId,symbol): ComponentId * Symbol) =
        // Other notes: can possibly be optimised by running each of the 3 flip attempts
        // one at a time and checking nad returning after first improvement
        let newSymbolH = flipSymbol symbol.Component.Label FlipHorizontal model
        let newSymbolV = flipSymbol symbol.Component.Label FlipVertical model
        let newSymbolVH = flipSymbol symbol.Component.Label FlipVertical (flipSymbol symbol.Component.Label FlipHorizontal model)
        let improvementH,newModelH,crossingsH = evaluateUpdateChanges crossings newSymbolH compId
        let improvementV,newModelV,crossingsV = evaluateUpdateChanges crossings newSymbolV compId
        let improvementVH,newModelVH,crossingsVH = evaluateUpdateChanges crossings newSymbolVH compId
        if (improvementH || improvementV || improvementVH) then
            printfn "flipped normal component"

        match improvementH, improvementV, improvementVH with
        | true, _, _ -> newModelH, crossingsH
        | false,true,_ -> newModelV, crossingsV
        | false,false,true -> newModelVH, crossingsVH
        | _ -> model, crossings


    // Optimize by swapping ports for all edges on a symbol
    let optimizeSymbolPorts (model: SheetT.Model) ((compId,symbol): ComponentId * Symbol) =
        let edgesWithPorts = 
            [Top;Bottom;Left;Right]
            |> List.filter (fun edge -> Map.containsKey edge symbol.PortMaps.Order)

        let newModel,_,_ =
            ((model,compId,symbol), edgesWithPorts)
            ||> List.fold swapPortsOnEdge

        newModel

    let initialCrossings = numOfWireRightAngleCrossings model

    let allSymbols =
        model.Wire.Symbol.Symbols
        |> Map.toList

    let customAndMuxComponents = filterComponents allSymbols

    let portSwapOptimized =
        (model, customAndMuxComponents)
        ||> List.fold optimizeSymbolPorts

    let updatedCrossings = numOfWireRightAngleCrossings portSwapOptimized

    let flipOptimized,finalCrossings =
        ((portSwapOptimized,updatedCrossings),allSymbols)
        ||> List.fold optimizeSymbolFlip

    let newModel = flipOptimized

    // Allow small threshold to potentially reduce useless iterations
    if (finalCrossings < initialCrossings - 2) then
        printfn $"after crossings: {finalCrossings}, before: {initialCrossings}"
        optimizePortOrder newModel
    else
        printfn "Beautified"
        if (finalCrossings < initialCrossings) then
            newModel
        else
            model
    

