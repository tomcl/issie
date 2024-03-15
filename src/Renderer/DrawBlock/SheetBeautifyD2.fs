module SheetBeautifyD2
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

    let partitionComponents (symbols: (ComponentId * Symbol) list) =
        symbols |> List.partition (fun (compId,symbol) -> match symbol.Component.Type with
                                                            | Custom _ -> true
                                                            | (Mux2 | Mux4 | Mux8) -> true
                                                            | _ -> false)

    // Evaluate change in wire crossings after adjustments
    let evaluateChange (model: SheetT.Model) (newModel: SheetT.Model) (compcompId: ComponentId): bool * SheetT.Model =
        let beforeCount = numOfWireRightAngleCrossings model
        let newWireModel = updateWires newModel.Wire [compcompId] {X= 0.0; Y= 0.0}
        let model' = {newModel with Wire = newWireModel}
        let afterCount = numOfWireRightAngleCrossings model'
        printfn $"after crossings: {afterCount}, before: {beforeCount}"
        (afterCount < beforeCount), model'

    // Recursion function to keep trying swapping port pairs until no improvement in wire crossing, then returns model.
    // For ports on a single edge 
    let swapPortsOnEdge ((model,compId,symbol): SheetT.Model * ComponentId * Symbol) (edge: Edge) =

        // Function to generate all possible combinations of port swaps
        let generateSwaps (order: string list) : (string list list) =
            let possibleOrders = order.Length
            if (possibleOrders > 1) then
                let swapElementsAtIndexes index1 index2 list =
                    list
                    |> List.mapi (fun i x ->
                        if i = index1 then List.item index2 list
                        elif i = index2 then List.item index1 list
                        else x
                    )

                order
                |> List.replicate possibleOrders
                |> List.mapi (fun i order ->
                    if i < possibleOrders-1 then
                        swapElementsAtIndexes i (i+1) order
                    else
                        swapElementsAtIndexes i 0 order
                )
            else []

        // create model with new port layout and evaluate
        let checkSwapFolder (model: SheetT.Model) (order: string list) =
            let newModel =
                model
                |> Optic.set symbols_ (Map.add compId (putPortOrder  edge order model.Wire.Symbol.Symbols[compId]) model.Wire.Symbol.Symbols)
            let improvement,model' = evaluateChange model newModel compId

            if improvement then
                printfn "Swapped a custom component port order"
                model'
            else
                model

        // create a model with swapped MUX sel port and evaluate
        let checkMuxSelSwap model symbol edge =
            if (isMuxSel symbol edge) then
                let flipDirection =
                    match edge with
                    | (Left | Right) -> FlipHorizontal
                    | (Top | Bottom) -> FlipVertical
                let newModel = flipSymbol symbol.Component.Label flipDirection model
                let improvement,model' = evaluateChange model newModel compId
                if improvement then
                    printfn "Flipped MUX Sel port edge"
                    model'
                else
                    model
            else
                model

        let swappedPortsList =
            symbol.PortMaps.Order[edge]
            |> generateSwaps

        let bestPortSwap =
            (model, swappedPortsList)
            ||> List.fold checkSwapFolder            
        
        let bestModel = checkMuxSelSwap bestPortSwap symbol edge
        (bestModel,compId,symbol)

    // Flip symbol horizontally, vertically, or both based on improvement
    let flipSymbol (model: SheetT.Model) ((compId,symbol): ComponentId * Symbol) =
        let newModelH = flipSymbol symbol.Component.Label FlipHorizontal model
        let newModelV = flipSymbol symbol.Component.Label FlipVertical model
        let newModelVH = flipSymbol symbol.Component.Label FlipVertical (flipSymbol symbol.Component.Label FlipHorizontal model)
        let improvementH,modelH = evaluateChange model newModelH compId
        let improvementV,modelV = evaluateChange model newModelV compId
        let improvementVH,modelVH = evaluateChange model newModelVH compId
        if (improvementH || improvementV || improvementVH) then
            printfn "flipped normal component"

        match improvementH, improvementV, improvementVH with
        | true, _, _ -> modelH
        | false,true,_ -> modelV
        | false,false,true -> modelVH
        | _ -> model


    // Swap ports on all edges of a symbol
    let swapSymbolEdgePorts (model: SheetT.Model) ((compId,symbol): ComponentId * Symbol) =
        let edgesWithPorts = 
            [Top;Bottom;Left;Right]
            |> List.filter (fun edge -> Map.containsKey edge symbol.PortMaps.Order)
        let model',_,_ =
            ((model,compId,symbol), edgesWithPorts)
            ||> List.fold swapPortsOnEdge
        model'

    let initialCrossings = numOfWireRightAngleCrossings model

    let symbols =
        model.Wire.Symbol.Symbols
        |> Map.toList

    let customAndMuxComponents, otherComponents = partitionComponents symbols
    let portSwapOptimized =
        (model,customAndMuxComponents)
        ||> List.fold swapSymbolEdgePorts
    let flipOptimized =
        (portSwapOptimized,otherComponents)
        ||> List.fold flipSymbol
    let model' = flipOptimized
    let improvement = (numOfWireRightAngleCrossings model' < initialCrossings)
    if improvement then
        optimizePortOrder model'
    else
        printfn "Beautified"
        model
    

