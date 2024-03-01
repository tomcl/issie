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

/// <summary>AUTHOR ec1221 - Adjusts port order on custom components, flip components,
/// flip MUX input order to reduce wire crossings.</summary>
/// <param name="model">  sheet model with components to optimize</param>
/// <returns>New sheet model with adjust ports/flip.</returns>
let sheetPortOrderFlip (model: SheetT.Model) : SheetT.Model =

    let filterCustomComponents (symbols: (ComponentId * Symbol) list) =
        symbols |> List.filter (fun (id,symbol) -> match symbol.Component.Type with
                                                    | Custom _ -> true
                                                    | _ -> false)

    // Recursion function to keep trying swapping port pairs until no improvement in wire crossing, then returns model.
    // For ports on a single edge 
    let rec swapPortsOnEdge ((model,id,symbol): SheetT.Model * ComponentId * Symbol) (edge: Edge) =

        // Function to generate all possible combinations of port swaps
        let generateSwaps (order: string list) : (string list list) =
            let possibleOrders = order.Length

            let swapElementsAtIndexes index1 index2 list =
                list
                |> List.mapi (fun i x ->
                    if i = index1 then
                        List.item index2 list
                    elif i = index2 then
                        List.item index1 list
                    else
                        x
                )

            order
            |> List.replicate possibleOrders
            |> List.mapi (fun i order ->
                if i < possibleOrders-1 then
                    swapElementsAtIndexes i (i+1) order
                else
                    swapElementsAtIndexes i 0 order
            )

        // create model with new port layout and evaluate
        let checkSwapFolder ((improvement,model): bool * SheetT.Model) (order: string list)  =
            let beforeCount = countCrossingWires model
            let model' = Optic.set symbols_ (Map.add id (setPortOrder model.Wire.Symbol.Symbols[id] edge order) model.Wire.Symbol.Symbols) model
            let afterCount = countCrossingWires model'

            if afterCount < beforeCount then
                true,model'
            else
                false,model
                
        let swappedPortsList =
            symbol.PortMaps.Order[edge]
            |> generateSwaps

        let improvement, bestModel = ((false,model), swappedPortsList) ||> List.fold checkSwapFolder 

        if improvement then
            swapPortsOnEdge (bestModel,id,symbol) edge
        else
            (bestModel,id,symbol)

    // Fold over the port swap combinations to find the one with the least wire crossings for a whole symbol
    let swapSymbolEdgePorts (model: SheetT.Model) ((id,symbol): ComponentId * Symbol) =
        let edgesWithPorts = 
            [Top;Bottom;Left;Right]
            |> List.filter (fun edge -> Map.containsKey edge symbol.PortMaps.Order)
        let model',_,_ = ((model,id,symbol), edgesWithPorts) ||> List.fold swapPortsOnEdge
        model'

    let symbols =
        model.Wire.Symbol.Symbols
        |> Map.toList

    let customComponentSymbols = filterCustomComponents symbols
    let (model') = (model,customComponentSymbols) ||> List.fold swapSymbolEdgePorts 
    model'
