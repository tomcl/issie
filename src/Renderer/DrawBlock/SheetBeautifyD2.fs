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

/// <summary>AUTHOR ec1221 - Adjusts port order on custom components, flip components,
/// flip MUX input order to reduce wire crossings.</summary>
/// <param name="model">  sheet model with components to optimize</param>
/// <returns>New sheet model with adjust ports/flip.</returns>
let optimizePortOrder (model: SheetT.Model) : SheetT.Model =

    let filterMUXandCustomComponents (symbols: (ComponentId * Symbol) list) =
        symbols |> List.filter (fun (id,symbol) -> match symbol.Component.Type with
                                                    | Custom _ -> true
                                                    | (Mux2 | Mux4 | Mux8) -> true
                                                    | _ -> false)

    // Recursion function to keep trying swapping port pairs until no improvement in wire crossing, then returns model.
    // For ports on a single edge 
    let swapPortsOnEdge ((model,id,symbol): SheetT.Model * ComponentId * Symbol) (edge: Edge) =

        // Function to generate all possible combinations of port swaps
        let generateSwaps (order: string list) : (string list list) =
            let possibleOrders = order.Length
            if (possibleOrders > 1) then
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
            else
                []


        // create model with new port layout and evaluate
        let checkSwapFolder (model: SheetT.Model) (order: string list) =
            let beforeCount = numOfWireRightAngleCrossings model
            let model' = Optic.set symbols_ (Map.add id (putPortOrder  edge order model.Wire.Symbol.Symbols[id]) model.Wire.Symbol.Symbols) model
            let wireModel = updateWires model'.Wire [id] {X= 0.0; Y= 0.0}
            let model'' = {model' with Wire = wireModel}
            let afterCount = numOfWireRightAngleCrossings model''
            printfn $"after crossings: {afterCount}, before: {beforeCount}"  

            if afterCount < beforeCount then
                printfn "Swapped a custom component port order"
                model''
            else
                model
                
        let swappedPortsList =
            symbol.PortMaps.Order[edge]
            |> generateSwaps

        let bestModel = (model, swappedPortsList) ||> List.fold checkSwapFolder            
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

    let customComponentSymbols = filterMUXandCustomComponents symbols
    let (model') = (model,customComponentSymbols) ||> List.fold swapSymbolEdgePorts
    printfn "Beautified"
    model'
    

