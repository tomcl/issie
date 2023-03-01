module SmartPortOrder
open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators

(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)

/// To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
/// symbolToOrder so that the connecting wires do not cross.
/// It should work out the interconnecting wires (wiresToOrder) from 
/// the two symbols, wModel.Wires and sModel.Ports
/// It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).

let tryUpdateAt index value list =
    if index >= 0 && index < List.length list then
        Some (List.updateAt index value list)
    else
        None

let updatePortMapList (sym:Symbol) (index:int) (portId:string) (edge:Edge) (newInputList: list<string>) =
    //let PortMapOrder = sym.PortMaps.Order
    let portMapOrder = newInputList

    match tryUpdateAt index portId newInputList  with
    | Some newList -> newList
    | None -> 
        printfn "Index out of range"
        newInputList

  
// need to use tryFindIndex here as well.
let findMinIndex (symbol: Symbol) (portList) (edge: Edge) =

    let portList' = 
            portList
            |> List.collect (fun id -> 
                match symbol.PortMaps.Order[edge] |> List.tryFindIndex (fun elm -> elm = id) with
                | Some index -> [index]
                | None -> [])

    match edge with
    | Top | Right -> 
        List.min portList'
    | Bottom | Left -> 
        List.max portList'

let findMaxIndex (symbol: Symbol) (portList) (edge: Edge) =

    let portList' = 
            portList
            |> List.collect (fun id -> 
                match symbol.PortMaps.Order[edge] |> List.tryFindIndex (fun elm -> elm = id) with
                | Some index -> [index]
                | None -> [])


    match edge with
    | Top | Right -> 
        List.max portList'
    | Bottom | Left ->
        List.min portList'

// clean this by putting match inside if/elif//else
// Use Options instead of .find
let findIndexShifted (interWires: list<Wire>) (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) (inputEdge: Edge) (outputEdge: Edge)
    (newInputList: list<string>) (newOutputList: list<string>) = 

    // code has to be written here
    // probably need to feed in inputPortList and outputPortLists that are connecgted

    let outputPortList = 
        interWires
            |> List.map(fun wire -> string wire.OutputPort)

    let inputPortList = 
        interWires
        |> List.map(fun wire -> string wire.InputPort)

    //need to find which edge is bigger and store that in a variable
    //let outputLength = otherSymbol.PortMaps.Order[outputEdge].Length - 1
    //let inputLength = symbolToOrder.PortMaps.Order[inputEdge].Length - 1

    let outputLength = newOutputList.Length - 1
    let inputLength = newInputList.Length - 1

    match inputEdge, outputEdge with
    | Top, Bottom | Bottom, Top | Left, Right | Right, Left | Left, Top | Bottom, Right ->
        if (inputLength = outputLength) then
            0
        elif inputLength > outputLength then
            let inputMinIndex = findMinIndex symbolToOrder inputPortList inputEdge
            printfn "INputMinINdex: %A" inputMinIndex 
            printfn "Diff input>output: %A" -(inputLength - inputMinIndex)
            -(inputLength - inputMinIndex)
        else
            let outputMinIndex = findMinIndex otherSymbol outputPortList outputEdge
            outputMinIndex
    
    | Top, Top | Top, Left | Right, Bottom ->
        if (inputLength = outputLength) then
            0
        elif inputLength > outputLength then
            let inputMaxIndex = findMaxIndex symbolToOrder inputPortList inputEdge
            printfn "Diff input>output: %A" -(inputLength - inputMaxIndex)
            -(inputLength - inputMaxIndex)
        else
            let outputMaxIndex = findMaxIndex otherSymbol outputPortList outputEdge
            outputMaxIndex

    | Bottom, Bottom | Left, Left | Bottom, Left | Left, Bottom ->
        if (inputLength = outputLength) then
                0
            elif inputLength > outputLength then
                let inputMinIndex = findMinIndex symbolToOrder inputPortList inputEdge
                printfn "InputMin: %A" inputMinIndex
                printfn "Diff input>output: %A" -(inputLength - inputMinIndex)
                -(inputLength - inputMinIndex)
            else
                let outputMaxIndex = findMaxIndex otherSymbol outputPortList outputEdge
                printfn "Diff else: %A" outputMaxIndex
                outputMaxIndex

    | Right, Right | Top, Right | Right, Top ->
        if (inputLength = outputLength) then
            0
        elif inputLength > outputLength then
            let inputMaxIndex = findMaxIndex symbolToOrder inputPortList inputEdge
            -(inputLength - inputMaxIndex)
        else
            let outputMinIndex = findMinIndex otherSymbol outputPortList outputEdge
            outputMinIndex


let getNewPortMap (symbolToOrder: Symbol) (otherSymbol: Symbol)
    (inputEdge: Edge) (outputEdge: Edge) 
    (interWires: list<Wire>) = 

    let inputEdgeList = symbolToOrder.PortMaps.Order[inputEdge]
    let outputEdgeList = otherSymbol.PortMaps.Order[outputEdge]

    let outputPortList = 
        interWires
        |> List.map(fun wire -> string wire.OutputPort)

    let inputPortList = 
        interWires
        |> List.map(fun wire -> string wire.InputPort)

    let filteredInputEdge = 
        inputEdgeList 
        |> List.filter (fun x -> List.contains x inputPortList)

    let filteredOutputEdge = 
        outputEdgeList 
        |> List.filter (fun x -> List.contains x outputPortList)

    filteredInputEdge, filteredOutputEdge


let changeOldPortMaps (symbolToOrder: Symbol) (newPortMapList: list<string>) (edge: Edge) 
    (indexChange:int) (inputPortId: string) = 

    let newLength = newPortMapList.Length

    let minIndex = 
            newPortMapList
            |> List.collect (fun id -> 
                match symbolToOrder.PortMaps.Order[edge] |> List.tryFindIndex (fun elm -> elm = id) with
                | Some index -> [index]
                | None -> [])
            |> List.min
    

    printfn"Min Index: %A length: %A" minIndex newLength
 
    let sliceList (start:int) (length:int) list =
        list
        |> Seq.skip start
        |> Seq.take length
        |> List.ofSeq

    let shortList = sliceList minIndex newLength symbolToOrder.PortMaps.Order[edge]

    let newPmap = symbolToOrder.PortMaps.Order[edge] |> List.removeManyAt minIndex newLength

    let newShortList = 
        match tryUpdateAt indexChange inputPortId shortList  with
        | Some newList -> newList
        | None -> 
            printfn "Index out of range"
            shortList

    printfn " oldSHortList: %A newShortList: %A" shortList newShortList

    // need to delete the previous 2

    printfn "Old Pmap: %A" newPmap
    
    let front, back = newPmap |> List.splitAt minIndex

    let result = front @ newShortList @ back

    printfn "new pmap: %A" result

    // let result =
    //     symbolToOrder.PortMaps.Order[edge]
    //     |> List.mapi (fun i x -> if i >= minIndex && i < minIndex + List.length newPortMapList then newPortMapList.[i - minIndex] else x)

    
    result



let changePortOrder (wModel: BusWireT.Model)
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol)
    (interWires: list<Wire>) =

    let sModel = wModel.Symbol

    printfn " PortMaps.Order before: %A" symbolToOrder.PortMaps.Order[Left]
    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label} {symbolToOrder.Id }, Other:{otherSymbol.Component.Label}"

    let updatedSymbol = 
        (symbolToOrder, interWires)
        ||> List.fold (fun symbol wire -> 

            //need to change port maps and .Port global map when permuting the order
            // we may need to also factor in rotated symbols which change the orientation but stay the same on paper

            let outputPortId = wire.OutputPort // port id of wire exiting
            let inputPortId = wire.InputPort // port id of  wire entering
                        
            let outputEdge = getPortOrientation sModel  (OutputId outputPortId)
            let inputEdge = getPortOrientation sModel  (InputId inputPortId)
           
            // let outputPortList = 
            //     interWires
            //     |> List.map(fun wire -> string wire.OutputPort)

            // let inputPortList = 
            //     interWires
            //     |> List.map(fun wire -> string wire.InputPort)

            // let outputLength = otherSymbol.PortMaps.Order[outputEdge].Length - 1
            // let inputLength = symbolToOrder.PortMaps.Order[inputEdge].Length - 1

            // returns new symbol
            let newPortMapOrder =
                match inputEdge, outputEdge with
                | Top, Bottom | Bottom, Top | Left, Right | Right, Left | Top, Top | Bottom, Bottom | Right, Right | Left, Left
                | Top, Left | Left, Top | Top, Right | Bottom, Left | Left, Bottom | Right, Top | Right, Bottom | Bottom, Right ->
                
                    // Assume we're working on case Right/Left: portMaps.Order[left], we have the wire
                    // make a function that takes in the inputPortMap[ipEdge] and OutputPorMap.Order[opedge]]
                    // returns the ports that are only present in the interwires list as a portMap list

                    // then we feed in the new portmaps to the findIndexShifted (returns 0) and to the updatePortMapList
                    // with the new portmap list we need to traverse over the old port map list and replace that section of list with this new list

                    let newInputList, newOutputList = getNewPortMap symbol otherSymbol inputEdge outputEdge interWires

                    let outputPortIndex =
                        newOutputList 
                        |> List.findIndex (fun elm -> elm = string outputPortId)

                    // in indexChange we need to change symbolToOrder length to match new index and the outputPortIndex
                    let remainder = findIndexShifted interWires symbol otherSymbol inputEdge outputEdge newInputList newOutputList
                    let indexChange = newInputList.Length - (outputPortIndex) - 1 + remainder
                    printfn"remaindder %A" remainder

                    //let indexChange = symbolToOrder.PortMaps.Order[inputEdge].Length - (outputPortIndex) - 1 + remainder
                    printfn " Port Index: %A  %A" outputPortIndex indexChange
                    //printfn " Port Index before : %A \n %A" indexChange symbolToOrder.PortMaps.Order[inputEdge].[indexChange]

                    //printfn "Old port: %A" newInputList 
                    //let newPortMapList = updatePortMapList symbol indexChange (string inputPortId) inputEdge newInputList
                    //printfn "New port: %A" newPortMapList
    
                    let newPortMapList' = changeOldPortMaps symbol newInputList inputEdge indexChange (string inputPortId)

                    printfn "PortMapList' is: %A" newPortMapList'

                    // need to have another function here to replace the oldPortMap list with the newShiftedPortMapList
                    //printfn "Input Edge is: %A" inputEdge
                    let newOrder = 
                        symbol.PortMaps.Order
                            |> Map.add inputEdge newPortMapList'

                    newOrder
            
            { symbol with PortMaps = { symbol.PortMaps with Order = newPortMapOrder } }
            )

    updatedSymbol

    // | Top, Right | Bottom, Left -> untested but done | Bottom, Right| Left, Bottom -| Right, Top | Right, Bottom 

/// Find all the InterConnecting Wires between 2 symbols given WireModel and the 2 symbols
let findInterconnectingWires (wireList:List<Wire>) (sModel)
    (symbolToOrder:Symbol) 
    (otherSymbol:Symbol) (getAllConnections:int) =
        
    wireList
    |> List.filter (fun value ->

        let inputPortHostId = string sModel.Ports[string value.InputPort].HostId
        let outputPortHostId = string  sModel.Ports[string value.OutputPort].HostId

        let symbolToOrderId = string symbolToOrder.Id
        let otherSymbolId = string otherSymbol.Id
        
        if (getAllConnections = 0) then
            (inputPortHostId = symbolToOrderId) && (outputPortHostId = otherSymbolId)
        else
            if ((inputPortHostId = symbolToOrderId) && (outputPortHostId = otherSymbolId)) then true
            elif ((inputPortHostId = otherSymbolId) && (outputPortHostId = symbolToOrderId)) then true
            else false

        )


type BusWireHelpers = {
    updateWire: Model -> Wire -> bool -> Wire
    updateSymbolWires: Model -> ComponentId -> Model
    }

let reOrderPorts 
    (wModel: BusWireT.Model) 
    (symbolToOrder: Symbol) 
    (otherSymbol: Symbol) 
    (busWireHelper: BusWireHelpers)
        : BusWireT.Model =

    printfn $"ReorderPorts: ToOrder:{symbolToOrder.Component.Label} {symbolToOrder.Id }, Other:{otherSymbol.Component.Label}"

    let sModel = wModel.Symbol

    let wireList =
        wModel.Wires
        |> Map.toList
        |> List.map snd

    let allWires = findInterconnectingWires wireList sModel symbolToOrder otherSymbol 1
    let wiresToOrder = findInterconnectingWires allWires sModel symbolToOrder otherSymbol 0 

    printfn "Wire length is %A" wiresToOrder.Length

    // we might need to seperate wires so that input edge and output edges are same
    let symbol' = changePortOrder wModel symbolToOrder otherSymbol wiresToOrder

    let newModel = 
        {wModel with 
            Symbol = {sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols}
        }

    printfn " PortMaps.Order after: %A" symbol'.PortMaps.Order[Left]
    let wModel' = busWireHelper.updateSymbolWires newModel symbol'.Id

    wModel'

    

// Will require for Group Task
let sheetReOrderPorts 
    (wModel: BusWireT.Model) 
    (busWireHelper: BusWireHelpers)
        : BusWireT.Model =

    printfn $"Ordering the whole sheet"

    // let sModel = wModel.Symbol

    // let wireList =
    //     wModel.Wires
    //     |> Map.toList
    //     |> List.map snd

    // let symbolList = 
    //     sModel.Symbols
    //     |> Map.toList
    //     |> List.map snd

    // let sModel' = 
    //     (sModel, wireList)
    //     ||> List.fold (fun symbol wire ->

    //         let outputPortId = string wire.OutputPort // port id of wire exiting
    //         let inputPortId = string wire.InputPort // port id of  wire entering

    //         let symbolToOrder = getSymbol symbol inputPortId
    //         let otherSymbol = getSymbol symbol outputPortId

    //         let symbol' = changePortOrder wModel symbolToOrder otherSymbol [wire]

    //         {sModel with Symbols = Map.add symbol'.Id symbol' symbol.Symbols}
    //     )

    // let newModel = 
    //     {wModel with 
    //         Symbol = sModel'
    //     }

    // //printfn " PortMaps.Order after: %A" symbol'.PortMaps.Order[Left]

    // let wModel' = 
    //     (newModel, symbolList)
    //     ||> List.fold (fun model symbol ->

    //         busWireHelper.updateSymbolWires model symbol.Id    
    //     )
    

    wModel


