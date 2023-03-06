module SmartPortOrder
open BusWireUpdateHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Optics
open Operators

// Authored exclusively by Josiah Begley CID: 01846829
// wire updated handeled in SheetUpdated.fs
(* 
    HLP23: This module will normally be used exclusively by team member doing the "smart port reorder" 
    part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    Normally it will update multiple wires and symbols in the BusWire model so could use the SmartHelper 
    functions for this purpose.
*)
(*
    To test this, it must be given two symbols interconnected by wires. It then reorders the ports on
    symbolToOrder so that the connecting wires do not cross.
    Tt should work out the interconnecting wires (wiresToOrder) from
    the two symbols, wModel.Wires and sModel.Ports
    It will do nothing if symbolToOrder is not a Custom component (which has re-orderable ports).
*)
let reOrderPorts (wModel: BusWireT.Model) (symbolToOrder: Symbol) (otherSymbol: Symbol): BusWireT.Model =
    // gets the symbol model used for symbol manipulation
    let sModel = wModel.Symbol
    printfn $"symbolToOrder.Component.InputPorts.Id: {symbolToOrder.Component.InputPorts |> List.map (fun x -> x.Id)}"
     
    // gets the list of wires connected between the two selected components
    let wiresToOrder =
        [ [symbolToOrder.Id]; [otherSymbol.Id] ]
        |> List.map (getConnectedWires wModel)
        |> (fun lst -> Set.intersect ((List.head lst) |> Set) ((List.head (List.tail lst)) |> Set))
        |> Set.toList
  
    // uses the list of wires to determine a list of connected port ids
    let portConnections =
        wiresToOrder
        |> List.map (fun x -> (x.OutputPort, x.InputPort))
        |> List.map (fun (outputId, inputId) -> (outputId.ToString(), inputId.ToString()))
    
    // check which of the two selected symbols is input and which is output
    let AtoBCheck: bool =
        let outputIds = otherSymbol.Component.OutputPorts |> List.map (fun x -> x.Id)
        portConnections |> List.forall (fun (x, _) -> List.contains x outputIds)

    // generates symbol map for each component
    let symbolAPortMap =
        match AtoBCheck with
            | true -> otherSymbol.Component.OutputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
            | false -> otherSymbol.Component.InputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
        
    let symbolBPortMap =
        match AtoBCheck with
            | true -> symbolToOrder.Component.InputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
            | false -> symbolToOrder.Component.OutputPorts
                    |> List.map (fun port -> (port.Id, port.PortNumber |> Option.defaultValue 0))
                    |> Map.ofList
        
    
    let inline reverseTuple tupleList =
        tupleList
        |> List.map (fun (x,y) -> (y,x))
    
    // used to sort a list of port connections when some connections dont exist (None)
    let sortList myList =
        myList |> List.sortWith(fun (x1, y1) (x2, y2) ->
            match y1, y2 with
            | None, None -> 0
            | None, _ -> 1
            | _, None -> -1
            | _, _ -> compare y1 y2)

    // returns the port connections by port number
    let getConnectedNumbers (map1: Map<string, int>) (map2: Map<string, int>) (connections: (string * string) list) : (int * int option) list =
        let map1' =
            map1
            |> Map.toList
            |> List.sortBy snd
            |> List.rev
            |> List.mapi (fun i (id, _) -> (id, i))
            |> Map.ofList
            
        let generalList =
            map1'
            |> Seq.map (fun x ->
                match List.tryFind (fun (_, id2) -> id2 = x.Key) (reverseTuple connections) with
                | Some (id1, _) ->
                    match map2.TryFind(id1) with
                    | Some (int2) -> (x.Value, Some(int2))
                    | None -> (x.Value, None)
                | None -> (x.Value, None))
            |> List.ofSeq
            |> List.sortBy fst

        match AtoBCheck with
            | true -> List.sortByDescending fst generalList
            | false -> sortList generalList

    // gets port connections
    let connectedNumbers = match AtoBCheck with
                                | true -> getConnectedNumbers symbolAPortMap symbolBPortMap portConnections
                                | false -> getConnectedNumbers symbolBPortMap symbolAPortMap portConnections
   
    printfn $"RIGHT HERE:{connectedNumbers}"
    
    // reorders the ports based on the existing port order and the port connections
    let reorderList (portIds: string list) (connections: (int*int option) list) =

        let filteredList = 
            portIds 
            |> List.mapi (fun i x -> (i, x)) 
            |> List.filter (fun (i, _) -> 
                not (List.exists (fun (_, index) -> index = Some i) connections)) 
            |> List.map snd

        let mutable filteredIndex = 0

        match AtoBCheck with
        | true -> match portIds.Length with
                  | 0 -> portIds
                  | _ -> List.map (fun (_,index) -> match index with
                                                        | Some int -> portIds.[int]
                                                        | None -> 
                                                            let filtered = filteredList.[filteredIndex]
                                                            filteredIndex <- filteredIndex + 1
                                                            filtered) connections
        | false -> match portIds.Length with
                   | 0 -> portIds
                   | _ -> List.map (fun (index,_) -> portIds[index]) connections
 
      
    // updates the corresponding area of the portMap      
    let updatedMapOrder =
        match AtoBCheck with
            | true -> let portMap =  Map.find Left symbolToOrder.PortMaps.Order
                      let reorderedList = reorderList portMap connectedNumbers
                      symbolToOrder.PortMaps.Order |> Map.add Left reorderedList
                      
            | false -> let portMap =  Map.find Right symbolToOrder.PortMaps.Order 
                       let reorderedList = reorderList portMap connectedNumbers
                       symbolToOrder.PortMaps.Order |> Map.add Right (reorderedList |> List.rev)
    
    
    let updatedPortMaps = { symbolToOrder.PortMaps with Order = updatedMapOrder }
    let symbol' = { symbolToOrder with PortMaps = updatedPortMaps }
    
    { wModel with
        Wires = wModel.Wires //wires update call handled in SheetUpdate
        Symbol = { sModel with Symbols = Map.add symbol'.Id symbol' sModel.Symbols } } //model updated with updated symbol with updated port map
    