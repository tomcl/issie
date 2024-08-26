module SymbolReplaceHelpers


open CommonTypes
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators




/// Helper function to change the number of bits expected in a port of each component type.
let changeNumberOfBitsf (symModel:Model) (compId:ComponentId) (newBits : int) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | Input1 (_, defaultVal) -> Input1 (newBits, defaultVal)
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsAdderNoCin _ -> NbitsAdderNoCin newBits
        | NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout newBits
        | NbitsAdderNoCout _ -> NbitsAdderNoCinCout newBits
        | NbitsXor(_, typ) -> NbitsXor(newBits,typ)
        | NbitsAnd _ -> NbitsAnd newBits
        | NbitsOr _ -> NbitsOr newBits
        | NbitsNot _ -> NbitsNot newBits
        | NbitSpreader _ -> NbitSpreader newBits 
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | Counter _ -> Counter newBits
        | CounterNoEnable _ -> CounterNoEnable newBits
        | CounterNoLoad _ -> CounterNoLoad newBits
        | CounterNoEnableLoad _ -> CounterNoEnableLoad newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_,b) -> BusSelection (newBits,b)
        | BusCompare (_,b) -> BusCompare (newBits,b)
        | BusCompare1 (_,v,t) -> BusCompare1 (newBits,v,t) 
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c
        
    set (component_ >-> type_) newcompotype symbol


/// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:bigint) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, newLsb)
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"

    set (component_ >-> type_) newcompotype symbol

/// This function should be called for Input1 components only. Sets the default
/// value to be used in simulations for an Input1 component if it is not driven.
let changeInputValue (symModel: Model) (compId: ComponentId) (newVal: bigint) =
    let symbol = Map.find compId symModel.Symbols
    let width =
        match symbol.Component.Type with
        | Input1 (width, _) -> width
        | _ -> failwithf "changeInputValue should only be called for Input components"

    set (component_ >-> type_) (Input1 (width, Some newVal)) symbol

/// Updates the value of a constant1 component and returns the updated symbol
let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:bigint) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    
    set (component_ >-> type_) newcompotype symbol

/// Updates the value of a busCompare1 component and returns the updated symbol
let changeBusComparef (symModel:Model) (compId:ComponentId) (constantVal:bigint) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | BusCompare1 (w, _, _) -> BusCompare1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    
    set (component_ >-> type_) newcompotype symbol

let changeReversedInputs (symModel: Model) (compId: ComponentId) =
    let symbol = Map.find compId symModel.Symbols
    let newValue =
        match symbol.ReversedInputPorts with
        |Some false -> Some true
        |Some true -> Some false
        |None -> Some true
    let newSymbolInfo = 
        match symbol.Component.SymbolInfo with
        |Some si -> Some {si with ReversedInputPorts = newValue}
        |None -> None
    let newcompo = {symbol.Component with SymbolInfo = newSymbolInfo }
    {symbol with Component = newcompo; ReversedInputPorts = newValue}


let createNewPort no hostID portType = 
            {
                Id = JSHelpers.uuid ()
                PortNumber = Some no
                PortType = portType
                HostId = hostID
            } 

let updateSymPortMaps newPortMaps newInputPorts newOutputPorts sym =
    sym
    |> set portMaps_ newPortMaps
    |> map component_ (
        set inputPorts_ newInputPorts >>
        set outputPorts_ newOutputPorts
        )

let portNoDown (port:Port) =
    let no = port.PortNumber |> Option.get
    {port with PortNumber = Some (no-1)}
let portNoUp (port:Port) =
    let no = port.PortNumber |> Option.get
    {port with PortNumber = Some (no+1)}

/// add the port specified by its type and number to the given symbol
/// if edgeOpt is None the port will be added on the same edge as the port
/// with the highest number
let addNumberPort (pType: PortType) (pNum: int) (sym: Symbol) (edgeOpt: Edge option) =
    let newPort = (createNewPort pNum sym.Component.Id pType)
    let newInputPorts, newOutputPorts =
        match pType with
        | PortType.Input ->
            sym.Component.InputPorts[..pNum-1] @ [newPort] @ List.map portNoUp sym.Component.InputPorts[pNum..],
            sym.Component.OutputPorts
        | PortType.Output ->
            sym.Component.InputPorts,
            sym.Component.OutputPorts[..pNum-1] @ [newPort] @ List.map portNoUp sym.Component.OutputPorts[pNum..]
    let insertIndex =
        match sym.STransform.flipped with
        | false -> 
            match pType with
            | PortType.Input -> pNum
            | PortType.Output -> 0
        | true ->
            match pType with
            | PortType.Input -> sym.Component.InputPorts.Length - 1 - pNum
            | PortType.Output -> sym.Component.OutputPorts.Length - 1 - pNum

    let addToPortMaps (port: Port) (edge: Edge) (sym: Symbol) =
        let order, orientation = sym.PortMaps.Order, sym.PortMaps.Orientation
        let newOrientation = Map.add newPort.Id edge orientation
        let edgeOrder = Map.find edge order
        let newOrder =
            Map.add edge (List.insertAt insertIndex port.Id edgeOrder) order
        {Order = newOrder; Orientation = newOrientation}
    
    let getDefaultEdge pType =
        match pType with
        | PortType.Input -> (List.tryItem (sym.Component.InputPorts.Length-1) sym.Component.InputPorts, Left)
        | PortType.Output -> (List.tryItem (sym.Component.OutputPorts.Length-1) sym.Component.OutputPorts, Right)
        ||> fun portOpt defaultEdge ->
            (Option.map (fun (port: Port) -> port.Id) portOpt, defaultEdge)
        ||> fun idOpt defaultEdge ->
            (Option.map (fun id -> Map.find id sym.PortMaps.Orientation) idOpt, defaultEdge)
        ||> fun edgeOpt defaultEdge ->
            Option.defaultValue defaultEdge edgeOpt
        
    let newPortMaps = addToPortMaps newPort (Option.defaultWith (fun _ -> getDefaultEdge pType) edgeOpt) sym

    updateSymPortMaps newPortMaps newInputPorts newOutputPorts sym

/// remove the port specified by its type and number from the given symbol
let deleteNumberPort (pType: PortType) (pNum: int) (sym: Symbol) =
    let pIdOpt, newInputPorts, newOutputPorts =
        match pType with
        | PortType.Input ->
            sym.Component.InputPorts
            |> List.tryItem pNum
            |> Option.map (fun port -> port.Id),
            sym.Component.InputPorts[..pNum-1] @ List.map portNoDown sym.Component.InputPorts[pNum+1..],
            sym.Component.OutputPorts
        | PortType.Output ->
            sym.Component.OutputPorts
            |> List.tryItem pNum
            |> Option.map (fun port -> port.Id),
            sym.Component.InputPorts,
            sym.Component.OutputPorts[..pNum-1] @ List.map portNoDown sym.Component.OutputPorts[pNum+1..]
    
    let removeFromPortMaps (pId) (sym: Symbol) =
        let order, orientation = sym.PortMaps.Order, sym.PortMaps.Orientation
        let edge = Map.find pId orientation
        let newOrientation = Map.remove pId orientation
        let edgeOrder = Map.find edge order
        let newEdgeOrder =
            edgeOrder
            |> List.filter (fun id -> id <> pId)
        let newOrder = Map.add edge newEdgeOrder order
        {Order = newOrder; Orientation = newOrientation}

        
    let newPortMaps =
        pIdOpt
        |> Option.map (fun id -> removeFromPortMaps id sym)
        |> Option.defaultValue sym.PortMaps
    
    updateSymPortMaps newPortMaps newInputPorts newOutputPorts sym

/// add the ports specified by the type and the list of port numbers
/// if edgeOpt is None they will be added on the same edge as the port
/// with the highest number
let addPorts (pType: PortType) (pNumList: int list) (edgeOpt: Edge option) (sym: Symbol) =
    (sym, pNumList)
    ||> List.fold (fun sym pNum -> addNumberPort pType pNum sym edgeOpt)

/// delete ports specified by the list of port numbers
/// must be in ascending order because otherwise wrong ports will be deleted
let deletePorts (pType: PortType) (pNumList: int list) (sym: Symbol) =
    (sym, List.rev pNumList) // reverse list to avoid deleting wrong elements
    ||> List.fold (fun sym pNum -> deleteNumberPort pType pNum sym)
    

/// add and remove ports to obtain the given number of input and output ports
/// the ports will be added at the highest index, as well as removed from the end
/// of the port list
let varyNumberOfPorts (pType: PortType) (numInPorts: int) (numOutPorts: int) (sym: Symbol) =
    let comp = sym.Component
    
    match comp.InputPorts.Length, comp.OutputPorts.Length, numInPorts, numOutPorts with
    | oldNIn, oldNOut, newNIn, newNOut when (newNIn >= oldNIn && newNOut >= oldNOut) ->
        sym
        |> addPorts PortType.Input [oldNIn..newNIn-1] None
        |> addPorts PortType.Output [oldNOut..newNOut-1] None
    | oldNIn, oldNOut, newNIn, newNOut when (newNIn >= oldNIn && newNOut < oldNOut) ->
        sym
        |> addPorts PortType.Input [oldNIn..newNIn-1] None
        |> deletePorts PortType.Output [newNOut..oldNOut-1]
    | oldNIn, oldNOut, newNIn, newNOut when (newNIn < oldNIn && newNOut >= oldNOut) ->
        sym
        |> deletePorts PortType.Input [newNIn..oldNIn-1]
        |> addPorts PortType.Output [oldNOut..newNOut-1] None
    | oldNIn, oldNOut, newNIn, newNOut when (newNIn < oldNIn && newNOut < oldNOut) ->
        sym
        |> deletePorts PortType.Input [newNIn..oldNIn-1]
        |> deletePorts PortType.Output [newNOut..oldNOut-1]
    | _ -> failwithf "new port counts can't be obtained"


let changeAdderComponent (symModel: Model) (compId: ComponentId) (oldComp:Component) (newCompType: ComponentType) =
    let oldCompType = oldComp.Type

    let inputEdge (rotation:Rotation) flipped =
        match rotation,flipped with
        |Degree0,false |Degree0,true -> Bottom
        |Degree90,false |Degree270,true -> Right
        |Degree180,true |Degree180,false  -> Top
        |Degree270,false |Degree90,true -> Left

    
    let symbol = Map.find compId symModel.Symbols
    let removeL, removePType = 
        match oldCompType,newCompType with
        |NbitsAdder _,NbitsAdderNoCin _
        |NbitsAdderNoCout _,NbitsAdderNoCinCout _-> [0], PortType.Input
        |NbitsAdder _,NbitsAdderNoCout _
        |NbitsAdderNoCin _,NbitsAdderNoCinCout _-> [1], PortType.Output
        |_ -> [], PortType.Input

    let addL, addPType =
        match oldCompType,newCompType with
        |NbitsAdderNoCin _,NbitsAdder _
        |NbitsAdderNoCinCout _, NbitsAdderNoCout _-> [0], PortType.Input
        |NbitsAdderNoCout _, NbitsAdder _
        |NbitsAdderNoCinCout _,NbitsAdderNoCin _-> [1], PortType.Output
        |_ -> [], PortType.Input


    let getEdge() =
        if addL <> [] then
            match oldCompType,newCompType with
            |NbitsAdderNoCin _,NbitsAdder _
            |NbitsAdderNoCinCout _, NbitsAdderNoCout _-> Some (inputEdge symbol.STransform.Rotation symbol.STransform.flipped)
            |NbitsAdderNoCout _, NbitsAdder _
            |NbitsAdderNoCinCout _,NbitsAdderNoCin _-> Some (Map.find oldComp.OutputPorts[0].Id symbol.PortMaps.Orientation)
            |_ -> failwithf "Can't happen"
        else
            None
    
    symbol
    |> addPorts addPType addL (getEdge())
    |> deletePorts removePType removeL
    |> map component_ (
        set type_ newCompType
        )
    


let changeCounterComponent (symModel: Model) (compId: ComponentId) (oldComp:Component) (newCompType: ComponentType) =
    let oldCompType = oldComp.Type
    let symbol = Map.find compId symModel.Symbols

    let findOpposite (edge:Edge) =
        match edge with
        |Right -> Left
        |Top -> Bottom
        |Left -> Right
        |Bottom -> Top

    let edge = findOpposite (Map.find oldComp.OutputPorts[0].Id symbol.PortMaps.Orientation)
    
    let removeL = 
        match oldCompType,newCompType with
        |Counter _,CounterNoLoad _
        |CounterNoEnable _,CounterNoEnableLoad _-> [0; 1]
        |Counter _,CounterNoEnable _ -> [2]
        |CounterNoLoad _,CounterNoEnableLoad _-> [0]
        |_,_ -> []

    let addL =
        match oldCompType,newCompType with
        |CounterNoLoad _, Counter _
        |CounterNoEnableLoad _, CounterNoEnable _ -> [0; 1]
        |CounterNoEnable _,Counter _ -> [2]
        |CounterNoEnableLoad _ , CounterNoLoad _ -> [0]
        |_,_ -> []
    
    let h',w' = match getComponentProperties newCompType "" with |_,_,h,w -> h,w

    symbol
    |> addPorts PortType.Input addL (Some edge)
    |> deletePorts PortType.Input removeL
    |> map component_ (
        set type_ newCompType >>
        set h_ h' >>
        set w_ w'
        )


let changeGateComponent (symModel: Model) (compId: ComponentId) (gateType: GateComponentType) (n: int) =
    Map.find compId symModel.Symbols
    |> varyNumberOfPorts PortType.Input n 1
    |> map component_ (
        set type_ (GateN (gateType, n)) >>
        set h_ (1.5*(float Constants.gridSize) * (float n)/2.)
        )

let changeMergeNComponent (symModel: Model) (compId: ComponentId) (numInputs: int) =
    let numInputsEdit = if numInputs > 2 then numInputs else 3
    Map.find compId symModel.Symbols
    |> varyNumberOfPorts PortType.Input numInputs 1
    |> map component_ (
        set type_ (MergeN numInputs) >>
        set h_ (2.*(float Constants.gridSize) * (float numInputsEdit)/2.)
        )

let changeSplitNComponent (symModel: Model) (compId: ComponentId) (numOutputs: int) (widths: int list) (lsbs: int list)=
    let numOutputsEdit = if numOutputs > 2 then numOutputs else 3
    Map.find compId symModel.Symbols
    |> varyNumberOfPorts PortType.Output 1  numOutputs
    |> map component_ (
        set type_ (SplitN (numOutputs, widths, lsbs)) >>
        set h_ (2.*(float Constants.gridSize) * (float numOutputsEdit)/2.)
        )



