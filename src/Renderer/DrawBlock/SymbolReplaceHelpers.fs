module SymbolReplaceHelpers


open CommonTypes
open Fable.React
open System.Text.RegularExpressions
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators
open System



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
        | NbitsXor _ -> NbitsXor newBits
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
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c
        
    set (component_ >-> type_) newcompotype symbol


/// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"

    set (component_ >-> type_) newcompotype symbol

/// This function should be called for Input1 components only. Sets the default
/// value to be used in simulations for an Input1 component if it is not driven.
let changeInputValue (symModel: Model) (compId: ComponentId) (newVal: int) =
    let symbol = Map.find compId symModel.Symbols
    let width =
        match symbol.Component.Type with
        | Input1 (width, _) -> width
        | _ -> failwithf "changeInputValue should only be called for Input components"

    set (component_ >-> type_) (Input1 (width, Some newVal)) symbol

/// Updates the value of a constant1 component and returns the updated symbol
let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
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

let changeAdderComponent (symModel: Model) (compId: ComponentId) (oldComp:Component) (newCompType: ComponentType) =
    let createNewPort no hostID portType = 
            {
                Id = JSHelpers.uuid ()
                PortNumber = Some no
                PortType = portType
                HostId = hostID
            } 
    let oldCompType = oldComp.Type
    let portNoDown (port:Port) =
        let no = port.PortNumber |> Option.get
        {port with PortNumber = Some (no-1)}
    let portNoUp (port:Port) =
        let no = port.PortNumber |> Option.get
        {port with PortNumber = Some (no+1)}
    let changeInputPortList (inputPortList:Port list) =
        inputPortList
        |> List.collect (fun port ->
            match oldCompType,newCompType with
            |NbitsAdder _,NbitsAdderNoCin _ 
            |NbitsAdderNoCout _, NbitsAdderNoCinCout _-> 
                match port.PortNumber with
                |Some 0 -> []
                |_ -> [portNoDown port]
            |NbitsAdderNoCin _ , NbitsAdder _ 
            |NbitsAdderNoCinCout _ , NbitsAdderNoCout _ ->
                match port.PortNumber with
                |Some 0 -> [(createNewPort 0 oldComp.Id PortType.Input);portNoUp port]
                |_ -> [portNoUp port]
            |_,_ -> [port] 
        )

    let changeOutputPortList (outputPortList:Port list) =
        outputPortList
        |> List.collect (fun port ->
            match oldCompType,newCompType with
            |NbitsAdder _,NbitsAdderNoCout _ 
            |NbitsAdderNoCin _, NbitsAdderNoCinCout _-> 
                match port.PortNumber with
                |Some 0 -> [port]
                |_ -> []
            |NbitsAdderNoCout _ , NbitsAdder _ 
            |NbitsAdderNoCinCout _ , NbitsAdderNoCin _ ->
                match port.PortNumber with
                |Some x -> [port; createNewPort 1 oldComp.Id PortType.Output]
                |_ -> []
            |_,_ -> [port]
        )

    let inputEdge (rotation:Rotation) flipped =
        match rotation,flipped with
        |Degree0,false |Degree0,true -> Bottom
        |Degree90,false |Degree270,true -> Right
        |Degree180,true |Degree180,false  -> Top
        |Degree270,false |Degree90,true -> Left


    let changePortMaps rotation flipped (oldMaps:PortMaps) addedId removedId =
        let order,orientation = oldMaps.Order, oldMaps.Orientation
        match addedId,removedId with
        |None, Some i ->
            let edge = Map.find i orientation
            let newOrientation = Map.remove i orientation
            let onEdge = Map.find edge order
            let newOnEdge = List.filter(fun x -> x<>i) onEdge
            let newOrder = Map.add edge newOnEdge order
            {Order=newOrder;Orientation=newOrientation}
        |Some i, None ->
            let edge = 
                match oldCompType,newCompType with
                |NbitsAdderNoCin _,NbitsAdder _
                |NbitsAdderNoCinCout _, NbitsAdderNoCout _-> inputEdge rotation flipped
                |NbitsAdderNoCout _, NbitsAdder _
                |NbitsAdderNoCinCout _,NbitsAdderNoCin _-> Map.find oldComp.OutputPorts[0].Id orientation
                |_ -> failwithf "Can't happen"
            let newOrientation = Map.add i edge orientation
            let onEdge = Map.find edge order
            let newOrder = 
                match flipped with
                |false -> Map.add edge (onEdge@[i]) order
                |true -> Map.add edge ([i]@onEdge) order
            {Order=newOrder;Orientation=newOrientation}
        |_,_ -> oldMaps

    let symbol = Map.find compId symModel.Symbols
    
    //printfn "here"
    let newInputPorts = (changeInputPortList symbol.Component.InputPorts)
    let newOutputPorts = (changeOutputPortList symbol.Component.OutputPorts)
    let removedId = 
        match oldCompType,newCompType with
        |NbitsAdder _,NbitsAdderNoCin _
        |NbitsAdderNoCout _,NbitsAdderNoCinCout _-> Some symbol.Component.InputPorts[0].Id
        |NbitsAdder _,NbitsAdderNoCout _
        |NbitsAdderNoCin _,NbitsAdderNoCinCout _-> Some symbol.Component.OutputPorts[1].Id
        |_ -> None 

    let addedId =
        match oldCompType,newCompType with
        |NbitsAdderNoCin _,NbitsAdder _
        |NbitsAdderNoCinCout _, NbitsAdderNoCout _-> Some newInputPorts[0].Id
        |NbitsAdderNoCout _, NbitsAdder _
        |NbitsAdderNoCinCout _,NbitsAdderNoCin _-> Some newOutputPorts[1].Id
        |_ -> None 
    
    let newPortMaps = changePortMaps symbol.STransform.Rotation symbol.STransform.flipped symbol.PortMaps addedId removedId
    
    

    symbol
    |> set portMaps_ newPortMaps
    |> map component_ (
        set type_ newCompType >>
        set inputPorts_ newInputPorts >>
        set outputPorts_ newOutputPorts
        )
    


let changeCounterComponent (symModel: Model) (compId: ComponentId) (oldComp:Component) (newCompType: ComponentType) =
    let createNewPort no hostID portType = 
            {
                Id = JSHelpers.uuid ()
                PortNumber = Some no
                PortType = portType
                HostId = hostID
            } 
    let oldCompType = oldComp.Type
    let portNoDown (port:Port) =
        let no = port.PortNumber |> Option.get
        {port with PortNumber = Some (no-1)}
    let portNoUp (port:Port) =
        let no = port.PortNumber |> Option.get
        {port with PortNumber = Some (no+1)}
    let symbol = Map.find compId symModel.Symbols
    let oldInputList = symbol.Component.InputPorts
    let newInputPorts =
        match oldCompType,newCompType with
        |Counter _,CounterNoLoad _ ->
            [portNoDown (portNoDown oldInputList[2])]        
        |CounterNoEnable _, CounterNoEnableLoad _-> 
            []
        |CounterNoLoad _ , Counter _ ->
            [(createNewPort 0 oldComp.Id PortType.Input);(createNewPort 1 oldComp.Id PortType.Input);portNoUp (portNoUp oldInputList[0])]
        |CounterNoEnableLoad _ , CounterNoEnable _ ->
            [(createNewPort 0 oldComp.Id PortType.Input);(createNewPort 1 oldComp.Id PortType.Input)]
        |Counter _, CounterNoEnable _ ->
            [oldInputList[0];oldInputList[1]]
        |CounterNoLoad _, CounterNoEnableLoad _ ->
            []
        |CounterNoEnableLoad _ , CounterNoLoad _ ->
            [(createNewPort 0 oldComp.Id PortType.Input)]
        |CounterNoEnable _, Counter _ ->
            oldInputList@[(createNewPort 2 oldComp.Id PortType.Input)]
        |_,_ -> oldInputList 
        

    let findOpposite (edge:Edge) =
        match edge with
        |Right -> Left
        |Top -> Bottom
        |Left -> Right
        |Bottom -> Top
    
    let changePortMaps flipped (oldMaps:PortMaps) removedId1 removedId2 added1 added2 =
        let order,orientation = oldMaps.Order, oldMaps.Orientation
        let edge = findOpposite (Map.find oldComp.OutputPorts[0].Id orientation)
        match removedId1,removedId2,added1,added2 with
        |Some i1, Some i2,None,None ->
            let newOrientation = Map.remove i1 orientation
            let newOrientation' =  Map.remove i2 newOrientation
            let onEdge = Map.find edge order
            let newOnEdge = List.filter(fun x -> (x<>i1)) onEdge
            let newOnEdge' = List.filter(fun x -> (x<>i2)) newOnEdge
            let newOrder = Map.add edge newOnEdge' order
            {Order=newOrder;Orientation=newOrientation'}
        |Some i, None, None, None ->
            let newOrientation = Map.remove i orientation
            let onEdge = Map.find edge order
            let newOnEdge = List.filter(fun x -> x<>i) onEdge
            let newOrder = Map.add edge newOnEdge order
            {Order=newOrder;Orientation=newOrientation}
        |None,None,Some i1, Some i2 ->
            let newOrientation = Map.add i1 edge orientation
            let newOrientation' = Map.add i2 edge newOrientation
            let onEdge = Map.find edge order
            let newOrder = 
                match flipped with
                |false -> Map.add edge ([i1;i2]@onEdge) order
                |true -> Map.add edge (onEdge@[i1;i2]) order
            {Order=newOrder;Orientation=newOrientation'}
        |None,None,Some i,None ->
            let newOrientation = Map.add i edge orientation
            let onEdge = Map.find edge order
            let newOrder = 
                match flipped with
                |false -> Map.add edge (onEdge@[i]) order
                |true -> Map.add edge ([i]@onEdge) order
            {Order=newOrder;Orientation=newOrientation}
        |_,_,_,_ -> oldMaps

    
    
    //printfn "here"
    let removedId1, removedId2 = 
        match oldCompType,newCompType with
        |Counter _,CounterNoLoad _
        |CounterNoEnable _,CounterNoEnableLoad _-> Some symbol.Component.InputPorts[0].Id, Some symbol.Component.InputPorts[1].Id
        |Counter _,CounterNoEnable _ -> Some symbol.Component.InputPorts[2].Id, None
        |CounterNoLoad _,CounterNoEnableLoad _-> Some symbol.Component.InputPorts[0].Id, None
        |_,_ -> None, None

    let added1,added2 =
        match oldCompType,newCompType with
        |CounterNoLoad _, Counter _
        |CounterNoEnableLoad _, CounterNoEnable _ -> Some newInputPorts[0].Id, Some newInputPorts[1].Id
        |CounterNoEnable _,Counter _ -> Some newInputPorts[2].Id, None
        |CounterNoEnableLoad _ , CounterNoLoad _ -> Some newInputPorts[0].Id, None
        |_,_ -> None, None

    let newPortMaps = changePortMaps symbol.STransform.flipped symbol.PortMaps removedId1 removedId2 added1 added2
    let h',w' = match getComponentProperties newCompType "" with |_,_,h,w -> h,w
    
    symbol
    |> set portMaps_ newPortMaps
    |> map component_ (
        set type_ newCompType >>
        set inputPorts_ newInputPorts >>
        set h_ h' >>
        set w_ w'
        )



