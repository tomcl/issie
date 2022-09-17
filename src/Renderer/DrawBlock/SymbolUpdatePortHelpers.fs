module SymbolUpdatePortHelpers

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators


/// Helper function to find which ports were deleted when changing Adder & Counter component type via properties
let findDeletedPorts (symModel: Model) (compId: ComponentId) (oldComp:Component) (newComp: ComponentType) =
    let symbol = Map.find compId symModel.Symbols
    let oldCompType = oldComp.Type
    let removedIds = 
        match oldCompType,newComp with
        |NbitsAdder _,NbitsAdderNoCin _
        |NbitsAdderNoCout _,NbitsAdderNoCinCout _-> [symbol.Component.InputPorts[0].Id]
        |NbitsAdder _,NbitsAdderNoCout _
        |NbitsAdderNoCin _,NbitsAdderNoCinCout _-> [symbol.Component.OutputPorts[1].Id]
        |Counter _,CounterNoLoad _
        |CounterNoEnable _,CounterNoEnableLoad _-> [symbol.Component.InputPorts[0].Id;symbol.Component.InputPorts[1].Id]
        |Counter _,CounterNoEnable _ -> [symbol.Component.InputPorts[2].Id]
        |CounterNoLoad _,CounterNoEnableLoad _-> [symbol.Component.InputPorts[0].Id]
        |_,_ -> []
    removedIds
    |> List.map (fun x -> Map.tryFind x symModel.Ports)

//////////////  Show Ports Helpers  /////////////////////

let showSymbolInPorts _ sym = 
    set (appearance_ >-> showPorts_) ShowInput sym

let showSymbolOutPorts _ sym = 
     set (appearance_ >-> showPorts_) ShowOutput sym 

let showSymbolBothForPortMovementPorts _ sym =
    set (appearance_ >-> showPorts_) ShowBothForPortMovement sym 

let hideSymbolPorts _ sym = 
    set (appearance_ >-> showPorts_) ShowNone sym 

let showSymbolPorts sym =
    set (appearance_ >-> showPorts_) ShowBoth sym 

/////////////////////////////////////////////////////////

/// Given a model it shows all input ports and hides all output ports, then returns the updated model
let inline showAllInputPorts (model: Model) =
    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolInPorts

    { model with Symbols = newSymbols }

/// Given a model it shows all output ports and hides all input ports, then returns the updated model
let inline showAllOutputPorts (model: Model) =
    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolOutPorts

    { model with Symbols = newSymbols }

/// Given a model it shows all ports of custom components and hides all other ports, then returns the updated model
let inline showAllCustomPorts (model: Model) =
    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolBothForPortMovementPorts

    { model with Symbols = newSymbols }

/// Given a model it hides all ports and returns the updated model
let inline deleteAllPorts (model: Model) =
    let updatedSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    { model with Symbols = updatedSymbols}

/// Given a model it shows all the specified components' ports and hides all the other ones
let inline showPorts (model: Model) compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        match Map.containsKey sId resetSymbols with
        | false -> prevSymbols
        | true ->
            prevSymbols |>
            Map.add sId (showSymbolPorts resetSymbols[sId])

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }


/// Given a model it shows only the custom components of all the specified components' ports and hides all the other ones
/// Different from the above (only custom components).
let inline showCustomPorts (model: Model) compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        match resetSymbols[sId].Component.Type with
        | Custom _ -> prevSymbols |> Map.add sId (showSymbolPorts resetSymbols[sId])
        | _ -> prevSymbols
    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }


let moveCustomPortsPopup() : ReactElement =
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]
    div [] [
    bSpan "Ports" ; tSpan " are the inputs and outputs of a component symbol."
    br []; br []
    ul [Style [ListStyle "disc"; MarginLeft "30px"]]
        [
            li [] [str "Normal components can be rotated and flipped to change port orientation, \
                         however port positions cannot be changed."]

            li [] [ str "2-input MUX components can have 0 & 1 inputs swapped using properties."]
            
            li [] [str "Custom components (sheets inserted as components) can have ports moved \
                        to any side of the symbol and reordered."]
            
            li [] [ 
                    str "To move custom component ports:"
                    ul [Style [ListStyle "circle"; MarginLeft "30px"]]
                        [   
                            li [] [str "Press CTRL and use a mouse to drag \
                                        a port to another position on the outline of the symbol."] 
                            li [] [str "You can reorder ports and place them on any symbol edge including top and bottom." ]           
                            li [] [str "The symbol will resize itself if you change the edge of a port."]
                            li [] [str "If default sizing makes port legends overlap you can scale custom component width and height in Properties"]
                        ]
                ]
            ]
        ]
        
   
/// Returns an Option Edge. Returns Some edge if position is on edge of Symbol, and None if it was not on an edge
/// Separates the symbol as shown below where the two triangles have height = 0.3*symbolHeight
// |-----------|
// |\   TOP   /|
// |  \     /  |       
// |    \ /    |
// |LEFT |     |
// |     |RIGHT|
// |    / \    |
// |  /     \  |
// |/ BOTTOM  \|
// |-----------|
let getCloseByEdge (sym:Symbol) (mousePos:XYPos) : Option<Edge> =
    let h,w = getRotatedHAndW sym
    let triangleCorner = {X=w/2.;Y=h*0.3}
    let tanTheta = triangleCorner.Y/triangleCorner.X 
    let mouseOffset = mousePos - sym.Pos
    let minX = min (abs mouseOffset.X) (abs w-mouseOffset.X)
    let outMargin = 60.  //how many pixels outside the symbol the port can be when moving it
    
    // Top Edge
    if ((-outMargin <= mouseOffset.Y) && (mouseOffset.Y <= (minX*tanTheta)) && ((-outMargin <= mouseOffset.X)) && ((w+outMargin) >= mouseOffset.X)) then Some Top
    // Bottom Edge
    elif (((h+outMargin) >= mouseOffset.Y) && (mouseOffset.Y >= (h - minX*tanTheta)) && ((-outMargin <= mouseOffset.X)) && ((w+outMargin) >= mouseOffset.X)) then Some Bottom
    // Away from symbol -> None
    elif ((-outMargin >= mouseOffset.Y) ||  ((h+outMargin) <= mouseOffset.Y) || (-outMargin >= mouseOffset.X) ||  ((w+outMargin) <= mouseOffset.X)) then None
    // Left Edge
    elif (-outMargin <= mouseOffset.X) && (mouseOffset.X <= (w/2.0)) then Some Left
    // Right Edge
    elif ((w+outMargin) >= mouseOffset.X) && (mouseOffset.X > (w/2.0)) then Some Right
    else None

///Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPosIndex (sym: Symbol) (pos: XYPos) (edge: Edge): int =
    let ports = sym.PortMaps.Order[edge] //list of ports on the same side as port
    //let index = float( List.findIndex (fun (p:string)  -> p = port.Id) ports ) need to find index
    let gap = getPortPosEdgeGap sym.Component.Type 
    let baseOffset = getPortBaseOffset sym edge  //offset of the side component is on
    let pos' = pos - sym.Pos + baseOffset 
    let h,w = getRotatedHAndW sym
    match ports.Length, edge with
    | 0, _ -> 0 
    | _, Left ->
        int (pos'.Y * ( float( ports.Length + 1) + 2.0*gap - 1.0) / float(h)  - gap + 0.5)
    | _, Right -> 
        -1 * int (pos'.Y * ( float( ports.Length + 1 ) + 2.0*gap - 1.0) / float(h) + 1.0 - gap - float( ports.Length + 1) - 0.5)
    | _, Bottom -> 
        int (pos'.X * (float (ports.Length + 1) + 2.0*gap - 1.0) / (float(w)) - gap + 0.5)
    | _, Top ->
        -1 * int (pos'.X * (float (ports.Length + 1) + 2.0*gap - 1.0) / float(w) - float( ports.Length + 1) + 1.0 - gap - 0.5)

let updatePortPos (sym:Symbol) (pos:XYPos) (portId: string) : Symbol =
    match sym.Component.Type with
    | Custom x ->
        let oldMaps = sym.PortMaps
        match getCloseByEdge sym pos with
        | None -> 
            printfn "not on edge"
            {sym with MovingPort = None}
        | Some edge -> 
            printfn $"{edge}"
            let newPortOrientation = oldMaps.Orientation |> Map.add portId edge
            let oldEdge = oldMaps.Orientation[portId]
            let newPortIdx = getPosIndex sym pos edge
            let oldIdx = oldMaps.Order[oldEdge] |> List.findIndex (fun el -> el = portId)
            
            let oldPortOrder' =
                oldMaps.Order 
                |> Map.add oldEdge (oldMaps.Order[oldEdge] |> List.filter (fun el -> el <> portId))
            let newPortIdx' =
                if newPortIdx > oldPortOrder'[edge].Length then oldPortOrder'[edge].Length
                else if edge = oldEdge && oldIdx < newPortIdx then newPortIdx - 1
                else newPortIdx
            printfn $"{(newPortIdx, newPortIdx')}"
            
            let newPortOrder = 
                oldPortOrder'
                |> Map.add edge (oldPortOrder'[edge] |> List.insertAt newPortIdx' portId) // to do then get index and insert at index
            let newSym =
                {sym with 
                    MovingPort = None;
                    PortMaps = {Orientation = newPortOrientation; Order = newPortOrder}
                }
            autoScaleHAndW newSym
    | _ -> {sym with MovingPort = None;}


/// Contains the code for the MovePort update msg
let movePortUpdate (model:Model) (portId:string) (pos:XYPos) : Model*Cmd<'a> =
    
    /// Get a port's position given the symbol, the side the port is on, the number of ports on that side and the index of the port on that side  
    let getPortPosWithIndex (sym: Symbol) portsNumber side portIndex: XYPos =
        let index = float(portIndex)
        let gap = getPortPosEdgeGap sym.Component.Type 
        let topBottomGap = gap + 0.3 // extra space for clk symbol
        let baseOffset = getPortBaseOffset sym side  //offset of the side component is on
        let baseOffset' = baseOffset + getMuxSelOffset sym side
        let portDimension = float portsNumber - 1.0
        let h,w = getRotatedHAndW sym
        match side with
        | Left ->
            let yOffset = float h * ( index + gap )/(portDimension + 2.0*gap)
            baseOffset' + {X = 0.0; Y = yOffset }
        | Right -> 
            let yOffset = float h * (portDimension - index + gap )/(portDimension + 2.0*gap)
            baseOffset' + {X = 0.0; Y = yOffset }
        | Bottom -> 
            let xOffset = float  w * (index + topBottomGap)/(portDimension + 2.0*topBottomGap)
            baseOffset' + {X = xOffset; Y = 0.0 }
        | Top ->
            let xOffset = float w * (portDimension - index + topBottomGap)/(portDimension + 2.0*topBottomGap)
            baseOffset' + {X = xOffset; Y = 0.0 }
    
    /// Helper function to get the X or Y offset of the Moving Port target, when the target is on the same side the port was before 
    let findOffsetSameEdge (symbol:Symbol) edge =
        let portsOnEdge = List.length symbol.PortMaps.Order[edge]
        if portsOnEdge = 1 then 0.0
        elif portsOnEdge = 2 then
            match edge with
            |Bottom -> ((getPortPosWithIndex symbol 2 edge 0).X)/2.0
            |Top -> (-(getPortPosWithIndex symbol 2 edge 1).X)/2.0
            |Left -> ((getPortPosWithIndex symbol 2 edge 0).Y)/2.0
            |Right -> (-(getPortPosWithIndex symbol 2 edge 1).Y)/2.0
        elif portsOnEdge > 2 then
            match edge with
            |Bottom |Top -> ((getPortPosWithIndex symbol portsOnEdge edge 1).X - (getPortPosWithIndex symbol portsOnEdge edge 0).X)/2.0
            | _ -> ((getPortPosWithIndex symbol portsOnEdge edge 1).Y - (getPortPosWithIndex symbol portsOnEdge edge 0).Y)/2.0 
        else 0.0

    /// Helper function to get the X or Y offset of the Moving Port target, when the target is NOT on the same side the port was before        
    let findOffsetDifferentEdge (symbol:Symbol) edge order=
        let portsOnEdge = List.length symbol.PortMaps.Order[edge]
        if portsOnEdge = 0 then 
            match edge with
            | Top | Bottom -> ((snd (getRotatedHAndW symbol))/2.0 )
            | _ -> ((fst (getRotatedHAndW symbol))/2.0 )
        
        elif (portsOnEdge>=1 && ((order=0) || (order=portsOnEdge)) ) then
            let (h,w) = (getRotatedHAndW symbol)
            let firstPortPos = getPortPosWithIndex symbol portsOnEdge edge 0
            let lastPortPos = getPortPosWithIndex symbol portsOnEdge edge (portsOnEdge-1)
            let firstPortXorY = match edge with |Top |Bottom -> firstPortPos.X | _ -> firstPortPos.Y 
            let lastPortXorY = match edge with | Top | Bottom ->  lastPortPos.X | _ -> lastPortPos.Y
            let hORw = match edge with | Top | Bottom -> w |_ -> h
            match (order,edge) with
            |(0,Bottom) |(0,Left) -> firstPortXorY/2.0 - 2.5
            |(0,_) -> (hORw + firstPortXorY)/2.0 + 2.5
            |(_,Bottom) | (_,Left) -> (hORw+lastPortXorY)/2.0 + 2.5
            |(_,_) -> lastPortXorY/2.0 - 2.5
        
        else 
            match edge with
            | Top | Bottom -> ((getPortPosWithIndex symbol portsOnEdge edge (order-1)).X + (getPortPosWithIndex symbol portsOnEdge edge order).X)/2.0
            | _ -> ((getPortPosWithIndex symbol portsOnEdge edge (order-1)).Y + (getPortPosWithIndex symbol portsOnEdge edge order).Y)/2.0

    /// Find the position of the target Port given the old/new edge and old/new order
    let findTargetPos (port:Port) (symbol:Symbol) = 
        let tempSymbol = updatePortPos symbol pos port.Id
        let oldEdge = symbol.PortMaps.Orientation[port.Id]
        let oldOrder = List.findIndex (fun elem -> elem = port.Id) symbol.PortMaps.Order[oldEdge]
        let newEdge = tempSymbol.PortMaps.Orientation[port.Id]
        let newOrder = List.findIndex (fun elem -> elem = port.Id) tempSymbol.PortMaps.Order[newEdge]
        let newPortPos = Symbol.getPortPos tempSymbol port
        
        let x = 
            match newEdge with
            | Right -> snd (getRotatedHAndW symbol)
            | _ -> newPortPos.X
        let y = 
            match newEdge with
            | Bottom -> fst (getRotatedHAndW symbol)
            | _ -> newPortPos.Y
        
        if newEdge = oldEdge then
            let diff = findOffsetSameEdge symbol newEdge
            if (newEdge = Top) || (newEdge = Bottom) then
                if oldOrder < newOrder then ({X=x+diff;Y=y},pos)
                elif oldOrder = newOrder then ({X=x;Y=y},pos)
                else ({X=x-diff;Y=y},pos)
            else
                if oldOrder < newOrder then ({X=x;Y=y+diff},pos)
                elif oldOrder = newOrder then ({X=x;Y=y},pos)
                else ({X=x;Y=y-diff},pos)      
        else 
            let offset = findOffsetDifferentEdge symbol newEdge newOrder
            if (newEdge = Top) || (newEdge = Bottom) then ({X=offset;Y=y},pos)
            else ({X=x;Y=offset},pos)
    
    /// return the correctly parameterised symbol given the edge the moving port is (or isn't) on
    let isTouchingEdge port symId oldSymbol = 
        match getCloseByEdge oldSymbol pos with
        | None -> 
            let newSymbol = 
                oldSymbol
                |> set movingPort_ (Some {|PortId = portId; CurrPos = pos|})
                |> set movingPortTarget_ None
                |> set (appearance_ >-> showPorts_) (ShowOneNotTouching port)
                        
            set (symbolOf_ symId) newSymbol model, Cmd.none            
        | Some _ -> 
            let target = Some (findTargetPos port oldSymbol)  
            let newSymbol = 
                oldSymbol
                |> set movingPort_ (Some {|PortId = portId; CurrPos = pos|}) 
                |> set movingPortTarget_ target
                |> set (appearance_ >-> showPorts_) (ShowOneTouching port)
            
            
            set (symbolOf_ symId) newSymbol model, Cmd.none
    
    let port = model.Ports[portId]
    let symId = ComponentId port.HostId
    let oldSymbol = model.Symbols[symId]
    match oldSymbol.Component.Type with
    | Custom _ -> isTouchingEdge port symId oldSymbol
    | _ -> model, Cmd.none 