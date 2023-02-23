module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartHelpers
open SheetCreator
open SymbolUpdate
open DrawModelType

open Optics
open Operators
open System 



(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)


// define a D.U. for the different return types of the smart autoroute function
type SmartAutorouteResult =
    // | SmartAutorouteModel of Model
    // | SmartAutorouteWire of Wire
    | ModelT of Model
    | WireT of Wire


// helper function for finding matching symbol in model for given port ids on wire - current issue is finding the symbol with the matching input port id
let findInputSymbol (model: Model) (wire: Wire) : Symbol option = 
    let port = string wire.InputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(port))
        |> List.tryHead
    symbolsWithPortId

// helper function for finding matching symbol in model for given port ids on wire - current issue is finding the symbol with the matching input port id
let findOutputSymbol (model: Model) (wire: Wire) : Symbol option = 
    let port = string wire.OutputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(port))
        |> List.tryHead
    symbolsWithPortId


// helper function for finding if wire is connected from and to the same symbol
let isSelfConnected (model: Model) (wire: Wire) : bool = 
    let inputPort = string wire.InputPort
    let outputPort = string wire.OutputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(inputPort) && symbol.PortMaps.Orientation.ContainsKey(outputPort))
        |> List.tryHead
    match symbolsWithPortId with
        | Some symbol -> true
        | None -> false


/// returns the height needed to hug the symbol, if needed
/// MAKE IT WORK FOR VERTICAL HUGGING CASES
let huggingDistance (wire: Wire) (symbol: Symbol) : float = 
    let inputPort = string wire.InputPort
    let portPos = symbol.PortMaps.Orientation |> Map.find inputPort
    let boundaryBox = symbolBox symbol
    let hugDistance = (snd boundaryBox[3]) - wire.StartPos.Y
    match portPos with
        | Left -> hugDistance
        | Right -> hugDistance
        | _ -> 0.0


/// helper function that routes a wire from input port to output port around the symbol, rather than through it
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol Option) : Wire = 
    let autoWire = autoroute model wire
    let selfConnected = isSelfConnected model wire
    let routing = 
        match selfConnected with
            | true -> 
                let symbolFound = symbol |> Option.get
                let hugLength = (huggingDistance wire symbolFound) + 15.0
                let newWires = 
                    let newOutputSegment: Segment = {wire.Segments[2] with Length =  wire.Segments[2].Length - 7.0}
                    let newInputSegment: Segment = {wire.Segments[6] with Length =  wire.Segments[6].Length + 7.0}
                    let newFirstSegment: Segment = {wire.Segments[3] with Length =  wire.Segments[3].Length + hugLength}
                    let newMiddleSegment: Segment = {wire.Segments[4] with Length =  wire.Segments[4].Length + 5.0}
                    let newThirdSegment: Segment = {wire.Segments[5] with Length =  wire.Segments[5].Length - hugLength}
                    let newRoute: Wire = {wire with Segments = [wire.Segments[0]; wire.Segments[1]; newOutputSegment; newFirstSegment;  newMiddleSegment; newThirdSegment; newInputSegment; wire.Segments[7]]}
                    newRoute 
                match hugLength with
                    | 15.0 -> wire
                    | _ -> newWires
            | false -> autoWire
    routing


let genWireLabelName : string = 
    let random = new Random()
    let randomNumber = random.Next(1, 101)
    let wireLabelName = "Wire" + string randomNumber
    wireLabelName


let wireLabelComponent (id: string) (name: string) : Component =
    let inputPorts = createPortList PortType.Input 1 id
    // printfn "input ports: %A" inputPorts
    let outputPorts = createPortList PortType.Output 1 id
    // printfn "output ports: %A" outputPorts
    {
        Id = id
        Type = IOLabel
        Label = name.ToUpper()
        InputPorts = inputPorts 
        OutputPorts = outputPorts
        X = 0.
        Y = 0.
        H = 30.
        W = 30.
        SymbolInfo = None
    }

/// helper function for creating a wire between two symbols
let createWire (model: Model) (sourceSymbol: Symbol) (targetSymbol: Symbol) (sourcePortIndex: int) (targetPortIndex: int): Model =  
    let sourcePort: Port = sourceSymbol.Component.OutputPorts[sourcePortIndex]
    let targetPort: Port = targetSymbol.Component.InputPorts[targetPortIndex]
    let sourcePortId: string = sourcePort.Id
    let targetPortId: string = targetPort.Id
    let sourcePos = Symbol.getOutputPortLocation None model.Symbol (OutputPortId sourcePortId)
    let targetPos = Symbol.getInputPortLocation None model.Symbol (InputPortId targetPortId)
    let wireId: string = JSHelpers.uuid()
    let wireId': ConnectionId = ConnectionId wireId
    let color: HighLightColor = HighLightColor.Grey

    // connection id - NEED TO ADD TO MAP? - CHECK
    let connId = ConnectionId (JSHelpers.uuid())
    // generate wire segments from sourcePos to targetPos
    let segments = 
        let outputSegment: Segment = 
            {
                Index = 0
                Length = 10.
                WireId = connId
                IntersectOrJumpList = []
                Draggable = false
                Mode = RoutingMode.Manual
            }
        let inputSegment: Segment = 
            {
                Index = 1
                Length = 10.
                WireId = connId
                IntersectOrJumpList = []
                Draggable = false
                Mode = RoutingMode.Manual
            }
        let firstSegment: Segment = 
            {
                Index = 2
                Length = targetPos.X - sourcePos.X - 20.
                WireId = connId
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Manual
            }
        let middleSegment: Segment = 
            {
                Index = 3
                Length = targetPos.Y - sourcePos.Y
                WireId = connId
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Manual
            }
        let thirdSegment: Segment = 
            {
                Index = 4
                Length = targetPos.X - sourcePos.X - 20.
                WireId = connId
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Manual
            }
        [outputSegment; inputSegment; firstSegment; middleSegment; thirdSegment]
    let wire: Wire = 
        {
            WId = wireId'
            InputPort = InputPortId targetPortId
            OutputPort = OutputPortId sourcePortId
            StartPos = sourcePos
            Segments = segments
            Color = color
            Width = 5
            InitialOrientation = Vertical
        }
    let newWireMap =
        model.Wires
        |> Map.add wireId' wire
    let newModel: Model = {model with Wires = newWireMap}
    newModel
    

/// helper function for creating wire labels at two symbols
let generateWireLabels (model: Model) (wire: Wire) (symbol: Symbol) : SmartAutorouteResult = 
    let inputPort = wire.InputPort
    let outputPort = wire.OutputPort
    let inputPortSymbol = (findInputSymbol model wire) |> Option.get
    let outputPortSymbol = (findOutputSymbol model wire) |> Option.get
    let inputPortEdge = inputPortSymbol.PortMaps.Orientation |> Map.find (string inputPort)
    let outputPortEdge = outputPortSymbol.PortMaps.Orientation |> Map.find (string outputPort)
    // get port positions
    let outputPortPos, inputPortPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    // create 2 wire label symbols to add to model
    let uuid: string = JSHelpers.uuid()
    let wireName = genWireLabelName
    let inputLabelComp = wireLabelComponent uuid wireName
    let inputLabel: Symbol = 
        let inputLabelPos = 
            match outputPortEdge with
                | Left -> {X = inputPortPos.X - 60.0; Y = inputPortPos.Y}
                | Right -> {X = inputPortPos.X + 60.0; Y = inputPortPos.Y}
                | _ -> {X = inputPortPos.X; Y = inputPortPos.Y}
        printfn "input label pos: %A" inputPortEdge
        
        {symbol with
            Id = ComponentId uuid
            Component = inputLabelComp
            Pos = inputLabelPos 
            Appearance = 
                {symbol.Appearance with
                    ShowPorts = ShowBoth
            }
        }

    let uuid2: string = JSHelpers.uuid()
    let outputLabelComp =  wireLabelComponent uuid2 wireName
    let outputLabel: Symbol = 
        let outputLabelPos = 
            match inputPortEdge with
                | Left -> {X = outputPortPos.X - 60.0; Y = outputPortPos.Y}
                | Right -> {X = outputPortPos.X + 60.0; Y = outputPortPos.Y}
                | _ -> {X = outputPortPos.X; Y = outputPortPos.Y}
        printfn "output label pos: %A" outputPortEdge

        {symbol with
            Id = ComponentId uuid2
            Component = outputLabelComp
            Pos = outputLabelPos
            Appearance = 
                {symbol.Appearance with
                    ShowPorts = ShowBoth
            }
        }
        // |> Symbol.autoScaleHAndW
 
    let newModel = {model with Symbol = {model.Symbol with Symbols = Map.add inputLabel.Id inputLabel model.Symbol.Symbols}}
    let newModel2 = {newModel with Symbol = {newModel.Symbol with Symbols = Map.add outputLabel.Id outputLabel newModel.Symbol.Symbols}}
    
    // create wires between ports and wire labels

    let getOutputPortIndex (portId: OutputPortId) (ports: Port list) : int =
        let rec indexFinder (ports: Port list) (index: int) : int =
            match ports with
                | [] -> -1
                | h::t -> 
                    if h.Id = (string portId) then index
                    else indexFinder t (index + 1)
        indexFinder ports 0

    let getInputPortIndex (portId: InputPortId) (ports: Port list) : int =
        let rec indexFinder (ports: Port list) (index: int) : int =
            match ports with
                | [] -> -1
                | h::t -> 
                    if h.Id = (string portId) then index
                    else indexFinder t (index + 1)
        indexFinder ports 0
    

    let OutputPortIndex = getOutputPortIndex outputPort outputPortSymbol.Component.OutputPorts  // CHECK if outputPort (id) is correct one associated to component of outputPortSymbol
    let InputPortIndex = getInputPortIndex inputPort inputPortSymbol.Component.InputPorts       // CHECK if inputPort (id) is correct one associated to component of inputPortSymbol
    // let conn1 = createConnection outputPortSymbol.Component.OutputPorts[OutputPortIndex] inputLabelComp.InputPorts.[0]
    // let conn2 = createConnection inputLabelComp.OutputPorts.[0] inputPortSymbol.Component.InputPorts.[InputPortIndex]
    let resModel1 = createWire newModel2 outputPortSymbol inputLabel OutputPortIndex 0
    let resModel2 = createWire resModel1 outputLabel inputPortSymbol 0 InputPortIndex
    ModelT resModel2


/// helper function that finds wire in model by connection id
let findWire (model: Model) (connId: ConnectionId) : Option<Wire> =
    match model.Wires |> Map.toList |> List.tryFind (fun (_, wire) -> wire.Segments.[0].WireId = connId) with
    | Some (_, wire) -> Some wire
    | _ -> None


/// helper function that deletes wire from model by filtering out the wire
/// FIXME: Wire remains connected to input port when moving function away from threshold
let deleteWire (model: Model) (wire: Wire) : SmartAutorouteResult = 
    
    printfn "wire id: %A" wire.WId 
    
    let newWires =
        model.Wires
        |> Map.filter (fun id w -> not (wire.WId = id))
    let model =
        {model with Wires = newWires}

    printfn "updated model: %A" newWires
    ModelT model
            
    
/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): SmartAutorouteResult =     
    let symbol = findInputSymbol model wire

    // let newModel = replaceLongWire model wire (symbol |> Option.get)
    let autoWire = autoroute model wire

    printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={autoWire.Segments}"
    

    let wireLength = autoWire.Segments[4].Length
    match wireLength with
        | l when l > 200.0 -> 
            // delete wire
            let newWireMap = deleteWire model wire
            // add wire labels at posiitons near ports
            match newWireMap with
                | ModelT newModel -> 
                    // generate wire labels
                    let foundsymbol = symbol |> Option.get
                    generateWireLabels newModel wire foundsymbol // function is already type ModelT
                    // match newWireMap with
                    //     | ModelT newModel -> ModelT newModel
                    //     | _ -> ModelT newModel
                | _ -> 
                    printfn "error" 
                    newWireMap
            

        | _ -> WireT (routeAroundSymbol model autoWire symbol)


// WIRE LABELS:
    // 1: DELETE WIRE 
    // 2: Place wire labels at appropriate position to input port and output port
    // 3: call function to create new wire (see if this works)
    // Change the output of autowire to return BusWire Model instead of Wire
    // Change the test code to use the new return of autowire 

