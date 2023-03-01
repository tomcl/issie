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


/// determines if wire is connected from and to the same symbol
let isSelfConnected (model: Model) (wire: Wire) : bool = 
    let inputSymbol = findSymbol model wire Input |> Option.get
    let outputSymbol = findSymbol model wire Output |> Option.get
    inputSymbol = outputSymbol


/// calculates the height needed to hug the symbol
let huggingDistance (model: Model) (wire: Wire) (symbol: Symbol) : float * float = 
    let inputPort = string wire.InputPort
    let portPos = symbol.PortMaps.Orientation |> Map.find inputPort
    let boundaryBox = symbolBox symbol
    let outputPortPos, inputPortPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)

    // tuple representing left and right hug distance
    let hugDistance = 
        (snd boundaryBox[3]) - outputPortPos.Y, (snd boundaryBox[3]) - inputPortPos.Y 

    match portPos with
        | Left -> hugDistance
        | Right -> hugDistance
        | _ -> 0.0, 0.0


/// route wire connected across same symbol
let sameSymbolRouting (model: Model) (wire: Wire) : Wire = 
    let symbol = findSymbol model wire Output
    let symbolFound = symbol |> Option.get
    let outputPortIndex = getSymbolIndex symbolFound (string wire.OutputPort)

    // make huglength dependent on the port index
    let lengthAdjustment = float outputPortIndex * 5.0
    let hugDist = huggingDistance model wire symbolFound
    let leftHugLength = 10.0 + fst hugDist + lengthAdjustment   
    let rightHugLength = 10.0 + snd hugDist + lengthAdjustment
    let seperation = 7.0 + lengthAdjustment

    // offset the input segment length (wire.Segments[6]) based on the output port index 
    let inputExtension = 
        match outputPortIndex with
            | l when l<2 -> -2.0
            | 2 -> 0.0
            | 3 -> 3.0
            | 4 -> 5.0
            | 5 -> 7.0
            | 6 -> 10.0
            | _ -> 12.0

    let newWire = 
        let segmentLengths = 
            [ wire.Segments[0].Length + (seperation / 2.); wire.Segments[1].Length; 
            wire.Segments[2].Length + seperation; rightHugLength; wire.Segments[4].Length - (seperation * 2.); 
            -leftHugLength; wire.Segments[6].Length + seperation - inputExtension; wire.Segments[7].Length ]
        updateWire wire segmentLengths
        
    let inputPort = string wire.InputPort
    let portPos = symbolFound.PortMaps.Orientation |> Map.find inputPort

    // FIND BETTER PLACE TO HAVE THE TWO FUNCTIONS BELOW
    let boundaryBox = symbolBox symbolFound 
    let outputPortPos, inputPortPos = 
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)

    match portPos with
        | Left -> newWire
        | Right -> newWire
        | _ -> 
            let rightVertical = 
                ((snd boundaryBox[3]) - inputPortPos.Y) + 10.0 + lengthAdjustment
            let leftVertical = 
                ((snd boundaryBox[3]) - outputPortPos.Y) + 8.0 + lengthAdjustment
            let horizontalSeperation = 
                match outputPortIndex with
                    | 0 -> 10.5
                    | 1 -> 11.0 + (float outputPortIndex * 7.0)
                    | 2 -> 11.0 + (float outputPortIndex * 7.0)
                    | _ -> 11.0 + (float outputPortIndex * 7.5)
            let segmentLengths = // index 3 and 7 are the vertical segments
                [ wire.Segments[0].Length; wire.Segments[1].Length; wire.Segments[2].Length + horizontalSeperation; rightVertical;
                wire.Segments[4].Length - horizontalSeperation; wire.Segments[5].Length; wire.Segments[6].Length; -leftVertical ]
            updateWire wire segmentLengths


/// generates a wire label name with a random number
let genWireLabelName : string = 
    let random = new Random()
    let randomNumber = random.Next(1, 99)
    let wireLabelName = "WL" + string randomNumber
    wireLabelName


/// creates wire labels near two symbols
let generateWireLabels (model: Model) (wire: Wire) (symbol: Symbol) : SmartAutorouteResult = 
    let inputPort = wire.InputPort
    let outputPort = wire.OutputPort

    let inputPortSymbol = (findSymbol model wire Input) |> Option.get
    let outputPortSymbol = (findSymbol model wire Output) |> Option.get

    let inputPortEdge = inputPortSymbol.PortMaps.Orientation |> Map.find (string inputPort)
    let outputPortEdge = outputPortSymbol.PortMaps.Orientation |> Map.find (string outputPort)

    let outputPortPos, inputPortPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    let wireName = genWireLabelName
    
    let inputLabelPos = 
        match outputPortEdge with
            | Left -> {X = inputPortPos.X - 40.0; Y = inputPortPos.Y}
            | Right -> {X = inputPortPos.X + 40.0; Y = inputPortPos.Y}
            | Bottom -> {X = inputPortPos.X; Y = inputPortPos.Y + 40.0}
            | _ -> {X = inputPortPos.X; Y = inputPortPos.Y - 40.0}
    let symbModelInput = addSymbol [] model.Symbol inputLabelPos ComponentType.IOLabel wireName
    let newModel = {model with Symbol = (fst symbModelInput)}

    let outputLabelPos = 
        match inputPortEdge with
            | Left -> {X = outputPortPos.X - 40.0; Y = outputPortPos.Y}
            | Right -> {X = outputPortPos.X + 40.0; Y = outputPortPos.Y}
            | Bottom -> {X = outputPortPos.X; Y = outputPortPos.Y + 40.0}
            | _ -> {X = outputPortPos.X; Y = outputPortPos.Y - 40.0}
    let symbModelOutput = addSymbol [] newModel.Symbol outputLabelPos ComponentType.IOLabel wireName
    let newModel2 = {model with Symbol = (fst symbModelOutput)}
    
    // create wires between ports and wire labels
    let inputPortSymbolPort : Port = 
        inputPortSymbol.Component.InputPorts
        |> List.filter (fun p -> p.Id = (string inputPort))
        |> List.head
    let outputPortSymbolPort : Port = 
        outputPortSymbol.Component.OutputPorts
        |> List.filter (fun p -> p.Id = (string outputPort))
        |> List.head

    // find input label symbol using its component id (snd symbModelInput)
    let inputLabelSymbol = newModel2.Symbol.Symbols[(snd symbModelInput)]
    let outputLabelSymbol = newModel2.Symbol.Symbols[(snd symbModelOutput)]

    let inputLabelSymbolPort : Port = inputLabelSymbol.Component.InputPorts[0]
    let outputLabelSymbolPort : Port = outputLabelSymbol.Component.OutputPorts[0]

    let inputConnection: Connection = createConnection inputPortSymbolPort outputLabelSymbolPort
    let outputConnection: Connection = createConnection outputPortSymbolPort inputLabelSymbolPort

    let inputWireID = ConnectionId inputConnection.Id
    let inputSegments = 
        let outputSegment: Segment = 
            {
                Index = 0
                Length = 8.
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = false
                Mode = RoutingMode.Auto
            }
        let inputSegment: Segment = 
            {
                Index = 1
                Length = 0.
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Auto
            }
        let firstSegment: Segment = 
            {
                Index = 2
                Length = 4.5
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Auto
            }
        let middleSegment: Segment = 
            {
                Index = 3
                Length = 0.
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Auto
            }
        let thirdSegment: Segment = 
            {
                Index = 4
                Length = 4.5
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Auto
            }
        let fourthSegment: Segment = 
            {
                Index = 5
                Length = 0.
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = true
                Mode = RoutingMode.Auto
            }
        let fifthSegment: Segment = 
            {
                Index = 6
                Length = 8.
                WireId = inputWireID
                IntersectOrJumpList = []
                Draggable = false
                Mode = RoutingMode.Auto
            }
        [outputSegment; inputSegment; firstSegment; middleSegment; thirdSegment; fourthSegment; fifthSegment]
    
    let inputWire: Wire = 
        {
            WId = inputWireID
            InputPort = InputPortId inputLabelSymbolPort.Id 
            OutputPort = OutputPortId outputPortSymbolPort.Id
            StartPos = inputPortPos
            Segments = inputSegments
            Color = HighLightColor.DarkSlateGrey
            Width = 1
            InitialOrientation = Horizontal
        }

    let outputWireID = ConnectionId outputConnection.Id
    let outputSegments = 
        let outputSegment = {inputSegments[0] with Length = -8.; WireId = outputWireID}
        let inputSegment = {inputSegments[1] with Length = 0.; WireId = outputWireID}
        let firstSegment = {inputSegments[2] with Length = -4.5; WireId = outputWireID}
        let middleSegment = {inputSegments[3] with Length = 0.; WireId = outputWireID}
        let thirdSegment = {inputSegments[4] with Length = -4.5; WireId = outputWireID}
        let fourthSegment = {inputSegments[5] with Length = 0.; WireId = outputWireID}
        let fifthSegment = {inputSegments[6] with Length = -8.; WireId = outputWireID}
        [outputSegment; inputSegment; firstSegment; middleSegment; thirdSegment; fourthSegment; fifthSegment] 
    let outputWire : Wire = 
        {
            WId = outputWireID
            InputPort = InputPortId inputPortSymbolPort.Id
            OutputPort = OutputPortId outputLabelSymbolPort.Id
            StartPos = outputPortPos
            Segments = outputSegments
            Color = HighLightColor.DarkSlateGrey
            Width = 1
            InitialOrientation = Horizontal
        }

    let wiresMap = 
        model.Wires
        |> Map.add inputWireID inputWire
        |> Map.add outputWireID outputWire
    ModelT {newModel2 with Wires = wiresMap}


/// finds wire in model by connection id
let findWire (model: Model) (connId: ConnectionId) : Option<Wire> =
    match model.Wires |> Map.toList |> List.tryFind (fun (_, wire) -> wire.Segments.[0].WireId = connId) with
    | Some (_, wire) -> Some wire
    | _ -> None


/// replaces wire with wire labels  - FIXME: FIX SO THAT IT WORKS WHEN MOVING SYMBOLS AROUND   
let replaceWithWireLabels (model: Model) (wire: Wire) (symbol: Symbol Option) : SmartAutorouteResult =
    let newWireMap = deleteWire model wire
    match newWireMap with
    | ModelT m -> 
        let foundsymbol = symbol |> Option.get
        generateWireLabels m wire foundsymbol
    | _ -> 
        printfn "error" 
        WireT wire


/// routes wire around symbols from input port to output port
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol Option) : SmartAutorouteResult = 
    let selfConnected = isSelfConnected model wire
    let routing = 
        match selfConnected with
            | true -> WireT (sameSymbolRouting model wire)
            | false -> 
                // determine if other symbols in map are in the way of the wire
                // if so, route around them by adjusting the length of the wire segments
                let leftCornerPos = (wire.StartPos.X + wire.Segments[2].Length, wire.StartPos.Y + wire.Segments[3].Length)
                let bottomLeftCornerPos = (fst leftCornerPos, snd leftCornerPos - wire.Segments[3].Length)
                let wireEndpos = (fst leftCornerPos + wire.Segments[4].Length, snd leftCornerPos)

                let symbolValues =
                    model.Symbol.Symbols
                    |> Map.toList
                    |> List.map snd
                
                let conditions (symbol: Symbol) : bool list = 
                    let symbolBox = symbolBox symbol
                    let symbolTopLeftPos = symbolBox[0]
                    let symbolTopRightPos = symbolBox[1]
                    let symbolBottomRightPos = symbolBox[3]
                    let symbolLeft = fst symbolTopLeftPos
                    let symbolTop = snd symbolTopLeftPos
                    let symbolBottom = snd symbolBottomRightPos
                    let symbolRight = fst symbolTopRightPos
                    let wireTop = snd leftCornerPos
                    let wireRight = fst wireEndpos
                    let wireLeft = fst leftCornerPos
                    let bottomLeftCornerY = snd bottomLeftCornerPos

                    let middleCondition = 
                        symbolLeft < wireLeft && symbolRight > wireLeft 
                        && ((symbolTop > wireTop && symbolBottom < (bottomLeftCornerY)) 
                        || (symbolTop < wireTop && symbolBottom > wireTop) 
                        || (symbolTop > wireTop && symbolBottom < (bottomLeftCornerY)) 
                        || (symbolTop > (bottomLeftCornerY)  && symbolBottom < wireTop) 
                        || (symbolTop > wireTop && symbolTop < (bottomLeftCornerY)))
                    
                    let leftCondition = 
                            symbolLeft > wire.StartPos.X && symbolLeft < (fst bottomLeftCornerPos) 
                            && symbolTop < (bottomLeftCornerY) 
                            && symbolBottom > (bottomLeftCornerY)
                    
                    let rightCondition = symbolLeft > wireLeft && symbolLeft < wireRight && symbolTop < wireTop && symbolBottom > wireTop

                    [leftCondition; middleCondition; rightCondition]

                let symbolInWay =
                    symbolValues
                    |> List.filter (fun symbol ->
                        let conditonList = conditions symbol
                        let leftCondition = conditonList[0]
                        let middleCondition = conditonList[1]
                        let rightCondition = conditonList[2]

                        let symbolInWay = 
                            if middleCondition then true
                            elif leftCondition then true
                            elif rightCondition then true
                            else false 

                        symbolInWay)                        
                            
                // iterate through the list of symbols in the way and adjust the wire segments accordingly and return the wire with the adjusted segments
                let rec adjustWireSegments wire symbolList =
                    match symbolList with
                    | [] -> wire
                    | symbol::symbols ->
                        let symbolBox = symbolBox symbol
                        let symbolTopLeftPos = symbolBox[0]
                        let symbolBottomRightPos = symbolBox[3]
                        let symbolLeft = fst symbolTopLeftPos
                        let symbolTop = snd symbolTopLeftPos
                        let symbolBottom = snd symbolBottomRightPos
                        let wireTop = snd leftCornerPos
                        let wireLeft = fst leftCornerPos
                        let wireEndposY = snd wireEndpos
                        let bottomLeftCornerY = snd bottomLeftCornerPos

                        let conditonList = conditions symbol
                        let leftCondition = conditonList[0]
                        let middleCondition = conditonList[1]
                        let rightCondition = conditonList[2]

                        let newWire =
                            if leftCondition && middleCondition then 
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length + 5. + (wireEndposY - symbolTop); 
                                    wire.Segments.[2].Length - 10. + (symbolLeft - wireLeft); wire.Segments.[3].Length - 5. - (2. * (wireTop - symbolTop));
                                    wire.Segments.[4].Length + 10. - (symbolLeft - wireLeft); wire.Segments.[5].Length + (wireEndposY - symbolTop);
                                    wire.Segments.[6].Length ]
                                updateWire wire segmentLengths
                            
                            elif rightCondition && middleCondition then
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length; wire.Segments.[2].Length - 10. + (symbolLeft - wireLeft);
                                    wire.Segments.[3].Length - 5. - (wireTop - symbolTop); wire.Segments.[4].Length + 10. - (symbolLeft - wireLeft);
                                    wire.Segments.[5].Length + 5. + (wireTop - symbolTop); wire.Segments.[6].Length ]
                                updateWire wire segmentLengths
                            
                            elif leftCondition then
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length + 5. + (symbolBottom - bottomLeftCornerY);
                                    wire.Segments.[2].Length; wire.Segments.[3].Length - 5. - (symbolBottom - bottomLeftCornerY); 
                                    wire.Segments.[4].Length; wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                updateWire wire segmentLengths
                            
                            elif middleCondition then
                                if symbolTop > bottomLeftCornerY then // symbol in MIDDLE and target symbol is BELOW source symbol
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments[1].Length; wire.Segments.[2].Length - 12. + (symbolLeft - wireLeft);
                                        wire.Segments.[3].Length + 5. + (symbolBottom - wireEndposY); wire.Segments.[4].Length + 12. - (symbolLeft - wireLeft);
                                        wire.Segments.[5].Length - 5. -  (symbolBottom - wireEndposY); wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths
                                else // symbol in MIDDLE and target symbol is ABOVE source symbol
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments[1].Length; wire.Segments.[2].Length - 10. + (symbolLeft - wireLeft);
                                        wire.Segments.[3].Length - 5. - (wireEndposY - symbolTop); wire.Segments.[4].Length + 10. - (symbolLeft - wireLeft);
                                        wire.Segments.[5].Length + 5. + (wireEndposY - symbolTop); wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths
                            
                            else // symbol in way of RIGHT segment of wire
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length; wire.Segments.[2].Length; wire.Segments.[3].Length - 5. - (wireTop - symbolTop);
                                    wire.Segments.[4].Length; wire.Segments.[5].Length + 5. + (wireTop - symbolTop); wire.Segments.[6].Length]
                                updateWire wire segmentLengths

                        adjustWireSegments newWire symbols
                
                if (List.length symbolInWay > 2) && (wire.Segments[4].Length > 300.0) then 
                    // if there are more than 2 symbols in the way of the wire and the wire is long enough - replace wire with wire labels
                    replaceWithWireLabels model wire symbol

                else 
                    let newWire = adjustWireSegments wire symbolInWay
                    WireT newWire
    
    routing

    
/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): SmartAutorouteResult =     
    let symbol = findSymbol model wire Output
    let autoWire = autoroute model wire
    let wireLength = autoWire.Segments[4].Length

    match wireLength with
    | l when l > 500.0 -> replaceWithWireLabels model wire symbol
    | _ -> routeAroundSymbol model autoWire symbol
