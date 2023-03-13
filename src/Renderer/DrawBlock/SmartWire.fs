module SmartWire
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWireUpdateHelpers
open SmartHelpers
open SheetCreator
open SymbolUpdate

open Optics
open Operators
open System 


// HLP23: Author Omar


/// discriminated union for return type of the smart autoroute and other SmartWire functions
type SmartAutorouteResult =
    | ModelT of Model
    | WireT of Wire


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


/// route wire connected across same symbol to be able to hug the symbol
/// wires are spaced out based on the net they are connected to
/// hugging of same-net wires is only implemented for wires connected across the same symbol
/// because the wires most often become hard to read when they are connected across the same symbol
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


/// returns a list of five new segments to create a new wire, given a segment lengths array and wire ID
/// used in generateWireLabels to create a new wire between a wire label and symbol
let createSegmentList (segLengths: float list) (wId: ConnectionId) : Segment list = 
    let outputSegment: Segment = 
        {
            Index = 0
            Length = segLengths[0]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = false
            Mode = RoutingMode.Auto
        }
    let inputSegment: Segment = 
        {
            Index = 1
            Length = segLengths[1]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = true
            Mode = RoutingMode.Auto
        }
    let firstSegment: Segment = 
        {
            Index = 2
            Length = segLengths[2]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = true
            Mode = RoutingMode.Auto
        }
    let middleSegment: Segment = 
        {
            Index = 3
            Length = segLengths[3]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = true
            Mode = RoutingMode.Auto
        }
    let thirdSegment: Segment = 
        {
            Index = 4
            Length = segLengths[4]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = true
            Mode = RoutingMode.Auto
        }
    let fourthSegment: Segment = 
        {
            Index = 5
            Length = segLengths[5]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = true
            Mode = RoutingMode.Auto
        }
    let fifthSegment: Segment = 
        {
            Index = 6
            Length = segLengths[6]
            WireId = wId
            IntersectOrJumpList = []
            Draggable = false
            Mode = RoutingMode.Auto
        }
    [outputSegment; inputSegment; firstSegment; middleSegment; thirdSegment; fourthSegment; fifthSegment]


/// generates a wire label name with a random number
let genWireLabelName : string = 
    let random = new Random()
    let randomNumber = random.Next(1, 99)
    let wireLabelName = "WL" + string randomNumber
    wireLabelName


/// creates wire labels near two symbols
let generateWireLabels (model: Model) (wire: Wire) : SmartAutorouteResult = 
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
    let newInputLabelModel = {model with Symbol = (fst symbModelInput)}

    let outputLabelPos = 
        match inputPortEdge with
            | Left -> {X = outputPortPos.X - 40.0; Y = outputPortPos.Y}
            | Right -> {X = outputPortPos.X + 40.0; Y = outputPortPos.Y}
            | Bottom -> {X = outputPortPos.X; Y = outputPortPos.Y + 40.0}
            | _ -> {X = outputPortPos.X; Y = outputPortPos.Y - 40.0}
    let symbModelOutput = addSymbol [] newInputLabelModel.Symbol outputLabelPos ComponentType.IOLabel wireName
    let newOutputLabelModel = {model with Symbol = (fst symbModelOutput)}
    
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
    let inputLabelSymbol = newOutputLabelModel.Symbol.Symbols[(snd symbModelInput)]
    let outputLabelSymbol = newOutputLabelModel.Symbol.Symbols[(snd symbModelOutput)]

    let inputLabelSymbolPort : Port = inputLabelSymbol.Component.InputPorts[0]
    let outputLabelSymbolPort : Port = outputLabelSymbol.Component.OutputPorts[0]

    let inputConnection: Connection = createConnection inputPortSymbolPort outputLabelSymbolPort
    let outputConnection: Connection = createConnection outputPortSymbolPort inputLabelSymbolPort

    let inputWireID = ConnectionId inputConnection.Id
    let inputSegments =
        let segmentLengths = [8.; 0.; 4.5; 0.; 4.5; 0.; 8.]
        createSegmentList segmentLengths inputWireID

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
        let segmentLengths = [-8.; 0.; -4.5 ; 0.; -4.5; 0.; -8.]
        createSegmentList segmentLengths outputWireID

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
    ModelT {newOutputLabelModel with Wires = wiresMap}


/// finds wire in model by connection id
let findWire (model: Model) (connId: ConnectionId) : Option<Wire> =
    match model.Wires |> Map.toList |> List.tryFind (fun (_, wire) -> wire.Segments.[0].WireId = connId) with
    | Some (_, wire) -> Some wire
    | _ -> None


/// replaces wire with wire labels 
let replaceWithWireLabels (model: Model) (wire: Wire) : SmartAutorouteResult =
    let newWireMap = deleteWire model wire
    generateWireLabels newWireMap wire


/// returns left, middle, and right conditions for symbol intersection with wire
let conditions (model: Model) (symbol: Symbol) (wire: Wire) : bool list = 
    let symbolBox = symbolBox symbol
    let symbolTopLeftPos = symbolBox[0]
    let symbolTopRightPos = symbolBox[1]
    let symbolBottomRightPos = symbolBox[3]
    let symbolLeft = fst symbolTopLeftPos
    let symbolTop = snd symbolTopLeftPos
    let symbolBottom = snd symbolBottomRightPos
    let symbolRight = fst symbolTopRightPos
    
    let leftCornerPos = (wire.StartPos.X + wire.Segments[2].Length, wire.StartPos.Y + wire.Segments[3].Length)
    let bottomLeftCornerPos = (fst leftCornerPos, snd leftCornerPos - wire.Segments[3].Length)
    let wireEndpos = (fst leftCornerPos + wire.Segments[4].Length, snd leftCornerPos)
    let wireTop = snd leftCornerPos
    let wireRight = fst wireEndpos
    let wireLeft = fst leftCornerPos
    let bottomLeftCornerY = snd bottomLeftCornerPos

    let outputPort = string wire.OutputPort 
    let outputSymbol = findSymbol model wire Output |> Option.get
    let outputPortEdge = outputSymbol.PortMaps.Orientation |> Map.find outputPort

    // conditions for horizontal wires
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

    // conditions for vertical wires
    let rightMidX = wire.StartPos.X + wire.Segments[3].Length
    let bottomSegmentY = wire.StartPos.Y + wire.Segments[2].Length
    let wireTopY = bottomSegmentY + wire.Segments[4].Length

    let verticalMiddleCondition = 
        (symbolLeft > wire.StartPos.X && symbolLeft < rightMidX
        || symbolLeft < wire.StartPos.X && symbolLeft > rightMidX
        || symbolLeft < wire.StartPos.X && symbolRight > wire.StartPos.X)
        && symbolTop < bottomSegmentY && symbolBottom > bottomSegmentY
    
    let verticalBottomCondition = 
        symbolBottom > bottomSegmentY && symbolBottom < wire.StartPos.Y
        && symbolLeft < wire.StartPos.X && symbolRight > wire.StartPos.X
    
    let verticalTopCondition = 
        symbolTop < bottomSegmentY && symbolTop > wireTopY
        && symbolLeft < rightMidX && symbolRight > rightMidX

    // conditions for 2 segment vertical wires
    let outputPortPos, inputPortPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)

    let cornerPosX = wire.StartPos.X
    let cornerPosY = wire.StartPos.Y + wire.Segments[2].Length
    let wireEndPointX = outputPortPos.X

    let verticalTwoSegVerticalCondition = 
        symbolLeft < cornerPosX && symbolRight > cornerPosX
        && ((symbolTop > cornerPosY && symbolTop < wire.StartPos.Y)         // vertical wire segment goes UP
        || (symbolBottom < cornerPosY && symbolBottom > wire.StartPos.Y))   // vertical wire segment goes DOWN

    let verticalTwoSegHorizontalCondition =
        ((symbolRight > cornerPosX && symbolRight < wireEndPointX)  // horiz wire to right of symbol
        || (symbolLeft < cornerPosX && symbolLeft > wireEndPointX)) // horiz wire to left of symbol
        && symbolTop < cornerPosY && symbolBottom > cornerPosY

    // conditions for 2 segment horizontal wires
    let cornerPosX' = float outputPortPos.X
    let cornerPosY' = float inputPortPos.Y
    let wireEndPointY = outputPortPos.Y

    let horizTwoSegVerticalCondition = 
        (symbolLeft < cornerPosX' && symbolRight > cornerPosX')
        && ((symbolTop < cornerPosY' && symbolTop > wireEndPointY)          // vertical wire segment goes UP
        || (symbolBottom > cornerPosY' && symbolBottom < wireEndPointY))    // vertical wire segment goes DOWN

    let horizTwoSegHorizontalCondition =
        ((symbolLeft < cornerPosX' && symbolLeft > wire.StartPos.X)      // horiz wire to right of symbol
        || (symbolRight > cornerPosX' && symbolRight < wire.StartPos.X)) // horiz wire to left of symbol
        && symbolTop < cornerPosY' && symbolBottom > cornerPosY'
    
    let inputPort = string wire.InputPort 
    let inputSymbol = findSymbol model wire Input |> Option.get
    let inputPortEdge = inputSymbol.PortMaps.Orientation |> Map.find inputPort

    let segListLength = wire.Segments |> List.length
    match segListLength with
        | l when l < 7 -> 
            // 2 segment wire
            match inputPortEdge with
                | Top -> [horizTwoSegVerticalCondition; horizTwoSegHorizontalCondition]
                | Bottom -> [horizTwoSegVerticalCondition; horizTwoSegHorizontalCondition]
                | Right -> [verticalTwoSegVerticalCondition; verticalTwoSegHorizontalCondition]
                | Left -> [verticalTwoSegVerticalCondition; verticalTwoSegHorizontalCondition]
        | _ -> 
            // 3 segment wire
            match outputPortEdge with
                | Left -> [leftCondition; middleCondition; rightCondition]
                | Right -> [leftCondition; middleCondition; rightCondition]
                | Top -> [verticalTopCondition; verticalMiddleCondition; verticalBottomCondition]
                | Bottom -> [verticalTopCondition; verticalMiddleCondition; verticalBottomCondition]


/// routes wire around symbols from input port to output port
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol Option) : SmartAutorouteResult = 
    let selfConnected = isSelfConnected model wire
    let routing = 
        match selfConnected with
            | true -> WireT (sameSymbolRouting model wire)
            | false -> 
                // determine if other symbols in map are in the way of the wire
                // if so, route around them by adjusting the length of the wire segments
                let symbolValues =
                    model.Symbol.Symbols
                    |> Map.toList
                    |> List.map snd

                let symbolInWay =
                    symbolValues
                    |> List.filter (fun symbol ->
                        let conditionList = conditions model symbol wire
                        let leftCondition = conditionList[0]
                        let middleCondition = conditionList[1]
                        let rightCondition = conditionList[2]

                        let symbolInWay = 
                            if middleCondition || leftCondition || rightCondition then true
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
                        let symbolRight = fst symbolBottomRightPos
                        let symbolTop = snd symbolTopLeftPos
                        let symbolBottom = snd symbolBottomRightPos
                        
                        let leftCornerPos = (wire.StartPos.X + wire.Segments[2].Length, wire.StartPos.Y + wire.Segments[3].Length)
                        let bottomLeftCornerPos = (fst leftCornerPos, snd leftCornerPos - wire.Segments[3].Length)
                        let wireEndpos = (fst leftCornerPos + wire.Segments[4].Length, snd leftCornerPos)
                        let wireTop = snd leftCornerPos
                        let wireLeft = fst leftCornerPos
                        let wireEndposY = snd wireEndpos
                        let bottomLeftCornerY = snd bottomLeftCornerPos

                        let conditionList = conditions model symbol wire
                        
                        let inputPort = string wire.InputPort 
                        let inputSymbol = findSymbol model wire Input |> Option.get
                        let inputPortEdge = inputSymbol.PortMaps.Orientation |> Map.find inputPort

                        let newWireHorizontal =
                            let leftCondition = conditionList[0]
                            let middleCondition = conditionList[1]
                            let rightCondition = conditionList[2]
                            
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
                                if inputPortEdge = Bottom then    // prevents the adjusted wire from going through the input symbol
                                    if (symbolBottom > wireTop) then 
                                        let segmentLengths = 
                                            [ wire.Segments.[0].Length; wire.Segments[1].Length; wire.Segments.[2].Length - 12. + (symbolLeft - wireLeft);
                                            wire.Segments.[3].Length + 5. + (symbolBottom - wireTop); wire.Segments.[4].Length + 12. - (symbolLeft - wireLeft);
                                            wire.Segments.[5].Length - 5. - (symbolBottom - wireTop); wire.Segments.[6].Length ]
                                        updateWire wire segmentLengths
                                    else
                                        let segmentLengths = 
                                            [ wire.Segments.[0].Length; wire.Segments[1].Length; wire.Segments.[2].Length - 12. + (symbolLeft - wireLeft);
                                            wire.Segments.[3].Length; wire.Segments.[4].Length + 12. - (symbolLeft - wireLeft);
                                            wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                        updateWire wire segmentLengths

                                else
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
                                if inputPortEdge = Bottom then    // prevents the adjusted wire from going through the input symbol
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length; wire.Segments.[2].Length; wire.Segments.[3].Length + 5. + (symbolBottom - wireTop);
                                        wire.Segments.[4].Length; wire.Segments.[5].Length - 5. - (symbolBottom - wireTop); wire.Segments.[6].Length]
                                    updateWire wire segmentLengths
                                else
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length; wire.Segments.[2].Length; wire.Segments.[3].Length - 5. - (wireTop - symbolTop);
                                        wire.Segments.[4].Length; wire.Segments.[5].Length + 5. + (wireTop - symbolTop); wire.Segments.[6].Length]
                                    updateWire wire segmentLengths

                        let newWireVertical =
                            let rightMidX = wire.StartPos.X + wire.Segments[3].Length
                            let bottomSegmentY = wire.StartPos.Y + wire.Segments[2].Length

                            let verticalTopCondition = conditionList[0]
                            let verticalMiddleCondition = conditionList[1]
                            let verticalBottomCondition = conditionList[2]

                            if verticalMiddleCondition && verticalTopCondition then
                                if wire.Segments.[3].Length > 0. then
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                        wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length; 
                                        wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths
                                else
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                        wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length; 
                                        wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths

                            elif verticalMiddleCondition && verticalBottomCondition then
                                if wire.Segments.[3].Length > 0. then
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length + (symbolLeft - wire.StartPos.X);
                                        wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length - (symbolLeft - wire.StartPos.X); 
                                        wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths
                                else
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length +  (symbolRight - wire.StartPos.X);
                                        wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length - (symbolRight - wire.StartPos.X); 
                                        wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths

                            elif verticalMiddleCondition then
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                    wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length; 
                                    wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                updateWire wire segmentLengths
                            
                            elif verticalTopCondition then
                                if wire.Segments.[3].Length > 0. then
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                        wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length; 
                                        wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths
                                else
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                        wire.Segments.[2].Length - (bottomSegmentY - symbolTop); wire.Segments.[3].Length + (symbolLeft - rightMidX); 
                                        wire.Segments.[4].Length + (bottomSegmentY - symbolTop); wire.Segments.[5].Length - (symbolLeft - rightMidX); wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths

                            else    
                                // verticalBottomCondition     2:  + 15. + (symbolBottom - bottomSegmentY), 4: - 15. - (symbolBottom - bottomSegmentY
                                if wire.Segments.[3].Length > 0. then
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length + (symbolLeft - wire.StartPos.X);
                                        wire.Segments.[2].Length; wire.Segments.[3].Length - (symbolLeft - wire.StartPos.X); 
                                        wire.Segments.[4].Length; wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths
                                else
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                        wire.Segments.[2].Length + 15. + (symbolBottom - bottomSegmentY); wire.Segments.[3].Length; 
                                        wire.Segments.[4].Length - 15. - (symbolBottom - bottomSegmentY) ; wire.Segments.[5].Length; wire.Segments.[6].Length ]
                                    updateWire wire segmentLengths

                        let outputPort = string wire.OutputPort 
                        let outputSymbol = findSymbol model wire Output |> Option.get
                        let outputPortEdge = outputSymbol.PortMaps.Orientation |> Map.find outputPort

                        let wireSegments = 
                            match outputPortEdge with
                                | Left -> newWireHorizontal
                                | Right -> newWireHorizontal
                                | _ -> newWireVertical

                        adjustWireSegments wireSegments symbols
                
                if (List.length symbolInWay > 3) && (wire.Segments[4].Length > 300.0) then 
                    // if there are more than 3 symbols in the way of the wire and the wire is long enough - replace wire with wire labels
                    replaceWithWireLabels model wire

                else 
                    let newWire = adjustWireSegments wire symbolInWay
                    WireT newWire
    
    routing

/// 2 (visable) segment wire routing
let routeTwoSegWires (model: Model) (wire: Wire) : SmartAutorouteResult = 
    let selfConnected = isSelfConnected model wire
    let routing = 
        match selfConnected with
            | true -> WireT (sameSymbolRouting model wire)
            | false -> 
                // determine if other symbols in map are in the way of the wire
                // if so, route around them by adjusting the length of the wire segments
                let symbolValues =
                    model.Symbol.Symbols
                    |> Map.toList
                    |> List.map snd

                let symbolInWay =
                    symbolValues
                    |> List.filter (fun sym ->
                        let conditionList = conditions model sym wire
                        let verticalCondition = conditionList[0]
                        let horizontalCondition = conditionList[1]

                        let symbolInWay = 
                            if verticalCondition || horizontalCondition then true
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
                        let symbolRight = fst symbolBottomRightPos
                        let symbolBottom = snd symbolBottomRightPos

                        let outputPortPos, inputPortPos =
                            Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)    
                        let conditionList = conditions model symbol wire
                        let verticalCondition = conditionList[0]

                        let outputPort = string wire.OutputPort 
                        let outputSymbol = findSymbol model wire Output |> Option.get
                        let outputPortEdge = outputSymbol.PortMaps.Orientation |> Map.find outputPort             

                        let newWireHorizontal =
                            if outputPortEdge = Left then wire
                            else
                                let cornerPosX = float outputPortPos.X
                                let cornerPosY = float inputPortPos.Y

                                if verticalCondition then
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                        wire.Segments.[2].Length - 5. - (cornerPosX - symbolLeft); wire.Segments.[3].Length; 
                                        wire.Segments.[4].Length + 5. + (cornerPosX - symbolLeft); wire.Segments.[5].Length]
                                    updateWire wire segmentLengths
                                
                                else   // horizontalCondition
                                    let segmentLengths = 
                                        [ wire.Segments.[0].Length; wire.Segments.[1].Length + 5. + (symbolBottom - cornerPosY);
                                        wire.Segments.[2].Length; wire.Segments.[3].Length - 5. - (symbolBottom - cornerPosY); 
                                        wire.Segments.[4].Length; wire.Segments.[5].Length]
                                    updateWire wire segmentLengths

                        let newWireVertical =
                            let cornerPosX = float inputPortPos.X
                            let cornerPosY = float outputPortPos.Y

                            if verticalCondition then
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length + 5. + (symbolRight - cornerPosX);
                                    wire.Segments.[2].Length; wire.Segments.[3].Length - 5. - (symbolRight - cornerPosX); 
                                    wire.Segments.[4].Length; wire.Segments.[5].Length]
                                updateWire wire segmentLengths
                            
                            else   // horizontalCondition
                                let segmentLengths = 
                                    [ wire.Segments.[0].Length; wire.Segments.[1].Length;
                                    wire.Segments.[2].Length + 5. + (symbolBottom - cornerPosY); wire.Segments.[3].Length; 
                                    wire.Segments.[4].Length - 5. - (symbolBottom - cornerPosY); wire.Segments.[5].Length]
                                updateWire wire segmentLengths
                        
                        
                        let inputPort = string wire.InputPort 
                        let inputSymbol = findSymbol model wire Input |> Option.get
                        let inputPortEdge = inputSymbol.PortMaps.Orientation |> Map.find inputPort
                        // printfn "inputportedge: %A" inputPortEdge
                        let wireSegments = 
                            match inputPortEdge with
                                | Bottom -> newWireHorizontal
                                | Top -> wire                                
                                | Left -> newWireVertical
                                | Right -> newWireVertical

                        adjustWireSegments wireSegments symbols
                
                if (List.length symbolInWay > 3) && (wire.Segments[4].Length > 300.0) then 
                    // if there are more than 3 symbols in the way of the wire and the wire is long enough - replace wire with wire labels
                    replaceWithWireLabels model wire

                else 
                    let newWire = adjustWireSegments wire symbolInWay
                    WireT newWire
    
    routing
            

/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): SmartAutorouteResult =     
    let symbol = findSymbol model wire Output
    let autoWire = autoroute model wire
    let segListLength = autoWire.Segments |> List.length
    // printfn "segment info %A" wire.Segments
    
    if segListLength < 7 then
        // 2 segment wire
        routeTwoSegWires model autoWire
    else
        // 3 segment wire
        let wireLength = autoWire.Segments[4].Length
        match wireLength with
        | l when l > 600.0 -> replaceWithWireLabels model wire
        | _ -> routeAroundSymbol model autoWire symbol
