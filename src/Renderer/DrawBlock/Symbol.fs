(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Fable.React
open Fable.React.Props
open Elmish

open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT



/// --------- STATIC VARIABLES --------- ///
module Constants =
    [<Literal>]
    let gridSize = 30 

    /// How large are component labels
    let labelFontSizeInPixels:float = 16 // otehr parameters scale correctly with this

    /// Due to a bug in TextMetrics we are restricted to monospace font, bold or normal, if we need accurate font width
    let componentLabelStyle: Text = 
        {defaultText with 
            TextAnchor = "start"; 
            FontSize = $"%.0f{labelFontSizeInPixels}px"; 
            FontFamily = "helvetica"; 
            FontWeight="600"}

    /// Offset between label position and symbol. This is also used as a margin for the label bounding box.
    let componentLabelOffsetDistance: float = 7. // offset from symbol outline, otehr parameters scale correctly
    
    /// Height of label text - used to determine where to print labels
    let componentLabelHeight: float = labelFontSizeInPixels

    /// Small manual correction added to claculated position for placing labels.
    /// Used to make labels equidistant on all sides of symbol.
    let labelCorrection = {X= 0.; Y= 2.}
    
    



//------------------------------------------------------------------//
//------------------------ Helper functions ------------------------//
//------------------------------------------------------------------//

let inline inputPortStr (InputPortId s) = s
let inline outputPortStr (OutputPortId s) = s

let inline invertRotation (rot: RotationType) =
    match rot with
    | RotateClockwise -> RotateAntiClockwise
    | RotateAntiClockwise -> RotateClockwise

/// Returns the correct height and width of a transformed symbol
/// as the tuple (real H, real W).
/// Needed because H & W in Component do not chnage with rotation.
/// NB Pos in component = Pos in Symbol and DOES change with rotation!
let inline getCompRotatedHAndW (comp: Component) (transform: STransform)  =
    match transform.Rotation with
    | Degree0 | Degree180 -> comp.H, comp.W
    | Degree90 | Degree270 -> comp.W, comp.H

/// Returns the correct height and width of a transformed symbol
/// as the tuple (real H, real W).
/// Needed because H & W in Component do not chnage with rotation.
/// NB Pos in component = Pos in Symbol and DOES change with rotation!
let inline getRotatedHAndW sym  = getCompRotatedHAndW sym.Component sym.STransform

/// Modify port position maps to move an existing Lefthand port (index in the list) to the bottom edge
let movePortToBottom (portMaps: PortMaps) index =
    let leftPorts = portMaps.Order[Left]
    let portId = leftPorts |> List.item index //get id of sel

    let newBottomPorts = [portId]
    let newLeftPorts = portMaps.Order[Left] |> List.removeAt index
    let newPortOrder =
        portMaps.Order
        |> Map.add Bottom newBottomPorts
        |> Map.add Left newLeftPorts
    let newPortOrientation =
        portMaps.Orientation |> Map.add portId Bottom
    {Order=newPortOrder; Orientation=newPortOrientation}


/// work out a label bounding box from symbol
/// the bb has a margin around the label text outline.
/// This requires accurate label text width estimates.
/// At the moment that only seems possible with monospaced fonts
/// due to TextMetrics not working ???
let calcLabelBoundingBox (textStyle: Text) (comp: Component) (transform: STransform) =
    let labelRotation = 
        match transform.flipped with
        | true -> match transform.Rotation with
                     | Degree90 -> Degree270
                     | Degree270 -> Degree90
                     | _ -> transform.Rotation
        | false -> transform.Rotation
    let h,w = getCompRotatedHAndW comp transform
    let centreX = comp.X + w / 2.
    let centreY = comp.Y + h / 2.
    let margin = Constants.componentLabelOffsetDistance
    let labH = Constants.componentLabelHeight
    let labW = getMonospaceWidth textStyle.FontSize comp.Label
    let labW = getTextWidthInPixels(comp.Label,textStyle)
    let boxTopLeft =
        match labelRotation with 
        | Degree0 -> {X = centreX - labW/2. - margin; Y = comp.Y - labH - 2.*margin }
        | Degree270 -> {X = comp.X + w; Y = centreY - labH/2. - margin}
        | Degree90 -> {X = comp.X - 2.*margin - labW ; Y = centreY - labH/2. - margin}
        | Degree180 -> {X = centreX - labW/2. - margin; Y = comp.Y + h}
    {TopLeft=boxTopLeft + Constants.labelCorrection; W = labW + 2.*margin; H = labH + 2.*margin}


//------------------------------------------------------------------//
//------------------- Helper functions for titles ------------------//
//------------------------------------------------------------------//

///Insert titles compatible with greater than 1 buswidth
let title (t:string) (n:int) : string =  
    match n with
    | 1 -> t
    | _ when n > 1 -> $"{t}({n})"
    | _ -> failwith "non positive bus width"

///Insert titles for bus select
/// used once 
let bustitle (wob:int) (lsb:int) : string = 
    match wob with
    | 1 -> $"{lsb}"
    | _ when wob > 1 -> $"({wob+lsb-1}..{lsb})"
    | _ -> failwith "non positive bus width in bustitle"

///Decodes the component type into component labels
let getPrefix compType = 
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
    | Mux2 -> "MUX"
    | Mux4 -> "MUX-4."
    | Mux8 -> "MUX-8."
    | Demux2 -> "DM"
    | Demux4 -> "DM-4."
    | Demux8 -> "DM-8."
    | NbitsAdder _ -> "A"
    | NbitsXor _ -> "XOR"
    | DFF | DFFE -> "FF"
    | Register _ | RegisterE _ -> "REG"
    | AsyncROM1 _ -> "AROM"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncRAM1 _ -> "ARAM"
    | Custom c ->
        c.Name.ToUpper() + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ -> "EQ"
    | Decode4 -> "DEC"
    | BusSelection _ -> "SEL"
    | _ -> ""


// Text to be put inside different Symbols depending on their ComponentType
let getComponentLegend (componentType:ComponentType) =
    match componentType with
    | And | Nand-> "&"
    | Or | Nor-> "≥1"
    | Xor | Xnor -> "=1"
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder n -> title "Adder" n
    | Register n | RegisterE n-> title "Register" n
    | AsyncROM1 _ -> "Async-ROM"
    | ROM1 _ -> "Sync-ROM"
    | RAM1 _ -> "Sync-RAM"
    | AsyncRAM1 _ -> "Async-RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | NbitsXor (x)->   title "N-bits-Xor" x
    | Custom x -> x.Name
    | _ -> ""

// Input and Output names of the ports depending on their ComponentType
let portNames (componentType:ComponentType)  = //(input port names, output port names)
    match componentType with
    | Decode4 -> (["Sel";"Data"]@["0"; "1";"2"; "3"])
    | NbitsAdder _ -> (["Cin";"A";"B"]@["Sum "; "Cout"])
    | Register _ -> (["D"]@["Q"])
    | RegisterE _ -> (["D"; "EN"]@["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["Addr"]@["Dout"])
    | RAM1 _ -> (["Addr"; "Din";"Wen" ]@["Dout"])
    | AsyncRAM1 _ -> (["Addr"; "Din";"Wen" ]@["Dout"])
    | DFF -> (["D"]@["Q"])
    | DFFE -> (["D";"EN"]@["Q"])
    | Mux2 -> (["0"; "1";"SEL"]@["OUT"])
    | Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"]@["OUT"])
    | Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"]@["OUT"])
    | Demux2 -> (["IN" ; "SEL"]@["0"; "1"])
    | Demux4 -> (["IN"; "SEL"]@["0"; "1";"2"; "3";])
    | Demux8 -> (["IN"; "SEL"]@["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
    | NbitsXor _ -> (["P"; "Q"]@ ["Out"])
    | Custom x -> (List.map fst x.InputLabels)@ (List.map fst x.OutputLabels)
    | _ -> ([]@[])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .

let movePortsToCorrectEdgeForComponentType (ct: ComponentType) (portMaps: PortMaps): PortMaps =
    match ct with //need to put some ports to different edges
    | Mux2 -> //need to remove select port from left and move to bottom
        movePortToBottom portMaps 2
    | Mux4 -> //need to remove select port from left and move to right
        movePortToBottom portMaps 4
    | Mux8 ->
        movePortToBottom portMaps 8
    | NbitsAdder _ -> 
        let rightSide = portMaps.Order[Right]
        let newRightSide = List.rev rightSide
        let newPortOrder = Map.add Right newRightSide portMaps.Order
        let portMaps' = {portMaps with Order = newPortOrder}
        movePortToBottom portMaps' 0
    | DFFE ->
        movePortToBottom portMaps 1
    | RegisterE _ ->
        movePortToBottom portMaps 1
    | Demux2 | Demux4 | Demux8 ->
        movePortToBottom portMaps 1
    | _ -> portMaps


/// Genererates a list of ports:
let portLists numOfPorts hostID portType =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                PortType = portType
                HostId = hostID
            }])


//-----------------------Skeleton Message type for symbols---------------------//

///Rounds an integer to any given number. The first parameter is the number to round to, the second parameter is the input number that will be rounded
let inline roundToN (n : int) (x : int) =
    x + abs((x % n) - n)

let customToLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

let customStringToLength (lst: string list) =
    let labelLengths = List.map String.length lst
    if List.isEmpty labelLengths then 0
    else List.max labelLengths

let addPortToMaps (edge: Edge) (portMaps: PortMaps) (portId: string) =
    {
        Order = portMaps.Order |> Map.add edge (portMaps.Order[edge] @ [portId])
        Orientation = portMaps.Orientation |> Map.add portId edge
    }

let deletePortFromMaps (port: string) (portMaps: PortMaps) =
    let deletePort (ports: string list) = List.filter ((<>) port) ports
    {
        Order = Map.map (fun edge pL -> deletePort pL) portMaps.Order
        Orientation = Map.remove port portMaps.Orientation
    }

/// work out the initial (default) port placing for a componenent.
/// also used for legacy circuits loaded without port layoiut info.
let initPortOrientation (comp: Component) =

    let defaultportOrder = 
        (Map.empty, [Left; Right; Top; Bottom])
        ||> List.fold (fun currMap edge -> Map.add edge [] currMap)

    let inputMaps : PortMaps =
        ({Order=defaultportOrder; Orientation=Map.empty}, comp.InputPorts)
        ||> List.fold (fun maps port -> addPortToMaps Left maps port.Id)

    let res = 
        (inputMaps, (List.rev comp.OutputPorts))
        ||> List.fold (fun maps port -> addPortToMaps Right maps port.Id)
    movePortsToCorrectEdgeForComponentType comp.Type res



    


/// helper function to initialise custom components
let getCustomCompArgs (x:CustomComponentType) (label:string) =
    let h = Constants.gridSize + Constants.gridSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
    let maxInLength, maxOutLength = customToLength x.InputLabels, customToLength x.OutputLabels
    let maxW = maxInLength + maxOutLength + label.Length
    let scaledW = roundToN Constants.gridSize (maxW * Constants.gridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
    let w = max scaledW (Constants.gridSize * 4) //Ensures a minimum width if the labels are very small
    ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)

/// obtain map from port IDs to port names for Custom Component.
/// for other components types this returns empty map
let getCustomPortIdMap (comp: Component)  =
        let label = comp.Label
        match comp.Type with
        | Custom customType ->
            let (n, nout, h, w) = getCustomCompArgs customType label
            let inputPorts = comp.InputPorts
            let outputPorts = comp.OutputPorts
            let inputPortIdLabels = List.zip inputPorts customType.InputLabels
            let outputPortIdLabels = List.zip outputPorts customType.OutputLabels

            let inputMap =
                (Map.empty, inputPortIdLabels) 
                ||> List.fold (fun currMap (port,label) -> Map.add port.Id (fst label) currMap)
            let finalMap =
                (inputMap, outputPortIdLabels)
                ||> List.fold (fun currMap (port, label) -> Map.add port.Id (fst label) currMap)

            finalMap
        | _ -> Map.empty


let makeMapsConsistent (portIdMap: Map<string,string>) (sym: Symbol) =
    let edgeToAddTo = Right // TODO - choose Left or Right based on inpit or output
    let newPortIds = Set (portIdMap |> Map.keys)
    let currentPortIds = Set (sym.PortMaps.Orientation |> Map.keys)
    let addedPortIds = newPortIds - currentPortIds
    let deletedPortIds = currentPortIds - newPortIds
    let maps = sym.PortMaps
    (maps, addedPortIds) 
    ||> Set.fold (fun maps port -> addPortToMaps edgeToAddTo maps port)
    |> (fun maps -> maps, deletedPortIds)
    ||> Set.fold (fun maps port -> deletePortFromMaps port maps )

/// adjust symbol (and component) dimensions based on current ports and labels of a custom component.
/// leaves otehr symbols unchanged
let autoScaleHAndW (sym:Symbol) : Symbol =
    //height same as before, just take max of left and right
        match sym.Component.Type with
        | Custom comp ->
                let portIdMap = getCustomPortIdMap sym.Component
                let portMaps = makeMapsConsistent portIdMap sym
                let convertIdsToLbls currMap edge idList =
                    let lblLst = List.map (fun id -> portIdMap[id]) idList
                    Map.add edge lblLst currMap
                let portLabels = 
                    (Map.empty, portMaps.Order) ||> Map.fold convertIdsToLbls
                let h = Constants.gridSize + Constants.gridSize * max (List.length portLabels[Left]) (List.length portLabels[Right])

                let maxLeftLength = customStringToLength portLabels[Left] 
                let maxRightLength = customStringToLength portLabels[Right]

                //need to check the sum of the lengths of top and bottom
                let topLength = customStringToLength portLabels[Top] 
                let bottomLength = customStringToLength portLabels[Bottom]

                //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
                let maxW = 
                    [(maxLeftLength + maxRightLength + sym.Component.Label.Length)*Constants.gridSize/5;
                    (List.length portLabels[Top] + 1)* max (topLength*Constants.gridSize/5)Constants.gridSize;
                    (List.length portLabels[Bottom]+ 1)*max (bottomLength*Constants.gridSize/5) Constants.gridSize]
                    |> List.max 
                let w = roundToN Constants.gridSize (maxW ) 
                let scaledW = max w (Constants.gridSize * 4) //Ensures a minimum width if the labels are very small
                let scaledH = max h (Constants.gridSize*2)
                {sym with
                    PortMaps = portMaps
                    Component = {sym.Component with H= float scaledH; W = float scaledW}}

        | _ -> sym


let makeComponent (pos: XYPos) (comptype: ComponentType) (id:string) (label:string) : Component =
    let defaultSTransform = {Rotation = Degree0; flipped = false}
    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent' (n, nout, h, w) label : Component=
        let inputPorts = portLists n id PortType.Input
        let outputPorts = portLists nout id PortType.Output
        let comptype' =
            match comptype with

            | _ -> comptype
        {
            Id = id 
            Type = comptype' 
            Label = label 
            InputPorts = inputPorts
            OutputPorts  = outputPorts
            X  = pos.X - float w / 2.0
            Y = pos.Y - float h / 2.0
            H = float h 
            W = float w
            SymbolInfo = Some { 
                LabelBoundingBox = None
                STransform=defaultSTransform; 
                PortOrder = Map.empty; 
                PortOrientation=Map.empty}
        }
    
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let gS = Constants.gridSize
    let args = 
        match comptype with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2 , 1, 2*gS , 2*gS) 
        | Not -> ( 1 , 1, 2*gS ,  2*gS) 
        | ComponentType.Input (a) -> ( 0 , 1, gS ,  2*gS)                
        | ComponentType.Output (a) -> (  1 , 0, gS ,  2*gS) 
        | ComponentType.Viewer a -> (  1 , 0, gS ,  gS) 
        | ComponentType.IOLabel  ->(  1 , 1, gS ,  2*gS) 
        | Decode4 ->( 2 , 4 , 4*gS  , 3*gS) 
        | Constant1 (a, b,_) | Constant(a, b) -> (  0 , 1, gS ,  2*gS) 
        | MergeWires -> ( 2 , 1, 2*gS ,  2*gS) 
        | SplitWire (a) ->(  1 , 2 , 2*gS ,  2*gS) 
        | Mux2 -> ( 3  , 1, 3*gS ,  2*gS) 
        | Mux4 -> ( 5  , 1, 5*gS ,  2*gS)   
        | Mux8 -> ( 9  , 1, 7*gS ,  2*gS) 
        | Demux2 ->( 2  , 2, 3*gS ,  2*gS) 
        | Demux4 -> ( 2  , 4, 150 ,  50) 
        | Demux8 -> ( 2  , 8, 200 ,  50) 
        | BusSelection (a, b) -> (  1 , 1, gS,  2*gS) 
        | BusCompare (a, b) -> ( 1 , 1, gS ,  2*gS) 
        | DFF -> (  1 , 1, 3*gS  , 3*gS) 
        | DFFE -> ( 2  , 1, 3*gS  , 3*gS) 
        | Register (a) -> ( 1 , 1, 3*gS  , 4*gS )
        | RegisterE (a) -> ( 2 , 1, 3*gS  , 4*gS) 
        | AsyncROM1 (a)  -> (  1 , 1, 4*gS  , 5*gS) 
        | ROM1 (a) -> (   1 , 1, 4*gS  , 5*gS) 
        | RAM1 (a) | AsyncRAM1 a -> ( 3 , 1, 4*gS  , 5*gS) 
        | NbitsXor (n) -> (  2 , 1, 4*gS  , 4*gS) 
        | NbitsAdder (n) -> (  3 , 2, 3*gS  , 4*gS) 
        | Custom cct -> getCustomCompArgs cct label
                
    makeComponent' args label


// Function to generate a new symbol
let createNewSymbol (ldcs: LoadedComponent list) (pos: XYPos) (comptype: ComponentType) (label:string) =
    let id = JSHelpers.uuid ()
    let style = Constants.componentLabelStyle
    let comp = makeComponent pos comptype id label
    let transform = {Rotation= Degree0; flipped= false}
    { 
      Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
      LabelBoundingBox = calcLabelBoundingBox style comp transform
      LabelHasDefaultPos = true
      Appearance =
          {
            HighlightLabel = false
            ShowInputPorts = false
            ShowOutputPorts = false
            Colour = "lightgrey"
            Opacity = 1.0
          }
      InWidth0 = None // set by BusWire
      InWidth1 = None
      Id = ComponentId id
      Component = comp
      Moving = false
      PortMaps = initPortOrientation comp
      STransform = transform
      MovingPort = None
      IsClocked = isClocked [] ldcs comp
    }
    |> autoScaleHAndW

// Function to add ports to port model     
let addToPortModel (model: Model) (sym: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = (model.Ports, sym.Component.InputPorts) ||> List.fold addOnePort
    (addedInputPorts, sym.Component.OutputPorts) ||> List.fold addOnePort

//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

/// hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
let inline getPortPosEdgeGap (ct: ComponentType) =
    match ct with
    | MergeWires | SplitWire _  -> 0.25
    | _ -> 1.0

///Given a symbol and a Port, it returns the orientation of the port
let inline getSymbolPortOrientation (sym: Symbol) (port: Port): Edge =
    let portId = port.Id
    sym.PortMaps.Orientation[portId]


/// Returns the xy offset of a side relative to the symbol topleft
let inline getPortBaseOffset (sym: Symbol) (side: Edge): XYPos=
    let h,w = getRotatedHAndW sym
    match side with 
    | Right -> {X = w; Y = 0.0}
    | Left -> {X = 0.0; Y = 0.0}
    | Top -> {X = 0.0; Y = 0.0}
    | Bottom -> {X = 0.0; Y = h}

/// Returns true if an edge has the select port of a mux
let isMuxSel (sym:Symbol) (side:Edge): bool =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 ->
        match sym.STransform.Rotation, side with
        | Degree0, Bottom | Degree0, Top
        | Degree90, Left | Degree90, Right
        | Degree180, Bottom | Degree180, Top
        | Degree270, Left | Degree270, Right
             -> true
        | _ -> false
    | _ -> false


/// Based on a symbol and an edge, if the port is a mux select, return an extra offset required for the port (because of the weird shape of the mux)
let getMuxSelOffset (sym: Symbol) (side: Edge): XYPos =
    let compType = sym.Component.Type
    if isMuxSel sym side && (compType=Mux2 || compType=Demux2) then
        match side with 
            | Top -> {X = 0.0; Y = 10}
            | Bottom -> {X = 0.0; Y = -10}
            | Left -> {X = 10; Y = 0.0}
            | Right -> {X = -10; Y = 0.0}
    elif isMuxSel sym side && (compType=Mux4 || compType=Demux4) then
        match side with 
            | Top -> {X = 0.0; Y = 15}
            | Bottom -> {X = 0.0; Y = -15}
            | Left -> {X = 15; Y = 0.0}
            | Right -> {X = -15; Y = 0.0}
    elif isMuxSel sym side && (compType=Mux8 || compType=Demux8) then
        match side with 
            | Top -> {X = 0.0; Y = 20}
            | Bottom -> {X = 0.0; Y = -20}
            | Left -> {X = 20; Y = 0.0}
            | Right -> {X = -20; Y = 0.0}
    else
        {X=0.0; Y=0.0}


///Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPortPos (sym: Symbol) (port: Port) : XYPos =
    //get ports on the same edge first
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let index = float( List.findIndex (fun (p:string)  -> p = port.Id) ports )
    let gap = getPortPosEdgeGap sym.Component.Type 
    let baseOffset = getPortBaseOffset sym side  //offset of the side component is on
    let baseOffset' = baseOffset + getMuxSelOffset sym side
    let h,w = getRotatedHAndW sym
    match side with
    | Left ->
        let yOffset = (float(h))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))
        baseOffset' + {X = 0.0; Y = yOffset }
    | Right -> 
        let yOffset = (float(h))* (( float( ports.Length ) - index - 1.0 + gap )/( float( ports.Length ) + 2.0*gap - 1.0))
        baseOffset' + {X = 0.0; Y = yOffset }
    | Bottom -> 
        let xOffset = (float(w))* ((index + gap)/(float (ports.Length) + 2.0*gap - 1.0))
        baseOffset' + {X = xOffset; Y = 0.0 }
    | Top ->
        let xOffset = (float(w))* (( float( ports.Length ) - index - 1.0 + gap)/(float (ports.Length) + 2.0*gap - 1.0))
        baseOffset' + {X = xOffset; Y = 0.0 }

/// Gives the port positions to the render function, it gives the moving port pos where the mouse is, if there is a moving port
let inline getPortPosToRender (sym: Symbol) (port: Port) : XYPos =
    match sym.MovingPort with
    | Some movingPort when port.Id = movingPort.PortId -> movingPort.CurrPos - sym.Pos
    | _ -> getPortPos sym port

let inline getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

//-----------------------------------------DRAWING HELPERS ---------------------------------------------------
// Text adding function with many parameters (such as bold, position and text)
let private addText (pos: XYPos) name alignment weight size =
    let text =
            {defaultText with TextAnchor = alignment; FontWeight = weight; FontSize = size}
    [makeText pos.X pos.Y name text]

let private addStyledText (style:Text) (pos: XYPos) (name: string) = 
    makeText pos.X pos.Y name style

/// Generate circles on ports
let inline private portCircles (pos: XYPos) = 
    [makeCircle pos.X pos.Y portCircle]

/// Puts name on ports
let private portText (pos: XYPos) name edge =
    let pos' = 
            match edge with 
            | Left -> pos + {X = 5.; Y = -6.}
            | Top -> pos + {X = 0.; Y = 5.}
            | Right -> pos + {X = -5.; Y = -6.}
            | Bottom -> pos + {X = 0.; Y = -15.}

    let align = 
            match edge with
            | Right -> "end"
            | Left -> "start"
            | _ -> "middle"
    (addText pos' name align "normal" "12px")

/// Print the name of each port 
let private drawPortsText (portList: list<Port>) (listOfNames: list<string>) (symb: Symbol) = 
    let getPortName name x = portText (getPortPosToRender symb portList[x]) name (symb.PortMaps.Orientation[portList.[x].Id])
    if listOfNames.Length < 1
    then []
    else 
        [0..(portList.Length-1)]
        |> List.map2 getPortName listOfNames 
        |> List.collect id

/// Function to draw ports using getPortPos. The ports are equidistant     
let private drawPorts (portList: Port List) (printPorts:bool) (symb: Symbol)= 
    if not (portList.Length < 1) && printPorts 
    then [0..(portList.Length-1)] |> List.collect (fun x -> (portCircles (getPortPosToRender symb portList[x])))
    else []

//------------------------------HELPER FUNCTIONS FOR DRAWING SYMBOLS-------------------------------------
let private createPolygon points colour opacity = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

let createBiColorPolygon points colour strokeColor opacity strokeWidth= 
    if strokeColor <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]

let addClock (pos: XYPos) colour opacity =
    let points = sprintf $"{pos.X},{pos.Y-1.},{pos.X+8.},{pos.Y-7.},{pos.X},{pos.Y-13.}"
    createPolygon points colour opacity
    |> List.append (addText (pos + {X = 10.; Y = -13.} ) " clk" "start" "normal" "12px")

let addHorizontalLine posX1 posX2 posY opacity = // TODO: Line instead of polygon?
    let points = sprintf $"{posX1},{posY},{posX2},{posY}"
    createPolygon points "lightgray" opacity

let outlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> c

let addHorizontalColorLine posX1 posX2 posY opacity (color:string) = // TODO: Line instead of polygon?
    let points = sprintf $"{posX1},{posY} {posX2},{posY}"
    let outlineColor = outlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=outlineColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

/// Takes points, height and width of original shape and returns the points for it given a rotation / flipped status.
/// Degree0 rotation has TopLeft = top left coordinate of the outline, which is a box of dimensions W X H.
/// Rotation rotates the box about its centre point, keeping TopLeft fixed.
let rotatePoints (points) (centre:XYPos) (transform:STransform) = 
    let offset = 
            match transform.Rotation with
            | Degree0 | Degree180 -> centre
            | Degree90 | Degree270 -> {X = centre.Y; Y = centre.X}

    let relativeToCentre = Array.map (fun x -> x - centre)
    let rotateAboutCentre pointsIn = 
        match transform.Rotation with
        | Degree0   -> pointsIn
        | Degree270 -> Array.map (fun (pos:XYPos) -> {X = -pos.Y ; Y = pos.X}) pointsIn
        | Degree180 -> Array.map (fun (pos:XYPos) -> {X = -pos.X ; Y = -pos.Y}) pointsIn
        | Degree90  -> Array.map (fun (pos:XYPos) -> {X = pos.Y ; Y = -pos.X}) pointsIn

    let relativeToTopLeft = Array.map (fun x -> x + offset ) 
    /// Flips the points, needed some hacks to avoid saving transforms somewhere / saving current points
    /// Also can't guarantee it will work if there are changes to rotation / flip with funkier shapes
    let flipIfNecessary pts =
        if not transform.flipped then pts
        else
            match transform.Rotation with
            | _ -> Array.map (fun (point:XYPos) -> {X = -point.X; Y = point.Y}) pts

    points
    |> relativeToCentre
    |> rotateAboutCentre
    |> flipIfNecessary
    |> relativeToTopLeft


/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///  



let drawSymbol (symbol:Symbol) (colour:string) (showInputPorts:bool) (showOutputPorts:bool) (opacity: float) = 
    let comp = symbol.Component
    let h,w = getRotatedHAndW symbol
    let H = float comp.H
    let W = float comp.W
    let transform = symbol.STransform

    let mergeSplitLine pos msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> sprintf $"({msb})"
            | false, _ -> sprintf $"({msb}:{lsb})"
        addText pos text "middle" "bold" "9px"

    let clockTxtPos = 
        match transform.Rotation, transform.flipped with
        | Degree0, false -> {X = 17.; Y = H - 13.}
        | Degree180, true -> {X = 17.; Y = 2.}
        | Degree90, false -> {X = float w - 8.; Y = float h - 20.}
        | Degree270, true ->  {X = float w - 10.; Y = 11.}
        | Degree180, false -> {X = W - 19.; Y = 2.}
        | Degree0, true -> {X = W - 17.; Y = H - 13.}
        | Degree270, false -> {X = 10.; Y = 11.}
        | Degree90, true -> {X = 8.; Y = float h - 20.}

    /// Points that define the edges of the symbol
    let points =
        let toString = Array.fold (fun x (pos:XYPos) -> x + (sprintf $" {pos.X},{pos.Y}")) "" 
        let originalPoints =
            match comp.Type with
            | Input _ -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W*4./5.;Y=H};{X=W;Y=H/2.};{X=W*0.8;Y=0}|] 
            | Output _ -> 
                [|{X=W/5.;Y=0};{X=0;Y=H/2.};{X=W/5.;Y=H};{X=W;Y=H};{X=W;Y=0}|]
            | Constant1 _ -> 
                [|{X=W;Y=H/2.};{X=W/2.;Y=H/2.};{X=0;Y=H};{X=0;Y=0};{X=W/2.;Y=H/2.}|]
            | IOLabel ->
                [|{X=W/3.;Y=0};{X=0;Y=H/2.};{X=W/3.;Y=H};{X=W*0.66;Y=H};{X=W;Y=H/2.};{X=W*0.66;Y=0}|]
            | Viewer _ ->
                [|{X=W/5.;Y=0};{X=0;Y=H/2.};{X=W/5.;Y=H};{X=W;Y=H};{X=W;Y=0}|]
            | MergeWires -> 
                [|{X=0;Y=H/6.};{X=W/2.;Y=H/6.};{X=W/2.;Y=H/2.};{X=W;Y=H/2.};{X=W/2.;Y=H/2.};{X=W/2.;Y=5.*H/6.};{X=0;Y=5.*H/6.};{X=W/2.;Y=5.*H/6.};{X=W/2.;Y=H/6.}|]
            | SplitWire _ -> 
                [|{X=W;Y=H/6.};{X=W/2.;Y=H/6.};{X=W/2.;Y=H/2.};{X=0;Y=H/2.};{X=W/2.;Y=H/2.};{X=W/2.;Y=5.*H/6.};{X=W;Y=5.*H/6.};{X=W/2.;Y=5.*H/6.};{X=W/2.;Y=H/6.}|]
            // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
            // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
            | Demux2 ->
                [|{X=0;Y=H/5.};{X=0;Y=H*0.8};{X=W;Y=H};{X=W;Y=0}|]
            | Mux2 ->
                [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H*0.8};{X=W;Y=H/5.}|]
            | Demux2 | Demux4 | Demux8 ->
                [|{X=0;Y=H/5.};{X=0;Y=H*0.8};{X=W;Y=H};{X=W;Y=0}|]
            | Mux2 | Mux4 | Mux8 -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H*0.8};{X=W;Y=H/5.}|]
            | BusSelection _ |BusCompare _ -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W*0.6;Y=H};{X=W*0.8;Y=H*0.7};{X=W;Y=H*0.7};{X=W;Y =H*0.3};{X=W*0.8;Y=H*0.3};{X=W*0.6;Y=0}|]
            | Not | Nand | Nor | Xnor -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H};{X=W;Y=H/2.};{X=W+9.;Y=H/2.};{X=W;Y=H/2.-8.};{X=W;Y=H/2.};{X=W;Y=0}|]
            | DFF | DFFE | Register _ | RegisterE _ | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> 
                [|{X=0;Y=H-13.};{X=8.;Y=H-7.};{X=0;Y=H-1.};{X=0;Y=0};{X=W;Y=0};{X=W;Y=H};{X=0;Y=H}|]
            | Custom x when symbol.IsClocked = true -> 
                [|{X=0;Y=H-13.};{X=8.;Y=H-7.};{X=0;Y=H-1.};{X=0;Y=0};{X=W;Y=0};{X=W;Y=H};{X=0;Y=H}|]
            | _ -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H};{X=W;Y=0}|]
        rotatePoints originalPoints {X=W/2.;Y=H/2.} transform
        |> toString 



    let additions =       // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
        let mergeWiresTextPos =
            let textPoints = rotatePoints [|{X=W/5.;Y=H/6.+2.};{X=W/5.;Y=H*5./6.+2.};{X=W*0.75;Y=H/2.+2.}|] {X=W/2.;Y=H/2.} transform
            match transform.Rotation with
            | Degree90 | Degree270 -> Array.map (fun pos -> pos + {X=12.;Y=0}) textPoints
            | Degree180 -> Array.map (fun pos -> pos + {X=0;Y= +5.}) textPoints
            | _ -> textPoints
        let splitWiresTextPos =
            let textPoints = rotatePoints [|{X=W*0.75;Y=H/6.+2.};{X=W*0.75;Y=H*5./6.+2.};{X=W/4.;Y=H/2.+2.}|] {X=W/2.;Y=H/2.} transform
            match transform.Rotation with
            | Degree90 | Degree270 -> Array.map (fun pos -> pos + {X=12.;Y=0}) textPoints
            | Degree180 -> Array.map (fun pos -> pos + {X=0;Y= +5.}) textPoints
            | _ -> textPoints

        match comp.Type with
        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            let values = [(midt,0);(msb,midb);(msb,0)]
            List.fold (fun og i -> og @ mergeSplitLine mergeWiresTextPos[i] (fst values[i]) (snd values[i]) ) [] [0..2]
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            let values = [(midt,0);(msb,midb);(msb,0)]
            List.fold (fun og i -> og @ mergeSplitLine splitWiresTextPos[i] (fst values[i]) (snd values[i]) ) [] [0..2]
        | DFF | DFFE | Register _ |RegisterE _ | ROM1 _ |RAM1 _ | AsyncRAM1 _  -> 
            (addText clockTxtPos " clk" "middle" "normal" "12px")
        | BusSelection(x,y) -> (addText {X = w/2.; Y = (h/2.7)-2.} (bustitle x y) "middle" "bold" "12px")
        | BusCompare (_,y) -> (addText {X = w/2.-2.; Y = h/2.7-1.} ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
        | Input (x) -> (addText {X = w/2.; Y = h/2.7-3.} (title "" x) "middle" "normal" "12px")
        | Output (x) -> (addText {X = w/2.; Y = h/2.7-3.} (title "" x) "middle" "normal" "12px")
        | Viewer (x) -> (addText {X = w/2.; Y = h/2.7 - 1.25} (title "" x) "middle" "normal" "9px")
        | _ when symbol.IsClocked -> (addText (Array.head (rotatePoints [|{X = 15.; Y = float H - 11.}|] {X=W/2.;Y=H/2.} transform )) " clk" "middle" "normal" "12px")
        | _ -> []

    let outlineColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColor colour, "2.0"
        | _ -> "black", "1.0"
    


    /// to deal with the label
    let addComponentLabel (comp: Component) transform = 
        // we are restricted to monospace font for labels to calc size.
        // bold and normal fonts work equally well, as do colors.
        let weight = Constants.componentLabelStyle.FontWeight // bold or normal
        let fill = Constants.componentLabelStyle.Fill // color
        let style = {Constants.componentLabelStyle with FontWeight = weight; Fill = fill}
        let box =
            match symbol.LabelHasDefaultPos with 
            | true -> calcLabelBoundingBox style comp transform
            | false -> symbol.LabelBoundingBox
        let margin = Constants.componentLabelOffsetDistance
        // uncomment this to display label bounding box corners for testing new fonts etc.
        let dimW = {X=box.W;Y=0.}
        let dimH = {X=0.;Y=box.H}
        let corners = 
            [box.TopLeft; box.TopLeft+dimW; box.TopLeft+dimH; box.TopLeft+dimW+dimH]
            |> List.map (fun c -> 
                let c' = c - symbol.Pos
                makeCircle (c'.X) (c'.Y) {defaultCircle with R=3.})

        addStyledText 
            {style with DominantBaseline="hanging"} 
            (box.TopLeft - symbol.Pos + {X=margin;Y=margin}) 
            comp.Label ::  [] //corners uncomment this to display label bounding box corners.



 
            
            
   
    // Put everything together 

    (drawPorts comp.OutputPorts showOutputPorts symbol)
    |> List.append (drawPorts comp.InputPorts showInputPorts symbol)
    |> List.append (drawPortsText (comp.InputPorts @ comp.OutputPorts) (portNames comp.Type) symbol)
    |> List.append (addText {X = float w/2.; Y = float h/2. - 7.} (getComponentLegend comp.Type) "middle" "bold" "14px")
    |> List.append (addComponentLabel comp transform)
    |> List.append (additions)
    |> List.append (createBiColorPolygon points colour outlineColour opacity strokeWidth)

let init () = 
    { 
        Symbols = Map.empty; CopiedSymbols = Map.empty
        Ports = Map.empty ; InputPortsConnected= Set.empty
        OutputPortsConnected = Map.empty;
    }, Cmd.none

//----------------------------View Function for Symbols----------------------------//
type private RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }

/// View for one symbol. Using FunctionComponent.Of to improve efficiency (not printing all symbols but only those that are changing)
let private renderSymbol =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y=fY}:XYPos) = symbol.Pos
            let appear = symbol.Appearance
            g ([ Style [ Transform(sprintf $"translate({fX}px, {fY}px)") ] ]) 
                (drawSymbol props.Symbol appear.Colour appear.ShowInputPorts appear.ShowOutputPorts appear.Opacity)
            
        , "Symbol"
        , equalsButFunctions
        )
    
/// View function for symbol layer of SVG
let MapsIntoLists map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |>Map.toList
        |>List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |>Map.toList
        |>List.map snd
    listMoving @ listNotMoving


let view (model : Model) (dispatch : Msg -> unit) =    
    /// View function for symbol layer of SVG
    let toListOfMovingAndNot map =
        let listMoving = 
            Map.filter (fun _ sym -> not sym.Moving) map
            |> Map.toList
            |> List.map snd
        let listNotMoving =
            Map.filter (fun _ sym -> sym.Moving) map
            |> Map.toList
            |> List.map snd
        listMoving @ listNotMoving

    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> toListOfMovingAndNot
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
        renderSymbol
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList
    |> TimeHelpers.instrumentInterval "SymbolView" start

//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.
/// Returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol.
/// Works with rotation.
let inline getSymbolBoundingBox (sym:Symbol): BoundingBox =
    let h,w = getRotatedHAndW sym
    {TopLeft = sym.Pos; H = float(h) ; W = float(w)}

/// Returns all the bounding boxes of all components in the model
let getBoundingBoxes (symModel: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun sId (sym:Symbol) -> (getSymbolBoundingBox sym)) symModel.Symbols

/// Returns bounding box of a component based on component id
let inline getBoundingBox (symModel: Model) (compid: ComponentId ): BoundingBox = 
    let symb = Map.find compid symModel.Symbols
    getSymbolBoundingBox symb

/// Returns the bounding boxes of all symbol labels in the model
let getLabelBoundingBoxes (model: Model) : Map<ComponentId, BoundingBox> =
    model.Symbols
    |> Map.map (fun _ sym -> sym.LabelBoundingBox)

/// Returns the bounding box of the symbol associated with compId
let getLabelBoundingBox (model: Model) (compId: ComponentId) : BoundingBox =
    Map.find compId model.Symbols
    |> (fun sym -> sym.LabelBoundingBox)


//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
/// Returns the center coordinates of a Symbol
let getSymbolPos (symbolModel: Model) compId = //makes sense or should we have getSymbol?
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: Model) : (ComponentId list) =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst


/// Returns the port object associated with a given portId
let inline getPort (symModel: Model) (portId: string) =
    symModel.Ports[portId]

let inline getSymbol (model: Model) (portId: string) =
    let port = getPort model portId
    model.Symbols[ComponentId port.HostId]

let inline getCompId (model: Model) (portId: string) =
    let symbol = getSymbol model portId
    symbol.Id

/// Returns the string of a PortId
let inline getPortIdStr (portId: PortId) = 
    match portId with
    | InputId (InputPortId id) -> id
    | OutputId (OutputPortId id) -> id

let inline getInputPortIdStr (portId: InputPortId) = 
    match portId with
    | InputPortId s -> s

let inline getOutputPortIdStr (portId: OutputPortId) = 
    match portId with
    | OutputPortId s -> s

/// returns what side of the symbol the port is on
let inline getPortOrientation (model: Model)  (portId: PortId) : Edge =
    let portIdStr = getPortIdStr portId
    let port = model.Ports[portIdStr]
    let sId = ComponentId port.HostId
    model.Symbols[sId].PortMaps.Orientation[portIdStr]

let inline getInputPortOrientation (model: Model) (portId: InputPortId): Edge =
    getPortOrientation model (InputId portId)

let inline getOutputPortOrientation (model: Model) (portId: OutputPortId): Edge =
    getPortOrientation model (OutputId portId)


/// Returns the location of a given portId, with good efficiency
let getPortLocation (defPos: XYPos option) (model: Model) (portId : string) : XYPos=
    let portOpt = Map.tryFind portId model.Ports
    let symbolIdOpt = 
        portOpt
        |> Option.map (fun port ->  ComponentId port.HostId)
    let symOpt = 
        symbolIdOpt
        |> Option.bind (fun symbolId -> Map.tryFind symbolId model.Symbols)
    match defPos, symOpt, portOpt with
    | _, Some sym, Some port -> getPortPos sym port + sym.Pos
    | Some pos, _, _ ->
        printfn $"Can't find port or symbol: Port='{portOpt}', Symbol='{symOpt}"
        pos       
    | _ -> failwithf $"Can't find port or symbol: Port='{portOpt}', Symbol='{symOpt}"

/// Returns the location of an input port based on their portId
let inline getInputPortLocation defPos (model:Model) (portId: InputPortId)  = 
    let id = getPortIdStr (InputId portId)
    getPortLocation defPos model id

/// Returns the location of an output port based on their portId
let inline getOutputPortLocation defPos (model:Model) (portId : OutputPortId) =
    let id = getPortIdStr (OutputId portId)
    getPortLocation defPos model id

/// Returns the locations of a given input port and output port based on their portId
let inline getTwoPortLocations (model: Model) (inputPortId: InputPortId ) (outputPortId: OutputPortId) =
    (getInputPortLocation None model inputPortId, getOutputPortLocation None model outputPortId)

///Returns the input port positions of the specified symbols in model
let getInputPortsLocationMap (model: Model) (symbols: Symbol list)  = 
    let getSymbolInputPortsLoc sym =
        sym.Component.InputPorts 
        |> List.map (fun port -> (InputPortId port.Id, (getPortPos sym port) + (sym.Pos)))
        
    symbols
    |> List.collect getSymbolInputPortsLoc
    |> Map.ofList

/// Returns the output port positions of the specified symbols in model
let getOutputPortsLocationMap (model: Model) (symbols: Symbol list)  =
    let getSymbolOutputPortsLoc sym =
        sym.Component.OutputPorts 
        |> List.map (fun port -> (OutputPortId port.Id, (getPortPos sym port) + (sym.Pos)))
        
    symbols
    |> List.collect getSymbolOutputPortsLoc
    |> Map.ofList


/// Returns all the port locations of the given components   
let getPortLocations (model: Model) (symbolIds: ComponentId list) = 
    let symbols = 
        model.Symbols 
        |> Map.filter (fun symbolId _  -> List.contains symbolId symbolIds)
        |> Map.toList
        |> List.map snd
        
    let getInputPortMap = getInputPortsLocationMap model symbols
    let getOutputPortMap = getOutputPortsLocationMap model symbols
       
    getInputPortMap , getOutputPortMap 
 
