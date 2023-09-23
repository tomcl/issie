﻿(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Fable.React
open Fable.React.Props
open Elmish

open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT
open BlockHelpers



/// --------- STATIC VARIABLES --------- ///
module Constants =
    [<Literal>]
    let gridSize = 30 
    let mergeSplitTextSize = "12px"
    let busSelectTextSize = "12px"
    let customPortSpacing = 40.
    let portPosEdgeGap = 0.7
    let gatePortPosEdgeGap = 0.3
    let legendVertOffset = 5.
    let legendLineSpacingInPixels = 16.
    let testShowLabelBoundingBoxes = false

    /// Font size for ports
    let portFontSizeInPixels :float = 12

    /// Font size for  component labels
    let labelFontSizeInPixels :float = 16 // other parameters scale correctly with this

    /// Font size for legends (names) in custom components
    let customLegendFontSizeInPixels: float = 16.

    /// Font size for legends on otehr components
    let otherLegendFontSizeInPixels: float = 14

    /// Text weight for bottom line (bit indication) of legends
    let bitIndicationFontWeight: string = "400"

    /// Most fonts now work perfectly - see playground.fs for some tests - add more as needed.
    /// Font style used by all component labels
    let componentLabelStyle: Text = 
        {defaultText with 
            TextAnchor = "start"; 
            FontSize = $"%.0f{labelFontSizeInPixels}px"; 
            FontFamily = "Verdana"; 
            FontWeight="500"}

    /// Most fonts now work perfectly - see playground.fs for some tests - add more as needed.
    /// Font style used by all Ports
    let componentPortStyle: Text =
        {defaultText with 
            TextAnchor = "start"; 
            FontSize = $"%.0f{portFontSizeInPixels}px"; 
            FontFamily = "Verdana"; 
            FontWeight="500"}

    /// Most fonts now work perfectly - see playground.fs for some tests - add more as needed.
    /// Style used by bus select bit legends
    let busSelectStyle: Text = 
        {defaultText with 
            TextAnchor = "start"; 
            FontSize = "12px"; 
            FontFamily = "helvetica"; 
            FontWeight="600"}


    /// Offset between label position and symbol. This is also used as a margin for the label bounding box.
    let componentLabelOffsetDistance: float =  // offset from symbol outline, otehr parameters scale correctly
        if testShowLabelBoundingBoxes then 0. else 7.

    /// Offset between label poistion and symbol for "thin" components.
    /// Wire labels etc.
    let thinComponentLabelOffsetDistance: float = 
        if testShowLabelBoundingBoxes then 0. else 3.
    
    /// Height of label text - used to determine where to print labels
    let componentLabelHeight: float = labelFontSizeInPixels

    /// Small manual correction added to claculated position for placing labels.
    /// Used to make labels equidistant on all sides of symbol.
    let labelCorrection = {X= 0.; Y= 0.}

    let customComponentHint = div [] [str "Press Ctrl and drag to"; br []; str "move ports or resize symbol"]
    
    
//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.

/// Returns the correct height and width of a transformed symbol
/// as the tuple (real H, real W).
/// Needed because H & W in Component do not chnage with rotation.
/// NB Pos in component = Pos in Symbol and DOES change with rotation!
let inline getCompRotatedHAndW (comp: Component) (transform: STransform) hScale vScale  =
    let hS,vS = (Option.defaultValue 1.0 hScale),(Option.defaultValue 1.0 vScale)
    match transform.Rotation with
    | Degree0 | Degree180 -> comp.H*vS, comp.W*hS
    | Degree90 | Degree270 -> comp.W*hS, comp.H*vS

/// Returns the correct height and width of a transformed symbol
/// as the tuple (real H, real W).
/// Needed because H & W in Component do not chnage with rotation.
/// NB Pos in component = Pos in Symbol and DOES change with rotation!
let inline getRotatedHAndW sym  = getCompRotatedHAndW sym.Component sym.STransform sym.HScale sym.VScale

/// returns the true centre of a component's symbol
let inline getRotatedCompCentre comp transform hScale vScale =
    // true component BB is (comp.X,comp.Y), h, w
    let h,w = getCompRotatedHAndW comp transform hScale vScale
    let centreX = comp.X + w / 2.
    let centreY = comp.Y + h / 2.
    {X=centreX;Y=centreY}

/// returns the true centre of a symbol, taking into account
/// its current rotation
let inline getRotatedSymbolCentre (symbol:Symbol) =
    getRotatedCompCentre symbol.Component symbol.STransform symbol.HScale symbol.VScale
/// Returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol.
/// Works with rotation. For a rotated symbol, TopLeft = Pos, and H,W swapped in getrotatedHAndW
let inline getSymbolBoundingBox (sym:Symbol): BoundingBox =
    let h,w = getRotatedHAndW sym
    if sym.Annotation = Some ScaleButton then 
        {TopLeft = sym.Pos - {X = 9.; Y = 9.}; H = 17. ; W = 17.}
    else 
        {TopLeft = sym.Pos; H = float(h) ; W = float(w)}

type Symbol with
    member this.SymbolBoundingBox = getSymbolBoundingBox this

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

let inline invertRotation (rot: Rotation) =
    match rot with
    | Degree0 -> Degree180
    | Degree180 -> Degree0
    | Degree90 -> Degree270
    | Degree270 -> Degree90


/// the output is the rotation got from the two inputs one after the other.
let inline combineRotation (r1:Rotation) (r2:Rotation) =
    let rot90 rot =
        match rot with
        | Degree0 -> Degree90
        | Degree90 -> Degree180
        | Degree180 -> Degree270
        | Degree270 -> Degree0
    let rot180 rot =
        match rot with 
        | Degree0 -> Degree180
        | Degree90 -> Degree270
        | Degree180 -> Degree0
        | Degree270 -> Degree90
    match r1 with
    | Degree0 -> r2
    | Degree90 -> rot90 r2
    | Degree180 -> rot180 r2
    | Degree270 -> (rot90 >> rot180) r2


/// Returns color for the fill of a symbol (when not selected)
/// Depends on theme and whether symbol is closed or not.
let getSymbolColour compType clocked (theme:ThemeType) =
    match theme with
    | White | Light -> "lightgray"
    | Colourful ->
        match compType with
        | Register _ | RegisterE _ 
        | ROM1 _ | DFF | DFFE | RAM1 _ | AsyncRAM1 _ 
        | Counter _ |CounterNoEnable _ | CounterNoLoad _  |CounterNoEnableLoad _ -> "lightblue"
        | Custom _ when clocked
            -> "lightblue"  //for clocked components
        |Input _ |Input1 (_,_) |Output _ |Viewer _ |Constant _ |Constant1 _ 
            -> "#E8D0A9"  //dark orange: for IO
        | SplitWire _ | MergeWires _ | BusSelection _ | NbitSpreader _ | IOLabel | NotConnected ->
            "rgb(120,120,120)"
        | _ -> "rgba(255,255,217,1)" //lightyellow: for combinational components



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


/// Work out a label bounding box from symbol, return symbol with box added.
/// The box has a margin Constants. componentLabelOffsetDistance around the label text outline.
/// This function should be included at the end of any function that changes component 
/// or label position or orientation or shape.
let calcLabelBoundingBox (sym: Symbol) =
    let textStyle = Constants.componentLabelStyle
    let transform = sym.STransform
    let comp = sym.Component
    let labelRotation = 
        match transform.flipped with
        | true -> match transform.Rotation with
                     | Degree90 -> Degree270
                     | Degree270 -> Degree90
                     | _ -> transform.Rotation
        | false -> transform.Rotation
        |> combineRotation (Option.defaultValue Degree0 sym.LabelRotation)
    let h,w = getRotatedHAndW sym
    let centre = getRotatedSymbolCentre sym

    let margin = 
        match sym.Component.Type with
        | IOLabel | BusSelection _ | NotConnected -> Constants.thinComponentLabelOffsetDistance
        | _ -> Constants.componentLabelOffsetDistance
    let labH = Constants.componentLabelHeight //height of label text
    //let labW = getMonospaceWidth textStyle.FontSize comp.Label
    let labW = getTextWidthInPixels textStyle comp.Label// width of label text
    let boxTopLeft =
        match labelRotation with 
        | Degree0 -> {X = centre.X - labW/2. - margin; Y = comp.Y - labH - 2.*margin }
        | Degree270 -> {X = comp.X + w; Y = centre.Y - labH/2. - margin}
        | Degree90 -> {X = comp.X - 2.*margin - labW ; Y = centre.Y - labH/2. - margin}
        | Degree180 -> {X = centre.X - labW/2. - margin; Y = comp.Y + h}
    let box =
        match comp.Label, sym.LabelHasDefaultPos with
        | "", _ -> 
            {TopLeft=boxTopLeft; W = 0.; H = 0.}
        | _, true -> 
            {TopLeft=boxTopLeft; W = labW + 2.*margin; H = labH + 2.*margin}
        | _, false -> 
            sym.LabelBoundingBox
    {sym with LabelBoundingBox = box}


//------------------------------------------------------------------//
//------------------- Helper functions for titles ------------------//
//------------------------------------------------------------------//

///Insert titles compatible with greater than 1 buswidth
let busTitleAndBits (t:string) (n:int) : string =  
    match n with
    | 1 -> 
        t
    | _ when t = "" && n > 1 -> 
        $"{t}({n-1}:{0})"
    | _ when n > 1 -> 
        $"{t}.({n-1}:{0})"
    | _ -> 
        failwith "non positive bus width"


let nBitsGateTitle (gateType:string) (n:int) : string =
    match n with
    |1 -> gateType
    |_ -> (string n) + "-bit " + gateType 

///Insert titles for bus select
/// used once 
let busSelectTitle (wob:int) (lsb:int) : string = 
    match wob with
    | 1 -> $"{lsb}"
    | _ when wob > 1 -> $"({wob+lsb-1}:{lsb})"
    | _ -> failwith "non positive bus width in bustitle"

///Decodes the component type into component labels
let getPrefix (compType:ComponentType) = 
    match compType with
    | Not | GateN _ -> "G"
    | Mux2 -> "MUX"
    | Mux4 -> "MUX"
    | Mux8 -> "MUX"
    | Demux2 -> "DM"
    | Demux4 -> "DM"
    | Demux8 -> "DM"
    | Shift _ -> "SHIFT"
    | NbitsAdder _ | NbitsAdderNoCin _
    | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ 
        -> "ADD"
    | NbitsXor _ -> "NXOR"
    | NbitsAnd _ -> "AND"
    | NbitsOr _ -> "OR"
    | NbitsNot _ -> "NOT"
    | NbitSpreader _ -> "S"
    | DFF | DFFE -> "FF"
    | Register _ | RegisterE _ -> "REG"
    | AsyncROM1 _ -> "AROM"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncRAM1 _ -> "ARAM"
    | Custom c ->
        c.Name.ToUpper() + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ |BusCompare1 _ -> "EQ"
    | Decode4 -> "DEC"
    | Counter _ |CounterNoEnable _
    | CounterNoLoad _ |CounterNoEnableLoad _ -> "CNT"
    | MergeWires -> "MW"
    | MergeN _ -> "MN"
    | SplitWire _ -> "SW"
    |_  -> ""


let getGateComponentLegend gType =
    match gType with
    | And | Nand -> "&"
    | Or | Nor -> "≥1"
    | Xor | Xnor -> "=1"

// Text to be put inside different Symbols depending on their ComponentType
let getComponentLegend (componentType:ComponentType) (rotation:Rotation) =
    match componentType with
    | GateN (gateType, _) -> getGateComponentLegend gateType
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder n | NbitsAdderNoCin n
    | NbitsAdderNoCinCout n | NbitsAdderNoCout n -> busTitleAndBits "Adder" n
    | Register n | RegisterE n-> 
        match rotation with
        |Degree90 |Degree270 -> busTitleAndBits "Reg" n
        |_ -> busTitleAndBits "Register" n
    | AsyncROM1 _ -> "Async.ROM"
    | ROM1 _ -> "Sync.ROM"
    | RAM1 _ -> "Sync.RAM"
    | AsyncRAM1 _ -> "Async.RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | Counter n |CounterNoEnable n
    | CounterNoLoad n |CounterNoEnableLoad n -> busTitleAndBits "Counter" n
    | NbitsXor (x, None)->   nBitsGateTitle "XOR" x
    | NbitsXor (x, Some Multiply)->   nBitsGateTitle "Multiply" x
    | NbitsOr (x)->   nBitsGateTitle "OR" x
    | NbitsAnd (x)->   nBitsGateTitle "AND" x
    | NbitsNot (x)->  nBitsGateTitle "NOT" x
    | Shift (n,_,_) -> busTitleAndBits "Shift" n
    | Custom x -> x.Name.ToUpper()
    | MergeN _ -> "Merge"
    | _ -> ""


/// By default all output ports are on the right, input ports on lefty, (of a normal orientation symbol).
/// A few symbols have ports on top or bottom as defined here.
/// Note that custom components can have ports positioned by user.
let movePortsToCorrectEdgeForComponentType (ct: ComponentType) (portMaps: PortMaps): PortMaps =
    match ct with //need to put some ports to different edges
    | Mux2  -> //need to remove select port from left and move to bottom
        movePortToBottom portMaps 2
    | Mux4 -> //need to remove select port from left and move to right
        movePortToBottom portMaps 4
    | Mux8 ->
        movePortToBottom portMaps 8
    | NbitsAdder _ |NbitsAdderNoCout _ -> 
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



let customToLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

/// get the max length (in pixels) of any of the Text strings in a list
/// Hack - for now assume text char width is constant
let customStringToLength (lst: string list) =
    let getPortTextWidth (portLab:string) =
        getTextWidthInPixels Constants.componentPortStyle portLab
    match lst with
    | [] -> 0.
    | portStrL ->
        portStrL
        |> List.map getPortTextWidth
        |> List.max


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



    

(*
/// helper function to initialise custom components
let getCustomCompArgs (x:CustomComponentType) (label:string) =
    let h = Constants.gridSize + Constants.gridSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
    let maxInLength, maxOutLength = customToLength x.InputLabels, customToLength x.OutputLabels
    let maxW = maxInLength + maxOutLength + label.Length
    let scaledW = (maxW * Constants.gridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
    let w = max scaledW (Constants.gridSize * 4) //Ensures a minimum width if the labels are very small
    ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)
*)
/// obtain map from port IDs to port names for Custom Component.
/// for other components types this returns empty map
let getCustomPortIdMap (comp: Component)  =
        let label = comp.Label
        match comp.Type with
        | Custom customType ->
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

/// Needed because the I/Os of a custom component can be changed on anotehr sheet.
/// When the component is reloaded its port maps will be inconsistent.
/// This function keeps existing layout, and adds new I/Os or deletes old ones.
let makeMapsConsistent (portIdMap: Map<string,string>) (sym: Symbol) =
    let newPortIds = Set (portIdMap |> Map.keys)
    let currentPortIds = Set (sym.PortMaps.Orientation |> Map.keys)
    let addedPortIds = newPortIds - currentPortIds
    let deletedPortIds = currentPortIds - newPortIds
    let maps = sym.PortMaps
    (maps, addedPortIds) 
    ||> Set.fold (fun maps port -> 
                    let edgeToAddTo = // default add new outputs on right, inputs on left
                        match List.exists (fun (p:Port) -> p.Id = port) sym.Component.InputPorts with
                        | true -> Left
                        | false -> Right
                    addPortToMaps edgeToAddTo maps port)
    |> (fun maps -> maps, deletedPortIds)
    ||> Set.fold (fun maps port -> deletePortFromMaps port maps )

/// adjust symbol (and component) dimensions based on current ports and labels of a custom component.
/// leaves other symbols unchanged
let autoScaleHAndW (sym:Symbol) : Symbol =
    //height same as before, just take max of left and right
        match sym.Component.Type with
        | Custom ct ->
                let portIdMap = getCustomPortIdMap sym.Component
                let portMaps = makeMapsConsistent portIdMap sym
                let convertIdsToLbls currMap edge idList =
                    let lblLst = List.map (fun id -> portIdMap[id]) idList
                    Map.add edge lblLst currMap
                let portLabels = 
                    (Map.empty, portMaps.Order) ||> Map.fold convertIdsToLbls
                let h = float Constants.gridSize + Constants.customPortSpacing * float (max portLabels[Left].Length portLabels[Right].Length)
                let getHorizontalSpace side =
                    let labs = portLabels[side]
                    match labs.Length % 2 with
                    | 0 -> 0. // no space needed because we have even number of connections
                    | _ -> // odd number
                        labs[labs.Length / 2]
                        |> fun s -> customStringToLength [s]
                        
                let leftSpace = getHorizontalSpace Left
                let rightSpace = getHorizontalSpace Right

                //need to check the sum of the lengths of top and bottom
                let topLength = customStringToLength portLabels[Top] 
                let bottomLength = customStringToLength portLabels[Bottom]
                let labelLength = (Constants.customLegendFontSizeInPixels/Constants.portFontSizeInPixels) * customStringToLength [ct.Name]
                //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
                let maxW = 
                    [
                        (2.0*(max leftSpace rightSpace) + labelLength*1.333 + 10.) // 1.6 should be a constant - ratio of sizes - TODO. Also 10.
                        float (portLabels[Top].Length + 1) * topLength
                        float (portLabels[Bottom].Length + 1) * bottomLength
                    ] |> List.max |> (*) 1.1
                let w = maxW
                //printfn $"MaxW = {maxW}"
                let scaledW = max w (float Constants.gridSize * 4.) //Ensures a minimum width if the labels are very small
                let scaledH = max h (float Constants.gridSize*2.)
                {sym with
                    PortMaps = portMaps
                    Component = {sym.Component with H= float scaledH; W = float scaledW; X = sym.Pos.X; Y=sym.Pos.Y}}

        | _ -> 
            let comp = sym.Component
            {sym with Component = {comp with X = sym.Pos.X; Y = sym.Pos.Y}}
        |> calcLabelBoundingBox

/// return (num inputs, num outputs, height, width)
let getComponentProperties (compType:ComponentType) (label: string)=
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let gS = float Constants.gridSize
    match compType with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Input _ ->
        failwithf "Legacy Input component types should never occur"
    | GateN (_, n) -> (n , 1, 1.5*gS * (float n)/2. , 1.5*gS)
    | MergeN (n, _) -> (n , 1, 1.5*gS * (float n)/2. , 2.0*gS)
    | Not -> ( 1 , 1, 1.0*gS ,  1.0*gS) 
    | Input1 _ -> ( 0 , 1, gS ,  2.*gS)                
    | ComponentType.Output (a) -> (  1 , 0, gS ,  2.*gS) 
    | ComponentType.Viewer a -> (  1 , 0, gS ,  gS) 
    | ComponentType.IOLabel  -> (  1 , 1, gS/2. ,  gS) 
    | ComponentType.NotConnected -> (  1, 0, gS , gS)
    | Decode4 ->( 2 , 4 , 8.*gS  , 3.*gS) 
    | Constant1 (a, b,_) | Constant(a, b) -> (  0 , 1, gS ,  2.*gS) 
    | MergeWires -> ( 2 , 1, 2.*gS ,  2.*gS) 
    | SplitWire (a) ->(  1 , 2 , 2.*gS ,  2.*gS) 
    | Mux2 -> ( 3  , 1, 3.*gS ,  2.*gS) 
    | Mux4 -> ( 5  , 1, 8.*gS ,  2.*gS)   
    | Mux8 -> ( 9  , 1, 16.*gS ,  2.*gS) 
    | Demux2 ->( 2  , 2, 3.*gS ,  2.*gS) 
    | Demux4 -> ( 2  , 4, 8. * gS ,  2.*gS) 
    | Demux8 -> ( 2  , 8, 16.*gS ,  2.*gS) 
    | BusSelection (a, b) -> (  1 , 1, gS/2.,  2.*gS) 
    | BusCompare _ | BusCompare1 _ -> ( 1 , 1, gS ,  2.*gS) 
    | DFF -> (  1 , 1, 2.5*gS, 2.5*gS) 
    | DFFE -> ( 2  , 1, 2.5*gS  , 2.5*gS) 
    | Register (a) -> ( 1 , 1, 2.*gS, 4.*gS )
    | RegisterE (a) -> ( 2 , 1, 2.*gS  , 4.*gS)
    | Counter (a) -> (3 , 1 , 4.*gS , 5.*gS)
    |CounterNoEnable (a) -> (2 , 1 , 3.*gS , 5.*gS)
    | CounterNoLoad (a) -> (1 , 1 , 2.*gS , 5.*gS)
    |CounterNoEnableLoad (a) -> (0 , 1 , 2.*gS , 3.5*gS)
    | AsyncROM1 (a)  -> (  1 , 1, 4.*gS  , 5.*gS) 
    | ROM1 (a) -> (   1 , 1, 4.*gS  , 5.*gS) 
    | RAM1 (a) | AsyncRAM1 a -> ( 3 , 1, 4.*gS  , 5.*gS) 
    | NbitsXor (n, _) | NbitsOr (n) |NbitsAnd (n) -> (  2 , 1, 4.*gS  , 4.*gS) 
    | NbitsNot (n)  -> (  1 , 1, 3.*gS  , 4.*gS) 
    | NbitSpreader (n) -> (1, 1, 2.*gS, 2.*gS)
    | NbitsAdder (n) -> (  3 , 2, 3.*gS  , 4.*gS)
    |NbitsAdderNoCin (n) -> (  2 , 2, 3.*gS  , 4.*gS)
    | NbitsAdderNoCout (n)-> (  3 , 1, 3.*gS  , 4.*gS)
    | NbitsAdderNoCinCout (n) -> (  2 , 1, 3.*gS  , 4.*gS)
    | Shift _ -> (  2 , 1, 3.*gS  , 4.*gS)
    | Custom cct -> cct.InputLabels.Length, cct.OutputLabels.Length, 0., 0.

/// make a completely new component
let makeComponent (pos: XYPos) (compType: ComponentType) (id:string) (label:string) : Component =
    let defaultSTransform = {Rotation = Degree0; flipped = false}
    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent' (n, nout, h, w) label : Component=
        let inputPorts = portLists n id PortType.Input
        let outputPorts = portLists nout id PortType.Output
        {
            Id = id 
            Type = compType
            Label = label 
            InputPorts = inputPorts
            OutputPorts  = outputPorts
            X  = pos.X - float w / 2.0
            Y = pos.Y - float h / 2.0
            H = float h 
            W = float w
            SymbolInfo = Some { 
                LabelBoundingBox = None
                LabelRotation = None
                STransform=defaultSTransform; 
                ReversedInputPorts=None
                PortOrder = Map.empty; 
                PortOrientation=Map.empty
                HScale = None
                VScale = None}
        }
    let props = getComponentProperties compType label           
    makeComponent' props label



/// Function to generate a new symbol
let createNewSymbol (ldcs: LoadedComponent list) (pos: XYPos) (comptype: ComponentType) (label:string) (theme:ThemeType) =
    let id = JSHelpers.uuid ()
    let style = Constants.componentLabelStyle
    let comp = makeComponent pos comptype id label
    let transform = {Rotation= Degree0; flipped= false}

    { 
      Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
      CentrePos = {X = 0.; Y = 0.}
      OffsetFromBBCentre = {X = 0.; Y = 0}
      LabelBoundingBox = {TopLeft=pos; W=0.;H=0.} // dummy, will be replaced
      LabelHasDefaultPos = true
      LabelRotation = None
      Annotation=None
      Appearance =
          {
            HighlightLabel = false
            ShowPorts = ShowNone
            ShowCorners = DontShow // HLP23 AUTHOR: BRYAN TAN
            Colour = getSymbolColour comptype (isClocked [] ldcs comp) theme
            Opacity = 1.0
          }
      InWidth0 = None // set by BusWire
      InWidth1 = None
      InWidths = None
      Id = ComponentId id
      Component = comp
      Moving = false
      PortMaps = initPortOrientation comp
      STransform = transform
      ReversedInputPorts = Some false
      MovingPort = None
      IsClocked = isClocked [] ldcs comp
      MovingPortTarget = None
      HScale = None
      VScale = None
    }
    |> autoScaleHAndW
    |> calcLabelBoundingBox

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
    | IsBinaryGate | Mux2 -> Constants.gatePortPosEdgeGap
    | _ -> Constants.portPosEdgeGap

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
    let sideOffset offset =
        match side with 
            | Top -> {X = 0.0; Y = offset}
            | Bottom -> {X = 0.0; Y = -offset}
            | Left -> {X = offset; Y = 0.0}
            | Right -> {X = -offset; Y = 0.0}
    let compType = sym.Component.Type
    if isMuxSel sym side then
        match compType with
        | Mux2 | Demux2 -> sideOffset 9.
        | Mux4 | Demux4 -> sideOffset 9.5
        | Mux8 | Demux8 -> sideOffset 9.5
        | _ -> {X=0.; Y= 0.}
    else    
        {X = 0.; Y = 0.}

    


///Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPortPos (sym: Symbol) (port: Port) : XYPos =
    //get ports on the same edge first
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let numberOnSide = List.length ports
    let index = ( List.findIndex (fun (p:string)  -> p = port.Id) ports )
    let index' = match sym.ReversedInputPorts with |Some true -> float (numberOnSide-1-index) | _ -> float(index)
    let gap = getPortPosEdgeGap sym.Component.Type 
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let baseOffset = getPortBaseOffset sym side  //offset of the side component is on
    let baseOffset' = baseOffset + getMuxSelOffset sym side
    let portDimension = float ports.Length - 1.0
    //printfn "symbol %A portDimension %f" sym.Component.Type portDimension
    let h,w = getRotatedHAndW sym
    match side with
    | Left ->
        let yOffset = float h * ( index' + gap )/(portDimension + 2.0*gap)
        baseOffset' + {X = 0.0; Y = yOffset }
    | Right -> 
        let yOffset = float h * (portDimension - index' + gap )/(portDimension + 2.0*gap)
        baseOffset' + {X = 0.0; Y = yOffset }
    | Bottom -> 
        let xOffset = float  w * (index' + topBottomGap)/(portDimension + 2.0*topBottomGap)
        baseOffset' + {X = xOffset; Y = 0.0 }
    | Top ->
        let xOffset = float w * (portDimension - index' + topBottomGap)/(portDimension + 2.0*topBottomGap)
        baseOffset' + {X = xOffset; Y = 0.0 }

/// Gives the port positions to the render function, it gives the moving port pos where the mouse is, if there is a moving port
let inline getPortPosToRender (sym: Symbol) (port: Port) : XYPos =
    match sym.MovingPort with
    | Some movingPort when port.Id = movingPort.PortId -> movingPort.CurrPos - sym.Pos
    | _ -> 
        //printfn "symbol %A portDimension %A" sym.Component.Type (getPortPos sym port)
        getPortPos sym port

let inline getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

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
 
