(*
  This module coordinates the draw block user interface managing component selection, moving, auto-scrolling etc
*)

module Sheet
open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open DrawHelpers
open DrawModelType
open DrawModelType.SheetT
open Optics
open Operators


/// Keep track of HTML "Canvas" element used by Draw Blcok to read and write HTML scroll info.
/// Set in view function from react hook.
let mutable canvasDiv:Types.Element option = None

//-------------------------------------------------------------------------------------------------//
//-----------------------------------Constants used in Sheet---------------------------------------//
//-------------------------------------------------------------------------------------------------//

module Constants =
    let symbolSnapLimit = 7. // how large is the snapping when moving symbols
    let segmentSnapLimit = 7. // how large is the snapping when moving segments
    let gridSize = 30.0 // Size of each grid square
    let defaultCanvasSize = 3500. // total size of canvas (determines how far you can zoom out)
    let wireBoundingBoxSize = 2. // increase to make it easier to select wire segments
    let maxMagnification = 2. // max zoom beyond which it is not worth going
    let minMagnification = 0.1 // how much is it possible to zoom out? this is related to the actual canvas size
    let zoomIncrement = 1.2 // factor by which zoom is increased or decreased
    let boxAspectRatio = 2. // aspect ratio required before align or distribute can be done
    /// geometry parameters for sizing circuits
    let boxParameters = {|
            BoxOfEmptyCircuit = ({X=100.;Y=100.}:XYPos)
            BoxMin = 30.; // minimum white edge in pixels after ctrlW
            BoxMarginFraction = 0.1;  // minimum white edge as fraction of screen after ctrlW
            CanvasBorder = 0.5 // minimum scrollable white space border as fraction of circuit size after ctrlW
            CanvasExtensionFraction = 0.1 // fraction of screen size used to extend canvas by when going off edge
        |}
    

//---------------------------------------Derived constants----------------------------------------//


//-------------------------------------------------------------------------------------------------//
// ------------------ Helper Functions that need to be before the Model type extensions ---------- //
//-------------------------------------------------------------------------------------------------//

let centreOfCanvas (model:Model) =
    let dim = model.CanvasSize / 2.
    {X=dim; Y=dim}

/// Creates a command to Symbol
let symbolCmd (msg: SymbolT.Msg) = Cmd.ofMsg (Wire (BusWireT.Symbol msg))

/// Creates a command to BusWire
let wireCmd (msg: BusWireT.Msg) = Cmd.ofMsg (Wire msg)




//-------------------------------------------------------------------------------------------------//
// ------------------------------------- Issie Interfacing functions ----------------------------- //
//-------------------------------------------------------------------------------------------------//

module SheetInterface =
    type Model with
        /// Given a compType, return a label
        member this.GenerateLabel (compType: ComponentType) : string =
            SymbolUpdate.generateLabel this.Wire.Symbol compType

        /// Given a compId, return the corresponding component
        member this.GetComponentById (compId: ComponentId) =
            SymbolUpdate.extractComponent this.Wire.Symbol compId

        /// Change the label of Component specified by compId to lbl
        member this.ChangeLabel (dispatch: Dispatch<Msg>) (compId: ComponentId) (lbl: string) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeLabel (compId, lbl) ) ) )

        /// Run Bus Width Inference check
        member this.DoBusWidthInference dispatch =
            dispatch <| (Wire (BusWireT.BusWidths))

        /// Given a compId and a width, update the width of the Component specified by compId
        member this.ChangeWidth (dispatch: Dispatch<Msg>) (compId: ComponentId) (width: int) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeNumberOfBits (compId, width) ) ) )
            this.DoBusWidthInference dispatch

        /// Given a compId and a width, update the width of the Component specified by compId
        member this.ChangeScale (dispatch: Dispatch<Msg>) (compId: ComponentId) (newScale: float) (whichScale:ScaleAdjustment) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeScale (compId, newScale, whichScale) ) ) )
            dispatch <| (Wire (BusWireT.UpdateSymbolWires compId))

        member this.ChangeAdderComp (dispatch: Dispatch<Msg>) (compId: ComponentId) (newComp:ComponentType) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeAdderComponent (compId,(this.GetComponentById compId), newComp) ) ) )
            let delPorts = SymbolUpdatePortHelpers.findDeletedPorts this.Wire.Symbol compId (this.GetComponentById compId) newComp
            dispatch <| (Wire (BusWireT.DeleteWiresOnPort delPorts))
            dispatch <| (Wire (BusWireT.UpdateSymbolWires compId))
            //this.DoBusWidthInference dispatch
        
        member this.ChangeCounterComp (dispatch: Dispatch<Msg>) (compId: ComponentId) (newComp:ComponentType) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeCounterComponent (compId,(this.GetComponentById compId), newComp) ) ) )
            let delPorts = SymbolUpdatePortHelpers.findDeletedPorts this.Wire.Symbol compId (this.GetComponentById compId) newComp
            dispatch <| (Wire (BusWireT.DeleteWiresOnPort delPorts))
            dispatch <| (Wire (BusWireT.UpdateSymbolWires compId))
        
        /// Given a compId, update the ReversedInputs property of the Component specified by compId
        member this.ChangeReversedInputs (dispatch: Dispatch<Msg>) (compId: ComponentId) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeReversedInputs (compId) ) ) )
            dispatch <| (Wire (BusWireT.UpdateSymbolWires compId))
            this.DoBusWidthInference dispatch

        /// Given a compId and a LSB, update the LSB of the Component specified by compId
        member this.ChangeLSB (dispatch: Dispatch<Msg>) (compId: ComponentId) (lsb: int64) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeLsb (compId, lsb) ) ) )

        member this.ChangeInputValue (dispatch: Dispatch<Msg>) (compId: ComponentId) (newVal: int) =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.ChangeInputValue (compId, newVal))))

        /// Return Some string if Sheet / BusWire / Symbol has a notification, if there is none then return None
        member this.GetNotifications =
            // Currently only BusWire has notifications
            this.Wire.Notifications

        /// Get the current canvas state in the form of (Component list * Connection list)
        member this.GetCanvasState () =
            let compList = SymbolUpdate.extractComponents this.Wire.Symbol
            let connList = BusWire.extractConnections this.Wire

            compList, connList

        /// Clears the Undo and Redo stack of Sheet
        member this.FlushCommandStack dispatch =
            dispatch <| FlushCommandStack

        /// Clears the canvas, removes all components and connections
        member this.ClearCanvas dispatch =
            dispatch <| ResetModel
            dispatch <| Wire BusWireT.ResetModel
            dispatch <| Wire (BusWireT.Symbol (SymbolT.ResetModel ) ) 

        /// Returns a list of selected components
        member this.GetSelectedComponents =
            this.SelectedComponents
            |> List.collect ( fun compId ->
                if Map.containsKey compId this.Wire.Symbol.Symbols then
                    [SymbolUpdate.extractComponent this.Wire.Symbol compId]
                else
                    [])
        
        /// Returns a list of selected connections
        member this.GetSelectedConnections =
            this.SelectedWires
            |> List.collect (fun connId ->
                if Map.containsKey connId this.Wire.Wires then
                    [BusWire.extractConnection this.Wire connId]
                else 
                    [])
        
        /// Returns a list of selected components and connections in the form of (Component list * Connection list)
        member this.GetSelectedCanvasState =
            this.GetSelectedComponents, this.GetSelectedConnections

        /// Given a list of connIds, select those connections
        member this.SelectConnections dispatch on connIds =
            dispatch <| UpdateSelectedWires (connIds, on)
        /// Update the memory of component
        member this.WriteMemoryType dispatch compId mem =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.WriteMemoryType (compId,mem))))
                /// Update the memory of component
        member this.UpdateMemory dispatch compId mem =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.UpdateMemory (compId,mem))))

        /// Update the memory of component specified by connId at location addr with data value
        member this.WriteMemoryLine dispatch connId addr value =
            dispatch <| (Wire (BusWireT.Symbol (SymbolT.WriteMemoryLine (connId, addr, value))))

//-------------------------------------------------------------------------------------------------//
// ------------------------------------------- Helper Functions ------------------------------------------- //
//-------------------------------------------------------------------------------------------------//


  

//Calculates the symmetric difference of two lists, returning a list of the given type
let symDiff lst1 lst2 =
    let a = Set.ofList lst1
    let b = Set.ofList lst2
    (a - b) + (b - a)
    |> Set.toList

/// return screen edge coords
let getScreenEdgeCoords (model:Model) =
    let canvas = document.getElementById "Canvas"
    let wholeApp = document.getElementById "WholeApp"
    let rightSelection = document.getElementById "RightSelection"
    let topMenu = document.getElementById "TopMenu"
    let scrollDeviation = model.ScreenScrollPos - {X=canvas.scrollLeft;Y=canvas.scrollTop}
    let leftScreenEdge = canvas.scrollLeft
    let rightScreenEdge = leftScreenEdge + wholeApp.clientWidth - rightSelection.offsetWidth
    let topScreenEdge = canvas.scrollTop
    let bottomScreenEdge = topScreenEdge + rightSelection.offsetHeight - topMenu.clientHeight
    {|Left=leftScreenEdge;Right=rightScreenEdge;Top=topScreenEdge;Bottom=bottomScreenEdge|}

let centreOfScreen model : XYPos =
    let edge = getScreenEdgeCoords model
    {
        X = (edge.Left + edge.Right)/(2. * model.Zoom)
        Y = (edge.Top + edge.Bottom)/(2. * model.Zoom)
    }

/// helper used inside Map.tryFind hence the unused parameter
/// returns true if pos is insoie boundingbox
let insideBox (pos: XYPos) boundingBox =
    let {BoundingBox.TopLeft={X = xBox; Y=yBox}; H=hBox; W=wBox} = boundingBox
    pos.X >= xBox && pos.X <= xBox + wBox && pos.Y >= yBox && pos.Y <= yBox + hBox

/// Checks if pos is inside any of the bounding boxes of the components in boundingBoxes
let inline insideBoxMap 
        (boundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>) 
        (pos: XYPos) 
            : CommonTypes.ComponentId Option =
    boundingBoxes
    |> Map.tryFindKey (fun k box -> insideBox pos box)// If there are multiple components overlapping (should not happen), return first one found

/// returns the symbol if pos is inside any symbol's LabelBoundingBox
let inline tryInsideLabelBox  (model: Model) (pos: XYPos) =
    Optic.get symbols_ model
    |> Map.tryPick (fun (sId:ComponentId) (sym:SymbolT.Symbol) ->
        if insideBox pos sym.LabelBoundingBox then Some sym else None)
    

/// return a BB equivalent to input but with (X,Y) = LH Top coord, (X+W,Y+H) = RH bottom coord
/// note that LH Top is lower end of the two screen coordinates
let standardiseBox (box:BoundingBox) =
    let x = min box.TopLeft.X (box.TopLeft.X+box.W)
    let y = min box.TopLeft.Y (box.TopLeft.Y+box.H)
    let w = abs box.W
    let h = abs box.H
    { TopLeft = {X=x; Y=y} ; W=w;H=h}


let transformScreenToPos (screenPos:XYPos) (scrollPos:XYPos) mag =
    {X=(screenPos.X + scrollPos.X)/mag;
     Y=(screenPos.Y + scrollPos.Y)/mag}


/// calculates the smallest bounding box that contains two BBs, in form with W,H > 0
let boxUnion (box:BoundingBox) (box':BoundingBox) =
    let maxX = max (box.TopLeft.X+box.W) (box'.TopLeft.X + box'.W)
    let maxY = max (box.TopLeft.Y + box.H) (box'.TopLeft.Y + box'.H)
    let minX = min box.TopLeft.X box'.TopLeft.X
    let minY = min box.TopLeft.Y box'.TopLeft.Y
    {
        TopLeft = {X = minX; Y = minY}
        W = maxX - minX
        H = maxY - minY
    }

/// Returns the smallest BB containing box and point.
/// Could be made more efficient
let boxPointUnion (box: BoundingBox) (point: XYPos) =
    let pBox = {TopLeft=point; W=0.;H=0.}
    boxUnion box pBox
    
let symbolToBB (centresOnly: bool) (symbol:SymbolT.Symbol) =
    let co = symbol.Component
    let h,w = Symbol.getRotatedHAndW symbol
    if centresOnly then
        {TopLeft = symbol.Pos + {X=w/2.; Y=h/2.}; W=0.;H=0.}
    else
        {TopLeft = symbol.Pos; W=w; H=h}

let symbolToCentre (symbol:SymbolT.Symbol) =
    let h,w = Symbol.getRotatedHAndW symbol
    {TopLeft = symbol.Pos + {X=w/2.; Y = h / 2.}; W=0.1; H=0.1}

/// Returns the smallest BB that contains all segments
/// of wire.
let wireToBB (wire:BusWireT.Wire) =
    let initBox = {TopLeft=wire.StartPos;W=0.;H=0.}
    (initBox,wire)
    ||> BusWire.foldOverNonZeroSegs (fun _ ePos box _ -> 
        boxPointUnion box ePos)


let symbolBBUnion (centresOnly: bool) (symbols: SymbolT.Symbol list) :BoundingBox option =
    match symbols with
    | [] -> None
    | sym :: rest ->
        (symbolToBB centresOnly sym, rest)
        ||> List.fold (fun (box:BoundingBox) sym ->
                if centresOnly then
                    boxUnion (symbolToBB centresOnly sym) box
                else
                    boxUnion box (boxUnion (symbolToBB centresOnly sym) (sym.LabelBoundingBox)))
        |> Some
    


/// Returns the smallest BB that contains all symbols, labels, and wire segments.
/// For empty circuit a BB is returned in middle of viewable screen.
let symbolWireBBUnion (model:Model) =
    let symbols =
        model.Wire.Symbol.Symbols
        |> Helpers.mapValues
        |> Array.toList
    let symbolBB =
        symbols
        |> symbolBBUnion false
    let labelsBB =
        symbols
        |> List.map (fun sym -> (Symbol.calcLabelBoundingBox sym).LabelBoundingBox)
    let labelBB =
        match labelsBB with
        | [] -> None
        | _ -> Some <| List.reduce boxUnion labelsBB
    let wireBB =
        let wiresBBA = 
            model.Wire.Wires
            |> Helpers.mapValues
            |> Array.map wireToBB
        match wiresBBA with
        | [||] -> None
        |  _ ->
            wiresBBA
            |> Array.reduce boxUnion
            |> Some
    [symbolBB;labelBB;wireBB]
    |> List.collect (function | Some bb -> [bb] | _ -> [])
    |> function | [] -> {TopLeft=centreOfScreen model; W=0.;H=0.}
                | [bb] -> bb
                | bbL -> List.reduce boxUnion bbL

let moveCircuit moveDelta (model: Model) =
    model
    |> Optic.map symbol_ (Symbol.moveSymbols moveDelta)
    |> Optic.map wire_ (BusWire.moveWires moveDelta)
    |> Optic.map wire_ (BusWireUpdateHelpers.updateWireSegmentJumps [])

/// get scroll and zoom paras to fit box all on screen centred and occupying as much of screen as possible
let getWindowParasToFitBox model (box: BoundingBox)  =
    let edge = getScreenEdgeCoords model
    let lh,rh,top,bottom = edge.Left,edge.Right,edge.Top,edge.Bottom
    let wantedMag = min ((rh - lh)/box.W) ((bottom-top)/box.H)
    let magToUse = min wantedMag Constants.maxMagnification
    let xMiddle = (box.TopLeft.X + box.W/2.)*magToUse
    let xScroll = xMiddle - (rh-lh)/2.
    let yMiddle = (box.TopLeft.Y + (box.H)/2.)*magToUse
    let yScroll = yMiddle - (bottom-top)/2.
    {|Scroll={X=xScroll; Y=yScroll}; MagToUse=magToUse|}

let addBoxMargin (fractionalMargin:float) (absoluteMargin:float) (box: BoundingBox) =
    let boxMargin = 
        (max box.W box.H) * fractionalMargin
        |> max absoluteMargin 
       
    {box with
        TopLeft = box.TopLeft - {X = boxMargin; Y = boxMargin}
        W = box.W + boxMargin*2.
        H = box.H + boxMargin*2.
     }

/// Check that canvas is large enough to have space all round the visible area.
/// If not, then change model by moving circuit on canvas and/or extending canvas.
/// Keep components in same visible position during this process.
/// returns new model with all positions updated if need be.
let ensureCanvasExtendsBeyondScreen model : Model =
    let boxParas = Constants.boxParameters
    let edge = getScreenEdgeCoords model
    let box = 
        symbolWireBBUnion model
        |> addBoxMargin boxParas.CanvasExtensionFraction  boxParas.BoxMin
    let quant = boxParas.CanvasExtensionFraction * min box.H box.W       
    let newSize =
        [box.H;box.W]
        |> List.map (fun x -> x + 4.*quant)
        |> List.max
        |> max model.CanvasSize
    let bottomRight = box.TopLeft + {X=box.W;Y=box.H}
    let size = model.CanvasSize
    let xIsOk = box.TopLeft.X > 0. && bottomRight.X < size
    let yIsOk = box.TopLeft.Y > 0. &&  bottomRight.Y < size
    if xIsOk && yIsOk then
        model
    else
        let circuitMove = 
            box
            |> (fun bb -> 
                let centre = bb.Centre()
                {
                    X = if xIsOk then 0. else newSize/2.- centre.X
                    Y = if yIsOk then 0. else newSize/2. - centre.Y
                })

        match canvasDiv, model.ScreenScrollPos + circuitMove*model.Zoom with
        | Some el, pos ->
            el.scrollLeft <- pos.X
            el.scrollTop <- pos.Y
        | None,_-> ()
        let posDelta :(XYPos -> XYPos) = ((+) circuitMove)
        let posScreenDelta :(XYPos -> XYPos) = ((+) (circuitMove*model.Zoom))
        model 
        |> moveCircuit circuitMove
        |> Optic.map screenScrollPos_ posDelta 
        |> Optic.set canvasSize_ newSize
        |> Optic.map screenScrollPos_ posScreenDelta
        |> Optic.map lastMousePos_ posDelta
        |> Optic.map lastMousePosForSnap_ posDelta
        |> Optic.map (scrollingLastMousePos_ >-> pos_) posDelta
        
           







/// shift circuit to middle of canvas, resizing canvas to allow enough border if needed.
/// return scroll and zoom paras to display all of circuit in middle of window
let fitCircuitToWindowParas (model:Model) =
    let boxParas = Constants.boxParameters

    let minBox = {TopLeft = {X=100.; Y=100.}; W=100.; H=100.}
    let sBox = 
        symbolWireBBUnion model
        |> addBoxMargin boxParas.BoxMarginFraction boxParas.BoxMin
    let newCanvasSize = 
        max sBox.W sBox.H
        |> ((*) (1. + 2. * boxParas.CanvasBorder))
        |> max Constants.defaultCanvasSize
    let offsetToCentreCircuit =
        {X=newCanvasSize / 2.; Y = newCanvasSize/2.} - sBox.Centre()
    let modelWithMovedCircuit =
        {model with CanvasSize = newCanvasSize}
        |> moveCircuit offsetToCentreCircuit

    let sBox = {sBox with TopLeft = sBox.TopLeft + offsetToCentreCircuit} 
    let paras = getWindowParasToFitBox model sBox
    {modelWithMovedCircuit with
        Zoom = paras.MagToUse
        ScreenScrollPos = paras.Scroll}, paras



let isBBoxAllVisible model (bb: BoundingBox) =
    let edge = getScreenEdgeCoords model
    let z = model.Zoom
    let lh,rh,top,bottom = edge.Left/z,edge.Right/z,edge.Top/z,edge.Bottom/z
    let bbs = standardiseBox bb
    lh < bb.TopLeft.Y && 
    top < bb.TopLeft.X && 
    bb.TopLeft.Y+bb.H < bottom && 
    bb.TopLeft.X+bb.W < rh

/// could be made more efficient, since segments contain redundant info
let getWireBBox (wire: BusWireT.Wire) =
    let updateBoundingBox segStart (segEnd: XYPos) state seg =
        let newTop = min state.TopLeft.Y segEnd.Y
        let newBottom = max (state.TopLeft.Y+state.H) segEnd.Y
        let newRight = max (state.TopLeft.X+state.W) segEnd.X
        let newLeft = min state.TopLeft.X segEnd.X
        {TopLeft={X=newTop; Y=newLeft}; W=newRight-newLeft; H=newBottom-newTop }
    BusWire.foldOverSegs updateBoundingBox {TopLeft = wire.StartPos; W=0; H=0;} wire
    

let isAllVisible (model: Model)(conns: ConnectionId list) (comps: ComponentId list) =
    let wVisible =
        conns
        |> List.map (fun cid -> Map.tryFind cid model.Wire.Wires)
        |> List.map (Option.map (fun wire -> getWireBBox wire))
        |> List.map (Option.map (isBBoxAllVisible model))
        |> List.map (Option.defaultValue true)
        |> List.fold (&&) true
    let cVisible =
        comps
        |> List.collect (fun comp ->
            if Map.containsKey comp model.Wire.Symbol.Symbols then 
                [Symbol.getBoundingBox model.Wire.Symbol comp]
            else
                [])
        |> List.map (isBBoxAllVisible model)
        |> List.fold (&&) true
    wVisible && cVisible

/// Calculates if two bounding boxes intersect by comparing corner coordinates of each box
let boxesIntersect (box1: BoundingBox) (box2: BoundingBox) =
    // Requires min and max since H & W can be negative, i.e. we don't know which corner is which automatically
    // Boxes intersect if there is overlap in both x and y coordinates 
    min box1.TopLeft.X (box1.TopLeft.X + box1.W) < max box2.TopLeft.X (box2.TopLeft.X + box2.W)
    && min box2.TopLeft.X (box2.TopLeft.X + box2.W) < max box1.TopLeft.X (box1.TopLeft.X + box1.W)
    && min box1.TopLeft.Y (box1.TopLeft.Y + box1.H) < max box2.TopLeft.Y (box2.TopLeft.Y + box2.H)
    && min box2.TopLeft.Y (box2.TopLeft.Y + box2.H) < max box1.TopLeft.Y (box1.TopLeft.Y + box1.H)
    
/// Finds all components that touch a bounding box (which is usually the drag-to-select box)
let findIntersectingComponents (model: Model) (box1: BoundingBox) =
    model.BoundingBoxes
    |> Map.filter (fun _ boundingBox -> boxesIntersect boundingBox box1)
    |> Map.toList
    |> List.map fst

let posAdd (pos : XYPos) (a : float, b : float) : XYPos =
    {X = pos.X + a; Y = pos.Y + b}

/// Finds all components (that are stored in the Sheet model) near pos
let findNearbyComponents (model: Model) (pos: XYPos) (range: float)  =
    // Larger Increments -> More Efficient. But can miss small components then.
    List.allPairs [-range .. 10.0 .. range] [-range .. 10.0 .. range] 
    |> List.map ((fun x -> posAdd pos x) >> insideBoxMap model.BoundingBoxes)
    |> List.collect ((function | Some x -> [x] | _ -> []))

/// Checks if pos is inside any of the ports in portList
let mouseOnPort portList (pos: XYPos) (margin: float) =
    let radius = 5.0

    let insidePortCircle (pos: XYPos) (portLocation: XYPos): bool =
        let distance = ((pos.X - portLocation.X) ** 2.0 + (pos.Y - portLocation.Y) ** 2.0) ** 0.5
        distance <= radius + margin

    // + 2.5 margin to make it a bit easier to click on, maybe it's due to the stroke width?
    match List.tryFind (fun (_, portLocation) -> insidePortCircle pos portLocation) portList with 
    | Some (portId, portLocation) -> Some (portId, portLocation)
    | None -> None

/// Returns the ports of all model.NearbyComponents
let findNearbyPorts (model: Model) =
    let inputPortsMap, outputPortsMap = Symbol.getPortLocations model.Wire.Symbol model.NearbyComponents

    (inputPortsMap, outputPortsMap) ||> (fun x y -> (Map.toList x), (Map.toList y))

/// Returns what is located at pos.
/// Priority Order: InputPort -> OutputPort -> Label -> Wire -> Component -> Canvas
let mouseOn (model: Model) (pos: XYPos) : MouseOn =

    let inputPorts, outputPorts = findNearbyPorts model
    match mouseOnPort inputPorts pos 2.5 with
    | Some (portId, portLoc) -> InputPort (portId, portLoc)
    | None ->
        match mouseOnPort outputPorts pos 2.5 with
        | Some (portId, portLoc) -> OutputPort (portId, portLoc)
        | None ->
            match tryInsideLabelBox model pos with
            | Some sym -> 
                Label sym.Id
            | None ->
                match BusWireUpdate.getClickedWire model.Wire pos (Constants.wireBoundingBoxSize/model.Zoom) with
                | Some connId -> Connection connId
                | None ->
                    match insideBoxMap model.BoundingBoxes pos with
                    | Some compId -> Component compId
                    | None -> Canvas


let notIntersectingComponents (model: Model) (box1: BoundingBox) (inputId: CommonTypes.ComponentId) =
   model.BoundingBoxes
   |> Map.filter (fun sId boundingBox -> boxesIntersect boundingBox box1 && inputId <> sId)
   |> Map.isEmpty


