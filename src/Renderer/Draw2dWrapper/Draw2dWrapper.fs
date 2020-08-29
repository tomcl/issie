(*
    f# wrapper for draw_lib library.
*)

module Draw2dWrapper

open CommonTypes
open JSTypes
open DiagramMessageType
open JSHelpers
//open DiagramStyle

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

// Interface with JS library.

type private IDraw2d =
    abstract setDispatchMessages :
        dispatchInferWidthsMessage_         :(unit->unit) ->
        dispatchOnSelectComponentMessage_   :(JSComponent->unit) ->
        dispatchOnUnselectComponentMessage_ :(unit->unit) ->
        dispatchHasUnsavedChangesMessage_   :(bool->unit) ->
        unit
    abstract printCanvas                  : canvas: JSCanvas -> handler: (string -> string -> unit) -> unit
    abstract createCanvas                 : id:string -> width:int -> height:int -> JSCanvas
    abstract initialiseCanvas             : canvas:JSCanvas -> unit
    abstract clearCanvas                  : canvas:JSCanvas -> unit
    abstract addComponentToCanvas         : canvas:JSCanvas -> comp:JSComponent -> unit
    abstract addConnectionToCanvas        : canvas:JSCanvas -> conn:JSConnection -> unit
    abstract addComponentLabel            : comp:JSComponent -> label:string ->  unit
    abstract setComponentLabel            : comp:JSComponent -> newLabel:string ->  unit
    abstract setConnectionLabel           : comp:JSConnection -> newLabel:string ->  unit
    abstract setComponentId               : comp:JSComponent -> id:string -> unit
    abstract setConnectionId              : conn:JSConnection -> id:string -> unit
    abstract setPortId                    : port:JSPort -> id:string -> unit
    abstract setComponentBackground       : comp:JSComponent -> string -> unit
    abstract setConnectionColor           : conn:JSConnection -> string -> unit
    abstract setConnectionStroke          : conn:JSConnection -> int -> unit
    abstract getConnectionVertices        : conn:JSConnection -> JSVertices
    abstract setConnectionVertices        : conn:JSConnection -> JSVertices -> unit
    abstract getInputPorts                : comp:JSComponent -> JSPorts
    abstract getOutputPorts               : comp:JSComponent -> JSPorts
    abstract installSelectionPolicy       : comp:JSComponent -> unit
    abstract createDigitalInput           : x:int -> y:int -> numberOfBits:int -> JSComponent
    abstract createDigitalConstant        : x:int -> y:int -> numberOfBits:int -> constValue: int -> JSComponent
    abstract createDigitalOutput          : x:int -> y:int -> numberOfBits:int -> JSComponent
    abstract createDigitalLabel           : x:int -> y:int -> JSComponent
    abstract createDigitalBusSelection    : x : int -> y: int -> numberOfBits: int -> bitSelected: int -> JSComponent
    abstract createDigitalNot             : x:int -> y:int -> JSComponent
    abstract createDigitalAnd             : x:int -> y:int -> JSComponent
    abstract createDigitalOr              : x:int -> y:int -> JSComponent
    abstract createDigitalXor             : x:int -> y:int -> JSComponent
    abstract createDigitalNand            : x:int -> y:int -> JSComponent
    abstract createDigitalNor             : x:int -> y:int -> JSComponent
    abstract createDigitalXnor            : x:int -> y:int -> JSComponent
    abstract createDigitalMux2            : x:int -> y:int -> JSComponent
    abstract createDigitalDemux2          : x:int -> y:int -> JSComponent
    abstract createDigitalNbitsAdder      : x:int -> y:int -> numberOfBits:int -> JSComponent
    abstract createDigitalDecode4         : x:int -> y:int -> JSComponent
    abstract createDigitalCustom          : x:int -> y:int -> name:string -> inputs:obj -> outputs:obj -> JSComponent
    abstract createDigitalMergeWires      : x:int -> y:int -> JSComponent
    abstract createDigitalSplitWire       : x:int -> y:int -> topOutputWidth:int -> JSComponent
    abstract createDigitalDFF             : x:int -> y:int -> JSComponent
    abstract createDigitalDFFE            : x:int -> y:int -> JSComponent
    abstract createDigitalRegister        : x:int -> y:int -> regWidth:int -> JSComponent
    abstract createDigitalRegisterE       : x:int -> y:int -> regWidth:int -> JSComponent
    abstract createDigitalAsyncROM        : x:int -> y:int -> addressWidth:int -> wordWidth:int -> memData:'jsInt64List -> JSComponent
    abstract createDigitalROM             : x:int -> y:int -> addressWidth:int -> wordWidth:int -> memData:'jsInt64List -> JSComponent
    abstract createDigitalRAM             : x:int -> y:int -> addressWidth:int -> wordWidth:int -> memData:'jsInt64List -> JSComponent
    abstract createDigitalConnection      : source:JSPort -> target:JSPort -> JSConnection
    abstract writeMemoryLine              : comp:JSComponent -> addr:int -> value:int64 -> unit
    abstract setNumberOfBits              : comp:JSComponent -> numberOfBits:int -> unit
    abstract setLsbBitNumber              : comp:JSComponent -> lsbBitNumber:int -> unit
    abstract setConstantNumber            : comp:JSComponent -> constValue:int -> unit
    abstract setTopOutputWidth            : comp:JSComponent -> topOutputWidth: int -> unit
    abstract setRegisterWidth             : comp:JSComponent -> topOutputWidth: int -> unit
    abstract updateMergeWiresLabels       : comp:JSComponent -> topInputWidth:int option -> bottomInputWidth:int option -> outputWidth:int option -> unit
    abstract updateSplitWireLabels        : comp:JSComponent -> inputWidth:int option ->topOutputWidth:int option ->bottomOutputWidth:int option -> unit
    abstract getComponentById             : canvas:JSCanvas -> id:string -> JSComponent
    abstract getConnectionById            : canvas:JSCanvas -> id:string -> JSConnection
    abstract getPortById                  : comp:JSComponent -> id:string -> JSPort
    abstract getAllJsComponents           : canvas:JSCanvas -> JSComponents
    abstract getAllJsConnections          : canvas:JSCanvas -> JSConnections
    abstract getSelectedJsComponents      : canvas:JSCanvas -> JSComponents
    abstract getSelectedJsConnections     : canvas:JSCanvas -> JSConnections
    abstract undoLastAction               : canvas:JSCanvas -> unit
    abstract redoLastAction               : canvas:JSCanvas -> unit
    abstract flushCommandStack            : canvas:JSCanvas -> unit
    abstract getScrollArea                : canvas: JSCanvas -> ResizeArray<int>
    abstract getZoom                      : canvas: JSCanvas -> float
    abstract setScrollZoom                : canvas: JSCanvas -> scrollLeft: int -> scrollTop:int -> zoom: float -> unit
    abstract resetSelection               : canvas: JSCanvas -> unit
    abstract addCompSelection             : canvas: JSCanvas -> comp: JSComponent -> unit
    abstract addConnSelection             : canvas: JSCanvas -> conn: JSConnection -> unit

[<Import("*", "./draw2d_fsharp_interface.js")>]
let private draw2dLib : IDraw2d = jsNative

// Helpers.



let private createAndInitialiseCanvas (id : string) : JSCanvas =
    let canvas = draw2dLib.createCanvas id CommonTypes.draw2dCanvasWidth CommonTypes.draw2dCanvasHeight
    draw2dLib.initialiseCanvas canvas
    canvas



let private setPorts (ports : Port list) (jsPorts : JSPorts) : unit =
    let jsPortsLen : int = getFailIfNull jsPorts ["length"]
    if jsPortsLen <> ports.Length then failwithf "what? setPort called with mismatching number of ports"
    [0..ports.Length - 1] |> List.map (fun i ->
        let jsPort : JSPort = jsPorts?(i)
        let port : Port = ports.[i]
        draw2dLib.setPortId jsPort port.Id
    ) |> ignore

let private createComponent
        (canvas : JSCanvas)
        (dispatch : JSDiagramMsg -> unit)
        (maybeId : string option) // Passing None will let the library decide it.
        (componentType : ComponentType)
        (label : string)
        (maybeInputPorts : (Port list) option) // Passing None will initialise new ports.
        (maybeOutputPorts : (Port list) option) // Passing None will initialise new ports.
        (x : int)
        (y : int)
        : JSComponent =
    let comp =
        match componentType with
        | Input w  -> draw2dLib.createDigitalInput x y w
        | Output w -> draw2dLib.createDigitalOutput x y w
        | IOLabel -> draw2dLib.createDigitalLabel x y
        | BusSelection (w,lsb) -> draw2dLib.createDigitalBusSelection x y w lsb
        | Constant (w,c) -> draw2dLib.createDigitalConstant x y w c
        | Not    -> draw2dLib.createDigitalNot x y
        | And    -> draw2dLib.createDigitalAnd x y
        | Or     -> draw2dLib.createDigitalOr x y
        | Xor    -> draw2dLib.createDigitalXor x y
        | Nand   -> draw2dLib.createDigitalNand x y
        | Nor    -> draw2dLib.createDigitalNor x y
        | Xnor   -> draw2dLib.createDigitalXnor x y
        | Decode4 -> draw2dLib.createDigitalDecode4 x y
        | Mux2   -> draw2dLib.createDigitalMux2 x y
        | Demux2 -> draw2dLib.createDigitalDemux2 x y
        | NbitsAdder numberOfBits -> 
            draw2dLib.createDigitalNbitsAdder x y numberOfBits
        | ComponentType.Custom custom ->
            draw2dLib.createDigitalCustom
                x y custom.Name (fshaprListToJsList custom.InputLabels)
                                (fshaprListToJsList custom.OutputLabels)
        | MergeWires -> draw2dLib.createDigitalMergeWires x y
        | SplitWire topWireWidth -> draw2dLib.createDigitalSplitWire x y topWireWidth
        | DFF  -> draw2dLib.createDigitalDFF x y
        | DFFE -> draw2dLib.createDigitalDFFE x y
        | Register  width -> 
            draw2dLib.createDigitalRegister x y width
        | RegisterE width -> draw2dLib.createDigitalRegisterE x y width
        | AsyncROM mem ->
            draw2dLib.createDigitalAsyncROM
                x y mem.AddressWidth mem.WordWidth (fshaprListToJsList mem.Data)
        | ROM mem ->
            draw2dLib.createDigitalROM
                x y mem.AddressWidth mem.WordWidth (fshaprListToJsList mem.Data)
        | RAM mem ->
            draw2dLib.createDigitalRAM
                x y mem.AddressWidth mem.WordWidth (fshaprListToJsList mem.Data)
    // Every component is assumed to have a label (may be empty string).

    draw2dLib.addComponentLabel comp label
    
    // Set Id if one is provided.
    match maybeId with
    | None -> ()
    | Some id -> draw2dLib.setComponentId comp id
    // Set ports if provided.
    match maybeInputPorts, maybeOutputPorts with
    | None, None -> ()
    | Some ip, Some op ->
        setPorts ip (draw2dLib.getInputPorts comp)
        setPorts op (draw2dLib.getOutputPorts comp)
    | _ -> failwithf "what? createComponent called with incomplete of input/output ports"
    // Install the behaviour on selection.
    draw2dLib.installSelectionPolicy comp
    draw2dLib.addComponentToCanvas canvas comp
    comp

let private createConnection
        (canvas : JSCanvas)
        (maybeId : string option)
        (vertices : (float * float) list)
        (source : JSPort)
        (target : JSPort) =
    let conn : JSConnection = draw2dLib.createDigitalConnection source target
    match maybeId with
    | None -> ()
    | Some id -> draw2dLib.setConnectionId conn id
    vertices
    |> List.map (fun (x, y) -> createObj ["x" ==> x; "y" ==> y])
    |> fshaprListToJsList
    |> draw2dLib.setConnectionVertices conn
    draw2dLib.addConnectionToCanvas canvas conn

let private editComponentLabel (canvas : JSCanvas) (id : string) (newLabel : string) : unit =
    let jsComponent = draw2dLib.getComponentById canvas id
    if isNull jsComponent
    then failwithf "what? could not find diagram component with Id: %s" id
    else 
        draw2dLib.setComponentLabel jsComponent newLabel
        //jsComponent?children?data?(0)?figure?setText(newLabel)
    

// React wrapper.

/// Determines size of schematic.
/// ToDo - make this more flexible and expose sizes
type DisplayModeType = DispMode of HTMLAttr

type private Draw2dReactProps = {
    Dispatch : JSDiagramMsg -> unit
    CanvasDisplayMode : DisplayModeType
}

type private Draw2dReact(initialProps) =
    inherit PureStatelessComponent<Draw2dReactProps>(initialProps)

    let divId = "Draw2dCanvas"

    override this.componentDidMount() =
        log "Mounting Draw2dReact component"
        createAndInitialiseCanvas divId |> InitCanvas |> this.props.Dispatch

    override this.render() =
        let style = match this.props.CanvasDisplayMode with | DispMode s -> s    
        div [ Id divId; style ] []

let inline private createDraw2dReact props = ofType<Draw2dReact,_,_> props []

/// interface to ta draw2d component that controls the schematic sheet
type Draw2dWrapper() =
    let mutable canvas : JSCanvas option = None
    let mutable dispatch : (JSDiagramMsg -> unit) option = None

    /// Executes action applied to the current Draw2d canvas
    let tryActionWithCanvas name action =
        match canvas with
        | None -> log <| sprintf "Warning: Draw2dWrapper.%s called when canvas is None" name
        | Some c -> action c

    /// Returns a react element containing the canvas.
    /// The dispatch function has to be: JSDiagramMsg >> dispatch
    member this.CanvasReactElement jsDiagramMsgDispatch displayMode =
        let reload() =
            dispatch <- Some jsDiagramMsgDispatch
            draw2dLib.setDispatchMessages
                (InferWidths >> jsDiagramMsgDispatch)
                (SelectComponent >> jsDiagramMsgDispatch)
                (UnselectComponent >> jsDiagramMsgDispatch)
                (SetHasUnsavedChanges >> jsDiagramMsgDispatch)
        // Initialise dispatch if needed.
        match dispatch with
        | None -> reload()
        | Some _ -> // even if canvas has previously been installed, HMR can break this - so do it again if debugging
                    // this will unselect all components. TODO: use an HMR callback to do this, or make state persistent via react props
            if debugLevel <> 0 then
                reload()  
        // Return react element with relevant props.
        createDraw2dReact {
            Dispatch = jsDiagramMsgDispatch
            CanvasDisplayMode = displayMode
        }

    member this.InitCanvas newCanvas =
        match canvas with
        | None -> canvas <- Some newCanvas
        | Some _ -> canvas <- Some newCanvas

    member this.printCanvas (handler: (string -> string -> unit)) =
        printfn "printing canvas!"
        tryActionWithCanvas "PrintCanvas" (fun canvas -> draw2dLib.printCanvas canvas handler)
    
    member this.ClearCanvas () =
        tryActionWithCanvas "ClearCanvas" draw2dLib.clearCanvas

    member this.GetScrollArea () =
        match canvas with
        | None -> 
            None
        | Some c -> 
            let a = draw2dLib.getScrollArea c
            Some {| Width = a.[0]; Height = a.[1]; Left = a.[2]; Top = a.[3]|}

    member this.GetZoom () =
        match canvas with
        | None -> 
            None
        | Some c -> 
            draw2dLib.getZoom c |> Some

    member this.SetScrollZoom scrollLeft scrollTop zoom =
        match canvas with
        | None -> 
            ()
        | Some c -> 
            draw2dLib.setScrollZoom c scrollLeft scrollTop zoom

    /// Brand new component.
    member this.CreateComponent componentType label x y =
        match canvas, dispatch with
        | None, _ | _, None ->
            log "Warning: Draw2dWrapper.CreateComponent called when canvas or dispatch is None"
            None
        | Some c, Some d ->
            Some <| createComponent c d None componentType label None None x y

    /// Create a JS component from the passed component and add it to the canvas.
    member this.LoadComponent (comp : Component) =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.LoadComponent called when canvas or dispatch is None"
        | Some c, Some d -> ignore <| createComponent
                                c d (Some comp.Id) comp.Type comp.Label
                                (Some comp.InputPorts) (Some comp.OutputPorts)
                                comp.X comp.Y

    member this.LoadConnection (useId : bool) (conn : Connection) =
        fun c ->
            let sourceParentNode : JSComponent =
                assertNotNull (draw2dLib.getComponentById c conn.Source.HostId) "sourceParentNode"
            let sourcePort : JSPort =
                assertNotNull (draw2dLib.getPortById sourceParentNode conn.Source.Id) "sourcePort"
            let targetParentNode : JSComponent =
                assertNotNull (draw2dLib.getComponentById c conn.Target.HostId) "targetParentNode"
            let targetPort : JSPort =
                assertNotNull (draw2dLib.getPortById targetParentNode conn.Target.Id) "targetPort"
            let connId = if useId then Some conn.Id else None
            createConnection c connId conn.Vertices sourcePort targetPort
        |> tryActionWithCanvas "LoadConnection"

    member this.EditComponentLabel componentId newLabel =
        fun c ->
            let jsComp = assertNotNull
                            (draw2dLib.getComponentById c componentId)
                            "EditComponentLabel"
            draw2dLib.setComponentLabel jsComp newLabel
        |> tryActionWithCanvas "EditComponentLabel"

    /// Repaint a connection
    /// Use default color base don width if colorOpt is None
    /// colors are from CommonTypes.HighLightColor. Any js color can be added (make D.U. name equal to color name)
    member this.PaintConnection connectionId width (colorOpt: CommonTypes.HighLightColor option) =
        fun c ->
            let jsConnection =
                assertNotNull (draw2dLib.getConnectionById c connectionId) "PaintConnection"
            let label, stroke, color =
                match width with
                | 1 -> "", 1, "black"
                | n when n > 1 -> (sprintf "%d\n/" n), 3, "purple"
                | n -> failwithf "what? PaintConnection called with width %d" n 
            let color' = match colorOpt with |Some newColor -> (sprintf "%A" newColor).ToLower() | None -> color
            draw2dLib.setConnectionLabel jsConnection label
            draw2dLib.setConnectionStroke jsConnection stroke
            draw2dLib.setConnectionColor jsConnection color'
        |> tryActionWithCanvas "PaintConnection"

    /// Highlight a specific component
    /// Color is from CommonTypes.HighLightColor - colors can be added, JS color name is same as D.U. case name.
    member this.HighlightComponent (color: CommonTypes.HighLightColor) componentId = 
        fun c ->
            let comp =
                assertNotNull (draw2dLib.getComponentById c componentId) "HighlightComponent"
            draw2dLib.setComponentBackground comp (JSHelpers.getColorString color)
        |> tryActionWithCanvas "HighlightComponent"

    /// UnHighlight a specific component
    member this.UnHighlightComponent componentId = 
        fun c ->
            let comp = draw2dLib.getComponentById c componentId
            match isNull comp with
            | true -> () // The component has been removed from the diagram while it was highlighted.
            | false -> draw2dLib.setComponentBackground comp "lightgray"
        |> tryActionWithCanvas "UnHighlightComponent"
    
    /// Highlight a specific connection
    member this.HighlightConnection connectionId color =
        fun c ->
            let conn =
                assertNotNull (draw2dLib.getConnectionById c connectionId) "HighlightConnection"
            draw2dLib.setConnectionColor conn color
            draw2dLib.setConnectionStroke conn 3
        |> tryActionWithCanvas "HighlightConnection"

    /// Unhighlight a specific connection
    member this.UnHighlightConnection connectionId = 
        fun c ->
            let conn = draw2dLib.getConnectionById c connectionId
            match isNull conn with
            | true -> () // The connection has been removed from the diagram while it was highlighted.
            | false -> draw2dLib.setConnectionColor conn "black"
                       draw2dLib.setConnectionStroke conn 1
        |> tryActionWithCanvas "UnHighlightConnection"

    member this.GetCanvasState () : JSCanvasState option =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasState called when canvas is None"
            None
        | Some c ->
            let comps = jsListToFSharpList <| draw2dLib.getAllJsComponents c
            let conns = jsListToFSharpList <| draw2dLib.getAllJsConnections c
            Some (comps, conns)
    /// Return selected (JSComponents list,JSConnections list).
    /// Use extractState to convert to F# components and connections
    member this.GetSelected () : JSCanvasState option =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetSelected called when canvas is None"
            None
        | Some c ->
            let comps = jsListToFSharpList <| draw2dLib.getSelectedJsComponents c
            let conns = jsListToFSharpList <| draw2dLib.getSelectedJsConnections c
            Some (comps, conns)

    member this.Undo () =
        tryActionWithCanvas "Undo" draw2dLib.undoLastAction

    member this.Redo () =
        tryActionWithCanvas "Redo" draw2dLib.redoLastAction
    
    member this.FlushCommandStack () =
        tryActionWithCanvas "FlushCommandStack" draw2dLib.flushCommandStack

    member this.UpdateMergeWiresLabels compId topInputWidth bottomInputWidth outputWidth =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "UpdateMergeWiresLabels"
            draw2dLib.updateMergeWiresLabels jsComp topInputWidth bottomInputWidth outputWidth
        |> tryActionWithCanvas "UpdateMergeWiresLabels"

    member this.UpdateSplitWireLabels compId inputWidth topOutputWidth bottomOutputWidth =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "UpdateSplitWireLabels"
            draw2dLib.updateSplitWireLabels jsComp inputWidth topOutputWidth bottomOutputWidth
        |> tryActionWithCanvas "UpdateSplitWireLabels"

    member this.WriteMemoryLine compId addr value =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "WriteMemoryLine"
            draw2dLib.writeMemoryLine jsComp addr value
        |> tryActionWithCanvas "WriteMemoryLine"

    member this.SetNumberOfBits compId numberOfBits =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "SetNumberOfBits"
            draw2dLib.setNumberOfBits jsComp numberOfBits
        |> tryActionWithCanvas "SetNumberOfBits"

    member this.SetConstantNumber compId cNum =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "SetConstantNumber"
            draw2dLib.setConstantNumber jsComp cNum
        |> tryActionWithCanvas "SetConstantNumber"

    member this.SetLsbBitNumber compId lsbBitNumber =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "SetLsbBitNumber"
            draw2dLib.setLsbBitNumber jsComp lsbBitNumber
        |> tryActionWithCanvas "SetLsbBitNumber"

    member this.SetTopOutputWidth compId topOutputWidth =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "SetTopOutputWidth"
            draw2dLib.setTopOutputWidth jsComp topOutputWidth
        |> tryActionWithCanvas "SetTopOutputWidth"

    member this.SetRegisterWidth compId regWidth =
        fun c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "SetRegisterWidth"
            draw2dLib.setRegisterWidth jsComp regWidth
        |> tryActionWithCanvas "SetRegisterWidth"

    member this.GetComponentById compId =
        match canvas with
        | None -> Error "Draw2dWrapper.GetComponentById called when canvas is None"
        | Some c ->
            let jsComp = draw2dLib.getComponentById c compId
            match isNull jsComp with
            | true -> Error <| sprintf "Could not find component with Id: %s" compId
            | false -> Ok jsComp
        
    member this.SetSelected on (conn: JSConnection) =
        match canvas with
        | Some c -> 
            let comps, conns =
                match this.GetSelected () with
                | Some (a, b) -> a, b
                | None -> [], []
            let conns' =
                match on with
                | false -> List.filter ((<>) conn) conns
                | true -> conn::conns |> List.distinct
            draw2dLib.resetSelection c
            List.map (draw2dLib.addCompSelection c) comps |> ignore
            List.map (draw2dLib.addConnSelection c) conns' |> ignore
            ()
        | None -> ()