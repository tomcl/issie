(*
    f# wrapper for draw_lib library.
*)

module Draw2dWrapper

open CommonTypes
open JSTypes
open DiagramMessageType
open JSHelpers
open DiagramStyle

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

// Interface with JS library.

type private IDraw2d =
    abstract setDispatchMessages :
        dispatchInferWidthsMessage_         :(unit->unit) ->
        dispatchOnSelectComponentMessage_   :(JSComponent->unit) ->
        dispatchOnUnselectComponentMessage_ :(unit->unit) ->
        dispatchHasUnsavedChangesMessage_   :(bool->unit) ->
        unit
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
    abstract createDigitalOutput          : x:int -> y:int -> numberOfBits:int -> JSComponent
    abstract createDigitalNot             : x:int -> y:int -> JSComponent
    abstract createDigitalAnd             : x:int -> y:int -> JSComponent
    abstract createDigitalOr              : x:int -> y:int -> JSComponent
    abstract createDigitalXor             : x:int -> y:int -> JSComponent
    abstract createDigitalNand            : x:int -> y:int -> JSComponent
    abstract createDigitalNor             : x:int -> y:int -> JSComponent
    abstract createDigitalXnor            : x:int -> y:int -> JSComponent
    abstract createDigitalMux2            : x:int -> y:int -> JSComponent
    abstract createDigitalDemux2          : x:int -> y:int -> JSComponent
    abstract createDigitalCustom          : x:int -> y:int -> name:string -> inputs:obj -> outputs:obj -> JSComponent
    abstract createDigitalMergeWires      : x:int -> y:int -> JSComponent
    abstract createDigitalSplitWire       : x:int -> y:int -> topOutputWidth:int -> JSComponent
    abstract createDigitalDFF             : x:int -> y:int -> JSComponent
    abstract createDigitalRegister        : x:int -> y:int -> regWidth:int -> JSComponent
    abstract createDigitalAsyncROM        : x:int -> y:int -> addressWidth:int -> wordWidth:int -> memData:'jsInt64List -> JSComponent
    abstract createDigitalROM             : x:int -> y:int -> addressWidth:int -> wordWidth:int -> memData:'jsInt64List -> JSComponent
    abstract createDigitalRAM             : x:int -> y:int -> addressWidth:int -> wordWidth:int -> memData:'jsInt64List -> JSComponent
    abstract createDigitalConnection      : source:JSPort -> target:JSPort -> JSConnection
    abstract writeMemoryLine              : comp:JSComponent -> addr:int -> value:int64 -> unit
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

[<Import("*", "./draw2d_fsharp_interface.js")>]
let private draw2dLib : IDraw2d = jsNative

// Helpers.

let private createAndInitialiseCanvas (id : string) : JSCanvas =
    let canvas = draw2dLib.createCanvas id 3000 2000
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
        | Not    -> draw2dLib.createDigitalNot x y
        | And    -> draw2dLib.createDigitalAnd x y
        | Or     -> draw2dLib.createDigitalOr x y
        | Xor    -> draw2dLib.createDigitalXor x y
        | Nand   -> draw2dLib.createDigitalNand x y
        | Nor    -> draw2dLib.createDigitalNor x y
        | Xnor   -> draw2dLib.createDigitalXnor x y
        | Mux2   -> draw2dLib.createDigitalMux2 x y
        | Demux2 -> draw2dLib.createDigitalDemux2 x y
        | ComponentType.Custom custom ->
            draw2dLib.createDigitalCustom
                x y custom.Name (fshaprListToJsList custom.InputLabels)
                                (fshaprListToJsList custom.OutputLabels)
        | MergeWires -> draw2dLib.createDigitalMergeWires x y
        | SplitWire topWireWidth -> draw2dLib.createDigitalSplitWire x y topWireWidth
        | DFF -> draw2dLib.createDigitalDFF x y
        | Register width -> draw2dLib.createDigitalRegister x y width
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
    draw2dLib.addConnectionToCanvas canvas conn
    // TODO: Setting vertices seems to break stuff, not sure why.
    // Vertices must be set only once the connection is already in the the
    // canvas, i.e. after the connection has been actually routed.
    //vertices
    //|> List.map (fun (x, y) -> createObj ["x" ==> x; "y" ==> y])
    //|> fshaprListToJsList
    //|> draw2dLib.setConnectionVertices conn

let private editComponentLabel (canvas : JSCanvas) (id : string) (newLabel : string) : unit =
    let jsComponent = draw2dLib.getComponentById canvas id
    if isNull jsComponent
    then failwithf "what? could not find diagram component with Id: %s" id
    else jsComponent?children?data?(0)?figure?setText(newLabel) // TODO: this only works for labels and it is very hacky.

// React wrapper.

type private Draw2dReactProps = {
    Dispatch : JSDiagramMsg -> unit
    DisplayMode : DisplayModeType
}

type private Draw2dReact(initialProps) =
    inherit PureStatelessComponent<Draw2dReactProps>(initialProps)

    let divId = "Draw2dCanvas"

    override this.componentDidMount() =
        log "Mounting Draw2dReact component"
        createAndInitialiseCanvas divId |> InitCanvas |> this.props.Dispatch

    override this.render() =
        let style = match this.props.DisplayMode with
                    | DisplayModeType.Hidden -> canvasHiddenStyle
                    | DisplayModeType.Visible -> canvasVisibleStyle
        div [ Id divId; style ] []

let inline private createDraw2dReact props = ofType<Draw2dReact,_,_> props []

type Draw2dWrapper() =
    let mutable canvas : JSCanvas option = None
    let mutable dispatch : (JSDiagramMsg -> unit) option = None

    /// Returns a react element containing the canvas.
    /// The dispatch function has to be: JSDiagramMsg >> dispatch
    member this.CanvasReactElement jsDiagramMsgDispatch displayMode =
        // Initialise dispatch if needed.
        match dispatch with
        | None ->
            dispatch <- Some jsDiagramMsgDispatch
            draw2dLib.setDispatchMessages
                (InferWidths >> jsDiagramMsgDispatch)
                (SelectComponent >> jsDiagramMsgDispatch)
                (UnselectComponent >> jsDiagramMsgDispatch)
                (SetHasUnsavedChanges >> jsDiagramMsgDispatch)
        | Some _ -> ()
        // Return react element with relevant props.
        createDraw2dReact {
            Dispatch = jsDiagramMsgDispatch
            DisplayMode = displayMode
        }

    member this.InitCanvas newCanvas =
        match canvas with
        | None -> canvas <- Some newCanvas
        | Some _ -> failwithf "what? InitCanvas should never be called when canvas is already created" 
    
    member this.ClearCanvas () =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.ClearCanvas called when canvas is None"
        | Some c -> draw2dLib.clearCanvas c

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
        match canvas with
        | None -> log "Warning: Draw2dWrapper.LoadConnection called when canvas or dispatch is None"
        | Some c ->
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

    member this.EditComponentLabel componentId newLabel = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.EditComponentLabel called when canvas is None"
        | Some c ->
            let jsComp = assertNotNull
                            (draw2dLib.getComponentById c componentId)
                            "EditComponentLabel"
            draw2dLib.setComponentLabel jsComp newLabel

    member this.PaintConnection connectionId width =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.PaintConnection called when canvas is None"
        | Some c ->
            let jsConnection =
                assertNotNull (draw2dLib.getConnectionById c connectionId) "PaintConnection"
            let label, stroke, color =
                match width with
                | 1 -> "", 1, "black"
                | n when n > 1 -> (sprintf "%d\n/" n), 3, "purple"
                | n -> failwithf "what? PaintConnection called with width %d" n 
            draw2dLib.setConnectionLabel jsConnection label
            draw2dLib.setConnectionStroke jsConnection stroke
            draw2dLib.setConnectionColor jsConnection color

    member this.HighlightComponent componentId = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.HighlightComponent called when canvas is None"
        | Some c ->
            let comp =
                assertNotNull (draw2dLib.getComponentById c componentId) "HighlightComponent"
            draw2dLib.setComponentBackground comp "red"

    member this.UnHighlightComponent componentId = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.UnHighlightComponent called when canvas is None"
        | Some c ->
            let comp = draw2dLib.getComponentById c componentId
            match isNull comp with
            | true -> () // The component has been removed from the diagram while it was highlighted.
            | false -> draw2dLib.setComponentBackground comp "lightgray"

    member this.HighlightConnection connectionId = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.HighlightConnection called when canvas is None"
        | Some c ->
            let conn =
                assertNotNull (draw2dLib.getConnectionById c connectionId) "HighlightConnection"
            draw2dLib.setConnectionColor conn "red"
            draw2dLib.setConnectionStroke conn 3

    member this.UnHighlightConnection connectionId = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.UnHighlightConnection called when canvas is None"
        | Some c ->
            let conn = draw2dLib.getConnectionById c connectionId
            match isNull conn with
            | true -> () // The connection has been removed from the diagram while it was highlighted.
            | false -> draw2dLib.setConnectionColor conn "black"
                       draw2dLib.setConnectionStroke conn 1

    member this.GetCanvasState () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasState called when canvas is None"
            None
        | Some c ->
            let comps = jsListToFSharpList <| draw2dLib.getAllJsComponents c
            let conns = jsListToFSharpList <| draw2dLib.getAllJsConnections c
            Some (comps, conns)

    member this.GetSelected () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetSelected called when canvas is None"
            None
        | Some c ->
            let comps = jsListToFSharpList <| draw2dLib.getSelectedJsComponents c
            let conns = jsListToFSharpList <| draw2dLib.getSelectedJsConnections c
            Some (comps, conns)

    member this.Undo () =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.Undo called when canvas is None"
        | Some c -> draw2dLib.undoLastAction c

    member this.Redo () =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.Redo called when canvas is None"
        | Some c -> draw2dLib.redoLastAction c
    
    member this.FlushCommandStack () =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.FlushCommandStack called when canvas is None"
        | Some c -> draw2dLib.flushCommandStack c

    member this.UpdateMergeWiresLabels compId topInputWidth bottomInputWidth outputWidth =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.UpdateMergeWiresLabels called when canvas is None"
        | Some c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "UpdateMergeWiresLabels"
            draw2dLib.updateMergeWiresLabels jsComp topInputWidth bottomInputWidth outputWidth

    member this.UpdateSplitWireLabels compId inputWidth topOutputWidth bottomOutputWidth =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.UpdateSplitWireLabels called when canvas is None"
        | Some c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "UpdateSplitWireLabels"
            draw2dLib.updateSplitWireLabels jsComp inputWidth topOutputWidth bottomOutputWidth

    member this.WriteMemoryLine compId addr value =
        match canvas with
        | None -> log "Warning: Draw2dWrapper.WriteMemoryLine called when canvas is None"
        | Some c ->
            let jsComp = assertNotNull (draw2dLib.getComponentById c compId) "WriteMemoryLine"
            draw2dLib.writeMemoryLine jsComp addr value

    member this.GetComponentById compId =
        match canvas with
        | None -> Error "Draw2dWrapper.GetComponentById called when canvas is None"
        | Some c ->
            let jsComp = draw2dLib.getComponentById c compId
            match isNull jsComp with
            | true -> Error <| sprintf "Could not find component with Id: %s" compId
            | false -> Ok jsComp
