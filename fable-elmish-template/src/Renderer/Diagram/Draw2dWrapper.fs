(*
    f# wrapper for draw_lib library.
*)

module Draw2dWrapper

open DiagramTypes
open JSHelpers
open DiagramStyle

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

// Interface with JS library.

type private IDraw2d =
    abstract createCanvas            : id:string -> width:int -> height:int -> JSCanvas
    abstract initialiseCanvas        : canvas:JSCanvas -> unit
    abstract clearCanvas             : canvas:JSCanvas -> unit
    abstract addComponentToCanvas    : canvas:JSCanvas -> comp:JSComponent -> unit
    abstract addConnectionToCanvas   : canvas:JSCanvas -> conn:JSConnection -> unit
    abstract addLabel                : comp:JSComponent -> label:string ->  unit
    abstract setComponentId          : comp:JSComponent -> id:string -> unit
    abstract setConnectionId         : conn:JSConnection -> id:string -> unit
    abstract setPortId               : port:JSPort -> id:string -> unit
    abstract getInputPorts           : comp:JSComponent -> JSPorts
    abstract getOutputPorts          : comp:JSComponent -> JSPorts
    abstract installSelectionPolicy  : comp:JSComponent -> onSelect:(JSComponent -> unit) -> onUnselect:(JSComponent -> unit) -> unit
    abstract createDigitalInput      : x:int -> y:int -> JSComponent
    abstract createDigitalOutput     : x:int -> y:int -> JSComponent
    abstract createDigitalNot        : x:int -> y:int -> JSComponent
    abstract createDigitalAnd        : x:int -> y:int -> JSComponent
    abstract createDigitalOr         : x:int -> y:int -> JSComponent
    abstract createDigitalXor        : x:int -> y:int -> JSComponent
    abstract createDigitalNand       : x:int -> y:int -> JSComponent
    abstract createDigitalNor        : x:int -> y:int -> JSComponent
    abstract createDigitalXnor       : x:int -> y:int -> JSComponent
    abstract createDigitalMux2       : x:int -> y:int -> JSComponent
    abstract createDigitalConnection : source:JSPort -> target:JSPort -> JSConnection
    abstract getComponentById        : canvas:JSCanvas -> id:string -> JSComponent
    abstract getPortById             : comp:JSComponent -> id:string -> JSPort
    abstract getCanvasState          : canvas:JSCanvas -> JSCanvasState

[<Import("*", "../../../app/public/lib/draw2d/extensions/draw2d_fsharp_interface.js")>]
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
        : unit =
    let comp = match componentType with
               | Input  -> draw2dLib.createDigitalInput x y
               | Output -> draw2dLib.createDigitalOutput x y
               | Not    -> draw2dLib.createDigitalNot x y
               | And    -> draw2dLib.createDigitalAnd x y
               | Or     -> draw2dLib.createDigitalOr x y
               | Xor    -> draw2dLib.createDigitalXor x y
               | Nand   -> draw2dLib.createDigitalNand x y
               | Nor    -> draw2dLib.createDigitalNor x y
               | Xnor   -> draw2dLib.createDigitalXnor x y
               | Mux2   -> draw2dLib.createDigitalMux2 x y
    // Every component is assumed to have a label (may be empty string).
    draw2dLib.addLabel comp label
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
        (SelectComponent >> dispatch)
        (UnselectComponent >> dispatch)
    draw2dLib.addComponentToCanvas canvas comp

let private createConnection (canvas : JSCanvas) (id : string) (source : JSPort) (target : JSPort) =
    let conn : JSConnection = draw2dLib.createDigitalConnection source target
    draw2dLib.setConnectionId conn id
    draw2dLib.addConnectionToCanvas canvas conn

// TODO: create a JSHelper to map a js list to a f# list.
let private getAllComponents (canvas : JSCanvas) : JSComponent list =
    let figures = canvas?getFigures()?data // JS list of components.
    [0..figures?length - 1] |> List.map (fun i -> figures?(i))

// TODO: for now only supports labels.
let private editComponent (canvas : JSCanvas) (id : string) (newLabel : string) : unit =
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
        | None -> dispatch <- Some jsDiagramMsgDispatch
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

    member this.CreateComponent componentType defaultLabel =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.CreateComponent called when canvas or dispatch is None"
        | Some c, Some d -> createComponent
                                c d None componentType defaultLabel
                                None None 100 100

    member this.LoadComponent (comp : Component) =
        match canvas, dispatch with
        | None, _ | _, None -> log "Warning: Draw2dWrapper.LoadComponent called when canvas or dispatch is None"
        | Some c, Some d -> createComponent
                                c d (Some comp.Id) comp.Type comp.Label
                                (Some comp.InputPorts) (Some comp.OutputPorts)
                                comp.X comp.Y

    member this.LoadConnection (conn : Connection) =
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
            createConnection c conn.Id sourcePort targetPort

    // For now only changes the label. TODO
    member this.EditComponent componentId newLabel = 
        match canvas with
        | None -> log "Warning: Draw2dWrapper.EditComponent called when canvas is None"
        | Some c -> editComponent c componentId newLabel

    member this.GetCanvasState () =
        match canvas with
        | None ->
            log "Warning: Draw2dWrapper.GetCanvasState called when canvas is None"
            None
        | Some c ->
            Some <| draw2dLib.getCanvasState c
