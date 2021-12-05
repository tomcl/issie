module Extractor

open CommonTypes
open JSTypes
open JSHelpers
open SimulatorTypes

open Fable.Core.JsInterop

let private extractLabel childrenArray : string =
    let rec extract children =
        match children with
        | [] -> // All components should have labels.
            failwithf "what? No label found among the when extracting component."
        | child :: children' ->
            let childFig = child?figure
            match isNull childFig with
            | true -> // No figure in this child.
                extract children'
            | false -> // Make sure the child is a label.
                match getFailIfNull childFig ["cssClass"] with
                | "draw2d_shape_basic_Label" -> getFailIfNull childFig ["text"]
                | _ -> extract children'
    // Children can be many things, but we care about the elements where the
    // figure is a label.
    extract <| jsListToFSharpList childrenArray

let private extractPort (maybeNumber : int option) (jsPort : JSPort) : Port =
    let portType = match getFailIfNull jsPort ["cssClass"] with
                   | "draw2d_InputPort" -> PortType.Input
                   | "draw2d_OutputPort" -> PortType.Output
                   | p -> failwithf "what? oprt with cssClass %s" p
    {
        Id         = getFailIfNull jsPort ["id"]
        PortNumber = maybeNumber
        PortType   = portType
        HostId     = getFailIfNull jsPort ["parent"; "id"]
        //PortPos = { X = 0.0; Y = 0.0 }
    }

let private extractPorts (jsPorts : JSPorts) : Port list =
    jsListToFSharpList jsPorts
    |> List.mapi (fun i jsPort -> extractPort (Some i) jsPort)

let private extractMemoryData (jsComponent : JSComponent) : Memory = {
    AddressWidth = getFailIfNull jsComponent ["addressWidth"]
    WordWidth = getFailIfNull jsComponent ["wordWidth"]
    Data = 
        let data = getFailIfNull jsComponent ["memData"]
        data
        |> Map.ofList
}

let extractComponentType (jsComponent : JSComponent) : ComponentType = failwithf "not needed anymore"

let private extractVertices (jsVertices : JSVertices) : (float * float) list =
    jsListToFSharpList jsVertices
    |> List.map (fun jsVertex -> jsVertex?x, jsVertex?y)

/// Transform a JSComponent into an f# data structure.
let extractComponent (jsComponent : JSComponent) : Component = 
    let x = ( (jsComponent?getOuterBoundingBox ()))
    let h = x?getHeight()
    let w = x?getWidth()
  
    {
        Id          = getFailIfNull jsComponent ["id"]
        Type        = extractComponentType jsComponent
        InputPorts  = extractPorts <| getFailIfNull jsComponent ["inputPorts"; "data"]
        OutputPorts = extractPorts <| getFailIfNull jsComponent ["outputPorts"; "data"]
        Label       = extractLabel <| getFailIfNull jsComponent ["children"; "data"]
        X           = getFailIfNull jsComponent ["x"]
        Y           = getFailIfNull jsComponent ["y"]
        H           = h
        W           = w
    }
    

/// Transform the CanvasState into an f# data structure, with layout data removed (for checking electrically significant changes).
/// Components and connections are sorted to make them order-invariant - selecting components alters order.
/// This is currently not properly used because the save and autosave logic is not yet properly re-implemented
/// after change to new draw block.
let extractReducedState (state : CanvasState) : CanvasState =
    let (components : Component list), (connections : Connection list) = state
    let comps = 
        components
        |> List.map (fun comp -> {comp with H=0;W=0;X=0;Y=0})
        |> List.sortBy (fun comp -> comp.Id)
                       
    let conns =                   
        connections
        |> List.map (fun conn -> {conn with Vertices = []})
        |> List.sortBy (fun conn -> conn.Id)
    comps, conns

/// Are two lists of vertices identical
let verticesAreSame tolerance (conns1:(float*float) list) (conns2: (float*float) list) =
    let sq x = x*x
    conns1.Length = conns2.Length &&
    List.zip conns1 conns2
    |> List.map (fun ((x1,y1),(x2,y2)) -> sq(x1-x2) + sq(y1-y2))
    |> List.sum
    |> (fun d -> d < tolerance)

/// Are two lists of connections identical
let compareConns tolerance conns1 conns2 =
    let connIdA (conns:Connection List) =
        conns
        |> Array.ofList
        |> Array.sortBy (fun conn -> conn.Id)      
    let connsA1 = connIdA conns1
    let connsA2 = connIdA conns2
    connsA1.Length = connsA2.Length &&
        Array.forall2 (fun c1 c2 ->
            verticesAreSame tolerance c1.Vertices c2.Vertices)  connsA1 connsA2

/// Are two lists of components identical
let compareComps tolerance comps1 comps2 =
    let isClose a b = float ((a-b)*(a-b)) < tolerance
    let compIdA (comps:Component List) =
        comps
        |> Array.ofList
        |> Array.sortBy (fun comp -> comp.Id)      
    let compsA1 = compIdA comps1
    let compsA2 = compIdA comps2
    compsA1.Length = compsA2.Length &&
        Array.forall2 (fun (c1: Component) (c2:Component) -> isClose c1.X c2.X && isClose c1.Y c2.Y)  compsA1 compsA2

/// Robust comparison of two schematics. Tolerance determines how similar
/// counts as equal.
/// cannot use equality because float vertices may not be identical
/// use to detemine whether schematic needs to be saved
/// NB for electrical circuit comparison use extractReducedState.
let compareCanvas 
        (tolerance: float)
        ((comps1,conns1):CanvasState) 
        ((comps2,conns2):CanvasState) =
    let reduce comps =
        comps
        |> List.toArray
        |> Array.map (fun comp -> {comp with H=0;W=0;X=0;Y=0})
        |> Array.sortBy (fun comp -> comp.Id)
    let compsOk = reduce comps1 = reduce comps2
    let compsSamePos = compareComps tolerance comps1 comps2
    let connsOk = compareConns tolerance conns1 conns2
    compsOk && compsSamePos && connsOk

/// Compare the name and IOs of two sheets as loadedcomponents
/// For backups, if these chnage something major has happened
let compareIOs (ldc1:LoadedComponent)  (ldc2:LoadedComponent) =
    Set(ldc1.InputLabels) = Set(ldc2.InputLabels) && ldc1.Name = ldc2.Name



