module Extractor

open DiagramTypes
open JSHelpers

open Fable.Core.JsInterop

let maybeExtractLabel childrenArray : string option =
    let childrenLen = getFailIfNull childrenArray ["length"]
    let rec maybeExtract idx =
        if idx = childrenLen
        then
            None // No label found among the children.
        else
            let child = getFailIfNull childrenArray?(idx) ["figure"]
            match isNull child with
            | true -> maybeExtract (idx+1) // No figure in this child.
            | false -> // Make sure the child is a label.
                match getFailIfNull child ["cssClass"] with
                | "draw2d_shape_basic_Label" -> Some <| getFailIfNull child ["text"]
                | _ -> maybeExtract (idx+1)
    // Children can be many things, but we care about the elements where the
    // figure is a label.
    maybeExtract 0

let private extractPort jsPort : Port =
    let portType = match getFailIfNull jsPort ["cssClass"] with
                   | "draw2d_InputPort" -> Input
                   | "draw2d_OutputPort" -> Output
                   | p -> failwithf "what? oprt with cssClass %s" p
    {
        Id = getFailIfNull jsPort ["id"]
        Label = maybeExtractLabel <| getFailIfNull jsPort ["children"; "data"]
        PortType = portType
    }

let private extractPorts jsPorts : Port list =
    let portsLen = getFailIfNull jsPorts ["length"]
    List.map (fun i -> extractPort jsPorts?(i)) [0..portsLen - 1]

let private extractComponentType (jsComponent : JSComponent) : ComponentType =
    match getFailIfNull jsComponent ["userData"; "componentType"] with
    | "Not" -> Not
    | "And" -> And
    | "Mux2" -> Mux2
    | ct -> failwithf "what? Component type %s does not exist" ct

let extractComponent (jsComponent : JSComponent) : Component = {
    Id =  getFailIfNull jsComponent ["id"]
    Type = extractComponentType jsComponent
    InputPorts = extractPorts <| getFailIfNull jsComponent ["inputPorts"; "data"]
    OutputPorts = extractPorts <| getFailIfNull jsComponent ["outputPorts"; "data"]
    Label = maybeExtractLabel <| getFailIfNull jsComponent ["children"; "data"]
}

let private extractConnection (jsConnection : JSConnection) : Connection = {
    Id = getFailIfNull jsConnection ["id"]
    Source = extractPort <| getFailIfNull jsConnection ["sourcePort"]
    Target = extractPort <| getFailIfNull jsConnection ["targetPort"]
}

let private extractComponents (jsComponents : JSComponents) : Component list =
    let componentsLen : int = getFailIfNull jsComponents ["length"]
    List.map (fun i -> extractComponent jsComponents?(i)) [0..componentsLen - 1]

let private extractConnections (jsConnections : JSConnections) : Connection list =
    let connectionsLen : int = getFailIfNull jsConnections ["length"]
    List.map (fun i -> extractConnection jsConnections?(i)) [0..connectionsLen - 1]

/// Transform the JSCanvasState into a f# data structure.
let extractState (state : JSCanvasState) : Component list * Connection list =
    log state
    let components : JSComponents = getFailIfNull state ["components"]
    let connections : JSConnections = getFailIfNull state ["connections"]
    extractComponents components, extractConnections connections
