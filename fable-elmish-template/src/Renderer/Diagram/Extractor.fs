module Extractor

open DiagramTypes
open JSHelpers

open Fable.Core.JsInterop

let maybeExtractLabel childrenArray : string option =
    let childrenLen = childrenArray?length
    let rec maybeExtract idx =
        if idx = childrenLen
        then
            None // No label found among the children.
        else
            let child = childrenArray?(idx)?figure
            match isNull child with
            | true -> maybeExtract (idx+1) // No figure in this child.
            | false -> // Make sure the child is a label.
                match child?cssClass with
                | "draw2d_shape_basic_Label" -> Some child?text
                | _ -> maybeExtract (idx+1)
    // Children can be many things, but we care about the elements where the
    // figure is a label.
    maybeExtract 0

let private extractPort jsPort : Port =
    let portType = match jsPort?cssClass with
                   | "draw2d_InputPort" -> Input
                   | "draw2d_OutputPort" -> Output
                   | p -> failwithf "what? oprt with cssClass %s" p
    {
        Id = jsPort?id
        Label = maybeExtractLabel jsPort?children?data
        PortType = portType
    }

let private extractPorts jsPorts : Port list =
    let portsLen = jsPorts?length
    List.map (fun i -> extractPort jsPorts?(i)) [0..portsLen - 1]

let private extractComponentType (userData : obj) : ComponentType =
    // TODO make this nice. Maybe a function: getFailIfNull
    if isNull userData then failwith "what? userData is null"
    else if isNull userData?componentType then failwith "what? componentType is null"
    else userData?componentType
    |> function
       | "Not" -> Not
       | "And" -> And
       | ct -> failwithf "what? Component type %s does not exist" ct

let extractComponent (jsComponent : JSComponent) : Component = {
    Id = jsComponent?id
    Type = extractComponentType jsComponent?userData
    InputPorts = extractPorts jsComponent?inputPorts?data
    OutputPorts = extractPorts jsComponent?outputPorts?data
    Label = maybeExtractLabel jsComponent?children?data
}

let private extractConnection (jsConnection : JSConnection) : Connection = {
    Id = jsConnection?id
    Source = extractPort jsConnection?sourcePort
    Target = extractPort jsConnection?targetPort
}

let private extractComponents (jsComponents : JSComponents) : Component list =
    let componentsLen : int = jsComponents?length
    List.map (fun i -> extractComponent jsComponents?(i)) [0..componentsLen - 1]

let private extractConnections (jsConnections : JSConnections) : Connection list =
    let connectionsLen : int = jsConnections?length
    List.map (fun i -> extractConnection jsConnections?(i)) [0..connectionsLen - 1]

/// Transform the JSCanvasState into a f# data structure.
let extractState (state : JSCanvasState) : Component list * Connection list =
    log state
    let components : JSComponents = state?components
    let connections : JSConnections = state?connections
    extractComponents components, extractConnections connections
