module Extractor

open DiagramTypes
open JSHelpers

open Fable.Core.JsInterop

let private extractLabel childrenArray : string =
    let childrenLen = getFailIfNull childrenArray ["length"]
    let rec extract idx =
        if idx = childrenLen
        then
            // All components should have labels.
            failwithf "what? No label found among the when extracting component."
        else
            let child = getFailIfNull childrenArray?(idx) ["figure"]
            match isNull child with
            | true -> extract (idx+1) // No figure in this child.
            | false -> // Make sure the child is a label.
                match getFailIfNull child ["cssClass"] with
                | "draw2d_shape_basic_Label" -> getFailIfNull child ["text"]
                | _ -> extract (idx+1)
    // Children can be many things, but we care about the elements where the
    // figure is a label.
    extract 0

let private extractPort (jsPort : JSPort) : Port =
    let portType = match getFailIfNull jsPort ["cssClass"] with
                   | "draw2d_InputPort" -> PortType.Input
                   | "draw2d_OutputPort" -> PortType.Output
                   | p -> failwithf "what? oprt with cssClass %s" p
    {
        Id = getFailIfNull jsPort ["id"]
        PortType = portType
        HostId = getFailIfNull jsPort ["parent"; "id"]
    }

let private extractPorts (jsPorts : JSPorts) : Port list =
    let portsLen = getFailIfNull jsPorts ["length"]
    List.map (fun i -> extractPort jsPorts?(i)) [0..portsLen - 1]

let private extractComponentType (jsComponent : JSComponent) : ComponentType =
    match getFailIfNull jsComponent ["userData"; "componentType"] with
    | "Input"  -> Input
    | "Output" -> Output
    | "Not"  -> Not
    | "And"  -> And
    | "Or"   -> Or
    | "Xor"  -> Xor
    | "Nand" -> Nand
    | "Nor"  -> Nor
    | "Xnor" -> Xnor
    | "Mux2" -> Mux2
    | ct -> failwithf "what? Component type %s does not exist" ct

/// Transform a JSComponent into an f# data structure.
let extractComponent (jsComponent : JSComponent) : Component = {
    Id          = getFailIfNull jsComponent ["id"]
    Type        = extractComponentType jsComponent
    InputPorts  = extractPorts <| getFailIfNull jsComponent ["inputPorts"; "data"]
    OutputPorts = extractPorts <| getFailIfNull jsComponent ["outputPorts"; "data"]
    Label       = extractLabel <| getFailIfNull jsComponent ["children"; "data"]
    X           = getFailIfNull jsComponent ["x"]
    Y           = getFailIfNull jsComponent ["y"]
}

let private extractConnection (jsConnection : JSConnection) : Connection = {
    Id     = getFailIfNull jsConnection ["id"]
    Source = extractPort <| getFailIfNull jsConnection ["sourcePort"]
    Target = extractPort <| getFailIfNull jsConnection ["targetPort"]
}

/// Transform the JSCanvasState into an f# data structure.
let extractState (state : JSCanvasState) : CanvasState =
    let (components : JSComponent list), (connections : JSConnection list) = state
    List.map extractComponent components, List.map extractConnection connections
