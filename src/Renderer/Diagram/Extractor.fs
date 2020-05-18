module Extractor

open DiagramTypes
open JSHelpers

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
    }

let private extractPorts (jsPorts : JSPorts) : Port list =
    jsListToFSharpList jsPorts
    |> List.mapi (fun i jsPort -> extractPort (Some i) jsPort)

let private extractComponentType (jsComponent : JSComponent) : ComponentType =
    match getFailIfNull jsComponent ["componentType"] with
    | "Input"  -> Input <| getFailIfNull jsComponent ["numberOfBits"]
    | "Output" -> Output <| getFailIfNull jsComponent ["numberOfBits"]
    | "Not"    -> Not
    | "And"    -> And
    | "Or"     -> Or
    | "Xor"    -> Xor
    | "Nand"   -> Nand
    | "Nor"    -> Nor
    | "Xnor"   -> Xnor
    | "Mux2"   -> Mux2
    | "Demux2" -> Demux2
    | "Custom" ->
        Custom {
            Name         = getFailIfNull jsComponent ["customComponentName"]
            InputLabels  = jsListToFSharpList (getFailIfNull jsComponent ["inputs"])
            OutputLabels = jsListToFSharpList (getFailIfNull jsComponent ["outputs"])
        }
    | "MakeBus2" -> MakeBus2
    | "SplitBus2" -> SplitBus2
    | "PushToBusFirst" -> PushToBusFirst
    | "PushToBusLast"  -> PushToBusLast
    | "PopFirstFromBus" -> PopFirstFromBus
    | "PopLastFromBus"  -> PopLastFromBus
    | ct -> failwithf "what? Component type %s does not exist" ct

let private extractVertices (jsVertices : JSVertices) : (float * float) list =
    jsListToFSharpList jsVertices
    |> List.map (fun jsVertex -> jsVertex?x, jsVertex?y)

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
    Id       = getFailIfNull jsConnection ["id"]
    Source   = extractPort None <| getFailIfNull jsConnection ["sourcePort"]
    Target   = extractPort None <| getFailIfNull jsConnection ["targetPort"]
    Vertices = extractVertices <| getFailIfNull jsConnection ["vertices"; "data"]
}

/// Transform the JSCanvasState into an f# data structure.
let extractState (state : JSCanvasState) : CanvasState =
    let (components : JSComponent list), (connections : JSConnection list) = state
    List.map extractComponent components, List.map extractConnection connections
