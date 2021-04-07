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
    
let private sortComponents comps =
    comps |> List.sortBy (fun comp -> comp.X + comp.Y)

/// Transform the JSCanvasState into an f# data structure, with layout data removed (for checking significant changes).
/// Components and connections are sorted to make them order-invariant - selecting components alters order.
let extractReducedState (state : CanvasState) : CanvasState =
    let (components : Component list), (connections : Connection list) = state
    let comps = 
        components
        |> List.sortBy (fun comp -> comp.Id)
                       
    let conns =                   
        connections
        |> List.sortBy (fun conn -> conn.Id)
        
    // Sort components by their location.
    comps, conns
