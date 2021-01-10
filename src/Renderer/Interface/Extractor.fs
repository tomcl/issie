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

let extractComponentType (jsComponent : JSComponent) : ComponentType =
    match getFailIfNull jsComponent ["componentType"] with
    | "Input"  -> Input <| getFailIfNull jsComponent ["numberOfBits"]
    | "Constant" ->
            let width = getFailIfNull jsComponent ["numberOfBits"]
            let constant = getFailIfNull jsComponent ["constValue"]
            Constant(width,constant)
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
    | "Decode4" -> Decode4
    | "NbitsAdder" -> NbitsAdder <| getFailIfNull jsComponent ["numberOfBits"]
    | "NbitsXor" -> NbitsXor <| getFailIfNull jsComponent ["numberOfBits"]
    | "Custom" ->
        Custom {
            Name         = getFailIfNull jsComponent ["customComponentName"]
            InputLabels  = jsListToFSharpList (getFailIfNull jsComponent ["inputs"])
            OutputLabels = jsListToFSharpList (getFailIfNull jsComponent ["outputs"])
        }
    | "MergeWires" -> MergeWires
    | "SplitWire"  -> SplitWire <| getFailIfNull jsComponent ["topOutputWidth"]
    | "BusSelection" -> 
        let width = getFailIfNull jsComponent ["numberOfBits"]
        let lsb = getFailIfNull jsComponent ["lsbBitNumber"]
        BusSelection <| (width, lsb)
    | "BusCompare" -> 
        let busWidth = getFailIfNull jsComponent ["numberOfBits"]
        let compareVal = getFailIfNull jsComponent ["compareVal"]
        BusCompare <| (busWidth, compareVal)
    | "DFF"        -> DFF
    | "DFFE"       -> DFFE
    | "Register"   -> Register  <| getFailIfNull jsComponent ["regWidth"]
    | "RegisterE"  -> RegisterE <| getFailIfNull jsComponent ["regWidth"]
    | "AsyncROM"   -> AsyncROM <| extractMemoryData jsComponent
    | "ROM"        -> ROM <| extractMemoryData jsComponent
    | "RAM"        -> RAM <| extractMemoryData jsComponent
    | "Label"      -> IOLabel
    | ct -> failwithf "what? Component type %s does not exist: this must be added to extractor:extractComponentType" ct

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
     

/// Transform a JSComponent into an f# data structure.
let extractReducedComponent (jsComponent : JSComponent) : Component = 
    let x = ( (jsComponent?getOuterBoundingBox ()))
    let h = x?getHeight()
    let w = x?getWidth()
    {
        Id          = getFailIfNull jsComponent ["id"]
        Type        = extractComponentType jsComponent
        InputPorts  = extractPorts <| getFailIfNull jsComponent ["inputPorts"; "data"]
        OutputPorts = extractPorts <| getFailIfNull jsComponent ["outputPorts"; "data"]
        Label       = extractLabel <| getFailIfNull jsComponent ["children"; "data"]
        X = 0
        Y = 0
        H = 0
        W = 0
    }
    

let extractConnection (jsConnection : JSConnection) : Connection = {
    Id       = getFailIfNull jsConnection ["id"]
    Source   = extractPort None <| getFailIfNull jsConnection ["sourcePort"]
    Target   = extractPort None <| getFailIfNull jsConnection ["targetPort"]
    Vertices = extractVertices <| getFailIfNull jsConnection ["vertices"; "data"]
}

let extractReducedConnection (jsConnection : JSConnection) : Connection = {
    Id       = getFailIfNull jsConnection ["id"]
    Source   = extractPort None <| getFailIfNull jsConnection ["sourcePort"]
    Target   = extractPort None <| getFailIfNull jsConnection ["targetPort"]
    Vertices = []
}

let private sortComponents comps =
    comps |> List.sortBy (fun comp -> comp.X + comp.Y)

/// Transform the JSCanvasState into an f# data structure.
let extractState (state : JSCanvasState) : CanvasState =
    let (components : JSComponent list), (connections : JSConnection list) = state
    let comps, conns = List.map extractComponent components,
                       List.map extractConnection connections
    // Sort components by their location.
    let comps = sortComponents comps
    comps, conns

/// Transform the JSCanvasState into an f# data structure, with layout data removed (for checking significant changes).
/// Components and connections are sorted to make them order-invariant - selecting components alters order.
let extractReducedState (state : JSCanvasState) : CanvasState =
    let (components : JSComponent list), (connections : JSConnection list) = state
    let comps = 
        List.map extractReducedComponent components
        |> List.sortBy (fun comp -> comp.Id)
                       
    let conns =                   
        List.map extractReducedConnection connections
        |> List.sortBy (fun conn -> conn.Id)
    // Sort components by their location.
    comps, conns
 
