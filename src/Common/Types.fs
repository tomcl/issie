module CommonTypes


let draw2dCanvasWidth = 3000
let draw2dCanvasHeight = 2000

//==========================================//
// Canvas state mapped to f# data structure //
//==========================================//

// Specify the position and type of a port in a JSComponent.
type PortType = Input | Output

/// A component I/O.
/// Id (like any other Id) is a string generated with 32 random hex charactes,
/// so it is (practically) globally unique. These Ids are used by the draw2d
/// library to uniquely refer to ports and components. They are generated via:
/// http://www.draw2d.org/draw2d_touch/jsdoc_6/#!/api/draw2d.util.UUID.
/// PortNumber is used to identify which port on a component, contiguous from 0
/// separately for inputs and outputs.
/// HostId is the unique Id of the component where the port is. For example,
/// all three ports on the same And component will have the same HostId.
type Port = {
    Id : string
    // For example, an And would have input ports 0 and 1, and output port 0.
    // If the port is used in a Connection record as Source or Target, the Number is None. 
    PortNumber : int option
    PortType : PortType
    HostId : string
}

/// Name identified the LoadedComponent used.
/// The labels define legends on symbol.
/// Label strings are unique per CustomComponent.
/// Multiple CustomComponent instances are differentiated by Component data.
type CustomComponentType = {
    Name: string
    // Tuples with (label * connection width).
    InputLabels: (string * int) list
    OutputLabels: (string * int) list 
}

type Memory = {
    // How many bits the address should have.
    // The memory will have 2^AddressWidth memory locations.
    AddressWidth : int 
    // How wide each memory word should be, in bits.
    WordWidth : int
    // Data is a list of <2^AddressWidth> elements, where each element is a
    // 64 bit integer. This makes words longer than 64 bits not supported.
    // This can be changed by using strings instead of int64, but that is way
    // less memory efficient.
    Data : int64 list
}

// Types instantiating objects in the Digital extension.
type ComponentType =
    | Input of BusWidth: int | Output of BusWidth: int | IOLabel 
    | BusSelection of OutputWidth: int * OutputLSBit: int
    | Constant of Width: int * ConstValue: int
    | Not | And | Or | Xor | Nand | Nor | Xnor |Decode4
    | Mux2 | Demux2
    | NbitsAdder of BusWidth: int
    | Custom of CustomComponentType // schematic sheet used as component
    | MergeWires | SplitWire of BusWidth: int // int is bus width
    // DFFE is a DFF with an enable signal.
    // No initial state for DFF or Register? Default 0.
    | DFF | DFFE | Register of BusWidth: int | RegisterE of BusWidth: int 
    | AsyncROM of Memory | ROM of Memory | RAM of Memory // memory is contents

/// JSComponent mapped to F# record.
/// Id uniquely identifies the component within a sheet and is used by draw2d library.
/// Label is optional descriptor displayed on schematic.
type Component = {
    Id : string
    Type : ComponentType
    Label : string // All components have a label that may be empty.
    InputPorts : Port list
    OutputPorts : Port list
    X : int
    Y : int
    H : int
    W : int
}

/// JSConnection mapped to F# record.
/// Id uniquely identifies connection globally and is used by library.
type Connection = {
    Id : string
    Source : Port
    Target : Port
    Vertices : (float * float) list
}

/// F# data describing the contents of a single schematic sheet.
type CanvasState = Component list * Connection list

//=======//
// Other //
//=======//

type NumberBase = | Hex | Dec | Bin | SDec

/// Colors to highlight components
/// Case name is used (lowercase) as HTML color name
/// See JSHelpers.getColorString
/// lots of colors can be added, see https://www.w3schools.com/colors/colors_names.asp
type HighLightColor = Red | Blue | Yellow | Green | Orange 

// The next types are not strictly necessary,
// but help in understanding what is what.
type ComponentId      = | ComponentId of string
type ConnectionId     = | ConnectionId of string
type ComponentLabel   = | ComponentLabel of string
type InputPortId      = | InputPortId of string
type OutputPortId     = | OutputPortId of string
type InputPortNumber  = | InputPortNumber of int
type OutputPortNumber = | OutputPortNumber of int

/// The driven (output) side of a connection.
/// This is stored with a NLComponent output port number.
/// Note that one output port can drive multiple NLTargets.
type NLTarget = {
    TargetCompId: ComponentId
    InputPort: InputPortNumber
    TargetConnId: ConnectionId
    }

/// The driving (input) side of a connection.
/// This is stored with a NLComponent input port number
type NLSource = {
    SourceCompId: ComponentId
    OutputPort: OutputPortNumber
    SourceConnId: ConnectionId
    }

/// components with inputs and outputs directly referencing otehr components
type NetListComponent = {
    Id : ComponentId
    Type : ComponentType
    Label : string
    // List of input port numbers, and single mapped driving output port
    // and component.
    Inputs : Map<InputPortNumber, NLSource option>
    // Mapping from each output port number to all of the input ports and
    // Components connected to that port.
    Outputs : Map<OutputPortNumber, NLTarget list>
 }

/// circuit topology with connections abstracted away
/// good for wave sim calculations
type NetList = Map<ComponentId,NetListComponent>

let getNetList ((comps,conns) : CanvasState) =
    let id2X f =
        comps
        |> List.map f
        |> Map.ofList
    let id2Outs = id2X (fun (c:Component) -> ComponentId c.Id,c.OutputPorts)
    let id2Ins = id2X (fun (c:Component) -> ComponentId c.Id,c.InputPorts)
    let id2Comp = id2X (fun (c:Component) -> ComponentId c.Id,c)

    let getPortInts sel initV ports = 
        ports
        |> List.map (fun port -> 
            match port.PortNumber with
            | Some pn -> sel pn , initV
            | _ -> failwithf "Missing port in list %A" ports)
        |> Map.ofList

    let initNets =
        comps
        |> List.map ( fun comp ->
            {
                Id = ComponentId comp.Id
                Type = comp.Type
                Label = comp.Label
                Inputs =  getPortInts InputPortNumber None comp.InputPorts 
                Outputs = getPortInts OutputPortNumber [] comp.OutputPorts
            })
        |> List.map (fun comp -> comp.Id,comp)
        |> Map.ofList

    let getOutputPortNumber (p:Port) = 
        id2Ins.[ComponentId p.HostId]
        |> List.find (fun p1 -> p1.Id = p.Id)
        |> (fun p -> match p.PortNumber with Some n -> n | None -> failwithf "Missing input port number on %A" p.HostId)
        |> OutputPortNumber
       
   
    let getInputPortNumber (p:Port) = 
        id2Outs.[ComponentId p.HostId]
        |> List.find (fun p1 -> p1.Id = p.Id)
        |> (fun p -> match p.PortNumber with Some n -> n | None -> failwithf "Missing input port number on %A" p.HostId)
        |> InputPortNumber
    
    let updateNComp compId updateFn (nets:NetList) =
        Map.add compId (updateFn nets.[compId]) nets

    let updateInputPorts pNum src (comp:NetListComponent) =
        { comp with Inputs = Map.add pNum (Some src) comp.Inputs}

    let updateInputsComp compId pNum src nets =
        let uFn = updateInputPorts pNum src
        updateNComp compId uFn nets

    let updateOutputPorts pNum tgt (comp:NetListComponent) =
        {comp with Outputs = Map.add pNum (tgt :: comp.Outputs.[pNum]) comp.Outputs}

    let updateOutputsComp compId pNum tgt nets =
        let uFn = updateOutputPorts pNum tgt
        updateNComp compId uFn nets
        
    let target (conn:Connection) =
        {
            TargetCompId = ComponentId conn.Target.HostId
            InputPort = getInputPortNumber conn.Target
            TargetConnId = ConnectionId conn.Id
        }
    let source (conn:Connection) =
        {
            SourceCompId = ComponentId conn.Source.HostId
            OutputPort = getOutputPortNumber conn.Target
            SourceConnId = ConnectionId conn.Id
        }

    let addConnectionsToNets (nets:Map<ComponentId,NetListComponent>) (conn:Connection) =
        let tgt = target conn
        let src = source conn
        let tComp = id2Comp.[tgt.TargetCompId]
        let sComp = id2Comp.[src.SourceCompId]
        nets
        |> updateOutputsComp (ComponentId sComp.Id) src.OutputPort tgt
        |> updateInputsComp (ComponentId tComp.Id)tgt.InputPort src

    (initNets, conns) ||> List.fold addConnectionsToNets
        


        
    

