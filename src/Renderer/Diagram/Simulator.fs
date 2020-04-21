module Simulator

open DiagramTypes
open JSHelpers

type private Bit = Zero | One

type private ConnectionsCount = | ConnectionsCount of int
type private ComponentId = | ComponentId of string
type private PortId = | PortId of string

type private SimulationComponent = {
    Id : ComponentId
    // Mapping from each input port id to its value (it will be set
    // during the simulation process).
    Inputs : Map<PortId, Bit>
    // Mapping from each outptut port id to all of the ports and
    // Components connected to that port. 
    Outputs : Map<PortId, (ComponentId * PortId) list>
    // Function that takes the inputs and transforms them into the outputs.
    // The length of each list must match the number of input/output ports.
    // The parameter 
    Reducer : Map<PortId, Bit> -> Map<PortId, Bit> option
}

// Map every ComponentId to its SimulationComponent.
type private SimulationGraph = Map<ComponentId, SimulationComponent>

// Simulating a circuit has three phases:
// - building a simulation graph made of SimulationComponents.
// - analyse the graph to look for errors, such as unconnected ports,
//   combinatorial loops, etc...
// - setting the values of the input nodes of the graph to kickstart the
//   simulation process.

// Ports constraints:
// - Source ports must be output ports.
// - Target ports must be input ports.
// - All ports have at least one connection that touches them.
// - Input ports have precisely one connection that touches them.

// In order to create the graph that represents the circuit ready to be
// simulated, iterate over each node and try to "connect" its outputs.

// During simulation, a Component can call the Reducer function only when
// all of the inputs have a value. Once this happens, it will calculate its
// outputs and set them in the next simulationComponent(s).

let private bitAnd bit0 bit1 =
    match bit0, bit1 with
    | One, One -> One
    | _, _ -> Zero

// TODO: remove.
let private testState : CanvasState = ([
    {
        Id = "input0";
        Type = Input;
        Label = "input0";
        InputPorts = [];
        OutputPorts = [
            {
                Id = "input0-OP0";
                PortNumber = Some 0
                PortType = PortType.Output;
                HostId = "input0"
            }
        ];
        X = 326;
        Y = 440
    }
    {
        Id = "input1";
        Type = Input;
        Label = "input1";
        InputPorts = [];
        OutputPorts = [
            {
                Id = "input1-OP0";
                PortNumber = Some 0
                PortType = PortType.Output;
                HostId = "input1"
            }
        ];
        X = 321;
        Y = 492
    }
    {
        Id = "And";
        Type = And;
        Label = "And";
        InputPorts = [
            {
                Id = "And-IP0";
                PortNumber = Some 0
                PortType = PortType.Input;
                HostId = "And"};
            {
                Id = "And-IP1";
                PortNumber = Some 1
                PortType = PortType.Input;
                HostId = "And"
            };
        ]
        OutputPorts = [
            {
                Id = "And-OP0";
                PortNumber = Some 0
                PortType = PortType.Output;
                HostId = "And"
            }
        ];
        X = 428;
        Y = 459
    }
    {
        Id = "output";
        Type = Output;
        Label = "output";
        InputPorts = [
            {
                Id = "output-IP0";
                PortNumber = Some 0
                PortType = PortType.Input;
                HostId = "output"
            }
        ];
        OutputPorts = [];
        X = 610;
        Y = 469
    }
    ],
    [
    {
        Id = "TopLeft";
        Source = {
            Id = "input0-OP0";
            PortNumber = None
            PortType = PortType.Output;
            HostId = "input0"
        };
        Target = {
            Id = "And-IP0";
            PortNumber = None
            PortType = PortType.Input;
            HostId = "And"
        };
    }
    {
        Id = "BottomLeft";
        Source = {
            Id = "input1-OP0";
            PortNumber = None
            PortType = PortType.Output;
            HostId = "input1"
        }
        Target = {
            Id = "And-IP1";
            PortNumber = None
            PortType = PortType.Input;
            HostId = "And"
        }
    }
    {
        Id = "Right";
        Source = {
            Id = "And-OP0";
            PortNumber = None
            PortType = PortType.Output;
            HostId = "And"
        }
        Target = {
            Id = "output-IP0";
            PortNumber = None
            PortType = PortType.Input;
            HostId = "output"
        }
    }
    ])

// For each source port in a connection, keep track of the ports it targets.
// TODO: check all source ports are Outputs, all target ports are Input.
let private buildSourceToTargetPort (connections : Connection list) : Map<PortId, (ComponentId * PortId) list> =
    (Map.empty, connections) ||> List.fold (fun map conn ->
        let key = PortId conn.Source.Id
        let target = ComponentId conn.Target.HostId, PortId conn.Target.Id
        // Append the new target to the list associated with the key.
        let newValue =
            match map.TryFind key with
            | None -> [target]
            | Some oldValue -> target :: oldValue
        map.Add (key, newValue)
    )

let private getReducer componentType =
    match componentType with
    | Input | Output -> id
    | And -> (fun (inputs : Bit list) ->
            match inputs with
            | [inp0; inp1] -> [bitAnd inp0 inp1]
            | [] | [_] -> [Zero] // Wait for more inputs.
            | _ -> failwithf "what? Unexpected inputs to And: %A" inputs 
        )
    | _ -> failwithf "what? Reducer for %A not implemented" componentType

let private buildSimulationComponent
        (sourceToTargetPort : Map<PortId, (ComponentId * PortId) list>)
        (comp : Component)
        : SimulationComponent =
    let inputs =
        comp.InputPorts
        |> List.map (fun port -> PortId port.Id, Zero) // Assume zero is the default bit value.
        |> Map.ofList
    let outputs =
        comp.OutputPorts
        |> List.map (fun port ->
            match sourceToTargetPort.TryFind <| PortId port.Id with
            | None -> failwithf "what? Unconnected output port %s in comp %s" port.Id comp.Id
            | Some targets -> PortId port.Id, targets
        )
        |> Map.ofList
    {
        Id = ComponentId comp.Id
        Inputs = inputs
        Outputs = outputs
        Reducer = (fun x -> Some x) //getReducer comp.Type
    }

let private buildSimulationGraph (canvasState : CanvasState) : SimulationGraph =
    let components, connections = canvasState
    let sourceToTargetPort = buildSourceToTargetPort connections
    components
    |> List.map (fun comp -> ComponentId comp.Id,
                             buildSimulationComponent sourceToTargetPort comp)
    |> Map.ofList

//let rec private simulateStep
//        (graph : SimulationGraph)
//        (inputs : Map<ComponentId, Bit list>) =
//    let currComp = 

let simulate () = log <| buildSimulationGraph testState
