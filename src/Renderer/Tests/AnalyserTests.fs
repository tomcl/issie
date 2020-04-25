module AnalyserTests

open DiagramTypes
open Analyser
open TestLib

let private makeError msg comps conns =
    Some {
        Msg = msg
        ComponentsAffected = comps |> List.map ComponentId
        ConnectionsAffected = conns |> List.map ConnectionId
    }

// TODO: remove?
let private makeConnection id (source, sourcePort) (target, targetPort) = {
    Id = id
    Source = {
        Id = source.OutputPorts.[sourcePort].Id
        PortNumber = None
        PortType = PortType.Output
        HostId = source.Id
    }
    Target = {
        Id = target.InputPorts.[targetPort].Id
        PortNumber = None
        PortType = PortType.Input
        HostId = target.Id
    }
}
 

/// Just a single input node. No conections.
let private state1 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
    ],
    []

/// Two unconnected input nodes a single input node. No conections.
let private state2 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "input-node1"; Type = Input; Label = "input-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node1"}]; X = 169; Y = 175}
    ],
    []

/// Simple circuit with one input connected to one output.
let private state3 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
    ]

/// Simple circuit with one input connected to two outputs.
let private state4 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {Id = "output-node1"; Type = Output; Label = "output"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node1"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
        {Id = "conn1"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node1"}}
    ]

/// Two inputs connected to the same output.
let private state5 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "input-node1"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node1"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
        {Id = "conn1"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node1"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
    ]

/// Two inputs, one And, one output.
let private state6 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = ""; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
    ]

/// Two inputs, one And, one output, with extra connection input to output.
let private state7 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = ""; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
        {Id = "conn3"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
    ]

/// Two inputs, one And, one output, with extra connections inputs to and.
let private state8 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = ""; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
        {Id = "conn3"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
        {Id = "conn4"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}}
    ]

/// Mux2 with only two connected ports.
let private state9 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = ""; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "mux"; Type = Mux2; Label = ""; InputPorts = [{Id = "mux-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "mux"}; {Id = "mux-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "mux"}; {Id = "mux-in2"; PortNumber = Some 2; PortType = PortType.Input; HostId = "mux"};]; OutputPorts = [{Id = "mux-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "mux"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = ""; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "mux-in0"; PortNumber = None; PortType = PortType.Input; HostId = "mux"}}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "mux-in1"; PortNumber = None; PortType = PortType.Input; HostId = "mux"}}
        {Id = "conn2"; Source = {Id = "mux-out0"; PortNumber = None; PortType = PortType.Output; HostId = "mux"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
    ]

/// Complex diagram with 3 Ands, one input, one output and 2 cycles (yet all
/// ports are connected prpoerly).
let private state10 : CanvasState =
    [
        {Id = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"; Type = And; Label = ""; InputPorts = [{Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = Some 0; PortType = PortType.Input; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}; {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = Some 1; PortType = PortType.Input; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}]; OutputPorts = [{Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}]; X = 392; Y = 79}
        {Id = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"; Type = And; Label = ""; InputPorts = [{Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = Some 0; PortType = PortType.Input; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}; {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = Some 1; PortType = PortType.Input; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}]; OutputPorts = [{Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = Some 0; PortType = PortType.Output; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}]; X = 593; Y = 86}
        {Id = "6549c3be-5246-ef3f-2110-c4b18fa700c2"; Type = Output; Label = "output"; InputPorts = [{Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = Some 0; PortType = PortType.Input; HostId = "6549c3be-5246-ef3f-2110-c4b18fa700c2"}]; OutputPorts = []; X = 770; Y = 187}
        {Id = "4b82319e-8b00-aac3-7100-36d04fcdbca4"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "4b82319e-8b00-aac3-7100-36d04fcdbca4"}]; X = 492; Y = 245}
        {Id = "8c221531-7d35-f5ee-405d-c45afca232a6"; Type = And; Label = ""; InputPorts = [{Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = Some 0; PortType = PortType.Input; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}; {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = Some 1; PortType = PortType.Input; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}]; OutputPorts = [{Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = Some 0; PortType = PortType.Output; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}]; X = 268; Y = 261}
    ],
    [
        {Id = "76f23441-a864-e919-b115-04d8fd0b7e2b"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "6549c3be-5246-ef3f-2110-c4b18fa700c2"}}
        {Id = "da42ebd0-ffbd-e544-9aa8-c11dd2dd79d6"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}}
        {Id = "cc444ae2-e563-0c75-5b65-f3353d164db0"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "4b82319e-8b00-aac3-7100-36d04fcdbca4"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}}
        {Id = "bb489c6b-c506-7348-af33-21e935791c1c"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "310578f7-0c5e-4d0a-eaa8-16cfc69ef123"}}
        {Id = "9cc659f7-137b-3939-1f9e-0f7bdbd5889a"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}}
        {Id = "e1f47a11-5285-80df-52e3-d204faa81291"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}}
        {Id = "8241627f-266e-3bea-d013-bde4971edce3"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "8c221531-7d35-f5ee-405d-c45afca232a6"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "a54fc9bc-c668-1904-3c14-960cba4bd8f7"}}
    ]

let testCasesCheckPortsAreConnectedProperly = [
    "Unconnected input node", state1,
    makeError
        "All ports must have at least one connection."
        ["input-node0"]
        []

    "Two unconnected input nodes", state2,
    makeError
        "All ports must have at least one connection."
        ["input-node0"]
        []

    "One input and one output", state3,
    None

    "One input and two outputs", state4,
    None

    "Two inputs and one output", state5,
    makeError
        "Input port receives 2 connections. An input port should receive precisely one connection."
        ["output-node0"]
        []
    
    "Two inputs, one And, one output", state6,
    None

    "Two inputs, one And, one output, with extra connection input to output", state7,
    makeError
        "Input port receives 2 connections. An input port should receive precisely one connection."
        ["output"]
        []
    
    "Two inputs, one And, one output, with extra connections inputs to and", state8,
    makeError
        "Input port receives 2 connections. An input port should receive precisely one connection."
        ["and"]
        []
    
    "Mux2 with only two connected ports", state9,
    makeError
        "All ports must have at least one connection."
        ["mux"]
        []

    "Complex diagram with two Ands", state10,
    None
]
