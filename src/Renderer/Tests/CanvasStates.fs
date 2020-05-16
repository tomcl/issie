module CanvasStates

open DiagramTypes

// Guidelines to create states:
// - draw the diagram you want to test in the actual application, then log its
//   state state. Use logStateToFSharp.py to transform the log output into
//   a (almost) valid FSharp data structure.
// - write it from scratch. Remember:
//   --> each component Id must be unique
//   --> each connection Id must be unique
//   --> each port Id must be unique

let private makeCustomComponent dep = {
    Name = dep.Name
    InputLabels = dep.InputLabels
    OutputLabels = dep.OutputLabels
}

/// Just a single input node. No conections.
let state1 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
    ],
    []

/// State1 loaded as a dependency.
let state1Dependency : LoadedComponent = {
    Name = "broken-one-input"
    FilePath = ""
    CanvasState = state1
    InputLabels = ["input-node0", 1]
    OutputLabels = []
}

/// State1 custom component.
let state1CustomComponent : CustomComponentType = 
    makeCustomComponent state1Dependency

/// Two unconnected input nodes. No conections.
let state2 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "input-node0-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "input-node1"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "input-node1-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node1"}]; X = 169; Y = 175}
    ],
    []

/// Simple circuit with one input connected to one output.
let state3 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}; Vertices = []}
    ]

/// State3 loaded as a dependency.
let state3Dependency : LoadedComponent = {
    Name = "input-output"
    FilePath = ""
    CanvasState = state3
    InputLabels = ["input-node0-label", 1]
    OutputLabels = ["output-node0-label", 1]
}

/// State3 custom component.
let state3CustomComponent : CustomComponentType = 
    makeCustomComponent state3Dependency

/// Simple circuit with one input connected to two outputs.
let state4 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-node0-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {Id = "output-node1"; Type = Output; Label = "output-node1-label"; InputPorts = [{Id = "output-node1-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node1"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "output-node0-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "output-node1-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node1"}; Vertices = []}
    ]

/// Two inputs connected to the same output.
let state5 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "input-node0-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "input-node1"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "input-node1-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node1"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "input-node0-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "input-node1-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node1"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}; Vertices = []}
    ]

/// Two inputs; one And; one output.
let state6 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
    ]

/// Two inputs; one And; one output; with extra connection input to output.
let state7 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
        {Id = "conn3"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
    ]

/// Two inputs; one And; one output; with extra connections inputs to and.
let state8 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
        {Id = "conn3"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn4"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
    ]

/// Mux2 with only two connected ports.
let state9 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "mux"; Type = Mux2; Label = ""; InputPorts = [{Id = "mux-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "mux"}; {Id = "mux-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "mux"}; {Id = "mux-in2"; PortNumber = Some 2; PortType = PortType.Input; HostId = "mux"};]; OutputPorts = [{Id = "mux-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "mux"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "mux-in0"; PortNumber = None; PortType = PortType.Input; HostId = "mux"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "mux-in1"; PortNumber = None; PortType = PortType.Input; HostId = "mux"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "mux-out0"; PortNumber = None; PortType = PortType.Output; HostId = "mux"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
    ]

/// Complex diagram with 3 Ands; one input; one output and 2 cycles (yet all
/// ports are connected prpoerly).
let state10 : CanvasState =
    [
        {Id = "and0"; Type = And; Label = ""; InputPorts = [{Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and0"}; {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and0"}]; OutputPorts = [{Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and0"}]; X = 392; Y = 79}
        {Id = "and1"; Type = And; Label = ""; InputPorts = [{Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and1"}; {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and1"}]; OutputPorts = [{Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and1"}]; X = 593; Y = 86}
        {Id = "output"; Type = Output; Label = "output"; InputPorts = [{Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 770; Y = 187}
        {Id = "input"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input"}]; X = 492; Y = 245}
        {Id = "and2"; Type = And; Label = ""; InputPorts = [{Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and2"}; {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and2"}]; OutputPorts = [{Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and2"}]; X = 268; Y = 261}
    ],
    [
        {Id = "conn0"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}; Vertices = []}
        {Id = "conn3"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}; Vertices = []}
        {Id = "conn4"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}; Vertices = []}
        {Id = "conn5"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}; Vertices = []}
        {Id = "conn6"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}; Vertices = []}
    ]

/// Complex diagram with 3 Ands; one input; one output and 1 cycles with three
/// components (yet all ports are connected prpoerly).
let state11 : CanvasState =
    [
        {Id = "and0"; Type = And; Label = ""; InputPorts = [{Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and0"}; {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and0"}]; OutputPorts = [{Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and0"}]; X = 392; Y = 79}
        {Id = "and1"; Type = And; Label = ""; InputPorts = [{Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and1"}; {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and1"}]; OutputPorts = [{Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and1"}]; X = 593; Y = 86}
        {Id = "output"; Type = Output; Label = "output"; InputPorts = [{Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 770; Y = 187}
        {Id = "input"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input"}]; X = 492; Y = 245}
        {Id = "and2"; Type = And; Label = ""; InputPorts = [{Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and2"}; {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and2"}]; OutputPorts = [{Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and2"}]; X = 268; Y = 261}
    ],
    [
        {Id = "conn0"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}; Vertices = []}
        {Id = "conn3"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}; Vertices = []}
        {Id = "conn4"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}; Vertices = []}
        {Id = "conn5"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}; Vertices = []}
        {Id = "conn6"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}; Vertices = []}
    ]

/// Complex diagram with 3 Ands; one input; one output and no cycles.
let state12 : CanvasState =
    [
        {Id = "and0"; Type = And; Label = ""; InputPorts = [{Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and0"}; {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and0"}]; OutputPorts = [{Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and0"}]; X = 392; Y = 79}
        {Id = "and1"; Type = And; Label = ""; InputPorts = [{Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and1"}; {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and1"}]; OutputPorts = [{Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and1"}]; X = 593; Y = 86}
        {Id = "output"; Type = Output; Label = "output"; InputPorts = [{Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 770; Y = 187}
        {Id = "input"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input"}]; X = 492; Y = 245}
        {Id = "and2"; Type = And; Label = ""; InputPorts = [{Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and2"}; {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and2"}]; OutputPorts = [{Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and2"}]; X = 268; Y = 261}
    ],
    [
        {Id = "conn0"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}; Vertices = []}
        {Id = "conn3"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}; Vertices = []}
        {Id = "conn4"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}; Vertices = []}
        {Id = "conn5"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}; Vertices = []}
        {Id = "conn6"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}; Vertices = []}
    ]

/// One bit adder.
let state13 : CanvasState =
    [
        {Id="2953603d-44e4-5c1f-3fb1-698f7863b6b5";Type=Input;Label="A";InputPorts=[];OutputPorts=[{Id="336aab97-a7bd-9a37-9062-56753b57c268";PortNumber= Some 0;PortType=PortType.Output;HostId="2953603d-44e4-5c1f-3fb1-698f7863b6b5"}];X=97;Y=111}
        {Id="170e69f4-b3d7-d9e0-9f1d-6a564ba62062";Type=Input;Label="B";InputPorts=[];OutputPorts=[{Id="1ee439f3-8d23-c049-ff9e-cd8f1b4d3d9d";PortNumber= Some 0;PortType=PortType.Output;HostId="170e69f4-b3d7-d9e0-9f1d-6a564ba62062"}];X=54;Y=203}
        {Id="253e21f0-b062-4858-c315-5a5315cadf45";Type=And;Label="";InputPorts=[{Id="1b2c73d1-b38e-64e6-5b19-e4e7f690a692";PortNumber= Some 0;PortType=PortType.Input;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}; {Id="9fdb22f6-f8eb-829f-68b3-87b9c8577299";PortNumber= Some 1;PortType=PortType.Input;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}];OutputPorts=[{Id="95a03834-e494-92b2-b8d4-a148e3c0763b";PortNumber= Some 0;PortType=PortType.Output;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}];X=498;Y=208}
        {Id="6b7bac71-eec5-4979-834a-c1bfe40d77b9";Type=Xor;Label="";InputPorts=[{Id="6a468f2c-4db4-d3ae-402f-a617885884e7";PortNumber= Some 0;PortType=PortType.Input;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}; {Id="9826a9b5-cd62-51ec-6368-93c4967745f9";PortNumber= Some 1;PortType=PortType.Input;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}];OutputPorts=[{Id="7cb01ee2-49d4-00b5-7523-14e4de3c5489";PortNumber= Some 0;PortType=PortType.Output;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}];X=501;Y=108}
        {Id="9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9";Type=Output;Label="Sum";InputPorts=[{Id="14b23a60-21ae-374f-d083-41aa2510eeab";PortNumber= Some 0;PortType=PortType.Input;HostId="9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9"}];OutputPorts=[];X=691;Y=122}
        {Id="94da6dd7-a263-a3ec-ec76-bfa07b0b0f34";Type=Output;Label="Carry";InputPorts=[{Id="b9d457f7-dcef-89a1-aa76-b9ba7f9ca3f4";PortNumber= Some 0;PortType=PortType.Input;HostId="94da6dd7-a263-a3ec-ec76-bfa07b0b0f34"}];OutputPorts=[];X=692;Y=223}
    ],
    [
        {Id="79df6e40-7ad2-b1d2-cb98-ea5d649ef1cc";Source={Id="7cb01ee2-49d4-00b5-7523-14e4de3c5489";PortNumber=None;PortType=PortType.Output;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"};Target={Id="14b23a60-21ae-374f-d083-41aa2510eeab";PortNumber=None;PortType=PortType.Input;HostId="9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9"}; Vertices = []}
        {Id="6b066c26-750b-6eb3-2516-bc122899f846";Source={Id="1ee439f3-8d23-c049-ff9e-cd8f1b4d3d9d";PortNumber=None;PortType=PortType.Output;HostId="170e69f4-b3d7-d9e0-9f1d-6a564ba62062"};Target={Id="9fdb22f6-f8eb-829f-68b3-87b9c8577299";PortNumber=None;PortType=PortType.Input;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}; Vertices = []}
        {Id="5f7c6896-8fbd-bfaf-ee0f-a677f7804283";Source={Id="1ee439f3-8d23-c049-ff9e-cd8f1b4d3d9d";PortNumber=None;PortType=PortType.Output;HostId="170e69f4-b3d7-d9e0-9f1d-6a564ba62062"};Target={Id="9826a9b5-cd62-51ec-6368-93c4967745f9";PortNumber=None;PortType=PortType.Input;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}; Vertices = []}
        {Id="8ed22d89-b542-a182-fef9-1a67170da60e";Source={Id="336aab97-a7bd-9a37-9062-56753b57c268";PortNumber=None;PortType=PortType.Output;HostId="2953603d-44e4-5c1f-3fb1-698f7863b6b5"};Target={Id="1b2c73d1-b38e-64e6-5b19-e4e7f690a692";PortNumber=None;PortType=PortType.Input;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}; Vertices = []}
        {Id="0cd3f109-22ee-3543-7247-db2b4ffe5fc8";Source={Id="336aab97-a7bd-9a37-9062-56753b57c268";PortNumber=None;PortType=PortType.Output;HostId="2953603d-44e4-5c1f-3fb1-698f7863b6b5"};Target={Id="6a468f2c-4db4-d3ae-402f-a617885884e7";PortNumber=None;PortType=PortType.Input;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}; Vertices = []}
        {Id="3230b338-cdd4-7632-e441-9d2f50df013a";Source={Id="95a03834-e494-92b2-b8d4-a148e3c0763b";PortNumber=None;PortType=PortType.Output;HostId="253e21f0-b062-4858-c315-5a5315cadf45"};Target={Id="b9d457f7-dcef-89a1-aa76-b9ba7f9ca3f4";PortNumber=None;PortType=PortType.Input;HostId="94da6dd7-a263-a3ec-ec76-bfa07b0b0f34"}; Vertices = []}
    ]

/// Similar to state4, but output nodes have the same label.
let state14 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output-duplicate-label"; InputPorts = [{Id = "output-node0-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {Id = "output-node1"; Type = Output; Label = "output-duplicate-label"; InputPorts = [{Id = "output-node1-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node1"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "output-node0-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "output-node1-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node1"}; Vertices = []}
    ]

/// Similar to state6, but input nodes have the same label.
let state15 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-duplicate-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-duplicate-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
    ]

/// One input and one output, connected to the state3CustomComponent.
let state16 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "outer-input-node0-label"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-output-node0"; Type = Output; Label = "outer-output-node0-label"; InputPorts = [{Id = "outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "inp-out-component"; Type = Custom state3CustomComponent; Label = "inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "inp-out-component"};
            Target = {Id = "outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-output-node0"};
            Vertices = []
        }
    ]

/// State16 loaded as a dependency.
let state16Dependency : LoadedComponent = {
    Name = "nested-input-output"
    FilePath = ""
    CanvasState = state16
    InputLabels = ["outer-input-node0-label", 1]
    OutputLabels = ["outer-output-node0-label", 1]
}

/// State16 custom component.
let state16CustomComponent : CustomComponentType = 
    makeCustomComponent state16Dependency

/// One input and one output, connected to the state16CustomComponent, which
/// contains the state3CustomComponent.
let state17 : CanvasState =
    [
        {Id = "outer-outer-input-node0"; Type = Input; Label = "outer-outer-input-node0-label"; InputPorts = []; OutputPorts = [{Id = "outer-outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-outer-output-node0"; Type = Output; Label = "outer-outer-output-node0-label"; InputPorts = [{Id = "outer-outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "wrapped-inp-out-component"; Type = Custom state3CustomComponent; Label = "wrapped-inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "wrapped-inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "wrapped-inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "wrapped-inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "wrapped-inp-out-component"};
            Target = {Id = "outer-outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-outer-output-node0"};
            Vertices = []
        }
    ]

/// State17 loaded as a dependency.
let state17Dependency : LoadedComponent = {
    Name = "doubly-nested-input-output"
    FilePath = ""
    CanvasState = state17
    InputLabels = ["outer-outer-input-node0-label", 1]
    OutputLabels = ["outer-outer-output-node0-label", 1]
}

/// State17 custom component.
let state17CustomComponent : CustomComponentType =
    makeCustomComponent state17Dependency

/// One input and one output, connected to the state17CustomComponent, which
/// contains state16CustomComponent, which contains the state3CustomComponent.
let state18 : CanvasState =
    [
        {Id = "outer-outer-outer-input-node0"; Type = Input; Label = "outer-outer-outer-input-node0-label"; InputPorts = []; OutputPorts = [{Id = "outer-outer-outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-outer-outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-outer-outer-output-node0"; Type = Output; Label = "outer-outer-outer-output-node0-label"; InputPorts = [{Id = "outer-outer-outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-outer-outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "doubly-wrapped-inp-out-component"; Type = Custom state3CustomComponent; Label = "doubly-wrapped-inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "doubly-wrapped-inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "doubly-wrapped-inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-outer-outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-outer-outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "doubly-wrapped-inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "doubly-wrapped-inp-out-component"};
            Target = {Id = "outer-outer-outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-outer-outer-output-node0"};
            Vertices = []
        }
    ]

/// One input connected to a broken dependency.
let state19 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "outer-input-node0-label"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "broken-input"; Type = Custom state1CustomComponent; Label = "broken-input-label"; InputPorts = [{Id = "broken-input-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "broken-input"}]; OutputPorts = []; X = 169; Y = 175}
    ],
    [
        {
            Id = "conn0"
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"}
            Target = {Id = "broken-input-port0"; PortNumber = None; PortType = PortType.Input; HostId = "broken-input"}
            Vertices = []
        }
    ]

/// Similar to state16, but uses state16CustomComponent instead of
/// state3CustomComponent to create a cycle.
let state20 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "outer-input-node0-label"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-output-node0"; Type = Output; Label = "outer-output-node0-label"; InputPorts = [{Id = "outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "inp-out-component"; Type = Custom state16CustomComponent; Label = "inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "inp-out-component"};
            Target = {Id = "outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-output-node0"};
            Vertices = []
        }
    ]

// Test a long dependecy cycle.
// state21 --> state22 --> state23 --> state21

let state21CustomComponent : CustomComponentType = {
    Name = "21-custom-component"
    InputLabels = ["21-inp", 1]
    OutputLabels = ["21-out", 1]
}

let state22CustomComponent : CustomComponentType = {
    Name = "22-custom-component"
    InputLabels = ["22-inp", 1]
    OutputLabels = ["22-out", 1]
}

let state23CustomComponent : CustomComponentType = {
    Name = "23-custom-component"
    InputLabels = ["23-inp", 1]
    OutputLabels = ["23-out", 1]
}

/// Simple input-output component that uses state22.
let state21 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "21-inp"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-output-node0"; Type = Output; Label = "21-out"; InputPorts = [{Id = "outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "inp-out-component"; Type = Custom state22CustomComponent; Label = "inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "inp-out-component"};
            Target = {Id = "outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-output-node0"};
            Vertices = []
        }
    ]

/// Simple input-output component that uses state23.
let state22 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "22-inp"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-output-node0"; Type = Output; Label = "22-out"; InputPorts = [{Id = "outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "inp-out-component"; Type = Custom state23CustomComponent; Label = "inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "inp-out-component"};
            Target = {Id = "outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-output-node0"};
            Vertices = []
        }
    ]

/// Simple input-output component that uses state21.
let state23 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "23-inp"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-output-node0"; Type = Output; Label = "23-out"; InputPorts = [{Id = "outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "inp-out-component"; Type = Custom state21CustomComponent; Label = "inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "inp-out-component"};
            Target = {Id = "outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-output-node0"};
            Vertices = []
        }
    ]

let state21Dependency : LoadedComponent = {
    Name = "21-custom-component"
    FilePath = ""
    CanvasState = state21
    InputLabels = ["21-inp", 1]
    OutputLabels = ["21-out", 1]
}

let state22Dependency : LoadedComponent = {
    Name = "22-custom-component"
    FilePath = ""
    CanvasState = state22
    InputLabels = ["22-inp", 1]
    OutputLabels = ["22-out", 1]
}

let state23Dependency : LoadedComponent = {
    Name = "23-custom-component"
    FilePath = ""
    CanvasState = state23
    InputLabels = ["23-inp", 1]
    OutputLabels = ["23-out", 1]
}

/// Simple input-output component that uses state23.
let state24 : CanvasState =
    [
        {Id = "outer-input-node0"; Type = Input; Label = "24-inp"; InputPorts = []; OutputPorts = [{Id = "outer-out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "outer-input-node0"}]; X = 169; Y = 175}
        {Id = "outer-output-node0"; Type = Output; Label = "24-out"; InputPorts = [{Id = "outer-inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "outer-output-node0"}]; OutputPorts = []; X = 364; Y = 175}
        {
            Id = "inp-out-component"; Type = Custom state23CustomComponent; Label = "inp-out-component-label";
            InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "inp-out-component"}];
            OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "inp-out-component"}];
            X = 169; Y = 175
        }
    ],
    [
        {
            Id = "conn0";
            Source = {Id = "outer-out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "outer-input-node0"};
            Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "inp-out-component"};
            Vertices = []
        }
        {
            Id = "conn1";
            Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "inp-out-component"};
            Target = {Id = "outer-inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "outer-output-node0"};
            Vertices = []
        }
    ]

/// Similar to state6, but with a missing connection from bottom input to and.
let state25 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
    ]

/// Similar to state6, but with a missing connection from and to output.
let state26 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn0"; Source = {Id = "top-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "top-input"}; Target = {Id = "and-in0"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
        {Id = "conn1"; Source = {Id = "bottom-input-out0"; PortNumber = None; PortType = PortType.Output; HostId = "bottom-input"}; Target = {Id = "and-in1"; PortNumber = None; PortType = PortType.Input; HostId = "and"}; Vertices = []}
    ]

/// Similar to state6, but with both inputs unconnected.
let state27 : CanvasState =
    [
        {Id = "top-input"; Type = Input; Label = "input-node0-label"; InputPorts = []; OutputPorts = [{Id = "top-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "top-input"}]; X = 326; Y = 440}
        {Id = "bottom-input"; Type = Input; Label = "input-node1-label"; InputPorts = []; OutputPorts = [{Id = "bottom-input-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "bottom-input"}]; X = 321; Y = 492}
        {Id = "and"; Type = And; Label = ""; InputPorts = [{Id = "and-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "and"}; {Id = "and-in1"; PortNumber = Some 1; PortType = PortType.Input; HostId = "and"}]; OutputPorts = [{Id = "and-out0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "and"}]; X = 428; Y = 459}
        {Id = "output"; Type = Output; Label = "output-node0-label"; InputPorts = [{Id = "output-in0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output"}]; OutputPorts = []; X = 610; Y = 469}
    ],
    [
        {Id = "conn2"; Source = {Id = "and-out0"; PortNumber = None; PortType = PortType.Output; HostId = "and"}; Target = {Id = "output-in0"; PortNumber = None; PortType = PortType.Input; HostId = "output"}; Vertices = []}
    ]


// Half adder.

let halfAdderState : CanvasState =
    [
        {Id = "50151b16-a5e1-76ca-93d2-c4446a6a6c60"; Type = Input; Label = "B"; InputPorts = []; OutputPorts = [{Id = "0ca8918f-28b0-0892-4335-3a375c35ef15"; PortNumber = Some 0; PortType = PortType.Output; HostId = "50151b16-a5e1-76ca-93d2-c4446a6a6c60"}]; X = 100; Y = 195}
        {Id = "36c5c713-b731-21d0-c2f9-9e9bf4c79048"; Type = Input; Label = "A"; InputPorts = []; OutputPorts = [{Id = "23fcb523-bd9d-0ca1-cb84-6a37779f2dce"; PortNumber = Some 0; PortType = PortType.Output; HostId = "36c5c713-b731-21d0-c2f9-9e9bf4c79048"}]; X = 100; Y = 100}
        {Id = "7227d00f-d329-af58-b769-0bf42ee6f2ef"; Type = And; Label = ""; InputPorts = [{Id = "49c3e9ab-4ad5-0c6c-cda9-987a820ad1c5"; PortNumber = Some 0; PortType = PortType.Input; HostId = "7227d00f-d329-af58-b769-0bf42ee6f2ef"}; {Id = "7d121026-401b-72e5-2051-3497b65d99b3"; PortNumber = Some 1; PortType = PortType.Input; HostId = "7227d00f-d329-af58-b769-0bf42ee6f2ef"}]; OutputPorts = [{Id = "45d7d345-38f8-0933-a86c-16159674e4a0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "7227d00f-d329-af58-b769-0bf42ee6f2ef"}]; X = 267; Y = 179}
        {Id = "5ea2bff6-ee86-e729-5c14-a52b302af517"; Type = Xor; Label = ""; InputPorts = [{Id = "1e8be0a9-1c87-e877-3e7b-a6335f5ddad4"; PortNumber = Some 0; PortType = PortType.Input; HostId = "5ea2bff6-ee86-e729-5c14-a52b302af517"}; {Id = "77dfc4b4-6616-6768-7c35-8c3c1c26acbd"; PortNumber = Some 1; PortType = PortType.Input; HostId = "5ea2bff6-ee86-e729-5c14-a52b302af517"}]; OutputPorts = [{Id = "27537cac-d2b0-dd3d-e0ee-a6f27d2e793e"; PortNumber = Some 0; PortType = PortType.Output; HostId = "5ea2bff6-ee86-e729-5c14-a52b302af517"}]; X = 267; Y = 97}
        {Id = "0be5c1d8-c5a8-5fad-6e4b-e74e873ce728"; Type = Output; Label = "Sum"; InputPorts = [{Id = "26723054-cb13-f8b2-3a5d-ae8224d532e1"; PortNumber = Some 0; PortType = PortType.Input; HostId = "0be5c1d8-c5a8-5fad-6e4b-e74e873ce728"}]; OutputPorts = []; X = 420; Y = 107}
        {Id = "56dd9000-1a54-8040-3e90-33e44ce390b9"; Type = Output; Label = "Cout"; InputPorts = [{Id = "617c8ca8-d4b3-182f-b8d1-ce7f4b6a5359"; PortNumber = Some 0; PortType = PortType.Input; HostId = "56dd9000-1a54-8040-3e90-33e44ce390b9"}]; OutputPorts = []; X = 420; Y = 189}
    ],
    [
        {Id = "5751a3dc-26e0-2df2-d797-af37b357f7bd"; Source = {Id = "27537cac-d2b0-dd3d-e0ee-a6f27d2e793e"; PortNumber = None; PortType = PortType.Output; HostId = "5ea2bff6-ee86-e729-5c14-a52b302af517"}; Target = {Id = "26723054-cb13-f8b2-3a5d-ae8224d532e1"; PortNumber = None; PortType = PortType.Input; HostId = "0be5c1d8-c5a8-5fad-6e4b-e74e873ce728"}; Vertices = [307.0,117.0; 420.0,117.0]}
        {Id = "4d823d53-2991-68a4-a72b-7c3aec1e6b75"; Source = {Id = "0ca8918f-28b0-0892-4335-3a375c35ef15"; PortNumber = None; PortType = PortType.Output; HostId = "50151b16-a5e1-76ca-93d2-c4446a6a6c60"}; Target = {Id = "7d121026-401b-72e5-2051-3497b65d99b3"; PortNumber = None; PortType = PortType.Input; HostId = "7227d00f-d329-af58-b769-0bf42ee6f2ef"}; Vertices = [130.0,205.0; 198.5,205.0; 198.5,205.66666666666666; 267.0,205.66666666666666]}
        {Id = "3491b238-ea58-d04a-8d55-78b630507094"; Source = {Id = "23fcb523-bd9d-0ca1-cb84-6a37779f2dce"; PortNumber = None; PortType = PortType.Output; HostId = "36c5c713-b731-21d0-c2f9-9e9bf4c79048"}; Target = {Id = "49c3e9ab-4ad5-0c6c-cda9-987a820ad1c5"; PortNumber = None; PortType = PortType.Input; HostId = "7227d00f-d329-af58-b769-0bf42ee6f2ef"}; Vertices = [130.0,110.0; 170.5,110.0; 170.5,192.33333333333334; 267.0,192.33333333333334]}
        {Id = "9c7f868c-0411-eeaf-eefe-9923ae6a66bd"; Source = {Id = "23fcb523-bd9d-0ca1-cb84-6a37779f2dce"; PortNumber = None; PortType = PortType.Output; HostId = "36c5c713-b731-21d0-c2f9-9e9bf4c79048"}; Target = {Id = "1e8be0a9-1c87-e877-3e7b-a6335f5ddad4"; PortNumber = None; PortType = PortType.Input; HostId = "5ea2bff6-ee86-e729-5c14-a52b302af517"}; Vertices = [130.0,110.0; 198.5,110.0; 198.5,110.33333333333333; 267.0,110.33333333333333]}
        {Id = "c5c8d60b-712d-b182-aaf1-b902c1eb4b7b"; Source = {Id = "0ca8918f-28b0-0892-4335-3a375c35ef15"; PortNumber = None; PortType = PortType.Output; HostId = "50151b16-a5e1-76ca-93d2-c4446a6a6c60"}; Target = {Id = "77dfc4b4-6616-6768-7c35-8c3c1c26acbd"; PortNumber = None; PortType = PortType.Input; HostId = "5ea2bff6-ee86-e729-5c14-a52b302af517"}; Vertices = [130.0,205.0; 198.5,205.0; 198.5,123.66666666666667; 267.0,123.66666666666667]}
        {Id = "712471ca-270a-0410-a9e5-a7894a6c2df4"; Source = {Id = "45d7d345-38f8-0933-a86c-16159674e4a0"; PortNumber = None; PortType = PortType.Output; HostId = "7227d00f-d329-af58-b769-0bf42ee6f2ef"}; Target = {Id = "617c8ca8-d4b3-182f-b8d1-ce7f4b6a5359"; PortNumber = None; PortType = PortType.Input; HostId = "56dd9000-1a54-8040-3e90-33e44ce390b9"}; Vertices = [307.0,199.0; 420.0,199.0]}
    ]

let halfAdderDependency : LoadedComponent = {
    Name = "half-adder"
    FilePath = ""
    CanvasState = halfAdderState
    InputLabels = ["A", 1; "B", 1]
    OutputLabels = ["Cout", 1; "Sum", 1]
}

let halfAdderCustom : CustomComponentType =
    makeCustomComponent halfAdderDependency

// Full adder: uses half adder.

let fullAdderState : CanvasState =
    [
        {Id = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"; Type = Custom {Name = "half-adder"; InputLabels = ["A", 1; "B", 1]; OutputLabels = ["Cout", 1; "Sum", 1]}; Label = "half-adder"; InputPorts = [{Id = "bab6da29-c4fe-adae-c94b-ada53dc9b948"; PortNumber = Some 0; PortType = PortType.Input; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}; {Id = "6ff179d8-a703-eed2-c948-a214c0aff3a9"; PortNumber = Some 1; PortType = PortType.Input; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}]; OutputPorts = [{Id = "0b68efeb-91be-da25-c7c5-dfa1b76b105b"; PortNumber = Some 0; PortType = PortType.Output; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}; {Id = "60b550b8-37d9-a0b8-c55c-cfc0044ef147"; PortNumber = Some 1; PortType = PortType.Output; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}]; X = 187; Y = 200}
        {Id = "2c610c78-72b7-2816-fec5-a53d2fa54742"; Type = Input; Label = "A"; InputPorts = []; OutputPorts = [{Id = "51e83d48-e73b-0961-3ebe-f488e7f89326"; PortNumber = Some 0; PortType = PortType.Output; HostId = "2c610c78-72b7-2816-fec5-a53d2fa54742"}]; X = 95; Y = 171}
        {Id = "7e461ed2-53f6-c225-e184-91d24532ef33"; Type = Input; Label = "B"; InputPorts = []; OutputPorts = [{Id = "1eaa9bb4-0fa1-4fb3-0792-fe36b5d94886"; PortNumber = Some 0; PortType = PortType.Output; HostId = "7e461ed2-53f6-c225-e184-91d24532ef33"}]; X = 95; Y = 265}
        {Id = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"; Type = Custom {Name = "half-adder"; InputLabels = ["A", 1; "B", 1]; OutputLabels = ["Cout", 1; "Sum", 1]}; Label = "half-adder"; InputPorts = [{Id = "15575f10-2ec6-6033-cd3a-3ddabda8dbf3"; PortNumber = Some 0; PortType = PortType.Input; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}; {Id = "7cdb3ebb-cfa1-6892-9d35-de8f15cb87ac"; PortNumber = Some 1; PortType = PortType.Input; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}]; OutputPorts = [{Id = "093b66de-366d-40fc-a3b5-2c2d396d7fb5"; PortNumber = Some 0; PortType = PortType.Output; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}; {Id = "8cbfb956-5d53-aa0f-2546-ff32426901d1"; PortNumber = Some 1; PortType = PortType.Output; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}]; X = 323; Y = 108}
        {Id = "eddfd0f6-f06d-5190-3af9-540371a13391"; Type = Input; Label = "Cin"; InputPorts = []; OutputPorts = [{Id = "042a92cb-f8d7-3cca-7519-a0bae85fdb6b"; PortNumber = Some 0; PortType = PortType.Output; HostId = "eddfd0f6-f06d-5190-3af9-540371a13391"}]; X = 100; Y = 70}
        {Id = "d8087733-5924-4af4-e67a-2c9630173608"; Type = Or; Label = ""; InputPorts = [{Id = "f812e552-daab-73d0-cfa0-de0190279c8d"; PortNumber = Some 0; PortType = PortType.Input; HostId = "d8087733-5924-4af4-e67a-2c9630173608"}; {Id = "d25307aa-412f-d108-0f70-335b4e331f52"; PortNumber = Some 1; PortType = PortType.Input; HostId = "d8087733-5924-4af4-e67a-2c9630173608"}]; OutputPorts = [{Id = "93f988ff-3b92-1904-2dcb-17ec5e88178f"; PortNumber = Some 0; PortType = PortType.Output; HostId = "d8087733-5924-4af4-e67a-2c9630173608"}]; X = 481; Y = 196}
        {Id = "9bf01350-53a9-bb16-1f02-0df29cb60b0f"; Type = Output; Label = "Cout"; InputPorts = [{Id = "17f9e9e2-3040-65ba-9578-b4ea6a8f8b28"; PortNumber = Some 0; PortType = PortType.Input; HostId = "9bf01350-53a9-bb16-1f02-0df29cb60b0f"}]; OutputPorts = []; X = 567; Y = 206}
        {Id = "434a7ce9-942f-0939-94eb-8dd7162ad916"; Type = Output; Label = "Sum"; InputPorts = [{Id = "f7ed4610-faf3-e0cb-d985-86f6da5fa148"; PortNumber = Some 0; PortType = PortType.Input; HostId = "434a7ce9-942f-0939-94eb-8dd7162ad916"}]; OutputPorts = []; X = 567; Y = 138}
    ],
    [
        {Id = "67479e2e-468f-ebe6-7fa8-fe4e7e25efac"; Source = {Id = "51e83d48-e73b-0961-3ebe-f488e7f89326"; PortNumber = None; PortType = PortType.Output; HostId = "2c610c78-72b7-2816-fec5-a53d2fa54742"}; Target = {Id = "bab6da29-c4fe-adae-c94b-ada53dc9b948"; PortNumber = None; PortType = PortType.Input; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}; Vertices = [125.0,181.0; 156.0,181.0; 156.0,220.0; 187.0,220.0]}
        {Id = "b658d7c4-7b6a-4c9d-7cbc-19f6b25d3631"; Source = {Id = "1eaa9bb4-0fa1-4fb3-0792-fe36b5d94886"; PortNumber = None; PortType = PortType.Output; HostId = "7e461ed2-53f6-c225-e184-91d24532ef33"}; Target = {Id = "6ff179d8-a703-eed2-c948-a214c0aff3a9"; PortNumber = None; PortType = PortType.Input; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}; Vertices = [125.0,275.0; 156.0,275.0; 156.0,240.0; 187.0,240.0]}
        {Id = "c7cf8ab2-7521-b09c-e4f8-4d09f0913d4f"; Source = {Id = "0b68efeb-91be-da25-c7c5-dfa1b76b105b"; PortNumber = None; PortType = PortType.Output; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}; Target = {Id = "d25307aa-412f-d108-0f70-335b4e331f52"; PortNumber = None; PortType = PortType.Input; HostId = "d8087733-5924-4af4-e67a-2c9630173608"}; Vertices = [247.0,220.0; 364.0,220.0; 364.0,222.66666666666666; 481.0,222.66666666666666]}
        {Id = "3330546c-fe28-33c1-5f7a-fb71ba73249b"; Source = {Id = "093b66de-366d-40fc-a3b5-2c2d396d7fb5"; PortNumber = None; PortType = PortType.Output; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}; Target = {Id = "f812e552-daab-73d0-cfa0-de0190279c8d"; PortNumber = None; PortType = PortType.Input; HostId = "d8087733-5924-4af4-e67a-2c9630173608"}; Vertices = [383.0,128.0; 432.0,128.0; 432.0,209.33333333333334; 481.0,209.33333333333334]}
        {Id = "18bf5873-fe65-59d4-9c42-4b578bac5dc6"; Source = {Id = "8cbfb956-5d53-aa0f-2546-ff32426901d1"; PortNumber = None; PortType = PortType.Output; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}; Target = {Id = "f7ed4610-faf3-e0cb-d985-86f6da5fa148"; PortNumber = None; PortType = PortType.Input; HostId = "434a7ce9-942f-0939-94eb-8dd7162ad916"}; Vertices = [383.0,148.0; 567.0,148.0]}
        {Id = "026d76ef-92b3-91ae-6873-e2cc505ccb8d"; Source = {Id = "60b550b8-37d9-a0b8-c55c-cfc0044ef147"; PortNumber = None; PortType = PortType.Output; HostId = "6d47f06e-e8ac-1554-9304-e549a85eb9f3"}; Target = {Id = "7cdb3ebb-cfa1-6892-9d35-de8f15cb87ac"; PortNumber = None; PortType = PortType.Input; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}; Vertices = [247.0,240.0; 285.0,240.0; 285.0,148.0; 323.0,148.0]}
        {Id = "3c37db26-e552-d5fb-10f5-57227c4ec6f5"; Source = {Id = "93f988ff-3b92-1904-2dcb-17ec5e88178f"; PortNumber = None; PortType = PortType.Output; HostId = "d8087733-5924-4af4-e67a-2c9630173608"}; Target = {Id = "17f9e9e2-3040-65ba-9578-b4ea6a8f8b28"; PortNumber = None; PortType = PortType.Input; HostId = "9bf01350-53a9-bb16-1f02-0df29cb60b0f"}; Vertices = [521.0,216.0; 567.0,216.0]}
        {Id = "e1f27e7e-d0ae-e207-db0e-59c9a872255d"; Source = {Id = "042a92cb-f8d7-3cca-7519-a0bae85fdb6b"; PortNumber = None; PortType = PortType.Output; HostId = "eddfd0f6-f06d-5190-3af9-540371a13391"}; Target = {Id = "15575f10-2ec6-6033-cd3a-3ddabda8dbf3"; PortNumber = None; PortType = PortType.Input; HostId = "9aacd9e5-45c1-dfdd-5cc2-f464dd3983ec"}; Vertices = [130.0,80.0; 226.5,80.0; 226.5,128.0; 323.0,128.0]}
    ]

let fullAdderDependency : LoadedComponent = {
    Name = "full-adder"
    FilePath = ""
    CanvasState = fullAdderState
    InputLabels = ["Cin", 1; "B", 1; "A", 1]
    OutputLabels = ["Sum", 1; "Cout", 1]
}

let fullAdderCustom : CustomComponentType =
    makeCustomComponent fullAdderDependency

// 2 bit adder: uses full adder.

let twoBitAdderState : CanvasState =
    [
        {Id = "57604fee-25b0-9498-06f1-3a065d046515"; Type = Custom {Name = "full-adder"; InputLabels = ["Cin", 1; "B", 1; "A", 1]; OutputLabels = ["Sum", 1; "Cout", 1]}; Label = "full-adder"; InputPorts = [{Id = "6a5725f2-f181-d678-371f-7add822462ea"; PortNumber = Some 0; PortType = PortType.Input; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; {Id = "13f98ff8-2e41-2aef-cbc8-1ed636c8c8c7"; PortNumber = Some 1; PortType = PortType.Input; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; {Id = "025e36fe-41f2-7f7c-08e2-7b1bb047b798"; PortNumber = Some 2; PortType = PortType.Input; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}]; OutputPorts = [{Id = "c14929a9-b56c-2414-38e6-6fab3127203f"; PortNumber = Some 0; PortType = PortType.Output; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; {Id = "0cb60845-f6b7-4e88-53bc-18e6c06364ca"; PortNumber = Some 1; PortType = PortType.Output; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}]; X = 298; Y = 129}
        {Id = "b2842635-e4ec-3113-9130-d827d48e2875"; Type = Custom {Name = "full-adder"; InputLabels = ["Cin", 1; "B", 1; "A", 1]; OutputLabels = ["Sum", 1; "Cout", 1]}; Label = "full-adder"; InputPorts = [{Id = "ce42647b-a5db-5fa1-9a94-baaeec7416cd"; PortNumber = Some 0; PortType = PortType.Input; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; {Id = "3496a30f-e2bf-2b12-576d-cf28201850ff"; PortNumber = Some 1; PortType = PortType.Input; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; {Id = "9c0d3115-bf93-6c1c-b5bb-784cd7ca7896"; PortNumber = Some 2; PortType = PortType.Input; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}]; OutputPorts = [{Id = "f6da6a43-076e-1171-53de-6bef710bcaf8"; PortNumber = Some 0; PortType = PortType.Output; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; {Id = "04744361-bbe8-ac74-8643-fae2126ed686"; PortNumber = Some 1; PortType = PortType.Output; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}]; X = 298; Y = 298}
        {Id = "78795182-35c4-1c50-2190-6fc944a2adea"; Type = Input; Label = "Zero"; InputPorts = []; OutputPorts = [{Id = "8b0b16ed-3a4e-2ade-6e4b-6285b0d7d2c8"; PortNumber = Some 0; PortType = PortType.Output; HostId = "78795182-35c4-1c50-2190-6fc944a2adea"}]; X = 155; Y = 72}
        {Id = "69a6ad2a-af19-369f-0483-0e09e6841da3"; Type = Input; Label = "B0"; InputPorts = []; OutputPorts = [{Id = "0b74f567-0740-9667-a7e9-3042aeb7ef8f"; PortNumber = Some 0; PortType = PortType.Output; HostId = "69a6ad2a-af19-369f-0483-0e09e6841da3"}]; X = 83; Y = 150}
        {Id = "82a03f0b-ae31-b487-ed1b-335e235adeb7"; Type = Input; Label = "A1"; InputPorts = []; OutputPorts = [{Id = "e3b566b6-abf4-1446-7267-37d2ad1deed7"; PortNumber = Some 0; PortType = PortType.Output; HostId = "82a03f0b-ae31-b487-ed1b-335e235adeb7"}]; X = 84; Y = 389}
        {Id = "a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1"; Type = Input; Label = "B1"; InputPorts = []; OutputPorts = [{Id = "d59e003b-088b-cf84-0f93-83c2235b5942"; PortNumber = Some 0; PortType = PortType.Output; HostId = "a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1"}]; X = 84; Y = 324}
        {Id = "86372781-c2f4-09f2-406f-f385ee7a47a9"; Type = Input; Label = "A0"; InputPorts = []; OutputPorts = [{Id = "0d00e79c-1a98-3744-be2f-b9eb78a1cf5b"; PortNumber = Some 0; PortType = PortType.Output; HostId = "86372781-c2f4-09f2-406f-f385ee7a47a9"}]; X = 83; Y = 200}
        {Id = "dbb1f55a-edf3-bde2-4c69-43a02560e17d"; Type = Output; Label = "Sum1"; InputPorts = [{Id = "8dcbc0d6-ab44-c2e2-9e26-90ee31fa2e35"; PortNumber = Some 0; PortType = PortType.Input; HostId = "dbb1f55a-edf3-bde2-4c69-43a02560e17d"}]; OutputPorts = []; X = 514; Y = 321}
        {Id = "8f5bded5-f46d-722d-6108-03dda4236c01"; Type = Output; Label = "Sum0"; InputPorts = [{Id = "3a537f37-4c6d-3c85-48a7-040d6263bebe"; PortNumber = Some 0; PortType = PortType.Input; HostId = "8f5bded5-f46d-722d-6108-03dda4236c01"}]; OutputPorts = []; X = 514; Y = 150}
        {Id = "7d948312-376d-1d4b-cf02-90872026be16"; Type = Output; Label = "Cout"; InputPorts = [{Id = "bb868347-969a-38d2-2389-2d4ae7b63ce8"; PortNumber = Some 0; PortType = PortType.Input; HostId = "7d948312-376d-1d4b-cf02-90872026be16"}]; OutputPorts = []; X = 514; Y = 389}
    ],
    [
        {Id = "9a089fef-870a-53d8-00a0-e954963ac8e1"; Source = {Id = "0cb60845-f6b7-4e88-53bc-18e6c06364ca"; PortNumber = None; PortType = PortType.Output; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; Target = {Id = "ce42647b-a5db-5fa1-9a94-baaeec7416cd"; PortNumber = None; PortType = PortType.Input; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; Vertices = [370.0,189.0; 399.0,189.0; 399.0,255.75; 278.0,255.75; 278.0,320.5; 298.0,320.5]}
        {Id = "77ab7f29-5375-44b2-d66f-b7c21d412c7f"; Source = {Id = "0b74f567-0740-9667-a7e9-3042aeb7ef8f"; PortNumber = None; PortType = PortType.Output; HostId = "69a6ad2a-af19-369f-0483-0e09e6841da3"}; Target = {Id = "13f98ff8-2e41-2aef-cbc8-1ed636c8c8c7"; PortNumber = None; PortType = PortType.Input; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; Vertices = [113.0,160.0; 214.5,160.0; 214.5,174.0; 298.0,174.0]}
        {Id = "dba98b52-cddf-04d3-648a-fa7b915d4f87"; Source = {Id = "d59e003b-088b-cf84-0f93-83c2235b5942"; PortNumber = None; PortType = PortType.Output; HostId = "a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1"}; Target = {Id = "3496a30f-e2bf-2b12-576d-cf28201850ff"; PortNumber = None; PortType = PortType.Input; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; Vertices = [114.0,334.0; 215.0,334.0; 215.0,343.0; 298.0,343.0]}
        {Id = "c7ce0fcc-c122-4ce6-3779-0f548cbfab38"; Source = {Id = "f6da6a43-076e-1171-53de-6bef710bcaf8"; PortNumber = None; PortType = PortType.Output; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; Target = {Id = "8dcbc0d6-ab44-c2e2-9e26-90ee31fa2e35"; PortNumber = None; PortType = PortType.Input; HostId = "dbb1f55a-edf3-bde2-4c69-43a02560e17d"}; Vertices = [370.0,328.0; 446.5,328.0; 446.5,331.0; 514.0,331.0]}
        {Id = "abc884ae-3a1a-5a35-6f6f-5695d4898759"; Source = {Id = "04744361-bbe8-ac74-8643-fae2126ed686"; PortNumber = None; PortType = PortType.Output; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; Target = {Id = "bb868347-969a-38d2-2389-2d4ae7b63ce8"; PortNumber = None; PortType = PortType.Input; HostId = "7d948312-376d-1d4b-cf02-90872026be16"}; Vertices = [370.0,358.0; 446.5,358.0; 446.5,399.0; 514.0,399.0]}
        {Id = "3c7b523c-71d3-9378-176c-7c45cfc516cf"; Source = {Id = "c14929a9-b56c-2414-38e6-6fab3127203f"; PortNumber = None; PortType = PortType.Output; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; Target = {Id = "3a537f37-4c6d-3c85-48a7-040d6263bebe"; PortNumber = None; PortType = PortType.Input; HostId = "8f5bded5-f46d-722d-6108-03dda4236c01"}; Vertices = [370.0,159.0; 446.5,159.0; 446.5,160.0; 514.0,160.0]}
        {Id = "ccb35ae8-54b6-4072-6680-3cc05bb00f95"; Source = {Id = "0d00e79c-1a98-3744-be2f-b9eb78a1cf5b"; PortNumber = None; PortType = PortType.Output; HostId = "86372781-c2f4-09f2-406f-f385ee7a47a9"}; Target = {Id = "025e36fe-41f2-7f7c-08e2-7b1bb047b798"; PortNumber = None; PortType = PortType.Input; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; Vertices = [113.0,210.0; 214.5,210.0; 214.5,196.5; 298.0,196.5]}
        {Id = "11759cef-e928-32c1-e6ec-b44b198d1a2e"; Source = {Id = "e3b566b6-abf4-1446-7267-37d2ad1deed7"; PortNumber = None; PortType = PortType.Output; HostId = "82a03f0b-ae31-b487-ed1b-335e235adeb7"}; Target = {Id = "9c0d3115-bf93-6c1c-b5bb-784cd7ca7896"; PortNumber = None; PortType = PortType.Input; HostId = "b2842635-e4ec-3113-9130-d827d48e2875"}; Vertices = [114.0,399.0; 215.0,399.0; 215.0,365.5; 298.0,365.5]}
        {Id = "2ae1f296-5dfb-986e-2c4c-3e3435d750b9"; Source = {Id = "8b0b16ed-3a4e-2ade-6e4b-6285b0d7d2c8"; PortNumber = None; PortType = PortType.Output; HostId = "78795182-35c4-1c50-2190-6fc944a2adea"}; Target = {Id = "6a5725f2-f181-d678-371f-7add822462ea"; PortNumber = None; PortType = PortType.Input; HostId = "57604fee-25b0-9498-06f1-3a065d046515"}; Vertices = [185.0,82.0; 250.5,82.0; 250.5,151.5; 298.0,151.5]}
    ]

let twoBitAdderDependency : LoadedComponent = {
    Name = "2-bit-adder"
    FilePath = ""
    CanvasState = twoBitAdderState
    InputLabels = ["A0", 1; "B1", 1; "A1", 1; "B0", 1; "Zero", 1]
    OutputLabels = ["Cout", 1; "Sum0", 1; "Sum1", 1]
}

let twoBitAdderCustom : CustomComponentType =
    makeCustomComponent twoBitAdderDependency
