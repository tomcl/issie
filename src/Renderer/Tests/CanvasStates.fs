module CanvasStates

open DiagramTypes

/// Just a single input node. No conections.
let state1 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
    ],
    []

/// Two unconnected input nodes a single input node. No conections.
let state2 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "input-node1"; Type = Input; Label = "input-label"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node1"}]; X = 169; Y = 175}
    ],
    []

/// Simple circuit with one input connected to one output.
let state3 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
    ]

/// Simple circuit with one input connected to two outputs.
let state4 : CanvasState =
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
let state5 : CanvasState =
    [
        {Id = "input-node0"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node0"}]; X = 169; Y = 175}
        {Id = "input-node1"; Type = Input; Label = "input"; InputPorts = []; OutputPorts = [{Id = "out-port0"; PortNumber = Some 0; PortType = PortType.Output; HostId = "input-node1"}]; X = 169; Y = 175}
        {Id = "output-node0"; Type = Output; Label = "output"; InputPorts = [{Id = "inp-port0"; PortNumber = Some 0; PortType = PortType.Input; HostId = "output-node0"}]; OutputPorts = []; X = 364; Y = 175}
    ],
    [
        {Id = "conn0"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node0"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
        {Id = "conn1"; Source = {Id = "out-port0"; PortNumber = None; PortType = PortType.Output; HostId = "input-node1"}; Target = {Id = "inp-port0"; PortNumber = None; PortType = PortType.Input; HostId = "output-node0"}}
    ]

/// Two inputs; one And; one output.
let state6 : CanvasState =
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

/// Two inputs; one And; one output; with extra connection input to output.
let state7 : CanvasState =
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

/// Two inputs; one And; one output; with extra connections inputs to and.
let state8 : CanvasState =
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
let state9 : CanvasState =
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
        {Id = "conn0"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
        {Id = "conn1"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}}
        {Id = "conn2"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}}
        {Id = "conn3"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}}
        {Id = "conn4"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}}
        {Id = "conn5"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}}
        {Id = "conn6"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}}
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
        {Id = "conn0"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
        {Id = "conn1"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}}
        {Id = "conn2"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}}
        {Id = "conn3"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}}
        {Id = "conn4"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}}
        {Id = "conn5"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}}
        {Id = "conn6"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}}
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
        {Id = "conn0"; Source = {Id = "41d15996-0838-6a41-e974-ee330fd13607"; PortNumber = None; PortType = PortType.Output; HostId = "and1"}; Target = {Id = "25886b76-feee-6892-6637-cc2378fe6094"; PortNumber = None; PortType = PortType.Input; HostId = "output"}}
        {Id = "conn1"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "5c4d25e1-c067-79d3-adc7-7e141f5a7905"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}}
        {Id = "conn2"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "9697ec8d-b6d3-9f2a-aaaf-0907fd087e05"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}}
        {Id = "conn3"; Source = {Id = "edb944e4-1fe8-e9e2-eaf2-e5278277b29d"; PortNumber = None; PortType = PortType.Output; HostId = "and0"}; Target = {Id = "23f32198-bf52-0456-6be2-1fbe92b36bbf"; PortNumber = None; PortType = PortType.Input; HostId = "and1"}}
        {Id = "conn4"; Source = {Id = "14344e8e-9448-4933-9004-85756859c64d"; PortNumber = None; PortType = PortType.Output; HostId = "input"}; Target = {Id = "aefefef8-61ea-a3cf-49f9-11b858342504"; PortNumber = None; PortType = PortType.Input; HostId = "and2"}}
        {Id = "conn5"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "8e8b684e-664f-5758-15c9-79c84c8fc81a"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}}
        {Id = "conn6"; Source = {Id = "26de501f-4e60-9f61-fce0-6e5fdb131f87"; PortNumber = None; PortType = PortType.Output; HostId = "and2"}; Target = {Id = "d9b40581-2587-506d-1868-8201d3802913"; PortNumber = None; PortType = PortType.Input; HostId = "and0"}}
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
        {Id="79df6e40-7ad2-b1d2-cb98-ea5d649ef1cc";Source={Id="7cb01ee2-49d4-00b5-7523-14e4de3c5489";PortNumber=None;PortType=PortType.Output;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"};Target={Id="14b23a60-21ae-374f-d083-41aa2510eeab";PortNumber=None;PortType=PortType.Input;HostId="9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9"}}
        {Id="6b066c26-750b-6eb3-2516-bc122899f846";Source={Id="1ee439f3-8d23-c049-ff9e-cd8f1b4d3d9d";PortNumber=None;PortType=PortType.Output;HostId="170e69f4-b3d7-d9e0-9f1d-6a564ba62062"};Target={Id="9fdb22f6-f8eb-829f-68b3-87b9c8577299";PortNumber=None;PortType=PortType.Input;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}}
        {Id="5f7c6896-8fbd-bfaf-ee0f-a677f7804283";Source={Id="1ee439f3-8d23-c049-ff9e-cd8f1b4d3d9d";PortNumber=None;PortType=PortType.Output;HostId="170e69f4-b3d7-d9e0-9f1d-6a564ba62062"};Target={Id="9826a9b5-cd62-51ec-6368-93c4967745f9";PortNumber=None;PortType=PortType.Input;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}}
        {Id="8ed22d89-b542-a182-fef9-1a67170da60e";Source={Id="336aab97-a7bd-9a37-9062-56753b57c268";PortNumber=None;PortType=PortType.Output;HostId="2953603d-44e4-5c1f-3fb1-698f7863b6b5"};Target={Id="1b2c73d1-b38e-64e6-5b19-e4e7f690a692";PortNumber=None;PortType=PortType.Input;HostId="253e21f0-b062-4858-c315-5a5315cadf45"}}
        {Id="0cd3f109-22ee-3543-7247-db2b4ffe5fc8";Source={Id="336aab97-a7bd-9a37-9062-56753b57c268";PortNumber=None;PortType=PortType.Output;HostId="2953603d-44e4-5c1f-3fb1-698f7863b6b5"};Target={Id="6a468f2c-4db4-d3ae-402f-a617885884e7";PortNumber=None;PortType=PortType.Input;HostId="6b7bac71-eec5-4979-834a-c1bfe40d77b9"}}
        {Id="3230b338-cdd4-7632-e441-9d2f50df013a";Source={Id="95a03834-e494-92b2-b8d4-a148e3c0763b";PortNumber=None;PortType=PortType.Output;HostId="253e21f0-b062-4858-c315-5a5315cadf45"};Target={Id="b9d457f7-dcef-89a1-aa76-b9ba7f9ca3f4";PortNumber=None;PortType=PortType.Input;HostId="94da6dd7-a263-a3ec-ec76-bfa07b0b0f34"}}
    ]