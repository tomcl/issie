module AnalyserTests

open DiagramTypes
open Simulator
open CanvasStates

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

    "Complex diagram with three Ands and two cycles", state10,
    None

    "Complex diagram with three Ands and one long cycle", state11,
    None

    "Complex diagram with three Ands and no cycle", state12,
    None

    "One bit adder", state13,
    None
]

let private getTestInputs state =
    let _, connections = state
    buildSimulationGraph state, connections

let testCasesAnalyseGraph = [
    "One input and one output", getTestInputs state3,
    None

    "Complex diagram with three Ands and two cycles", getTestInputs state10,
    makeError
        "Cycle detected in combinatorial logic"
        ["and2"; "and0"]
        ["conn5"; "conn4"]

    "Complex diagram with three Ands and one long cycle", getTestInputs state11,
    makeError
        "Cycle detected in combinatorial logic"
        ["and1"; "and2"; "and0"]
        ["conn1"; "conn5"; "conn3"]

    "Complex diagram with three Ands and no cycle", getTestInputs state12,
    None

    "One bit adder", getTestInputs state13,
    None
]
