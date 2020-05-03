module SimulatorTests

open DiagramTypes
open CanvasStates

type private SimulatorTestCaseInput = CanvasState * LoadedComponent list * (ComponentId * Bit) list
type private SimulatorTestCaseOutput = Result<((ComponentId * ComponentLabel) * Bit) list, SimulationError>
type private SimulatorTestCase = string * SimulatorTestCaseInput * SimulatorTestCaseOutput

let private makeError msg deps comps conns =
    Error {
        Msg = msg
        InDependency = deps
        ComponentsAffected = comps |> List.map ComponentId
        ConnectionsAffected = conns |> List.map ConnectionId
    }

// The input to a test case is formed by:
// - a CanvasState,
// - a list of loaded dependencies,
// - a list of input values that will be applied to the simulation graph.

// The dependency list and the inputs do not matter since the test has to fail
// in the earlier checks.
let private testCasesSimulatorPortError : SimulatorTestCase list = [
    "Unconnected input node",
    (state1, [], []),
    makeError
        "All ports must have at least one connection."
        None
        ["input-node0"]
        []

    "Two unconnected input nodes",
    (state2, [], []),
    makeError
        "All ports must have at least one connection."
        None
        ["input-node0"]
        []

    "Two inputs and one output",
    (state5, [], []),
    makeError
        "Input port receives 2 connections. An input port should receive precisely one connection."
        None
        ["output-node0"]
        []

    "Two inputs, one And, one output, with extra connection input to output",
    (state7, [], []),
    makeError
        "Input port receives 2 connections. An input port should receive precisely one connection."
        None
        ["output"]
        []

    "Two inputs, one And, one output, with extra connections inputs to and",
    (state8, [], []),
    makeError
        "Input port receives 2 connections. An input port should receive precisely one connection."
        None
        ["and"]
        []

    "Mux2 with only two connected ports",
    (state9, [], []),
    makeError
        "All ports must have at least one connection."
        None
        ["mux"]
        []
]

let private testCasesSimulatorDuplicatIOError : SimulatorTestCase list = [
    "Simple circuit with duplicated output label",
    (state14, [], []),
    makeError
        "Two Output components cannot have the same label: output-duplicate-label"
        None
        ["output-node0"; "output-node1"]
        []

    "Simple And circuit with duplicated input label",
    (state15, [], []),
    makeError
        "Two Input components cannot have the same label: input-duplicate-label"
        None
        ["top-input"; "bottom-input"]
        []
]

let private testCasesSimulatorCycleError : SimulatorTestCase list = [
    "Complex diagram with three Ands and two cycles",
    (state10, [], []),
    makeError
        "Cycle detected in combinatorial logic"
        None
        ["and2"; "and0"]
        ["conn5"; "conn4"]

    "Complex diagram with three Ands and one long cycle",
    (state11, [], []),
    makeError
        "Cycle detected in combinatorial logic"
        None
        ["and1"; "and2"; "and0"]
        ["conn1"; "conn5"; "conn3"]
]

// In the next tests, we have no dependencies.
// The inputs are set since the simulation graph should be fine.
let private testCasesSimulatorOkNoDependencies : SimulatorTestCase list = [
    "Simple circuit with one input and one output (zero)",
    (state3, [], [ComponentId "input-node0", Zero]),
    Ok [(ComponentId "output-node0", ComponentLabel "output-node0-label"), Zero]

    "Simple circuit with one input and one output (one)",
    (state3, [], [ComponentId "input-node0", One]),
    Ok [(ComponentId "output-node0", ComponentLabel "output-node0-label"), One]

    "Simple circuit with one input connected to two outputs (Zero)",
    (state4, [], [ComponentId "input-node0", Zero]),
    Ok [
        (ComponentId "output-node0", ComponentLabel "output-node0-label"), Zero
        (ComponentId "output-node1", ComponentLabel "output-node1-label"), Zero
    ]

    "Simple circuit with one input connected to two outputs (One)",
    (state4, [], [ComponentId "input-node0", One]),
    Ok [
        (ComponentId "output-node0", ComponentLabel "output-node0-label"), One
        (ComponentId "output-node1", ComponentLabel "output-node1-label"), One
    ]

    "Two inputs; one And; one output (Zero, Zero)",
    (state6, [], [ComponentId "top-input", Zero; ComponentId "bottom-input", Zero]),
    Ok [(ComponentId "output", ComponentLabel "output-node0-label"), Zero]

    "Two inputs; one And; one output (Zero, One)",
    (state6, [], [ComponentId "top-input", Zero; ComponentId "bottom-input", One]),
    Ok [(ComponentId "output", ComponentLabel "output-node0-label"), Zero]

    "Two inputs; one And; one output (One, Zero)",
    (state6, [], [ComponentId "top-input", One; ComponentId "bottom-input", Zero]),
    Ok [(ComponentId "output", ComponentLabel "output-node0-label"), Zero]

    "Two inputs; one And; one output (One, One)",
    (state6, [], [ComponentId "top-input", One; ComponentId "bottom-input", One]),
    Ok [(ComponentId "output", ComponentLabel "output-node0-label"), One]

    "Weird diagram with a series of and gates (Zero)",
    (state12, [], [ComponentId "input", Zero]),
    Ok [(ComponentId "output", ComponentLabel "output"), Zero]

    "Weird diagram with a series of and gates (One)",
    (state12, [], [ComponentId "input", One]),
    Ok [(ComponentId "output", ComponentLabel "output"), One]

    "One bit adder (Zero, Zero)",
    (state13, [], [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", Zero;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", Zero;
    ]),
    Ok [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), Zero
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
    ]

    "One bit adder (Zero, One)",
    (state13, [], [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", Zero;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", One;
    ]),
    Ok [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), One
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
    ]

    "One bit adder (One, Zero)",
    (state13, [], [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", One;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", Zero;
    ]),
    Ok [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), One
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
    ]

    "One bit adder (One, One)",
    (state13, [], [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", One;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", One;
    ]),
    Ok [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), Zero
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), One
    ]
]

// TODO add cases with dependencies.

let testCasesSimulator =
    testCasesSimulatorPortError @
    testCasesSimulatorCycleError @
    testCasesSimulatorDuplicatIOError @
    testCasesSimulatorOkNoDependencies
