module SimulatorTests

open DiagramTypes
open SimulatorTypes
open CanvasStates
open Simulator

type private SimulatorTestCaseInput = string * CanvasState * LoadedComponent list * (ComponentId * Bit) list
type private SimulatorTestCaseOutput = Result<((ComponentId * ComponentLabel) * Bit) list, SimulationError>
type private SimulatorTestCase = string * SimulatorTestCaseInput * SimulatorTestCaseOutput

let private makeError msg deps comps conns =
    Error {
        Msg = msg
        InDependency = deps
        ComponentsAffected = comps |> List.map ComponentId
        ConnectionsAffected = conns |> List.map ConnectionId
    }

/// Auto generate all the testcases for a CanvasState (i.e. a full thruth table).
let private createAllTestCases
        (title : string)
        (diagramName : string)
        (state : CanvasState)
        (dependencies : LoadedComponent list)
        (inputLabels : ComponentId list)
        (expectedResults : (((ComponentId * ComponentLabel) * Bit) list) list)
        : SimulatorTestCase list =
    let allInputCombinations = makeAllBitCombinations inputLabels
    assert(List.length allInputCombinations = List.length expectedResults)
    (allInputCombinations, expectedResults)
    ||> List.map2 (fun inputs outputs ->
        sprintf "%s: %A" title inputs,
        (diagramName, state, dependencies, inputs),
        Ok outputs
    )

// The input to a test case is formed by:
// - a CanvasState,
// - a list of loaded dependencies,
// - a list of input values that will be applied to the simulation graph.

// The dependency list and the inputs do not matter since the test has to fail
// in the earlier checks.
let private testCasesSimulatorPortError : SimulatorTestCase list = [
    "Unconnected input node",
    ("", state1, [], []),
    makeError
        "All ports must have at least one connection."
        None
        ["input-node0"]
        []

    "Two unconnected input nodes",
    ("", state2, [], []),
    makeError
        "All ports must have at least one connection."
        None
        ["input-node0"]
        []

    "Two inputs and one output",
    ("", state5, [], []),
    makeError
        "Input port receives 2 connections. Every net must have a single driving input."
        None
        ["output-node0"]
        []

    "Two inputs, one And, one output, with extra connection input to output",
    ("", state7, [], []),
    makeError
        "Input port receives 2 connections. Every net must have a single driving input."
        None
        ["output"]
        []

    "Two inputs, one And, one output, with extra connections inputs to and",
    ("", state8, [], []),
    makeError
        "Input port receives 2 connections. Every net must have a single driving input."
        None
        ["and"]
        []

    "Mux2 with only two connected ports",
    ("", state9, [], []),
    makeError
        "All ports must have at least one connection."
        None
        ["mux"]
        []
]

let private testCasesSimulatorDuplicatIOError : SimulatorTestCase list = [
    "Simple circuit with duplicated output label",
    ("", state14, [], []),
    makeError
        "Two Output components cannot have the same label: output-duplicate-label"
        None
        ["output-node0"; "output-node1"]
        []

    "Simple And circuit with duplicated input label",
    ("", state15, [], []),
    makeError
        "Two Input components cannot have the same label: input-duplicate-label"
        None
        ["top-input"; "bottom-input"]
        []
]

let private testCasesSimulatorCycleError : SimulatorTestCase list = [
    "Complex diagram with three Ands and two cycles",
    ("", state10, [], []),
    makeError
        "Cycle detected in combinatorial logic"
        None
        ["and2"; "and0"]
        ["conn5"; "conn4"]

    "Complex diagram with three Ands and one long cycle",
    ("", state11, [], []),
    makeError
        "Cycle detected in combinatorial logic"
        None
        ["and1"; "and2"; "and0"]
        ["conn1"; "conn5"; "conn3"]
]

// In the next tests, we have no dependencies.
// The inputs are set since the simulation graph should be fine.
let private testCasesSimulatorOkNoDependencies : SimulatorTestCase list =
    createAllTestCases
        "Simple circuit with one input and one output"
        "main" state3 [] [ComponentId "input-node0"]
        [
           [(ComponentId "output-node0", ComponentLabel "output-node0-label"), Zero]
           [(ComponentId "output-node0", ComponentLabel "output-node0-label"), One]
        ]
    @
    createAllTestCases
        "Simple circuit with one input connected to two outputs"
        "main" state4 [] [ComponentId "input-node0"]
        [
            [
                (ComponentId "output-node0", ComponentLabel "output-node0-label"), Zero
                (ComponentId "output-node1", ComponentLabel "output-node1-label"), Zero
            ]
            [
                (ComponentId "output-node0", ComponentLabel "output-node0-label"), One
                (ComponentId "output-node1", ComponentLabel "output-node1-label"), One
            ]
        ]
    @
    createAllTestCases
        "Two inputs; one And; one output"
        "main" state6 [] [ComponentId "top-input"; ComponentId "bottom-input"]
        [
            [(ComponentId "output", ComponentLabel "output-node0-label"), Zero]
            [(ComponentId "output", ComponentLabel "output-node0-label"), Zero]
            [(ComponentId "output", ComponentLabel "output-node0-label"), Zero]
            [(ComponentId "output", ComponentLabel "output-node0-label"), One]
        ]
    @
    createAllTestCases
        "Weird diagram with a series of and gates"
        "main" state12 [] [ComponentId "input"]
        [
            [(ComponentId "output", ComponentLabel "output"), Zero]
            [(ComponentId "output", ComponentLabel "output"), One]
        ]
    @
    createAllTestCases
        "One bit adder (Zero, Zero)"
        "main" state13 [] [
            ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5"
            ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062"
        ]
        [
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), Zero
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
            ]
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), One
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
            ]
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), One
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
            ]
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), Zero
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), One
            ]
        ]

let testCasesSimulatorDependencyError : SimulatorTestCase list =
    createAllTestCases
        "Broken unused dependency." // Since the dependency is unused the test should pass.
        "main" state3 [state1Dependency] [ComponentId "input-node0"]
        [
           [(ComponentId "output-node0", ComponentLabel "output-node0-label"), Zero]
           [(ComponentId "output-node0", ComponentLabel "output-node0-label"), One]
        ]
    @
    [
        // Broken dependencies.

        "Input connected to broken depdendency.",
        ("main", state19, [state1Dependency], []),
        makeError
            "All ports must have at least one connection."
            (Some "broken-one-input") [] []

        // Dependency cycle.

        "Component using itself.",
        (state16Dependency.Name, state20, [state16Dependency], []),
        makeError
            (sprintf "Found a cycle in dependencies: %s --> %s." state16Dependency.Name state16Dependency.Name)
            None [] []

        "Long cycle starting at root.",
        (state23Dependency.Name, state23, [state21Dependency; state22Dependency], []),
        makeError
            (sprintf "Found a cycle in dependencies: %s --> %s --> %s --> %s." state23Dependency.Name state21Dependency.Name state22Dependency.Name state23Dependency.Name)
            None [] []

        "Long cycle.",
        ("main", state24, [state21Dependency; state22Dependency; state23Dependency], []),
        makeError
            (sprintf "Found a cycle in dependencies: %s --> %s --> %s --> %s." state23Dependency.Name state21Dependency.Name state22Dependency.Name state23Dependency.Name)
            None [] []

        // Missing dependencies.

        "2 bit full adder missing dependencies",
        ("2-bit-adder", twoBitAdderState, [], []),
        makeError
            "Unresolved dependency: \"full-adder\""
            (Some "2-bit-adder") [] []

        "2 bit full adder missing half adder dependency",
        ("2-bit-adder", twoBitAdderState, [fullAdderDependency], []),
        makeError
            "Unresolved dependency: \"half-adder\""
            (Some "full-adder") [] []

        "2 bit full adder missing full adder dependency",
        ("2-bit-adder", twoBitAdderState, [halfAdderDependency], []),
        makeError
            "Unresolved dependency: \"full-adder\""
            (Some "2-bit-adder") [] []
    ]

// Outputs fot the 2bit adder test.

let private zero = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), Zero
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), Zero
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), Zero
    ]
let private one = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), Zero
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), One
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), Zero
    ]
let private two = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), One
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), Zero
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), Zero
    ]
let private three = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), One
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), One
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), Zero
    ]
let private four = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), Zero
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), Zero
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), One
    ]
let private five = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), Zero
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), One
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), One
    ]
let private six = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), One
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), Zero
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), One
    ]
let private seven = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1"), One
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0"), One
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout"), One
    ]

let testCasesSimulatorOkWithDependencies : SimulatorTestCase list =
    createAllTestCases
        "main" "Simple input-output dependency"
        state16 [state3Dependency] [ComponentId "outer-input-node0"]
        [
            [(ComponentId "outer-output-node0", ComponentLabel "outer-output-node0-label"), Zero]
            [(ComponentId "outer-output-node0", ComponentLabel "outer-output-node0-label"), One]
        ]
    @
    createAllTestCases
        "main" "Nested input-output dependency"
        state17 [state16Dependency; state3Dependency] [ComponentId "outer-outer-input-node0"]
        [
            [(ComponentId "outer-outer-output-node0", ComponentLabel "outer-outer-output-node0-label"), Zero]
            [(ComponentId "outer-outer-output-node0", ComponentLabel "outer-outer-output-node0-label"), One]
        ]
    @
    createAllTestCases
        "main" "Doubly nested input-output dependency"
        state18 [state17Dependency; state16Dependency; state3Dependency] [ComponentId "outer-outer-outer-input-node0"]
        [
            [(ComponentId "outer-outer-outer-output-node0", ComponentLabel "outer-outer-outer-output-node0-label"), Zero]
            [(ComponentId "outer-outer-outer-output-node0", ComponentLabel "outer-outer-outer-output-node0-label"), One]
        ]
    @
    createAllTestCases
        "2-bit-adder" "2 bit adder"
        twoBitAdderState [fullAdderDependency; halfAdderDependency] [
            ComponentId "78795182-35c4-1c50-2190-6fc944a2adea" // Cin
            ComponentId "a63fe5a2-9f4d-e70f-131b-ed35d3f3a9e1" // B1
            ComponentId "69a6ad2a-af19-369f-0483-0e09e6841da3" // B0
            ComponentId "82a03f0b-ae31-b487-ed1b-335e235adeb7" // A1
            ComponentId "86372781-c2f4-09f2-406f-f385ee7a47a9" // A0
        ]
        [
            zero;one;two;three;
            one;two;three;four;
            two;three;four;five;
            three;four;five;six;
            one;two;three;four;
            two;three;four;five;
            three;four;five;six;
            four;five;six;seven
        ]

let testCasesSimulator =
    testCasesSimulatorPortError @
    testCasesSimulatorCycleError @
    testCasesSimulatorDuplicatIOError @
    testCasesSimulatorOkNoDependencies @
    testCasesSimulatorDependencyError @
    testCasesSimulatorOkWithDependencies
