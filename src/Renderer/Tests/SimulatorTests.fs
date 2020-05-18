module SimulatorTests

open DiagramTypes
open SimulatorTypes
open CanvasStates
open CanvasStatesWithBuses
open Simulator

/// Tuple with: (diagramName, state, loadedComponents, inputs).
type private SimulatorTestCaseInput = string * CanvasState * LoadedComponent list * (ComponentId * WireData) list
type private SimulatorTestCaseOutput = Result<(SimulationIO * WireData) list, SimulationError>
type private SimulatorTestCase = string * SimulatorTestCaseInput * SimulatorTestCaseOutput

let private makeError msg deps comps conns =
    Error {
        Msg = msg
        InDependency = deps
        ComponentsAffected = comps |> List.map ComponentId
        ConnectionsAffected = conns |> List.map ConnectionId
    }

/// Auto generate all the testcases for a CanvasState (i.e. a full thruth
/// table). The thruth table considers ALL inputs to be single bit inputs,
/// please do not use this function if not all inputs are like that. 
let private createAllTestCases
        (title : string)
        (diagramName : string)
        (state : CanvasState)
        (dependencies : LoadedComponent list)
        (inputLabels : ComponentId list)
        (expectedResults : ((SimulationIO * WireData) list) list)
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
           [(ComponentId "output-node0", ComponentLabel "output-node0-label", 1), [Zero]]
           [(ComponentId "output-node0", ComponentLabel "output-node0-label", 1), [One]]
        ]
    @
    createAllTestCases
        "Simple circuit with one input connected to two outputs"
        "main" state4 [] [ComponentId "input-node0"]
        [
            [
                (ComponentId "output-node0", ComponentLabel "output-node0-label", 1), [Zero]
                (ComponentId "output-node1", ComponentLabel "output-node1-label", 1), [Zero]
            ]
            [
                (ComponentId "output-node0", ComponentLabel "output-node0-label", 1), [One]
                (ComponentId "output-node1", ComponentLabel "output-node1-label", 1), [One]
            ]
        ]
    @
    createAllTestCases
        "Two inputs; one And; one output"
        "main" state6 [] [ComponentId "top-input"; ComponentId "bottom-input"]
        [
            [(ComponentId "output", ComponentLabel "output-node0-label", 1), [Zero]]
            [(ComponentId "output", ComponentLabel "output-node0-label", 1), [Zero]]
            [(ComponentId "output", ComponentLabel "output-node0-label", 1), [Zero]]
            [(ComponentId "output", ComponentLabel "output-node0-label", 1), [One]]
        ]
    @
    createAllTestCases
        "Weird diagram with a series of and gates"
        "main" state12 [] [ComponentId "input"]
        [
            [(ComponentId "output", ComponentLabel "output", 1), [Zero]]
            [(ComponentId "output", ComponentLabel "output", 1), [One]]
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
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum", 1), [Zero]
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry", 1), [Zero]
            ]
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum", 1), [One]
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry", 1), [Zero]
            ]
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum", 1), [One]
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry", 1), [Zero]
            ]
            [
                (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum", 1), [Zero]
                (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry", 1), [One]
            ]
        ]

let testCasesSimulatorDependencyError : SimulatorTestCase list =
    createAllTestCases
        "Broken unused dependency." // Since the dependency is unused the test should pass.
        "main" state3 [state1Dependency] [ComponentId "input-node0"]
        [
           [(ComponentId "output-node0", ComponentLabel "output-node0-label", 1), [Zero]]
           [(ComponentId "output-node0", ComponentLabel "output-node0-label", 1), [One]]
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
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [Zero]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [Zero]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [Zero]
    ]
let private one = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [Zero]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [One]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [Zero]
    ]
let private two = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [One]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [Zero]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [Zero]
    ]
let private three = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [One]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [One]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [Zero]
    ]
let private four = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [Zero]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [Zero]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [One]
    ]
let private five = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [Zero]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [One]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [One]
    ]
let private six = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [One]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [Zero]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [One]
    ]
let private seven = [
        (ComponentId "dbb1f55a-edf3-bde2-4c69-43a02560e17d", ComponentLabel "Sum1", 1), [One]
        (ComponentId "8f5bded5-f46d-722d-6108-03dda4236c01", ComponentLabel "Sum0", 1), [One]
        (ComponentId "7d948312-376d-1d4b-cf02-90872026be16", ComponentLabel "Cout", 1), [One]
    ]

let testCasesSimulatorOkWithDependencies : SimulatorTestCase list =
    createAllTestCases
        "Simple input-output dependency" "main"
        state16 [state3Dependency] [ComponentId "outer-input-node0"]
        [
            [(ComponentId "outer-output-node0", ComponentLabel "outer-output-node0-label", 1), [Zero]]
            [(ComponentId "outer-output-node0", ComponentLabel "outer-output-node0-label", 1), [One]]
        ]
    @
    createAllTestCases
        "Nested input-output dependency" "main"
        state17 [state16Dependency; state3Dependency] [ComponentId "outer-outer-input-node0"]
        [
            [(ComponentId "outer-outer-output-node0", ComponentLabel "outer-outer-output-node0-label", 1), [Zero]]
            [(ComponentId "outer-outer-output-node0", ComponentLabel "outer-outer-output-node0-label", 1), [One]]
        ]
    @
    createAllTestCases
        "Doubly nested input-output dependency" "main"
        state18 [state17Dependency; state16Dependency; state3Dependency] [ComponentId "outer-outer-outer-input-node0"]
        [
            [(ComponentId "outer-outer-outer-output-node0", ComponentLabel "outer-outer-outer-output-node0-label", 1), [Zero]]
            [(ComponentId "outer-outer-outer-output-node0", ComponentLabel "outer-outer-outer-output-node0-label", 1), [One]]
        ]
    @
    createAllTestCases
        "2 bit adder" "2-bit-adder"
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

let private testCasesSimulatorBusesError : SimulatorTestCase list = [
    "Two inputs make a bus2, then Push input a to bus, then try to split into 2 single bits (fail)",
    ("main", stateBus11, [], []),
    makeError "Wrong wire width. Expecting 1 but got 2." None [] ["conn"]

    "A 4 bit input connected to a 3 bit output",
    ("main", stateBus14, [], []),
    makeError "Wrong wire width. Expecting 3 but got 4." None [] ["conn"]

    "A 3 bit input connected to a 4 bit output",
    ("main", stateBus15, [], []),
    makeError "Wrong wire width. Expecting 4 but got 3." None [] ["conn"]
]

let private testCasesSimulatorOkWithBuses : SimulatorTestCase list =
    createAllTestCases
        "Two inputs, packed into a bus, unpacked into two outputs" "main"
        stateBus10 [] [
            ComponentId "a91be585-2d3b-d872-be0f-b416c8eb03d2" // a
            ComponentId "9985ebc6-1cd5-8863-1341-1d543d236d38" // b
        ]
        (makeAllBitCombinations [
            (ComponentId "8a9392fc-493b-7e96-72ec-b6f5f11ded8a", ComponentLabel "a-out", 1)
            (ComponentId "dfcf6cff-fbac-e54f-7a9d-7059d17e3a0b", ComponentLabel "b-out", 1)
        ])
    @
    createAllTestCases
        "Four inputs, packed into a bus, unpacked into four outputs" "main"
        stateBus12 [] [
            ComponentId "76de964a-124b-5c16-6de1-6158626344ac" // a
            ComponentId "a91be585-2d3b-d872-be0f-b416c8eb03d2" // b
            ComponentId "9985ebc6-1cd5-8863-1341-1d543d236d38" // c
            ComponentId "9824ceb8-e999-8e48-9a56-7a4349e495b1" // d
        ]
        (makeAllBitCombinations [
            (ComponentId "59b45f9c-192c-98ce-da25-a94db45a5790", ComponentLabel "a-out", 1)
            (ComponentId "8a9392fc-493b-7e96-72ec-b6f5f11ded8a", ComponentLabel "b-out", 1)
            (ComponentId "dfcf6cff-fbac-e54f-7a9d-7059d17e3a0b", ComponentLabel "c-out", 1)
            (ComponentId "214620f0-51f6-59fe-1558-ed47fd2c680a", ComponentLabel "d-out", 1)
        ])
    @
    [
        "A 4 bit input connected to a four bit output (1)",
        ("main", stateBus13, [], [
            ComponentId "9bcba47e-deae-0b3f-2079-a1b124526b00", [One; One; One; One]
        ]),
        [
            (ComponentId "ad2ef0c3-537e-9d2e-0064-ac6b952e4b97", ComponentLabel "b", 4), [One; One; One; One]
        ] |> Ok

        "A 4 bit input connected to a four bit output (2)",
        ("main", stateBus13, [], [
            ComponentId "9bcba47e-deae-0b3f-2079-a1b124526b00", [Zero; One; Zero; One]
        ]),
        [
            (ComponentId "ad2ef0c3-537e-9d2e-0064-ac6b952e4b97", ComponentLabel "b", 4), [Zero; One; Zero; One]
        ] |> Ok

        "A 2 bit input split into 2 single bit outputs (1)",
        ("main", stateBus16, [], [
            ComponentId "c6f000db-310f-d8ad-ff5e-938d7c2aaa7c", [One; Zero]
        ]),
        [
            (ComponentId "60e2df66-bb8c-53f1-832d-e154c30cf9dd", ComponentLabel "b", 1), [One]
            (ComponentId "85e19389-c087-8b30-6c0a-02f7cc753695", ComponentLabel "c", 1), [Zero]
        ] |> Ok

        "A 2 bit input split into 2 single bit outputs (2)",
        ("main", stateBus16, [], [
            ComponentId "c6f000db-310f-d8ad-ff5e-938d7c2aaa7c", [Zero; One]
        ]),
        [
            (ComponentId "60e2df66-bb8c-53f1-832d-e154c30cf9dd", ComponentLabel "b", 1), [Zero]
            (ComponentId "85e19389-c087-8b30-6c0a-02f7cc753695", ComponentLabel "c", 1), [One]
        ] |> Ok

        "3 bit input merged with 4 bit input, then split in the same way",
        ("main", stateBus17, [], [
            ComponentId "6bcdc74a-9d71-3304-537d-1a17f02924eb", [One; Zero; One] // 3 bits.
            ComponentId "97c4b56d-4f8c-2b00-fb61-a08cdd01dd76", [Zero; One; Zero; One] // 4 bits.
        ]),
        [
            (ComponentId "1a6e1bb4-cfe0-77f9-a207-f409168ef210", ComponentLabel "out3", 3), [One; Zero; One]
            (ComponentId "d2676492-2302-24d9-52eb-6e69e7971339", ComponentLabel "out4", 4), [Zero; One; Zero; One]
        ] |> Ok
    ]

let testCasesSimulator =
    testCasesSimulatorPortError @
    testCasesSimulatorCycleError @
    testCasesSimulatorDuplicatIOError @
    testCasesSimulatorOkNoDependencies @
    testCasesSimulatorDependencyError @
    testCasesSimulatorOkWithDependencies @
    testCasesSimulatorBusesError @
    testCasesSimulatorOkWithBuses
