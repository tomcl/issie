module SimulatorSyncTests

open DiagramTypes
open SimulatorTypes
open CanvasStatesSync

/// Tuple with: (diagramName, state, loadedComponents, number of clock ticks, inputss).
/// Note that the inputss is a list of inputs. I.e. inputss provides an inputs
/// for every time step. The first set of inputs will be fed before the first
/// clock tick, then second after the second clock tick and so on. If no inputs
/// are provided for a certain iteration, none will fed.
type private TestCaseInput = string * CanvasState * LoadedComponent list * int * ((ComponentId * WireData) list) list
type private IterationOutput = (SimulationIO * WireData) list // Output after every clock tick.
type private TestCaseOutput = Result<IterationOutput list, SimulationError>
type private TestCase = string * TestCaseInput * TestCaseOutput

let testCasesSimulatorSync : TestCase list = [
    "Simple D-flip-flop, one input, one output (zero)",
    ("main", stateSync1, [], 5, [
        [ (ComponentId "6e7a2000-439c-108e-df6d-93cff7a41266", [Zero]) ]
    ]),
    Ok (
        // Check it is zero for 5 ticks.
        [(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]
        |> List.replicate 5
    )

    "Simple D-flip-flop, one input, one output (one)",
    ("main", stateSync1, [], 5, [
        [ (ComponentId "6e7a2000-439c-108e-df6d-93cff7a41266", [One]) ]
    ]),
    Ok (
        // Tick 0, out is zero.
        [[(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]]
        @
        // Then out is 1 four times.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [One]]
         |> List.replicate 4)
    )

    "Two D-flip-flop connected in series, one input, one output (one)",
    ("main", stateSync2, [], 5, [
        [ (ComponentId "3739e54a-fd21-bf60-8fc2-a3d10108c947", [One]) ]
    ]),
    Ok (
        // Tick 0 and 1, out is zero.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]
         |> List.replicate 2)
        @
        // Then out is 1 three times.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [One]]
         |> List.replicate 3)
    )

    "Three D-flip-flop connected in series, one input, one output (one)",
    ("main", stateSync3, [], 5, [
        [ (ComponentId "3739e54a-fd21-bf60-8fc2-a3d10108c947", [One]) ]
    ]),
    Ok (
        // Tick 0,1 and 2, out is zero.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [Zero]]
         |> List.replicate 3)
        @
        // Then out is 1 two times.
        ([(ComponentId "a5d52bcd-0a6d-d123-7313-61d0b8b367fd", ComponentLabel "out", 1), [One]]
         |> List.replicate 2)
    )

    "StateSync1 custom component followed by a DFF (one)",
    ("main", stateSync4, [stateSync1Dependency], 5, [
        [ (ComponentId "03e4c81a-4703-d9f5-dfaf-301de006610f", [One]) ]
    ]),
    Ok (
        // Tick 0 and 1, out is zero.
        ([(ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [Zero]]
         |> List.replicate 2)
         @
        // Then out is 1 three times.
        ([(ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One]]
         |> List.replicate 3)
    )

    "StateSync1 custom component followed by a DFF, time varying inputs",
    ("main", stateSync4, [stateSync1Dependency], 10, [
        [ (ComponentId "03e4c81a-4703-d9f5-dfaf-301de006610f", [One]) ]
        [ (ComponentId "03e4c81a-4703-d9f5-dfaf-301de006610f", [Zero]) ]
        [ (ComponentId "03e4c81a-4703-d9f5-dfaf-301de006610f", [One]) ]
    ]),
    Ok [
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [Zero] ]
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [Zero] ]
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]  // After 2 timesteps, the One arrives.
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [Zero] ] // Back to Zero.
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]  // Back to One.
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]  // And statys one...
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One] ]
    ]

    "A DFF looping to itself via a Not gate. Two output nodes to probe the wires before and after the Not gate.",
    ("main", stateSync5, [], 4, []),
    Ok [
        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [Zero]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [One] ]
        
        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [One]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [Zero] ]

        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [Zero]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [One] ]

        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [One]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [Zero] ]
    ]

    "Similar to stateSync5, but with a stateSync1 custom component instead of a DFF.",
    ("main", stateSync6, [stateSync1Dependency], 4, []),
    Ok [
        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [Zero]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [One] ]
        
        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [One]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [Zero] ]

        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [Zero]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [One] ]

        [ (ComponentId "62a3108e-1198-502b-e338-e677815aead3", ComponentLabel "out1", 1), [One]
          (ComponentId "023094a0-9787-47ce-26af-03086cdc4b15", ComponentLabel "out2", 1), [Zero] ]
    ]

    "Similar to stateSync6, but with an extra connection input to output.",
    ("main", stateSync7, [stateSync1Dependency], 4, [
      [(ComponentId "03e4c81a-4703-d9f5-dfaf-301de006610f", [One])]
    ]),
    Ok [
        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [Zero]
          (ComponentId "fc5099db-d220-b42f-2add-b8c057164cb1", ComponentLabel "b1", 1), [One] ]

        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [Zero]
          (ComponentId "fc5099db-d220-b42f-2add-b8c057164cb1", ComponentLabel "b1", 1), [One] ]

        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One]
          (ComponentId "fc5099db-d220-b42f-2add-b8c057164cb1", ComponentLabel "b1", 1), [One] ]

        [ (ComponentId "781e7d9d-b18c-d614-dbc0-23bac9e617b7", ComponentLabel "b", 1), [One]
          (ComponentId "fc5099db-d220-b42f-2add-b8c057164cb1", ComponentLabel "b1", 1), [One] ]
      ]

    "Create a Not-ed self loop with the custom component of stateSync7.",
    ("main", stateSync8, [stateSync7Dependency; stateSync1Dependency], 4, []),
    Error {
      Msg = "Cycle detected in combinatorial logic. The cycle contains at least one combinatorial custom
                           component. Note that a custom component is considered
                           combinatorial if there is at least one combinatorial
                           path from input to output (i.e. at least one path
                           from input to output that encounters no clocked
                           component)."
      InDependency = None
      ComponentsAffected = ["5baefd71-8841-6e27-5930-ce3c4530fc4d"; "5339d358-0ac2-f907-4b2c-ba52b1a090b6"] |> List.map ComponentId
      ConnectionsAffected = ["d424a273-3637-84bd-e8b1-b4cba64f19ae"; "a1d8be49-12df-11ea-e0fc-03f882516cb9"] |> List.map ConnectionId
    }

    "StateSync8 connected to two outputs.",
    ("main", stateSync9, [stateSync8Dependency; stateSync7Dependency; stateSync1Dependency], 4, []),
    Error {
      Msg = "Cycle detected in combinatorial logic. The cycle contains at least one combinatorial custom
                           component. Note that a custom component is considered
                           combinatorial if there is at least one combinatorial
                           path from input to output (i.e. at least one path
                           from input to output that encounters no clocked
                           component)."
      InDependency = Some "fake-combinatorial-loop"
      ComponentsAffected = ["5baefd71-8841-6e27-5930-ce3c4530fc4d"; "5339d358-0ac2-f907-4b2c-ba52b1a090b6"] |> List.map ComponentId
      ConnectionsAffected = [] // Connections are not inferred in dependencies.
    }
]
