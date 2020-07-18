module SimulatorSyncTests

open CommonTypes
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

    "A connected to output via both sync and comb paths.",
    ("main", stateSync7, [], 4, [
        [(ComponentId "ff10125a-601f-e1d5-e379-7eb7c65eb91f", [One])]
        [(ComponentId "ff10125a-601f-e1d5-e379-7eb7c65eb91f", [Zero])]
    ]),
    Ok [
        [ (ComponentId "794d5154-6969-3f4e-9c8b-4bc17927c28f", ComponentLabel "B-Comb", 1), [One]
          (ComponentId "95452292-b507-ab43-f082-85152d3e4cf2", ComponentLabel "B-Sync", 1), [Zero] ]

        [ (ComponentId "794d5154-6969-3f4e-9c8b-4bc17927c28f", ComponentLabel "B-Comb", 1), [Zero]
          (ComponentId "95452292-b507-ab43-f082-85152d3e4cf2", ComponentLabel "B-Sync", 1), [One] ]

        [ (ComponentId "794d5154-6969-3f4e-9c8b-4bc17927c28f", ComponentLabel "B-Comb", 1), [Zero]
          (ComponentId "95452292-b507-ab43-f082-85152d3e4cf2", ComponentLabel "B-Sync", 1), [Zero] ]
        
        [ (ComponentId "794d5154-6969-3f4e-9c8b-4bc17927c28f", ComponentLabel "B-Comb", 1), [Zero]
          (ComponentId "95452292-b507-ab43-f082-85152d3e4cf2", ComponentLabel "B-Sync", 1), [Zero] ]
    ]

    "stateSync7 Not-ed self looped in the synchronous branch.",
    ("main", stateSync8, [stateSync7Dependency], 4, []),
    Ok [
        [ (ComponentId "66030e1a-4a97-244a-f0bb-d9e5fd25627f", ComponentLabel "B", 1), [One] ]
        [ (ComponentId "66030e1a-4a97-244a-f0bb-d9e5fd25627f", ComponentLabel "B", 1), [Zero] ]
        [ (ComponentId "66030e1a-4a97-244a-f0bb-d9e5fd25627f", ComponentLabel "B", 1), [One] ]
        [ (ComponentId "66030e1a-4a97-244a-f0bb-d9e5fd25627f", ComponentLabel "B", 1), [Zero] ]
    ]

    "stateSync8 wrapped",
    ("main", stateSync9, [stateSync8Dependency; stateSync7Dependency], 4, []),
    Ok [
        [ (ComponentId "a100bada-b27f-15ca-accb-153e717a31f1", ComponentLabel "B", 1), [One] ]
        [ (ComponentId "a100bada-b27f-15ca-accb-153e717a31f1", ComponentLabel "B", 1), [Zero] ]
        [ (ComponentId "a100bada-b27f-15ca-accb-153e717a31f1", ComponentLabel "B", 1), [One] ]
        [ (ComponentId "a100bada-b27f-15ca-accb-153e717a31f1", ComponentLabel "B", 1), [Zero] ]
    ]

    "A fully connected DFFE.",
    ("main", stateSync10, [], 8, [
        [ (ComponentId "5916f1cf-408e-4186-a839-c80926bfddf0", [Zero])   // in
          (ComponentId "cab54371-5e07-9586-eb9b-be8cc417e610", [Zero]) ] // enable
        [ (ComponentId "5916f1cf-408e-4186-a839-c80926bfddf0", [Zero])
          (ComponentId "cab54371-5e07-9586-eb9b-be8cc417e610", [One]) ] // turn on enable
        [ (ComponentId "5916f1cf-408e-4186-a839-c80926bfddf0", [One])
          (ComponentId "cab54371-5e07-9586-eb9b-be8cc417e610", [One]) ] // turn on enable
        [ (ComponentId "5916f1cf-408e-4186-a839-c80926bfddf0", [Zero])
          (ComponentId "cab54371-5e07-9586-eb9b-be8cc417e610", [Zero]) ] // turn off enable
        [ (ComponentId "5916f1cf-408e-4186-a839-c80926bfddf0", [One])
          (ComponentId "cab54371-5e07-9586-eb9b-be8cc417e610", [One]) ] // turn on enable again
        [ (ComponentId "5916f1cf-408e-4186-a839-c80926bfddf0", [Zero])
          (ComponentId "cab54371-5e07-9586-eb9b-be8cc417e610", [One]) ] // set to zero
    ]),
    Ok [
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [Zero] ]
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [Zero] ]
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [Zero] ] // enabled but zero
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [One] ]  // set to one
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [One] ]  // enable is off
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [One] ]  // enable is on again
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [Zero] ] // set to zero
        [ (ComponentId "a2c874bb-eaeb-d62d-8a72-5eeae48db694", ComponentLabel "out", 1), [Zero] ] // set to zero
    ]

    "StateSync7 Not-ed self looped in the combinatorial branch.",
    ("main", stateSync11, [stateSync7Dependency], 4, []),
    Error {
      Msg = "Cycle detected in combinatorial logic."
      InDependency = None
      ComponentsAffected = ["c9d9659a-4476-de3a-a838-eeab15496c99"; "5c24921a-88e9-7bc2-ee89-97fefb694902"] |> List.map ComponentId
      ConnectionsAffected = ["bc1f9a51-5ca8-1aa0-b4bd-b86bc9646c20"; "cc7a9da2-f78a-f3e1-1c2f-7323f2b43d15"] |> List.map ConnectionId
    }

    "StateSync11 connected to an output. Should spot cycle in the dependency.",
    ("main", stateSync12, [stateSync11Dependency; stateSync7Dependency], 4, []),
    Error {
      Msg = "Cycle detected in combinatorial logic."
      InDependency = Some "combinatorial-loop"
      ComponentsAffected = ["c9d9659a-4476-de3a-a838-eeab15496c99"; "5c24921a-88e9-7bc2-ee89-97fefb694902"] |> List.map ComponentId
      ConnectionsAffected = [] // Connections are not inferred in dependencies.
    }
]
