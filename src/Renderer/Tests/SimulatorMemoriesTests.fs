module SimulatorMemoriesTests

open DiagramTypes
open SimulatorTypes
open CanvasStatesMemories
open NumberHelpers
open Helpers

/// Tuple with: (diagramName, state, loadedComponents, number of clock ticks, inputss).
/// Note that the inputss is a list of inputs. I.e. inputss provides an inputs
/// for every time step. The first set of inputs will be fed before the first
/// clock tick, then second after the second clock tick and so on. If no inputs
/// are provided for a certain iteration, none will fed.
type private TestCaseInput = string * CanvasState * LoadedComponent list * int * ((ComponentId * WireData) list) list
type private IterationOutput = (SimulationIO * WireData) list // Output after every clock tick.
type private TestCaseOutput = Result<IterationOutput list, SimulationError>
type private TestCase = string * TestCaseInput * TestCaseOutput

let private toWD num w = convertIntToWireData (int64 num) w

let private makeStateMem3Input addr dataIn write =
    [ (ComponentId "81030fc6-2471-a568-160f-922709edeb2e", toWD addr 3)
      (ComponentId "266d8763-6fbf-37c2-6825-d3487153053b", toWD dataIn 4)
      (ComponentId "f0769200-24e3-5c7b-3591-c5c3711d9336", toWD write 1) ]

let private makeStateMem3Output dataOut =
    [ (ComponentId "1f9704f9-88fc-124c-3ff8-21c36cfb7328", ComponentLabel "data-out", 4), toWD dataOut 4 ]

let testCasesSimulatorMemories : TestCase list = [
    "Synchronous ROM connected to address and output",
    ("main", stateMem1, [], 6, [
        [ (ComponentId "4f65afe9-f03c-2b97-fde9-c657ca32f246", toWD 0 2) ]
        [ (ComponentId "4f65afe9-f03c-2b97-fde9-c657ca32f246", toWD 1 2) ]
        [ (ComponentId "4f65afe9-f03c-2b97-fde9-c657ca32f246", toWD 2 2) ]
        [ (ComponentId "4f65afe9-f03c-2b97-fde9-c657ca32f246", toWD 3 2) ]
    ]),
    Ok [
        [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 4), toWD 0 4 ]  // Mem[0]
        [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 4), toWD 0 4 ]  // Mem[0]
        [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 4), toWD 1 4 ]  // Mem[1]
        [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 4), toWD 4 4 ]  // Mem[2]
        [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 4), toWD 15 4 ] // Mem[3]
        [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 4), toWD 15 4 ] // Mem[3]
    ]

    "Synchronous ROM connected to address and output. ROM is big.",
    ("main", stateMem2, [], pow2(8)+1, (
        [0..pow2(8)-1] |> List.map (fun i ->
            [ (ComponentId "4f65afe9-f03c-2b97-fde9-c657ca32f246", toWD i 8) ]
        )
    )),
    Ok (
        [ [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 8), toWD 0 8 ] ] // Mem[0]
        @
        (
            [0..pow2(8)-1] |> List.map (fun i ->
                [ (ComponentId "fbb5aa79-a471-ac24-4201-56ae39d537c6", ComponentLabel "data", 8), toWD i 8 ] 
            )
        )
    )

    "RAM only writes if write flag is set",
    ("main", stateMem3, [], 10, [
        makeStateMem3Input 0 1 0
        makeStateMem3Input 0 1 0
        makeStateMem3Input 0 1 1 // Turn on write flag. Set to one.
        makeStateMem3Input 0 0 1 // Set to zero.
        makeStateMem3Input 0 1 1 // Set to one.
        makeStateMem3Input 0 0 0 // Turn off write flag. Stays at one.
        makeStateMem3Input 0 0 0
        makeStateMem3Input 0 0 0
        makeStateMem3Input 0 0 0
        makeStateMem3Input 0 0 0
    ]),
    Ok [
        makeStateMem3Output 0 // Mem[0], unchanged
        makeStateMem3Output 0 // Mem[0], unchanged
        makeStateMem3Output 0 // Mem[0], unchanged
        makeStateMem3Output 1 // Mem[0], set to 1
        makeStateMem3Output 0 // Mem[0], set to 0
        makeStateMem3Output 1 // Mem[0], set to 1
        makeStateMem3Output 1 // Mem[0], stays 1
        makeStateMem3Output 1 // Mem[0], stays 1
        makeStateMem3Output 1 // Mem[0], stays 1
        makeStateMem3Output 1 // Mem[0], stays 1
    ]

    "RAM writes and reads all locations.",
    ("main", stateMem3, [], 17, 
        ( [0..pow2(3)-1] |> List.map(fun i -> makeStateMem3Input i (i+1) 1) ) // Write all locations.
        @
        ( [0..pow2(3)-1] |> List.map(fun i -> makeStateMem3Input i 0 0) ) // Read all locations.
    ),
    Ok (
        [ makeStateMem3Output 0 ]
        @
        ( [0..pow2(3)-1] |> List.map(fun i -> makeStateMem3Output <| i+1) )
        @
        ( [0..pow2(3)-1] |> List.map(fun i -> makeStateMem3Output <| i+1) )
    )
]
