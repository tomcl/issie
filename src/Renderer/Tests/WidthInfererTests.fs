module WidthInfererTests

open DiagramTypes
open BusTypes
open CanvasStates
open CanvasStatesWithBuses

type private WidthInfererTestCaseInput = CanvasState
type private WidthInfererTestCaseOutput = Result<ConnectionsWidth, WidthInferError>
type private WidhtInfererTestCase = string * WidthInfererTestCaseInput * WidthInfererTestCaseOutput

let private testCasesWidthInfererSimple : WidhtInfererTestCase list = [
    "Two unconnected input nodes. No conections", state2,
    Ok Map.empty

    "Simple circuit with one input connected to one output", state3,
    [
       ConnectionId "conn0", Some 1
    ] |> Map.ofList |> Ok

    "Simple circuit with one input connected to two outputs", state4,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok

    "Two inputs; one And; one output", state6,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
        ConnectionId "conn2", Some 1
    ] |> Map.ofList |> Ok

    "Partially connected and: missing bottom connections", state25,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn2", Some 1
    ] |> Map.ofList |> Ok

    "Partially connected and: missing connection to output", state26,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok

    "Partially connected and: missing both input connections", state27,
    [
        ConnectionId "conn2", Some 1
    ] |> Map.ofList |> Ok

    "One input and one output, connected to the state3CustomComponent", state16,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok

    "Nested custom components", state18,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok
]

let private testCasesWidthInfererBuses : WidhtInfererTestCase list = [
    "Two inputs connected to a MakeBus2 component. No other connections", stateBus1,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok

    "A MakeBus2 connected to a SplitBus2", stateBus2,
    [
        ConnectionId "conn0", Some 2
    ] |> Map.ofList |> Ok

    "A MakeBus2 connected to a SplitBus2, with loop", stateBus4,
    [
        ConnectionId "conn0", Some 2
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok

    "All the bus components in series, properly connected. No other components", stateBus6,
    [
        ConnectionId "conn0", Some 2
        ConnectionId "conn1", Some 3
        ConnectionId "conn2", Some 4
        ConnectionId "conn3", Some 3
        ConnectionId "conn4", Some 2
    ] |> Map.ofList |> Ok

    "Non-inferrable loop: PushToBusFirst connected to PushToBusLast and loop back", stateBus7,
    [
        ConnectionId "conn0", None
        ConnectionId "conn1", None
    ] |> Map.ofList |> Ok
]

let private testCasesWidthInfererError : WidhtInfererTestCase list = [
    "Two inputs connected to the same output", state5,
    Error {
        Msg = "Wire driven by multiple outputs"
        ConnectionsAffected = ["conn1"; "conn0"] |> List.map ConnectionId
    }

    "Two inputs; one And; one output; with extra connection input to output", state7,
    Error {
        Msg = "Wire driven by multiple outputs"
        ConnectionsAffected = ["conn3"; "conn2"] |> List.map ConnectionId
    }

    "A MakeBus2 connected to a SplitBus2 and a single-bit output node", stateBus3,
    Error {
        Msg = "Wrong wire width. Expecting 1 but got 2."
        ConnectionsAffected = ["conn1"] |> List.map ConnectionId
    }

    "Two inputs connected to a PushToBusFirst component. No other connections", stateBus5,
    Error {
        Msg = "Wrong wire width. Expecting at least size 2 but got 1."
        ConnectionsAffected = ["conn1"] |> List.map ConnectionId
    }
]

let testCasesWidthInferer : WidhtInfererTestCase list =
    testCasesWidthInfererSimple @
    testCasesWidthInfererBuses @
    testCasesWidthInfererError
