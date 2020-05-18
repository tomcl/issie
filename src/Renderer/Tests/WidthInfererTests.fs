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
    "Two inputs connected to a MergeWires component. No other connections", stateBus1,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", Some 1
    ] |> Map.ofList |> Ok

    "A MergeWires connected to a SplitWire 1.", stateBus2,
    [
        ConnectionId "conn0", None
    ] |> Map.ofList |> Ok

    "A MergeWires connected to a SplitWire 1 and a single-bit output node", stateBus3,
    [
        ConnectionId "conn0", None
        ConnectionId "conn1", None
    ] |> Map.ofList |> Ok

    "A MergeWires connected to a SplitWire 1, with loop", stateBus4,
    [
        ConnectionId "conn0", None
        ConnectionId "conn1", None
    ] |> Map.ofList |> Ok

    "All the bus components in series, properly connected. No other components", stateBus6,
    [
        ConnectionId "conn0", None
        ConnectionId "conn1", None
        ConnectionId "conn2", None
        ConnectionId "conn3", None
        ConnectionId "conn4", None
    ] |> Map.ofList |> Ok

    "Non-inferrable loop", stateBus7,
    [
        ConnectionId "conn0", None
        ConnectionId "conn1", None
    ] |> Map.ofList |> Ok

    "Mux connected to two MergeWires. Width not inferrable", stateBus8,
    [
        ConnectionId "conn0", Some 1
        ConnectionId "conn1", None
    ] |> Map.ofList |> Ok

    "A 4 bit input connected to a four bit output", stateBus13,
    [
        ConnectionId "conn", Some 4
    ] |> Map.ofList |> Ok

    "A 2 bit input split into 2 single bit outputs", stateBus16,
    [
        ConnectionId "conn0", Some 2
        ConnectionId "conn1", Some 1
        ConnectionId "conn2", Some 1
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

    "Mux connected to a SplitWire 1", stateBus9,
    Error {
        Msg = "Wrong wire width. Expecting at least size 2 but got 1."
        ConnectionsAffected = ["conn0"] |> List.map ConnectionId
    }

    "A 4 bit input connected to a 3 bit output", stateBus14,
    Error {
        Msg = "Wrong wire width. Expecting 3 but got 4."
        ConnectionsAffected = ["conn"] |> List.map ConnectionId
    }

    "A 3 bit input connected to a 4 bit output", stateBus15,
    Error {
        Msg = "Wrong wire width. Expecting 4 but got 3."
        ConnectionsAffected = ["conn"] |> List.map ConnectionId
    }
]

let testCasesWidthInferer : WidhtInfererTestCase list =
    testCasesWidthInfererSimple @
    testCasesWidthInfererBuses @
    testCasesWidthInfererError
