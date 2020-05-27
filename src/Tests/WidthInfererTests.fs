module WidthInfererTests

open CommonTypes
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

    "3 bit input merged with 4 bit input, then split in the same way", stateBus17,
    [
        ConnectionId "678ffa07-0ed7-7b6f-ba9b-b14839c08a71", Some 7
        ConnectionId "7438de0c-bbaf-c1f6-0307-42ece66c6c00", Some 4
        ConnectionId "82cc4ce9-7b15-40ac-0226-98a7f8e2fb9a", Some 4
        ConnectionId "9df32195-00b4-9795-3dc1-78fb70d453f2", Some 3
        ConnectionId "cd13c70f-6037-0cf4-1295-5795e92d745c", Some 3
    ] |> Map.ofList |> Ok
]

let private testCasesWidthInfererError : WidhtInfererTestCase list = [
    "Two inputs connected to the same output", state5,
    Error {
        Msg = "A wire must have precisely one driving component. If you want to merge two wires together, use a MergeWires component."
        ConnectionsAffected = ["conn1"; "conn0"] |> List.map ConnectionId
    }

    "Two inputs; one And; one output; with extra connection input to output", state7,
    Error {
        Msg = "A wire must have precisely one driving component. If you want to merge two wires together, use a MergeWires component."
        ConnectionsAffected = ["conn3"; "conn2"] |> List.map ConnectionId
    }

    "Mux connected to a SplitWire 1", stateBus9,
    Error {
        Msg = "Wrong wire width. Target port expects a signal with at least 2 bits, but source port produces a 1 bit(s) signal."
        ConnectionsAffected = ["conn0"] |> List.map ConnectionId
    }

    "A 4 bit input connected to a 3 bit output", stateBus14,
    Error {
        Msg = "Wrong wire width. Target port expects a 3 bit(s) signal, but source port produces a 4 bit(s) signal."
        ConnectionsAffected = ["conn"] |> List.map ConnectionId
    }

    "A 3 bit input connected to a 4 bit output", stateBus15,
    Error {
        Msg = "Wrong wire width. Target port expects a 4 bit(s) signal, but source port produces a 3 bit(s) signal."
        ConnectionsAffected = ["conn"] |> List.map ConnectionId
    }
]

let testCasesWidthInferer : WidhtInfererTestCase list =
    testCasesWidthInfererSimple @
    testCasesWidthInfererBuses @
    testCasesWidthInfererError
