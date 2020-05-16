module WidthInfererTests

open DiagramTypes
open BusTypes
open CanvasStates

type private WidthInfererTestCaseInput = CanvasState
type private WidthInfererTestCaseOutput = Result<ConnectionsWidth, WidthInferError>
type private WidhtInfererTestCase = string * WidthInfererTestCaseInput * WidthInfererTestCaseOutput

let private testCasesWidthInfererOk : WidhtInfererTestCase list = [
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
]

let testCasesWidthInferer : WidhtInfererTestCase list =
    testCasesWidthInfererOk @ testCasesWidthInfererError
