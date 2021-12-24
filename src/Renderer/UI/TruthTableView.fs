//(*
//    TruthTableView.fs
//
//    View for Truth Table in the right tab.
//*)
//

module TruthTableView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open NumberHelpers
open Helpers
open TimeHelpers
open JSHelpers
open DiagramStyle
open Notifications
open PopupView
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open Extractor
open Simulator
open TruthTableCreate
open BusWidthInferer

let getPortIdsfromConnectionId (cid: ConnectionId) (conns: Connection list) = 
    ([],conns)
    ||> List.fold (fun pIds c -> if c.Id = (string cid) then pIds @ [c.Source.Id;c.Target.Id] else pIds)
    

let isPortInConnections (port: Port) (conns: Connection list) =
    (false,conns)
    ||> List.fold (fun b c -> (port.Id = c.Source.Id || port.Id = c.Target.Id) || b )

let isPortInComponents (port: Port) (comps: Component list) =
    (false,comps)
    ||> List.fold (fun b c -> 
        let compPortIds = (c.InputPorts @ c.OutputPorts) |> List.map (fun p -> p.Id)
        List.contains port.Id compPortIds || b)
        

let correctCanvasState (canvasState: CanvasState) =
    let components,connections = canvasState
    let dummyInputPort = {
        Id = "DummyIn"
        PortNumber = None
        PortType = Input
        HostId = "DummyIn_Host"
    }
    let dummyOutputPort = {
        Id = "DummyOut"
        PortNumber = None
        PortType = Output
        HostId = "DummyOut_Host"
    }

    let connectionWidths = 
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok cw -> cw
        | Error _ -> failwithf "what> WidthInferrer failed to infer widths from whole canvas during TT Calculation"

    let portWidths =
        Map.toList connectionWidths
        |> List.fold (fun acc (cid,widthopt) ->
            let pIdEntries = 
                getPortIdsfromConnectionId cid connections
                |> List.map (fun pId -> (pId,widthopt))
            acc @ pIdEntries) []
        |> Map.ofList

    let getPortWidth pId =
        match Map.find pId portWidths with
        | Some w -> w
        | None -> failwithf "WidthInferrer did not infer a width for a port"

    let addExtraConnections (comps: Component list,conns: Connection list) =
        comps,
        (conns,comps)
        ||> List.fold (fun acc comp -> 
            let extraInputConns = 
                comp.InputPorts
                |> List.filter (fun p -> not (isPortInConnections p conns))
                |> List.map (fun p -> 
                    {
                        Id = JSHelpers.uuid()
                        Source = dummyOutputPort
                        Target = p
                        Vertices = [(0.0,0.0)] // Irrelevant as we never draw this connection
                    })
            let extraOutputConns =
                comp.OutputPorts
                |> List.filter (fun p -> not (isPortInConnections p conns))
                |> List.map (fun p -> 
                    {
                        Id = JSHelpers.uuid()
                        Source = p
                        Target = dummyInputPort
                        Vertices = [(0.0,0.0)] // Irrelevant as we never draw this connection
                    })
            acc @ extraInputConns @ extraOutputConns)

    let addExtraIOs (comps: Component list,conns: Connection list) =
        let mutable inputCount = 0
        let mutable outputCount = 0
        (comps,conns)
        ||> List.mapFold (fun acc con ->
            if not (isPortInComponents con.Source comps) then
                let newId = JSHelpers.uuid()
                let newLabel = "TT_IN" + string inputCount
                inputCount <- inputCount + 1
                let newPort = {
                    Id = JSHelpers.uuid()
                    PortNumber = Some 0
                    PortType = PortType.Output
                    HostId = newId}
                let extraInput = {
                    Id = newId
                    Type = Input(getPortWidth con.Target.Id)
                    Label = newLabel
                    InputPorts = []
                    OutputPorts = [newPort]
                    X = 0
                    Y = 0
                    H = 0
                    W = 0}
                {con with Source = newPort}, acc @ [extraInput]
            else if not (isPortInComponents con.Target comps) then
                let newId = JSHelpers.uuid()
                let newLabel = "TT_OUT" + string outputCount
                outputCount <- outputCount + 1
                let newPort = {
                    Id = JSHelpers.uuid()
                    PortNumber = Some 0
                    PortType = PortType.Input
                    HostId = newId}
                let extraOutput = {
                    Id = newId
                    Type = Output(getPortWidth con.Source.Id)
                    Label = newLabel
                    InputPorts = [newPort]
                    OutputPorts = []
                    X = 0
                    Y = 0
                    H = 0
                    W = 0}
                {con with Target = newPort}, acc @ [extraOutput]
            else
                con,acc)
        |> (fun (a,b) -> (b,a))

    match components.Length with
    | 0 -> Error "No components selected"
    | _ -> 
        (components,connections)
        |> addExtraConnections
        |> addExtraIOs
        |> Ok


//let makeSimDataSelected model =
//    match model.Sheet.GetSelectedCanvasState , model.CurrentProj with
//    | _ , None -> None
//    | 

let viewTruthTable model dispatch =
    let tempPrint simRes =
        match simRes with
        | Some (Ok sd,_) -> TruthTableCreate.printTruthTable sd
        | _ -> failwithf "Nothing"
    printf "Viewing Truth Table"
    let simRes = SimulationView.makeSimData model
    let buttonColor, buttonText =
        match simRes with
        | None -> IColor.IsWhite, ""
        | Some (Ok sd,_) -> 
            if sd.IsSynchronous = false then 
                IColor.IsSuccess, "Generate Truth Table" 
            else 
                IColor.IsInfo, "Combinational Only!"
        | Some (Error _,_) -> IColor.IsWarning, "See Problems"
    div [] [
        str "Generate Truth Tables for the whole sheet using this tab."
        br []
        str "Please note that Truth Tables can only be generated for Combinational Logic Circuits"
        br []; br []
        Button.button
            [ 
                Button.Color buttonColor; 
                Button.OnClick (fun _ -> tempPrint simRes ) ; 
            ]
            [ str buttonText ]
    ]