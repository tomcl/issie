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
open DrawHelpers
open DiagramStyle
open Notifications
open PopupView
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open TruthTableTypes
open ConstraintView
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

let filterResults results = 
    let rec filter lst success error =
        match lst with
        | [] -> success,error
        | (Ok c)::tl -> filter tl (success @ [c]) error
        | (Error e)::tl -> filter tl success (error @ [e])
    filter results [] []

let convertConnId (ConnectionId cId) = ConnectionId cId
        
let correctCanvasState (selectedCanvasState: CanvasState) (wholeCanvasState: CanvasState) =
    let components,connections = selectedCanvasState
    let dummyInputPort = {
        Id = "DummyIn"
        PortNumber = None
        PortType = PortType.Input
        HostId = "DummyIn_Host"
    }
    let dummyOutputPort = {
        Id = "DummyOut"
        PortNumber = None
        PortType = PortType.Output
        HostId = "DummyOut_Host"
    }

    let portWidths =
        match BusWidthInferer.inferConnectionsWidth wholeCanvasState with
        | Ok cw ->
            Map.toList cw
            |> List.fold (fun acc (cid,widthopt) ->
                let pIdEntries = 
                    getPortIdsfromConnectionId cid (snd wholeCanvasState)
                    |> List.map (fun pId -> (pId,widthopt))
                acc @ pIdEntries) []
            |> Map.ofList
            |> Ok
        | Error e -> Error e

    let getPortWidth pId =
        match portWidths with
        | Error e -> Error e
        | Ok pw ->
            (match Map.tryFind pId pw with
            | Some(Some w) -> Some w
            | Some(None) -> failwithf "what? WidthInferrer did not infer a width for a port"
            | None -> None)
            |> Ok

    let inferIOLabel (port: Port) =
        let hostComponent =
            components 
            |> List.filter (fun c -> port.HostId = c.Id)
            |> function 
                | [comp] -> comp
                | [] -> failwithf "what? Port HostId does not match any ComponentIds in model"
                | _ -> failwithf "what? Port HostId matches multiple ComponentIds in model"
        let portOnComponent =
            match port.PortNumber with
            | Some n -> port
            | None ->
                components
                |> List.collect (fun c -> List.append c.InputPorts c.OutputPorts)
                |> List.filter (fun cp -> port.Id = cp.Id)
                |> function
                    | [p] -> p
                    | _ -> failwithf "what? connection port does not map to a component port"
                
        match portOnComponent.PortNumber, port.PortType with
        | None,_ -> failwithf "what? no PortNumber. A connection port was probably passed to inferIOLabel"
        | Some pn, PortType.Input -> 
            match Symbol.portDecName hostComponent with
            | ([],_) -> hostComponent.Label + "_IN" + (string pn)
            | (lst,_) -> 
                if pn >= lst.Length then
                    failwithf "what? input PortNumber is greater than number of input port names on component"
                else
                    hostComponent.Label + "_" + lst[pn]
        | Some pn, PortType.Output ->
            match Symbol.portDecName hostComponent with
            | (_,[]) -> hostComponent.Label + "_OUT" + (string pn)
            | (_,lst) ->
                if pn >= lst.Length then
                    failwithf "what? output PortNumber is greater than number of output port names on component"
                else
                    hostComponent.Label + "_" + lst[pn]


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
                        Target = {p with PortNumber = None}
                        Vertices = [(0.0,0.0)] // Irrelevant as we never draw this connection
                    })
            let extraOutputConns =
                comp.OutputPorts
                |> List.filter (fun p -> not (isPortInConnections p conns))
                |> List.map (fun p -> 
                    {
                        Id = JSHelpers.uuid()
                        Source = {p with PortNumber = None}
                        Target = dummyInputPort
                        Vertices = [(0.0,0.0)] // Irrelevant as we never draw this connection
                    })
            acc @ extraInputConns @ extraOutputConns)

    let addExtraIOs (comps: Component list,conns: Connection list) =
        // let mutable inputCount = 0
        // let mutable outputCount = 0
        let compsOk : Result<Component,SimulationError> list = List.map (fun c -> Ok c) comps

        (compsOk,conns)
        ||> List.mapFold (fun acc con ->
            if  not (isPortInComponents con.Source comps) && not (isPortInComponents con.Target comps) then
                let error = {
                    Msg = "Selected logic includes a wire connected to no components."
                    InDependency = None
                    ComponentsAffected = []
                    ConnectionsAffected = [ConnectionId(con.Id)]}
                Error error,acc
            else if not (isPortInComponents con.Source comps) then
                match getPortWidth con.Target.Id with
                | Ok (Some pw) ->
                    let newId = JSHelpers.uuid()
                    let newLabel = inferIOLabel con.Target
                    // inputCount <- inputCount + 1
                    let newPort = {
                        Id = JSHelpers.uuid()
                        PortNumber = Some 0
                        PortType = PortType.Output
                        HostId = newId}
                    let extraInput = {
                        Id = newId
                        Type = Input(pw)
                        Label = newLabel
                        InputPorts = []
                        OutputPorts = [newPort]
                        X = 0
                        Y = 0
                        H = 0
                        W = 0}
                    Ok {con with Source = {newPort with PortNumber = None}}, acc @ [Ok extraInput]
                | Ok (None) ->
                    let error = {
                        Msg = "Could not infer the width for an input into the selected logic."
                        InDependency = None
                        ComponentsAffected = [ComponentId(con.Target.HostId)]
                        ConnectionsAffected = []
                    }
                    Ok con, acc @ [Error error]
                | Error e -> 
                    let error = {
                        Msg = e.Msg
                        InDependency = None
                        ConnectionsAffected = e.ConnectionsAffected |> List.map convertConnId
                        ComponentsAffected = []
                    }
                    Ok con, acc @ [Error error]
            else if not (isPortInComponents con.Target comps) then
                match getPortWidth con.Source.Id with
                | Ok (Some pw) ->
                    let newId = JSHelpers.uuid()
                    let newLabel = inferIOLabel con.Source
                    //outputCount <- outputCount + 1
                    let newPort = {
                        Id = JSHelpers.uuid()
                        PortNumber = Some 0
                        PortType = PortType.Input
                        HostId = newId}
                    let extraOutput = {
                        Id = newId
                        Type = Output(pw)
                        Label = newLabel
                        InputPorts = [newPort]
                        OutputPorts = []
                        X = 0
                        Y = 0
                        H = 0
                        W = 0}
                    Ok {con with Target = {newPort with PortNumber = None}}, acc @ [Ok extraOutput]
                | Ok (None) ->
                    let error = {
                        Msg = "Could not infer the width for an output produced by the selected logic."
                        InDependency = None
                        ComponentsAffected = [ComponentId(con.Source.HostId)]
                        ConnectionsAffected = []
                    }
                    Ok con, acc @ [Error error]
                | Error e -> 
                    let error = {
                        Msg = e.Msg
                        InDependency = None
                        ConnectionsAffected = e.ConnectionsAffected |> List.map convertConnId
                        ComponentsAffected = []
                    }
                    Ok con, acc @ [Error error]
            else
                Ok con,acc)
        |> (fun (a,b) -> (b,a))
    
    let checkCanvasWasCorrected (compsRes: Result<Component,SimulationError> list,connsRes: Result<Connection,SimulationError> list) =
        let comps,compErrors = filterResults compsRes
        let conns,connErrors = filterResults connsRes

        match compErrors,connErrors with
        | [],[] -> Ok (comps,conns)
        | e::tl,_ -> Error e
        | _,e::tl -> Error e
        

    (components,connections)
    |> addExtraConnections
    |> addExtraIOs
    |> checkCanvasWasCorrected
    
let makeSimDataSelected model : (Result<SimulationData,SimulationError> * CanvasState) option =
    let (selComponents,selConnections) = model.Sheet.GetSelectedCanvasState
    let wholeCanvas = model.Sheet.GetCanvasState()
    let selOtherComponents =
        ([],selComponents)
        ||> List.fold (fun acc comp ->
            match comp.Type with
            | Custom cc -> acc @ [cc.Name]
            | _ -> acc)

    match selComponents, selConnections, model.CurrentProj with
    | _,_,None -> None
    | [],[],_ -> None
    | [],_,_ -> Some <| (Error {
        Msg = "Only connections selected. Please select a combination of connections and components."
        InDependency = None
        ComponentsAffected = []
        ConnectionsAffected =[] }, (selComponents,selConnections))
    | selComps,selConns,Some project ->
        let selLoadedComponents =
            project.LoadedComponents
            |> List.filter (fun comp ->
                comp.Name <> project.OpenFileName
                && List.contains comp.Name selOtherComponents)
        match correctCanvasState (selComps,selConns) wholeCanvas with
        | Error e -> Some (Error e, (selComps,selConns))
        | Ok (correctComps,correctConns) ->
            match CanvasStateAnalyser.analyseState (correctComps,correctConns) selLoadedComponents with
            | Some e -> Some (Error e,(correctComps,correctConns))
            | None ->
                Some (prepareSimulation project.OpenFileName (correctComps,correctConns) selLoadedComponents , (correctComps,correctConns))

let truncationWarning table =
    $"The Truth Table has been truncated to {table.TableMap.Count} input combinations. 
    Not all rows may be shown. Please use more restrictive input constraints to avoid truncation."

let regenerateTruthTable model (dispatch: Msg -> Unit) =
    match model.CurrentTruthTable with
    | None -> failwithf "what? Adding constraint when no Truth Table exists"
    | Some (Error e) -> 
        failwithf "what? Constraint add option should not exist when there is TT error"
    | Some (Ok table) ->
        let tt = truthTableRegen table.TableSimData model.TTInputConstraints model.TTBitLimit
        if tt.IsTruncated then
                    let popup = Notifications.warningPropsNotification (truncationWarning tt)
                    dispatch <| SetPropertiesNotification popup
        tt
        |> Ok
        |> GenerateTruthTable
        |> dispatch

let hideColumnns model (dispatch: Msg -> Unit) =
    match model.CurrentTruthTable with
    | None -> failwithf "what? Hiding columns when no Truth Table exists"
    | Some (Error e) ->
        failwithf "what? Hding columns option should not exist when there is TT error"
    | Some (Ok table) ->
        let newTableMap =
            if table.TableMap.IsEmpty || model.TTHiddenColumns.IsEmpty then
                table.TableMap
            else
                table.TableMap
                |> Map.map (fun lhs rhs ->
                    rhs
                    |> List.filter (fun cell -> 
                        List.contains cell.IO model.TTHiddenColumns))
        {table with TableMap = newTableMap}
        |> Ok
        |> GenerateTruthTable
        |> dispatch
                

let applyNumericalOutputConstraint (table: Map<TruthTableRow,TruthTableRow>) (con: Constraint) =
    table
    |> Map.filter (fun _ right ->
        right
        |> List.exists (fun cell ->
            match con with
            | Equality e ->
                if e.IO <> cell.IO then 
                    false
                else 
                    match cell.Data with
                    | Algebra _ -> failwithf "what? Algebra cellData when applying output constraints"
                    | DC -> true
                    | Bits wd ->
                        let cellVal = convertWireDataToInt wd
                        cellVal = e.Value
            | Inequality i ->
                if i.IO <> cell.IO then
                    false
                else
                    match cell.Data with
                    | Algebra _ -> failwithf "what? Algebra cellData when applying output constraints"
                    | DC -> true
                    | Bits wd ->
                        let cellVal = convertWireDataToInt wd
                        i.LowerBound <= int cellVal && cellVal <= i.UpperBound
                        ))

let filterTruthTable model (dispatch: Msg -> Unit) =
    printfn "Refiltering Table"
    match model.CurrentTruthTable with
    | None -> failwithf "what? Trying to filter table when no Truth Table exists"
    | Some (Error e) ->
        failwithf "what? Filtering option should not exist when there is TT Error"
    | Some (Ok table) ->
        let allOutputConstraints =
            (model.TTOutputConstraints.Equalities
            |> List.map Equality)
            @
            (model.TTOutputConstraints.Inequalities
            |> List.map Inequality)
        let filteredMap = 
            (table.TableMap, allOutputConstraints)
            ||> List.fold applyNumericalOutputConstraint
        {table with FilteredMap = filteredMap}
        |> Ok
        |> GenerateTruthTable
        |> dispatch
        
let private makeMenuGroup openDefault title menuList =
    details [Open openDefault] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let tableAsList (table: TruthTable): TruthTableRow list =
    table.FilteredMap
    |> Map.toList
    |> List.map (fun (lhs,rhs) -> List.append lhs rhs)

let viewCellAsHeading (cell: TruthTableCell) = 
    let addToolTip tip react = 
        div [ 
            HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-top"
            Tooltip.dataTooltip tip
        ] [react]
    match cell.IO with
    | SimIO (_,label,_) ->
        let headingText = string label
        th [] [str headingText]
    | Viewer ((label,fullName), width) ->
        let headingEl = 
            label |> string |> str
            |> (fun r -> if fullName <> "" then addToolTip fullName r else r)
        th [] [headingEl]

let viewCellAsData (cell: TruthTableCell) =
    match cell.Data with 
    | Bits [] -> failwithf "what? Empty WireData in TruthTable"
    | Bits [bit] -> td [] [str <| bitToString bit]
    | Bits bits ->
        let width = List.length bits
        let value = viewFilledNum width Hex <| convertWireDataToInt bits
        td [] [str value]
    | Algebra a -> td [] [str <| a]
    | DC -> td [] [str <| "X"]

let viewRowAsData (row: TruthTableRow) =
    let cells = 
        row
        |> List.map viewCellAsData
        |> List.toSeq
    tr [] cells
        
let viewTruthTableError simError =
    let error = 
        match simError.InDependency with
        | None ->
            div [] [
                str simError.Msg
                br []
                str <| "Please fix the error and retry."
            ]
        | Some dep ->
            div [] [
                str <| "Error found in dependency \"" + dep + "\":"
                br []
                str simError.Msg
                br []
                str <| "Please fix the error in the dependency and retry."
            ]
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        error
    ]
    
let viewTruthTableData (table: TruthTable) =
    if table.FilteredMap.IsEmpty then // Should never be matched
        div [] [str "No Rows in Truth Table"]
    else
        let TTasList = tableAsList table
        let headings =
            TTasList.Head
            |> List.map viewCellAsHeading
            |> List.toSeq
        let body =
            TTasList
            |> List.map viewRowAsData
            |> List.toSeq
            

        div [] [
            Table.table [
                Table.IsBordered
                Table.IsFullWidth
                Table.IsStriped
                Table.IsHoverable] 
                [ 
                    thead [] [tr [] headings]
                    tbody [] body
                ]
        ]

let viewTruthTable model dispatch =
    let generateTruthTable simRes =
        match simRes with 
        | Some (Ok sd,_) -> 
            let tt = truthTable sd model.TTInputConstraints model.TTBitLimit
            if tt.IsTruncated then
                let popup = Notifications.warningPropsNotification (truncationWarning tt)
                dispatch <| SetPropertiesNotification popup
            tt
            |> Ok
            |> GenerateTruthTable
            |> dispatch
        | Some (Error e,_) ->
            Error e
            |> GenerateTruthTable
            |> dispatch
        | None -> ()

    match model.CurrentTruthTable with
    | None ->
        let wholeSimRes = SimulationView.makeSimData model
        let wholeButton =
            match wholeSimRes with
            | None -> div [] []
            | Some (Error _,_) -> 
                Button.button 
                    [
                        Button.Color IColor.IsWarning
                        Button.OnClick (fun _ -> generateTruthTable wholeSimRes)
                    ] [str "See Problems"]
            | Some (Ok sd,_) -> 
                if sd.IsSynchronous = false then 
                    Button.button 
                        [
                            Button.Color IColor.IsSuccess
                            Button.OnClick (fun _ -> generateTruthTable wholeSimRes)
                        ] [str "Generate Truth Table"]
                else 
                    Button.button 
                        [
                            Button.Color IColor.IsSuccess
                            Button.IsLight
                            Button.OnClick (fun _ -> 
                                let popup = 
                                    Notifications.errorPropsNotification 
                                        "Truth Table generation only supported for Combinational Logic"
                                dispatch <| SetPropertiesNotification popup)
                        ] [str "Generate Truth Table"]

        let selSimRes = makeSimDataSelected model
        let selButton =
            match selSimRes with
            | None -> div [] []
            | Some (Error _,_) -> 
                Button.button 
                    [
                        Button.Color IColor.IsWarning
                        Button.OnClick (fun _ -> generateTruthTable selSimRes)
                    ] [str "See Problems"]
            | Some (Ok sd,_) -> 
                if sd.IsSynchronous = false then 
                    Button.button 
                        [
                            Button.Color IColor.IsSuccess
                            Button.OnClick (fun _ -> generateTruthTable selSimRes)
                        ] [str "Generate Truth Table"]
                else 
                    Button.button 
                        [
                            Button.Color IColor.IsSuccess
                            Button.IsLight
                            Button.OnClick (fun _ -> 
                                let popup = Notifications.errorPropsNotification "Truth Table generation only supported for Combinational Logic"
                                dispatch <| SetPropertiesNotification popup)
                        ] [str "Generate Truth Table"]
        div [] [
            str "Generate Truth Tables for combinational logic using this tab."
            br[]
            hr[]
            Heading.h5 [] [str "Truth Table for whole sheet"]
            br []
            wholeButton
            hr[]
            Heading.h5 [] [str "Truth Table for selected logic"]
            br []
            br []
            selButton
            hr[]
        ]
    | Some tableopt ->
        match model.TTIsOutOfDate with
        | Some Regenerate ->
            regenerateTruthTable model dispatch
            // Re-hide columns after regeneration in the next view cycle.
            // Refilter will be called in the subsequent cycle.
            HideColumn |> Some |> SetTTOutOfDate |> dispatch
        | Some HideColumn ->
            hideColumnns model dispatch
            Refilter |> Some |> SetTTOutOfDate |> dispatch
        | Some Refilter ->
            filterTruthTable model dispatch
            dispatch <| SetTTOutOfDate None
        | None -> ()

        let closeTruthTable _ =
            dispatch <| ClearInputConstraints
            dispatch <| ClearOutputConstraints
            dispatch CloseTruthTable
        let body = 
            match tableopt with
            | Error e -> viewTruthTableError e
            | Ok table -> viewTruthTableData table
        let constraints =
            match tableopt with
            | Error _ -> div [] []
            | Ok _ -> div [] [hr []; viewConstraints model dispatch]
        
        let menu = 
            Menu.menu []  [
                makeMenuGroup false "Filter" [constraints; br [] ; hr []]
                makeMenuGroup true "Truth Table" [body; br []; hr []]
            ]    

        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick closeTruthTable ]
                [ str "Close Truth Table" ]
            br []; br []
            str "The Truth Table generator uses the diagram as it was at the moment of
                 pressing the \"Generate Truth Table\" button."
            // constraints
            // br []
            // hr []
            // body
            // br []
            // hr []
            menu
            ]