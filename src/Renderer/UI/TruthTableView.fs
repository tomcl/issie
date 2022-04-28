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
open TruthTableReduce
open BusWidthInferer
open Symbol
open SymbolUpdate
open Sheet.SheetInterface

/// Updates MergeWires and SplitWire Component labels to MWx/SWx.
/// Previous Issie versions had empty labels for these components.
// This change is necessary as all components must have labels for automatic IO
// generation when calculating Truth Tables for partial selections.
let updateMergeSplitWireLabels (model: Model) dispatch =
    let symModel = model.Sheet.Wire.Symbol
    let mwStartLabel =
        SymbolUpdate.generateLabel symModel MergeWires
    let mutable mwIdx =
        (mwStartLabel.[mwStartLabel.Length - 1]
        |> int) - (int '0')

    let swStartLabel =
        SymbolUpdate.generateLabel symModel (SplitWire 1)
    let mutable swIdx =
        (swStartLabel.[swStartLabel.Length - 1]
        |> int) - (int '0')
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    model.Sheet.GetCanvasState()
    |> fst
    |> List.iter (fun c ->
        match c.Type, c.Label with
        | MergeWires, "" | MergeWires, "L" ->
            let newLabel = sprintf "MW%i" mwIdx
            mwIdx <- mwIdx + 1
            setComponentLabel model sheetDispatch c newLabel
        | SplitWire _, "" | SplitWire _, "L" ->
            let newLabel = sprintf "SW%i" swIdx
            swIdx <- swIdx + 1
            setComponentLabel model sheetDispatch c newLabel
        | _ -> ())

//-------------------------------------------------------------------------------------//
//-----------Functions for generating Truth Tables from selection----------------------//
//-------------------------------------------------------------------------------------//

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

/// Splits a list of results into a list of Ok and list of Error
let filterResults results =
    let rec filter lst success error =
        match lst with
        | [] -> success,error
        | (Ok c)::tl -> filter tl (success @ [c]) error
        | (Error e)::tl -> filter tl success (error @ [e])
    filter results [] []

/// Corrects the Selected Canvas State by adding extra connections and IOs to components
/// not connected to anything. On success returns a new, corrected CanvasState compatible
/// with Step Simulator. On failure returns SimulationError.
let correctCanvasState (selectedCanvasState: CanvasState) (wholeCanvasState: CanvasState) =
    let components,connections = selectedCanvasState

    // Dummy ports for temporary use within function. Connections/Components with these
    // ports should never be in the CanvasState returned by this function!
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

    let rec getPortWidth' (port: Port) pw run =
        let pId = string port.Id
        (match Map.tryFind pId pw with
        | Some(Some w) -> Some w
        | Some(None) -> failwithf "what? WidthInferrer did not infer a width for a port"
        | None -> getPortWidthFromComponent port pw run)

    and
        getPortWidthFromComponent port pw run =
            let hostComponent =
                components
                |> List.filter (fun c -> port.HostId = c.Id)
                |> function
                    | [comp] -> comp
                    | [] -> failwithf "what? Port HostId does not match any ComponentIds in model"
                    | _ -> failwithf "what? Port HostId matches multiple ComponentIds in model"
            match hostComponent.Type with
            | Not | And | Or | Xor | Nand | Nor | Xnor -> Some 1
            | Input w -> Some w
            | Output w -> Some w
            | Constant (w,_) -> Some w
            | BusCompare (w,_) -> Some w
            | BusSelection (w,_) -> Some w
            | NbitsAdder w -> Some w
            | NbitsXor w -> Some w
            | IOLabel ->
                if run > 0 then
                    None
                else
                    let otherPort =
                        if port.PortType = PortType.Input then
                            hostComponent.OutputPorts.Head
                        else
                            hostComponent.InputPorts.Head
                    getPortWidth' otherPort pw (run+1)
            | _ -> None

    /// Find the width of a port. First try using WidthInferrer, and if that fails then
    /// try use the properties of the host component.
    let getPortWidth (port:Port) =
        match portWidths with
        | Error e -> Error e
        | Ok pw ->
            Ok <| getPortWidth' port pw 0

    /// Infer the label for newly created IOs so that they can be displayed in Truth Tables.
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
            match Symbol.portNames hostComponent.Type with
            | ([]) -> hostComponent.Label + "_IN" + (string pn)
            | lst ->
                if pn >= lst.Length then
                    failwithf "what? input PortNumber is greater than number of input port names on component"
                else
                    hostComponent.Label + "_" + lst[pn]
        | Some pn, PortType.Output ->
            match Symbol.portNames hostComponent.Type with
            | ([]) -> hostComponent.Label + "_OUT" + (string pn)
            | lst ->
                if pn >= lst.Length then
                    failwithf "what? output PortNumber is greater than number of output port names on component"
                else
                    let offset = hostComponent.InputPorts.Length
                    hostComponent.Label + "_" + lst[offset+pn]

    let removeDuplicateConnections (comps: Component list,conns: Connection list) =
        let checkDuplicate con lst =
            lst
            |> List.exists (fun c ->
                (c.Source = con.Source)
                && not (isPortInComponents c.Target comps)
                && not (isPortInComponents con.Target comps))

        comps,
        ([],conns)
        ||> List.fold (fun acc con ->
            match acc with
            | [] -> [con]
            | lst ->
                if checkDuplicate con lst then
                    lst
                else
                    con::lst)

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
                        Vertices = [(0.0,0.0,false)] // Irrelevant as we never draw this connection
                    })
            let extraOutputConns =
                comp.OutputPorts
                |> List.filter (fun p -> not (isPortInConnections p conns))
                |> List.map (fun p ->
                    {
                        Id = JSHelpers.uuid()
                        Source = {p with PortNumber = None}
                        Target = dummyInputPort
                        Vertices = [(0.0,0.0,false)] // Irrelevant as we never draw this connection
                    })
            acc @ extraInputConns @ extraOutputConns)

    let addExtraIOs (comps: Component list,conns: Connection list) =
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
                match getPortWidth con.Target with
                | Ok (Some pw) ->
                    let newId = JSHelpers.uuid()
                    let newLabel = inferIOLabel con.Target
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
                        W = 0
                        SymbolInfo = None}
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
                        ConnectionsAffected = e.ConnectionsAffected
                        ComponentsAffected = []
                    }
                    Ok con, acc @ [Error error]
            else if not (isPortInComponents con.Target comps) then
                match getPortWidth con.Source with
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
                        W = 0
                        SymbolInfo = None}
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
                        ConnectionsAffected = e.ConnectionsAffected
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
    |> removeDuplicateConnections
    |> addExtraConnections
    |> addExtraIOs
    |> checkCanvasWasCorrected

/// Make and return Simulation Data (or Simulation Error) for the model for selected components.
/// Identical functionality to SimulationView.makeSimData, but only considers selected components.
let makeSimDataSelected model : (Result<SimulationData,SimulationError> * CanvasState) option =
    let (selComponents,selConnections) = model.Sheet.GetSelectedCanvasState
    let wholeCanvas = model.Sheet.GetCanvasState()
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
                comp.Name <> project.OpenFileName)
        match correctCanvasState (selComps,selConns) wholeCanvas with
        | Error e -> Some (Error e, (selComps,selConns))
        | Ok (correctComps,correctConns) ->
            match CanvasStateAnalyser.analyseState (correctComps,correctConns) selLoadedComponents with
            | Some e -> Some (Error e,(correctComps,correctConns))
            | None ->
                Some (prepareSimulation project.OpenFileName (correctComps,correctConns) selLoadedComponents , (correctComps,correctConns))

//-------------------------------------------------------------------------------------//
//-------------Functions for manipulating existing Truth Tables------------------------//
//-------------------------------------------------------------------------------------//

let truncationWarning table =
    $"The Truth Table has been truncated to {table.TableMap.Count} input combinations.
    Not all rows may be shown. Please use more restrictive input constraints to avoid truncation."

/// Regenerate the Truth Table after applying new input constraints
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

/// Hide output columns in the Table
let hideColumns model (dispatch: Msg -> Unit) =
    printfn "Hiding Columns: %A" model.TTHiddenColumns
    match model.CurrentTruthTable with
    | None -> failwithf "what? Hiding columns when no Truth Table exists"
    | Some (Error e) ->
        failwithf "what? Hding columns option should not exist when there is TT error"
    | Some (Ok table) ->
        let oldTableMap =
            // Apply hiding to DCMap if it exists, otherwise TableMap
            match table.DCMap with
            | Some dc -> dc
            | None -> table.TableMap
        let newTableMap =
            if oldTableMap.IsEmpty || model.TTHiddenColumns.IsEmpty then
                oldTableMap
            else
                oldTableMap
                |> Map.map (fun lhs rhs ->
                    rhs
                    |> List.filter (fun cell ->
                        not <| List.contains cell.IO model.TTHiddenColumns))
        {table with HiddenColMap = newTableMap}
        |> Ok
        |> GenerateTruthTable
        |> dispatch

/// Apply a single numerical output constraint to a Truth Table Map
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

/// Apply all output constraints to the Truth Table
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
            (table.HiddenColMap, allOutputConstraints)
            ||> List.fold applyNumericalOutputConstraint
        {table with FilteredMap = filteredMap}
        |> Ok
        |> GenerateTruthTable
        |> dispatch

/// Comparison function for CellData values
let compareCellData (cd1: CellData) (cd2: CellData) =
    match cd1, cd2 with
    | DC, DC -> 0 // X = X
    | DC, _ -> 1 // DC is considered larger than anything
    | _, DC -> -1
    | Algebra _, Bits _ -> 1 // Algebra is considered larger than Bits
    | Bits _, Algebra _ -> -1
    | Algebra a1, Algebra a2 -> 
        compare a1 a2 // Algebra compared by alphabetical order
    | Bits wd1, Bits wd2 -> // Bits compared by which is largest
        (convertWireDataToInt wd1, convertWireDataToInt wd2)
        ||> compare

let sortByIO (io: CellIO) (lst: TruthTableRow list) = 
    let idx = 
        lst.Head
        |> List.tryFindIndex (fun c ->
            c.IO = io)
        |> function
            | Some i -> i
            | None -> failwithf "what? Failed to find IO: %s while sorting TT" io.getLabel
    
    lst
    |> List.sortWith (fun r1 r2 ->
        let cd1 = r1[idx].Data
        let cd2 = r2[idx].Data
        compareCellData cd1 cd2)

let sortTruthTable model dispatch =
    match model.CurrentTruthTable with
    | None -> failwithf "what? Trying to sort table when no Truth Table exists"
    | Some (Error e) ->
        failwithf "what? Sorting option should not exist when there is TT Error"
    | Some (Ok table) ->
        let sortedTable =
            match model.TTSortType, tableAsList table.FilteredMap with
            | _, [] -> {table with SortedListRep = []}
            | None, lst ->
                {table with SortedListRep = lst}
            | Some (io, Ascending), lst ->
                let sortedLst = sortByIO io lst
                {table with SortedListRep = sortedLst}
            | Some (io, Descending), lst ->
                let sortedLst = 
                    sortByIO io lst
                    |> List.rev
                {table with SortedListRep = sortedLst}
        sortedTable
        |> Ok
        |> GenerateTruthTable
        |> dispatch

//-------------------------------------------------------------------------------------//
//----------View functions for Truth Tables and Tab UI components----------------------//
//-------------------------------------------------------------------------------------//

let makeOnOffToggle state changeAction onText offText =
    Level.item [ Level.Item.HasTextCentered ] [
        Field.div [ Field.HasAddonsCentered ] [
            Control.div [] [ Button.button [
                Button.Color (if state = true then IsSuccess else NoColor)
                Button.OnClick (if state = false then changeAction else (fun _ -> ()))
            ] [ str onText ] ]
            Control.div [] [ Button.button [
                Button.Color (if state = false then IsDanger else NoColor)
                Button.OnClick (if state = true then changeAction else (fun _ -> ()))
            ] [ str offText ] ]
        ]
    ]

let makeSortingArrows (io:CellIO) sortInfo dispatch =
    let upSel, downSel =
        match sortInfo with
        | None -> false, false
        | Some (cio, Ascending) -> cio = io, false
        | Some (cio, Descending) -> false, cio = io
    let upArrow = 
        Button.button
            [
                Button.Props [sortArrowStyle]
                if upSel then Button.Color IsInfo
                Button.OnClick (fun _ -> 
                    (io,Ascending) |> Some |> SetTTSortType |> dispatch
                    ReSort |> Some |> SetTTOutOfDate |> dispatch)
            ]
            [str "\ufe3f"]
    let downArrow =
        Button.button
            [
                Button.Props [sortArrowStyle]
                if downSel then Button.Color IsInfo
                Button.OnClick (fun _ -> 
                    (io,Descending) |> Some |> SetTTSortType |> dispatch
                    ReSort |> Some |> SetTTOutOfDate |> dispatch)
            ]
            [str "\ufe40"]
    div [] [upArrow; downArrow]

let private makeMenuGroup openDefault title menuList =
    details [Open openDefault] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let viewCellAsHeading dispatch sortInfo (cell: TruthTableCell) =
    let addToolTip tip react =
        div [
            HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-top"
            Tooltip.dataTooltip tip
        ] [react]
    match cell.IO with
    | SimIO (_,label,_) ->
        let headingText = string label
        th [] 
            [makeElementLine [str headingText] [makeSortingArrows cell.IO sortInfo dispatch]] 
    | Viewer ((label,fullName), width) ->
        let headingEl =
            label |> string |> str
            |> (fun r -> if fullName <> "" then addToolTip fullName r else r)
        th [] [makeElementLine [headingEl] [makeSortingArrows cell.IO sortInfo dispatch]]

let viewOutputHider table hidden dispatch =
    let makeToggleRow io =
        let isChecked = not <| List.contains io hidden
        let changeAction = (fun _ ->
            dispatch <| ToggleHideTTColumn io
            HideColumn |> Some |> SetTTOutOfDate |> dispatch)
        let toggle = makeOnOffToggle isChecked changeAction "Visible" "Hidden"
        let ioLabel = str io.getLabel
        makeElementLine [ioLabel;toggle]
    if table.FilteredMap.IsEmpty then
        div [] [str "No Rows in Truth Table"]
    else
        let preamble = div [] [
            str "Hide or Un-hide Output or Viewer columns in the Truth Table."
            br []
            i [] [str "Note: Hiding a column will delete any constraints associated
                with that column."]
            br []; br []
        ]
        let toggleRows =
            table.TableMap
            |> Map.toList
            |> List.head
            |> snd
            |> List.map (fun cell -> makeToggleRow cell.IO [])
        div [] (preamble::toggleRows)

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

let viewTruthTableData (table: TruthTable) (mapType: MapToUse) sortInfo dispatch =
    let tMap = table.getMap mapType
    if tMap.IsEmpty then
        div [] [str "No Rows in Truth Table"]
    else
        let headings =
            table.SortedListRep.Head
            |> List.map (viewCellAsHeading dispatch sortInfo) 
            |> List.toSeq
        let body =
            table.SortedListRep
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
    // Truth Table Generation for selected components requires all components to have distinct labels.
    // Older Issie versions did not have labels for MergeWires and SplitWire components.
    // This step is needed for backwards compatability with older projects.
    updateMergeSplitWireLabels model dispatch

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
            br []
            hr []
            Heading.h5 [] [str "Truth Table for whole sheet"]
            br []
            wholeButton
            hr []
            Heading.h5 [] [str "Truth Table for selected logic"]
            br  []
            br  []
            selButton
            hr []
        ]
    | Some tableopt ->
        match model.TTIsOutOfDate with
        | Some Regenerate ->
            regenerateTruthTable model dispatch
            // Re-hide columns after regeneration in the next view cycle.
            // Refilter will be called in the subsequent cycle.
            HideColumn |> Some |> SetTTOutOfDate |> dispatch
        | Some HideColumn ->
            hideColumns model dispatch
            Refilter |> Some |> SetTTOutOfDate |> dispatch
        | Some Refilter ->
            filterTruthTable model dispatch
            ReSort |> Some |> SetTTOutOfDate |> dispatch
        | Some ReSort ->
            sortTruthTable model dispatch
            None |> SetTTOutOfDate |> dispatch
        | None -> ()

        let closeTruthTable _ =
            dispatch ClearInputConstraints
            dispatch ClearOutputConstraints
            dispatch ClearHiddenTTColumns
            dispatch (SetTTSortType None)
            dispatch CloseTruthTable
        let body =
            match tableopt with
            | Error e -> viewTruthTableError e
            | Ok table ->
                let startReducing () =
                    reduceTruthTable model.TTInputConstraints table model.TTBitLimit
                    |> Ok
                    |> GenerateTruthTable
                    |> dispatch
                    HideColumn |> Some |> SetTTOutOfDate |> dispatch
                let goBack () =
                    dispatch ClearDCMap
                    HideColumn |> Some |> SetTTOutOfDate |> dispatch
                match table.DCMap with
                | None ->
                    div [] [
                        (Button.button [Button.Color IsSuccess; Button.OnClick (fun _ -> startReducing())]
                        [str "Reduce"])
                        br []; br []
                        viewTruthTableData table Filtered model.TTSortType dispatch] 
                | Some dc ->
                    div [] [
                        (Button.button [Button.Color IsInfo; Button.OnClick (fun _ -> goBack ())]
                        [str "Back to Full Table"])
                        br []; br []
                        viewTruthTableData table Filtered model.TTSortType dispatch
                    ]
        let constraints =
            match tableopt with
            | Error _ -> div [] []
            | Ok _ -> div [] [viewConstraints model dispatch]
        let hidden =
            match tableopt with
            | Error _ -> div [] []
            | Ok table -> div [] [viewOutputHider table model.TTHiddenColumns dispatch]
        let menu =
            Menu.menu []  [
                makeMenuGroup false "Filter" [constraints; br [] ; hr []]
                makeMenuGroup false "Hide/Un-hide Columns" [hidden; br []; hr []]
                makeMenuGroup true "Truth Table" [body; br []; hr []]
            ]

        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick closeTruthTable ]
                [ str "Close Truth Table" ]
            br []; br []
            str "The Truth Table generator uses the diagram as it was at the moment of
                 pressing the \"Generate Truth Table\" button."
            menu
            ]