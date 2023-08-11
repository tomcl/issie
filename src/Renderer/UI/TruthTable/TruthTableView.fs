﻿//(*
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
open PopupHelpers
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open TruthTableTypes
open ConstraintReduceView
open Extractor
open Simulator
open TruthTableCreate
open TruthTableReduce
open BusWidthInferer
open Symbol
open SymbolUpdate
open Sheet.SheetInterface
open DrawModelType

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
            FileMenuHelpers.setComponentLabel model sheetDispatch c newLabel
        | SplitWire _, "" | SplitWire _, "L" ->
            let newLabel = sprintf "SW%i" swIdx
            swIdx <- swIdx + 1
            FileMenuHelpers.setComponentLabel model sheetDispatch c newLabel
        | _ -> ())

//-------------------------------------------------------------------------------------//
//-----------Functions for generating Truth Tables from selection----------------------//
//-------------------------------------------------------------------------------------//

/// From a Connection Id and a list of all Connections, return the Ids for the source
/// and target ports of Connection described by the Connection Id.
let getPortIdsfromConnectionId (cid: ConnectionId) (conns: Connection list) =
    ([],conns)
    ||> List.fold (fun pIds c -> if c.Id = (string cid) then pIds @ [c.Source.Id;c.Target.Id] else pIds)

/// Returns true if the provided port is present in any of the connections in the
/// Connection list.
let isPortInConnections (port: Port) (conns: Connection list) =
    (false,conns)
    ||> List.fold (fun b c -> (port.Id = c.Source.Id || port.Id = c.Target.Id) || b )

/// Returns true if the provided port is present in any of the components in the
/// Component list.
let isPortInComponents (port: Port) (comps: Component list) =
    (false,comps)
    ||> List.fold (fun b c ->
        let compPortIds = (c.InputPorts @ c.OutputPorts) |> List.map (fun p -> p.Id)
        List.contains port.Id compPortIds || b)

/// Partitions a list of results into a list of Ok and list of Error
let partitionResults results =
    let rec filter lst success error =
        match lst with
        | [] -> success,error
        | (Ok c)::tl -> filter tl (success @ [c]) error
        | (Error e)::tl -> filter tl success (error @ [e])
    filter results [] []

/// Fills in internal connections between any two selected components.
/// This makes a contiguous circuit. This allows truth tables to be selected via comnponents only.
let addInternalConnections 
    ((comps, conns): CanvasState) 
    ((wholeCanvasComps,wholeCanvasConns): CanvasState) =
    let circuitPorts =
        comps
        |> List.collect (fun comp -> comp.InputPorts @ comp.OutputPorts)
        |> List.map (fun port -> port.Id)
    let shouldAdd conn =
        not (List.contains conn conns) &&
        List.contains conn.Source.Id circuitPorts &&
        List.contains conn.Target.Id circuitPorts

    let addedConns =
        wholeCanvasConns
        |> List.filter shouldAdd
    comps, (addedConns @ conns)

    
/// Corrects the Selected Canvas State by adding extra connections and IOs to components
/// not connected to anything. On success returns a new, corrected CanvasState compatible
/// with Step Simulator. On failure returns SimulationError.
let correctCanvasState (selectedCanvasState: CanvasState) (wholeCanvasState: CanvasState) =
    let components,connections = addInternalConnections selectedCanvasState wholeCanvasState

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

    // Mapping between PortIds and their corresponding port widths.
    // Is error when WidthInferrer fails to infer widths.
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

    // First try to get port width from the portWidths Map, which contains mappings
    // generated by WidthInferrer
    let rec getPortWidth' (port: Port) pw run =
        let pId = string port.Id
        match Map.tryFind pId pw with
        | Some(Some w) -> 
            Some w
        | Some(None) -> 
            failwithf "what? WidthInferrer did not infer a width for a port"
        | None -> 
            // Try see if width can be inferred from componenet properties
            getPortWidthFromComponent port pw run
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
            | Not | And | Or | Xor | Nand | Nor | Xnor ->
                // Logic gates inputs/outputs always have width 1.
                Some 1
            | Input w | Output w | Constant (w,_)| BusCompare (w,_)
            | BusSelection (w,_)| NbitsAdder w | NbitsXor(w,_) -> 
                // The above components all have width specified in their
                // component properties.
                Some w
            | IOLabel ->
                // For IOLabels, try find the width of the component connected to the other port.
                // NOTE: This case calls getPortWidth again: MUTUAL RECURSION.
                // The run count (run) ensures that this mutual call doesn't happen more than once.
                if run > 0 then
                    None
                else
                    let otherPort =
                        if port.PortType = PortType.Input then
                            hostComponent.OutputPorts.Head
                        else
                            hostComponent.InputPorts.Head
                    getPortWidth' otherPort pw (run+1)
            | Custom c ->
                let indexInput = List.tryFindIndex (fun (p:Port) -> p.Id = port.Id) hostComponent.InputPorts
                let indexOutput = List.tryFindIndex (fun (p:Port) -> p.Id = port.Id) hostComponent.OutputPorts
                match (indexInput,indexOutput) with
                |(Some i,None) ->
                    c.InputLabels[i] |> snd |> Some
                |(None, Some o) ->
                    c.OutputLabels[o] |> snd |> Some
                |_ -> None
                //let s = List.tryFind ()
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
        // The component the port is on
        let hostComponent =
            components
            |> List.filter (fun c -> port.HostId = c.Id)
            |> function
                | [comp] -> comp
                | [] -> failwithf "what? Port HostId does not match any ComponentIds in model"
                | _ -> failwithf "what? Port HostId matches multiple ComponentIds in model"
        // If the port is on a component, return the same port.
        // If the port is on a connection, find the equivalent port on a component.
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
            match CanvasStateAnalyser.portNames hostComponent.Type with
            | ([],_) when List.length hostComponent.InputPorts > 1 -> hostComponent.Label + "_IN" + (string pn)
            | ([],_) -> hostComponent.Label + ".IN" 
            | (lst,_) ->
                if pn >= lst.Length then
                    failwithf "what? input PortNumber is greater than number of input port names on component"
                else
                    lst[pn] //hostComponent.Label + "." + lst[pn]
        | Some pn, PortType.Output ->
            match CanvasStateAnalyser.portNames hostComponent.Type with
            | (_,[]) when List.length hostComponent.OutputPorts > 1 -> hostComponent.Label + "_OUT" + (string pn)
            | (_,[]) -> hostComponent.Label + "_OUT" 
            | (_,lst) ->
                if pn >= lst.Length then
                    failwithf "what? output PortNumber is greater than number of output port names on component"
                else
                    let offset = hostComponent.InputPorts.Length
                    lst[pn]//hostComponent.Label + "." + lst[offset+pn]

    // Check for cases where a component has two connections connected to its output port,
    // with both of these connection selected, but not the components on the other side of
    // the connection. Remove duplicate connections like these, as they would otherwise cause
    // two identical extra IOs to be added.
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

    // Add connections to any ports in the selection that are not connected to anything.
    // These extra connections are temporarily connected to dummy ports.
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

    let getUniqueNamesMap (acc:Result<Component,SimulationError> list) =
        let allLabels =
            acc
            |> List.collect (fun res ->
                match res with
                | Ok comp -> [comp.Label]
                | Error e -> []
            )
        let shortLabels =
            allLabels
            |> List.map (fun s ->
                let index = s.IndexOf(".")
                let len = String.length s
                match index with
                |(-1) -> s
                |_ -> s[(index+1)..(len-1)] |> string
            )

        if List.length shortLabels = List.length (List.distinct shortLabels) then
            List.zip allLabels shortLabels |> Map.ofList
        else 
            List.zip allLabels allLabels |> Map.ofList
    
    // Find any dangling connections and add an IO component to the side not
    // connected to any components.
    let addExtraIOs (comps: Component list,conns: Connection list) =
        let compsOk : Result<Component,SimulationError> list = List.map (fun c -> Ok c) comps
        let result = 
            (compsOk,conns)
            ||> List.mapFold (fun acc con ->
                // Source and target ports of connection are not on any selected components.
                // This is an invalid selection.
                if  not (isPortInComponents con.Source comps) && not (isPortInComponents con.Target comps) then
                    let error = {
                        ErrType = WrongSelection "Selected logic includes a wire connected to no components."
                        InDependency = None
                        ComponentsAffected = []
                        ConnectionsAffected = [ConnectionId(con.Id)]}
                    Error error,acc
                // Source port is not in components, so try add an Input Component
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
                            Type = Input1 (pw,None)
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
                            ErrType = InferConnWidths "Could not infer the width for an input into the selected logic."
                            InDependency = None
                            ComponentsAffected = [ComponentId(con.Target.HostId)]
                            ConnectionsAffected = []
                        }
                        Ok con, acc @ [Error error]
                    | Error e ->
                        let error = {
                            ErrType = InferConnWidths e.Msg
                            InDependency = None
                            ConnectionsAffected = e.ConnectionsAffected
                            ComponentsAffected = []
                        }
                        Ok con, acc @ [Error error]
                // Target port is not in components, so try add an Output Component
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
                            ErrType = InferConnWidths "Could not infer the width for an output produced by the selected logic."
                            InDependency = None
                            ComponentsAffected = [ComponentId(con.Source.HostId)]
                            ConnectionsAffected = []
                        }
                        Ok con, acc @ [Error error]
                    | Error e ->
                        let error = {
                            ErrType = InferConnWidths e.Msg
                            InDependency = None
                            ConnectionsAffected = e.ConnectionsAffected
                            ComponentsAffected = []
                        }
                        Ok con, acc @ [Error error]
                // Otherwise, everything is ok for this connection, no action needed.
                else
                    Ok con,acc)
            |> (fun (a,b) -> (b,a))
        
        let resComps = fst result
        let resConns = snd result
        let uniqueNamesMap = getUniqueNamesMap (fst result)
        
        let resCompsFixedNames =
            resComps
            |> List.map (fun res ->
                match res with
                |Ok comp -> Ok {comp with Label = uniqueNamesMap[comp.Label]}
                |Error e -> Error e
            )
        
        (resCompsFixedNames,resConns)

    // If canvas correction completed successfully, return the corrected canvas.
    // Otherwise, return the first error which stopped successful canvas correction.
    let checkCanvasWasCorrected (compsRes: Result<Component,SimulationError> list,connsRes: Result<Connection,SimulationError> list) =
        let comps,compErrors = partitionResults compsRes
        let conns,connErrors = partitionResults connsRes
        match compErrors,connErrors with
        | [],[] -> Ok (comps,conns)
        | e::tl,_ -> Error e
        | _,e::tl -> Error e

    (components,connections)
    |> removeDuplicateConnections
    |> addExtraConnections
    |> addExtraIOs
    |> checkCanvasWasCorrected

// Used to store last canvas state, the last corrected canvas, and the selected simulation
type SelectionCache = {
    UncorrectedCanvas: CanvasState
    CorrectedCanvas: CanvasState
    StoredResult: Result<SimulationData, SimulationError>
}

let emptySelCache  = {
    UncorrectedCanvas = ([],[])
    CorrectedCanvas = ([],[])
    StoredResult = Ok {
        FastSim = 
            printfn "creating empty selcache"
            FastCreate.simulationPlaceholder
        Graph = Map.empty 
        Inputs = []
        Outputs = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
}

let mutable selCache: SelectionCache = emptySelCache

/// Make and return Simulation Data (or Simulation Error) for the model for selected components.
/// Identical functionality to SimulationView.makeSimData, but only considers selected components.
/// Includes memoization: if the selected components have not changed then the cached corrected
/// canvas and simulation are returned.
let makeSimDataSelected (model:Model) : (Result<SimulationData,SimulationError> * CanvasState) option =
    let (selComponents,selConnections) = model.Sheet.GetSelectedCanvasState
    let wholeCanvas = model.Sheet.GetCanvasState()
    match selComponents, selConnections, model.CurrentProj with
    | _,_,None -> None
    | [],[],_ -> None
    | [],_,_ -> 
        let affected =
            selConnections
            |> List.map (fun c -> ConnectionId c.Id)
        Some <| (Error { 
            ErrType = WrongSelection "Only connections selected. Please select a combination of connections and components."
            InDependency = None
            ComponentsAffected = []
            ConnectionsAffected = affected },  (selComponents,selConnections))
    | selComps,selConns,Some project ->
        let state = (selComponents,selConnections)
        // Check if selected components have changed
        if stateIsEqual state selCache.UncorrectedCanvas then
            Some (selCache.StoredResult, selCache.CorrectedCanvas)
        else
            let selLoadedComponents =
                project.LoadedComponents
                |> List.filter (fun comp ->
                    comp.Name <> project.OpenFileName)
            match correctCanvasState (selComps,selConns) wholeCanvas with
            | Error e -> 
                selCache <- {
                    UncorrectedCanvas = state
                    CorrectedCanvas = (selComponents,selConnections)
                    StoredResult = Error e 
                }
                Some (Error e, (selComps,selConns))
            | Ok (correctComps,correctConns) ->
                match CanvasStateAnalyser.analyseState (correctComps,correctConns) selLoadedComponents with
                | Some e, _ -> Some(Error e, (correctComps, correctConns))
                | None, _ ->
                    let sim =
                        startCircuitSimulationFData
                            2
                            project.OpenFileName 
                            (correctComps,correctConns) 
                            selLoadedComponents
                    let newState = (correctComps,correctConns)
                    selCache <- {
                        UncorrectedCanvas =  state
                        CorrectedCanvas = newState
                        StoredResult = sim
                    }
                    Some (sim, newState)

//-------------------------------------------------------------------------------------//
//----------View functions for Truth Tables and Tab UI components----------------------//
//-------------------------------------------------------------------------------------//

let makeSortingArrows (io: CellIO) sortInfo dispatch =
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
                    (io,Ascending) |> Some |> SetTTSortType |> dispatch)
            ]
            [str "▲"]
    let downArrow =
        Button.button
            [
                Button.Props [sortArrowStyle]
                if downSel then Button.Color IsInfo
                Button.OnClick (fun _ -> 
                    (io,Descending) |> Some |> SetTTSortType |> dispatch)
            ]
            [str "▼"]
    div [] [upArrow; downArrow]

let makeColumnMoveArrows (io: CellIO) headingEl dispatch =
    let leftArrow = 
        Button.button
            [
                Button.Props [colMoveArrowStyle]
                Button.OnClick (fun _ -> 
                    (io,MLeft) |> MoveColumn |> dispatch)
            ]
            [str "<"]
    let rightArrow = 
        Button.button
            [
                Button.Props [colMoveArrowStyle]
                Button.OnClick (fun _ -> 
                    (io,MRight) |> MoveColumn |> dispatch)
            ]
            [str ">"]
    div [] [
            makeElementLine [leftArrow] [rightArrow] 
            headingEl
        ]

let private makeMenuGroup openDefault title menuList =
    details [Open openDefault] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let viewOutputHider table hidden dispatch =
    let makeToggleRow io =
        let isChecked = not <| List.contains io hidden
        let changeAction = (fun _ ->
            dispatch <| ToggleHideTTColumn io)
        let toggle = makeOnOffToggle isChecked changeAction "Visible" "Hidden"
        let ioLabel = str io.getLabel
        makeElementLine [ioLabel;toggle]
    if table.FilteredMap.IsEmpty then
        div [] [str "No Rows in Truth Table"]
    else
        let preamble = div [] [
            str "Hide or Un-hide Output or Viewer columns in the Truth Table."
            br []; br []
        ]
        let toggleRows =
            table.TableMap
            |> Map.toList
            |> List.head
            |> snd
            |> List.map (fun cell -> makeToggleRow cell.IO [])
        div [] (preamble::toggleRows)

let viewCellAsHeading dispatch sortInfo (styleInfo: Map<CellIO,CSSProp list>) (cell: TruthTableCell) =
    let addMoveArrows el = makeColumnMoveArrows cell.IO el dispatch
    let cellStyle =
        match Map.tryFind cell.IO styleInfo with
        | None -> failwithf "what? IO %A not found in Grid Styles" cell.IO
        | Some s -> Style <| (FontWeight "bold")::(s @ [BorderBottom "3px solid black"])
    match cell.IO with
    | SimIO (_,label,_) ->
        let headingText = string label
        div [cellStyle] 
            [
               makeElementLine [(str headingText)] //[makeColumnMoveArrows cell.IO (str headingText) dispatch] 
                    [makeSortingArrows cell.IO sortInfo dispatch]
               |> addMoveArrows
            ] 
    | Viewer ((label,fullName), width) ->
        let headingEl =
            label |> string |> str
            |> (fun r -> if fullName <> "" then addToolTipTop fullName r else r)
        div [cellStyle] [
            makeElementLine [headingEl]  //[makeColumnMoveArrows cell.IO headingEl dispatch] 
                [makeSortingArrows cell.IO sortInfo dispatch]
            |> addMoveArrows
            ]

let viewRowAsData numBase styleInfo i (row: TruthTableCell list) =
    let viewCellAsData (cell: TruthTableCell) =
        let cellStyle =
            match Map.tryFind cell.IO styleInfo, i%2 with
            | None, _ -> failwithf "what? IO %A not found in Grid Styles" cell.IO
            | Some s, 1 -> Style <| (BackgroundColor "whitesmoke")::s
            | Some s, _ -> Style s
        match cell.Data with
        | Bits [] -> failwithf "what? Empty WireData in TruthTable"
        | Bits [bit] -> div [cellStyle] [str <| bitToString bit]
        | Bits bits ->
            let width = List.length bits
            let value = viewFilledNum width numBase <| convertWireDataToInt bits
            div [cellStyle] [str value]
        | Algebra a -> div [cellStyle] [str <| a]
        | DC -> div [cellStyle] [str <| "X"]

    let cells =
        row
        |> List.map viewCellAsData
    cells

let viewTruthTableError simError =
    let error =
        match simError.InDependency with
        | None ->
            div [] [
                str (errMsg simError.ErrType)
                br []
                str <| "Please fix the error and retry."
            ]
        | Some dep ->
            div [] [
                str <| "Error found in dependency \"" + dep + "\":"
                br []
                str (errMsg simError.ErrType)
                br []
                str <| "Please fix the error in the dependency and retry."
            ]
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        error
    ]


let viewTruthTableData (table: TruthTable) (model:Model) dispatch =
    match model.TTConfig.GridCache with
    | Some grid -> 
        grid
    | None ->
        let tLst = table.SortedListRep
        let sortInfo = model.TTConfig.SortType
        let styleInfo = model.TTConfig.GridStyles
        if tLst.IsEmpty then
            div [] [str "No Rows in Truth Table"]
        else
            let headings =
                tLst.Head
                |> List.map (viewCellAsHeading dispatch sortInfo styleInfo) 
            let body =
                tLst
                |> List.mapi (viewRowAsData table.TableSimData.NumberBase styleInfo)
                |> List.concat

            let all = headings @ body
            let grid = div [(ttGridContainerStyle model)] all
            dispatch <| SetTTGridCache (Some grid)
            grid

let restartTruthTable canvasState model dispatch = fun _ ->
    let ttDispatch (ttMsg: TTMsg) : Unit = dispatch (TruthTableMsg ttMsg)

    let wholeSimRes = SimulationView.simulateModel None 2 canvasState model
    match wholeSimRes with
    | Error simError, _ ->
        SimulationView.setSimErrorFeedback simError model dispatch
    | Ok _, _ -> ()
    GenerateTruthTable (Some wholeSimRes) |> ttDispatch

let viewTruthTable canvasState model dispatch =
    let ttDispatch (ttMsg: TTMsg) : Unit = dispatch (TruthTableMsg ttMsg)

    // Truth Table Generation for selected components requires all components to have distinct labels.
    // Older Issie versions did not have labels for MergeWires and SplitWire components.
    // This step is needed for backwards compatability with older projects.
    updateMergeSplitWireLabels model dispatch

    match model.CurrentTruthTable with
    | None ->
        let wholeSimRes = SimulationView.simulateModel None 2 canvasState model
        let wholeButton =
            match wholeSimRes with
            | Error simError,_ ->
                Button.button
                    [
                        Button.Color IColor.IsWarning
                        Button.OnClick (fun _ ->
                            SimulationView.setSimErrorFeedback simError model dispatch
                            GenerateTruthTable (Some wholeSimRes) |> ttDispatch)
                    ] [str "See Problems"]
            | Ok sd,_ ->
                if sd.IsSynchronous = false then
                    Button.button
                        [
                            Button.Color IColor.IsSuccess
                            Button.OnClick (fun _ -> GenerateTruthTable (Some wholeSimRes) |> ttDispatch)
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
            | Some (Error simError,_) ->
                Button.button
                    [
                        Button.Color IColor.IsWarning
                        Button.OnClick (fun _ ->
                            SimulationView.setSimErrorFeedback simError model dispatch
                            GenerateTruthTable selSimRes |> ttDispatch)
                    ] [str "See Problems"]
            | Some (Ok sd,_) ->
                if sd.IsSynchronous = false then
                    Button.button
                        [
                            Button.Color IColor.IsSuccess
                            Button.OnClick (fun _ -> GenerateTruthTable selSimRes |> ttDispatch)
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
        let closeTruthTable _ =
            dispatch <| Sheet (SheetT.ResetSelection) // Remove highlights.
            dispatch ClosePropertiesNotification
            ttDispatch CloseTruthTable
        let body =
            match tableopt with
            // | Error e -> viewTruthTableError e
            | Error e -> SimulationView.viewSimulationError canvasState e model TruthTable dispatch
            | Ok table -> 
                let truncation =
                    Notification.notification [Notification.Color IsWarning; Notification.IsLight] [
                        str "Due to a large number of input combinations, caused by inputs that are
                            too wide or too numerous, the truth table has been truncated. Please use
                            more restrictive input constraints, or set wider inputs as algebraic variables."
                    ]
                div [] [
                    if table.IsTruncated then
                        truncation
                    viewReductions table model dispatch
                    br []; br []
                    viewTruthTableData table model ttDispatch]
               
        let constraints =
            match tableopt with
            | Error _ -> div [] []
            | Ok _ -> div [] [viewConstraints model dispatch]
        let hidden =
            match tableopt with
            | Error _ -> div [] []
            | Ok table -> div [] [viewOutputHider table model.TTConfig.HiddenColumns ttDispatch]
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
