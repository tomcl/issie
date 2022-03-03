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

let labelFromIO (sIO: SimulationIO) =
    let (_,label,_) = sIO
    string label

let regenerateTruthTable model (dispatch: Msg -> Unit) =
    match model.CurrentTruthTable with
    | None -> failwithf "what? Adding constraint when no Truth Table exists"
    | Some (Error e) -> 
        failwithf "what? Constraint add option should not exist when there is TT error"
    | Some (Ok table) ->
        truthTableRegen table.TableSimData model.TTInputConstraints model.TTBitLimit
        |> Ok
        |> GenerateTruthTable
        |> dispatch


let tableAsList (table: TruthTable): TruthTableRow list =
    table.TableMap
    |> Map.toList
    |> List.map (fun (lhs,rhs) -> List.append lhs rhs)

let viewCellAsHeading (cell: TruthTableCell) = 
    let (_,label,_) = cell.IO
    let headingText = string label
    th [ ] [str headingText]

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
    if table.TableMap.IsEmpty then // Should never be matched
        div [] [str "No Truth Table to Display"]
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

let makeElementLine (els: ReactElement list) =
    let itemList =
        els
        |> List.map (fun el -> Level.item [] [el])
    
    Level.level [] [
            Level.left [] itemList
        ]

let truncationWarning table =
    $"The Truth Table has been truncated to {table.TableMap.Count} input combinations. 
    Not all rows may be shown. Please use more restrictive input constraints to avoid truncation."
   
let inCon2str con =
    match con with
    | Equality e -> 
        let (_,label,_) = e.IO
        $"{label} = {e.Value}" 
    | Inequality i -> 
        let (_,label,_) = i.IO
        $"{i.LowerBound} \u2264 {label} \u2264 {i.UpperBound}"

let viewInputConstraints inputCons dispatch =
    let makeInConTag(con: Constraint) =
        let tagText = inCon2str con
        Tag.tag [Tag.IsLight] [ 
                str tagText
                Delete.delete 
                    [Delete.OnClick(fun _ -> 
                        dispatch <| DeleteInputConstraint con
                        dispatch <| SetTTOutOfDate true)] []
            ]
        
    let equEls =
        inputCons.Equalities
        |> List.map(fun con ->
            con
            |> Equality
            |> makeInConTag)
    let inequEls =
        inputCons.Inequalities
        |> List.map(fun con ->
            con
            |> Inequality
            |> makeInConTag)
    let tags = List.append equEls inequEls
    div [] tags

let viewOutputConstraints outputCons dispatch =
    div [] [] // Not yet implemented

let constraintsOverlap (con1: Constraint) (con2: Constraint) =
    let equAndIneqOverlap (equ: EqualityConstraint) (ineq: InequalityConstraint) =
        equ.IO = ineq.IO && equ.Value >= ineq.LowerBound && equ.Value <= ineq.UpperBound
        
    let checkTwoIneq (in1: InequalityConstraint) (in2: InequalityConstraint) =
        in1.IO = in2.IO &&
        ((in1.LowerBound >= in2.LowerBound && in1.LowerBound <= in2.UpperBound)
        || (in1.UpperBound >= in2.LowerBound && in1.UpperBound <= in2.UpperBound))
    match con1, con2 with
    | Equality c1, Equality c2 -> (c1 = c2)
    | Equality c1, Inequality c2 -> equAndIneqOverlap c1 c2
    | Inequality c1, Equality c2 -> equAndIneqOverlap c2 c1
    | Inequality c1, Inequality c2 ->
        (checkTwoIneq c1 c2) || (checkTwoIneq c2 c1)


let validateInputConstraint (con: Constraint) (allConstraints: ConstraintSet)
    : Result<Constraint,string> =
    let conText = inCon2str con
    match con with 
    | Equality e -> 
        if List.contains e allConstraints.Equalities then
            Error <| sprintf "Constraint '%s' already exists." conText
        else
            (Ok e, allConstraints.Inequalities)
            ||> List.fold (fun state c -> 
                match state with
                | Error err -> Error err
                | Ok eqc -> 
                    if constraintsOverlap con (Inequality c) then
                        let constr = inCon2str(Inequality c)
                        sprintf "This constraint overlaps with another constraint: %s. 
                        Please change your new constraint or delete the old one." constr
                        |> Error
                    else 
                        Ok eqc)
                    
            |> (function | Error err -> Error err | Ok eqc -> Ok (Equality eqc))
    | Inequality ineq ->
        let (_,_,width) = ineq.IO
        // Convert any negative numbers in the bounds to their unsigned equivalents
        let unsignedLower = 
            (width,int64 ineq.LowerBound) 
            ||> convertIntToWireData
            |> convertWireDataToInt
        let unsignedUpper = 
            (width,int64 ineq.UpperBound) 
            ||> convertIntToWireData
            |> convertWireDataToInt
        if unsignedLower = unsignedUpper then
            "Lower Bound and Upper Bound cannot have the same value."
            |> Error
        else if unsignedLower > unsignedUpper then
            "Lower Bound cannot have a greater value than Upper Bound"
            |> Error
        else
            let checkWithEqu =
                (Ok ineq, allConstraints.Equalities)
                ||> List.fold (fun state c ->
                    match state with 
                    | Error err -> Error err
                    | Ok ineqc ->
                        if constraintsOverlap con (Equality c) then
                            let constr = inCon2str(Equality c)
                            sprintf "This constraint overlaps with another constraint: %s. 
                            Please change your new constraint or delete the old one." constr
                            |> Error
                        else 
                            Ok ineqc)
            (checkWithEqu,allConstraints.Inequalities)
            ||> List.fold (fun state c ->
                    match state with
                    | Error err -> Error err
                    | Ok ineqc ->
                        if constraintsOverlap con (Inequality c) then
                            let constr = inCon2str(Inequality c)
                            sprintf "This constraint overlaps with another constraint: %s. 
                            Please change your new constraint or delete the old one." constr
                            |> Error
                        else 
                            Ok ineqc)
            |> (function | Error err -> Error err | Ok ineqc -> Ok (Inequality ineqc))

let createInputConstraintPopup (model: Model) (dispatch: Msg -> Unit) =
    0 |> Some |> SetPopupDialogInt |> dispatch
    (int64 0) |> Some |> SetPopupDialogInt2 |> dispatch
    let dialogPopupInConBody =
        fun (dialogData: PopupDialogData) -> //div [] []
            let inputs =
                match model.CurrentTruthTable with
                | None -> failwithf "what? No current Truth Table when adding Input Constraints"
                | Some (Error _) -> failwithf "what? Constraint add option should not exist when there is TT error"
                | Some (Ok tt) ->
                    tt.TableMap
                    |> Map.toList
                    |> List.map fst 
                    |> List.head
                    |> List.map (fun cell -> cell.IO)

            let selected =
                match dialogData.ConstraintIOSel with
                | None -> 
                    // Default IO is the first in the Truth Table
                    inputs.Head |> Some |> SetPopupConstraintIOSel |> dispatch
                    inputs.Head
                | Some io -> io

            let inputSelect =
                let buttons =
                    inputs
                    |> List.map (fun io ->
                        let (_,label,_) = io
                        let action = (fun _ -> io |> Some |> SetPopupConstraintIOSel |> dispatch)
                        let buttonProps =
                            if io = selected then
                                [Button.Color IsPrimary; Button.OnClick action]
                            else 
                                [Button.OnClick action]
                        Button.button buttonProps [str <| (string label)])
                div [] buttons
            (*        
            // Code for original input selection interface
            // Works, but cannot support a large number of inputs
            // as they get cut off.
            let menuItem sIO =
                Menu.Item.li [
                    Menu.Item.IsActive (sIO = selected)
                    Menu.Item.OnClick (fun _ -> 
                        sIO |> Some |> SetPopupConstraintIOSel |> dispatch) 
                    ] [str <|(labelFromIO sIO)] 

            let inputSelect =
                Dropdown.dropdown [ Dropdown.IsUp; Dropdown.IsHoverable] [ 
                    Dropdown.trigger [] [
                        Button.button [Button.Color IsPrimary; Button.IsLight] [
                            str <| (labelFromIO selected)
                        ] 
                    ]                            
                    Dropdown.menu [Props [Style [Width "300px"]]] [
                        Dropdown.content [Props [Style [ZIndex 1000]]] [
                            Dropdown.Item.div [] [
                                Menu.menu [Props [Style [OverflowY OverflowOptions.Scroll]]] [
                                    Menu.list [] (List.map menuItem inputs) 
                                ]]]]]
            *)
            let typeSelect =
                let (_,_,selwidth) = selected
                if selwidth = 1 then
                    Equ |> Some |> SetPopupConstraintTypeSel |> dispatch
                    Level.item [ Level.Item.HasTextCentered ] [
                        Field.div [ Field.HasAddonsCentered ] [
                            Control.div [] [ Button.button [
                                Button.Color (IsPrimary)
                                Button.OnClick (fun _ -> 
                                    Equ |> Some |> SetPopupConstraintTypeSel |> dispatch)
                            ] [ str "Equality Constraint" ] ]        
                        ]
                    ]
                else 
                    match dialogData.ConstraintTypeSel with
                    | None -> 
                        //Default Constraint Type is Equality Constraint
                        Equ |> Some |> SetPopupConstraintTypeSel |> dispatch
                        div [] []
                    | Some x ->
                        Level.item [ Level.Item.HasTextCentered ] [
                            Field.div [ Field.HasAddonsCentered ] [
                                Control.div [] [ Button.button [
                                    Button.Color (if isEqu x then IsPrimary else NoColor)
                                    Button.OnClick (fun _ -> 
                                        Equ |> Some |> SetPopupConstraintTypeSel |> dispatch)
                                ] [ str "Equality Constraint" ] ]
                                Control.div [] [ Button.button [
                                    Button.Color (if not (isEqu x) then IsPrimary else NoColor)
                                    Button.OnClick (fun _ -> 
                                        Ineq |> Some |> SetPopupConstraintTypeSel |> dispatch)
                                ] [ str "Inequality Constraint" ] ]        
                            ]
                        ]
            
            let numField1 width =
                Input.text [
                    Input.Key ("Hex")
                    Input.DefaultValue (viewNum Hex 0)
                    Input.Props [
                        constraintNumberStyle
                        OnChange (getTextEventValue >> (fun text ->
                            match strToIntCheckWidth width text, 
                            System.String.IsNullOrWhiteSpace text with
                            | _, true ->
                                "Blank constraint field" 
                                |> Some |> SetPopupConstraintErrorMsg |> dispatch
                            | Error err, _ ->
                                err |> Some |> SetPopupConstraintErrorMsg |> dispatch
                            | Ok num, _ ->
                                None |> SetPopupConstraintErrorMsg |> dispatch
                                (int num) |> Some |> SetPopupDialogInt |> dispatch))]]

            let numField2 width =
                Input.text [
                    Input.Key ("Hex")
                    Input.DefaultValue (viewNum Hex 0)
                    Input.Props [
                        constraintNumberStyle
                        OnChange (getTextEventValue >> (fun text ->
                            match strToIntCheckWidth width text, 
                            System.String.IsNullOrWhiteSpace text with
                            | _, true ->
                                "Blank constraint field" 
                                |> Some |> SetPopupConstraintErrorMsg |> dispatch
                            | Error err, _ ->
                                err |> Some |> SetPopupConstraintErrorMsg |> dispatch
                            | Ok num, _ ->
                                None |> SetPopupConstraintErrorMsg |> dispatch
                                (num |> Some |> SetPopupDialogInt2 |> dispatch)))]]
            
            let constraintEditor =
                match dialogData.ConstraintTypeSel, dialogData.ConstraintIOSel with
                | None, _ -> div [] []
                | _, None -> div [] []
                | Some Equ, Some io ->
                    let (_,label,width) = io
                    makeElementLine [
                        str <| sprintf "%s = " 
                            (SimulationView.makeIOLabel (string label) width)
                        numField1 width
                    ]
                | Some Ineq, Some io -> 
                    let (_,label,width) = io
                    makeElementLine [
                        numField1 width
                        str <| sprintf "\u2264 %s \u2264" 
                            (SimulationView.makeIOLabel (string label) width)
                        numField2 width
                    ]

            let errorMsg =
                match dialogData.ConstraintErrorMsg with
                | None -> div [] []
                | Some msg -> div [] [hr []; str msg]
            

            match dialogData.Int, dialogData.Int2, dialogData.ConstraintErrorMsg, 
            dialogData.ConstraintIOSel, dialogData.ConstraintTypeSel with
            | Some v, _, None, Some io, Some Equ -> 
                let tentative = Equality {IO = io; Value = v}
                match validateInputConstraint tentative model.TTInputConstraints with
                | Error err ->
                    err |> Some |> SetPopupConstraintErrorMsg |> dispatch
                | Ok c ->
                    None |> SetPopupConstraintErrorMsg |> dispatch
                    c |> Some |> SetPopupNewConstraint |> dispatch
            | Some lower, Some upper, None, Some io, Some Ineq ->
                let tentative = Inequality <| makeInequalityConstraint lower io (int upper)
                match validateInputConstraint tentative model.TTInputConstraints with
                | Error err ->
                    err |> Some |> SetPopupConstraintErrorMsg |> dispatch
                | Ok c ->
                    None |> SetPopupConstraintErrorMsg |> dispatch
                    c |> Some |> SetPopupNewConstraint |> dispatch
            | _, _, _, _, _ -> 
                None |> SetPopupNewConstraint |> dispatch
            
            div [] [
                Heading.h6 [] [str "Select Input"]
                inputSelect
                hr []
                Heading.h6 [] [str "Constraint Type"]
                typeSelect
                hr []
                Heading.h6 [] [str "Edit Constraint"]
                constraintEditor
                errorMsg
            ]
            

    let title = "Add Input Constraint"
    let body = dialogPopupInConBody
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData: PopupDialogData) ->
            match dialogData.NewConstraint with
            | None -> ()
            | Some con -> 
                con |> AddInputConstraint |> dispatch
                true |> SetTTOutOfDate |> dispatch
                dispatch ClosePopup
    let isDisabled =
        fun (dialogData: PopupDialogData) -> 
            match dialogData.ConstraintErrorMsg, dialogData.NewConstraint with
            | None, Some _ -> false
            | _, _ -> true
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let viewConstraints model dispatch =
    let inputCons = model.TTInputConstraints
    let outputCons = model.TTOutputConstraints
    let addButton action =
        Button.button [
                Button.OnClick action
                ]
            [str "Add"]
    let clearButton action =
        Button.button [Button.OnClick action]
            [str "Clear All"]
    
    div [] 
        [
            Heading.h6 [] [str "Input Constraints"]
            viewInputConstraints inputCons dispatch
            br []
            makeElementLine [
                addButton (fun _ -> createInputConstraintPopup model dispatch) 
                clearButton (fun _ -> 
                    dispatch ClearInputConstraints
                    dispatch <| SetTTOutOfDate true)]
            Heading.h6 [] [str "Output Constraints"]
            viewOutputConstraints outputCons dispatch
            br []
            makeElementLine [
                addButton (fun _ -> dispatch OpenOutConAdder)
                clearButton (fun _ -> dispatch ClearOutputConstraints)]
        ]

let viewTruthTable model dispatch =
    let generateTruthTable simRes =
        match simRes with 
        | Some (Ok sd,_) -> 
            truthTable sd model.TTInputConstraints model.TTBitLimit
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
        if model.TTIsOutOfDate then
            // Regenerate the truth table to be displayed in the next view cycle
            regenerateTruthTable model dispatch
            dispatch <| SetTTOutOfDate false
        let closeTruthTable _ =
            dispatch <| ClearInputConstraints
            dispatch <| ClearOutputConstraints
            dispatch CloseTruthTable
        let body = 
            match tableopt with
            | Error e -> viewTruthTableError e
            | Ok table ->
                if table.IsTruncated then
                    let popup = Notifications.warningPropsNotification (truncationWarning table)
                    dispatch <| SetPropertiesNotification popup
                viewTruthTableData table
        let constraints =
            match tableopt with
            | Error _ -> div [] []
            | Ok _ -> div [] [hr []; viewConstraints model dispatch]
        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick closeTruthTable ]
                [ str "Close Truth Table" ]
            br []; br []
            str "The Truth Table generator uses the diagram as it was at the moment of
                 pressing the \"Generate Truth Table\" button."
            constraints
            br []
            hr []
            body
            br []
            hr []
            ]