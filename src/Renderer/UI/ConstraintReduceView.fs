//(*
//    ConstraintReduceView.fs
//
//    View functions for constraints + constraint popups, and reduction popups
//*)
//
module ConstraintReduceView

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
open TruthTableReduce

let addToolTipTop tip react =
        div [
            HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-top"
            Tooltip.dataTooltip tip
        ] [react]

let addToolTipRight tip react =
        div [
            HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-right"
            Tooltip.dataTooltip tip
        ] [react]

//-------------------------------------------------------------------------------------//
//-----------View/Helper Functions for Constraints & Constraint Popups-----------------//
//-------------------------------------------------------------------------------------//

/// Convert a constraint to its textual representation
let inCon2str con =
    match con with
    | Equality e ->
        sprintf "%s = 0x%X" (e.IO.getLabel) e.Value
    | Inequality i ->
        sprintf "0x%X \u2264 %s \u2264 0x%X" i.LowerBound (i.IO.getLabel) i.UpperBound

let makeElementLine (elsLeftAlign: ReactElement list) (elsRightAlign: ReactElement list)=
    let itemListLeft =
        elsLeftAlign
        |> List.map (fun el -> Level.item [] [el])

    let itemListRight =
        elsRightAlign
        |> List.map (fun el -> Level.item [] [el])

    Level.level [] [
            Level.left [] itemListLeft
            Level.right [] itemListRight
        ]

let viewNumericalConstraints cons dispatch =
    let makeConTag(con: Constraint) =
        let tagText = inCon2str con
        Tag.tag [Tag.Color IsInfo; Tag.IsLight] [
                str tagText
                Delete.delete
                    [Delete.OnClick(fun _ ->
                        dispatch <| DeleteInputConstraint con
                        dispatch <| DeleteOutputConstraint con
                        Regenerate |> Some |> SetTTOutOfDate |> dispatch)] []
            ]

    let equEls =
        cons.Equalities
        |> List.map(fun con ->
            con
            |> Equality
            |> makeConTag)
    let inequEls =
        cons.Inequalities
        |> List.map(fun con ->
            con
            |> Inequality
            |> makeConTag)
    let tags = List.append equEls inequEls
    Tag.list [] tags

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

/// Check if a newly added numerical constraint is valid and consistent with current constraints
let validateNumericalConstraint (con: Constraint) (allConstraints: ConstraintSet)
    : Result<Constraint,string> =
    let conText = inCon2str con
    match con with
    | Equality e ->
        // Check if given constraint already exists
        if List.contains e allConstraints.Equalities then
            Error <| sprintf "Constraint '%s' already exists." conText
        else
            (Ok e, allConstraints.Inequalities)
            ||> List.fold (fun state c ->
                // Check if given constraint overlaps with existing inequality constraints
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
        let width = ineq.IO.getWidth
        // Convert any negative numbers in the bounds to their unsigned equivalents
        let unsignedLower =
            (width,int64 ineq.LowerBound)
            ||> convertIntToWireData
            |> convertWireDataToInt
        let unsignedUpper =
            (width,int64 ineq.UpperBound)
            ||> convertIntToWireData
            |> convertWireDataToInt
        // Check that bounds are distinct and the right way round
        if unsignedLower = unsignedUpper then
            "Lower Bound and Upper Bound cannot have the same value."
            |> Error
        else if unsignedLower > unsignedUpper then
            "Lower Bound cannot have a greater value than Upper Bound"
            |> Error
        else
            // Check if given constraint overlaps with existing constraints
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

let dialogPopupNumericalConBody (cellIOs: CellIO list) existingCons model dispatch =
    fun (dialogData: PopupDialogData) -> //div [] []
        let selected =
            match dialogData.ConstraintIOSel with
            | None ->
                // Default IO is the first in the Truth Table
                cellIOs.Head |> Some |> SetPopupConstraintIOSel |> dispatch
                cellIOs.Head
            | Some io -> io

        let ioSelect =
            let buttons =
                cellIOs
                |> List.map (fun io ->
                    let action = (fun _ ->
                        io |> Some |> SetPopupConstraintIOSel |> dispatch
                        dispatch <| SetPopupConstraintErrorMsg None)
                    let buttonProps =
                        if io = selected then
                            [Button.Color IsPrimary; Button.OnClick action]
                        else
                            [Button.OnClick action]
                    Button.button buttonProps [str <| io.getLabel])
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
            if selected.getWidth = 1 then
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
                                    Equ |> Some |> SetPopupConstraintTypeSel |> dispatch
                                    dispatch <| SetPopupConstraintErrorMsg None)
                            ] [ str "Equality Constraint" ] ]
                            Control.div [] [ Button.button [
                                Button.Color (if not (isEqu x) then IsPrimary else NoColor)
                                Button.OnClick (fun _ ->
                                    Ineq |> Some |> SetPopupConstraintTypeSel |> dispatch
                                    dispatch <| SetPopupConstraintErrorMsg None)
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
                            dispatch <| SetPopupConstraintErrorMsg None
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
                            dispatch <| SetPopupConstraintErrorMsg None
                            (num |> Some |> SetPopupDialogInt2 |> dispatch)))]]

        let constraintEditor =
            match dialogData.ConstraintTypeSel, dialogData.ConstraintIOSel with
            | None, _ -> div [] []
            | _, None -> div [] []
            | Some Equ, Some io ->
                let label,width = io.getLabel,io.getWidth
                makeElementLine [
                    str <| sprintf "%s = "
                        (SimulationView.makeIOLabel (string label) width)
                    numField1 width
                ] []
            | Some Ineq, Some io ->
                let label,width = io.getLabel,io.getWidth
                makeElementLine [
                    numField1 width
                    str <| sprintf "\u2264 %s \u2264"
                        (SimulationView.makeIOLabel (string label) width)
                    numField2 width
                ] []

        let errorMsg =
            match dialogData.ConstraintErrorMsg with
            | None -> div [] []
            | Some msg -> div [] [hr []; str msg]


        match dialogData.Int, dialogData.Int2, dialogData.ConstraintErrorMsg,
        dialogData.ConstraintIOSel, dialogData.ConstraintTypeSel with
        | Some v, _, None, Some io, Some Equ ->
            let tentative = Equality {IO = io; Value = v}
            match validateNumericalConstraint tentative existingCons with
            | Error err ->
                err |> Some |> SetPopupConstraintErrorMsg |> dispatch
            | Ok c ->
                None |> SetPopupConstraintErrorMsg |> dispatch
                c |> Some |> SetPopupNewConstraint |> dispatch
        | Some lower, Some upper, None, Some io, Some Ineq ->
            let tentative = Inequality <| makeInequalityConstraint lower io (int upper)
            match validateNumericalConstraint tentative existingCons with
            | Error err ->
                err |> Some |> SetPopupConstraintErrorMsg |> dispatch
            | Ok c ->
                None |> SetPopupConstraintErrorMsg |> dispatch
                c |> Some |> SetPopupNewConstraint |> dispatch
        | _, _, _, _, _ ->
            None |> SetPopupNewConstraint |> dispatch

        div [] [
            Heading.h6 [] [str "Select Input"]
            ioSelect
            hr []
            Heading.h6 [] [str "Constraint Type"]
            typeSelect
            hr []
            Heading.h6 [] [str "Edit Constraint"]
            constraintEditor
            errorMsg
        ]

/// Popup for creating a new input constraint
let createInputConstraintPopup (model: Model) (dispatch: Msg -> Unit) =
    // Set Defaults
    0 |> Some |> SetPopupDialogInt |> dispatch
    (int64 0) |> Some |> SetPopupDialogInt2 |> dispatch
    Equ |> Some |> SetPopupConstraintTypeSel |> dispatch
    dispatch <| SetPopupConstraintIOSel None
    dispatch <| SetPopupNewConstraint None


    let title = "Add Input Constraint"
    let inputs =
        match model.CurrentTruthTable with
        | None -> failwithf "what? No current Truth Table when adding Constraints"
        | Some (Error _) -> failwithf "what? Constraint add option should not exist when there is TT error"
        | Some (Ok tt) ->
            tt.TableMap
            |> Map.toList
            |> List.map fst
            |> List.head
            |> List.map (fun cell -> cell.IO)
    let body = dialogPopupNumericalConBody inputs model.TTInputConstraints model dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData: PopupDialogData) ->
            match dialogData.NewConstraint with
            | None -> ()
            | Some con ->
                con |> AddInputConstraint |> dispatch
                Regenerate |> Some |> SetTTOutOfDate |> dispatch
                dispatch ClosePopup
    let isDisabled =
        fun (dialogData: PopupDialogData) ->
            match dialogData.ConstraintErrorMsg, dialogData.NewConstraint with
            | None, Some _ -> false
            | _, _ -> true
    dialogPopup title body buttonText buttonAction isDisabled dispatch

/// Popup for creating a new output constraint
let createOutputConstraintPopup (model: Model) (dispatch: Msg -> Unit) =
    // Set Defaults
    0 |> Some |> SetPopupDialogInt |> dispatch
    (int64 0) |> Some |> SetPopupDialogInt2 |> dispatch
    Equ |> Some |> SetPopupConstraintTypeSel |> dispatch
    dispatch <| SetPopupConstraintIOSel None
    dispatch <| SetPopupNewConstraint None

    let title = "Add Output Constraint"
    let outputs =
        match model.CurrentTruthTable with
        | None -> failwithf "what? No current Truth Table when adding Constraints"
        | Some (Error _) -> failwithf "what? Constraint add option should not exist when there is TT error"
        | Some (Ok tt) ->
            tt.TableMap
            |> Map.toList
            |> List.map snd
            |> List.head
            |> List.map (fun cell -> cell.IO)
    let body = dialogPopupNumericalConBody outputs model.TTInputConstraints model dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData: PopupDialogData) ->
            match dialogData.NewConstraint with
            | None -> ()
            | Some con ->
                con |> AddOutputConstraint |> dispatch
                Refilter |> Some |> SetTTOutOfDate |> dispatch
                dispatch ClosePopup
    let isDisabled =
        fun (dialogData: PopupDialogData) ->
            match dialogData.ConstraintErrorMsg, dialogData.NewConstraint with
            | None, Some _ -> false
            | _, _ -> true
    dialogPopup title body buttonText buttonAction isDisabled dispatch

/// View function for the constraints section on the right-tab
let viewConstraints model dispatch =
    let inputCons = model.TTInputConstraints
    let outputCons = model.TTOutputConstraints
    let addButton action =
        Button.button [ Button.OnClick action] [str "Add"]
    let clearButton action =
        Button.button [Button.OnClick action] [str "Clear All"]
    div []
        [
            str "Filter Rows in the Truth Table using Input or Output constraints"
            br []; br []
            Heading.h6 [] [str "Input Constraints"]
            viewNumericalConstraints inputCons dispatch
            br []
            makeElementLine [
                addButton (fun _ -> createInputConstraintPopup model dispatch)
                clearButton (fun _ ->
                    dispatch ClearInputConstraints
                    Regenerate |> Some |> SetTTOutOfDate |> dispatch)] []
            Heading.h6 [] [str "Output Constraints"]
            viewNumericalConstraints outputCons dispatch
            br []
            makeElementLine [
                addButton (fun _ -> createOutputConstraintPopup model dispatch)
                clearButton (fun _ ->
                    dispatch ClearOutputConstraints
                    Refilter |> Some |> SetTTOutOfDate |> dispatch)] []
        ]

//-------------------------------------------------------------------------------------//
//-------------------View/Helper Functions for Reduction Popups------------------------//
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

/// Checks if changing an input to Algebra/Values results in a valid simulation
/// (i.e. an AlgebraNotImplemented exception is not raised)
let validateAlgebraInput (io: SimulationIO) (fsi: FSInterface) (tableSD: SimulationData) =
    let (cid,_,_) = io
    try
        FastRun.changeInput cid fsi tableSD.ClockTickNumber tableSD.FastSim
        Ok 0 // Value of the result doesn't matter here, just that it is Ok.
    with
    | AlgebraNotImplemented err -> Error err

let dialogPopupReductionBody inputs tableSD (dispatch: Msg -> unit) =
    fun (dialogData: PopupDialogData) ->
        let explanation = 
            str <|
            "Reduce the Truth Table down into something more compact and informative by setting
            certain inputs to your logic as algebra. The resultant new Truth Table will show
            outputs as a function of your inputs. As algebraic inputs are directly evaluated by 
            the simulator, they are a great way to avoid Truth Table truncation." 
        let algInputs =
            match dialogData.AlgebraInputs with
            | Some l -> l
            | None -> failwithf "what? PopupDialogData.AlgebraInputs is None in popup body"
        let toggleAction io =
            fun _ -> dispatch <| TogglePopupAlgebraInput (io,tableSD)
        let toggles =
            inputs
            |> List.map (fun io ->
                let state = not(List.contains io algInputs)
                let toggle = makeOnOffToggle state (toggleAction io) "Values" "Algebra"
                let (_,label,_) = io
                makeElementLine [(str <| string label);toggle] [])
                |> div []
        let error = 
            match dialogData.AlgebraError with
            | None -> div [] []
            | Some {Msg=m;InDependency=(Some d);ComponentsAffected=_;ConnectionsAffected=_} ->
                div [] [
                    str m
                    br []
                    str $"Component in question: {d}"
                ]
            | _ -> 
                failwithf "what? SimError for AlgebraNotImplemented should always have an
                    InDependency field. Error: %A" dialogData.AlgebraError
                
        div [] [
            explanation
            hr []
            toggles
            hr []
            error
        ]

let createAlgReductionPopup model dispatch =
    let title = "Reduction using Algebraic Inputs"
    let inputs, tableSD =
        match model.CurrentTruthTable with
        | None -> failwithf "what? No current Truth Table when reducing"
        | Some (Error _) -> failwithf "what? Reduction option should not exist when there is TT error"
        | Some (Ok tt) ->
            tt.TableMap
            |> Map.toList
            |> List.map fst
            |> List.head
            |> (List.map (fun cell ->
                match cell.IO with
                | SimIO s -> s
                | Viewer _ -> failwithf "what? Found viewer in Truth Table inputs")),
                tt.TableSimData
    let body = dialogPopupReductionBody inputs tableSD dispatch
    let buttonText = "Apply"
    let buttonAction =
        fun (dialogData: PopupDialogData) ->
            match dialogData.AlgebraInputs with
            | None -> failwithf "what? what? PopupDialogData.AlgebraInputs is None in popup body"
            | Some l -> 
                dispatch <| SetTTAlgebraInputs l
                Regenerate |> Some |> SetTTOutOfDate |> dispatch
                dispatch ClosePopup
    let isDisabled =
        fun (dialogData: PopupDialogData) ->
            match dialogData.AlgebraInputs, dialogData.AlgebraError with
            | _, Some _ | None, _ | Some [], _ -> true
            | Some lst, None -> false
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let viewReductions (table: TruthTable) (model: Model) dispatch =
    let startReducing () =
        reduceTruthTable model.TTInputConstraints table model.TTBitLimit
        |> Ok
        |> GenerateTruthTable
        |> dispatch
        HideColumn |> Some |> SetTTOutOfDate |> dispatch
    let goBackButton = 
        match table.DCMap, model.TTAlgebraInputs with
        | Some _, _::_ -> failwithf "what? Table cannot be DC Reduced and Algebraic"
        | Some _, [] ->
            (Button.button [Button.Color IsInfo; Button.OnClick (fun _ -> 
                dispatch ClearDCMap; HideColumn |> Some |> SetTTOutOfDate |> dispatch)]
            [str "Back to Full Table"])
        | None, _::_ ->
            (Button.button [Button.Color IsInfo; Button.OnClick (fun _ ->
                dispatch <| SetTTAlgebraInputs []
                dispatch <| SetPopupAlgebraInputs (Some [])
                Regenerate |> Some |> SetTTOutOfDate |> dispatch)]
            [str "Back to Numeric Table"])
        | None, [] -> div [] [] // Button is never displayed in this case
    let reduceButton =
        if table.IsTruncated then
            let textEl = 
                str "Reduce"
                |> addToolTipRight "DC Reduction unavailable for truncated tables"
            (Button.button [Button.Disabled true; Button.OnClick (fun _ -> startReducing())]
            [textEl])
        else
            (Button.button [Button.Color IsSuccess; Button.OnClick (fun _ -> startReducing())]
            [str "Reduce"])
    let algebraButton =
        Button.button [Button.Color IsSuccess; Button.OnClick (fun _ -> createAlgReductionPopup model dispatch)]
            [str "Algebra"]
    match table.DCMap, model.TTAlgebraInputs with
    | None, [] -> // Table is neither DC Reduces or Algebraic
        div [] [
            makeElementLine [reduceButton; str "   ";algebraButton] []]
    | Some dc, [] -> // Table is DC Reduced
        div [] [
            goBackButton]
    | None, _::_ -> // Table is Algebraic
        div [] [
            makeElementLine [goBackButton; str "   ";algebraButton] []]
    | Some _, _::_ -> failwithf "what? Table cannot be DC Reduced and Algebraic"
