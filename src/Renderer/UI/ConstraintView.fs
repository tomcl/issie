//(*
//    ConstraintView.fs
//
//    View functions for constraints and constraint popup
//*)
//
module ConstraintView

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

let inCon2str con =
    match con with
    | Equality e -> 
        let (_,label,_) = e.IO
        //$"{label} = {e.Value}" 
        sprintf "%s = 0x%X" (string label) e.Value
    | Inequality i -> 
        let (_,label,_) = i.IO
        //$"{i.LowerBound} \u2264 {label} \u2264 {i.UpperBound}"
        sprintf "0x%X \u2264 %s \u2264 0x%X" i.LowerBound (string label) i.UpperBound

let makeElementLine (els: ReactElement list) =
    let itemList =
        els
        |> List.map (fun el -> Level.item [] [el])
    
    Level.level [] [
            Level.left [] itemList
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


let validateNumericalConstraint (con: Constraint) (allConstraints: ConstraintSet)
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

let dialogPopupNumericalConBody (simIOs: SimulationIO list) existingCons model dispatch =
        fun (dialogData: PopupDialogData) -> //div [] []
            let selected =
                match dialogData.ConstraintIOSel with
                | None -> 
                    // Default IO is the first in the Truth Table
                    simIOs.Head |> Some |> SetPopupConstraintIOSel |> dispatch
                    simIOs.Head
                | Some io -> io

            let ioSelect =
                let buttons =
                    simIOs
                    |> List.map (fun io ->
                        let (_,label,_) = io
                        let action = (fun _ -> 
                            io |> Some |> SetPopupConstraintIOSel |> dispatch
                            dispatch <| SetPopupConstraintErrorMsg None)
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

let viewConstraints model dispatch =
    let inputCons = model.TTInputConstraints
    let outputCons = model.TTOutputConstraints
    let addButton action =
        Button.button [ Button.OnClick action] [str "Add"]
    let clearButton action =
        Button.button [Button.OnClick action] [str "Clear All"]
    //printfn "%A" model.TTOutputConstraints
    div [] 
        [
            Heading.h6 [] [str "Input Constraints"]
            viewNumericalConstraints inputCons dispatch
            br []
            makeElementLine [
                addButton (fun _ -> createInputConstraintPopup model dispatch) 
                clearButton (fun _ -> 
                    dispatch ClearInputConstraints
                    Regenerate |> Some |> SetTTOutOfDate |> dispatch)]
            Heading.h6 [] [str "Output Constraints"]
            viewNumericalConstraints outputCons dispatch
            br []
            makeElementLine [
                addButton (fun _ -> createOutputConstraintPopup model dispatch)
                clearButton (fun _ -> 
                    dispatch ClearOutputConstraints
                    Refilter |> Some |> SetTTOutOfDate |> dispatch)]
        ]
