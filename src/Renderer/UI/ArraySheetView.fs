module ArraySheetView

(*
    ArraySheetView.fs

    Making an ARRAY COMPONENT, and editing the settings of one.

    An array component is a sheet whose hardware is several copies of what is drawn on it. For the
    UI it is a component like a Verilog component: it is made from the Catalogue, it appears there
    to be placed, and it has a sheet of its own in the Sheets menu which is opened to edit it.

    Two entry points, and nothing else in the UI knows about arrays:

      newArrayComponentPopup   the Catalogue's "New array component", which offers a new sheet, an
                               existing sheet made into one, or a copy of an existing sheet
      viewArraySettings        the block the Properties pane shows for the open sheet when it is
                               an array component: how many copies, and a way to stop being one

    See CommonTypes.ArrayInfo for what a sheet carries, ArrayExpand for what it means, and
    ArrayElaborate for what it becomes.
*)

open Fable.React
open Fable.React.Props
open Fulma

open CommonTypes
open ParameterTypes
open ModelType
open PopupHelpers
open ModelHelpers
open FilesIO
open MenuHelpers
open Notifications
// the open sheet's live canvas is the draw block's, not the LoadedComponent's
open Sheet.SheetInterface
open DrawModelType

/// The default loop variable of a new array component. A single letter, because it is written in
/// property boxes constantly - `i` beside a bus select's LSB is the whole idiom.
let private defaultLoopName = "i"

/// Whether the open sheet still holds components that only an array component may have.
let private arrayComponentsOn (model: Model) =
    model.Sheet.GetCanvasState()
    |> fst
    |> List.filter (fun comp -> isArrayOnlyComponent comp.Type)
    |> List.map (fun comp -> comp.Label)

/// Replace the open sheet's array settings, and bring the ports it derives into line with them.
///
/// The ports are recomputed here rather than left to the next canvas edit, because changing the
/// copy count changes them - an Output gives one port per copy - and everything that draws or
/// checks an instance of this sheet reads them. This is also the one edit in the feature that
/// reaches beyond its own sheet: instances elsewhere are then out of date, and get the dialog that
/// a sheet's ports changing already raises.
let private setArrayInfo (model: Model) (info: ArrayInfo option) (dispatch: Msg -> unit) =
    match model.CurrentProj with
    | None -> ()
    | Some project ->
        let canvas = model.Sheet.GetCanvasState()
        let update (lc: LoadedComponent) =
            match lc.Name = project.OpenFileName with
            | false -> lc
            | true ->
                let ins, outs =
                    CanvasExtractor.parseDiagramSignatureFor info lc.LCParameterSlots canvas
                { lc with ArrayInfo = info; InputLabels = ins; OutputLabels = outs }
        dispatch
        <| UpdateModel (fun m ->
            { m with
                CurrentProj =
                    Some { project with LoadedComponents = List.map update project.LoadedComponents } }
            // the canvas is untouched by a change to what the sheet IS, so say the sheet needs
            // saving rather than leaving it to be inferred from a canvas that is identical
            |> ParameterView.markSheetParamsChanged
            // an Output draws the loop variable, and a join its channel, only while the sheet IS an
            // array component - so both appear and disappear with these settings
            |> ModelHelpers.syncArrayTextOfOpenSheet)

        // Saved at once, and that is not tidiness. Changing what this sheet IS changes how many
        // ports it has, so every instance of it elsewhere is now out of date - and the dialog that
        // offers to bring them into line is raised by FinishUICmd, which only
        // saveOpenFileActionWithModelUpdate dispatches. Without this the sheet sat merely marked as
        // changed: the instances stayed wrong, and stayed wrong across a switch to another sheet,
        // until something else happened to save it.
        //
        // ExecFuncInMessage so that what gets saved is the model with the new settings in it - the
        // UpdateModel above has not been applied at the point this line runs.
        dispatch
        <| ExecFuncInMessage ((fun model' dispatch' ->
                MenuHelpers.saveOpenFileActionWithModelUpdate model' dispatch' |> ignore), dispatch)

//-------------------------------------------------------------------------------------------//
//--------------------------------THE PROPERTIES PANE BLOCK----------------------------------//
//-------------------------------------------------------------------------------------------//

/// What the Properties pane shows for the open sheet when it is an array component: nothing at all
/// when it is not, so an ordinary sheet's properties are unchanged by the feature existing.
///
/// The copy count is here rather than in a dialog because it is the sheet's most important
/// property - it is what the sheet MEANS - and the pane is where a sheet's properties are.
let viewArraySettings (model: Model) (dispatch: Msg -> unit) : ReactElement =
    match openSheetArrayInfo model with
    | None -> null
    | Some info ->
        let (ParamName loopName) = info.LoopParam
        let simIsOpen = ModelHelpers.simulationIsOpen model

        /// What is in the box: what has been typed on THIS sheet, or the sheet's real copy count
        /// where nothing has been. Model.ArrayCopiesTyped says why it is held in the model.
        let openName = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""
        let typedHere =
            match model.ArrayCopiesTyped with
            | Some (sheet, text) when sheet = openName -> Some text
            | _ -> None

        let setTyped (text: string option) =
            dispatch <| UpdateModel (fun m ->
                { m with ArrayCopiesTyped = text |> Option.map (fun t -> openName, t) })

        /// What a copy count has to be. The bound is Issie's own - the most copies it will expand -
        /// and the message quotes it rather than saying "too many", because a number is what the
        /// box holds and a number is what the user needs to be told.
        let copiesProblem (text: string) =
            match System.Int32.TryParse (text.Trim()) with
            | true, n when n >= 1 && n <= ArrayElaborate.Constants.maxArrayCopies -> None
            | _ ->
                let most = ArrayElaborate.Constants.maxArrayCopies
                Some $"Number of copies must be a number >= 1 and <= {most}"

        /// What is wrong with what has been typed, if anything: shown AS IT IS TYPED, not on
        /// leaving the box.
        ///
        /// The value applies on leaving, but the complaint cannot wait for that - a box that
        /// quietly dropped what it could not use left the user with a sheet still at the old count
        /// and nothing said about why. Every other properties box in Issie objects as you type, and
        /// this is that rule.
        let typedProblem = typedHere |> Option.bind copiesProblem

        /// Apply what has been typed, and go back to showing the sheet's own count.
        ///
        /// On leaving the box or on Enter, not on every keystroke: the number of copies decides how
        /// many ports the component has, so applying it per keystroke would make every instance of
        /// it out of date on the way to the number wanted. Something unusable - empty, 0, more than
        /// the limit - applies nothing, and the box goes back to saying what is true rather than
        /// sitting there showing a number the sheet does not have.
        let commit () =
            match typedHere with
            | None -> ()
            | Some text ->
                match System.Int32.TryParse (text.Trim()) with
                | true, n when n >= 1 && n <= ArrayElaborate.Constants.maxArrayCopies && n <> info.Copies ->
                    setArrayInfo model (Some { info with Copies = n }) dispatch
                | _ -> ()
                setTyped None

        let copyBox =
            div [] [
                PropertiesHelp.fieldLabel "Copies"
                Input.number [
                    Input.Props [
                        Style [ Width "120px" ]
                        Min 1
                        Max ArrayElaborate.Constants.maxArrayCopies
                        OnBlur (fun _ -> commit ())
                        // Enter is a canvas shortcut, so it is stopped here rather than left to
                        // reach the sheet behind the pane.
                        OnKeyDown (fun ev ->
                            if ev.key = "Enter" then
                                ev.stopPropagation()
                                commit ())
                    ]
                    // red while what is typed cannot be used, as every parameter box in the pane is
                    if typedProblem.IsSome then Input.Option.CustomClass "is-danger"
                    Input.Disabled simIsOpen
                    Input.Value (typedHere |> Option.defaultValue (string info.Copies))
                    Input.OnChange (fun ev -> setTyped (Some (JSHelpers.getTextEventValue ev)))
                ]
                // the class ParameterView.paramInputField puts its complaint in, so that a bad copy
                // count reads exactly as a bad width does two boxes further down
                div [ Class "propertyMessage" ] [ str (Option.defaultValue "" typedProblem) ]
            ]

        // Shown, and not editable. Every property expression on the sheet refers to it BY NAME - a
        // bus select whose LSB is `i` - so renaming it would silently break each of them, and there
        // is nothing here that could rewrite them.
        let loopBox =
            div [ Style [ MarginTop "6px" ] ] [
                PropertiesHelp.fieldLabel "Loop variable"
                Input.text [
                    Input.Props [ Style [ Width "120px" ] ]
                    Input.Disabled true
                    Input.Value loopName
                ]
            ]

        let explanation =
            p [ Style [ FontSize "0.85em"; Color "grey"; MarginTop "6px" ] ] [
                str $"This sheet's hardware is {info.Copies} copies of what is drawn on it. Write \
                      '{loopName}' in any property box to make one copy differ from the next: it \
                      counts 0 to {info.Copies - 1}."
                br []
                str "Changing the number of copies changes how many ports this component has, so \
                     every instance of it elsewhere will need updating."
            ]

        // Only once the components that say how the copies join up have gone: leaving them would
        // make a sheet that cannot be simulated, and deleting them silently would throw away work.
        let stopButton =
            match arrayComponentsOn model with
            | [] ->
                Button.button [
                    Button.Color IsDanger
                    Button.Size IsSmall
                    Button.Disabled simIsOpen
                    Button.Props [ Style [ MarginTop "8px" ] ]
                    Button.OnClick (fun _ -> setArrayInfo model None dispatch)
                ] [ str "Make this an ordinary sheet" ]
            | labels ->
                p [ Style [ FontSize "0.85em"; Color "grey"; MarginTop "8px" ] ] [
                    str ("This cannot become an ordinary sheet while it holds "
                         + (labels |> String.concat ", ")
                         + ": those components only mean something on an array component.")
                ]

        div [ Style [ MarginBottom "10px" ] ] [
            PropertiesHelp.fieldLabel "Array component"
            copyBox
            loopBox
            explanation
            stopButton
        ]

//-------------------------------------------------------------------------------------------//
//---------------------------------MAKING AN ARRAY COMPONENT---------------------------------//
//-------------------------------------------------------------------------------------------//

/// How many copies a newly made array component starts with.
let private defaultCopies = 4

/// The loop variable a dialog box holds, with the default where nothing has been typed.
///
/// Empty means the default rather than an error: the name matters only to whoever writes it in a
/// property box, `i` is what an index is called, and asking someone to type it before they know
/// what it is for would be a question with one sensible answer.
let private loopNameOf (defaultName: string) (typed: string) =
    if typed.Trim() = "" then defaultName else typed.Trim()

/// The properties a sheet already declares, by name. A loop variable may not be one of them.
let private declaredNames (sheet: LoadedComponent option) =
    sheet
    |> Option.map (ParameterView.getDefaultParamDefs >> Map.toList >> List.map (fst >> fun (ParamName n) -> n))
    |> Option.defaultValue []

/// The name a loop variable is offered under: `i`, unless the sheet has a property of that name
/// already.
///
/// `i` is what an index is called and is what nearly every array component will use. It can only be
/// taken in one case - a sheet that already declares properties being made into an array component
/// - so the alternatives are tried only there, and they are the next letters an index is called
/// before falling back to numbering. Offering a free name rather than a taken one means the dialog
/// opens on something that works, instead of on an error the user has to read and fix.
let private freeLoopName (sheet: LoadedComponent option) =
    let taken = declaredNames sheet
    Seq.append [defaultLoopName; "j"; "k"] (Seq.initInfinite (fun n -> $"{defaultLoopName}{n + 1}"))
    |> Seq.filter (fun name -> isValidParamName name && not (List.contains name taken))
    |> Seq.head

/// The labels of the components on a sheet whose slot expressions name the given variable.
///
/// A slot that names a variable the sheet does not have is a sheet that cannot be read, and Issie
/// does not let one be made: ParameterView.deleteParameterBox refuses to delete a property that is
/// still used and lists the slots using it. This is that same rule at the one other place a name
/// can change out from under its uses - copying an array component under a different loop variable
/// - and it reports the same thing, the components the name is used in.
let private componentsUsingName (sheet: LoadedComponent) (name: ParamName) =
    let labelOf =
        fst sheet.CanvasState |> List.map (fun c -> c.Id, c.Label) |> Map.ofList
    ParameterView.getParamSlots sheet
    |> ParameterTypes.slotsUsingParam name
    |> List.choose (fun (slot, _) -> Map.tryFind slot.CompId labelOf)
    |> List.distinct
    |> List.sort

/// Whether a loop variable name can be used on a sheet: one an expression could refer to, not a
/// property that sheet already declares - the two would be the same word meaning two things - and
/// not a rename away from a variable the sheet's own expressions still use.
let private loopNameProblem (sheet: LoadedComponent option) (defaultName: string) (typed: string) =
    let name = loopNameOf defaultName typed
    let sheetName = sheet |> Option.map (fun s -> $"'{s.Name}'") |> Option.defaultValue "this sheet"
    /// Renaming the loop variable does not rewrite the expressions that use it, so a copy made
    /// under a new name would carry expressions naming a variable it does not have. Only reachable
    /// when copying an array component: everywhere else the loop variable is fixed once and shown
    /// read-only.
    let renamedAwayFrom =
        match sheet |> Option.bind (fun s -> s.ArrayInfo |> Option.map (fun info -> s, info)) with
        | Some (s, info) when info.LoopParam <> ParamName name ->
            let (ParamName old) = info.LoopParam
            match componentsUsingName s info.LoopParam with
            | [] -> None
            | used -> Some (old, used)
        | _ -> None
    if not (isValidParamName name) then
        Some $"'{name}' cannot be a loop variable: a name is a letter followed by letters and digits, and not min, max or clog2"
    elif List.contains name (declaredNames sheet) then
        Some $"{sheetName} already has a property called '{name}', and one word cannot mean both - \
               the loop variable counts the copies, while a property is set once per instance. \
               Choose another name."
    else
        renamedAwayFrom
        |> Option.map (fun (old, used) ->
            let names = String.concat ", " used
            $"{sheetName} uses '{old}' in the properties of {names}, and renaming the loop variable \
               here does not rewrite them. Copy it as '{old}', or change those components on \
               {sheetName} first.")

/// Ask for a sheet name and a loop variable, then run `make` with both. Refuses a name the project
/// already has, or one the file system will not take, using the check the New Sheet dialog uses.
///
/// `sheet` is the one the new component's contents will come from, where there is one: a copy
/// carries that sheet's properties with it, so a loop variable clashing with one of them is a clash
/// on the sheet being made even though it does not exist yet. None for a component made empty,
/// which declares nothing for a name to collide with.
let private askForName
        title
        prompt
        (sheet: LoadedComponent option)
        (defaultLoop: string)
        (project: Project)
        (make: string -> string -> Model -> unit)
        model
        dispatch =
    let before1 =
        fun (dialogData: PopupDialogData) ->
            div [] [
                str prompt
                br []
                Option.defaultValue (div [] []) (MiscMenuView.maybeWarning (getText dialogData) project)
            ]
    let before2 =
        fun (dialogData: PopupDialogData) ->
            div [] [
                br []
                str "What should its loop variable be called? Write this in any property box on the \
                     sheet to make one copy differ from the next."
                br []
                match loopNameProblem sheet defaultLoop (getText2 dialogData) with
                | Some msg -> span [ Style [ Color "red" ] ] [ str msg ]
                | None -> null
            ]
    let body =
        dialogPopupBodyTwoTexts
            (before1, "Insert array component name")
            (before2, $"default: {defaultLoop}")
            dispatch
    let buttonAction =
        fun (model': Model) ->
            make
                ((getText model'.PopupDialogData).ToLower())
                (loopNameOf defaultLoop (getText2 model'.PopupDialogData))
                model'
            dispatch ClosePopup
            dispatch FinishUICmd
    let isDisabled =
        fun (model': Model) ->
            let text = getText model'.PopupDialogData
            text = "" || isFileInProject text project || (MiscMenuView.maybeWarning text project).IsSome
            || (loopNameProblem sheet defaultLoop (getText2 model'.PopupDialogData)).IsSome
    dialogPopup title body "Create" buttonAction isDisabled [] dispatch

/// Ask only for a loop variable, for a sheet that already exists and keeps its name.
let private askForLoopVariable title (sheet: LoadedComponent) (make: string -> Model -> unit) dispatch =
    // the name the box opens on, and what an empty box means: free of this sheet's own properties
    let defaultLoop = freeLoopName (Some sheet)
    let before =
        fun (dialogData: PopupDialogData) ->
            div [] [
                str $"'{sheet.Name}' will become an array component. What should its loop variable \
                      be called? Write this in any property box on the sheet to make one copy differ \
                      from the next."
                br []
                match loopNameProblem (Some sheet) defaultLoop (getText dialogData) with
                | Some msg -> span [ Style [ Color "red" ] ] [ str msg ]
                | None -> null
            ]
    let body = dialogPopupBodyOnlyText before $"default: {defaultLoop}" dispatch
    let buttonAction =
        fun (model': Model) ->
            make (loopNameOf defaultLoop (getText model'.PopupDialogData)) model'
            dispatch ClosePopup
            dispatch FinishUICmd
    let isDisabled =
        fun (model': Model) -> (loopNameProblem (Some sheet) defaultLoop (getText model'.PopupDialogData)).IsSome
    dialogPopup title body "Make it an array component" buttonAction isDisabled [] dispatch

/// Add a sheet to the project, with array settings, and open it for editing.
let private addArraySheet (project: Project) (name: string) (loopName: string) (canvasFrom: LoadedComponent option) model dispatch =
    match canvasFrom with
    | None -> ComponentLibraries.createEmptySheetFile project name |> displayAlertOnError dispatch
    | Some source ->
        // the copy gets fresh ids for everything, so it cannot clash with the sheet it came from
        ComponentLibraries.copySheetWithNewIds source.FilePath (ComponentLibraries.sheetFilePath project name)

    // A copy of an array component keeps ITS settings: copying one is how a second array of the
    // same shape is made, and starting the copy at four copies of a sixteen-copy original would be
    // a silent change to the thing being copied. Anything else starts at the default.
    let copiesOf =
        canvasFrom
        |> Option.bind (fun source -> source.ArrayInfo)
        |> Option.map copiesOfArray
        |> Option.defaultValue defaultCopies
    let info loop = { LoopParam = ParamName loop; Copies = copiesOf }
    match tryLoadComponentFromPath (ComponentLibraries.sheetFilePath project name) with
    | Error err -> displayFileErrorNotification err dispatch
    | Ok loaded ->
        // The array settings are put on here rather than written into the file, so that the one
        // place they are ever set is the same one the Properties pane uses. The ports follow from
        // them, so they are recomputed too.
        let ins, outs =
            CanvasExtractor.parseDiagramSignatureFor
                (Some (info loopName)) loaded.LCParameterSlots loaded.CanvasState
        let ldc =
            { loaded with
                Name = name
                ArrayInfo = Some (info loopName)
                InputLabels = ins
                OutputLabels = outs
                LoadedComponentIsOutOfDate = true }
        let project' =
            { project with LoadedComponents = ldc :: (project.LoadedComponents |> List.filter (fun l -> l.Name <> name)) }
        openFileInProject' true name project' model dispatch

/// The Catalogue's "New array component": what an array component IS, and the three ways to get
/// one. An existing sheet can become one, because the usual way to find out you want an array is to
/// have drawn one copy of it already.
let newArrayComponentPopup (model: Model) (dispatch: Msg -> unit) =
    match model.CurrentProj with
    | None -> Log.warn "newArrayComponentPopup called with no project open"
    | Some project ->

    /// The sheet the user is looking at, which is the one these act on. Only the user's own
    /// sheets: a library sheet is held at what it loaded with, and cannot become anything.
    let openSheet =
        project.LoadedComponents
        |> List.tryFind (fun lc -> lc.Name = project.OpenFileName && lc.Form = Some User)

    /// One of the ways in, as a button with a line saying what it does.
    let choice label description action =
        div [ Style [ MarginBottom "10px" ] ] [
            Button.button [
                Button.Color IsInfo
                Button.OnClick (fun _ -> dispatch ClosePopup; action ())
            ] [ str label ]
            p [ Style [ FontSize "0.85em"; Color "grey"; MarginTop "4px" ] ] [ str description ]
        ]

    let body =
        fun (_: Model) ->
            div [] [
                // First, and cut off from the rest: somebody who wanted the simple thing should be
                // able to read one paragraph and leave, rather than a page about the advanced one.
                UIPopups.helpText AppMessages.Confirm.arrayComponentIsAdvanced
                hr []
                UIPopups.helpText AppMessages.Confirm.usingArraySheets
                hr []
                choice "New array component"
                    "An empty sheet to draw one copy on."
                    (fun () ->
                        // No sheet behind it: an empty component declares no properties, so `i`
                        // is always free and nothing can clash with it.
                        askForName "New array component" "A new sheet will be created for it:"
                            None (freeLoopName None) project
                            (fun name loop model' -> addArraySheet project name loop None model' dispatch)
                            model dispatch)
                // Whichever of the two applies to the sheet the user is looking at, and not both:
                // what can be done to a sheet follows from what it IS, so offering the other one -
                // greyed out, or leading to a list of sheets - would be asking a question that has
                // only one answer. It also makes "the current sheet" in these descriptions true.
                (match openSheet with
                 | Some ({ ArrayInfo = None } as lc) ->
                    choice "Turn the current design sheet into an array component"
                        "The current sheet is used as one copy of the array: you can add array components to it."
                        (fun () ->
                            askForLoopVariable "Make an array component" lc
                                (fun loop _ ->
                                    // setArrayInfo works on the OPEN sheet and reads its live
                                    // canvas to derive the ports, which is this sheet - so nothing
                                    // has to be opened first. ExecFuncInMessage for the model as it
                                    // is when the dialog is answered, not as it was when it opened.
                                    dispatch
                                    <| ExecFuncInMessage ((fun model' dispatch' ->
                                            setArrayInfo model'
                                                (Some { LoopParam = ParamName loop; Copies = defaultCopies })
                                                dispatch'), dispatch))
                                dispatch)
                 | Some ({ ArrayInfo = Some info } as lc) ->
                    let (ParamName loop) = info.LoopParam
                    choice "Copy the current array component design sheet as a new array component"
                        "A copy of the current sheet is made with properties and content you can modify"
                        (fun () ->
                            // Saved first when there is anything to save, exactly as duplicating a
                            // sheet does and for the same reason: the copy is made from the FILE,
                            // so without this it would be of the last save rather than of what is
                            // on screen.
                            if model.SavedSheetIsOutOfDate then
                                match MenuHelpers.saveOpenFileToModel model with
                                | Some { CurrentProj = Some p } -> dispatch <| SetProject p
                                | _ -> ()
                            let prompt =
                                $"A separate array component will be made from '{lc.Name}', which \
                                  has {copiesOfArray info} copies. The copy starts the same and can \
                                  then be changed on its own. It will be called:"
                            // The SOURCE sheet, so that a loop variable clashing with one of the
                            // properties the copy inherits is caught here rather than after it is
                            // made. Its own loop variable is offered, since the copy starts as it
                            // did and that name is free on it by construction.
                            askForName "Copy as an array component" prompt (Some lc) loop project
                                (fun name loop model' -> addArraySheet project name loop (Some lc) model' dispatch)
                                model dispatch)
                 | None -> null)
            ]

    let foot =
        fun (_: Model) ->
            div [ Style [ Display DisplayOptions.Flex; JustifyContent "flex-end" ] ] [
                Button.button [ Button.OnClick (fun _ -> dispatch ClosePopup) ] [ str "Cancel" ]
            ]

    // Wider than a popup's default, because what this holds is prose - three choices, each
    // explained in a sentence or two - and at the default width every one of those wrapped to four
    // or five lines.
    //
    // Bounded by the WINDOW as well, and not just given a number: 800 is wider than an Issie window
    // that has been narrowed or is on a small screen, and a modal wider than the window it is in
    // hangs off the side of it with no way to scroll to what it is hiding.
    dynamicClosablePopup "New array component" body foot [ Width 1200; MaxWidth "90vw" ] dispatch
