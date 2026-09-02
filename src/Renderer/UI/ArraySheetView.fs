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
            |> ParameterView.markSheetParamsChanged)

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

        let copyBox =
            div [] [
                PropertiesHelp.fieldLabel "Copies"
                Input.number [
                    Input.Props [ Style [ Width "120px" ]; Min 1; Max ArrayElaborate.Constants.maxArrayCopies ]
                    Input.Disabled simIsOpen
                    Input.DefaultValue (string info.Copies)
                    Input.OnChange (fun ev ->
                        match System.Int32.TryParse (JSHelpers.getTextEventValue ev) with
                        | true, n when n >= 1 && n <= ArrayElaborate.Constants.maxArrayCopies ->
                            setArrayInfo model (Some { info with Copies = n }) dispatch
                        | _ -> ())
                ]
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

/// The sheets that could BECOME an array component: the user's own, and not one already - there is
/// nothing to do to a sheet that is one.
let private convertible (project: Project) =
    project.LoadedComponents
    |> List.filter (fun lc -> lc.Form = Some User && lc.ArrayInfo.IsNone)
    |> List.sortBy (fun lc -> lc.Name)

/// The sheets that could be COPIED into an array component: any of the user's own, an array
/// component included.
///
/// Copying one is the ordinary way to get a second array of the same shape - a 4-bit adder and a
/// 16-bit one - and it is the only way, since an array component's copy count is a property of the
/// sheet and not of the instance. What comes out is a separate sheet with its own settings, so the
/// two can then be changed apart.
let private copyable (project: Project) =
    project.LoadedComponents
    |> List.filter (fun lc -> lc.Form = Some User)
    |> List.sortBy (fun lc -> lc.Name)

/// How many copies a newly made array component starts with.
let private defaultCopies = 4

/// The loop variable a dialog box holds, with the default where nothing has been typed.
///
/// Empty means the default rather than an error: the name matters only to whoever writes it in a
/// property box, `i` is what an index is called, and asking someone to type it before they know
/// what it is for would be a question with one sensible answer.
let private loopNameOf (typed: string) =
    if typed.Trim() = "" then defaultLoopName else typed.Trim()

/// Whether a loop variable name can be used on a sheet: one an expression could refer to, and not
/// a property that sheet already declares - the two would be the same word meaning two things.
let private loopNameProblem (sheet: LoadedComponent option) (typed: string) =
    let name = loopNameOf typed
    let declared =
        sheet
        |> Option.map (ParameterView.getDefaultParamDefs >> Map.toList >> List.map (fst >> fun (ParamName n) -> n))
        |> Option.defaultValue []
    if not (isValidParamName name) then
        Some $"'{name}' cannot be a loop variable: a name is a letter followed by letters and digits, and not min, max or clog2"
    elif List.contains name declared then
        Some $"this sheet already has a property called '{name}'"
    else None

/// Ask for a sheet name and a loop variable, then run `make` with both. Refuses a name the project
/// already has, or one the file system will not take, using the check the New Sheet dialog uses.
let private askForName title prompt (defaultLoop: string) (project: Project) (make: string -> string -> Model -> unit) model dispatch =
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
                str "What should its loop variable be called? Write this in any property box on the                      sheet to make one copy differ from the next."
                br []
                match loopNameProblem None (getText2 dialogData) with
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
                (match (getText2 model'.PopupDialogData).Trim() with
                 | "" -> defaultLoop
                 | typed -> typed)
                model'
            dispatch ClosePopup
            dispatch FinishUICmd
    let isDisabled =
        fun (model': Model) ->
            let text = getText model'.PopupDialogData
            text = "" || isFileInProject text project || (MiscMenuView.maybeWarning text project).IsSome
            || (loopNameProblem None (getText2 model'.PopupDialogData)).IsSome
    dialogPopup title body "Create" buttonAction isDisabled [] dispatch

/// Ask only for a loop variable, for a sheet that already exists and keeps its name.
let private askForLoopVariable title (sheet: LoadedComponent) (make: string -> Model -> unit) dispatch =
    let before =
        fun (dialogData: PopupDialogData) ->
            div [] [
                str $"'{sheet.Name}' will become an array component. What should its loop variable                       be called? Write this in any property box on the sheet to make one copy differ                       from the next."
                br []
                match loopNameProblem (Some sheet) (getText dialogData) with
                | Some msg -> span [ Style [ Color "red" ] ] [ str msg ]
                | None -> null
            ]
    let body = dialogPopupBodyOnlyText before $"default: {defaultLoopName}" dispatch
    let buttonAction =
        fun (model': Model) ->
            make (loopNameOf (getText model'.PopupDialogData)) model'
            dispatch ClosePopup
            dispatch FinishUICmd
    let isDisabled =
        fun (model': Model) -> (loopNameProblem (Some sheet) (getText model'.PopupDialogData)).IsSome
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

    let convertible = convertible project
    let copyable = copyable project

    /// One of the three ways in, as a button with a line saying what it does.
    let choice label description enabled action =
        div [ Style [ MarginBottom "10px" ] ] [
            Button.button [
                Button.Color IsInfo
                Button.Disabled (not enabled)
                Button.OnClick (fun _ -> dispatch ClosePopup; action ())
            ] [ str label ]
            p [ Style [ FontSize "0.85em"; Color "grey"; MarginTop "4px" ] ] [ str description ]
        ]

    /// Pick one of the project's ordinary sheets, then do something with it.
    /// Pick one of a list of sheets, then do something with it. The list is a parameter because
    /// the two ways in take different ones: any sheet can be COPIED into an array component, while
    /// only a sheet that is not already one can BECOME one.
    let pickSheet (sheets: LoadedComponent list) title prompt (andThen: LoadedComponent -> unit) =
        let body =
            fun (_: Model) ->
                div [] [
                    p [ Style [ MarginBottom "8px" ] ] [ str prompt ]
                    div []
                        (sheets
                         |> List.map (fun lc ->
                            Button.button [
                                Button.Size IsSmall
                                Button.Props [ Style [ MarginRight "6px"; MarginBottom "6px" ] ]
                                Button.OnClick (fun _ -> dispatch ClosePopup; andThen lc)
                            ] [ str lc.Name ]))
                ]
        let foot =
            fun (_: Model) ->
                div [ Style [ Display DisplayOptions.Flex; JustifyContent "flex-end" ] ] [
                    Button.button [ Button.OnClick (fun _ -> dispatch ClosePopup) ] [ str "Cancel" ]
                ]
        dynamicClosablePopup title body foot [] dispatch

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
                    true
                    (fun () ->
                        askForName "New array component" "A new sheet will be created for it:"
                            defaultLoopName project
                            (fun name loop model' -> addArraySheet project name loop None model' dispatch)
                            model dispatch)
                choice "Make an existing sheet an array component"
                    "The sheet is left as it is and becomes one copy of the array. This is the \
                     usual way: you find out you want an array after drawing one copy."
                    (not convertible.IsEmpty)
                    (fun () ->
                        pickSheet convertible "Make an array component" "Which sheet?"
                            (fun lc ->
                                askForLoopVariable "Make an array component" lc
                                    (fun loop _ ->
                                        // opened first: setArrayInfo works on the OPEN sheet, and
                                        // reads its live canvas to derive the ports
                                        openFileInProject' true lc.Name project model dispatch
                                        dispatch
                                        <| ExecFuncInMessage ((fun model' dispatch' ->
                                                setArrayInfo model'
                                                    (Some { LoopParam = ParamName loop; Copies = defaultCopies })
                                                    dispatch'), dispatch))
                                    dispatch))
                choice "Copy an existing sheet as an array component"
                    "The sheet is left alone and a copy of it becomes the array, so an ordinary \
                     design and an array version of it can both exist."
                    (not copyable.IsEmpty)
                    (fun () ->
                        pickSheet copyable "Copy as an array component" "Which sheet should be copied?"
                            (fun lc ->
                                // Copying an array component is allowed, and is the ordinary way to
                                // get a second array of the same shape. The copy keeps its source's
                                // copy count and loop variable, so what is asked for is the one
                                // thing that must differ - and the dialog says which it is copying,
                                // since the button that led here does not.
                                let prompt, loop =
                                    match lc.ArrayInfo with
                                    | Some info ->
                                        $"'{lc.Name}' is already an array component, of                                           {copiesOfArray info} copies. A separate one will be made                                           from it, which can then be changed on its own. It will be                                           called:",
                                        let (ParamName n) = info.LoopParam in n
                                    | None -> "The copy will be called:", defaultLoopName
                                askForName "Copy as an array component" prompt loop project
                                    (fun name loop model' -> addArraySheet project name loop (Some lc) model' dispatch)
                                    model dispatch))
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
