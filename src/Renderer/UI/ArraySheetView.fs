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
            explanation
            stopButton
        ]

//-------------------------------------------------------------------------------------------//
//---------------------------------MAKING AN ARRAY COMPONENT---------------------------------//
//-------------------------------------------------------------------------------------------//

/// The sheets that could be made, or copied, into an array component: the user's own, and not one
/// already.
let private candidateSheets (project: Project) =
    project.LoadedComponents
    |> List.filter (fun lc -> lc.Form = Some User && lc.ArrayInfo.IsNone)
    |> List.sortBy (fun lc -> lc.Name)

/// The settings a newly made array component starts with.
let private startingInfo = { LoopParam = ParamName defaultLoopName; Copies = 4 }

/// Ask for a name, then run `make` with it. Refuses a name the project already has, or one the
/// file system will not take, using the same check the New Sheet dialog uses.
let private askForName title prompt (project: Project) (make: string -> Model -> unit) model dispatch =
    let before =
        fun (dialogData: PopupDialogData) ->
            let text = getText dialogData
            div [] [
                str prompt
                br []
                Option.defaultValue (div [] []) (MiscMenuView.maybeWarning text project)
            ]
    let body = dialogPopupBodyOnlyText before "Insert array component name" dispatch
    let buttonAction =
        fun (model': Model) ->
            make ((getText model'.PopupDialogData).ToLower()) model'
            dispatch ClosePopup
            dispatch FinishUICmd
    let isDisabled =
        fun (model': Model) ->
            let text = getText model'.PopupDialogData
            text = "" || isFileInProject text project || (MiscMenuView.maybeWarning text project).IsSome
    dialogPopup title body "Create" buttonAction isDisabled [] dispatch

/// Add a sheet to the project, with array settings, and open it for editing.
let private addArraySheet (project: Project) (name: string) (canvasFrom: LoadedComponent option) model dispatch =
    match canvasFrom with
    | None -> ComponentLibraries.createEmptySheetFile project name |> displayAlertOnError dispatch
    | Some source ->
        // the copy gets fresh ids for everything, so it cannot clash with the sheet it came from
        ComponentLibraries.copySheetWithNewIds source.FilePath (ComponentLibraries.sheetFilePath project name)

    match tryLoadComponentFromPath (ComponentLibraries.sheetFilePath project name) with
    | Error err -> displayFileErrorNotification err dispatch
    | Ok loaded ->
        // The array settings are put on here rather than written into the file, so that the one
        // place they are ever set is the same one the Properties pane uses. The ports follow from
        // them, so they are recomputed too.
        let ins, outs =
            CanvasExtractor.parseDiagramSignatureFor
                (Some startingInfo) loaded.LCParameterSlots loaded.CanvasState
        let ldc =
            { loaded with
                Name = name
                ArrayInfo = Some startingInfo
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

    let candidates = candidateSheets project

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
    let pickSheet title prompt (andThen: LoadedComponent -> unit) =
        let body =
            fun (_: Model) ->
                div [] [
                    p [ Style [ MarginBottom "8px" ] ] [ str prompt ]
                    div []
                        (candidates
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
                UIPopups.helpText AppMessages.Confirm.usingArraySheets
                hr []
                choice "New array component"
                    "An empty sheet to draw one copy on."
                    true
                    (fun () ->
                        askForName "New array component" "A new sheet will be created for it:" project
                            (fun name model' -> addArraySheet project name None model' dispatch)
                            model dispatch)
                choice "Make an existing sheet an array component"
                    "The sheet is left as it is and becomes one copy of the array. This is the \
                     usual way: you find out you want an array after drawing one copy."
                    (not candidates.IsEmpty)
                    (fun () ->
                        pickSheet "Make an array component" "Which sheet?"
                            (fun lc ->
                                openFileInProject' true lc.Name project model dispatch
                                dispatch
                                <| ExecFuncInMessage ((fun model' dispatch' ->
                                        setArrayInfo model' (Some startingInfo) dispatch'), dispatch)))
                choice "Copy an existing sheet as an array component"
                    "The sheet is left alone and a copy of it becomes the array, so an ordinary \
                     design and an array version of it can both exist."
                    (not candidates.IsEmpty)
                    (fun () ->
                        pickSheet "Copy as an array component" "Which sheet should be copied?"
                            (fun lc ->
                                askForName "Copy as an array component" "The copy will be called:" project
                                    (fun name model' -> addArraySheet project name (Some lc) model' dispatch)
                                    model dispatch))
            ]

    let foot =
        fun (_: Model) ->
            div [ Style [ Display DisplayOptions.Flex; JustifyContent "flex-end" ] ] [
                Button.button [ Button.OnClick (fun _ -> dispatch ClosePopup) ] [ str "Cancel" ]
            ]

    dynamicClosablePopup "New array component" body foot [] dispatch
