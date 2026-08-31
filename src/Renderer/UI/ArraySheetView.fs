module ArraySheetView

(*
    ArraySheetView.fs

    Making a sheet an ARRAY DESIGN SHEET, and editing the settings of one that already is.

    Both are reached from the right-click menu on the sheet's background and nowhere else. The
    properties pane deliberately gains nothing: an array sheet is a rare thing, and every ordinary
    sheet would otherwise carry a control for a feature its user will never touch.

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
// the open sheet's live canvas is the draw block's, not the LoadedComponent's
open Sheet.SheetInterface
open DrawModelType

/// The Array out components on a canvas, which are what a multiplexer can be declared over.
let private arrayOutLabels ((comps, _): CanvasState) =
    comps
    |> List.filter (fun comp -> match comp.Type with | ArrayOut _ -> true | _ -> false)
    |> List.sortBy (fun comp -> comp.Y, comp.X)
    |> List.map (fun comp -> comp.Label)

/// Whether the open sheet still holds components that only an array sheet may have.
let private hasArrayComponents (model: Model) =
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
//---------------------------------MAKING ONE AN ARRAY SHEET---------------------------------//
//-------------------------------------------------------------------------------------------//

/// Ask for a loop variable and a copy count, and make the open sheet an array design sheet.
///
/// The first use of the feature is where it gets explained, as it is for sheet properties: until
/// someone makes one, nothing in Issie hints that array sheets exist.
let makeArraySheetPopup (model: Model) (dispatch: Msg -> unit) =
    match model.CurrentProj with
    | None -> Log.warn "makeArraySheetPopup called with no project open"
    | Some project ->

    let declaredNames =
        project.LoadedComponents
        |> List.tryFind (fun lc -> lc.Name = project.OpenFileName)
        |> Option.map (ParameterView.getDefaultParamDefs >> Map.toList >> List.map (fst >> fun (ParamName n) -> n))
        |> Option.defaultValue []

    let ask () =
        let textPrompt =
            fun _ ->
                div [] [
                    str "This sheet's hardware will be several copies of what is drawn on it, one \
                         per value of a loop variable. What should that variable be called?"
                    br []
                ]
        let intPrompt =
            fun _ ->
                div [] [
                    br []
                    str "The loop variable counts from 0 up to and including:"
                ]
        let body =
            dialogPopupBodyTextAndInt textPrompt "example: i" intPrompt 7 dispatch

        let buttonAction =
            fun (model': Model) ->
                let name = getText model'.PopupDialogData
                let endValue = getInt model'.PopupDialogData
                setArrayInfo model
                    (Some { LoopParam = ParamName name; EndValue = endValue; Muxes = [] })
                    dispatch
                dispatch ClosePopup
                dispatch
                <| SetPropertiesNotification (Notifications.successPropertiesNotification
                    $"This sheet is now an array design sheet of {endValue + 1} copies. Use \
                      '{name}' in any property box to make one copy differ from the next, and the \
                      Array sheet section of the catalogue to say how the copies join up.")

        // The name has to be one an expression can refer to, since referring to it is the whole
        // point, and it must not be a property this sheet already declares - the loop variable is
        // not one of those, and two things of one name in the same expressions would be a puzzle
        // rather than an error. The copy count is what the array's ports are derived from, so it
        // cannot be negative.
        let isDisabled =
            fun (model': Model) ->
                let name = getText model'.PopupDialogData
                not (isValidParamName name)
                || List.contains name declaredNames
                || getInt model'.PopupDialogData < 0
                || getInt model'.PopupDialogData + 1 > ArrayElaborate.Constants.maxArrayCopies

        dialogPopup "Make this an array sheet" body "Make it an array sheet" buttonAction isDisabled [] dispatch

    // said once, before the first array sheet in a project: it is a feature a design can perfectly
    // well never use, and until one exists nothing in the editor hints that it is there
    let projectHasOne =
        project.LoadedComponents |> List.exists (fun lc -> lc.ArrayInfo.IsSome)
    match projectHasOne with
    | true -> ask ()
    | false ->
        let body = UIPopups.helpText AppMessages.Confirm.usingArraySheets
        confirmationPopup "Array design sheets" "Continue" body
            (fun _ ->
                dispatch ClosePopup
                ask ())
            dispatch

//-------------------------------------------------------------------------------------------//
//-----------------------------------EDITING THE SETTINGS------------------------------------//
//-------------------------------------------------------------------------------------------//

/// The settings of the open array design sheet, edited in place.
///
/// A live editor rather than a dialog: each change is its own edit to the sheet, which is what
/// undo already understands, and there is nothing here that only makes sense committed as a set.
let arraySheetSettingsPopup (model: Model) (dispatch: Msg -> unit) =
    let title = "Array sheet settings"

    /// Read from the model as it is now, so the popup shows what the last change did.
    let infoOf (model': Model) = openSheetArrayInfo model'

    let body =
        fun (model': Model) ->
            match infoOf model' with
            | None -> div [] [ str "This sheet is no longer an array design sheet." ]
            | Some info ->
                let (ParamName loopName) = info.LoopParam
                let copies = copiesOfArray info
                let canvas = model'.Sheet.GetCanvasState()
                let sources = arrayOutLabels canvas

                /// Change one field of the settings.
                let change f = setArrayInfo model' (Some (f info)) dispatch

                let copyCountBox =
                    div [] [
                        PropertiesHelp.fieldLabel "Copies"
                        Input.number [
                            Input.Props [ Style [ Width "120px" ]; Min 1; Max ArrayElaborate.Constants.maxArrayCopies ]
                            Input.DefaultValue (string copies)
                            Input.OnChange (fun ev ->
                                match System.Int32.TryParse (JSHelpers.getTextEventValue ev) with
                                | true, n when n >= 1 && n <= ArrayElaborate.Constants.maxArrayCopies ->
                                    change (fun i -> { i with EndValue = n - 1 })
                                | _ -> ())
                        ]
                        // said rather than left to be discovered: it is the one edit here that
                        // reaches other sheets
                        p [ Style [ FontSize "0.85em"; Color "grey" ] ] [
                            str $"The loop variable '{loopName}' counts 0 to {copies - 1}. Changing \
                                  this changes how many ports this sheet has, so every component \
                                  made from it elsewhere will need updating."
                        ]
                    ]

                /// One declared multiplexer, and the button that removes it.
                let muxRow (spec: ArrayMuxSpec) =
                    tr [] [
                        td [] [ str spec.MuxName ]
                        td [] [ str spec.MuxSource ]
                        td [] [
                            Button.button [
                                Button.Size IsSmall
                                Button.Color IsDanger
                                Button.OnClick (fun _ ->
                                    change (fun i -> { i with Muxes = List.filter ((<>) spec) i.Muxes }))
                            ] [ str "Delete" ]
                        ]
                    ]

                /// Add a multiplexer over each Array out that has none yet. One button per source
                /// rather than a dialog: the only thing to choose is which Array out it reads, and
                /// its name follows from that - a second one over the same source is the rare case
                /// and can be renamed by hand in the file until there is a reason to build for it.
                let addButtons =
                    sources
                    |> List.filter (fun src -> not (info.Muxes |> List.exists (fun m -> m.MuxSource = src)))
                    |> List.map (fun src ->
                        Button.button [
                            Button.Size IsSmall
                            Button.Color IsInfo
                            Button.Props [ Style [ MarginRight "6px" ] ]
                            Button.OnClick (fun _ ->
                                change (fun i -> { i with Muxes = i.Muxes @ [ {MuxSource = src; MuxName = src} ] }))
                        ] [ str $"Add multiplexer over {src}" ])

                let muxSection =
                    div [ Style [ MarginTop "12px" ] ] [
                        PropertiesHelp.fieldLabel "Multiplexers"
                        (match info.Muxes with
                         | [] ->
                            p [ Style [ FontSize "0.85em"; Color "grey" ] ] [
                                str "A multiplexer reads back one copy's Array out value. It adds a \
                                     select input and an output to this sheet."
                            ]
                         | muxes ->
                            Table.table [ Table.IsNarrow; Table.IsFullWidth ] [
                                thead [] [ tr [] [ th [] [str "Output"]; th [] [str "Array out"]; th [] [] ] ]
                                tbody [] (List.map muxRow muxes)
                            ])
                        (match sources, addButtons with
                         | [], _ ->
                            p [ Style [ FontSize "0.85em"; Color "grey" ] ] [
                                str "This sheet has no Array out components to select between."
                            ]
                         | _, [] ->
                            p [ Style [ FontSize "0.85em"; Color "grey" ] ] [
                                str "Every Array out on this sheet already has a multiplexer."
                            ]
                         | _, buttons -> div [] buttons)
                    ]

                /// Stop being an array sheet, which only makes sense once the components that say
                /// how its copies join up have gone: leaving them would make the sheet one that
                /// cannot be simulated, and deleting them silently would throw away work.
                let stopSection =
                    div [ Style [ MarginTop "16px" ] ] [
                        match hasArrayComponents model' with
                        | [] ->
                            Button.button [
                                Button.Color IsDanger
                                Button.OnClick (fun _ ->
                                    setArrayInfo model' None dispatch
                                    dispatch ClosePopup)
                            ] [ str "Stop being an array sheet" ]
                        | labels ->
                            p [ Style [ FontSize "0.85em"; Color "grey" ] ] [
                                str ("This sheet cannot stop being an array sheet while it holds "
                                     + (labels |> String.concat ", ")
                                     + ": those components only mean something on one.")
                            ]
                    ]

                div [] [ copyCountBox; muxSection; stopSection ]

    let foot =
        fun (_: Model) ->
            div [ Style [ Display DisplayOptions.Flex; JustifyContent "flex-end" ] ] [
                Button.button [ Button.Color IsPrimary; Button.OnClick (fun _ -> dispatch ClosePopup) ] [ str "Close" ]
            ]

    dynamicClosablePopup title body foot [] dispatch
