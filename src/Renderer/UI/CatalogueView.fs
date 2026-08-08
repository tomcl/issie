(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView
open EEExtensions
open VerilogTypes
open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open DiagramStyle
open NumberHelpers
open ModelType
open ModelHelpers
open CommonTypes
open PopupHelpers
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open NearleyBindings
open ErrorCheck
open CodeEditorHelpers
open Fable.SimpleJson
open Fable.Core.JsInterop
open System
open TopMenuView
open MenuHelpers
open SheetCreator
open ParameterTypes
open Optics
open Optics.Operators

NearleyBindings.importGrammar
NearleyBindings.importFix
NearleyBindings.importParser

module Constants =
    let maxGateInputs = 19
    let maxSplitMergeBranches = 19
    /// The size a library component is carried at before its sheet has been read - see GhostBox.
    let ghostBoxW = 100.
    let ghostBoxH = 60.
    /// The colour the draw block gives a symbol dragged on top of another. A carried component
    /// that would land on one is drawn in it, and for the same reason: it cannot be dropped there.
    let ghostErrorColour = "Red"

let menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]

/// Where on the sheet a point on the screen is, if it is over the canvas at all. A gesture that
/// ends anywhere else - over the catalogue, the menu bar, outside the window - is over no position
/// on the sheet and places nothing, which is what lets a drag be abandoned by dropping it nowhere.
///
/// This is the conversion the canvas applies to its own mouse events - SheetDisplay.getDrawBlockPos
/// - except that the scroll is read from the canvas rather than from the sheet model. The model's
/// copy of it is refreshed by Sheet.update, and a catalogue drag sends the sheet nothing, so during
/// one it can be as old as the last thing done to the sheet: after opening a project and going
/// straight to the catalogue it still holds the scroll the sheet was created with.
let private sheetPosOfCursor (cursor: XYPos) (sheet: SheetT.Model) : XYPos option =
    match Browser.Dom.document.getElementById "Canvas" with
    | null -> None
    | canvas ->
        let box = canvas.getBoundingClientRect ()
        match cursor.X >= box.left && cursor.X <= box.right
              && cursor.Y >= box.top && cursor.Y <= box.bottom with
        | false -> None
        | true ->
            Some {
                X = (cursor.X + canvas.scrollLeft) / sheet.Zoom
                Y = (cursor.Y - getHeaderHeight + canvas.scrollTop) / sheet.Zoom
            }

/// The space a carried component would take up on the sheet if it were dropped at `sheetPos`.
///
/// For a symbol this is the box of the symbol that would be created there, worked out by creating
/// it: a component's size depends on its ports and on the text in it, so anything else here would
/// be a second, disagreeing, opinion of how big it is.
let private ghostBoundingBox (model: Model) (ghost: DragGhost) (sheetPos: XYPos) : BoundingBox =
    match ghost with
    | GhostSymbol compType ->
        Symbol.createNewSymbol (tryGetLoadedComponents model) sheetPos compType "" model.Sheet.Wire.Symbol.Theme
        |> Symbol.getSymbolBoundingBox
    | GhostBox _ ->
        {
            TopLeft = { X = sheetPos.X - Constants.ghostBoxW / 2.; Y = sheetPos.Y - Constants.ghostBoxH / 2. }
            W = Constants.ghostBoxW
            H = Constants.ghostBoxH
        }

/// Whether a component carried to this point would land on top of one already on the sheet.
///
/// The sheet refuses to place a component over another, so a drag that would do it is shown as
/// refused - the ghost turns red - and releasing it does nothing at all.
let private ghostOverlaps (model: Model) (ghost: DragGhost) (cursor: XYPos) : bool =
    match sheetPosOfCursor cursor model.Sheet with
    | None -> false
    | Some sheetPos ->
        let box = ghostBoundingBox model ghost sheetPos
        model.Sheet.BoundingBoxes
        |> Map.exists (fun _ symbolBox -> DrawHelpers.boxesIntersect symbolBox box)

/// A catalogue item that places a component. `place` is what the item does when it is asked for -
/// create the component, or open the popup that will - and it runs when the gesture ends.
///
/// Click and drag are one gesture here: a click is a drag that ended somewhere other than the
/// canvas, and both end in the same call to `place`. The exception is a drag released over a
/// component already on the sheet, which is refused and calls nothing. Both are driven from pointer
/// events rather than a click handler because the capture taken on press retargets the click to
/// this item even when the release was over the canvas, so an OnClick would fire a second time for
/// every drop.
let private placementItem styles (ghost: DragGhost) label (place: unit -> unit) (model: Model) dispatch =
    /// Whether this item is the one being carried, marked on the element by its own press and
    /// unmarked when that press ends.
    ///
    /// The question is put to the DOM rather than to the model because these handlers hold the
    /// model as it was when the catalogue was last drawn: a click quick enough to beat the
    /// re-render would look like a gesture that had never started, and would place nothing.
    /// Being per-element it settles the other direction too - a press that began somewhere else,
    /// on the canvas or on another catalogue item, places nothing here.
    let carryingThis (ev: Browser.Types.PointerEvent) : bool = ev.currentTarget?__issieCarrying = true
    let setCarrying (ev: Browser.Types.PointerEvent) (carrying: bool) =
        ev.currentTarget?__issieCarrying <- carrying
    Menu.Item.li [
        Menu.Item.IsActive false
        Menu.Item.Props [
            Style styles
            OnPointerDown (fun ev ->
                setCarrying ev true
                // Captured, so the gesture is followed across the whole window. Without this the
                // ghost would stop moving the instant the pointer left this item, which is at
                // once - the canvas it is being dragged to is a few pixels away. Best effort:
                // capture can be refused for a pointer the browser no longer considers active,
                // and losing the ghost is a far smaller thing than losing the ability to place,
                // which is why nothing else here depends on it.
                try ev.currentTarget?setPointerCapture (ev.pointerId) |> ignore with _ -> ()
                dispatch (StartDragPlacement (ghost, { X = ev.clientX; Y = ev.clientY })))
            OnPointerMove (fun ev ->
                // Only while carrying something: hovering over the catalogue must not dispatch
                // on every mouse move.
                if carryingThis ev then
                    dispatch (MoveDragPlacement { X = ev.clientX; Y = ev.clientY }))
            OnPointerUp (fun ev ->
                if carryingThis ev then
                    setCarrying ev false
                    let cursor = { X = ev.clientX; Y = ev.clientY }
                    match ghostOverlaps model ghost cursor with
                    // The release the red ghost is warning about. Nothing happens at all: `place`
                    // is not called, so a component that asks a popup for its parameters does not
                    // ask for a placement that is not going to happen. The popup belongs to a
                    // placement that succeeds.
                    | true -> dispatch EndDragPlacement
                    | false ->
                        // Released over free canvas the component goes where it was dropped;
                        // released off the canvas this was a click, and the component follows the
                        // cursor as it always has.
                        match sheetPosOfCursor cursor model.Sheet with
                        | Some pos -> dispatch (DropDragPlacement pos)
                        | None -> dispatch EndDragPlacement
                        place ())
            // A gesture the system takes away - a touch turning into a scroll, the window losing
            // the pointer - places nothing, exactly as a release off the canvas does.
            OnPointerCancel (fun ev ->
                if carryingThis ev then
                    setCarrying ev false
                    dispatch EndDragPlacement)
        ]
    ] [ str label ]

/// Hand a placement to the sheet. The component follows the cursor until it is clicked into
/// place, except when the placement came from a catalogue drag that has already been dropped:
/// then it goes where it was dropped, put there by the move-then-click the user would otherwise
/// have performed by hand.
///
/// Driving the drop through the ordinary mouse path rather than placing directly is what keeps
/// the two gestures identical in everything that follows - snapping, the overlap check, the undo
/// entry - and it means a drop onto an occupied space settles into the same follow-the-cursor
/// state a click would have left it in, rather than being refused.
let private startPlacement (placeMsg: SheetT.Msg) (model: Model) dispatch =
    dispatch (Sheet placeMsg)
    match model.DragPlacement with
    | Some (DroppedAt pos) ->
        let atDropPoint op : DrawHelpers.MouseT = {
            Pos = pos
            ScreenMovement = { X = 0.; Y = 0. }
            ScreenPage = { X = 0.; Y = 0. }
            ShiftKeyDown = false
            Op = op
        }
        dispatch EndDragPlacement
        dispatch (Sheet (SheetT.MouseMsg (atDropPoint DrawHelpers.Move)))
        dispatch (Sheet (SheetT.MouseMsg (atDropPoint DrawHelpers.Down)))
    | _ -> ()

let private createComponent compType label createParam model dispatch =
    startPlacement
        (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, compType, label, createParam))
        model dispatch

// Anything requiring a standard label should be checked and updated with the correct number suffix in Symbol/Sheet, 
// so give the label ""
let createCompStdLabel comp createParam model dispatch =
    createComponent comp "" createParam model dispatch

/// Place an instance of `loadedComponent` with the given parameter bindings, whose ports are
/// sized at the values those bindings give. `createParam`, if any, records the instance's
/// parameter slots once the component has an id.
let private placeCustomComponent
        (loadedComponent: LoadedComponent)
        (bindings: ParamBindings)
        (createParam: (ComponentId -> Unit) option)
        model
        dispatch =
    // The instance's ports are the child sheet resolved at these bindings, which is
    // CanvasExtractor.signatureOfInstance's job: a binding may be an expression in the parameters
    // of the sheet being placed ONTO - that is what the popup's bind-to-parent button writes - and
    // it has to be evaluated there before it can size a port. Evaluating it in the child's own
    // environment instead makes a parameter bound to a same-named parent parameter look
    // self-referential, and the instance is placed with the child's default widths.
    // The sheet is put at the head of the list so that a library component just materialised into
    // the project is found whether or not the model has caught up with it.
    let ldcs =
        loadedComponent
        :: (tryGetLoadedComponents model |> List.filter (fun l -> l.Name <> loadedComponent.Name))
    let inputLabels, outputLabels =
        CanvasExtractor.signatureOfInstance ldcs (ParameterView.paramBindingsOfModel model) loadedComponent.Name bindings
        |> Option.defaultValue (loadedComponent.InputLabels, loadedComponent.OutputLabels)

    let custom = Custom {
        Name = loadedComponent.Name
        InputLabels = inputLabels
        OutputLabels = outputLabels
        Form = loadedComponent.Form
        Description = loadedComponent.Description
        ParameterBindings = if Map.isEmpty bindings then None else Some bindings
    }

    startPlacement (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, custom, "", createParam)) model dispatch

/// Start placing an instance of a sheet. A sheet that declares parameters asks for a value for
/// each of them first: placing without asking would silently freeze the sheet's defaults into the
/// instance, which is exactly the stale-chain problem.
let startPlacingCustomComponent (loadedComponent: LoadedComponent) model dispatch =
    match ParameterView.getDefaultParamDefs loadedComponent |> Map.isEmpty with
    | true -> placeCustomComponent loadedComponent Map.empty None model dispatch
    | false ->
        let place bindings specs =
            placeCustomComponent
                loadedComponent bindings
                (Some (ParameterView.addParamComponents specs dispatch))
                model dispatch
        ParameterView.customComponentParamPopup loadedComponent place model dispatch

/// An instance of `loadedComponent` as it is drawn before any parameters have been chosen: the
/// sheet at its own declared defaults, which is what the instance would be if the popup were
/// simply accepted.
let private customGhost (loadedComponent: LoadedComponent) =
    GhostSymbol (Custom {
        Name = loadedComponent.Name
        InputLabels = loadedComponent.InputLabels
        OutputLabels = loadedComponent.OutputLabels
        Form = loadedComponent.Form
        Description = loadedComponent.Description
        ParameterBindings = None
    })

/// Placing a sheet reads the model as it is when the gesture ends rather than as it was when the
/// catalogue was drawn, because the drop position is written into it moments earlier - by the
/// pointer-up that leads here.
let private placeSheetOnGesture (loadedComponent: LoadedComponent) dispatch =
    fun () ->
        dispatch <| ExecFuncInMessage (
            (fun model' dispatch' -> startPlacingCustomComponent loadedComponent model' dispatch'),
            dispatch)

let private makeCustom styles model dispatch (loadedComponent: LoadedComponent)  =
    placementItem styles (customGhost loadedComponent) loadedComponent.Name
        (placeSheetOnGesture loadedComponent dispatch) model dispatch

//------------------------------------------------------------------------------------------------//
//---------------------------------- Shipped component libraries ---------------------------------//
//------------------------------------------------------------------------------------------------//

/// Materialise a library component and everything it needs into the project, and return the
/// LoadedComponent to instantiate - the last one, since dependencies come first.
///
/// Three things happen to each sheet on the way in, and all of them are needed:
///   - it is renamed to L<n>_<name>, so it cannot clash with a user sheet
///   - its ids are regenerated, so two projects using the same library share nothing
///   - every custom component in it that names another component OF THIS LIBRARY is repointed at
///     that component's new name. Dependencies are referenced by name, so without this a
///     multi-sheet component would be placed holding instances of sheets that are not there.
let private materialiseLibraryComponent
        (libName: string)
        (files: ComponentLibraries.LibraryFile list)
        (project: Project)
        : Result<LoadedComponent list, string> =
    let index = ComponentLibraries.libraryIndexFor project.LoadedComponents libName
    let inLibrary = files |> List.map (fun (header, _) -> header.Name) |> Set.ofList
    let renameDependencies (ldc: LoadedComponent) =
        let comps, conns = ldc.CanvasState
        let comps =
            comps
            |> List.map (fun comp ->
                match comp.Type with
                | Custom cc when Set.contains cc.Name inLibrary ->
                    {comp with Type = Custom {cc with Name = ComponentLibraries.sheetNameFor index cc.Name}}
                | _ -> comp)
        {ldc with CanvasState = comps, conns}

    let materialiseOne (existing: LoadedComponent list) ((header, body): ComponentLibraries.LibraryFile) =
        let sheetName = ComponentLibraries.sheetNameFor index header.Name
        match existing |> List.tryFind (fun ldc -> ldc.Name = sheetName) with
        | Some already -> Ok already
        | None ->
            let destPath = pathJoin [| project.ProjectPath; sheetName + ".dgm" |]
            // the body IS a .dgm, so it goes to disk verbatim and comes back through the ordinary
            // loader: nothing here has to understand the canvas format
            match writeFile destPath body with
            | Error msg -> Error $"Could not write {sheetName}: {msg}"
            | Ok () ->
                match tryLoadComponentFromPath destPath with
                | Error msg -> Error $"Could not add {header.Name} from library {libName}: {msg}"
                | Ok ldc ->
                    let ldc =
                        {Helpers.RegenerateIds.regenerateSheetIds ldc with
                            Name = sheetName
                            Form = Some (Library (libName, header.Name))}
                        |> renameDependencies
                    writeComponentToFile ldc |> ignore
                    Ok ldc

    // every sheet, dependencies first, with the component to instantiate last
    (([]: LoadedComponent list), files)
    ||> Helpers.ResultList.fold (fun got file ->
        materialiseOne (project.LoadedComponents @ got) file |> Result.map (fun ldc -> got @ [ldc]))
    |> Result.bind (fun ldcs ->
        match ldcs with
        | [] -> Error $"Library {libName} has no component to place"
        | _ -> Ok ldcs)

/// Place an instance of a library component, adding its sheet - and any it needs - to the project
/// first. The .ldgm files are read here, when the user asks for the component, not when the
/// library was listed: listing reads headers only.
let private startPlacingLibraryComponent
        (library: ComponentLibraries.OpenedLibrary)
        (listing: ComponentLibraries.LibraryListing)
        model
        dispatch =
    match model.CurrentProj with
    | None -> ()
    | Some project ->
        let placed =
            ComponentLibraries.readComponentAndDependencies library.Path listing.Header.Name
            |> Result.bind (fun files -> materialiseLibraryComponent library.Name files project)
        match placed with
        | Error msg -> dispatch <| SetFilesNotification (Notifications.errorFilesNotification msg)
        | Ok [] -> ()
        | Ok ldcs ->
            // EVERY new sheet must reach the model, not just the one being instantiated: a
            // multi-sheet component's dependencies were written to disk but left out of
            // LoadedComponents, so simulating before the project was reloaded failed with
            // DependencyNotFound on a sheet that was sitting right there in the directory.
            dispatch <| UpdateModel (fun m ->
                match m.CurrentProj with
                | None -> m
                | Some p ->
                    let added =
                        (p.LoadedComponents, ldcs)
                        ||> List.fold (fun known ldc ->
                            match known |> List.exists (fun c -> c.Name = ldc.Name) with
                            | true -> known
                            | false -> known @ [ldc])
                    {m with CurrentProj = Some {p with LoadedComponents = added}})
            // the component itself is last; the sheets before it are what it needs
            let toPlace = List.last ldcs
            dispatch <| ExecFuncInMessage ((fun model' dispatch' ->
                startPlacingCustomComponent toPlace model' dispatch'), dispatch)

/// `keep` is the catalogue's search filter, applied to a sheet's name and its description.
let private makeCustomList keep styles model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.filter (fun comp ->
            match JSHelpers.debugLevel <> 0 with
            |true -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel || comp.Form = Some ProtectedSubSheet)
            |false -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel)
        )
        |> List.filter (fun comp -> keep comp.Name (Option.defaultValue "" comp.Description))
        |> List.sortBy (fun x -> x.Name)
        |> List.map (makeCustom styles model dispatch)

/// A Verilog-generated sheet is placed exactly as any other sheet is, parameters included.
let private makeVerilog styles model dispatch (loadedComponent: LoadedComponent)  =
    placementItem styles (customGhost loadedComponent) loadedComponent.Name
        (placeSheetOnGesture loadedComponent dispatch) model dispatch

let private makeVerilogList keep styles model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.filter (fun comp -> match comp.Form with Some (Verilog _)-> true |_ -> false)
        |> List.filter (fun comp -> keep comp.Name (Option.defaultValue "" comp.Description))
        |> List.map (makeVerilog styles model dispatch)



let private createInputPopup typeStr (compType: int * bigint option -> ComponentType) (model:Model) (dispatch: Msg -> unit) =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let beforeDefaultValue = fun _ -> str <| sprintf "If the input is undriven, what should the default value be?"
    let intDefault = model.LastUsedDialogWidth
    let body =
        dialogPopupBodyTextAndTwoInts 1 (beforeText, placeholder) (beforeInt, beforeDefaultValue) (bigint intDefault, 0I) dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let widthInt = getInt dialogData
            let defaultValueInt = getInt2 dialogData
            createComponent (compType (widthInt, Some defaultValueInt)) (MenuHelpers.formatLabelFromType (compType (widthInt, Some defaultValueInt)) inputText) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let notGoodLabel =
                getText dialogData
                |> (fun s -> s = "" || not (String.startsWithLetter s))
            (getInt dialogData < 1) || notGoodLabel
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createIOPopup hasInt typeStr compType (model:Model) dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = model.LastUsedDialogWidth
    let body = 
        match hasInt with
        | true -> dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
        | false -> dialogPopupBodyOnlyText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            createComponent (compType inputInt) (MenuHelpers.formatLabelFromType (compType inputInt) inputText) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let notGoodLabel =
                getText dialogData
                |> (fun s -> s = "" || not (String.startsWithLetter s))
            (getInt dialogData < 1) || notGoodLabel
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


/// The nets on this sheet that already have a name. A net IS its name - every net label carrying
/// it is the same net - so the names, and not the components carrying them, are what a new label
/// has to choose between.
let private netLabelNames (model: Model) : string list =
    model.Sheet.Wire.Symbol.Symbols
    |> Map.toList
    |> List.choose (fun (_, sym) ->
        match sym.Component.Type with
        | IOLabel -> Some sym.Component.Label
        | _ -> None)
    |> List.distinct
    |> List.sort

/// The name a label would carry if the text in the box were accepted. Comparing anything against
/// what was typed rather than against this would let "data" through as a new net beside DATA.
let private typedNetName (model: Model) : string =
    MenuHelpers.formatLabelFromType IOLabel (getText model.PopupDialogData)

/// What is wrong with the name in the box, if anything - the message to show, and the reason the
/// Add button is dead.
///
/// A name that an existing net already has is refused rather than quietly joined: joining is what
/// the pills are for, and a name that collided by accident would otherwise make two nets that were
/// never meant to be one into a single net, silently and some way from where the user was looking.
let private newNetProblem (model: Model) : string option =
    match getText model.PopupDialogData with
    | "" -> None
    | typed when not (String.startsWithLetter typed) -> Some "Name must start with a letter"
    | _ ->
        let name = typedNetName model
        match List.contains name (netLabelNames model) with
        | true -> Some $"{name} is already a net on this sheet - join it above instead"
        | false -> None

/// Placing a net label.
///
/// A net label is a name rather than a component that happens to have one, and the thing a user
/// wants to do with it is nearly always join a net that already exists. So the nets on the sheet
/// come first, as pills: clicking one places a label on that net and is the whole of the gesture.
/// The box below is for the other case, a net that does not exist yet, and refuses any name that
/// does.
let private createNetLabelPopup (model: Model) dispatch =
    let title = "Add net label"
    /// Place one net label carrying this name and close the popup - what both the pills and the
    /// button do, and the only thing this dialog exists to do.
    ///
    /// The model is the one the popup was opened with rather than the one it is drawn with, because
    /// that is where a drop position lives: opened by a drop, the label goes where it was dropped.
    let placeNetLabel (name: string) =
        createComponent IOLabel (MenuHelpers.formatLabelFromType IOLabel name) None model dispatch
        dispatch ClosePopup
    let body =
        fun (model': Model) ->
            let netPill (name: string) =
                Tag.tag [
                    Tag.Color IsInfo
                    Tag.Props [
                        // one of a list of siblings, so React is told which is which
                        Key name
                        Style [Cursor "pointer"]
                        OnClick (fun _ -> placeNetLabel name) ]
                ] [ str name ]
            let nets = netLabelNames model'
            let problem = newNetProblem model'
            div [] [
                match nets with
                | [] -> null
                | names ->
                    div [Style [MarginBottom "10px"]] [
                        str "Join a net on this sheet:"
                        Tag.list [Tag.List.Props [Style [MarginTop "6px"]]] (List.map netPill names)
                    ]
                str (match nets with [] -> "Name the net:" | _ -> "Or name a new net:")
                Input.text [
                    Input.Props [AutoFocus true; SpellCheck false]
                    Input.Placeholder "Net name"
                    Input.OnChange (JSHelpers.getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
                ]
                match problem with
                | None -> null
                | Some message -> span [Style [FontStyle "Italic"; Color "Red"]] [str message]
                hr [Style [MarginTop "12px"; MarginBottom "8px"]]
                div [Style [FontSize "0.9em"]] [
                    str "In a digital schematic a set of connected wires is called a net."
                    br []
                    str "Every net label with the same name is one net, so a signal can be picked \
                         up anywhere on the sheet with no wire drawn to it."
                    br []
                    str "Use net labels instead of a wire that would cross the sheet, and for high \
                         fan-out: drive the net once, then join it at each input it feeds."
                    br []
                    str "Exactly one label in a net may be driven; the rest drive inputs."
                ]
            ]
    let buttonText = "Add"
    let buttonAction = fun (model': Model) -> placeNetLabel (getText model'.PopupDialogData)
    let isDisabled =
        fun (model': Model) ->
            getText model'.PopupDialogData = "" || (newNetProblem model').IsSome
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let createSheetDescriptionPopup (model:Model) previousDescr sheetName dispatch =
    let title = sprintf "Sheet Description"
    let beforeText =
        fun _ -> str <| sprintf "Add description for sheet '%s'" sheetName
    let body =  dialogPopupBodyOnlyTextWithDefaultValue beforeText "Description" previousDescr dispatch
    let buttonText = "Save"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let descr = getText dialogData
            let descrToSave =
                match descr with
                |"" -> None
                |_ -> Some descr
            
            match model.CurrentProj with
            |None -> failwithf "Can't happen"
            |Some p ->
                let target_ldc = p.LoadedComponents |> List.find (fun x -> x.Name = sheetName)
                let other_ldc = p.LoadedComponents |> List.filter (fun x -> x.Name <> sheetName)
                let target_ldc' = {target_ldc with Description=descrToSave}  //add description to ldc
                
                let other_ldc' =  //find all custom comps originating from that sheet and update their description
                    other_ldc 
                    |> List.map (fun ldc -> 
                        let newComps = 
                            ldc.CanvasState
                            |> fst
                            |> List.map (fun comp ->
                                match comp.Type with
                                |Custom x when x.Name = sheetName -> 
                                    let newCompType = Custom {x with Description = descrToSave} 
                                    {comp with Type = newCompType}
                                |_ -> comp
                        )
                        let newCS = newComps,(ldc.CanvasState |> snd)
                        {ldc with CanvasState = newCS}
                    )

                let fixed_ldcs = other_ldc'@[target_ldc'] 
                let p' = {p with LoadedComponents=fixed_ldcs}
                let model' = {model with CurrentProj = Some p'}
                dispatch <| SetProject p'
                saveOpenFileActionWithModelUpdate model' dispatch |> ignore
            dispatch ClosePopup
    let isDisabled =
        fun (model: Model) ->
            false  //allow all
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createGateNPopup (gateType: GateComponentType) (model: Model) dispatch =
    let title = $"Add N input {gateType} gate"
    let beforeInt =
        fun _ -> str "How many inputs should the gate have?"
    let intDefault = 2
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (GateN (gateType, inputInt)) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let intIn = getInt model'.PopupDialogData
            intIn < 2 || intIn > Constants.maxGateInputs
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createMergeNPopup (model: Model) dispatch =
    let title = $"Add N input merge"
    let beforeInt =
        fun _ -> str "How many inputs should the merge component have?"
    let intDefault = 2
    let body = dialogPopupBodyOnlyBoundedInt beforeInt intDefault 2 Constants.maxSplitMergeBranches dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (MergeN inputInt) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let intIn = getInt model'.PopupDialogData
            intIn < 2 || intIn > Constants.maxSplitMergeBranches
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createSplitNPopup (model: Model) dispatch =
    let title = $"Add N output split"
    let beforeInt =
        fun _ -> str "How many outputs should the split component have?"
    let numInputsDefault = 2
    let body = dialogPopupBodyNInts beforeInt numInputsDefault 1 Constants.maxSplitMergeBranches dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let outputInt = getInt dialogData 
            let outputWidthList = getIntList dialogData numInputsDefault 1
            let outputLSBList = getIntList2 dialogData numInputsDefault 0
            createCompStdLabel (SplitN (outputInt, outputWidthList, outputLSBList)) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let intIn = getInt model'.PopupDialogData
            intIn < 2 || intIn > Constants.maxSplitMergeBranches
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


/// Component creation popup box for n-bit arithmetic components
let private createArithmeticPopup compType (model: Model) dispatch =
    let compName, toComp =
        match compType with
        | NbitsAdder _ -> "adder", NbitsAdder
        | NbitsXor _ -> "XOR", fun n -> NbitsXor (n, None)
        | NbitsAnd _ -> "AND", NbitsAnd
        | NbitsOr _ -> "OR", NbitsOr
        | NbitsNot _ -> "NOT", NbitsNot
        | _ -> failwithf $"Invalid component type {compType} for arithmetic popup"

    let title = $"Add N bits {compName}"
    let prompt = "How many bits should the input/output have?"
    let buttonText = "Add"
    let intDefault = model.LastUsedDialogWidth
    let slot = Buswidth
    
    let constraints = [MinVal (PInt 1, $"Number of bits in {compName} must be positive")]
 
    let defaultParamSpec = {
        CompSlot = slot
        Expression = PInt intDefault
        Constraints = constraints
        Value = intDefault
    }

    let inputField model' =
        ParameterView.paramInputField model' prompt intDefault None constraints None slot dispatch

    let buttonAction =
        fun (model': Model) ->
            let inputFieldDialog = 
                match model'.PopupDialogData.DialogState with
                | Some inputSpec -> Map.find slot inputSpec
                | None -> failwithf "Param input field must set new param info"
            let compParamSpec =
                match inputFieldDialog with
                | Ok paramSpec -> paramSpec
                | Error err ->
                    failwithf $"Received error message '{err}' when creating N-bits {compName}"
            let comp = toComp compParamSpec.Value
            let addParamCompFunction = 
                Some <| ParameterView.addParamComponent compParamSpec dispatch

            createCompStdLabel comp addParamCompFunction model dispatch
            dispatch <| ReloadSelectedComponent compParamSpec.Value
            dispatch ClosePopup

    let isDisabled =
        fun model' ->
            model'.PopupDialogData.DialogState
            |> function
               | Some specs -> Map.find slot specs |> Result.isError
               | None -> failwithf "Dialog state must exist for input box"

    dispatch <| AddPopupDialogParamSpec (slot, Ok defaultParamSpec)
    dialogPopup title inputField buttonText buttonAction isDisabled [] dispatch


/// Component creation popup for the shifter: bus width plus a choice of shift kind.
/// The chosen kind is held in PopupDialogData.Int2 (0 = LSL, 1 = LSR, 2 = ASR).
let private createShiftPopup (model: Model) dispatch =
    let title = "Add N bits shift"
    let buttonText = "Add"
    let intDefault = model.LastUsedDialogWidth
    let slot = Buswidth
    let constraints = [MinVal (PInt 1, "Number of bits in shift must be positive")]

    let defaultParamSpec = {
        CompSlot = slot
        Expression = PInt intDefault
        Constraints = constraints
        Value = intDefault
    }

    let shiftKinds = [ LSL, 0I, "LSL", "logical shift left"
                       LSR, 1I, "LSR", "logical shift right"
                       ASR, 2I, "ASR", "arithmetic shift right" ]

    let chosenKind (model': Model) =
        shiftKinds
        |> List.tryPick (fun (kind, tag, _, _) ->
            if model'.PopupDialogData.Int2 = Some tag then Some kind else None)
        |> Option.defaultValue LSL

    let body (model': Model) =
        div [] [
            ParameterView.paramInputField model' "How many bits should the input/output have?" intDefault None constraints None slot dispatch
            br []
            str "Which kind of shift?"
            br []; br []
            Field.div [Field.HasAddons] (
                shiftKinds
                |> List.map (fun (kind, tag, short, descr) ->
                    Control.div [] [
                        Button.button [
                            Button.Color (if chosenKind model' = kind then IsPrimary else IsLight)
                            Button.OnClick (fun _ -> dispatch <| SetPopupDialogInt2 (Some tag))
                        ] [ str $"{short}: {descr}" ]
                    ]))
        ]

    let buttonAction =
        fun (model': Model) ->
            let inputFieldDialog =
                match model'.PopupDialogData.DialogState with
                | Some inputSpec -> Map.find slot inputSpec
                | None -> failwithf "Param input field must set new param info"
            let compParamSpec =
                match inputFieldDialog with
                | Ok paramSpec -> paramSpec
                | Error err ->
                    failwithf $"Received error message '{err}' when creating N-bits shift"
            let w = compParamSpec.Value
            let comp = Shift (w, shifterWidthFor w, chosenKind model')
            let addParamCompFunction =
                Some <| ParameterView.addParamComponent compParamSpec dispatch

            createCompStdLabel comp addParamCompFunction model dispatch
            dispatch <| ReloadSelectedComponent w
            dispatch ClosePopup

    let isDisabled =
        fun (model': Model) ->
            model'.PopupDialogData.DialogState
            |> function
               | Some specs -> Map.find slot specs |> Result.isError
               | None -> failwithf "Dialog state must exist for input box"

    dispatch <| SetPopupDialogInt2 (Some 0I)
    dispatch <| AddPopupDialogParamSpec (slot, Ok defaultParamSpec)
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createNbitSpreaderPopup (model:Model) dispatch =
    let title = sprintf "Add 1-to-N bit spreader"
    let beforeInt =
        fun _ -> str "How many bits should the output bus contain?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (NbitSpreader inputInt) None {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createSplitWirePopup model dispatch =
    let title = sprintf "Add SplitWire node" 
    let beforeInt =
        fun _ -> str "How many bits should go to the top (LSB) wire? The remaining bits will go to the bottom (MSB) wire. \
                      Use Edit -> Flip Vertically after placing component to swap top and bottom"
    let intDefault = 1
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (SplitWire inputInt) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// two react text lines in red
let private twoErrorLines errMsg1 errMsg2 =
    span [Style [Color Red]] [str errMsg1; br []; str errMsg2; br [] ]

/// two line message giving constant value
let private constantValueMessage w (cVal:bigint) =
    let mask = (1I <<< w) - 1I 
    let uVal = cVal &&& mask
    let sVal = if cVal &&& (1I <<< (w-1)) <> 0I then  uVal - (1I <<< w) else uVal
    let hVal = NumberHelpers.BigIntToPaddedString Constants.maxNumericCharsBeforeTruncation Hex w uVal
    let line1 = $"Decimal value: {uVal} ({sVal} signed)"
    let line2 = $"Hex value: %s{hVal}"
    span [] [str line1; br [] ; str line2; br [] ]

/// two line message giving constant value
let private busCompareValueMessage w (cVal:bigint) =
    let mask = 
            (1I <<< w) - 1I
    let uVal = cVal &&& mask
    let sVal = if uVal &&& (1I <<< (w - 1)) = 0I then uVal else (1I <<< w) - uVal
    let hVal = NumberHelpers.BigIntToPaddedString Constants.maxNumericCharsBeforeTruncation Hex w uVal
    let line1 = $"Decimal value: {uVal} ({sVal} signed)"
    let line2 = $"Hex value: %s{hVal}"
    span [] [str line1; br [] ; str line2; br [] ]

/// check constant parameters and return two react lines with
/// error message or value details.
let parseConstant wMax w cText =
    if w < 1 || w > wMax then
            twoErrorLines $"Constant width must be in the range 1..{wMax}" "", None
    else
        match NumberHelpers.strToIntCheckWidth w cText with
        | Ok n ->
            constantValueMessage w n, Some (Constant1 (w,n,cText))
        | Error msg ->
            twoErrorLines msg "", None

let parseBusCompareValue wMax w cText =
    if w < 1 || w > wMax then
            twoErrorLines $"Bus Compare width must be in the range 1..{wMax}" "", None
    else
        match NumberHelpers.strToIntCheckWidth w cText with
        | Ok n ->
            busCompareValueMessage w n, Some (BusCompare1 (w,n,cText))
        | Error msg ->
            twoErrorLines msg "", None

/// create react popup to set a constant
let private createConstantPopup model dispatch =
    let title = sprintf "Add Constant" 
    let beforeInt =
        fun _ -> str "How many bits has the wire carrying the constant?"
    let intDefault = 1
    let parseConstantDialog model' =
        let dialog = model'.PopupDialogData
        parseConstant Constants.maxIssieBusWidth
            (Option.defaultValue intDefault dialog.Int)
            (Option.defaultValue "" dialog.Text)
    let beforeText = (fun d -> div [] [d |> parseConstantDialog |> fst; br [] ])
    let placeholder = "Value: decimal, 0x... hex, 0b... binary"   
    let body = dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
            let width = getInt dialogData
            let text = Option.defaultValue "" dialogData.Text
            let constant = 
                match NumberHelpers.strToIntCheckWidth width text with
                | Ok n -> n
                | Error _ -> 0I // should never happen?
            let text' = if text = "" then "0" else text
            createCompStdLabel (Constant1(width, constant, text')) None model dispatch
            dispatch ClosePopup
    let isDisabled = parseConstantDialog >> snd >> Option.isNone
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createBusSelectPopup model dispatch =
    let title = sprintf "Add Bus Selection node" 
    let beforeInt2 =
        fun _ -> str "Which input bit is the least significant output bit?"
    let beforeInt =
        fun _ -> str "How many bits width is the output bus?"
    let intDefault = 1I
    let intDefault2 = 0I
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, intDefault2) "60px" dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let width = getInt dialogData
            let lsb = int32 (getInt2 dialogData)
            createCompStdLabel (BusSelection(width,lsb)) None model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1 || getInt2 model'.PopupDialogData < 0I
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createBusComparePopup (model:Model) dispatch =
    let title = sprintf "Add Bus Compare node" 
    let beforeInt =
        fun _ -> str "How many bits width is the input bus?"
    let intDefault = 1
    let parseBusCompDialog model' =
        let dialog = model'.PopupDialogData
        parseBusCompareValue Constants.maxIssieBusWidth (Option.defaultValue intDefault dialog.Int) (Option.defaultValue "" dialog.Text)
    let beforeText = (fun d -> div [] [d |> parseBusCompDialog |> fst; br [] ])
    let placeholder = "Value: decimal, 0x... hex, 0b... binary"   
    let body = dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let width = getInt dialogData
            let text = Option.defaultValue "" dialogData.Text
            let constant = 
                match NumberHelpers.strToIntCheckWidth width text with
                | Ok n -> n
                | Error _ -> 0I // should never happen?
            let text' = if text = "" then "0" else text
            createCompStdLabel (BusCompare1(width,constant,text')) None model dispatch
            dispatch ClosePopup
    let isDisabled = parseBusCompDialog >> snd >> Option.isNone
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createRegisterPopup regType (model:Model) dispatch =
    let title = match regType with
                    | Register _ -> sprintf "Add Register" 
                    | RegisterE _ -> sprintf "Add Register with Enable"
                    | Counter _-> sprintf "Add Counter"
                    | _ -> failwithf "Invalid register type"

    let prompt = match regType with
                    | Register _ -> "How wide should the register be (in bits)?"
                    | RegisterE _ -> "How wide should the register be (in bits)?"
                    | Counter _ -> "How wide should the counter be (in bits)?"
                    | _ -> failwithf "Invalid register type"
    let intDefault = model.LastUsedDialogWidth
    let slot = Buswidth

    let constraints = [MinVal (PInt 1, "Register width must be positive")]

    let defaultParamSpec = {
        CompSlot = slot
        Expression = PInt intDefault
        Constraints = constraints
        Value = intDefault
    }

    let inputField model' =
        ParameterView.paramInputField model' prompt intDefault None constraints None slot dispatch

    let buttonText = "Add"
    let buttonAction =
        fun model' ->
            let inputFieldDialog = 
                match model'.PopupDialogData.DialogState with
                | Some inputSpec -> Map.find slot inputSpec
                | None -> failwithf "Param input field must set new param info"
            let compParamSpec =
                match inputFieldDialog with
                | Ok paramSpec -> paramSpec
                | Error err -> failwithf $"Received error message {err} when creating N-bits XOR"
            let addParamCompFunction = 
                Some <| ParameterView.addParamComponent compParamSpec dispatch

            let regWidth = compParamSpec.Value;

            match regType with
            | Register _ -> createCompStdLabel (Register regWidth) addParamCompFunction model dispatch
            | RegisterE _ -> createCompStdLabel (RegisterE regWidth) addParamCompFunction model dispatch
            | Counter _ -> createCompStdLabel (Counter regWidth) addParamCompFunction model dispatch
            | _ -> failwithf "Invalid register type"
            dispatch ClosePopup

    let isDisabled =
        fun model' ->
            model'.PopupDialogData.DialogState
            |> function
               | Some specs -> Map.find slot specs |> Result.isError
               | None -> failwithf "Dialog state must exist for input box"

    dispatch <| AddPopupDialogParamSpec (slot, Ok defaultParamSpec)
    dialogPopup title inputField buttonText buttonAction isDisabled [] dispatch

let private createMemoryPopup memType model (dispatch: Msg -> Unit) =
    let title = "Create memory"
    let intDefault = model.LastUsedDialogWidth
    let addError errorOpt (memSetup:(int*int*InitMemData*string option) option) : (int*int*InitMemData*string option) option =
        match memSetup with
        | Some (n1,n2,mem, _) ->
            Some (n1,n2,mem,errorOpt)
        | _ -> None
    let body = dialogPopupBodyMemorySetup intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let addressWidth, wordWidth, source, msgOpt = getMemorySetup dialogData intDefault
            let initMem = {
                AddressWidth = addressWidth
                WordWidth = wordWidth
                Init = source
                Data = Map.empty
                // initialiseMem fills these in from the .ram file when there is one
                Comments = None
                }

            let memory = FilesIO.initialiseMem  initMem dialogData.ProjectPath
            match memory with
            | Ok mem ->
                createCompStdLabel (memType mem) None model dispatch
                dispatch ClosePopup
            | Error mess ->
                dispatch <| SetPopupDialogMemorySetup (addError (Some mess) dialogData.MemorySetup)
    let isDisabled =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let addressWidth, wordWidth, source,_ = getMemorySetup dialogData 1
            let error = 
                match dialogData.MemorySetup with 
                | Some(_,_,ToFileBadName _,_) ->
                    Some "File name must be alphanumeric without prefix"
                | None -> 
                    Some ""
                | Some (_,_,SignedMultiplier,_) 
                | Some(_,_,UnsignedMultiplier,_) ->
                    if addressWidth % 2 <> 0 then
                        Some "The address width must be even for a multiplier"
                    elif addressWidth > 16 then
                        Some "The maximum multiplier size is 8X8 - 16 address bits"
                    else None
                | _ -> 
                    None
            match error with
            | Some msg when msg <> "" ->
                match dialogData.MemorySetup with
                | Some (_,_,_,e) when e = Some msg -> ()
                | _ -> dispatch <| SetPopupDialogMemorySetup (addError (Some msg) dialogData.MemorySetup)
            | _ -> ()
            addressWidth < 1 || wordWidth < 1 || error <> None
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch



let rec createVerilogPopup model showExtraErrors correctedCode moduleName (origin:CodeEditorOpen) dispatch =
    let title = sprintf "Create Combinational or Synchronous Components using Verilog" 
    let beforeText =
        fun _ -> str <| sprintf "ISSIE Component Name"
    let noErrors = List.isEmpty model.PopupDialogData.VerilogErrors
    let errorDiv = if noErrors then null else getErrorDiv model.PopupDialogData.VerilogErrors
    let errorList = if showExtraErrors then model.PopupDialogData.VerilogErrors else [] 

    let saveButtonAction =
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
            | Some project ->
                let name = (Option.get moduleName)
                let folderPath = project.ProjectPath
                let path = pathJoin [| folderPath; name + ".v" |]
                let path2 = pathJoin [| folderPath; name + ".dgm" |]
                let code = getCode dialogData
                
                match writeFile path code with
                | Ok _ -> ()
                | Error _ -> failwithf "Writing verilog file FAILED"

                let parsedCodeNearley = parseFromFile(code)
                let output = Json.parseAs<ParserOutput> parsedCodeNearley
                let result = Option.get output.Result
                let fixedAST = fix result
                let parsedAST = fixedAST |> Json.parseAs<VerilogInput>

                let cs = SheetCreator.createSheet parsedAST project model dispatch
                let toSaveCanvasState = Helpers.JsonHelpers.stateToJsonString (cs, None, Some {
                                Form = Some (Verilog name);
                                Description=None;
                                ParameterDefinitions = None
                                IsTopSheet = None})

                match toSaveCanvasState |> Result.bind (writeFile path2) with
                | Ok _ -> 
                    let newComponent = 
                        match tryLoadComponentFromPath path2 with
                        |Ok comp -> comp
                        |Error _ -> failwithf "failed to load the created Verilog file"
                    let updatedProject =
                        {project with LoadedComponents = newComponent :: project.LoadedComponents}
                    openFileInProject project.OpenFileName updatedProject model dispatch
                | Error _ -> failwithf "Writing .dgm file FAILED"
            dispatch ClosePopup

    let updateButton = 
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
            | Some project ->
                let name = (Option.get moduleName)
                let folderPath = project.ProjectPath
                let path = pathJoin [| folderPath; name + ".v" |]
                let code = getCode dialogData
                match writeFile path code with
                | Ok _ -> ()
                | Error _ -> failwithf "Writing verilog file FAILED"
                
                let parsedCodeNearley = parseFromFile(code)
                let output = Json.parseAs<ParserOutput> parsedCodeNearley
                let result = Option.get output.Result
                let fixedAST = fix result
                let parsedAST = fixedAST |> Json.parseAs<VerilogInput>
                let newCS = SheetCreator.createSheet parsedAST project model dispatch

                dispatch (StartUICmd SaveSheet)               
                updateVerilogFileActionWithModelUpdate newCS name model dispatch |> ignore
                dispatch <| Sheet(SheetT.DoNothing)


    let compile =
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in compiling Verilog Component"
            | Some project ->
                let code = getCode dialogData
                let parsedCodeNearley = parseFromFile(code)
                let output = Json.parseAs<ParserOutput> parsedCodeNearley
                if isNullOrUndefined output.Error then
                    match Option.isNone output.Result with 
                    |true-> () //do nothing if parser failed for some reason
                    |false ->
                        let result = Option.get output.Result
                        let fixedAST = fix result
                        let linesIndex = Option.get output.NewLinesIndex |> Array.toList
                        let parsedAST = fixedAST |> Json.parseAs<VerilogInput>                        
                        let moduleName = parsedAST.Module.ModuleName.Name
                        let errorList = ErrorCheck.getSemanticErrors parsedAST linesIndex origin project
                        let dataUpdated = {dialogData with VerilogErrors = errorList; VerilogCode=Some code}
                        let showErrors' = 
                            match List.isEmpty errorList with
                            | true -> showExtraErrors
                            | false -> showExtraErrors 
                        createVerilogPopup {model with PopupDialogData = dataUpdated } showErrors' None (Some moduleName) origin dispatch
                else
                    let error = Option.get output.Error
                    let error'= CodeEditorHelpers.getSyntaxErrorInfo error
                    let dataUpdated = {dialogData with VerilogErrors = [error'] }
                    createVerilogPopup {model with PopupDialogData = dataUpdated } showExtraErrors None moduleName origin dispatch
 

    let addButton =
        fun (dialogData : PopupDialogData) ->
            fun (suggestion,replaceType,line, col) -> 
                
                let findLastIOAndAssignment (oldCode:string) =
                    let isSmallerThan x y = y <= x

                    let parsedCodeNearley = parseFromFile(oldCode)
                    let output = Json.parseAs<ParserOutput> parsedCodeNearley
                    let result = Option.get output.Result
                    let fixedAST = fix result
                    let linesIndex = Option.get output.NewLinesIndex |> Array.toList
                    let parsedAST = fixedAST |> Json.parseAs<VerilogInput>      
                    let ioDecls = parsedAST.Module.ModuleItems.ItemList |> Array.filter (fun item -> Option.isSome item.IODecl)
                    let lastIODecl = Array.tryLast ioDecls
                    let lastIOLocation =
                        match lastIODecl with
                        |Some d -> d.Location
                        |None -> 1
                    let prevIndexIO = List.findIndexBack (fun x -> isSmallerThan lastIOLocation x) linesIndex
                    
                    let assignments = parsedAST.Module.ModuleItems.ItemList |> Array.filter (fun item -> Option.isSome item.Statement)
                    let lastAssignment = Array.tryLast assignments
                    let lastAssignmentLocation =
                        match lastAssignment with
                        |Some d -> d.Location
                        |None -> lastIOLocation
                    let prevIndexA = List.findIndexBack (fun x -> isSmallerThan lastAssignmentLocation x) linesIndex                    
                    
                    (prevIndexIO,prevIndexA)
                let replaceSubstringAtLocation (originalString: string) (replacement: string) (startIndex: int) (length: int) =
                    let prefix = originalString.[..(startIndex-1)]
                    let suffix = originalString.[(startIndex+length)..]
                    prefix + replacement + suffix

                // need to update the variable at the specific location
                let putToCorrectPlace (oldCode:string) suggestion replaceType line =
                    let sepCode = oldCode.Split([|"\n"|],StringSplitOptions.RemoveEmptyEntries)
                    let linesList = Seq.toList sepCode
                    match replaceType with
                    |IODeclaration |Assignment ->
                        let untilError = (linesList[0],[2..line+1])||> List.fold (fun s v -> s+"\n"+linesList[v-1])
                        let fixedError = untilError+"\n  "+ suggestion
                        (fixedError,[line+1..(List.length linesList)-1])||> List.fold (fun s v -> s+"\n"+linesList[v])
                    |Variable error ->
                        let untilError = ("",[1..line-1])||> List.fold (fun s v -> 
                            match v with
                            |1 -> linesList[v-1]
                            |_ -> s+"\n"+linesList[v-1]
                        )
                        let fixedLine = replaceSubstringAtLocation linesList[line-1] suggestion (col-1) error.Length
                        let fixedError = 
                            match line with
                            |1 -> fixedLine
                            |_ -> untilError+"\n"+ fixedLine
                        (fixedError,[line..(List.length linesList)-1])||> List.fold (fun s v -> s+"\n"+linesList[v])
                    |NoReplace ->
                        oldCode

                let lineToPut =
                    match replaceType with
                    |IODeclaration -> fst <| findLastIOAndAssignment (Option.defaultValue "" dialogData.VerilogCode)
                    |Assignment -> snd <| findLastIOAndAssignment (Option.defaultValue "" dialogData.VerilogCode)
                    |_ -> line
                let replacedCode = putToCorrectPlace (Option.defaultValue "" dialogData.VerilogCode) suggestion replaceType lineToPut
                createVerilogPopup model showExtraErrors (Some replacedCode) moduleName origin dispatch
    
    let moreInfoButton = 
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in compiling Verilog Component"
            | Some project ->
                let errors = dialogData.VerilogErrors
                createVerilogPopup model (not showExtraErrors) None moduleName origin dispatch
    
    let body= dialogVerilogCompBody beforeText moduleName errorDiv errorList showExtraErrors correctedCode compile addButton dispatch

    let isDisabled =
        fun (dialogData : PopupDialogData) ->   //OverflowX OverflowOptions.Hidden;OverflowY OverflowOptions.Hidden
            not noErrors
    
    let extra = if showExtraErrors then [Width "80%";Height "75%";OverflowX OverflowOptions.Hidden;Position PositionOptions.Fixed;] else [Width "50%";Height "75%";OverflowX OverflowOptions.Hidden;Position PositionOptions.Fixed;]
    let saveUpdateText = match origin with |NewVerilogFile -> "Save" |UpdateVerilogFile _ -> "Update"
    let saveUpdateButton = match origin with |NewVerilogFile -> saveButtonAction |UpdateVerilogFile _ -> updateButton
    dialogVerilogPopup title body saveUpdateText noErrors showExtraErrors saveUpdateButton moreInfoButton isDisabled extra dispatch


/// One section of the catalogue.
///
/// `isOpen` is true while a search is running, so that what matched is visible without the user
/// opening nine sections to find it. A section whose items have all been filtered out is not drawn
/// at all - a row of empty headings tells the user nothing about where their component is.
let private makeMenuGroupOpen isOpen title menuList =
    match menuList with
    | [] -> null
    | menuList ->
        details [Open isOpen] [
            summary [menuLabelStyle] [ str title ]
            Menu.list [] menuList
        ]

let private makeMenuGroup title menuList = makeMenuGroupOpen false title menuList

let private makeMenuGroupWithTipOpen isOpen styles title tip menuList =
    match menuList with
    | [] -> null
    | menuList ->
        details [
            Open isOpen;
            HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
            Tooltip.dataTooltip tip
            Style styles
        ] [
            summary [menuLabelStyle] [ str title ]
            Menu.list [] menuList
        ]

let private makeMenuGroupWithTip styles title tip menuList =
    makeMenuGroupWithTipOpen false styles title tip menuList

/// The component a catalogue drag is carrying, drawn following the cursor.
///
/// This is the draw block's own drawing - createNewSymbol and drawComponent, the pair the canvas
/// itself uses - and not a picture of one, so what is being carried cannot fall out of step with
/// what will be placed: a symbol whose look depends on its ports, its size or the theme comes out
/// right here for nothing, and stays right when that drawing changes. It is scaled by the sheet's
/// zoom for the same reason.
///
/// Nothing is added to any model. The symbol is built, drawn and discarded on each mouse move,
/// which is what lets the popup still stand in front of the component being created.
///
/// Carried over a component already on the sheet it is drawn red, because there it cannot be
/// dropped - see ghostOverlaps.
let viewDragGhost (model: Model) : ReactElement =
    match model.DragPlacement with
    | None | Some (DroppedAt _) -> null
    | Some (Dragging (ghost, cursor)) ->
        let theme = model.Sheet.Wire.Symbol.Theme
        let zoom = model.Sheet.Zoom
        let overlapping = ghostOverlaps model ghost cursor
        /// Room around the symbol for what the drawing puts outside its own box - port labels
        /// above all - so that the ghost is not clipped by the svg holding it.
        let pad = 40.
        let drawing, w, h =
            match ghost with
            | GhostSymbol compType ->
                let placed = Symbol.createNewSymbol (tryGetLoadedComponents model) {X = 0.; Y = 0.} compType "" theme
                let sym =
                    match overlapping with
                    | true -> placed |> Optic.set (SymbolT.appearance_ >-> SymbolT.colour_) Constants.ghostErrorColour
                    | false -> placed
                SymbolView.drawComponent sym theme, sym.Component.W, sym.Component.H
            | GhostBox name ->
                // A library component's ports are not known until its sheet is read, and reading
                // it means going to disk - that belongs at placement, not on a mouse-down. So it
                // is carried as a named box, and takes its true shape once it is placed.
                let w, h = Constants.ghostBoxW, Constants.ghostBoxH
                let fill = if overlapping then Constants.ghostErrorColour else "lightgray"
                [ rect [
                    SVGAttr.Width w; SVGAttr.Height h; SVGAttr.Rx 5.
                    SVGAttr.Fill fill; SVGAttr.Stroke "black"; SVGAttr.StrokeWidth 1. ] []
                  text [
                    SVGAttr.X (w / 2.); SVGAttr.Y (h / 2.)
                    SVGAttr.TextAnchor "middle"
                    SVGAttr.Custom ("dominantBaseline", "middle")
                    SVGAttr.FontSize "12px" ] [ str name ] ],
                w, h
        let svgW = (w + 2. * pad) * zoom
        let svgH = (h + 2. * pad) * zoom
        div [
            Style [
                Position PositionOptions.Fixed
                // Centred on the cursor, as the symbol will be when it is created: createNewSymbol
                // centres a component on the position it is given.
                // qualified: Left and Top are also Edge cases, from DrawModelType
                CSSProp.Left $"{cursor.X - svgW / 2.}px"
                CSSProp.Top $"{cursor.Y - svgH / 2.}px"
                Width $"{svgW}px"
                Height $"{svgH}px"
                // The ghost must never be what the pointer is over. The drop is decided by what
                // is under the cursor, and something drawn at the cursor would always be it.
                CSSProp.PointerEvents "none"
                Opacity 0.65
                // Above the right-hand pane, which is 31, and far below a popup.
                ZIndex 32
            ]
        ] [
            svg [ Style [ Width $"{svgW}px"; Height $"{svgH}px" ] ] [
                g [ Style [ Transform $"scale({zoom})" ] ] [
                    g [ Style [ Transform $"translate({pad}px, {pad}px)" ] ] drawing
                ]
            ]
        ]

let compareModelsApprox (m1:Model) (m2:Model) =

    let m1r = reduceApprox m1
    let m2r = reduceApprox m2
    let b = m1r = m2r
    b




let viewCatalogue model dispatch =

        let dispatchAsFunc (func: Model -> (Msg -> Unit) -> Unit) =
            dispatch <| ExecFuncInMessage (func, dispatch)


        let muxTipMessage (numBusses:string) = $"Selects the one of its {numBusses} input busses numbered by the value of the select input 
                                to be the output. Adjusts bus width to match"

        let deMuxTipMessage (numBits:string) = $"The output numbered by the binary value 
        of the {numBits} sel inputs is equal to Data, the others are 0"

        let viewCatOfModel = fun model ->
            let styles =
                match model.Sheet.Action with
                | SheetT.InitialisedCreateComponent _ -> [Cursor "grabbing"]
                | _ -> []

            /// Memory contents for a ghost only. A memory symbol is one fixed size whatever it
            /// holds, so these values are never seen: the real ones are asked for by the popup.
            let ghostMemory = { Init = FromData; AddressWidth = 4; WordWidth = 8; Data = Map.empty; Comments = None }

            /// What the user has typed, lower-cased once.
            let searchTerm = model.CatalogueSearch.Trim().ToLower()
            let searching = searchTerm <> ""

            /// Whether a component is shown. The tooltip is searched as well as the name, because
            /// the name is Issie's word for the thing and the tooltip is usually the user's: a
            /// search for "subtract" finds the N bits XOR, and "invert" finds Not.
            let keep (name: string) (tip: string) =
                not searching
                || name.ToLower().Contains searchTerm
                || tip.ToLower().Contains searchTerm

            /// One catalogue component. `ghost` is what the item draws while it is being carried:
            /// the component it places, at the parameters that component would have if the popup,
            /// where there is one, were simply accepted.
            ///
            /// A list rather than one element, so that a component filtered out by the search
            /// contributes nothing to its section and a section left with nothing in it can tell.
            let catTip1 name (ghost: ComponentType) func (tip:string) : ReactElement list =
                match keep name tip with
                | false -> []
                | true ->
                    let react = placementItem styles (GhostSymbol ghost) name (fun () -> func ()) model dispatch
                    [ div [ HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                            Tooltip.dataTooltip tip
                            Style styles
                          ]
                          [ react ] ]

            /// A catalogue section. Open while a search is running, so what matched can be seen.
            let group title items = makeMenuGroupOpen searching title (List.concat items)
            let groupWithTip title tip items =
                makeMenuGroupWithTipOpen searching styles title tip items

            /// The search box. Kept outside the scrolling menu below it so that it stays put while
            /// the components scroll.
            let searchBox =
                div [Style [Padding "0 6px 6px 6px"]] [
                    Input.text [
                        Input.Placeholder "Search components"
                        Input.Value model.CatalogueSearch
                        Input.Size IsSmall
                        Input.OnChange (fun ev ->
                            let text: string = JSHelpers.getTextEventValue ev
                            dispatch <| UpdateModel (Optics.Optic.set catalogueSearch_ text))
                    ]
                ]

            /// Said instead of an empty catalogue, which otherwise looks like a fault.
            let noMatches =
                div [Style [Padding "10px"; Color "grey"]]
                    [str $"No component matches '{model.CatalogueSearch.Trim()}'."]

            let sections = [
                    group
                        "Input / Output"
                        [ catTip1 "Input" (Input1 (1, None))  (fun _ -> dispatchAsFunc (createInputPopup "input" Input1)) "Input connection to current sheet: one or more bits"
                          catTip1 "Output" (Output 1) (fun  _ -> dispatchAsFunc (createIOPopup true "output" Output)) "Output connection from current sheet: one or more bits"
                          catTip1 "Viewer" (Viewer 1) (fun  _ -> dispatchAsFunc (createIOPopup true "viewer" Viewer)) "Viewer to expose value in step simulation: works in subsheets. \
                                                                                                         Can also be used to terminate an unused output."
                          catTip1 "Constant" (Constant1 (1, 0I, "0")) (fun  _ -> dispatchAsFunc (createConstantPopup)) "Define a one or more bit constant value of specified width, \
                                                                                            e.g. 0 or 1, to drive an input. Values can be written \
                                                                                            in hex, decimal, or binary."
                          catTip1 "Net Label" (IOLabel) (fun  _ -> dispatchAsFunc createNetLabelPopup) "Every net label with the same name is one net, \
                                                                                                                         connected within a sheet without wires: use them for \
                                                                                                                         long connections and high fan-out. Each net must have \
                                                                                                                         exactly one driving input. A net label can also be used \
                                                                                                                         to terminate an unused output"
                          catTip1 "Not Connected" (NotConnected) (fun  _ -> dispatchAsFunc (createComponent (NotConnected) "" None)) "Not connected component to terminate unused output."]                          
                    group
                        "Buses"
                        [ 
                        catTip1 "MergeWires" (MergeWires)  (fun  _ -> dispatchAsFunc (createComponent MergeWires "" None)) "Use Mergewires when you want to \
                                                                                       join the bits of a two busses to make a wider bus. \
                                                                                       Default has LS bits connected to top arm. Use Edit -> Flip Vertically \
                                                                                       after placing component to change this."
                            
                        catTip1 "MergeN" (MergeN 2)  (fun  _ -> dispatchAsFunc (createMergeNPopup))
                                $"Use MergeN when you want to join the bits of between 2 \
                                 and {Constants.maxSplitMergeBranches} busses to make a wider bus."
                        catTip1 "SplitWire" (SplitWire 1) (fun  _ -> dispatchAsFunc (createSplitWirePopup)) "Use Splitwire when you want to split the \
                                                                                             bits of a bus into two sets. \
                                                                                             Default has LS bits connected to top arm. Use Edit -> Flip Vertically \
                                                                                             after placing component to change this."
                        catTip1 "SplitN" (SplitN (2, [1; 1], [0; 1])) (fun  _ -> dispatchAsFunc (createSplitNPopup))
                            $"Use SplitN when you want to split \
                              between 2 and {Constants.maxSplitMergeBranches} separate fields from a bus."                                                                          
                        catTip1 "Bus Select" (BusSelection (1, 0)) (fun  _ -> dispatchAsFunc (createBusSelectPopup))
                            "Bus Select output connects to one or \
                             more selected bits of its input"
                        catTip1 "Bus Compare" (BusCompare1 (1, 0I, "0")) (fun  _ -> dispatchAsFunc (createBusComparePopup)) "Bus compare outputs 1 if the input bus \
                                                                                                 matches a constant value as written in decimal, hex, or binary." 
                        catTip1 "N bits spreader" (NbitSpreader 1) (fun  _ -> dispatchAsFunc (createNbitSpreaderPopup)) "Replicates a 1 bit input onto all N bits of an output bus"]
                    group
                        "Gates"
                        [ catTip1 "Not" (Not)  (fun  _ -> dispatchAsFunc (createCompStdLabel Not None) ) "Invertor: output is negation of input"
                          catTip1 "And" (GateN (And, 2))  (fun  _ -> dispatchAsFunc (createCompStdLabel (GateN (And, 2)) None))
                                                "Output is 1 if all the inputs are 1. Use Properties to add more inputs"
                          catTip1 "Or" (GateN (Or, 2))   (fun  _ -> dispatchAsFunc (createCompStdLabel (GateN (Or, 2)) None))
                                                "Output is 1 if any of the inputs are 1. Use Properties to add more inputs"
                          catTip1 "Xor" (GateN (Xor, 2))  (fun  _ -> dispatchAsFunc (createCompStdLabel (GateN (Xor, 2)) None))
                                                "Output is 1 if an odd number of inputs are 1. Use Properties to add more inputs"
                          catTip1 "Nand" (GateN (Nand, 2)) (fun  _ -> dispatchAsFunc (createCompStdLabel (GateN (Nand, 2)) None))
                                                "Output is 0 if all the inputs are 1. Use Properties to add more inputs"
                          catTip1 "Nor" (GateN (Nor, 2))  (fun  _ -> dispatchAsFunc (createCompStdLabel (GateN (Nor, 2)) None))
                                                "Output is 0 if any of the inputs are 1. Use Properties to add more inputs"
                          catTip1 "Xnor" (GateN (Xnor, 2)) (fun  _ -> dispatchAsFunc (createCompStdLabel (GateN (Xnor, 2)) None))
                                                "Output is 1 if an even number of inputs are 1. Use Properties to add more inputs"]
                    group
                        "Mux / Demux"
                        [ catTip1 "2-Mux" (Mux2) (fun  _ -> dispatchAsFunc (createCompStdLabel Mux2 None)) <| muxTipMessage "two"
                          catTip1 "4-Mux" (Mux4) (fun  _ -> dispatchAsFunc (createCompStdLabel Mux4 None)) <| muxTipMessage "four"
                          catTip1 "8-Mux" (Mux8) (fun  _ -> dispatchAsFunc (createCompStdLabel Mux8 None)) <| muxTipMessage "eight"                                                             
                          catTip1 "2-Demux" (Demux2) (fun  _ -> dispatchAsFunc (createCompStdLabel Demux2 None))  <| deMuxTipMessage "two"  
                          catTip1 "4-Demux" (Demux4) (fun  _ -> dispatchAsFunc (createCompStdLabel Demux4 None))  <| deMuxTipMessage "four"
                          catTip1 "8-Demux" (Demux8) (fun  _ -> dispatchAsFunc (createCompStdLabel Demux8 None))  <| deMuxTipMessage "eight" ]
                    group
                        "Arithmetic"
                        [ catTip1 "N bits adder" (NbitsAdder 1) (fun  _ -> dispatchAsFunc (createArithmeticPopup <| NbitsAdder 1)) "N bit Binary adder with carry in to bit 0 and carry out from bit N-1"
                          catTip1 "N bits XOR" (NbitsXor (1, None)) (fun  _ -> dispatchAsFunc (createArithmeticPopup <| NbitsXor (1, None))) "N bit XOR gates - use to make subtractor or comparator"
                          catTip1 "N bits AND" (NbitsAnd 1) (fun  _ -> dispatchAsFunc (createArithmeticPopup <| NbitsAnd 1)) "N bit AND gates"
                          catTip1 "N bits OR" (NbitsOr 1) (fun  _ -> dispatchAsFunc (createArithmeticPopup <| NbitsOr 1)) "N bit OR gates"
                          catTip1 "N bits NOT" (NbitsNot 1) (fun  _ -> dispatchAsFunc (createArithmeticPopup <| NbitsNot 1)) "N bit NOT gates"
                          catTip1 "N bits shift" (Shift (1, 1, LSL)) (fun  _ -> dispatchAsFunc (createShiftPopup)) "N bit shifter: shifts the input by the number of positions on the SHIFT input. \
                                                    The kind of shift - logical left (LSL), logical right (LSR) or arithmetic right (ASR) - is chosen when the component is created"]

                    group
                        "Flip Flops and Registers"
                        [ catTip1 "D-flip-flop" (DFF) (fun  _ -> dispatchAsFunc (createCompStdLabel DFF None)) "D flip-flop - note that clock is assumed always connected to a global clock, \
                                                                                                   so ripple counters cannot be implemented in Issie"
                          catTip1 "D-flip-flop with enable" (DFFE) (fun  _ -> dispatchAsFunc (createCompStdLabel DFFE None)) "D flip-flop: output will remain unchanged when En is 0"
                          catTip1 "Register" (Register 1) (fun  _ -> dispatchAsFunc (createRegisterPopup (Register 0))) "N D flip-flops with inputs and outputs combined into single N bit busses"
                          catTip1 "Register with enable" (RegisterE 1) (fun  _ -> dispatchAsFunc (createRegisterPopup (RegisterE 0))) "As register but outputs stay the same if En is 0"
                          catTip1 "Counter" (Counter 1) (fun  _ -> dispatchAsFunc (createRegisterPopup (Counter 0))) "N-bits counter with customisable enable and load inputs"]
                    group
                        "Memories"
                        [ catTip1 "ROM (asynchronous)" (AsyncROM1 ghostMemory) (fun  _ -> dispatchAsFunc (createMemoryPopup AsyncROM1)) "This is combinational: \
                                                    the output is available in the same clock cycle that the address is presented"
                          catTip1 "ROM (synchronous)" (ROM1 ghostMemory) (fun  _ -> dispatchAsFunc (createMemoryPopup ROM1)) "A ROM whose output contains \
                                                    the addressed data in the clock cycle after the address is presented"
                          catTip1 "RAM (synchronous)" (RAM1 ghostMemory) (fun  _ -> dispatchAsFunc (createMemoryPopup RAM1))  "A RAM whose output contains the addressed \
                                                   data in the clock cycle after the address is presented" 
                          catTip1 "RAM (async read)" (AsyncRAM1 ghostMemory) (fun  _ -> dispatchAsFunc (createMemoryPopup AsyncRAM1))  "A RAM whose output contains the addressed \
                                                   data in the same clock cycle as address is presented" ]

                    groupWithTip
                        "This project"
                        "Every design sheet is available for use in other sheets as a custom component: \
                        it can be added any number of times, each instance replicating the sheet logic"
                        (makeCustomList keep styles model dispatch)

                    groupWithTip
                        "Verilog"
                        "Write combinational or synchronous logic in Verilog and use it as a Custom Component.
                         To edit/delete a verilog component add it in a sheet and click on 'properties'"
                        (List.append
                            // "New Verilog Component" makes a component rather than placing one, so
                            // it is not something a search for a component should turn up.
                            (if searching then [] else
                                [menuItem styles "New Verilog Component" (
                                    fun _ -> dispatchAsFunc(fun model _ -> createVerilogPopup model true None None NewVerilogFile dispatch)) ])
                            (makeVerilogList keep styles model dispatch))

                    // A library's components are not read until it is opened, so there are no
                    // component names here to search: the libraries themselves are matched instead.
                    groupWithTip
                        "Library"
                        "Ready-made parameterised components. Choosing one adds its sheet to \
                         this project and asks for the values its parameters should take."
                        (model.ComponentLibraries
                         |> List.filter (fun lib -> keep lib.Name "")
                         |> List.map (fun lib ->
                            // the components are read HERE, when the library is opened. This
                            // is an event handler, not the render function, so it may read the
                            // disk; startup knows only that the library exists.
                            menuItem styles lib.Name (fun _ ->
                                let opened = ComponentLibraries.openLibrary lib
                                dispatch <| UpdateModel (Optics.Optic.set openLibrary_ (Some opened)))))
                    ]

            div [Style styles] [
                searchBox
                Menu.menu [Props [Class "py-1"; Style ([Height "calc(100vh - 240px)"; OverflowY OverflowOptions.Auto] @ styles)]]
                    (match sections |> List.filter (fun s -> not (isNull (box s))) with
                     | [] -> [noMatches]
                     | drawn -> drawn)
            ]

        /// The catalogue body replaced by one opened library's components, grouped into sections
        /// as the catalogue itself is. Back returns to the catalogue.
        let viewLibrary (library: ComponentLibraries.OpenedLibrary) = fun (model: Model) ->
            let styles =
                match model.Sheet.Action with
                | SheetT.InitialisedCreateComponent _ -> [Cursor "grabbing"]
                | _ -> []
            let componentItem (listing: ComponentLibraries.LibraryListing) =
                div [ HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                      Tooltip.dataTooltip listing.Header.Description
                      Style styles ]
                    [ placementItem styles (GhostBox listing.Header.Name) listing.Header.Name
                        (fun () -> dispatchAsFunc (startPlacingLibraryComponent library listing))
                        model dispatch ]
            Menu.menu [Props [Class "py-1"; Style ([Height "calc(100vh - 200px)"; OverflowY OverflowOptions.Auto] @ styles)]] [
                div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; JustifyContent "space-between"; MarginBottom "6px"]] [
                    b [] [str $"{library.Name} library"]
                    Button.button [
                        Button.Size IsSmall
                        Button.OnClick (fun _ -> dispatch <| UpdateModel (Optics.Optic.set openLibrary_ None))
                    ] [str "Back"]
                ]
                // a component that will not read costs that component, not the library: the rest
                // are still offered, and the trouble is said out loud rather than swallowed
                match library.Problems with
                | [] -> null
                | problems ->
                    div [Style [Color "red"; FontSize "11px"; MarginBottom "6px"]]
                        (problems |> List.map (fun problem -> div [] [str problem]))
                yield!
                    library.Components
                    |> List.groupBy (fun listing -> listing.Header.Section)
                    |> List.sortBy fst
                    |> List.map (fun (section, comps) ->
                        makeMenuGroup section (comps |> List.map componentItem))
            ]

        match model.OpenLibrary with
        | Some library -> viewLibrary library model
        | None -> viewCatOfModel model
