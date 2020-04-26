module Diagram

open Fulma
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser
open Fable.Core
open Fable.Core.JsInterop

open DiagramTypes
open Draw2dWrapper
open JSHelpers
open CommonStyle
open DiagramStyle
open Extractor
open Simulator

open StateIO

type Model = {
    Diagram : Draw2dWrapper
    State : CanvasState // TODO: remove
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    RightTab : RightTab
    // If the content of the diagram has been loaded or saved from/to file keep
    // track of the path, instead of reasking every time.
    OpenPath : string option
    Hilighted : ComponentId list * ConnectionId list
}

// -- Init Model

let init() = {
    Diagram = new Draw2dWrapper()
    State = [], []
    SelectedComponent = None
    Simulation = None
    RightTab = Catalogue
    OpenPath = None
    Hilighted = [], []
}

// -- Create View

let prettyPrintState (components, connections) =
    [ str "Components:"; br [] ] @
    List.collect (fun c -> [ str <| sprintf "%A" c; br [] ]) components @
    [ str "Connections:"; br [] ] @
    List.collect (fun c -> [ str <| sprintf "%A" c; br [] ]) connections

let getStateAction model dispatch =
    match model.Diagram.GetCanvasState () with
    | None -> ()
    | Some state -> extractState state |> UpdateState |> dispatch

let saveStateAction model dispatch =
    match model.Diagram.GetCanvasState () with
    | None -> ()
    | Some state -> extractState state
                    |> saveStateToFile model.OpenPath
                    |> SetOpenPath
                    |> dispatch 

let loadStateAction model dispatch =
    dispatch <| SetHighlighted ([],[])
    loadStateFromFile model.Diagram |> SetOpenPath |> dispatch

// Views

let viewCatalogue model =
    let menuItem label onClick =
        Menu.Item.li
            [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick ] ]
            [ str label ]
    Menu.menu [ ] [
            Menu.label [ ] [ str "Input / Output" ]
            Menu.list []
                [ menuItem "Input"  (fun _ -> model.Diagram.CreateComponent Input "input")
                  menuItem "Output" (fun _ -> model.Diagram.CreateComponent Output "output") ]
            Menu.label [ ] [ str "Gates" ]
            Menu.list []
                [ menuItem "Not"  (fun _ -> model.Diagram.CreateComponent Not "")
                  menuItem "And"  (fun _ -> model.Diagram.CreateComponent And "")
                  menuItem "Or"   (fun _ -> model.Diagram.CreateComponent Or "")
                  menuItem "Xor"  (fun _ -> model.Diagram.CreateComponent Xor "")
                  menuItem "Nand" (fun _ -> model.Diagram.CreateComponent Nand "")
                  menuItem "Nor"  (fun _ -> model.Diagram.CreateComponent Nor "")
                  menuItem "Xnor" (fun _ -> model.Diagram.CreateComponent Xnor "") ]
            Menu.label [ ] [ str "Mux / Demux" ]
            Menu.list []
                [ menuItem "Mux2" (fun _ -> model.Diagram.CreateComponent Mux2 "mux2") ]
        ]

let viewSelectedComponent model =
    match model.SelectedComponent with
    | None -> div [] [ str "Select a component in the diagram" ]
    | Some comp ->
        let formId = "component-properties-form-" + comp.Id
        let readOnlyFormField name value =
            Field.div [] [
                Label.label [] [ str name ]
                Control.div [] [ Input.text [ Input.Props [ ReadOnly true; Name name ]; Input.IsStatic true; Input.Value value ] ] ]
        let formField name defaultValue =
            // Use comp.Id as key to refresh. DefaultValue is only updated when
            // the form is created and not anymore. The Key force the re-rendering
            // of the element every time the Key value changes.
            Field.div [] [
                Label.label [] [ str name ]
                Control.div [] [ Input.text [ Input.Props [ Name name; Key comp.Id ]; Input.DefaultValue defaultValue ] ] ]
        let formButton text onClick =
            Field.div [ Field.IsGrouped ] [
                Control.div [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun e ->
                            e.preventDefault()
                            onClick ()
                        )
                    ] [ str text ]
                ]
            ]
        form [ Id formId ] [
            readOnlyFormField "Id" comp.Id
            readOnlyFormField "Type" <| sprintf "%A" comp.Type
            formField "Label" comp.Label
            readOnlyFormField "Input ports" <| sprintf "%d" comp.InputPorts.Length
            readOnlyFormField "Output ports" <| sprintf "%d" comp.OutputPorts.Length
            // Submit.
            formButton "Update" (fun _ ->
                // TODO: dont think this is the right way to do it.
                let form = document.getElementById <| formId
                let label : string = getFailIfNull form ["elements"; "Label"; "value"]
                model.Diagram.EditComponent comp.Id label
            )
        ]

let viewSimulationInputs (simulationGraph : SimulationGraph) (inputs : (SimulationIO * Bit) list) dispatch =
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel), bit) =
        let bitButton =
            Button.button [
                Button.Props [ Style [ Width "50px" ] ]
                Button.Color (match bit with Zero -> IsBlack | One -> IsPrimary)
                Button.Size IsSmall
                Button.OnClick (fun _ ->
                    let newBit = match bit with
                                 | Zero -> One
                                 | One -> Zero
                    feedSimulationInput simulationGraph (ComponentId inputId) newBit
                    |> SetSimulationGraph |> dispatch
                )
            ] [ str (match bit with Zero -> "0" | One -> "1") ]
        Level.level [] [
            Level.left [] [
                Level.item [] [ str inputLabel ]
            ]
            Level.right [] [
                Level.item [] [ bitButton ]
            ]
        ]
    div [] (
        // Sort inputs by label.
        inputs
        |> List.sortBy (fun ((_, ComponentLabel label), _) -> label)
        |> List.map makeInputLine
    )

let viewSimulationOutputs (simOutputs : (SimulationIO * Bit) list) =
    div [] (
        simOutputs
        |> List.collect (fun ((_, ComponentLabel outputLabel), bit) ->
            [ str <| sprintf "%s\t%A" outputLabel bit; br [] ]
        )
    )

let viewSimulationError (simError : SimulationError) dispatch =
    (simError.ComponentsAffected, simError.ConnectionsAffected)
    |> SetHighlighted |> dispatch
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        str simError.Msg
    ]

let viewSimulationData (simData : SimulationData) dispatch =
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
        viewSimulationInputs
            simData.Graph
            (extractSimulationIOs simData.Inputs simData.Graph)
            dispatch
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Outputs" ]
        viewSimulationOutputs <| extractSimulationIOs simData.Outputs simData.Graph
    ]

let viewSimulation model dispatch =
    let startSimulation () =
        match model.Diagram.GetCanvasState () with
        | None -> ()
        | Some jsState -> extractState jsState
                         |> prepareSimulation
                         |> StartSimulation
                         |> dispatch
    match model.Simulation with
    | None ->
        div [] [
            Button.button
                [ Button.Color IsSuccess; Button.OnClick (fun _ -> startSimulation()) ]
                [ str "Start simulation" ]
        ]
    | Some sim ->
        let body = match sim with
                   | Error simError -> viewSimulationError simError dispatch
                   | Ok simData -> viewSimulationData simData dispatch
        let endSimulation _ =
            dispatch <| SetHighlighted ([], [])
            dispatch EndSimulation
        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick endSimulation ]
                [ str "End simulation" ]
            body
        ]

let viewRightTab model dispatch =
    match model.RightTab with
    | Catalogue ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ] ] [ str "Click component to add it to the diagram" ]
            viewCatalogue model
        ]
    | Properties ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Component properties" ]
            viewSelectedComponent model
        ]
    | Simulation ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Simulation" ]
            viewSimulation model dispatch
        ]

let hideView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Hidden
    ]

let displayView model dispatch =
    div [] [
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Visible
        div [ rightSectionStyle ] [
            Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed; Tabs.Props [ ] ] [
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.RightTab = Catalogue) ]
                    [ a [ OnClick (fun _ -> ChangeRightTab Catalogue |> dispatch ) ] [ str "Catalogue" ] ]
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.RightTab = Properties) ]
                    [ a [ OnClick (fun _ -> ChangeRightTab Properties |> dispatch ) ] [ str "Properties" ] ]
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.RightTab = Simulation) ]
                    [ a [ OnClick (fun _ -> ChangeRightTab Simulation |> dispatch ) ] [ str "Simulation" ] ]
            ]
            viewRightTab model dispatch
        ]
        div [ bottomSectionStyle ] [
            Button.button [ Button.Props [ OnClick (fun _ -> getStateAction model dispatch) ] ] [ str "Get state" ]
            Button.button [ Button.Props [ OnClick (fun _ -> saveStateAction model dispatch ) ] ] [ str "Save diagram" ]
            Button.button [ Button.Props [ OnClick (fun _ -> loadStateAction model dispatch) ] ] [ str "Load diagram" ]
            Button.button [ Button.Props [ OnClick (fun _ -> model.Diagram.Undo ()) ] ] [ str "Undo" ]
            Button.button [ Button.Props [ OnClick (fun _ -> model.Diagram.Redo ()) ] ] [ str "Redo" ]
            div [] (prettyPrintState model.State)
        ]
    ]

// -- Update Model

let handleJSDiagramMsg msg model =
    match msg with
    | InitCanvas canvas -> // Should be triggered only once.
        model.Diagram.InitCanvas canvas
        model
    | SelectComponent jsComponent ->
        { model with SelectedComponent = Some <| extractComponent jsComponent }
    | UnselectComponent jsComponent ->
         { model with SelectedComponent = None }

let update msg model =
    match msg with
    | JSDiagramMsg msg' -> handleJSDiagramMsg msg' model
    | UpdateState (com, con) -> { model with State = (com, con) }
    | StartSimulation simData -> { model with Simulation = Some simData }
    | SetSimulationGraph graph ->
        match model.Simulation with
        | None -> failwithf "what? Simulation graph set when no simulation running"
        | Some sim ->
            match sim with
            | Error _ -> failwithf "what? Simulation graph set when simulation is error"
            | Ok simData -> { model with Simulation = { simData with Graph = graph } |> Ok |> Some }
    | EndSimulation -> { model with Simulation = None }
    | ChangeRightTab newTab -> { model with RightTab = newTab }
    | SetOpenPath openPath -> { model with OpenPath = openPath }
    | SetHighlighted (componentIds, connectionIds) ->
        let oldComponentIds, oldConnectionIds = model.Hilighted
        oldComponentIds
        |> List.map (fun (ComponentId c) -> model.Diagram.UnHighlightComponent c)
        |> ignore
        componentIds
        |> List.map (fun (ComponentId c) -> model.Diagram.HighlightComponent c)
        |> ignore
        oldConnectionIds
        |> List.map (fun (ConnectionId c) -> model.Diagram.UnHighlightConnection c)
        |> ignore
        connectionIds
        |> List.map (fun (ConnectionId c) -> model.Diagram.HighlightConnection c)
        |> ignore
        { model with Hilighted = (componentIds, connectionIds) }
