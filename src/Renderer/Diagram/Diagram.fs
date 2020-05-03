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
open Helpers
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
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    LoadedComponents : LoadedComponent list
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
    Clipboard = [], []
    LoadedComponents = []
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
    match loadStateFromFile () with
    | None -> ()
    | Some (path, canvasState) ->
        dispatch <| SetHighlighted ([],[]) // Remove current highlights.
        model.Diagram.FlushCommandStack () // Discard all undo/redo.
        model.Diagram.ClearCanvas()
        Some path |> SetOpenPath |> dispatch // Set the new filepath.
        // Finally load the new state in the canvas.
        let components, connections = canvasState
        List.map model.Diagram.LoadComponent components |> ignore
        List.map (model.Diagram.LoadConnection true) connections |> ignore

// TODO replace this with an openProject logic.
let loadComponentsFromFolder dispatch =
    (parseAllDiagramsInFolder "/home/marco/Documents/Imperial/FYP/diagrams/")
    |> SetLoadedComponents
    |> dispatch

let copyAction model dispatch =
    match model.Diagram.GetSelected () with
    | None -> ()
    | Some jsState -> extractState jsState |> SetClipboard |> dispatch

/// Map the port Ids of the old component to the equivalent Ports of the new
/// component. For example, if the component is a Not, the mapping will have two
/// entries, the input port and the output port.
let mapPorts (oldComp : Component) (newComp : Component) : (string * Port) list =
    let mapPort oldPort newPort =
        assertThat (oldPort.PortNumber = newPort.PortNumber) <| "cloned components have different port numbers"
        assertThat (oldPort.PortType = newPort.PortType) <| "cloned components have different port types"
        oldPort.Id, newPort
    assertThat (oldComp.Id <> newComp.Id) "cloned component has same id as old one"
    assertThat (oldComp.Type = newComp.Type) "cloned components have different types"
    assertThat (oldComp.Label = newComp.Label) "cloned components have different labels"
    let inputs = (oldComp.InputPorts, newComp.InputPorts) ||> List.map2 mapPort
    let outputs = (oldComp.OutputPorts, newComp.OutputPorts) ||> List.map2 mapPort
    inputs @ outputs

/// Transform a connection replacing the ports using the provided mapping.
let mapToNewConnection (portMappings : Map<string,Port>) oldConnection : Connection =
    let mapGet pId = match portMappings.TryFind pId with
                     | None -> failwithf "what? Could not find port Id %s while cloning" pId
                     | Some port -> port
    {
        Id = "" // This will be ignored and replaced with an actual new Id.
        Source = { (mapGet oldConnection.Source.Id) with PortNumber = None }
        Target = { (mapGet oldConnection.Target.Id) with PortNumber = None }
        Vertices = oldConnection.Vertices |> List.map (fun (x,y)->x+30.0,y+30.0)
    }

let pasteAction model =
    let oldComponents, oldConnections = model.Clipboard
    // Copy the old components and add them to the diagram.
    let newComponents =
        oldComponents
        |> List.map ((fun comp ->
            match model.Diagram.CreateComponent comp.Type comp.Label (comp.X+30) (comp.Y+30) with
            | None -> failwithf "what? Could not paste component %A" comp
            | Some jsComp -> jsComp) >> extractComponent)
    // The new (copied) components have been added to the diagram, now we need
    // to copy the connections among them.

    // Map the old components' ports to the new components' ports.
    let portMappings =
        (oldComponents, newComponents)
        ||> List.map2 mapPorts
        |> List.collect id
        |> Map.ofList
    // Iterate over the old connections replacing the ports to refer to the new
    // components, and add the newly created connections to the diagram.
    oldConnections
    |> List.map ((mapToNewConnection portMappings) >>
                 (model.Diagram.LoadConnection false))
    |> ignore

// Views

let viewCatalogue model =
    let menuItem label onClick =
        Menu.Item.li
            [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick ] ]
            [ str label ]
    let makeCustom loadedComponent =
        menuItem loadedComponent.Name (fun _ ->
            let custom = Custom {
                Name = loadedComponent.Name
                InputLabels = loadedComponent.InputLabels
                OutputLabels = loadedComponent.OutputLabels
            }
            model.Diagram.CreateComponent custom loadedComponent.Name 100 100
            |> ignore
        )
    Menu.menu [ ] [
            Menu.label [ ] [ str "Input / Output" ]
            Menu.list []
                [ menuItem "Input"  (fun _ -> model.Diagram.CreateComponent Input "input" 100 100 |> ignore)
                  menuItem "Output" (fun _ -> model.Diagram.CreateComponent Output "output" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Gates" ]
            Menu.list []
                [ menuItem "Not"  (fun _ -> model.Diagram.CreateComponent Not "" 100 100 |> ignore)
                  menuItem "And"  (fun _ -> model.Diagram.CreateComponent And "" 100 100 |> ignore)
                  menuItem "Or"   (fun _ -> model.Diagram.CreateComponent Or "" 100 100 |> ignore)
                  menuItem "Xor"  (fun _ -> model.Diagram.CreateComponent Xor "" 100 100 |> ignore)
                  menuItem "Nand" (fun _ -> model.Diagram.CreateComponent Nand "" 100 100 |> ignore)
                  menuItem "Nor"  (fun _ -> model.Diagram.CreateComponent Nor "" 100 100 |> ignore)
                  menuItem "Xnor" (fun _ -> model.Diagram.CreateComponent Xnor "" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Mux / Demux" ]
            Menu.list []
                [ menuItem "Mux2" (fun _ -> model.Diagram.CreateComponent Mux2 "mux2" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Custom" ]
            Menu.list []
                (model.LoadedComponents |> List.map makeCustom)
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
        | Some jsState -> (extractState jsState, model.LoadedComponents)
                          ||> prepareSimulation
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
        div [ canvasSmallMenuStyle ] [
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick (fun _ -> model.Diagram.Undo ()) ] ] [ str "< undo" ]
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick (fun _ -> model.Diagram.Redo ()) ] ] [ str "redo >" ]
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick (fun _ -> copyAction model dispatch) ] ] [ str "copy" ]
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick (fun _ -> pasteAction model); ] ] [ str "paste" ]
        ]
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
            Button.button [ Button.Props [ OnClick (fun _ -> loadComponentsFromFolder dispatch) ] ] [ str "Load components" ]
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
    | SetClipboard components -> { model with Clipboard = components }
    | SetLoadedComponents lc -> { model with LoadedComponents = lc }
