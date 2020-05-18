module DiagramMainView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open BusWidthInferer
open BusTypes

open DiagramStyle
open SimulatorTypes
open DiagramMessageType
open DiagramModelType
open Draw2dWrapper
open Extractor
open OnDiagramButtonsView
open CatalogueView
open SelectedComponentView
open SimulationView
open PopupView
open FileMenuView

// -- Init Model

let init() = {
    Diagram = new Draw2dWrapper()
    SelectedComponent = None
    Simulation = None
    RightTab = Catalogue
    CurrProject = None
    Hilighted = [], []
    Clipboard = [], []
    Popup = None
    PopupDialogData = {Text = None; Int = None}
    Notifications = {
        FromDiagram = None
        FromSimulation = None
    }
}

let runBusWidthInference model =
    let paintEach connsWidth =
        connsWidth
        |> Map.map (fun (BusTypes.ConnectionId connId) width ->
            match width with
            | None -> () // Could not infer.
            | Some w -> model.Diagram.PaintConnection connId w
        )
        |> ignore
    match model.Diagram.GetCanvasState () with
    | None -> model
    | Some state ->
        state |> extractState |> inferConnectionsWidth |> function
            | Error e ->
                // TODO: this makes the conent of the model.Higlighted inconsistent.
                // Need to dispatch SetHighlighted (can do by using mkProgram).
                e.ConnectionsAffected
                |> List.map (fun (BusTypes.ConnectionId c) -> model.Diagram.HighlightConnection c)
                |> ignore
                // Display notification with error message.
                { model with Notifications =
                                { model.Notifications with FromDiagram =
                                                            Some <| errorNotification e.Msg CloseDiagramNotification} }
            | Ok connsWidth ->
                paintEach connsWidth
                // Close the notification if all is good.
                { model with Notifications = {model.Notifications with FromDiagram = None} }

// -- Create View

/// Display the content of the right tab.
let viewRightTab model dispatch =
    match model.RightTab with
    | Catalogue ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ] ] [ str "Click on a component to add it to the diagram" ]
            viewCatalogue model dispatch
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
        viewTopMenu model dispatch
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) Visible
        viewNoProjectMenu model dispatch
        viewPopup model
        viewNotifications model dispatch
        viewOnDiagramButtons model dispatch
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
    ]

// -- Update Model

let handleJSDiagramMsg msg model =
    match msg with
    | InitCanvas canvas -> // Should be triggered only once.
        model.Diagram.InitCanvas canvas
        model
    | SelectComponent jsComponent ->
        { model with SelectedComponent = Some <| extractComponent jsComponent }
    | UnselectComponent () ->
        { model with SelectedComponent = None }
    | InferWidths () ->
        runBusWidthInference model

let update msg model =
    match msg with
    | JSDiagramMsg msg' -> handleJSDiagramMsg msg' model
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
    | SetProject project -> { model with CurrProject = Some project }
    | CloseProject -> { model with CurrProject = None }
    | ShowPopup popup -> { model with Popup = Some popup }
    | ClosePopup -> { model with Popup = None; PopupDialogData = {Text = None; Int = None} }
    | SetPopupDialogText text ->
        { model with PopupDialogData = {model.PopupDialogData with Text = text} }
    | SetPopupDialogInt int ->
        { model with PopupDialogData = {model.PopupDialogData with Int = int} }
    | CloseDiagramNotification ->
        { model with Notifications = {model.Notifications with FromDiagram = None} }
    | SetSimulationNotification n ->
        { model with Notifications =
                        { model.Notifications with FromSimulation = Some n}}
    | CloseSimulationNotification ->
        { model with Notifications = {model.Notifications with FromSimulation = None} }
