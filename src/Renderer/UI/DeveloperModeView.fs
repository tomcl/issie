module DeveloperModeView

open EEExtensions
open VerilogTypes
open Fulma

open Fable.React
open Fable.React.Props

open JSHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupHelpers
open UIPopups
open Notifications
open Sheet.SheetInterface
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open FilesIO
open CatalogueView
open TopMenuView
open MenuHelpers
open DiagramStyle
open BlockHelpers
open SheetBeautifyHelpers

let selected = "Level 1"

let developerModeView (model: ModelType.Model) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let menuItem menuName description (level: BeautifyLevel) dispatch =

        Menu.Item.li
            [ (Menu.Item.IsActive(level = model.BeautifyLevel))
              Menu.Item.OnClick(fun _ -> dispatch (SelectBeautifyLevel level)) ]
            [ strong [] [ str menuName ]; p [] [ str description ] ]

    let beautificationLevelSelect =
        Menu.menu
            []
            [ Menu.label [] [ str "Beautification Level" ]
              Menu.list
                  []
                  [ menuItem "Level 1: Standard" "Fastest performance" (Level1) dispatch
                    menuItem "Level 2: Enhanced" "For larger projects" (Level2) dispatch
                    menuItem "Level 3: Aggressive" "May cause artefacts" (Level3) dispatch ] ]

    let instructionText =
        div
            [ Style [ Margin "15px 0 200px 0" ] ]
            [ p [] [ str "Sample Text 1" ]
              p [] [ str "Sample Text 2" ]
              p [] [ str "Sample Text 3" ] ]

    let intersectCounter =
        div
            [ Style [ Margin "15px 0" ] ]
            [ Level.level
                  []
                  [ Level.item
                        [ Level.Item.HasTextCentered ]
                        [ div
                              []
                              [ Level.heading [] [ str "Wire-Sym Intersects" ]
                                Level.title [] [ str ((countVisibleSegsIntersectingSymbols model.Sheet).ToString()) ] ] ]
                    Level.item
                        [ Level.Item.HasTextCentered ]
                        [ div
                              []
                              [ Level.heading [] [ str "Sym-Sym Intersects" ]
                                Level.title [] [ str ((countIntersectingSymbolPairs model.Sheet).ToString()) ] ] ] ] ]
    let statsCounter =
        div
            [ Style [ Margin "15px 0" ] ]
            [ Level.level
                  []
                  [ Level.item
                        [ Level.Item.HasTextCentered ]
                        [ div
                              []
                              [ Level.heading [] [ str "90º Degree Wire Bends" ]
                                Level.title [] [ str ((countVisibleBends model.Sheet).ToString()) ] ] ]
                    Level.item
                        [ Level.Item.HasTextCentered ]
                        [ div [] [ Level.heading [] [ str "Near-Straight Wires" ]; Level.title [] [ str "TBC" ] ] ] ] ]

    let counterMenu =
        details
            [ Open(model.SheetStatsExpanded)
              OnClick(fun _ -> dispatch (ToggleSheetStats)) ]
            [ summary [ menuLabelStyle ] [ str "Sheet Stats " ]
              p
                  []
                  [ p
                        []
                        [ str ("MousePos: ")
                          code
                              []
                              [ str (
                                    (model.Sheet.LastMousePos.X.ToString("F2"))
                                    + ", "
                                    + (model.Sheet.LastMousePos.Y.ToString("F2"))
                                ) ] ]
                    intersectCounter
                    statsCounter ] ]

    // for Symbol.PortMaps
    let createTableFromPortMapsOrder (map: Map<Edge, string list>) =
        Table.table
            []
            (map
             |> Map.toList
             |> List.map (fun (edge, strList) ->
                 tr
                     []
                     [ td [] [ str (edge.ToString()) ]
                       td
                           []
                           (strList
                            |> List.collect (fun s -> [ code [] [ str ("• " + s) ]; br [] ])) ]))

    let symbolToListItem (model: ModelType.Model) (symbol: Symbol) =
        let SymbolTableInfo =
            (Table.table
                [ Table.IsFullWidth; Table.IsBordered ]
                [ tbody
                      []
                      [ tr
                            []
                            [ td [] [ strong [] [ str "Id: " ] ]
                              td [] [ code [] [ str (symbol.Id.ToString()) ] ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "Pos: " ] ]
                              td
                                  []
                                  [ str (
                                        symbol.Pos.X.ToString("F2")
                                        + ", "
                                        + symbol.Pos.Y.ToString("F2")
                                    ) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "Comp. Type: " ] ]
                              td [] [ code [] [ str (symbol.Component.Type.ToString()) ] ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "Comp. Label: " ] ]
                              td [] [ str (symbol.Component.Label.ToString()) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "Comp. H,W: " ] ]
                              td
                                  []
                                  [ str (
                                        symbol.Component.H.ToString("F2")
                                        + ", "
                                        + symbol.Component.W.ToString("F2")
                                    ) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "STransform: " ] ]
                              td
                                  []
                                  [ str (
                                        "Rotation: "
                                        + symbol.STransform.Rotation.ToString()
                                    )
                                    br []
                                    str ("flipped: " + symbol.STransform.flipped.ToString()) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "HScale, VScale: " ] ]
                              td
                                  []
                                  [ (match symbol.HScale with
                                     | Some hscale -> str ("HScale: " + hscale.ToString("F2"))
                                     | None -> str "HScale: N/A")
                                    br []
                                    (match symbol.VScale with
                                     | Some vscale -> str ("VScale: " + vscale.ToString("F2"))
                                     | None -> str "VScale: N/A") ] ] ] ])

        let createTableFromPorts (portsMap: Map<string, Edge>) =
            let referencePortTable =
                // get a list of ports from the selected component. more efficient to search smaller list
                // than looking of ports in model.Sheet.Wire.Symbol.Symbols
                symbol.Component.InputPorts
                @ symbol.Component.OutputPorts
                |> List.map (fun port -> port.Id, port)
                |> Map.ofList
            let portDetailMap =
                portsMap
                |> Map.map (fun key _ -> Map.tryFind key referencePortTable)
                |> Map.filter (fun _ value -> value.IsSome)
                |> Map.map (fun _ value -> value.Value)
            let tableRows =
                portDetailMap
                |> Map.toList
                |> List.map (fun (key, port) ->
                    tr
                        []
                        [ td [] [ code [] [ str port.Id ] ]
                          td
                              []
                              [ str (
                                    match port.PortNumber with
                                    | Some num -> num.ToString()
                                    | None -> "N/A"
                                ) ]
                          td
                              []
                              [ str (
                                    match port.PortType with
                                    | CommonTypes.PortType.Input -> "In"
                                    | CommonTypes.PortType.Output -> "Out"
                                ) ]
                          td [] [ code [] [ str port.HostId ] ] ])
            Table.table
                []
                [ tr
                      []
                      [ th [] [ str "Port Id" ]
                        th [] [ str "No." ]
                        th [] [ str "Type" ]
                        th [] [ str "Host Id" ] ]
                  yield! tableRows ]
        [ details
              [ Open(model.SymbolInfoTableExpanded)
                OnClick(fun _ -> dispatch (ToggleSymbolInfoTable)) ]
              [ summary [ menuLabelStyle ] [ str "Symbol " ]; p [] [ SymbolTableInfo ] ]
          details
              [ Open model.SymbolPortsTableExpanded
                OnClick(fun _ -> dispatch (ToggleSymbolPortsTable)) ]
              [ summary [ menuLabelStyle ] [ str "Ports" ]
                p [] [ (createTableFromPorts symbol.PortMaps.Orientation) ] ]
          details
              [ Open model.SymbolPortMapsTableExpanded
                OnClick(fun _ -> dispatch (ToggleSymbolPortMapsTable)) ]
              [ summary [ menuLabelStyle ] [ str "PortMaps" ]
                p [] [ (createTableFromPortMapsOrder symbol.PortMaps.Order) ] ] ]

    let wireToListItem (wire: Wire) =
        let WireTableInfo =
            (Table.table
                [ Table.IsFullWidth; Table.IsBordered ]
                [ tbody
                      []
                      [ tr
                            []
                            [ td [] [ strong [] [ str "WId: " ] ]
                              td [] [ code [] [ str (wire.WId.ToString()) ] ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "StartPos: " ] ]
                              td
                                  []
                                  [ str (
                                        wire.StartPos.X.ToString("F2")
                                        + ", "
                                        + wire.StartPos.Y.ToString("F2")
                                    ) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "InputPort: " ] ]
                              td [] [ code [] [ str (wire.InputPort.ToString()) ] ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "OutputPort: " ] ]
                              td [] [ code [] [ str (wire.OutputPort.ToString()) ] ] ]
                        tr [] [ td [] [ strong [] [ str "Width: " ] ]; td [] [ str (wire.Width.ToString()) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "InitialOrientation: " ] ]
                              td [] [ str (wire.InitialOrientation.ToString()) ] ] ] ])

        let createTableFromASegments (segments: ASegment list) =
            Table.table
                []
                [ tr
                      []
                      [ th [] [ str "Len" ]
                        th [] [ str "Start" ]
                        th [] [ str "End" ]
                        th [] [ str "Drag?" ]
                        th [] [ str "Route?" ] ]
                  yield!
                      segments
                      |> List.map (fun seg ->
                          tr
                              []
                              [ td [] [ str (sprintf "%.1f" seg.Segment.Length) ]
                                td [] [ str (sprintf "%.1f, %.1f" seg.Start.X seg.Start.Y) ]
                                td [] [ str (sprintf "%.1f, %.1f" seg.End.X seg.End.Y) ]

                                td
                                    []
                                    [ str (
                                          if seg.Segment.Draggable then
                                              "T"
                                          else
                                              "F"
                                      ) ]
                                td
                                    []
                                    [ str (
                                          match seg.Segment.Mode with
                                          | Manual -> "M"
                                          | Auto -> "A"
                                      ) ] ]) ]

        let absSegments = getAbsSegments wire
        let WireSegmentsTableInfo = createTableFromASegments absSegments

        [ details
              [ Open model.WireTableExpanded; OnClick(fun _ -> dispatch (ToggleWireTable)) ]
              [ summary [ menuLabelStyle ] [ str "Wire " ]; p [] [ WireTableInfo ] ]
          details
              [ Open model.WireSegmentsTableExpanded
                OnClick(fun _ -> dispatch (ToggleWireSegmentsTable)) ]
              [ summary [ menuLabelStyle ] [ str "Wire Segments" ]
                p [] [ WireSegmentsTableInfo ] ] ]

    let viewComponent =
        match model.Sheet.SelectedComponents, model.Sheet.SelectedWires with
        | [ compId: ComponentId ], [] ->
            let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
            let symbol: SymbolT.Symbol = model.Sheet.Wire.Symbol.Symbols[compId]

            div [ Key comp.Id ] [ ul [] (symbolToListItem model symbol) ]
        | [], [ wireId: ConnectionId ] ->
            let wire = model.Sheet.Wire.Wires.[wireId]
            div [ Key(wireId.ToString()) ] [ ul [] (wireToListItem wire) ]
        | _ ->
            match model.CurrentProj with
            | Some proj ->
                let sheetName = proj.OpenFileName
                let sheetLdc =
                    proj.LoadedComponents
                    |> List.find (fun ldc -> ldc.Name = sheetName)
                let sheetDescription = sheetLdc.Description

                div
                    []
                    [ p [] [ str "Select a component in the diagram to view its attributes." ]
                      br [] ]
            | None -> null

    let viewComponentWrapper =
        div [ Style [ Margin "15px 0" ] ] [ p [ menuLabelStyle ] []; viewComponent ]
    div [ Style [ Margin "0 0 20px 0" ] ] ([ beautificationLevelSelect; counterMenu; viewComponentWrapper ])
