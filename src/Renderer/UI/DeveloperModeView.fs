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
open SheetBeautifyD1
open SheetUpdateHelpers
open Symbol
open Optics
open BusWireRoute
open BusWireRoutingHelpers.Constants
open BusWireRoutingHelpers
open Sheet

/// returns the an string ID with extra formatting of a hovered wire, symbol, or port
let findHoveredID (pos: XYPos) (model: SheetT.Model) =
    let dummySymbolId: ComponentId = ComponentId "dummy"
    // we add a 'dummy symbol' to the model to represent the mouse position
    // solely for calculation purposes, it will not be added to the actual model
    let h, w = 30.0, 30.0
    let mouseComponentDummy =
        { Id = "dummy"
          Type = Not
          Label = "dummy"
          InputPorts = List.empty
          OutputPorts = List.empty
          X = pos.X - float w / 2.0
          Y = pos.Y - float h / 2.0
          H = float h
          W = float w
          SymbolInfo = None }
    let mouseSymbolDummy: Symbol =
        { (createNewSymbol [] pos NotConnected "" White) with
            Component = mouseComponentDummy }
    let dummyModel =
        model
        |> Optic.set (SheetT.symbols_) (Map.add dummySymbolId mouseSymbolDummy model.Wire.Symbol.Symbols)
        |> updateBoundingBoxes // just in case

    let mouseBoundingBox = getSymbolBoundingBox mouseSymbolDummy

    let intersectingWiresInfo =
        dummyModel.Wire.Wires
        |> Map.values
        // findWireSymbolIntersections returns a list of bounding boxes of symbols intersected by wire.
        |> Seq.map (fun wire -> (wire, (findWireSymbolIntersections dummyModel.Wire wire)))
        // we have (Wire * BoundingBox list) seq. Now to look through every tuple and get any wire whose bbox list contains symbolBoundingBox
        // we might get more than one wire – so get a list
        |> Seq.choose (fun (wire, bboxes) ->

            if
                bboxes
                |> List.exists (fun box ->

                    // findWireSymbolIntersections returns bounding boxes that have been enlarged with minWireSeparation
                    let correctedBox =
                        { W = box.W - minWireSeparation * 2.
                          H = box.H - minWireSeparation * 2.
                          TopLeft =
                            box.TopLeft
                            |> updatePos Right_ minWireSeparation
                            |> updatePos Down_ minWireSeparation }
                    // printf "cBox: %A mouseBBox: %A" correctedBox mouseBoundingBox
                    mouseBoundingBox = correctedBox)
            then
                Some(wire.WId.ToString())

            else
                None)
        |> Seq.toList
        // return the first element if it exists else None
        |> List.tryHead

    let intersectingSymbolInfo =
        model.BoundingBoxes
        |> Map.toList
        // get all boundingBoxes in model not equal to symbolBoundingBox
        |> List.filter (fun (compId, box) -> not (box =~ mouseBoundingBox))
        // see if they overlap with the symbolBoundingBox, if they do, return the compId
        |> List.choose (fun (compId, box) ->
            match (overlapArea2DBox mouseBoundingBox box) with
            | Some area -> Some(compId.ToString())
            | None -> None)
        |> List.tryHead

    // inpisred by Sheet.mouseOn
    match intersectingWiresInfo, intersectingSymbolInfo with
    | _, Some symbolId ->
        let inputPorts, outputPorts =
            Symbol.getPortLocations model.Wire.Symbol [ ComponentId symbolId ]
            |> fun (x, y) -> Map.toList x, Map.toList y
        match mouseOnPort inputPorts pos 2.5 with
        | Some(portId, portLoc) -> "InputPort: ", portId.ToString()
        | None ->
            match mouseOnPort outputPorts pos 2.5 with
            | Some(portId, portLoc) -> "OutputPort: ", portId.ToString()
            | None -> "Symbol: ", symbolId.ToString()
    | Some wireId, _ -> "Wire: ", wireId.ToString()
    | _ -> "Component: ", "Nothing Selected"

let developerModeView (model: ModelType.Model) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let menuItem menuName description (level: BeautifyLevel) dispatch =

        Menu.Item.li
            [ (Menu.Item.IsActive(level = model.BeautifyLevel))
              Menu.Item.OnClick(fun _ -> dispatch (SelectBeautifyLevel level)) ]
            [ strong [] [ str menuName ]; p [] [ str description ] ]

    let beautificationLevelSelect =
        let beautifyMenu =
            Menu.menu
                []
                [ Menu.list
                      []
                      [ menuItem "Level 1: Standard" "Fastest performance" (Level1) dispatch
                        menuItem "Level 2: Enhanced" "For larger projects" (Level2) dispatch
                        menuItem "Level 3: Aggressive" "May cause artefacts" (Level3) dispatch ] ]
        details
            [ Open(model.BeautifyMenuExpanded) ]
            [ summary
                  [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleBeautifyMenu)) ]
                  [ str "Beautification Level " ]
              div [] [ beautifyMenu ] ]

    let instructionText =
        div
            [ Style [ Margin "15px 0 200px 0" ] ]
            [ p [] [ str "Sample Text 1" ]
              p [] [ str "Sample Text 2" ]
              p [] [ str "Sample Text 3" ] ]

    let createCounterItem title value =
        Level.item
            [ Level.Item.HasTextCentered ]
            [ div
                  []
                  [ Level.heading [] [ str title ]
                    strong [ Style [ FontSize "20px" ] ] [ str value ] ] ]

    let counters =
        let counterItems =
            [ ("Wire-Sym Intersects", (countVisibleSegsIntersectingSymbols model.Sheet).ToString())
              ("Wire-Wire Intersects", (countVisibleSegsPerpendicularCrossings model.Sheet).ToString())
              ("Sym-Sym Intersects", (countIntersectingSymbolPairs model.Sheet).ToString())
              ("90º Degree Wire Bends", (countVisibleBends model.Sheet).ToString())
              ("Near-Straight Wires", (countAlmostStraightWiresOnSheet model.Sheet).ToString())
              ("Singly-Conn Wires", (countSinglyConnectedWires model.Sheet).ToString())
              ("Vis. Seg. Length", (countVisibleSegmentLength model.Sheet).ToString("F2"))
              ("Sym-Sym Overlap", (countIntersectingSymbolPairsWithOverlapArea model.Sheet).ToString()) ]
        //   ("Free Space!!!", ":)") ]

        counterItems
        |> List.chunkBySize 2
        |> List.map (fun chunk ->
            div
                [ Style [ Margin "5px 0" ] ]
                [ Level.level
                      []
                      (chunk
                       |> List.map (fun (title, value) -> createCounterItem title value)) ])
        |> div []
    let hoveredType, hoveredId = findHoveredID model.Sheet.LastMousePos model.Sheet
    let sheetStatsMenu =
        details
            [ Open(model.SheetStatsExpanded) ]
            [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSheetStats)) ] [ str "Sheet Stats " ]
              div
                  []
                  [ div
                        [ Style [ MarginBottom "10px" ] ]
                        [ strong [] [ str ("Mouse Position: ") ]
                          br []
                          code
                              []
                              [ str (
                                    (model.Sheet.LastMousePos.X.ToString("F2"))
                                    + ", "
                                    + (model.Sheet.LastMousePos.Y.ToString("F2"))
                                ) ]

                          br []
                          strong [] [ str ("Hovered " + hoveredType) ]
                          br []
                          code [] [ str (hoveredId) ] ]

                    counters ] ]

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
                        th [] [ str "I/O" ]
                        th [] [ str "Host Id" ] ]
                  yield! tableRows ]
        [ details
              [ Open(model.SymbolInfoTableExpanded) ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSymbolInfoTable)) ] [ str "Symbol " ]
                div [] [ SymbolTableInfo ] ]
          details
              [ Open model.SymbolPortsTableExpanded ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSymbolPortsTable)) ] [ str "Ports" ]
                div [] [ (createTableFromPorts symbol.PortMaps.Orientation) ] ]
          details
              [ Open model.SymbolPortMapsTableExpanded ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSymbolPortMapsTable)) ] [ str "PortMaps" ]
                div [] [ (createTableFromPortMapsOrder symbol.PortMaps.Order) ] ] ]

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
              [ Open model.WireTableExpanded ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleWireTable)) ] [ str "Wire " ]
                div [] [ WireTableInfo ] ]
          details
              [ Open model.WireSegmentsTableExpanded ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleWireSegmentsTable)) ] [ str "Wire Segments" ]
                div [] [ WireSegmentsTableInfo ] ] ]

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

    let viewComponentWrapper = div [] [ p [ menuLabelStyle ] []; viewComponent ]
    div [ Style [ Margin "0 0 20px 0" ] ] ([ beautificationLevelSelect; sheetStatsMenu; viewComponentWrapper ])
