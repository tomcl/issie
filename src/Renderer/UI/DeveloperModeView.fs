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

/// function that returns the an string ID with extra formatting of a hovered wire, symbol, or port
let findHoveredID (pos: XYPos) (model: SheetT.Model) =
    let dummySymbolId: ComponentId = ComponentId "dummy"
    // we add a 'dummy symbol' to the model to represent the mouse position
    // solely for calculation purposes, it will not be added to the actual model
    // for convenience, we let dummy symbol be 30x30, equal to a Not gate size
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

    // create a mouse dummy symbol, find its bounding box, add it to a dummy model
    let mouseSymbolDummy: Symbol =
        { (createNewSymbol [] pos NotConnected "" White) with
            Component = mouseComponentDummy }
    let dummyModel =
        model
        |> Optic.set (SheetT.symbols_) (Map.add dummySymbolId mouseSymbolDummy model.Wire.Symbol.Symbols)
        |> updateBoundingBoxes // just in case

    // we calculate the bounding box of the mouse
    let mouseBoundingBox = getSymbolBoundingBox mouseSymbolDummy

    // inspired by SheetBeautifyD1's findAllBoundingBoxesOfSymIntersections
    let intersectingWiresInfo =
        dummyModel.Wire.Wires
        |> Map.values
        // findWireSymbolIntersections returns a list of bounding boxes of symbols intersected by wire.
        // we find the wires that have a boundingBox in their intersection list that contains our mouseBoundingBox
        // we might get more than one wire – so get a list

        |> Seq.map (fun wire -> (wire, (findWireSymbolIntersections dummyModel.Wire wire)))
        |> Seq.choose (fun (wire, bboxes) ->
            if
                bboxes
                |> List.exists (fun box ->

                    // findWireSymbolIntersections returns bounding boxes that have been enlarged with minWireSeparation
                    // we correct this
                    let correctedBox =
                        { W = box.W - minWireSeparation * 2.
                          H = box.H - minWireSeparation * 2.
                          TopLeft =
                            box.TopLeft
                            |> updatePos Right_ minWireSeparation
                            |> updatePos Down_ minWireSeparation }
                    mouseBoundingBox =~ correctedBox)
            then
                Some(wire.WId.ToString())

            else
                None)
        |> Seq.toList
        |> List.tryHead

    // inspired by SheetBeautifyD1's findAllBoundingBoxesOfSymIntersections
    let intersectingSymbolInfo =
        model.BoundingBoxes
        |> Map.toList
        // get all boundingBoxes in model not equal to symbolBoundingBox, see if they overlap with symbolBoundingBox, if yes, return compId
        |> List.filter (fun (compId, box) -> not (box =~ mouseBoundingBox))
        |> List.choose (fun (compId, box) ->
            match (overlapArea2DBox mouseBoundingBox box) with
            | Some area -> Some(compId.ToString())
            | None -> None)
        |> List.tryHead

    // inpisred by Sheet.mouseOn
    // priority: check for mouse over ports first, then symbols, then wires
    // the code for checking for mouse over ports is the same as in Sheet.mouseOn
    // otherwise symbol and wire mouseover is calculated based on intersection with mouseBoundingBox
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

/// Top Level function for developer mode
let developerModeView (model: ModelType.Model) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let counterItems =
        [ ("Wire-Sym Intersects", (countVisibleSegsIntersectingSymbols model.Sheet).ToString())
          ("Wire-Wire Intersects", (countVisibleSegsPerpendicularCrossings model.Sheet).ToString())
          ("Sym-Sym Intersects", (countIntersectingSymbolPairs model.Sheet).ToString())
          ("90º Degree Wire Bends", (countVisibleBends model.Sheet).ToString())
          ("Near-Straight Wires", (countAlmostStraightWiresOnSheet model.Sheet).ToString())
          ("Singly-Conn Wires", (countSinglyConnectedWires model.Sheet).ToString())
          ("Vis. Seg. Length", (countVisibleSegmentLength model.Sheet).ToString("F1"))
          //   ("Sym-Sym Overlap", (countIntersectingSymbolPairsWithOverlapArea model.Sheet).ToString()) ]
          ("Free Space!!!", ":)") ]

    let menuItem menuName description (level: BeautifyLevel) dispatch =

        Menu.Item.li
            [ (Menu.Item.IsActive(level = model.BeautifyLevel))
              Menu.Item.OnClick(fun _ -> dispatch (SelectBeautifyLevel level)) ]
            [ strong [] [ str menuName ]; p [] [ str description ] ]

    let trackingMenuItem trackingMenuName (tracking: bool) (cachedStringData: (string list) option) dispatch =
        Menu.Item.li
            [ (Menu.Item.IsActive(tracking = model.Tracking))
              Menu.Item.OnClick(fun _ -> dispatch (SelectTracking(tracking, cachedStringData))) ]
            [ strong [] [ str trackingMenuName ] ]

    let settingsMenu =
        let settingsMenu =
            Menu.menu
                []
                [ Menu.list
                      []
                      [ trackingMenuItem "Turn On Tracker" true None dispatch
                        trackingMenuItem "Turn Off Tracker" false None dispatch ] ]

        details
            [ Open(model.SettingsMenuExpanded) ]
            [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSettingsMenu)) ] [ str "Settings " ]
              div [] [ settingsMenu ] ]

    /// A drop down menu that allows the user to select the level of beautification. Open/close state persists between updates thanks to
    /// a bool in the model called model.BeautifyMenuExpanded
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

    /// Some instructions for the user (deprecated)
    let instructionText =
        div
            [ Style [ Margin "15px 0 200px 0" ] ]
            [ p [] [ str "Sample Text 1" ]
              p [] [ str "Sample Text 2" ]
              p [] [ str "Sample Text 3" ] ]

    /// Create a counter item (a title + number) for the sheet stats menu
    let createCounterItem title value (cache: string option) =
        match cache with
        | Some cache ->
            Level.item
                [ Level.Item.HasTextCentered ]
                [ div
                      [ Style [ Width "170px" ] ]
                      [ Level.heading [] [ str title ]
                        strong [ Style [ FontSize "17px" ] ] [ str ((value) + "   (" + cache + ")") ] ] ]
        | _ ->
            Level.item
                [ Level.Item.HasTextCentered ]
                [ div
                      []
                      [ Level.heading [] [ str title ]
                        strong [ Style [ FontSize "17px" ] ] [ str (value) ] ] ]

    let trackerSetting =
        let cachedSheetStats = counterItems |> List.map snd

        div
            [ Style [ Margin "5px 0" ] ]
            [ Level.level
                  []
                  [ Level.item
                        [ Level.Item.HasTextCentered ]
                        [ div
                              []
                              [ Menu.list
                                    []
                                    [ trackingMenuItem "Turn On Tracker" (true) (Some cachedSheetStats) dispatch ] ] ]
                    Level.item
                        [ Level.Item.HasTextCentered ]
                        [ div [] [ Menu.list [] [ trackingMenuItem "Turn Off Tracker" (false) None dispatch ] ]

                          ] ]

              ]
    // trackingMenuItem "Turn On Tracker" "Keep track of changes across stats" (true) dispatch

    /// Create a list of counter items for the sheet stats menu. Can be expanded to include more stats
    /// Functions take in a SheetT.Model and output a string/int/float
    let counters =
        match model.CachedSheetStats with
        | Some cachedSheetStats ->
            let cachedChunks = cachedSheetStats |> List.chunkBySize 2
            let counterChunks = counterItems |> List.chunkBySize 2
            (cachedChunks, counterChunks)
            ||> List.map2 (fun cachedChunk counterChunk ->
                div
                    [ Style [ Margin "5px 0" ] ]
                    [ Level.level
                          []
                          ((cachedChunk, counterChunk)
                           ||> List.map2 (fun cache (title, value) -> createCounterItem title value (Some cache))) ])
            |> div []
        | _ ->
            let counterChunks = counterItems |> List.chunkBySize 2
            (counterChunks)
            |> List.map (fun counterChunk ->
                div
                    [ Style [ Margin "5px 0" ] ]
                    [ Level.level
                          []
                          (counterChunk
                           |> List.map (fun (title, value) -> createCounterItem title value None)) ])
            |> div []

    /// Stores string details of the currently hovered comp to be used in sheetStatsMenu
    let hoveredType, hoveredId = findHoveredID model.Sheet.LastMousePos model.Sheet

    /// Contains the mouse position, hovered comp data, and the counters
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

                    counters
                    trackerSetting ] ]

    /// Function to programmatically generate a html table from PortMaps.Order
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

    /// Function to programmatically generate a html table from a Map PortMaps.Oritentation
    let createTableFromPorts (portsMap: Map<string, Edge>) (symbol: Symbol) =
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

    /// Function to programmatically generate data for a symbol. Includes the symbol's data, its port data, and portmap
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

        // expandable menu persists between updates due to the model keeping track of the expanded state.
        // this is unlike the Catalogue menu that immediately shuts expandable menu when the user clicks away
        [ details
              [ Open(model.SymbolInfoTableExpanded) ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSymbolInfoTable)) ] [ str "Symbol " ]
                div [] [ SymbolTableInfo ] ]
          details
              [ Open model.SymbolPortsTableExpanded ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSymbolPortsTable)) ] [ str "Ports" ]
                div [] [ (createTableFromPorts symbol.PortMaps.Orientation symbol) ] ]
          details
              [ Open model.SymbolPortMapsTableExpanded ]
              [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSymbolPortMapsTable)) ] [ str "PortMaps" ]
                div [] [ (createTableFromPortMapsOrder symbol.PortMaps.Order) ] ] ]

    /// Function to programmatically generate data for a wire. Includes the wire's data and its segments
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

    /// Code taken from the Properties tab. If nothing is selected, a message is displayed.
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

    /// Top level div for the developer mode view
    let viewComponentWrapper = div [] [ p [ menuLabelStyle ] []; viewComponent ]
    div [ Style [ Margin "0 0 20px 0" ] ] ([ beautificationLevelSelect; sheetStatsMenu; viewComponentWrapper ])
