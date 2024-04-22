module DeveloperModeView

open EEExtensions
open VerilogTypes
open Fulma
open Fulma.Extensions.Wikiki
open System

open Fable.React
open Fable.React.Props

open JSHelpers
open ModelType
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DiagramStyle
open BlockHelpers
open DeveloperModeHelpers
open Symbol
open Optics
open BusWireRoute
open BusWireRoutingHelpers.Constants
open BusWireRoutingHelpers
open Sheet
open DrawModelType.SheetT



(*
STRUCTURE
1. mouseSensitiveDataSection
    a. Mouse Position
    b. Hovered Component Data
2. sheetStatsMenu
    a. Counters
    b. Hold/Unhold Button

// If a symbol is highlighted:
3. Symbol
4. Ports
5. PortMaps

// If a wire is highlighted:
3. Wire
4. Wire Segments
*)


/// Top Level function for developer mode
let developerModeView (model: ModelType.Model) dispatch =
// --------------------------------------------------- //
//                       Counters                      //
//     Feel free to modify `counterItems` as needed!   //
// --------------------------------------------------- //

    /// Contains a record of a counter's display name, tooltip description, and value
    /// A counter is a UI element for a function that takes in a SheetT.Model and outputs a string/int/float
    /// They output useful information about the sheet
    let counterItems =
        [
          {|DisplayName="T1 Sym-Sym Intersections"          ;
            ToolTipDescription = "Counts the number of symbols intersecting other \nsymbols on the sheet.";
            Value=(numOfIntersectedSymPairs model.Sheet).ToString() |}
          {|DisplayName="T2 Seg-Sym Intersections"      ;
            ToolTipDescription = "Counts the number of visible wire segments \nintersecting on the sheet";
            Value=(numOfIntersectSegSym model.Sheet).ToString() |}
          {|DisplayName="T3 Vis-Wire Seg 90º Cross"  ;
            ToolTipDescription = "Counts the number of visible wire segments that \nintersect at 90 degrees.";
            Value=(numOfWireRightAngleCrossings model.Sheet).ToString() |}
          {|DisplayName="T4 Sum of Vis-Wire Segs"   ;
            ToolTipDescription = "Counts the total length of all visible \nwire segments on the sheet.\n\n Assumption: \nOverlapping segments share the same starting net, and may\ndiverge at some point but will not return to overlap.";
            Value=(calcVisWireLength model.Sheet).ToString("F2") |}
          {|DisplayName="T5 Count Visible R-Angles"    ;
            ToolTipDescription = "Counts the number of visible right angles \nfound in the wire segments on the sheet.";
            Value=(numOfVisRightAngles model.Sheet).ToString() |}
          {|DisplayName="T6 RetracingSegments";
            ToolTipDescription = "Counts the number of retracing segments on sheet.\nZero-length segments with non-zero segments on \nboth sides that have lengths of opposite signs lead to a \nwire retracing itself";
            Value=(List.length (findRetracingSegments model.Sheet).RetraceSegsInSymbol).ToString() |}
           ]

// ----------------------------------------------------------------- //
//        Mouse Sensitive Data- Updates based on Mouse Position      //
// ----------------------------------------------------------------- //

    /// Stores string details of the currently hovered comp to be used in sheetStatsMenu
    let hoveredType, hoveredId = findHoveredID model.Sheet.LastMousePos model.Sheet

    /// Stores the mouse position and hovered component data
    let mouseSensitiveDataSection =
                    div
                        [ Style [ MarginBottom "20px" ] ]
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


// -------------------------------------------- //
//      Sheet Stats Menu (sheetStatsMenu)       //
// -------------------------------------------- //

    /// Contains the mouse position, hovered comp data, and the counters
    let sheetStatsMenu =
        /// Selecting the (hold/unhold) button shows/hides the current sheet counter stats to a column on sheetstats. Used for comparison purposes
        let holdUnholdButton =
            let cachedSheetStats = counterItems |> List.map (fun counterRecord -> counterRecord.Value)
            div
                [ Style [ Margin "5px 0 10px 0" ] ]
                [ Level.level
                    []
                    [ Level.item
                            [ Level.Item.HasTextCentered ]
                            [ div
                                [ Style [ FontSize "14px"; Width "100%"; Border "1.1px solid #555" ] ]
                                [ Menu.list []
                                    [ Menu.Item.li
                                        [(Menu.Item.IsActive(model.Tracking));
                                                    Menu.Item.OnClick(fun _ ->
                                                        let updatedCachedData =
                                                            match model.Tracking with
                                                            | true -> None
                                                            | false -> Some cachedSheetStats
                                                        dispatch (SelectTracking((not model.Tracking), updatedCachedData))
                                                    )]
                                        [ strong [] [ str "Hold/Unhold Values" ] ]
                                    ]
                                ]
                            ]
                    ]
                ]

        /// Contains the counters in a html table format
        let counters =
            let heldColumnText = (if model.HeldCounterValues.IsSome then "Held" else "")

            let firstColumnWidth, secondColumnWidth, thirdColumnWidth =
                match model.HeldCounterValues with
                | Some _ -> "60%", "20%", "20%"
                | None -> "72%", "0%", "28%"
            let counterRows =
                let combinedItems =
                    match model.HeldCounterValues with
                    | Some stats -> List.zip counterItems stats
                    | None -> List.map (fun item -> (item, "")) counterItems

                combinedItems
                |> List.mapi (fun i (entry, stat) ->
                    let isEven = i % 2 = 0
                    let backgroundColor = if isEven then "#eee" else "transparent"
                    let tooltip = if entry.ToolTipDescription = "" then (Id "no-tooltip") else Tooltip.dataTooltip (str entry.ToolTipDescription)
                    tr
                        []
                        [
                            td [Style [BackgroundColor backgroundColor; Width firstColumnWidth; Padding "3px 1px 3px 7px"; FontSize "13px"; LineHeight "24px";
                                            Margin 0; BorderTop "1px solid #dbdbdb";BorderBottom "1px solid #dbdbdb" ;FontWeight "600"];]
                                [ div [Style  [ Width "320px" ]; HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-top" ; tooltip] [str(entry.DisplayName)]]
                            td [Style [BackgroundColor backgroundColor; Width secondColumnWidth; Padding "3px "; Margin 0; BorderTop "1px solid #dbdbdb";BorderBottom "1px solid #dbdbdb";FontWeight "500";]] [str stat]
                            td [Style [BackgroundColor backgroundColor; Width thirdColumnWidth; Padding "3px "; Margin 0; BorderTop "1px solid #dbdbdb";BorderBottom "1px solid #dbdbdb";FontWeight "500";]] [str(entry.Value)]
                        ])

            div [] [
                table
                    [Style [ Width "100%"; TableLayout "fixed"; BorderCollapse "collapse";]]
                    [
                        tr []
                            [
                                th [Style [BackgroundColor "#485fc7"; Color "White";Width firstColumnWidth; Padding "3px 1px 3px 7px"; Margin 0; BorderTop "1px solid #dbdbdb";BorderBottom "1px solid #dbdbdb";FontWeight "600"]]
                                 [str "Helper/Counter"];
                                th [Style [BackgroundColor "#485fc7"; Color "White";Width secondColumnWidth; Padding "3px 3px"; Margin 0; BorderTop "1px solid #dbdbdb";BorderBottom "1px solid #dbdbdb";FontWeight "600"]]
                                 [str heldColumnText];
                                th [Style [BackgroundColor "#485fc7"; Color "White";Width thirdColumnWidth; Padding "3px 10px 3px 3px"; Margin 0; BorderTop "1px solid #dbdbdb";BorderBottom "1px solid #dbdbdb";FontWeight "600"]]
                                 [str "Current"];
                            ];
                        yield! counterRows
            ]]


        details
            [ Open(model.SheetStatsExpanded) ]
            [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSheetStats)) ] [ str "Sheet Stats " ]
              div
                  []
                  [
                    counters
                    div [HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-bottom";Tooltip.dataTooltip (str "Hold a copy of the existing sheet values in the \ntable ('Held' column) for comparison purposes.\nCurrent values column is always dynamic.")]
                     [holdUnholdButton]
                     ]  ]

    // ----------------- //
    //      Symbols      //
    // ----------------- //

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
                              td [] [ code [] [ str (getComponentTypeDescrFromSym symbol) ] ] ]
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


    // ---------------- //
    //       Wire       //
    // ---------------- //

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
                              td [] [ str (wire.InitialOrientation.ToString()) ] ]
                        tr
                            []
                            [ td [] [ strong [] [ str "Length: " ] ]
                              td [] [ str ((wire.Segments |>List.sumBy (fun seg -> abs(seg.Length))).ToString("F2")) ] ] ] ])


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


    // --------- Grouped Components -------- //

    let symbols = model.Sheet.Wire.Symbol.Symbols

    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let groupedComponentIds =
        model.Sheet.GroupMap
        |> Map.toList
        |> List.collect snd

    let ungroupedComponentsMap =
        model.Sheet.Wire.Symbol.Symbols
        |> Map.filter (fun k v -> v.Component.Label <> "" && not (List.exists ((=) k) groupedComponentIds))


    let groupProperties =

        let addToGroup (groupMap : Map<GroupId, ComponentId list>) (groupId: GroupId) (componentId: ComponentId) =
            match Map.tryFind groupId groupMap with
            | Some componentIds ->
                let newComponentIds = componentId :: componentIds
                Map.add groupId newComponentIds groupMap
            | None -> Map.add groupId [componentId] groupMap

        let deleteFromGroup (groupMap : Map<GroupId, ComponentId list>)(groupId: GroupId) (componentId: ComponentId) =
            match Map.tryFind groupId groupMap with
            | Some componentIds ->
                let newComponentIds = List.filter ((<>) componentId) componentIds
                Map.add groupId newComponentIds groupMap
            | None -> groupMap


        let deleteWholeGroup (groupMap : Map<GroupId, ComponentId list>) (groupId: GroupId) =
            match Map.tryFind groupId groupMap with
            | Some componentIds ->
                Map.remove groupId groupMap
            | None -> groupMap


        let createTableFromSymbols (symbolsMap: Map<ComponentId, SymbolT.Symbol>) (groupMap: Map<GroupId, ComponentId list>) =

            let symbols = Map.toList symbolsMap |> List.map snd
            let groupKeys = Map.toList groupMap |> List.map fst




            let dropdownItems(compId : ComponentId) =
                let groupItems =
                    groupKeys
                    |> List.mapi (fun index groupId ->
                        Dropdown.Item.a [] [p [OnClick (fun _ -> sheetDispatch (DrawModelType.SheetT.SetModelGroupMap(addToGroup groupMap groupId compId)))] [str ("Group " + (index + 1).ToString()) ]])
                let newGroupItem = [Dropdown.Item.a [] [p [OnClick (fun _ -> sheetDispatch (DrawModelType.SheetT.SetModelGroupMap(addToGroup groupMap (GroupId DateTime.Now) compId)))] [str "Create new group"]]]
                groupItems @ newGroupItem


            let addGroupDropDown (compId: ComponentId) =
                Dropdown.dropdown [ Dropdown.IsHoverable; Dropdown.IsRight ]
                    [ Dropdown.trigger [] [ Button.button [ Button.Size IsSmall ]
                        [ str "Add to..." ;
                        Icon.icon [ Icon.Size IsSmall ] [] ] ];
                    Dropdown.menu []
                        [ Dropdown.content []
                            (dropdownItems compId)]]


            let tableRows =
                symbols
                // sort alphabetically by label
                |> List.sortBy (fun symbol -> symbol.Component.Label.ToString())
                // remove any symbols with empty label (gets rid of annotations e.g. rotation or scale buttons for selected syms)
                |> List.filter (fun symbol -> symbol.Component.Label.ToString() <> "")
                |> List.map (fun symbol ->
                    let compTypeDescr = getComponentTypeDescrFromSym symbol
                    tr
                        []
                        [ td [] [ str (symbol.Component.Label.ToString())  ];
                        td
                            []
                            [ code [] [ str ( compTypeDescr )] ];
                        td [] [ addGroupDropDown (ComponentId symbol.Component.Id) ] ])

            if tableRows.Length = 0 then
                div [Style [MarginBottom "25px"; Border "1px solid lightgrey"]] [str "No ungrouped components."]
            else
                // div [Style [MarginBottom "25px"; MaxHeight "300px"; OverflowY OverflowOptions.Scroll; Border "1px solid lightgrey"]] [
                div [Style [MarginBottom "25px";]] [
                Table.table
                    [Table.IsFullWidth;]
                    [ tr
                        []
                        [ th [] [ str "Label" ];
                            th [] [ str "Type" ];
                            th [] [ str "Action"] ];
                        yield! tableRows ];

                ]





        let highlightGroup (groupId: GroupId) (groupMap: Map<GroupId, ComponentId list>) =
        //  returns a (compIds, connIds, colour)
            let compIds =
                match Map.tryFind groupId groupMap with
                | Some compIds -> compIds
                | None -> []
            let connIds = []

            (compIds, connIds, HighLightColor.SkyBlue)





        let createGroupRows (groupId: GroupId) (groupLabel: int) (componentIds: ComponentId list) (symbolsMap: Map<ComponentId, SymbolT.Symbol>) (groupMap: Map<GroupId, ComponentId list>)=
            let validComponentIds =
                componentIds
                |> List.filter (fun componentId -> Map.containsKey componentId symbolsMap)
                |> List.filter (fun componentId ->
                    match Map.tryFind componentId symbolsMap with
                    | Some symbol -> symbol.Component.Label <> ""
                    | None -> false)
            let validComponentsCount = validComponentIds.Length

            let groupRows =
                validComponentIds
                    |> List.mapi (fun index componentId ->
                            match Map.tryFind componentId symbolsMap with
                            | Some symbol ->
                                let compTypeDescr = getComponentTypeDescrFromSym symbol
                                let groupIdElement =
                                    match index with
                                    | 0 -> td [] [ str ((groupLabel+1).ToString()); Button.button [Button.Color IsDanger; Button.Size IsSmall; Button.OnClick (fun _ -> sheetDispatch (DrawModelType.SheetT.SetModelGroupMap(deleteWholeGroup groupMap groupId)))] [str "Delete"]]
                                    | _ -> td [] []
                                let borderBottomStyle =
                                    match (index + 1 = validComponentsCount) with
                                    | true -> "4px solid lightgrey"
                                    | false -> "0px"

                                tr [
                                    Style [BorderBottom borderBottomStyle];
                                ] [
                                    // groupIdElement;
                                    td [] [ str (symbol.Component.Label.ToString())  ];
                                    td [] [ code [] [ str ( compTypeDescr )] ];
                                    td [ Style [Padding "10px" ;TextAlign TextAlignOptions.Right; VerticalAlign "middle";]; OnClick (fun _ -> sheetDispatch (DrawModelType.SheetT.SetModelGroupMap(deleteFromGroup groupMap groupId componentId))) ] [Delete.delete [Delete.Size IsMedium] []]
                                ]
                            | None -> tr [] [])

            let groupHeaderRow =
                tr [Style [BorderTop "4px solid lightgrey"; BackgroundColor "WhiteSmoke"]] [
                                           td [Style [FontWeight "bold"]] [ str ("Group " + (groupLabel + 1).ToString())];
                                        //    td [Style [Padding "5px 10px";TextAlign TextAlignOptions.Left; ]] [ ];
                                           td [] [];
                                           td [Style [Padding "5px 10px";TextAlign TextAlignOptions.Right; ]] [

                                            div [Style [Display DisplayOptions.Block]] [

                                            Button.button [Button.Color IsInfo; Button.Size IsSmall; Button.OnClick (fun _ -> sheetDispatch (DrawModelType.SheetT.ColourSelection(highlightGroup groupId groupMap)))] [str "Highlight"];

                                            span [Style[Padding "0px 2px"]] []

                                            Button.button [Button.Color IsDanger; Button.Size IsSmall; Button.OnClick (fun _ -> sheetDispatch (DrawModelType.SheetT.SetModelGroupMap(deleteWholeGroup groupMap groupId)))] [str "Delete All"]]

                                            ]
                ]

            groupHeaderRow :: groupRows



        let createGroupTableFromGroupMap (symbolsMap: Map<ComponentId, SymbolT.Symbol>) (groupMap : Map<GroupId, ComponentId list>) =
            if groupMap |> Map.isEmpty then
                div [Style [MarginBottom "25px" ]] [str "No groups created."]
            else
                let tableRows =
                    groupMap
                    |> Map.toList
                    |> List.mapi (fun index (groupId, componentIds) -> (index, groupId, componentIds))
                    |> List.collect (fun (index, groupId, componentIds) ->
                        let sortedComponentIds =
                            componentIds
                            |> List.sortBy (fun componentId ->
                                match Map.tryFind componentId symbolsMap with
                                | Some symbol -> symbol.Component.Label.ToString()
                                | None -> "")
                        let groupRows = createGroupRows groupId index sortedComponentIds symbolsMap groupMap
                        groupRows)
            // div [Style [MarginBottom "25px"; (*MaxHeight "300px"; OverflowY OverflowOptions.Scroll ;*) Border "1px solid lightgrey"]]
                div [Style [MarginBottom "25px";]] [Table.table
                [Table.IsFullWidth;]
                [ tr
                    []
                    [
                        // th [] [ str "Group" ];
                        // th [] [ str "Label" ];
                        // th [] [ str "Type" ];
                        // th [] [ ];
                        // th [] []
                        ];
                    yield! tableRows ]

                    ]



        details
            [ Open(model.GroupMenuExpanded) ]
            [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleGroup)) ] [ str "Grouped Components "];

            div [Style [Margin "10px 0px"]] [
                Heading.h5 [] [str "Grouped Components"]
                createGroupTableFromGroupMap symbols model.Sheet.GroupMap
                Heading.h5 [] [str "Ungrouped Components"]
                p [Style [Margin "-10px 0 10px" ]] [str "Choose a component to add to the group."]
                createTableFromSymbols ungroupedComponentsMap model.Sheet.GroupMap
            ]
        ]


    div [ Style [ Margin "-10px 0 20px 0" ] ] ([ mouseSensitiveDataSection; sheetStatsMenu; viewComponentWrapper; groupProperties ])
