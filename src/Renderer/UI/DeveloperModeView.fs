module DeveloperModeView

open EEExtensions
open VerilogTypes
open Fulma
open Fulma.Extensions.Wikiki

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
open BusWire
open IntersectionHelpers


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


/// Top Level function for developer mode (tdc21)
let developerModeView (model: ModelType.Model) dispatch =
// --------------------------------------------------- //
//                       Counters                      //
//     Feel free to modify `counterItems` as needed!   //
// --------------------------------------------------- //

    /// Contains a record of a counter's display name, tooltip description, and value
    /// A counter is a React element for a function that takes in a SheetT.Model and outputs a string/int/float
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
          {|DisplayName="CountAlmostStraight Wires";
            ToolTipDescription = "Counts the number of wires that are almost straight.\nWires that have a maximum deviation of 40px \nfrom the majority direction."
            Value=(countAlmostStraightWiresOnSheet model.Sheet 40.0).ToString() |}
           ]

    // let testSegmentIntersectsBBox  (model: ModelType.Model) : string =
    //     // check for at least two symbols, take the first and second, run with reSizeSymbolTopLevel
    //     match model.Sheet.Wire.Symbol.Symbols.Count >= 1 && model.Sheet.Wire.Wires.Count >= 1 with
    //     | true ->
    //         let firstWire = model.Sheet.Wire.Wires |> Map.values |> Array.head

    //         model.Sheet.Wire.Symbol.Symbols
    //         |> Map.map (fun _ symbol ->
    //             let bbox = getSymbolBoundingBox symbol
    //             getAbsSegments firstWire
    //             |> List.skip 1
    //             |> fun list -> List.take (List.length list - 2 ) list
    //             |> List.map (fun (aSeg: ASegment) ->

    //                 let segStart, segEnd = aSeg.Start, aSeg.End
    //                 (aSeg.GetId, segmentIntersectsBoundingBox bbox segStart segEnd))
    //             // get rid of Nones and convert to strings including ID
    //             |> List.choose (fun ((id, _), optIntersection) ->
    //                 match optIntersection with
    //                 | Some intersection -> Some (sprintf "id %A with intersect distance %f" (id.ToString()) intersection)
    //                 | None -> None)
    //             |> String.concat ","

    //             )
    //         |> Map.toList
    //         |> List.map snd
    //         |> String.concat ",   "


    //     | false -> ""
    // let removeSingleWireInvisibleSegments (wire: Wire) =
    //     let uniqueVertices =
    //         segmentsToIssieVertices wire.Segments wire
    //         |> List.distinctBy (fun (x, y, _) -> (x, y))
    //     let newSegments = issieVerticesToSegments wire.WId uniqueVertices
    //     // for each wire, set the segments to the new segments
    //     wire |> Optic.set segments_ newSegments

    // let testSegmentIntersectsBBox2  (model: ModelType.Model) : string =
    //     // check for at least two symbols, take the first and second, run with reSizeSymbolTopLevel
    //     match model.Sheet.Wire.Symbol.Symbols.Count >= 1 && model.Sheet.Wire.Wires.Count >= 1 with
    //     | true ->
    //         let firstWire = model.Sheet.Wire.Wires |> Map.values |> Array.head
    //         model.Sheet.Wire.Symbol.Symbols
    //         |> Map.map (fun _ symbol ->
    //             let bbox = getSymbolBoundingBox symbol
    //             getAbsSegments firstWire
    //             |> List.skip 1
    //             |> fun list -> List.take (List.length list - 1 ) list
    //             |> List.map (fun (aSeg: ASegment) ->

    //                 let segStart, segEnd = aSeg.Start, aSeg.End
    //                 (aSeg.GetId, getSegmentIntersectBBox bbox segStart segEnd))
    //             // get rid of Nones and convert to strings including ID
    //             |> List.choose (fun (( id,_), optIntersection) ->
    //                 match optIntersection with
    //                 | Some intersection -> Some (sprintf "id %A with intersect area %A" (id.ToString()) (intersection.ToString()))
    //                 | None -> None)
    //             |> String.concat ", "

    //             )
    //         |> Map.toList
    //         |> List.map snd
    //         |> String.concat ", "


    //     | false -> ""

    // let testOverlap1DIn2DInfo  (model: ModelType.Model)=
    //     match model.Sheet.Wire.Symbol.Symbols.Count >= 2 && model.Sheet.Wire.Wires.Count >= 2 with
    //     | true ->
    //         let wires =
    //             model.Sheet.Wire.Wires
    //             |> Map.values
    //             |> Array.toList
    //             |> List.mapi (fun i wire -> (i, wire))

    //         List.allPairs wires wires
    //         |> List.filter (fun ((i1, wire1), (i2, wire2)) -> i1 > i2)
    //         |> List.map (fun ((i1, wire1), (i2, wire2)) ->
    //             let wire1Abs, wire2Abs = getAbsSegments wire1, getAbsSegments wire2
    //             List.allPairs wire1Abs wire2Abs
    //             |> List.filter (fun (seg1: ASegment, seg2) -> seg1.Segment.Index <> seg2.Segment.Index)
    //             |> List.map (fun (seg1: ASegment, seg2) ->  overlap1DIn2DInfo (seg1.Start, seg1.End) (seg2.Start, seg2.End))
    //             |> List.choose id
    //             |> List.map (fun rect ->
    //                 if rect.BottomRight = rect.TopLeft then
    //                     rect.BottomRight.ToString()
    //                 else
    //                     sprintf "Overlap with TopLeft %A and BottomRight %A" rect.TopLeft rect.BottomRight
    //             )
    //             |> String.concat ",")
    //         |> String.concat "; "
    //     | false -> ""

// let countIntersectingSymbolPairs (model: SheetT.Model) =
//     let boxes =
//         mapValues model.BoundingBoxes
//         |> Array.toList
//         |> List.mapi (fun n box -> n, box)
//     List.allPairs boxes boxes
//     |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
//     |> List.length
//     // divide by 2
//     |> (fun x -> x / 2)

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
        /// A counter is a React element for a function that takes in a SheetT.Model and outputs a string/int/float
        /// They output useful information about the sheet
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
            ];
                // div [Style [MarginBottom "20px"]] [
                //     code [] [str (testOverlap1DIn2DInfo model)]
                // ];

                // div [Style [MarginBottom "20px"]] [
                //     code [] [str (testSegmentIntersectsBBox model)]
                // ];

                // div [] [
                //     code [] [str (testSegmentIntersectsBBox2 model)]
                // ]
                // div [] [
                //     code [] [str (testSegmentIntersectsBBox2 model)]
                // ]
            ]


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
    div [ Style [ Margin "-10px 0 20px 0" ] ] ([ mouseSensitiveDataSection; sheetStatsMenu; viewComponentWrapper ])
