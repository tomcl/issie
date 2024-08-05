module DeveloperModeView

open EEExtensions
open VerilogTypes
open Fulma
open Fulma.Extensions.Wikiki

open Fable.React
open Fable.React.Props
open Fable.Core
open Fable.Core.JsInterop

open JSHelpers
open System

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
open UIContextualSideBar
open Groups.ColourGenerator
open DrawModelType.SheetT
open SymbolUpdate
open Groups
open Notifications
open PopupHelpers

// AUTHOR: TDC21

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

// Model Groups
*)

let buttonStyles =
    Style [ Margin "0px 3px"; Padding "6px 10px"; Cursor "Pointer"; Height "1.1em" ]

// +----------------------------------------------------------+ //
// |         Example of  Building a (Simple) Sidebar          | //
// +----------------------------------------------------------+ //
let simpleSidebar =
    // ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    // +-------- First, Define SidebarOptions. --------+
    let sidebarOptions: SidebarOptions =
        { ExtraStyles = fun _ -> []
          TitleText = fun _ -> "Test Sidebar" // this will be displayed on the sidebar
          // +-------- If true, a cancel button will be displayed. Put false if you want a mandatory action
          Cancellable = ContextualSidebarCancellable.Bool true
          SideBarButtons =
            [ {
                // +-------- Add classes for the button. "button" is already pre-pended for you. https://bulma.io/documentation/elements/button/
                ButtonClassNames = fun _ -> "is-info"
                // +-------- Add text for the button.
                ButtonText = fun _ ->  "Test Button 1: Pan Right"
                // +-------- Add props for the button.  https://fulma.github.io/Fulma/#fulma/elements/button
                ButtonProps = fun _ -> [];
                // +-------- Define the action for the button.
                // +-------- Onclick Type signature: ModelType.Model -> (Msg -> Unit) -> Browser.Types.MouseEvent -> Unit
                // +-------- You usually don't need to access the browser event so in this example, an underscore is used.
                // +-------- This button triggers moves the sheet to the right by 20 units.
                ButtonAction =
                  (fun model dispatch _ ->
                      printf "Test Button 1: Move Sheet\n"
                      dispatch (
                          UpdateModel(fun model ->
                              { model with
                                  Sheet =
                                      { model.Sheet with
                                          ScreenScrollPos = model.Sheet.ScreenScrollPos + { X = 20.; Y = 0. } } })
                      )) }
              { ButtonClassNames = fun _ -> "is-danger"
                ButtonText = fun _ -> "Test Button 2: Centre"
                ButtonProps = fun _ -> [];
                ButtonAction =
                  (fun model dispatch _ ->
                      let sheetDispatch sMsg = dispatch (Sheet sMsg)
                      printf "Test Button 2: Centre\n"
                      // +-------- This button triggers moves the sheet to centre, equal to pressing CtrlW.
                      (sheetDispatch (KeyPress CtrlW))) }

              ]

        }

    // ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    // +-------- Next, Define the sidebarBody. --------+
    // +--------  sidebarBody must be type (ModelType.Msg -> Unit) ->  ModelType.Model ->  ReactElement
    //                                           |
    //                                       dispatch

    let sidebarBody: ((ModelType.Msg -> Unit) -> ModelType.Model -> ReactElement) =
        // +-------- define the sidebarBody as a function that takes in dispatch and model, and returns a ReactElement
        fun dispatch model ->
            let numberOfComponents = model.Sheet.Wire.Symbol.Symbols |> Map.count

            // +-------- define the div to return
            div
                []
                [ p
                      []
                      [ str "You are currently viewing the sidebar! This example was created using the function"
                        codeInline "fsharp" "buildSimpleSidebar"
                        str ". A sidebar contains a body and buttons, which both have access to dispatch." ]

                  br []

                  p
                      []
                      [ str
                            $"This a dynamic body and can access the current model. For example, there are now {numberOfComponents} components on the sheet. Note that annotations (the resize/rotate buttons) are also counted." ]

                  br []

                  // +-------- I have defined a codeblock react element that can display fsharp code
                  codeBlock
                      "fsharp"
                      ("
type SidebarOptions = {
    ExtraStyle: CSSProp list;
    TitleText: string;
    SideBarButtons: SidebarButton list;
    Cancellable: bool;
}

type SidebarButton = {
    ButtonClassNames: string; // for colours with fulma
    ButtonText: string; // for the text on the button
    ButtonAction: ModelType.Model -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit
}
                    ")

                  br []
                  p
                      []
                      [ str "The sidebar buttons OnClick functions are of type"
                        codeInline "fsharp" "ModelType.Model -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit"
                        str ". In order to define a function for the button, modify "
                        codeInline "fsharp" "ButtonAction"
                        str "field, using a signature such as"
                        codeInline "fsharp" "fun model dispatch _ -> ..."
                        str
                            ". The underscore is a placeholder for the browser event, which is not commonly used. See the DeveloperModeView file for a guide and example." ]

                  br []

                  ]
    // ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    // +-------- Make the sidebar.
    buildSidebar sidebarOptions sidebarBody

// +---------------------------------------------------------+ //
// |        Example of Building a Sidebar with Dialog        | //
// +---------------------------------------------------------+ //

let sidebarWithDialog =
    let sidebarOptions: SidebarOptions =
        { ExtraStyles =  fun _ -> []
          TitleText = fun _ ->  "Test Sidebar with Dialog"
          Cancellable = ContextualSidebarCancellable.Bool true
          SideBarButtons =
            [ { ButtonClassNames = fun _ -> "is-danger is-small"
                ButtonText = fun _ -> "Test Button: Flip Vertically"
                ButtonProps = fun _ -> [];
                ButtonAction =
                  (fun model dispatch _ ->
                      let sheetDispatch sMsg = dispatch (Sheet sMsg)
                      printf "Test Button: Flip Vertically\n"
                      (sheetDispatch <| SheetT.Flip SymbolT.FlipVertical)) }
              { ButtonClassNames = fun _ -> "is-warning is-small"
                ButtonText = fun _ -> $"Rename first component to Text Dialog"
                ButtonProps = fun _ -> [];
                ButtonAction =
                  (fun model dispatch _ ->
                      printf
                          $"Test Button: Rename first component to Text Dialog, {model.ContextualSidebarDialogData.Text}\n"
                      let currentSymbols = model.Sheet.Wire.Symbol.Symbols
                      let newSymbols =
                          match model.ContextualSidebarDialogData.Text with
                          | None ->
                              printf "No text provided"
                              currentSymbols
                          | Some symbolLabel ->
                              // create new Symbols map with the first symbol label renamed

                              match Map.toList currentSymbols with
                              | (id, symbol) :: _ ->
                                  let newSymbol =
                                      { symbol with Component = { symbol.Component with Label = symbolLabel } }
                                  Map.add id newSymbol currentSymbols
                              | [] -> currentSymbols

                      dispatch
                      <| UpdateModel(fun (model: ModelType.Model) ->
                          model |> Optic.set (ModelType.symbols_) newSymbols)
                      dispatch CloseContextualSidebar) } ]

        }

    let sidebarBody: ((ModelType.Msg -> Unit) -> ModelType.Model -> ReactElement) =
        let beforeText: (ContextualSidebarDialogData -> ReactElement) =
            (fun (beforeData: ContextualSidebarDialogData) ->
                div
                    []
                    [ p
                          []
                          [ str
                                "This is the dialog body for text. Unlike the sidebar body, we have acess to the beforeData, the current ContextualSidebarDialogData." ]
                      p
                          []
                          [ str "Currently, "
                            codeInline "fsharp" "ContextualSidebarDialogData"
                            str "contains:" ]
                      br []
                      codeBlock "" (beforeData.ToString()) ])

        let beforeInt: (ContextualSidebarDialogData -> ReactElement) =
            (fun (_: ContextualSidebarDialogData) -> div [] [ p [] [ str "This is the dialog body for int." ] ])

        fun dispatch model ->
            div
                []
                [ textDialogSidebarElement "placeholder string" (Some "default text") beforeText dispatch model

                  br []
                  p
                      []
                      [ str
                            "As the dialog data structure suggests, there is support for two dialogs at once where one modifies text and the other modifies int. So this means either "
                        codeInline "fsharp" "textDialogSidebarElement"
                        str " and an "
                        codeInline "fsharp" "intDialogSidebarElement"
                        str " together, or a "
                        codeInline "fsharp" "textDialogSidebarElement"
                        str " and an "
                        codeInline "fsharp" "boundedIntDialogSidebarElement"
                        str " together." ]

                  intDialogSidebarElement (Some 42) beforeInt dispatch model

                  br []
                  p
                      []
                      [ str
                            "We still have access to other parts of the sidebar body, and buttons still show up. To test the flip vertically button, select a symbol and click the button." ]
                  br []
                  p
                      []
                      [ str "To save user input, define a button that takes the model's dialogData "
                        codeInline "fsharp" "model.ContextualSidebarDialogData : ContextualSidebarDialogData"
                        str
                            " and updates the model using the new data. The button can then also close the sidebar. You can see the example code in "
                        code [] [ str "DeveloperModeView.fs" ]
                        str
                            ". The yellow button renames the first component label on the sheet to the text in the dialog." ]

                  ]

    buildSidebar sidebarOptions sidebarBody

let sidebarWithBoundedIntDialog =
    let sidebarOptions: SidebarOptions =
        { ExtraStyles =fun _ ->  []
          TitleText = fun _ -> "Test Sidebar with Bounded Int Dialog"
          SideBarButtons = []
          Cancellable = ContextualSidebarCancellable.Bool true }

    let sidebarBody =
        let beforeInt: (ContextualSidebarDialogData -> ReactElement) =
            (fun (beforeData: ContextualSidebarDialogData) ->
                div
                    []
                    [ p
                          []
                          [ str
                                "This is the dialog body for int, but accepts only a bounded input. This example accepts a number between 0 to 100 inclusive." ]
                      p
                          []
                          [ str "Currently, "
                            codeInline "fsharp" "ContextualSidebarDialogData"
                            str "contains:" ]
                      br []
                      codeBlock "" (beforeData.ToString()) ]

            )

        fun dispatch model -> div [] [ boundedIntDialogSidebarElement 0 100 42 beforeInt dispatch model ]

    buildSidebar sidebarOptions sidebarBody

// +--------------------------------------------+ //
// |         Top Level of Developermode         | //
// +--------------------------------------------+ //
/// Top Level function for developer mode (tdc21)
let developerModeView (model: ModelType.Model) dispatch =
    // +--------------------------------------------------+ //
    // |                     Counters                     | //
    // |   Feel free to modify `counterItems` as needed!  | //
    // +--------------------------------------------------+ //

    /// Contains a record of a counter's display name, tooltip description, and value
    /// A counter is a React element for a function that takes in a SheetT.Model and outputs a string/int/float
    /// They output useful information about the sheet
    let counterItems =
        [ {| DisplayName = "T1 Sym-Sym Intersections"
             ToolTipDescription = "Counts the number of symbols intersecting other \nsymbols on the sheet."
             Value = (numOfIntersectedSymPairs model.Sheet).ToString() |}
          {| DisplayName = "T2 Seg-Sym Intersections"
             ToolTipDescription = "Counts the number of visible wire segments \nintersecting on the sheet"
             Value = (numOfIntersectSegSym model.Sheet).ToString() |}
          {| DisplayName = "T3 Vis-Wire Seg 90º Cross"
             ToolTipDescription = "Counts the number of visible wire segments that \nintersect at 90 degrees."
             Value = (numOfWireRightAngleCrossings model.Sheet).ToString() |}
          {| DisplayName = "T4 Sum of Vis-Wire Segs"
             ToolTipDescription =
              "Counts the total length of all visible \nwire segments on the sheet.\n\n Assumption: \nOverlapping segments share the same starting net, and may\ndiverge at some point but will not return to overlap."
             Value = (calcVisWireLength model.Sheet).ToString("F2") |}
          {| DisplayName = "T5 Count Visible R-Angles"
             ToolTipDescription = "Counts the number of visible right angles \nfound in the wire segments on the sheet."
             Value = (numOfVisRightAngles model.Sheet).ToString() |}
          {| DisplayName = "T6 RetracingSegments"
             ToolTipDescription =
              "Counts the number of retracing segments on sheet.\nZero-length segments with non-zero segments on \nboth sides that have lengths of opposite signs lead to a \nwire retracing itself"
             Value = (List.length (findRetracingSegments model.Sheet).RetraceSegsInSymbol).ToString() |} ]

    // +-----------------------------------------------------------------+ //
    // |       Mouse Sensitive Data- Updates based on Mouse Position     | //
    // +-----------------------------------------------------------------+ //

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

    let testHighlightSymbols (model: ModelType.Model) = // get 2 componentids
        let compIds =
            model.Sheet.Wire.Symbol.Symbols
            |> Map.keys
            |> Seq.toList
            |> List.take 2

        let (colour) = generateColourFromModel model.Sheet.Wire.Symbol

        let newSheetModel: SheetT.Model =
            model.Sheet
            |> Optic.set (SheetT.symbol_) (highlightSymbols model.Sheet.Wire.Symbol compIds colour)

        model
        |> Optic.set (ModelType.sheet_) newSheetModel

    /// The testDiv contains buttons that run tests
    let testDiv =

        div
            []
            [ Button.button
                  [ Button.Color IsPrimary
                    Button.Size IsSmall
                    Button.Props[OnClick(fun _ -> dispatch (ShowContextualSidebar(Some simpleSidebar)))
                                 Style [ Margin "5px 2.5px 0 2.5px " ]] ]
                  [ str "Test Simple Sidebar" ]

              Button.button
                  [ Button.Color IsPrimary
                    Button.Size IsSmall
                    Button.Props[OnClick(fun _ -> dispatch (ShowContextualSidebar(Some sidebarWithDialog)))
                                 Style [ Margin "5px 2.5px 0 2.5px " ]] ]
                  [ str "Test Sidebar with Dialog" ]

              Button.button
                  [ Button.Color IsDark
                    Button.Size IsSmall
                    Button.Props[OnClick(fun _ -> dispatch (ShowContextualSidebar(Some sidebarWithBoundedIntDialog)))
                                 Style [ Margin "5px 2.5px 0 2.5px " ]] ]
                  [ str "Test Sidebar with Bounded Int Dialog" ]

              Button.button
                  [ Button.Color IsDanger
                    Button.Size IsSmall
                    Button.Props[OnClick(fun _ -> dispatch (ShowContextualSidebar(Some sidebarToCreateNewGroup)))
                                 Style [ Margin "5px 2.5px 0 2.5px " ]] ]
                  [ str "Test Choose Group Sidebar" ]

            // sidebarToAddToExistingGroup
              Button.button
                  [ Button.Color IsDanger
                    Button.Size IsSmall
                    Button.Props[OnClick(fun _ ->
                            // check that at least one group exists in groupInfo, then show the sidebar
                            if model.Sheet.Wire.Symbol.GroupMap |> Map.isEmpty then
                                printfn "No groups exist"
                            else
                                let groupId = model.Sheet.Wire.Symbol.GroupMap |> Map.keys |> Array.head
                                dispatch (ShowContextualSidebar(Some (sidebarToAddToExistingGroup groupId))));
                                 Style [ Margin "5px 2.5px 0 2.5px " ]] ]
                  [ str "Test Add to Existing Group Sidebar (To 1st Group)" ]

              Button.button
                  [ Button.Color IsInfo
                    Button.Size IsSmall
                    Button.Props[OnClick(fun _ -> dispatch (UpdateModel(testHighlightSymbols)))
                                 Style [ Margin "5px 2.5px 0 2.5px " ]] ]
                  [ str "Test Highlight + Random Colour" ]


              ]

    // +--------------------------------------------+ //
    // |     Sheet Stats Menu (sheetStatsMenu)      | //
    // +--------------------------------------------+ //

    /// Contains the mouse position, hovered comp data, and the counters
    let sheetStatsMenu =
        /// Selecting the (hold/unhold) button shows/hides the current sheet counter stats to a column on sheetstats. Used for comparison purposes
        let holdUnholdButton =
            let cachedSheetStats =
                counterItems
                |> List.map (fun counterRecord -> counterRecord.Value)
            div
                [ Style [ Margin "5px 0 10px 0" ] ]
                [ Level.level
                      []
                      [ Level.item
                            [ Level.Item.HasTextCentered ]
                            [ div
                                  [ Style [ FontSize "14px"; Width "100%"; Border "1.1px solid #555" ] ]
                                  [ Menu.list
                                        []
                                        [ Menu.Item.li
                                              [ (Menu.Item.IsActive(model.Tracking))
                                                Menu.Item.OnClick(fun _ ->
                                                    let updatedCachedData =
                                                        match model.Tracking with
                                                        | true -> None
                                                        | false -> Some cachedSheetStats
                                                    dispatch (SelectTracking((not model.Tracking), updatedCachedData))) ]
                                              [ strong [] [ str "Hold/Unhold Values" ] ] ] ] ] ] ]

        /// Contains the counters in a html table format
        /// A counter is a React element for a function that takes in a SheetT.Model and outputs a string/int/float
        /// They output useful information about the sheet
        let counters =
            let heldColumnText =
                (if model.HeldCounterValues.IsSome then
                     "Held"
                 else
                     "")

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
                    let tooltip =
                        if entry.ToolTipDescription = "" then
                            (Id "no-tooltip")
                        else
                            Tooltip.dataTooltip (str entry.ToolTipDescription)
                    tr
                        []
                        [ td
                              [ Style
                                    [ BackgroundColor backgroundColor
                                      Width firstColumnWidth
                                      Padding "3px 1px 3px 7px"
                                      FontSize "13px"
                                      LineHeight "24px"
                                      Margin 0
                                      BorderTop "1px solid #dbdbdb"
                                      BorderBottom "1px solid #dbdbdb"
                                      FontWeight "600" ] ]
                              [ div
                                    [ Style [ Width "320px" ]
                                      HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-top"
                                      tooltip ]
                                    [ str (entry.DisplayName) ] ]
                          td
                              [ Style
                                    [ BackgroundColor backgroundColor
                                      Width secondColumnWidth
                                      Padding "3px "
                                      Margin 0
                                      BorderTop "1px solid #dbdbdb"
                                      BorderBottom "1px solid #dbdbdb"
                                      FontWeight "500" ] ]
                              [ str stat ]
                          td
                              [ Style
                                    [ BackgroundColor backgroundColor
                                      Width thirdColumnWidth
                                      Padding "3px "
                                      Margin 0
                                      BorderTop "1px solid #dbdbdb"
                                      BorderBottom "1px solid #dbdbdb"
                                      FontWeight "500" ] ]
                              [ str (entry.Value) ] ])

            div
                []
                [ table
                      [ Style [ Width "100%"; TableLayout "fixed"; BorderCollapse "collapse" ] ]
                      [ tr
                            []
                            [ th
                                  [ Style
                                        [ BackgroundColor "#485fc7"
                                          Color "White"
                                          Width firstColumnWidth
                                          Padding "3px 1px 3px 7px"
                                          Margin 0
                                          BorderTop "1px solid #dbdbdb"
                                          BorderBottom "1px solid #dbdbdb"
                                          FontWeight "600" ] ]
                                  [ str "Helper/Counter" ]
                              th
                                  [ Style
                                        [ BackgroundColor "#485fc7"
                                          Color "White"
                                          Width secondColumnWidth
                                          Padding "3px 3px"
                                          Margin 0
                                          BorderTop "1px solid #dbdbdb"
                                          BorderBottom "1px solid #dbdbdb"
                                          FontWeight "600" ] ]
                                  [ str heldColumnText ]
                              th
                                  [ Style
                                        [ BackgroundColor "#485fc7"
                                          Color "White"
                                          Width thirdColumnWidth
                                          Padding "3px 10px 3px 3px"
                                          Margin 0
                                          BorderTop "1px solid #dbdbdb"
                                          BorderBottom "1px solid #dbdbdb"
                                          FontWeight "600" ] ]
                                  [ str "Current" ] ]
                        yield! counterRows ] ]

        details
            [ Open(model.SheetStatsExpanded) ]
            [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleSheetStats)) ] [ str "Sheet Stats " ]
              div
                  []
                  [ counters
                    div
                        [ HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-bottom"
                          Tooltip.dataTooltip (
                              str
                                  "Hold a copy of the existing sheet values in the \ntable ('Held' column) for comparison purposes.\nCurrent values column is always dynamic."
                          ) ]
                        [ holdUnholdButton ] ] ]

    // +----------------------------+ //
    // |           Symbols          | //
    // +----------------------------+ //

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

    // +--------------------------+ //
    // |           Wires          | //
    // +--------------------------+ //

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
                              td
                                  []
                                  [ str (
                                        (wire.Segments
                                         |> List.sumBy (fun seg -> abs (seg.Length)))
                                            .ToString("F2")
                                    ) ] ] ] ])

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

    // +--------------------------+ //
    // |    Grouped Components    | //
    // +--------------------------+ //

    let symbols = model.Sheet.Wire.Symbol.Symbols

    // Unfortunately, due to symbol colouring only accessible in the group, we use symbolDispatch to set groups.
    // So any changes to the group require SymbolT.Msg that is wrapped up to become a BusWireT.Msg, SheetT.Msg, up to a ModelType.Msg, which goes into dispatch.
    let symbolDispatch symMsg =
        symMsg |> Symbol |> Wire |> Sheet |> dispatch
    let sheetDispatch sheetMsg = sheetMsg |> Sheet |> dispatch

    let groupedComponentIds =
        model.Sheet.Wire.Symbol.GroupMap
        |> Map.toList
        |> List.collect snd

    let ungroupedComponentsMap =
        model.Sheet.Wire.Symbol.Symbols
        |> Map.filter (fun k v ->
            v.Component.Label <> ""
            && not (List.exists ((=) k) groupedComponentIds))

    /// Create UI to add/delete groups, add symbols to groups, remove symbols from groups, select symbols
    let viewGroupProperties =

        /// Create UI table to add any ungrouped symbols to a group
        let createGroupAssignerForSymbols
            (symbolsMap: Map<ComponentId, SymbolT.Symbol>)
            (groupMap: Map<GroupId, ComponentId list>)
            =

            let symbols = Map.toList symbolsMap |> List.map snd
            let groupKeys = Map.toList groupMap |> List.map fst

            /// Dropdown items to add a list of symbols to a group
            let dropdownItemsForGroupAddition (compIds: ComponentId list) =
                /// dropdown options list to add to existing groups
                let groupItems =
                    groupKeys
                    |> List.mapi (fun index groupId ->
                        Dropdown.Item.a
                            [ Dropdown.Item.Option.Props[Style [ Width "inherit" ]
                                                         OnClick(fun _ ->
                                                             symbolDispatch (
                                                                 DrawModelType.SymbolT.SetGroupMap(
                                                                     addToGroup model.Sheet groupId compIds
                                                                 )
                                                             ))] ]
                            [ p [] [ str ("Group " + (index + 1).ToString()) ] ])
                /// the dropdown option to create a new group
                let newGroupItem =
                    [ Dropdown.Item.a
                          [ Dropdown.Item.Option.Props[Style [ Width "inherit" ]
                                                       OnClick(fun _ ->
                                                           symbolDispatch (
                                                               DrawModelType.SymbolT.SetGroupMapAndInfo(
                                                                   createNewGroup model.Sheet.Wire.Symbol compIds
                                                               )
                                                           ))] ]

                          [ div [] [ str "Create new group" ] ] ]

                groupItems @ newGroupItem

            /// Dropdown to add a list of symbols to a group
            let addGroupDropDown (compIds: ComponentId list) =
                Dropdown.dropdown
                    [ Dropdown.IsHoverable; Dropdown.IsRight ]
                    [ Dropdown.trigger [] [ button [ buttonStyles; ClassName "button is-small  " ] [ str "Add to..." ] ]
                      Dropdown.menu [] [ Dropdown.content [] (dropdownItemsForGroupAddition compIds) ] ]

            /// filter any symbols with empty label (gets rid of annotations e.g. rotation or scale buttons for selected syms). We also exclude MergeWires, can be expanded if needed
            let filteredSymbols =
                symbols
                |> List.filter (fun symbol -> symbol.Component.Label.ToString() <> "")
                |> List.filter (fun symbol ->
                    match symbol.Component.Type with
                    | MergeWires
                    | SplitWire _
                    | BusSelection _
                    | NbitSpreader _ -> false
                    | _ -> true)

            /// Make a table row for each unselected symbol that is not in a group
            let unselectedSymbolsRows =
                filteredSymbols
                // sort alphabetically by label
                |> List.sortBy (fun symbol -> symbol.Component.Label.ToString())
                //
                |> List.filter (fun symbol ->
                    not (List.exists ((=) (ComponentId symbol.Component.Id)) model.Sheet.SelectedComponents))
                |> List.map (fun symbol ->
                    let compTypeDescr = getComponentTypeDescrFromSym symbol
                    tr
                        []
                        [ td [] [ str (symbol.Component.Label.ToString()) ]
                          td [] [ code [] [ str (compTypeDescr) ] ]
                          td [] [ addGroupDropDown ([ ComponentId symbol.Component.Id ]) ] ])

            /// Get the selected symbols of a sheet and sort them by label
            let selectedSymbols =
                filteredSymbols
                |> List.sortBy (fun symbol -> symbol.Component.Label.ToString())
                |> List.filter (fun symbol ->
                    (List.exists ((=) (ComponentId symbol.Component.Id)) model.Sheet.SelectedComponents))

            /// Get the ids of the selected symbols
            let selectedSymbolsIds =
                selectedSymbols
                |> List.map (fun symbol -> ComponentId symbol.Component.Id)

            /// Make a table row for each selected symbol that is not in a group
            let selectedSymbolRows =
                selectedSymbols
                |> List.map (fun symbol ->
                    let compTypeDescr = getComponentTypeDescrFromSym symbol
                    tr
                        [ Style [ BackgroundColor "#efe" ] ]
                        [ td [] [ str (symbol.Component.Label.ToString()) ]
                          td [] [ code [] [ str (compTypeDescr) ] ] ])
            /// Create a table for unselected symbols/components
            let unselectedSymbolsTable =
                if unselectedSymbolsRows.Length = 0 then
                    div [ Style [ MarginBottom "25px" ] ] []
                else
                    div
                        [ Style [MarginBottom "25px"] ]
                        [ Table.table
                              [ Table.IsFullWidth; Table.TableOption.Props[Style [ MarginBottom "10px" ]] ]
                              [ tr [] [ th [] [ str "Label" ]; th [] [ str "Type" ]; th [] [ str "Action" ] ]
                                yield! unselectedSymbolsRows ] ]

            /// mass-add selected symbols/components to a group
            let selectedSymbolsTable =
                let bottomSpace = (25 + groupMap.Count * 40).ToString() + "px" // bottom space for the group table so the dropdown has space to expand
                if selectedSymbolRows.Length = 0 then
                    div [ Style [MarginBottom (bottomSpace) ] ] []
                else
                    div
                        [ Style [ BackgroundColor "#efe"; Padding "10px";  MarginBottom (bottomSpace) ] ]
                        [

                          p [] [ str "Selected Components" ]
                          Table.table
                              [ Table.IsFullWidth; Table.TableOption.Props[Style [ MarginBottom "10px" ]] ]
                              [ tr [ Style [ BackgroundColor "#efe" ] ] [ th [] [ str "Label" ]; th [] [ str "Type" ] ]
                                yield! selectedSymbolRows ]

                          div
                              [ Style [ MarginLeft "auto" ] ]
                              [ Dropdown.dropdown
                                    [ Dropdown.IsHoverable ]
                                    [ Dropdown.trigger
                                          []
                                          [ button [ buttonStyles; ClassName "button is-small  " ] [ str "Add to..." ] ]
                                      Dropdown.menu
                                          []
                                          [ Dropdown.content [] (dropdownItemsForGroupAddition selectedSymbolsIds) ] ]

                                ] ]
            div [] [ unselectedSymbolsTable; selectedSymbolsTable ]

        /// Selects a group on the sheet
        let selectGroupOnSheet (groupId: GroupId) (model) =
            let groupMap = model.Sheet.Wire.Symbol.GroupMap
            let compIds =
                match Map.tryFind groupId groupMap with
                | Some compIds -> compIds
                | None -> []
            let connIds = []

            (compIds, connIds, HighLightColor.Orange)

        /// Create a row for each component in a group
        let createGroupRows
            (groupId: GroupId)
            (groupLabel: int)
            (componentIds: ComponentId list)
            (symbolsMap: Map<ComponentId, SymbolT.Symbol>)
            (groupInfoMap: Map<GroupId, GroupInfo>)
            =
            let groupColour =
                Map.find groupId groupInfoMap
                |> fun groupInfo -> groupInfo.Colour
            let validComponentIds =
                componentIds
                |> List.filter (fun componentId -> Map.containsKey componentId symbolsMap)
                |> List.filter (fun componentId ->
                    match Map.tryFind componentId symbolsMap with
                    | Some symbol -> symbol.Component.Label <> ""
                    | None -> false)
            let validComponentsCount = validComponentIds.Length

            /// Create a row for each component in a group
            let groupRows =
                validComponentIds
                |> List.mapi (fun index componentId ->
                    match Map.tryFind componentId symbolsMap with
                    | Some symbol ->
                        let compTypeDescr = getComponentTypeDescrFromSym symbol

                        let borderBottomStyle =
                            match (index + 1 = validComponentsCount) with
                            | true -> "4px solid lightgrey"
                            | false -> "0px"

                        tr
                            [ Style [ BorderBottom borderBottomStyle ] ]
                            [
                              // groupIdElement;
                              td [] [ str (symbol.Component.Label.ToString()) ]
                              td [] [ code [] [ str (compTypeDescr) ] ]
                              td
                                  [ Style [ Padding "10px"; TextAlign TextAlignOptions.Right; VerticalAlign "middle" ]
                                    OnClick(fun _ ->
                                        symbolDispatch (
                                            DrawModelType.SymbolT.SetGroupMapAndInfo(
                                                deleteComponentFromGroup model.Sheet.Wire.Symbol groupId componentId
                                            )
                                        )) ]
                                  [ Delete.delete [ Delete.Size IsMedium ] [] ] ]
                    | None -> tr [] [])

            let groupHeaderRow =
                tr
                    [ Style [ BorderTop "4px solid lightgrey"; BackgroundColor groupColour ] ]
                    [ td
                          [ Style [ FontWeight "bold";  ] ]
                          [ span [Style [(*BackgroundColor groupColour; Padding "5px"; BorderRadius "4px"; Border "1px solid grey"; *) VerticalAlign "middle"]] [str ("Group " + (groupLabel + 1).ToString()) ]]
                      //    td [Style [Padding "5px 10px";TextAlign TextAlignOptions.Left; ]] [ ];
                      td
                          [ Style [ TextAlign TextAlignOptions.Right; PaddingLeft 0; PaddingRight 0 ] ]
                          [ button
                                [ buttonStyles
                                  ClassName "button is-info is-small"
                                  OnClick(fun _ ->
                                      sheetDispatch (
                                          DrawModelType.SheetT.ColourSelection((selectGroupOnSheet groupId model))
                                      )) ]
                                [ str "Select" ] ]
                      td
                          [ Style [ TextAlign TextAlignOptions.Left; PaddingLeft 0; PaddingRight 0 ] ]
                          [

                            button
                                [ buttonStyles
                                  ClassName "is-danger button is-small"
                                  OnClick(fun _ ->
                                      symbolDispatch (
                                          DrawModelType.SymbolT.SetGroupMapAndInfo(
                                              deleteWholeGroup model.Sheet.Wire.Symbol groupId
                                          )
                                      )) ]
                                [ str "Delete All" ] ]

                      ]

            groupHeaderRow :: groupRows

        /// Create a table of all groups and their components
        let createGroupTableFromGroupMap (symbolsMap: Map<ComponentId, SymbolT.Symbol>) (sheetModel: SheetT.Model) =
            let groupMap = sheetModel.Wire.Symbol.GroupMap
            let groupInfoMap = sheetModel.Wire.Symbol.GroupInfoMap
            if groupMap |> Map.isEmpty then
                div [ Style [ MarginBottom "25px" ] ] [ str "No groups created." ]
            else
                let tableRows =
                    groupMap
                    |> Map.toList
                    // append respective groupInfo
                    |> List.map (fun (groupId, componentIds) -> (groupId, componentIds, Map.find groupId groupInfoMap))
                    // sort by creation date
                    |> List.sortBy (fun (_, _, groupInfo) -> groupInfo.CreationDate)
                    // create rows for each group
                    |> List.mapi (fun index (groupId, componentIds, _) -> (index, groupId, componentIds))
                    |> List.collect (fun (index, groupId, componentIds) ->
                        let sortedComponentIds =
                            componentIds
                            |> List.sortBy (fun componentId ->
                                match Map.tryFind componentId symbolsMap with
                                | Some symbol -> symbol.Component.Label.ToString()
                                | None -> "")
                        let groupRows =
                            createGroupRows
                                groupId
                                index
                                sortedComponentIds
                                symbolsMap
                                sheetModel.Wire.Symbol.GroupInfoMap
                        groupRows)

                div
                    [ Style [ MarginBottom "25px" ] ]
                    [ Table.table
                          [ Table.IsFullWidth ]
                          [ tr
                                []
                                [

                                ]
                            yield! tableRows ]

                      ]

        details
            [ Open(model.GroupMenuExpanded) ]
            [ summary [ menuLabelStyle; OnClick(fun _ -> dispatch (ToggleGroup)) ] [ str "Grouped Components " ]

              div
                  [ Style [ Margin "10px 0px 90px 0px"; Height "auto" ] ]
                  [ Heading.h5 [] [ str "Grouped Components" ]
                    createGroupTableFromGroupMap symbols model.Sheet
                    Heading.h5 [] [ str "Ungrouped Components" ]
                    p [ Style [ Margin "-10px 0 10px" ] ] [ str "Choose a component to add to the group." ]
                    createGroupAssignerForSymbols ungroupedComponentsMap model.Sheet.Wire.Symbol.GroupMap ] ]

    div
        [ Style [ Margin "-10px 0 20px 0" ] ]
        ([ mouseSensitiveDataSection
           testDiv
           sheetStatsMenu
           viewComponentWrapper
           viewGroupProperties ])
