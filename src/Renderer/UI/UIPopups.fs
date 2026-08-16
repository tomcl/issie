module UIPopups

open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open CommonTypes
open PopupHelpers
open ModelType
open ModelHelpers
open System
open Optics
open Optics.Operators

//-------------------------------------------------------------------------------------------------------------------//
//----------------------------------------------UI Popup Implementations---------------------------------------------//
//-------------------------------------------------------------------------------------------------------------------//


/// A popup displaying a progress bar
let progressPopup (legend: Model -> PopupProgress -> ReactElement) (model: Model) (dispatch: Msg->Unit) =
    let extraStyle = []
    let pp = Option.get model.PopupDialogData.Progress
    let body _ _ =  
        div [] [
            legend model pp
            Fulma.Progress.progress [Progress.Value pp.Value; Progress.Max pp.Max; Progress.Color Color.IsPrimary ] []
        ]
    let foot _ _ = div [] []
    let close dispatch _ = 
        dispatch <| SetPopupProgress None
    buildPopup pp.Title body foot close extraStyle dispatch model
    
/// A legend with speed info for the progress bar popup
let simulationLegend (model:Model) (pp: PopupProgress) =
    match model.CurrentStepSimulationStep with
    | Some (Ok simData) ->
        let speed = pp.Speed
        str <| $"simulation speed: %6.0f{speed} component-clocks / ms"
    | _ -> div [] []

/// Popup to implement spinner for long operations
let viewSpinnerPopup (spinPayload:SpinPayload) (model: Model) (dispatch: (Msg -> Unit)) =
    let body (dispatch: Msg->Unit) (model: Model) =
        Progress.progress
            [   Progress.Color IsSuccess
                Progress.Value (spinPayload.Total - spinPayload.ToDo)
                Progress.Max (spinPayload.Total)
            ]
            [ str $"{spinPayload.Total - spinPayload.ToDo}"]

    let foot (dispatch:Msg->Unit) (model: Model) =
        Level.level [ Level.Level.Props [ Style [ Width "100%"] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> 
                            dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
            ]
        ]
        
    buildPopup spinPayload.Name body foot (fun dispatch _ -> dispatch ClosePopup) [] dispatch model



/// helper to make heading text
let makeH h =
    Text.span [ Modifiers [
        Modifier.TextSize (Screen.Desktop, TextSize.Is6)
        Modifier.TextWeight TextWeight.Bold
    ] ] [str h; br []]
let styledSpan styles txt = span [Style styles] [str <| txt]
let bSpan txt = styledSpan [FontWeight "bold"] txt
let iSpan txt = styledSpan [FontStyle "italic"] txt
let tSpan txt = span [] [str txt]

/// top-level tabbed info popup
let makeInfoPopupButton (title: string) (info: ReactElement) dispatch =

    let foot _ = div [] []
    let popup dispatch = dynamicClosablePopup title (fun _ -> info) foot [Width 1000] dispatch
    // button driving a popup with a page of info
    Button.button
        [
            Button.OnClick (fun _ -> popup dispatch)
            Button.Size IsSmall
            Button.IsRounded
            Button.Color IColor.IsInfo
            Button.Props [Style [
                Height "32px"
                FontSize "24px"; 
                MarginLeft "10px"; 
                MarginRight "10px"; 
                MarginTop "3px";
                MarginBottom "0px"
                Padding "5px"; 
                PaddingTop "5px"; 
                PaddingBottom "8px"]]
        ]
        [str Constants.infoSignUnicode]


//-------------------------------------------------------------------------------------------------------------------//
//--------------------------------------------------INFO POPUP-------------------------------------------------------//
//-------------------------------------------------------------------------------------------------------------------//


/// The Issie documentation website. Depth lives there, not in this popup: a page can be as long as
/// it needs to be, is kept in step with the code by the same pull request, and is readable before
/// Issie is downloaded at all. What belongs here is the short path that gets somebody started
/// without leaving the application.
let private docsSite = "https://tomcl.github.io/issie"

/// What a link in a help message does when clicked.
///
/// Everything opens in the user's own browser: in Electron an ordinary link would navigate the
/// application window away from Issie, and there is no way back. A message writes a documentation
/// page as a bare file name, which is expanded here, so the messages carry no host name to go
/// stale if the site moves.
let helpLink (url: string) =
    let full = if url.StartsWith "http" then url else $"{docsSite}/{url}"
    openInBrowser full ()

/// A help message, rendered. The one entry point from AppMessages to the screen.
let helpText (markdown: string) = Markdown.render helpLink markdown

let viewInfoPopupAtTab (startTab: int) dispatch =

    let title = "ISSIE: Interactive Schematic Simulator and Integrated Editor"

    /// Version, acknowledgments and the technology Issie is built on. The version is a fact
    /// about the build rather than text, so it is handed to the message.
    let about = helpText (AppMessages.Info.about Version.VersionString)

    let gettingStarted =
        div [] [
            helpText AppMessages.Info.gettingStarted
            // a button rather than a link in the prose: it leaves Issie for the source code,
            // which is a different kind of thing from the documentation links above it
            button
                [OnClick <| openInBrowser "https://github.com/tomcl/ISSIE"]
                [ str "Issie on GitHub"]
            br [] ; br [] ]

    let tips = helpText AppMessages.Info.tips

    let bugReport = helpText AppMessages.Info.bugReport

    let keys =
        let keyTable: ReactElement =
            let makeKeyStrSpan (keyList: List<String>) (keyPaddingChar: String): ReactElement = 
                match keyList with
                | [] ->
                    span [] [str "(none)"]
                | _ ->
                    keyList 
                    |> List.mapFold (fun i e -> if i <> keyList.Length-1 then [e; keyPaddingChar], i+1 else [e], i+1) 0
                    |> fst |> List.concat |> List.fold (fun s e -> s+e) "" |> fun s -> span [] [str s]

            let makeKeyTableRow (action: String) (windowsKeyList: List<String>) (macosKeyList: List<String>)
                : ReactElement =
                tr [] [
                    th [Scope "Row"] [str action]
                    td [] [makeKeyStrSpan windowsKeyList " + "]
                    td [] [makeKeyStrSpan macosKeyList "-"]
                ]

            /// Generated from the shortcut table rather than written out here, so it cannot drift
            /// from what actually fires - which the hand-maintained list it replaces had already
            /// done: it claimed a developer-tools shortcut that release builds do not have, and
            /// omitted Ctrl+wheel zoom, the waveform cursor keys and Shift+drag scrolling
            /// entirely. Rows are grouped, with a heading row between groups.
            let keyRows: ReactElement list =
                let categoryName c =
                    match c with
                    | KeyTypes.CatFile -> "Project and sheets"
                    | KeyTypes.CatEdit -> "Editing the schematic"
                    | KeyTypes.CatView -> "View"
                    | KeyTypes.CatWaveSim -> "Waveform simulator"
                    | KeyTypes.CatTextEntry -> "Typing in a box"
                    | KeyTypes.CatGesture -> "Mouse and gestures"
                    | KeyTypes.CatDev -> "Development"

                let order c =
                    match c with
                    | KeyTypes.CatFile -> 0
                    | KeyTypes.CatEdit -> 1
                    | KeyTypes.CatView -> 2
                    | KeyTypes.CatWaveSim -> 3
                    | KeyTypes.CatTextEntry -> 4
                    | KeyTypes.CatGesture -> 5
                    | KeyTypes.CatDev -> 6

                /// only the first chord is shown: later ones are alternatives, not extra keys
                let partsFor isMac (s: KeyTypes.ShortcutSpec) =
                    match s.Trigger with
                    | KeyTypes.Gesture(win, mac) -> [ if isMac then mac else win ]
                    | KeyTypes.Chords _ ->
                        KeyTypes.chordsFor isMac s
                        |> List.tryHead
                        |> Option.map (KeyTypes.chordParts isMac)
                        |> Option.defaultValue []

                KeyTypes.shortcuts
                |> List.filter (fun s ->
                    s.Doc <> "" && not (s.DevOnly && JSHelpers.debugLevel = 0))
                |> List.groupBy (fun s -> s.Category)
                |> List.sortBy (fst >> order)
                |> List.collect (fun (cat, specs) ->
                    let heading =
                        tr [] [ th [Scope "Row"; ColSpan 3; Style [PaddingTop "1em"]]
                                   [str (categoryName cat)] ]
                    heading
                    :: (specs |> List.map (fun s ->
                            makeKeyTableRow s.Doc (partsFor false s) (partsFor true s))))


            let head =
                ["Action"; "Windows/Linux"; "macOS"]
                |> List.map (fun s -> th [Scope "Col"] [str s])
                |> (fun l -> [tr [] l])
            Table.table [] [
                thead [] head
                tbody [] keyRows
            ]


        let otherInputTable: ReactElement =
            let makeOtherInputTableRow (action: String) (mouse: String) (touchpad: String) (touchscreen: String) =
                tr [] [
                    th [Scope "Row"] [str action]
                    td [] [str mouse]
                    td [] [str touchpad]
                    td [] [str touchscreen]
                ]
            
            let otherInputData: List<String*String*String*String> =
                [
                    "Scroll", "Shift + Left click on canvas + Drag", "Two-finger scrolling", "One-finger drag";
                ]
            
            let head =
                ["Action"; "Mouse"; "Touchpad"; "Touchscreen"]
                |> List.map (fun s -> th [Scope "Col"] [str s])
                |> (fun l -> [tr [] l])
            let body = otherInputData |> List.map (fun (a, m, tp, ts) -> makeOtherInputTableRow a m tp ts)

            Table.table [] [
                thead [] head
                tbody [] body
            ]
    
        div [] [
            makeH "Mouse and Other Input Gestures:"
            otherInputTable
            makeH "Keyboard Shortcuts:"
            div
                [Style [FontStyle "Italic"]]
                [str "Note: the same actions are on the Edit and View menus and on right-click \
                      context menus."]
            div
                [Style [FontStyle "Italic"]]
                [str "Note: a shortcut acts on whatever has the keyboard. In a text box the \
                      editing keys edit the text; press Return to hand the keyboard back to the \
                      schematic."]
            keyTable
        ]


    let body model =
        let dialogData = model.PopupDialogData
        let tab = dialogData.Int

        // Getting Started first, and so the default: this window is most valuable to somebody who
        // has not used Issie before, and what they need is what to do next. The version number and
        // the acknowledgments are worth having but nobody opened this window for them.
        let tabNames =
            [ "Getting Started"; "Tips & Features"; "Keyboard Shortcuts"; "About Issie"; "Bug Reports" ]

        div [] [
            Tabs.tabs
                [ Tabs.IsFullWidth
                  Tabs.IsBoxed ]
                (tabNames
                 |> List.mapi (fun i name ->
                     Tabs.tab [ Tabs.Tab.IsActive (tab = Some i) ]
                         [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some i)) ] [ str name ] ]))

            match tab with
            | Some 0 -> gettingStarted
            | Some 1 -> tips
            | Some 2 -> keys
            | Some 3 -> about
            | Some 4 -> bugReport
            | _ -> dispatch <| SetPopupDialogInt (Some 0)
        ]

    let foot _ = div [] []
    // the tab is remembered between openings, so a caller that wants a particular one must say so
    dispatch <| SetPopupDialogInt (Some startTab)
    dynamicClosablePopup title body foot [Width 900] dispatch

/// The Info window, on the tab it is most useful to open on.
let viewInfoPopup dispatch = viewInfoPopupAtTab 0 dispatch

let viewWaveInfoPopup dispatch feature =
    let title = feature

    // The names matched here are what the user clicked on. Three of them come from the
    // "WaveSimHelp" right-click menu in ContextMenus.fs and must stay spelled the same as the
    // items there - see the note beside that list. "Getting Started" and "Instructions" come from
    // the viewer's own Info button in WaveSimTop. A name nothing answers to can only mean those
    // lists have drifted apart, which is what the last case is for.
    let waveInfo =
        match feature with
        | "Getting Started" -> helpText AppMessages.WaveHelp.gettingStarted
        | "Viewing Waveforms" -> helpText AppMessages.WaveHelp.viewingWaveforms
        | "Waveform and RAM selection" -> helpText AppMessages.WaveHelp.selection
        | "Instructions" -> helpText AppMessages.WaveHelp.instructions
        | "Miscellaneous" -> helpText AppMessages.WaveHelp.miscellaneous
        | unknown -> helpText (AppMessages.WaveHelp.noHelpFor unknown)

    let body (model: Model) =
        waveInfo
    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 1000] dispatch

/// Waveform Selection confirmation popup
let viewWaveSelectConfirmationPopup numRequired numWaves action dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br []]
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]
    
    let title = "Warning"
    
    let warning = 
        div [] [
            str $"You have selected {numWaves} waveforms. "; br []
            str $"Consider reducing this number to less than {numRequired}."; br []
            str "Too many waveforms selected in the viewer may impact viewer reponsiveness."; br []
            str "Best practice is to select only the waveforms you need to view, deleting older waveforms."
            br []; br []
            bSpan "HINT. "; str "You can view your selected waveforms using the top line controls by removing text in all filter boxes "
            str "and checking the "; bSpan "Show Only Selected "
            str "checkbox."; br []
            str "The wave selection check boxes then allow you to deselect unwanted waveforms."
        ]  
       
    let body (dialogData:PopupDialogData) =
        warning
    let foot _ = div [] []
    choicePopup title warning "Select waveforms" "Change selection"  action dispatch

/// Waveform selection refusal: more waveforms than the viewer can show at all.
///
/// Unlike the warning above this offers no way through - the one button returns to the selection -
/// so it is a closablePopup with a foot of its own rather than a choicePopup. confirmationPopup is
/// the near miss: its foot adds a Cancel beside the action button, and two buttons that both mean
/// "go back" say less than one.
let viewWaveSelectRefusalPopup numAllowed numWaves dispatch =
    let bSpan txt = span [Style [FontWeight "bold"]] [str txt]
    let body =
        div [] [
            str $"You have selected {numWaves} waveforms, which is more than the {numAllowed} the "
            str "viewer can show. Deselect some before leaving this dialog."
            br []; br []
            bSpan "HINT. "
            str "Clear the filter boxes above and tick "; bSpan "Show Only Selected"
            str " to list what you have chosen, wherever in the design it came from. The check "
            str "boxes then deselect it."
        ]
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button
                        [ Button.Color IsPrimary; Button.OnClick (fun _ -> dispatch ClosePopup) ]
                        [ str "Back to selection" ]
                ]
            ]
        ]
    closablePopup "Too many waveforms" body foot [] dispatch

//-------------------------------------------------------------------------------------------------------------------//
//---------------------------------------------- Paste as an array --------------------------------------------------//
//-------------------------------------------------------------------------------------------------------------------//

module PasteArray =
    open DrawModelType

    /// Everything the Paste array dialog needs before anything is typed into it: what was copied,
    /// how much of it fits, and the labels involved on both sides of the paste. Fixed for as long
    /// as the dialog is open, since the dialog changes nothing on the sheet.
    type Choices = {
        /// the bounding box of the copied fragment, which is what an array is built out of
        Box: BoundingBox
        MaxVertical: int
        MaxHorizontal: int
        /// labels of the copied symbols that will take a suffix; the unlabelled ones are not here
        CopiedLabels: string list
        /// every label already on the sheet, which a suffixed one may not collide with
        LabelsOnSheet: Set<string>
    }

    /// Which way round the dialog is currently set to array, kept in the dialog's spare text field.
    /// A direction rather than a bool so that what is stored says what it means when read back.
    let private directionOf (dialogData: PopupDialogData) =
        match dialogData.Text with
        | Some "horizontal" -> SheetT.ArrayHorizontal
        | _ -> SheetT.ArrayVertical

    let private nameOf =
        function
        | SheetT.ArrayVertical -> "vertical"
        | SheetT.ArrayHorizontal -> "horizontal"

    let private setDirection dir dispatch =
        dispatch <| SetPopupDialogText(Some(nameOf dir))

    /// Everything the dialog needs to know before anything is typed into it: the copied fragment's
    /// box, how many copies of it fit each way on this sheet, and the labels involved on both
    /// sides of the paste. None when nothing has been copied, which is what greys the menu item.
    let arrayChoices (model: Model) : Choices option =
        Sheet.copiedFragmentBox model.Sheet
        |> Option.map (fun box ->
            let fits dir = Sheet.maxArrayCopies dir model.Sheet.CanvasSize box
            { Box = box
              MaxVertical = fits SheetT.ArrayVertical
              MaxHorizontal = fits SheetT.ArrayHorizontal
              // Merge and split components have no label and are left without one, so they are
              // not here: there is nothing to put a suffix on, and no name to collide.
              CopiedLabels =
                BlockHelpers.copiedSymbolsInPasteOrder model.Sheet.Wire.Symbol
                |> List.map (fun s -> s.Component.Label)
                |> List.filter (fun l -> l <> "")
              LabelsOnSheet =
                model.Sheet.Wire.Symbol.Symbols
                |> Map.valuesL
                |> List.map (fun s -> s.Component.Label)
                |> Set.ofList })

    /// Nothing that fits: the copied circuit is over half the sheet in both directions, so no
    /// array of it - in any direction, at any count - has anywhere to go.
    let private tooBigPopup dispatch =
        let body =
            div [] [
                str "The copied circuit is too large for two of it to fit on one sheet, either "
                str "above one another or side by side, so there is no array to paste."
                br []; br []
                str "Copy a smaller part of the circuit, or paste one copy at a time."
            ]
        let foot =
            Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button
                            [ Button.Color IsPrimary; Button.OnClick(fun _ -> dispatch ClosePopup) ]
                            [ str "Close" ]
                    ]
                ]
            ]
        closablePopup "Nothing to array" body foot [] dispatch

    /// Do the paste, and get out of the way so that the array can be placed.
    let private doPaste dir copies firstSuffix dispatch =
        dispatch ClosePopup
        dispatch <| Sheet(SheetT.PasteArray(dir, copies, firstSuffix))

    /// The dialog itself, over choices already worked out. Separate from pasteArrayPopup because it
    /// does not touch the dialog's fields: the warning below comes back here, and what the user had
    /// chosen before being warned should still be chosen when they return to change it.
    ///
    /// Mutually recursive with the warning, which replaces this dialog rather than covering it -
    /// Issie shows one popup at a time.
    let rec private showArrayDialog (choices: Choices) dispatch =
        let maxFor dir =
            match dir with
            | SheetT.ArrayVertical -> choices.MaxVertical
            | SheetT.ArrayHorizontal -> choices.MaxHorizontal

        /// What the dialog currently asks for, and the first thing wrong with it. An empty message
        /// means it can be pasted. Every check is here rather than at paste time, so that what
        /// cannot be done is said while it can still be changed.
        let entry (dialogData: PopupDialogData) =
            let dir = directionOf dialogData
            let limit = maxFor dir
            let first = int (getInt2 dialogData)
            /// A suffixed label that some component on the sheet already has. Suffixes are the
            /// point of an array, so a clash is the user's to resolve - by starting the numbering
            /// somewhere else - and not something to paper over with a generated name.
            let clash n =
                Seq.allPairs (seq { first .. first + n - 1 }) choices.CopiedLabels
                |> Seq.map (fun (i, label) -> label + string i)
                |> Seq.tryFind (fun label -> Set.contains label choices.LabelsOnSheet)
            match dialogData.Int with
            | _ when limit < 2 ->
                None, first, $"Two copies side by side {nameOf dir}ly do not fit on this sheet."
            | None -> None, first, "Enter how many copies you want."
            | Some n when n < 2 -> None, first, "An array is two copies or more."
            | Some n when n > limit ->
                None, first, $"At most {limit} copies fit {nameOf dir}ly on this sheet."
            | Some _ when first < 0 -> None, first, "Suffixes start at 0 or above."
            | Some n ->
                match clash n with
                | Some label ->
                    None, first,
                    $"{label} is already the label of a component on this sheet. Start the "
                    + "suffixes somewhere else, or rename that component."
                | None -> Some n, first, ""

        let body =
            fun (model: Model) ->
                let dialogData = model.PopupDialogData
                let dir = directionOf dialogData
                let count, first, err = entry dialogData
                /// The two directions as a pair of buttons, the chosen one filled in. A direction
                /// with no room for two copies is still shown, disabled, so that the choice reads
                /// as two ways round of which one is unavailable rather than as one way round.
                let dirButton thisDir label =
                    Button.button
                        [ Button.Color(if dir = thisDir then IsPrimary else IsLight)
                          Button.Disabled(maxFor thisDir < 2)
                          Button.OnClick(fun _ -> setDirection thisDir dispatch) ]
                        [ str label ]
                /// What the numbering will come out as, said in full rather than as a rule, since
                /// it is the thing most worth checking before pasting: which existing labels the
                /// new ones sit next to is exactly what the two boxes are being set to control.
                let suffixes =
                    match count, dialogData.Int with
                    | Some n, _ | None, Some n when n >= 2 ->
                        $"Suffixes {first} to {first + n - 1} will be added to component labels."
                    | _ -> ""
                div [] [
                    str "Copies are placed one after another with a gap of a fifth of the circuit "
                    str "between them, and each copy's labels get its suffix after them: with "
                    str "suffix 0, MUX1 becomes MUX10. Merge and split components have no label "
                    str "and are left without one."
                    br []; br []
                    div [ Style [ Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center ] ] [
                        span [ Style [ MarginRight "10px" ] ] [ str "Array runs" ]
                        // CSSProp. because CommonTypes.ComponentType.Custom is also in scope
                        div [ Style [ Display DisplayOptions.Flex; CSSProp.Custom("gap", "6px") ] ] [
                            dirButton SheetT.ArrayVertical "Vertically"
                            dirButton SheetT.ArrayHorizontal "Horizontally"
                        ]
                    ]
                    br []
                    div [ Style [ Display DisplayOptions.Flex; CSSProp.Custom("gap", "24px") ] ] [
                        div [] [
                            str $"Number of copies (2 to {maxFor dir}):"
                            br []
                            Input.number
                                [ Input.Props [ OnPaste preventDefault; Style [ Width "70px" ]; AutoFocus true ]
                                  Input.DefaultValue(string (Option.defaultValue 2 dialogData.Int))
                                  Input.OnChange(JSHelpers.getIntEventValue >> Some >> SetPopupDialogInt >> dispatch) ]
                        ]
                        div [] [
                            str "Suffixes start from:"
                            br []
                            Input.number
                                [ Input.Props [ OnPaste preventDefault; Style [ Width "70px" ] ]
                                  Input.DefaultValue(string first)
                                  Input.OnChange(
                                      JSHelpers.getIntEventValue >> bigint >> Some >> SetPopupDialogInt2 >> dispatch) ]
                        ]
                    ]
                    br []
                    div [] [ str suffixes ]
                    span [ Style [ Color "Red" ] ] [ str err ]
                ]

        let foot =
            fun (model: Model) ->
                let dialogData = model.PopupDialogData
                let dir = directionOf dialogData
                let count, first, err = entry dialogData
                let confirm _ =
                    match count with
                    | Some n when err = "" ->
                        if Sheet.arrayIsAgainstShape dir choices.Box then
                            againstShapeWarning choices dir n first dispatch
                        else doPaste dir n first dispatch
                    | _ -> ()
                Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                    Level.left [] []
                    Level.right [] [
                        Level.item [] [
                            Button.button
                                [ Button.Color IsLight; Button.OnClick(fun _ -> dispatch ClosePopup) ]
                                [ str "Cancel" ]
                        ]
                        Level.item [] [
                            Button.button
                                [ Button.Color IsPrimary
                                  Button.Disabled(err <> "")
                                  Button.OnClick confirm ]
                                [ str
                                    (match count with
                                     | Some n when err = "" -> $"Paste {n} copies as {nameOf dir} array"
                                     | _ -> $"Paste as {nameOf dir} array") ]
                        ]
                    ]
                ]

        dynamicClosablePopup "Paste as array" body foot [] dispatch

    /// Arraying a fragment along the side it is already long on: allowed, because it is sometimes
    /// what is meant, but worth saying out loud first - the result is a strip several times longer
    /// again, which is awkward to place and hard to read.
    and private againstShapeWarning (choices: Choices) dir copies firstSuffix dispatch =
        let box = choices.Box
        let long, short, better =
            match dir with
            | SheetT.ArrayVertical -> "taller than it is wide", "horizontal", "horizontally"
            | SheetT.ArrayHorizontal -> "wider than it is tall", "vertical", "vertically"
        let arrayed = Sheet.arrayBox dir copies box
        // The threshold is not quoted, so that the wording cannot go stale if
        // Constants.arrayAspectWarning is changed. The sizes below say more anyway.
        let body =
            div [] [
                str $"The copied circuit is much {long} - %.0f{box.W} by %.0f{box.H} - and a "
                str $"{nameOf dir} array stacks the copies along that same side. {copies} of them "
                str $"come to %.0f{arrayed.W} by %.0f{arrayed.H}, which is a long strip to place "
                str "and to read."
                br []; br []
                str $"Arraying it {better} instead would keep the copies beside each other. To do "
                str $"that, go back and choose the {short} direction - or lay the circuit out the "
                str "other way round before copying it."
            ]
        choicePopup
            "This array runs along the circuit's long side"
            body
            $"Paste {copies} copies anyway"
            "Go back"
            (fun proceed _ ->
                if proceed then doPaste dir copies firstSuffix dispatch
                // back to the dialog exactly as it was left: the fields are untouched by either
                // of these, so what was chosen is still chosen
                else showArrayDialog choices dispatch)
            dispatch

    /// Edit > Paste array. Sets the dialog up - which way round to offer, and how many copies -
    /// and shows it.
    let pasteArrayPopup (model: Model) dispatch =
        match arrayChoices model with
        | None -> ()      // nothing copied: the menu item is greyed, so this cannot normally happen
        | Some choices when max choices.MaxVertical choices.MaxHorizontal < 2 -> tooBigPopup dispatch
        | Some choices ->
            // A fragment is arrayed across its short side, so a wide one stacks vertically. Where
            // that way round has no room on the sheet the other one is offered instead.
            let preferred =
                let wanted = Sheet.arrayDirectionFor choices.Box
                let fits =
                    match wanted with
                    | SheetT.ArrayVertical -> choices.MaxVertical >= 2
                    | SheetT.ArrayHorizontal -> choices.MaxHorizontal >= 2
                match fits, wanted with
                | true, _ -> wanted
                | false, SheetT.ArrayVertical -> SheetT.ArrayHorizontal
                | false, SheetT.ArrayHorizontal -> SheetT.ArrayVertical
            setDirection preferred dispatch
            dispatch <| SetPopupDialogInt(Some 2)
            dispatch <| SetPopupDialogInt2(Some 0I)
            showArrayDialog choices dispatch

/// Memory Properties Info Button Popup
let memPropsInfoButton dispatch =
    let title = AppMessages.Memories.title
    let info = helpText AppMessages.Memories.help
    makeInfoPopupButton title info dispatch





//-------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------Top-level Popup functions-------------------------------------------------//
//-------------------------------------------------------------------------------------------------------------------//

/// make a popup button with the given popup
let makePopupButton (title: string) (menu: Model -> ReactElement) (buttonLegend: string) dispatch =

    let foot _ = div [] []
    let popup dispatch = 
        dynamicClosablePopup title (fun model -> menu model) foot [Width 600] dispatch
    // button driving a popup with a page of info
    Button.button
        [
            Button.OnClick (fun _ -> popup dispatch)
            Button.Color IsPrimary
        ]
        [str buttonLegend]


/// Display popup, if any is present.
/// A progress popup, if present, overrides any display popup.
/// A spinner popup, if present, overrides all other popups.
/// Called from the view function
let viewPopup model dispatch =
    match model.PopupDialogData.Progress, model.PopupViewFunc, model.SpinnerPayload with
    | _, _, Some ({UseProgressBar=true} as payload) ->
        viewSpinnerPopup payload model dispatch
    | Some amount, _, _ ->
        progressPopup simulationLegend model dispatch
    | None, Some popup, _ -> popup dispatch model
    | _ ->  div [] []




//-------------------------------------------------------------------------------------------------------------------//
//-----------------------------------------New-style Waveform Simulator Popups---------------------------------------//
//-------------------------------------------------------------------------------------------------------------------//

/// Create the body of a popup to configure Waveform Simulator.
/// This must include the OK button since enable for this comes from the error checkaing here.
let dialogWaveSimConfigPopup (dispatch: Msg -> unit) (model:Model) =
    let inBounds bMin bMax n = n <= bMax && n >= bMin
    let config_ = waveSimModel_ >-> wSConfig_
    let configDialog_ = waveSimModel_ >->  wSConfigDialog_  >-> Option.withDefault_ (getWSModel model).WSConfig
    let initConfig = Optic.get configDialog_ model
    let wsModel = getWSModel model
    // the design's per-cycle cost, priced from the flattened design without building anything -
    // this runs on every render of the dialog, so it must not be the thing it is budgeting for
    let designCost = ModelHelpers.waveSimStepCost model

    let arraySizeMessage (c: WSConfig) =
        match designCost with
        | Error _ -> "Unknown: correct schematic error to get size information"
        | Ok cost ->
            let needed = float cost.TotalBytes * float c.LastClock
            $"Simulating {c.LastClock} cycles of this design needs \
              {SimTypes.SimulationBudget.formatBytes needed} of simulation memory. At most \
              {FastCreate.maxCyclesFor cost} cycles of it can be simulated."

    /// Too big to simulate at all, rather than merely large. FastCreate.maxCyclesFor is the same
    /// limit the simulator applies when it builds, so OK is disabled here rather than the
    /// simulation being refused after the dialog has been closed.
    let sizeIsRefused (c: WSConfig) =
        match designCost with
        | Error _ -> c.LastClock > Constants.maxWarnSimulationSize
        | Ok cost -> c.LastClock > FastCreate.maxCyclesFor cost

    let errorKeys, messages  =
        let c = model |> Optic.get configDialog_
        [
            ["first"], c.FirstClock < 0, "The first clock cycle cannot be negative"
            ["last"], c.LastClock > Constants.maxSimulationSize, $"The last clock cannot be larger than {Constants.maxSimulationSize}"
            ["first";"last"], c.FirstClock > c.LastClock - Constants.minScrollingWindow, $"The difference between first and last clock cycles must \
                                                                                             be at least {Constants.minScrollingWindow} cycles."
            ["fontsize"], not <| inBounds 12 24 c.FontSize, $"Font size must be between 12 and 24"
            ["fontweight"], not <| inBounds 100 900 c.FontWeight, $"Font weight must be between 100 and 900"
            // an error and not a warning: a simulation this size is refused when it is built, so
            // letting OK through here would only move the refusal to somewhere it explains less
            ["last"], sizeIsRefused c, $"{arraySizeMessage c} Reduce the last clock cycle, or \
                                         simulate one subsheet rather than the whole design."
        ]  
        |> List.filter (fun (_, isError, _) -> isError)
        |> List.map (fun (key, _, message) -> key, message)
        |> List.unzip

    let hasError key = List.contains key (errorKeys |> List.concat)

    let setConfigInt (optic_: Lens<WSConfig,int>) (value:int) =
        dispatch <| UpdateModel (Optic.map (configDialog_ >-> optic_) (fun _ -> value))

    let isValid = List.isEmpty (errorKeys |> List.concat)

    let closeAction changeConfig dispatch model =
        let wsm = getWSModel model
        if changeConfig then
            let dialog = wsm.WSConfigDialog
            if dialog = None then Log.warn "no WSConfigDialog when closing the waveform configuration popup"
            dispatch <| UpdateModel (Optic.set (waveSimModel_ >-> wSConfig_) (Option.defaultValue wsm.WSConfig dialog))
        dispatch <| ClosePopup
        dispatch <| UpdateModel (Optic.map (waveSimModel_ >-> wSConfigDialog_) (fun _ -> None))



   
    let boxStyle = Style [Width Constants.wsButtonWidth; Font Constants.wsButtonFontSize; Height 24; Margin 10]
    let colStyles = [   [Width 200; Margin "50px"; PaddingTop "10px"; FontWeight 600];
                        [Width 70; PaddingRight "50px"];
                        [Width 800; LineHeight "24px"; Margin "40px"; PaddingTop "10px"]]
    let itemStyle = [Border "none"]
    let row items = tr [Style [BorderCollapse "collapse"; Height "60px"; TextAlign TextAlignOptions.Justify]]
                        (List.mapi (fun i item ->
                            td [ Style (itemStyle @ colStyles[i]) ] [item]) items)

    div [Style []] [
        table [Style [LineHeight "40px"; BorderStyle "none"; BorderColor "white"; TextAlign TextAlignOptions.Left]] [
            tbody [] [

                row [
                        span [boxStyle] [str "Waveform font size:"]           
                        Input.number [
                            Input.Props [OnPaste preventDefault;  boxStyle; AutoFocus true; ]
                            Input.DefaultValue <| string initConfig.FontSize
                            Input.Color (if hasError "fontsize" then IColor.IsDanger else IColor.IsBlack)
                            Input.OnChange (JSHelpers.getIntEventValue >> setConfigInt fontSize_)
                        ]
                        str "A larger size will be easier to read but will make numeric values overflow (and be greyed out) out more easily."
                    ]
                row [
                        span [boxStyle] [str "Waveform font weight:"]           
                        Input.number [
                            Input.Props [OnPaste preventDefault;  boxStyle; AutoFocus true; ]
                            Input.DefaultValue <| string initConfig.FontWeight
                            Input.Color (if hasError "fontweight" then IColor.IsDanger else IColor.IsBlack)
                            Input.OnChange (JSHelpers.getIntEventValue >> setConfigInt fontWeight_)
                        ]
                        str "Font weight of 300 = normal, 600 = bold, etc"
                    ]
                row [
                    span [boxStyle] [str "Max clock cycle:"]           
                    Input.number [
                        Input.Props [OnPaste preventDefault;  boxStyle; AutoFocus true; ]
                        Input.DefaultValue <| string initConfig.LastClock
                        Input.Color (if hasError "last" then IColor.IsDanger else IColor.IsBlack)
                        Input.OnChange (JSHelpers.getIntEventValue >> setConfigInt lastClock_)
                    ]
                    str "Note that the waveform simulator will only simulate and scroll up to the current last cycle \
                         which is much smaller than this unless cursor movement or scroll forces more to be simulated."
                ]

            ]
        ]

        div [Style [Color "red"; Height 100]] (messages
                                                |> List.map (fun (mess:string) -> [str mess; br []])
                                                |> List.concat)

        newButtonFoot (closeAction true) "Ok" (closeAction false) (fun _ -> not isValid) dispatch model
    ]


    
let makeWSPopupButton (body: DynamicElement) (iColor: IColor) (cssProps: CSSProp list) dispatch model=
    Button.button
        [
            Button.OnClick (fun _ -> dispatch <| ShowPopup body)
            Button.Color IsPrimary
            Button.Disabled ((getWSModel model).State = Success)
            Button.Props [Style cssProps]
        ]
        [str "Configure"]   


let makeWSConfigButton dispatch model =
    let buttonProps =  [Height Constants.wsButtonHeight; Width Constants.wsButtonWidth]
    let props = [Height 600; Width 1200]
    let closeConfigDialog = UpdateModel <| Optic.set (waveSimModel_ >-> wSConfigDialog_) None
    let popup = newBuildPopup
                    "Advanced Configuration"
                    dialogWaveSimConfigPopup
                    (fun _ _ -> div [] [])
                    (fun dispatch _ -> dispatch <| closeConfigDialog)
                    props
    makeWSPopupButton
        popup
        IColor.IsSuccess
        buttonProps
        dispatch
        model
       
       
        




