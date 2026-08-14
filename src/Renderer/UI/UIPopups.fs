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
       
       
        




