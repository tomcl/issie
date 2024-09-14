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


let viewInfoPopup dispatch =

    let title = "ISSIE: Interactive Schematic Simulator and Integrated Editor"

    let about = div [] [
        makeH "Version"
        str Version.VersionString
        br []; br []
        makeH "Acknowledgments"
        str "ISSIE was created by Marco Selvatici (EIE 3rd year) as his BEng final year project. \
             The waveform viewer was created \
             by Edoardo Santi (EEE 3rd year) during Summer UROP work. The new F# schematic editor \
             was written as 2021 coursework by HLP students in EEE, \
             and particularly Team 4. The new editor was integrated \
             by Jo Merrick (EIE 3rd year) for her BEng final year project. \
             In Spring 2022 the HLP class implemented a draw block with component rotation and much better routing. \
             In Summer 2022 Jason Zheng rewrote the waveform simulator, Aditya Despande wrote the truth table generator, \
             and Archontis Pantelopoulos spent all Summer on a UROP writing the Verilog entry block and making many improvements. \
             In 2023 HLP students implemented intelligent routing, Yujie Wang made the simulator faster, and Petra Ratkai \
             implemented a much better Verilog compiler."
        br []; br [] 
        makeH "Technology"
        Text.span [] [
            str "ISSIE is written in "
            a [OnClick <| openInBrowser "https://fsharp.org/"] [str "F#"] 
            str " compiled to Javascript by "
            a [OnClick <| openInBrowser "https://fable.io/"] [str "FABLE"]
            str " and running under the "
            a [OnClick <| openInBrowser "https://www.electronjs.org/"] [str "Electron"]
            str " framework" 
            ]    
        ]

    let intro = div [] [
        str "Issie designs are hierarchical, made of one main sheet and optional subsheets. Include the hardware defined on one sheet in another \
        by adding any number of  'custom components' from the 'My Project' section of the Catalog. The Sheet menu shows the hierarchy." 
        br []; br []
        str "Issie supports step simulation for all circuits, and waveform simulation to view the waveforms of clocked circuits. \
        Use whichever works for you." 
        br []; br [];
        str "In Issie all clocked components (blue fill) use the same clock signal Clk. \
        Clk connections are not shown: all Clk ports are \
        automatically connected together. In the waveform display active clock edges, 1 per clock cycle, are indicated \
        by vertical lines through the waveforms."
        br []  ; br [];  
        button 
            [OnClick <| openInBrowser "https://github.com/tomcl/ISSIE"] 
            [ str "See the Issie Github Repo for more information"]
        br [] ; br [] ]

    let tips = div [] [
        Table.table [] [
                tbody [] [
                    tr [] [
                        td [] [str "Right-Click Menus"]
                        td [] [str "Explore the Right-Click Menus to find context-dependent operations"]
                    ]
 
                    tr [] [
                        td [] [str "Ctrl-W"]
                        td [] [str "Use Ctrl-W to fit the current sheet to the window so you can see all the components"]
                    ]
                    tr [] [
                        td [] [str "Sheet descriptions"]
                        td [] [str "Add short descriptions to your design sheets"]
                    ]
                    tr [] [
                        td [] [str "Copy, Paste"]; 
                        td [] [str "Use copy and one or more Pastes (keys or on-screen buttons) to make duplicate \
                                    components with the same name and increasing numbers. Copy multiple items onto the same sheet or a new sheet"]
                    ]
                    tr [] [
                        td [] [str "Undo, Redo"]; 
                        td [] [str "From onscreen buttons or keys - use them, they work well!"]
                    ]

                    tr [] [
                        td [] [str "Ctrl-drag"]; 
                        td [] [str "Ctrl-drag ports on custom components to a new position on any side. Change the component height, width in properties if it is the wrong size."]
                    ]
                    tr [] [
                        td [] [str "2-MUX properties"]; 
                        td [] [str "Swap 0/1 inputs in properties if this makes a neater diagram"]
                    ]
                    tr [] [
                        td [] [str "Counters, Adders"]; 
                        td [] [str "Hide inputs/outputs you do not need from properties"]
                    ]
                    tr [] [
                        td [] [str "Set Default input values"]; 
                        td [] [str "Set the input values you want in the step simulator and 'click set default inputs', or set individually in input properties. \
                                    This will remember the values for both step simulator and waveform viewer"]
                    ]
                    tr [] [
                        td [] [str "Use properties"]; 
                        td [] [str "Use properties to change labels, bus widths, etc of all components."]
                                    
                    ]
                    tr [] [
                        td [] [str "Use radix for constant values"]; 
                        td [] [str "Enter constant values for constants and bus comparators in the radix which \
                                    makes most sense - they will dispaly as you have entered it."]
                    ]
                    tr [] [
                        td [] [str "Position labels, rotate and flip components"]; 
                        td [] [str "Drag or rotate (key) labels, reposition, rotate or flip components, drag wires, as needed to get a neat schematic. \
                                    You can select and reposition multiple components"]
                    ]




                ]
            ]
        ]
    

    let bugReport = 
        let oTextList txtL = Content.content [] [Content.Ol.ol [] (List.map (fun txt -> li [] [str txt]) txtL)]
        div [] [
            str
                "If you think Issie is not working it is very helpful if you can give us details: we usually answer \
                and fix bugs, if they exist, very quickly. Before you contact us, look at the list below and answer as much \
                as possible to make your Bug Report (sometimes it is not all possible, send what you can)."
            oTextList 
                [
                    "Which version of Issie (Info tab, About Issie)"
                    "Which platform (Windows, Macos)"    
                    "What did you do that led to unexpected behaviour?"   
                    "What result did you expect?"   
                    "What result did you get?"   
                    "What project files caused this, the top-level sheet? Enclose project as zipped file \
                    deleting the maybe large backup directory when you zip."   
                    "If you can reproduce the bug yourself, try opening dev tools (Ctrl-Shift-I). You can do this after the bug happens. 2/3 \
                    of problems result in error messages displayed there. Screenshot the error and its backtrace and send it."   
                    "What precise actions (if you know them) led to the bug after loading this project"
                ]
            ]
    let keyOf2 s1 s2 = span [] [bSpan s1; tSpan " + "; bSpan s2]
    let keyOf3 s1 s2 s3 = span [] [bSpan s1; tSpan " + "; bSpan s2 ; tSpan " + "; bSpan s3]
    let rule = hr [Style [MarginTop "0.5em"; MarginBottom "0.5em"]]
    let keys = div [] [
        makeH "Keyboard & mouse gesture shortcuts - also available on top menus and right-click context menus"
        span [Style [FontStyle "Italic"]] [str "On Mac use Cmd instead of Ctrl."]
        ul [] [
            li [] [rule; tSpan "Save: "; keyOf2 "Ctrl" "S"; rule]
            li [] [tSpan "Select all: " ; keyOf2  "Ctrl" "A"]
            li [] [tSpan "Copy selected diagram items: " ; keyOf2  "Ctrl" "C"]
            li [] [tSpan "Paste diagram items: " ; keyOf2  "Ctrl" "V"; rule]
            li [] [tSpan "Undo last diagram action: " ; keyOf2  "Ctrl" "Z"]
            li [] [tSpan "Redo last diagram action: " ; keyOf2  "Ctrl" "Y"; rule]
            li [] [tSpan "Zoom application in: " ; keyOf3  "Ctrl" "Shift" "+"]
            li [] [tSpan "Zoom application out: " ; keyOf3  "Ctrl" "Shift" "-"; rule]
            li [] [tSpan "Zoom canvas in/out: " ; keyOf2  "Ctrl" "MouseWheel"]
            li [] [tSpan "Zoom canvas in: " ; keyOf2  "Alt" "Up"]
            li [] [tSpan "Zoom canvas out: " ; keyOf2  "Alt" "Down"; rule]
            li [] [tSpan "Zoom circuit to fit screen: " ; keyOf2  "Ctrl" "W"]
            li [] [tSpan "Scroll (mouse): " ; keyOf2 "Shift" "Left-Click"; bSpan " on canvas and drag"]
            li [] [tSpan "Scroll (touch-pad): " ; bSpan "Two-finger scrolling on touchpad"]
            li [] [tSpan "Scroll (touch-screen): " ; bSpan "One-finger drag on screen"; rule]
            li [] [tSpan "Rotate symbol (clockwise/anticlockwise): "; keyOf2 "Ctrl" "Right/Left Arrow"]
            li [] [tSpan "Flip symbol (vertical/horizontal): "; keyOf2 "Ctrl" "Up/Down Arrow";rule]
            li [] [tSpan "Align symbols: "; keyOf3 "Ctrl" "Shift" "A"]
            li [] [tSpan "Distribute symbols: "; keyOf3 "Ctrl" "Shift" "D"]
            li [] [tSpan "Rotate label: "; keyOf3 "Ctrl" "Shift" "Right arrow"]
         ] ]
    let body model =
        let dialogData = model.PopupDialogData
        let tab = dialogData.Int

        div [] [
            Tabs.tabs 
                [ Tabs.IsFullWidth
                  Tabs.IsBoxed ]
                [ Tabs.tab [ Tabs.Tab.IsActive (tab = Some 0) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 0)) ]
                    [ str "About Issie" ] ]
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 1) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 1)) ]
                    [ str "Introduction" ] ] 
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 2) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 2)) ]
                    [ str "Tips & Features" ] ]  
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 3) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 3)) ]
                    [ str "Keyboard Shortcuts" ] ]
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 4) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 4)) ]
                    [ str "Bug Reports" ] ] ]

            match tab with
            | Some 0 -> about
            | Some 1 -> intro
            | Some 2 -> tips
            | Some 3 -> keys
            | Some 4 -> bugReport
            | _ -> dispatch <| SetPopupDialogInt (Some 0)
        ]

    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 900] dispatch

let viewWaveInfoPopup dispatch feature =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br []]
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]

    let title = feature

    let waveInfo =
        div [] [
        match feature with

        | "Getting Started" ->

            ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
                li [] [str "The waveform viewer can show waveforms selected from "; bSpan  " any sheet"; str " in the design being simulated."]         
                li [] [str "Choose the top sheet you want to simulate. Press"; bSpan " Start"; str " to start the viewer. Then press the";
                       bSpan " Select Waves "; str " button to select or change which waveforms are viewed. See the selection popup info button for more info."]                    
                li [] [str "Use Ctrl/Shift/- and Ctrl/Shift/+ buttons to resize the viewer so you can comfortably see the correct number of \
                            waveforms." ; str " Use Alt/UpArrow and Alt/DownArrow to zoom the Schematic Editor canvas."]      
                li [] [str "Drag the"; bSpan " grey horizontal divider bar "; str " to make the waveform viewer wider."] 
            ]

        | "Viewing Waveforms" ->

            ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
                li [] [str "Hover on a waveform name to see component and connections highlighted in editor."]      
                li [] [str "Drag waveform names to reorder waveforms; Click the x button to delete a waveform."]                    
                li [] [str "Click on waveforms to change the highlighted clock cycle: see values of signals for this cycle on the righthand side."]      
                li [] [str "Use the right-hand input box to move to a new highlighted cycle number, or the arrows to change current cycle."]
                li [] [str "Drag the scrollbar to scroll. When the thumb is at the righthand side drag it more to extend the simulated cycles."]
                li [] [str "Use the lefthand zoom buttons to zoom out or in. Use the radix buttons to change display radix."]
            ]

        | "Waveform and RAM selection" ->

            ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
                li [] [str "The waveform viewer can view signals on"; bSpan  " any sheet"; str " in the design being simulated."]
         
                li [] [str "Use 'select waves' window to select which waveforms are viewed. The search box allows them to be selected by part of name. \
                           Alternatively, expand groups to explore design and find components and ports."]
                    
                li [] [str "The waveforms you view can be changed whenever the simulation is running. It is good practice to \
                            delete waveforms you are not using, and order waveforms logically."]
                li [] [str "Use 'select RAM' to view RAMs showing contents, read and write location, in the current (cursor) cycle."]
                li [] [str "Selected waveforms are preserved from one simulation to the next."]
            ]
        | "Instructions" ->
        
            ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
                li [] [ str "Hover mouse over a waveform name in the viewer to see it highlighted on the current schematic sheet."]
                li [] [ str "Change schematic sheet to view or alter components on subsheets."]
                li [] [ str "Drag names to reorder waveforms, use delete icon to delete, use "; bSpan "Select Waves"; str " to add or delete."]     
                li [] [ str "Scroll or use " ; bSpan "Scrollbar arrows" ; str " and "; DiagramStyle.zoomOutSVG; DiagramStyle.zoomInSVG;
                        str " controls to show which cycles to display."]
                li [] [ str "Move the coloured"; bSpan " cursor clock cycle"; str " using "; bSpan "a click on the waveforms,";
                        str " the "; bSpan "cursor box number,"; bSpan " box controls,"; str " or "; bSpan "Left/Right Arrow Keys";
                        str " when the mouse is on the righthand side of the grey divider." ]
                li [] [ str "The column to the right of the waveforms shows signal values in the cursor cycle" ]
                li [] [ str "Drag the"; bSpan " grey vertical divider "; str "to alter the screen space used by waveforms"]
                li [] [ str "Waveforms will scroll vertically if you select more than will fit on the screen."]
                li [] [ str "Use "; bSpan "Select RAM"; str " to view RAM contents for the current cycle."]
                li [] [ str "Use "; bSpan "Bin Hex uDec sDec"; str " buttons to change the display radix."]
                li [] [ bSpan "Ctrl/Shift/+"; str " and "; bSpan "Ctrl/Shift/-"; str " will "; bSpan " zoom "; str "the viewer."]
            ]
        | "Miscellaneous" ->
        
            ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
                li [] [str "During a simulation you can move to any sheet and view or edit the design. \
                           When any part of the design, or linked memory contents files, changes the green update button will be enabled allowing \
                           update to the newer design."] 
                li [] [str "You can change default values for sheet inputs in Input component property boxes. \
                           The top sheet inputs of the simulation are given these values throughout the simulation. \
                           Adjustable values anywhere else in the design can be implemented using constants."]
                li [] [str "The waveform radix can be changed. When waveforms are too small to fit binary this will be changed to hex. \
                            Numeric values not displayed on the waveform can be viewed using the cursor and the righthand panel."]
            ]
        | _ ->
            p [] [str "Feature not explained"]
    ]
   
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
            str $"You have selected {numWaves} waveforms. "
            str $"Consider reducing this number to less than {numRequired}. Too many waveforms selected in the viewer may impact viewer reponsiveness. \
                 Best practice is to select only the waveforms you need to view."
        ]  
       
    let body (dialogData:PopupDialogData) =
        warning
    let foot _ = div [] []
    choicePopup title warning "Select waveforms" "Change selection"  action dispatch

/// Memory Properties Info Button Popup
let memPropsInfoButton dispatch =
    let title = "Issie Memories: how RAM and ROM data works"
    let bullet s = li [] [str s]
    let info = 
        ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
            bullet "RAMs and ROMs need to have initial data contents defined. For RAMs the fixed initial data \
                    is reset for clock cycle 0 whenever a simulation is started, the RAM data can change during simulation."  
            bullet "The default initial data is all 0s. Initial data is stored with the design sheet and  may be viewed or \
                    modified with the memory editor from properties. The editor can change locations numbered higher than 15 by entering a \
                    number in the 'first location displayed' box."
            bullet "During the Step or Waveform Viewer simulation RAM data can be viewed, but not manually changed. RAM data may change as the result of writes. \
                    These changes don't affect the initial data."
            bullet "When using external tools like an assembler it is useful to enter RAM or ROM initial data from a text file. Memory data can be \
                    written to a file with extension '.ram'. If a '.ram' file is placed in the project directory a RAM or ROM component can be linked to the \
                    file, or unlinked, by selecting it from the properties page."
            bullet "Linked memories will have initial data updated to latest file contents, if they change. Update is automatic
                    when a new simulation is started and otherwise will happen if needed when the Issie screen refreshes."
        ]
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
    let fs = resimulateWaveSimForErrors model

    let arraySize (c:WSConfig) =
        match fs with
        | Error _ -> Error "Unknown: correct schematic error to get size information"
        | Ok sd ->
            let stepCost = sd.FastSim.TotalArraySizePerStep
            Ok <| float stepCost * float c.LastClock / (System.Math.Pow(1024.0, 3.0))

    let warnSizeLarge c =    
        match arraySize c with
        | Error e -> c.LastClock > Constants.maxWarnSimulationSize
        | Ok size -> size > 2.5

    let arraySizeMessage c = 
        match arraySize c  with
        | Error e -> e
        | Ok size -> $"Array memory use for the current design is estimated as %.1f{size} GB"    

    let errorKeys, messages  =
        let c = model |> Optic.get configDialog_
        [
            ["first"], c.FirstClock < 0, "The first clock cycle cannot be negative"
            ["last"], c.LastClock > Constants.maxSimulationSize, $"The last clock cannot be larger than {Constants.maxSimulationSize}"
            ["first";"last"], c.FirstClock > c.LastClock - Constants.minScrollingWindow, $"The difference between first and last clock cycles must \
                                                                                             be at least {Constants.minScrollingWindow} cycles."
            ["fontsize"], not <| inBounds 12 24 c.FontSize, $"Font size must be between 12 and 24"
            ["fontweight"], not <| inBounds 100 900 c.FontWeight, $"Font weight must be between 100 and 900"
            [], warnSizeLarge c, $"Warning: very large simulation lengths and big designs result in high memory use and low performance. \
                                   Simulation data memory use for the current design is estimated as\n: \
                                   {arraySizeMessage c}, in addition up to 5GB will be required for heap and code. \
                                   Systems using more than around 3GB simulation array memory may to crash. This limit does not \
                                   depend on PC physical memory."                                                                
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
            if dialog = None then printf $"Unexpected WSConfigDialog = None when closing configuration popup. changeConfig = {changeConfig}"
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
       
       
        




