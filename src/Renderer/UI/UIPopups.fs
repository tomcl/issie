﻿module UIPopups

open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open PopupHelpers
open ModelType
open System

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
    dispatch <| UpdateModel spinPayload.Payload
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
        str "Issie designs are hierarchical, made of one main sheet and optional subsheets. Include the hardware defined on one sheet in another
        by adding a 'custom component' from the 'My Project' section of the Catalog. \
        Top-level sheets which are not used as subsheets are bolded on the sheet menu." 
        br []; br []
        str "Issie supports step simulation for all circuits, and waveform simulation to view the waveforms of clocked circuits.
        Use whichever works for you." 
        br []; br [];
        str "In Issie all clocked components use the same clock signal Clk. \
        Clk connections are not shown: all Clk ports are
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
                        td [] [str "Left-Click Menus"]
                        td [] [str "Explore the Left-Click Menus to find context-dependent operations"]
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
        makeH "Keyboard & mouse gesture shortcuts - also available on top menus and left-click context menus"
        span [Style [FontStyle "Italic"]] [str "On Mac use Cmd instead of Ctrl."]
        ul [] [
            li [] [rule; tSpan "Save: "; keyOf2 "Ctrl" "S"; rule]
            li [] [tSpan "Select all: " ; keyOf2  "Ctrl" "A"]
            li [] [tSpan "Copy selected diagram items: " ; keyOf2  "Ctrl" "C"]
            li [] [tSpan "Paste diagram items: " ; keyOf2  "Ctrl" "V"; rule]
            li [] [tSpan "Undo last diagram action: " ; keyOf2  "Ctrl" "Z"]
            li [] [tSpan "Redo last diagram action: " ; keyOf2  "Ctrl" "Y"; rule]
            li [] [tSpan "Zoom application in: " ; keyOf3  "Ctrl" "Shift" "="]
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

let viewWaveInfoPopup dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br []]
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]

    let title = "How to Use the Waveform Viewer"

    let waveInfo = div [] [
        makeH "Waveform and RAM Selection"
        ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
            li [] [str "The waveform viewer can view signals on"; bSpan  " any sheet"; str " in the design being simulated."]
         
            li [] [str "Use 'select waves' window to select which waveforms are viewed. The search box allows them to be selected by part of name. \
                       Alternatively, expand groups to explore design and find components and ports."]
                    
            li [] [str "The waveforms you view can be changed whenever the simulation is running. It is good practice to \
                        delete waveforms you are not using, and order waveforms logically."]
            li [] [str "Use 'select RAM' to view RAMs showing contents, read and write location, in the current (cursor) cycle."]
            li [] [str "Selected waveforms are preserved from one simulation to the next."]
        ]

        makeH "Waveform Operations"
        ul [Style [ListStyle "disc"; MarginLeft "30px"]] [
            li [] [ str "Hover mouse over a waveform name in the viewer to see it highlighted if it is on the current sheet."]
            li [] [ str "Change sheet to view or alter components on subsheets."]
            li [] [ str "Drag names to reorder waveforms, use delete icon to delete, use 'select waves' to add."]
     
            li [] [ str "Use cursor and zoom controls at any time to show which cycles to display."]
            li [] [ str "The cursor current cycle is greyed and can be moved by clicking the the waveforms, \
                        altering the number in the cursor box, or clicking arrows."]
            li [] [ str "Drag the grey divider to alter space used by waveforms"]
        ]
        makeH "Miscellaneous"
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
    ]

   
    let body (model: Model) =
        waveInfo
    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 1000] dispatch

/// Waveform Selection confirmation popup
let viewWaveSelectConfirmationPopup numWaves action dispatch =
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
            str "Consider reducing this number to less than 20. Too many waveforms selected in the viewer may impact viewer reponsiveness. \
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
    | None, None, None -> 
        div [] []
    | _, _, Some payload ->
        viewSpinnerPopup payload model dispatch
    | Some amount, _, _ ->
        progressPopup simulationLegend model dispatch
    | None, Some popup, _ -> popup dispatch model 


