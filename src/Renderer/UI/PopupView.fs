(*
    PopupView.fs

    This module provides a handy interface to create popups and notifications.
    Popups and notifications appear similar, but are actually quite different:
    - Model.Popup is a function that takes a STRING and produces a ReactElement.
    - Model.Notifications are a functions that take DISPATCH and produce a
      ReactElement.
    This means that at the moment of creation, a popup must already have the
    dispatch function, while the notification does not. This, in turn, means
    that notifications can be created from messages dispatched by JS code.
*)


(*

Popups must be careful in handling internal state because this cannot be updated by
dispatch as would be expected.

viewpopup model ->
model.Popup model.PopupDialogData -> (PopupDialogData contains memory setup data (widths) but not memory component data)

model.Popup <-- 
    unclosablePopup maybeTitle (body memoryEditorData) maybeFoot extraStyle
        (from showMemoryEditorPopup maybeTitle body maybeFoot extraStyle dispatch)

        Here body contains the relevant state and is generated from:

        let openMemoryEditor memory compId model dispatch : unit =
        ....
        let body = makeEditor memory compId model dispatch
        ....
        showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch


Although showPopup

*)

module PopupView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open JSHelpers
open Helpers
open ModelType
open CommonTypes
open DiagramStyle
open EEExtensions


//=======//
//HELPERS//
//=======//

let openInBrowser url =
    (fun _ -> Electron.Electron.electron.shell.openExternal url |> ignore)

let extractLabelBase (text:string) : string =
    text.ToUpper()
    |> Seq.takeWhile (fun ch -> ch <> '(')
    |> Seq.filter (fun ch -> System.Char.IsLetterOrDigit ch || ch = '_')
    |> Seq.map (fun ch -> ch.ToString())
    |> String.concat ""

let formatLabelAsBus (width:int) (text:string) =
    let text' = extractLabelBase text
    match width with
    | 1 -> text'
    | _ -> sprintf "%s(%d:%d)" text' (width-1) 0
   

let formatLabelFromType compType (text:string) =
    let text' = extractLabelBase text
    match compType with
    | Input 1 | Output 1 -> text'
    //| Input width | Output width -> sprintf "%s(%d:%d)" text' (width-1) 0
    | _ -> text'


let formatLabel (comp:Component) (text:string) =
    formatLabelFromType comp.Type (text:string)

// TODO: removed formatLabel for now
let setComponentLabel model (sheetDispatch) (comp:Component) text =
    // let label = formatLabel comp text
    let label = text // TODO

    model.Sheet.ChangeLabel sheetDispatch (ComponentId comp.Id) label
    //model.Diagram.EditComponentLabel comp.Id label



//========//
// Popups //
//========//

let getText (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text

let getInt (dialogData : PopupDialogData) =
    Option.defaultValue 1 dialogData.Int

let getInt2 (dialogData : PopupDialogData) : int64 =
    Option.defaultValue 0L dialogData.Int2

let getMemorySetup (dialogData : PopupDialogData) wordWidthDefault =
    Option.defaultValue (4,wordWidthDefault,FromData,None) dialogData.MemorySetup

let getMemoryEditor (dialogData : PopupDialogData) =
    Option.defaultValue
        { Address = None; OnlyDiff = false; NumberBase = Hex ; Start = 0L}
        dialogData.MemoryEditorData

/// Unclosable popup.
let unclosablePopup maybeTitle (body:ReactElement) (maybeFoot: ReactElement option) extraStyle =
    fun dispatch  ->
        let propStyle extraStyle  = Props [Style (UserSelect UserSelectOptions.None :: extraStyle)]
        let head =
            match maybeTitle with
            | None -> div [] []
            | Some title -> Modal.Card.head [propStyle []] [ Modal.Card.title [] [ str title ] ]
        let foot =
            match maybeFoot with
            | None -> div [] []
            | Some foot -> Modal.Card.foot [] [ foot ]
        Modal.modal [ Modal.IsActive true ] [
            Modal.background [] []
            Modal.Card.card [Props [Style  extraStyle]] [
                head
                Modal.Card.body [Props [Style [UserSelect UserSelectOptions.None]]] [ body ]
                foot
            ]
        ]

let noDispatch (react: ReactElement) =
    fun (_dispatch: Msg->Unit) -> react

let mapNoDispatch (optReact: ReactElement option) =
    Option.map noDispatch optReact

let showMemoryEditorPopup maybeTitle body maybeFoot extraStyle dispatch =
    fun _ dialogData->
        let memoryEditorData = getMemoryEditor dialogData
        unclosablePopup maybeTitle (body memoryEditorData) maybeFoot extraStyle dispatch
    |> ShowPopup |> dispatch

let private buildPopup title body foot close extraStyle =
    fun (dispatch:Msg->Unit) (dialogData : PopupDialogData) ->
        Modal.modal [ Modal.IsActive true; Modal.CustomClass "modal1"] [
            Modal.background [ Props [ OnClick (close dispatch)]] []
            Modal.Card.card [ Props [
                Style ([
                    OverflowY OverflowOptions.Auto
                    OverflowX OverflowOptions.Visible
                    UserSelect UserSelectOptions.None
                    ] @ extraStyle)
                ] ] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick (close dispatch) ] []
                ]
                Modal.Card.body [Props [Style [ OverflowY OverflowOptions.Visible ;OverflowX OverflowOptions.Visible]]] [ body dispatch dialogData ]
                Modal.Card.foot [] [ foot dispatch dialogData ]
            ]
        ]



let showWaveSetupPopup maybeTitle (popupBody: MoreWaveSetup option ->ReactElement) maybeFoot extraStyle dispatch =
    fun _ (dialogData:PopupDialogData)->
        printfn "starting morewavesetup popup function"
        unclosablePopup maybeTitle (popupBody dialogData.WaveSetup) maybeFoot extraStyle dispatch
    |> ShowPopup |> dispatch


/// Body and foot are functions that take a string of text and produce a
/// reactElement. The meaning of the input string to those functions is the
/// content of PopupDialogText (i.e. in a dialog popup, the string is the
/// current value of the input box.).
let private dynamicClosablePopup title (body:PopupDialogData -> ReactElement) (foot: PopupDialogData -> ReactElement) (extraStyle: CSSProp list) (dispatch: Msg->Unit) =
    buildPopup title (fun _ -> body) (fun _ -> foot) (fun dispatch _ -> dispatch ClosePopup) extraStyle
    |> ShowPopup |> dispatch

/// As dynamicClosablePopup but accept functions of dispatch and return the popup function
let private dynamicClosablePopupFunc title body foot extraStyle =
        buildPopup title body foot (fun dispatch _ -> dispatch ClosePopup) extraStyle

/// Popup to track progress of some long operation. Progress is captured via two dialog integers, current and max number.
/// Typically the number is number of steps.
/// The popup display is controlled by model.PopupDialog integers. Progress model updates must change these.
let dynamicProgressPopupFunc title (cancel: (Msg -> Unit) -> Unit) =
    let body (dispatch:Msg->Unit) (dialog:PopupDialogData) =
        let n = Option.defaultValue 0 dialog.Int        
        Progress.progress
            [   Progress.Color IsSuccess
                Progress.Value n
                Progress.Max (int (Option.defaultValue 100L (dialog.Int2))) ]
            [ str $"{n}"]

    let foot (dispatch:Msg->Unit) (dialog:PopupDialogData) =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> 
                            cancel dispatch
                            dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
            ]
        ]
        
    buildPopup title body foot (fun dispatch _ -> dispatch ClosePopup) []

/// Create a popup and add it to the page. Body and foot are static content.
/// Can be closed by the ClosePopup message.
let closablePopup title body foot extraStyle dispatch =
    dynamicClosablePopup title (fun _ -> body) (fun _ -> foot) extraStyle dispatch

/// As closablePopup but accept functions and return the popup function
/// Can be closed by the ClosePopup message.
let closablePopupFunc title (body:(Msg->Unit)->ReactElement) (foot:(Msg->Unit)->ReactElement) extraStyle  =
        dynamicClosablePopupFunc title (fun dispatch _ -> body dispatch) (fun dispatch _ -> foot dispatch) extraStyle

/// Create the body of a dialog Popup with only text.
let dialogPopupBodyOnlyText before placeholder dispatch =
    fun (dialogData : PopupDialogData) ->
        div [] [
            before dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with only an int.
let dialogPopupBodyOnlyInt beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]
/// Create the body of a dialog Popup with two ints.
let dialogPopupBodyTwoInts (beforeInt1,beforeInt2) (intDefault1,intDefault2) (width2:string) dispatch =

    let setPopupTwoInts (whichInt:IntMode, optText) =
        fun (n:int64) -> (Some n, whichInt, optText) |> SetPopupDialogTwoInts |> dispatch

    setPopupTwoInts (FirstInt,None) (int64 intDefault1)
    setPopupTwoInts (SecondInt, None) intDefault2 

    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt1 dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault1
                Input.OnChange (getIntEventValue >> int64 >> setPopupTwoInts (FirstInt,None))
            ]
            br []
            beforeInt2 dialogData
            br []
            Input.text [
                Input.Props [Style [Width width2]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault2
                Input.OnChange (fun ev ->
                    let text = getTextEventValue ev
                    let n = getInt64EventValue ev
                    setPopupTwoInts(SecondInt, Some text) n)
            ]
        ]

/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeText dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            br []
            br []
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
            br []
            br []
            beforeText dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]

        ]

let makeSourceMenu 
        (dialog: PopupDialogData)
        (dispatch: Msg -> Unit) =

    let popupKey =
        match dialog.MemorySetup with
        | Some(_,_, key,_) -> key
        | None -> 
            printfn "No memory setup"
            FromData

    let onSelect key  =
        let n1,n2, _,_ = getMemorySetup dialog 1
        printfn $"Select {key}"
        dispatch <| ModelType.SetPopupDialogMemorySetup (Some(n1,n2,key,None))

    let files =
        FilesIO.readFilesFromDirectoryWithExtn dialog.ProjectPath ".ram"
        |> List.map (FilesIO.removeExtn ".ram" >> Option.get)

    let inputValidate text =
         (text = "" || 
            List.exists ((=) text) files || 
            not (Seq.forall System.Char.IsLetterOrDigit (text)) || 
            not (System.Char.IsLetter (char text.[0])))
         |> not
 
    let fileEntryBox =
        let n1,n2, _,_ = getMemorySetup dialog 1
        match popupKey with
        | ToFile fName | ToFileBadName fName ->
            Input.text [
                Input.Props [Style [MarginLeft "2em"]]
                Input.DefaultValue fName
                Input.Placeholder "Enter file name"
                Input.Color (if inputValidate fName then IsSuccess else IsDanger)
                Input.OnChange 
                    (getTextEventValue 
                    >> (fun newName -> 
                            let newKey = if inputValidate newName then ToFile newName else ToFileBadName newName
                            dispatch <| ModelType.SetPopupDialogMemorySetup (Some(n1,n2, newKey,None) ) ) )
                ]
        | _ -> str ""

       
    let existingFiles =
        List.map FromFile files

    /// Create one item in the drop-down RAM source menu
    let printSource inList key =

        let hSpace width = span [Style [Display DisplayOptions.InlineBlock; Width width]] []

        let questionIcon = str "\u003F"

        let tip txt =
            span [
                    Style [Float FloatOptions.Right]
                    HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                    Tooltip.dataTooltip txt
                ]
                [
                    Text.span [
                        Modifiers [
                            Modifier.TextWeight TextWeight.Bold
                            Modifier.TextColor IsLight
                            Modifier.BackgroundColor IsPrimary]
                        Props [
                            Style [
                                Display DisplayOptions.InlineBlock
                                Width "50px"
                                TextAlign TextAlignOptions.Center]]
                ] [questionIcon] ]

        let (aWidth,dWidth,_,_) = getMemorySetup dialog 1

        let multiplyTip mType =
            tip ($"Dout = Addr[{aWidth-1}:{aWidth/2}] * Addr[{aWidth/2-1}:0]. \
            Multiplication is {mType}." + 
             (if dWidth < aWidth then 
                $"The result will be truncated to bits [{dWidth-1}:0] on Dout."
             else
                ""))

        match key with
        | FromData -> [str "Enter data later"]
        | SignedMultiplier -> [
            str "Signed multiply"
            hSpace "30px"
            multiplyTip "signed"
            ]
        | UnsignedMultiplier -> [
            str "Unsigned multiply "; 
            hSpace "30px"
            multiplyTip "unsigned"
            ]
        | ToFile _ | ToFileBadName _ -> // not needed - direct write from properties is better
            [ str "Enter data later - create a new file " ; if inList then str "" else fileEntryBox]
        | FromFile s -> [str $"{s}.ram"]

    let sources =
            [
                FromData
                SignedMultiplier
                UnsignedMultiplier
                //ToFileBadName ""
            ] @ existingFiles


    let isActiveFile key = 
        match popupKey, key with
        | ToFile _, ToFile _ -> true
        | ToFileBadName _,ToFileBadName _ -> true
        | popup, key -> key = popup

    let menuItem (key) =
        let react = printSource true key
        Menu.Item.li
            [ Menu.Item.IsActive (isActiveFile key)
              Menu.Item.OnClick (fun _ -> onSelect key) ] react 
    
    Dropdown.dropdown [ Dropdown.IsUp; Dropdown.IsHoverable; ]
        [ Dropdown.trigger [ ]
            [ Button.button [Button.Color IsPrimary; Button.IsLight] (printSource false popupKey) ]                                
          Dropdown.menu [Props [Style [Width "300px"] ]]
            [ Dropdown.content [Props [Style [ZIndex 1000]] ]
                [ Dropdown.Item.div [ ] [
                    Menu.menu []
                        [ Menu.list [] (List.map menuItem sources) ]
                    ] ] ] ] 
    

/// Create the body of a memory dialog popup: asks for AddressWidth and
/// WordWidth, two integers.
let dialogPopupBodyMemorySetup intDefault dispatch =

    //Some (4, intDefault, FromData, None) 
    //|> SetPopupDialogMemorySetup |> dispatch
    fun (dialogData : PopupDialogData) ->
        let setup =
            match dialogData.MemorySetup with 
            | None -> 
                let setup = getMemorySetup dialogData intDefault
                dispatch <| SetPopupDialogMemorySetup (Some setup)
                setup
            | Some setup ->
                setup
        // needed in case getMemorySetup has delivered default values not yet stored
        if dialogData.MemorySetup <> Some setup then
            dispatch <| SetPopupDialogMemorySetup (Some setup)
        let addressWidth, wordWidth, source, errorOpt = setup
        let dataSetupMess =
            match source with
            | FromData -> $"You will be able to set up memory content later from the component Properties menu"
            | FromFile x -> $"Memory content is fixed by the '{x}.ram' file in the project directory"
            | ToFile x -> $"You will be able to set up memory content later from the component Properties menu, \
                            it will be written to the '{x}.ram file in the project directory"
            | ToFileBadName x -> ""
            | _ -> "Memory initial data is determined by the requested multiplication"
        div [] [
            str $"How many bits should be used to address the data in memory?"
            br [];
            str <| sprintf "%d bits yield %d memory locations." addressWidth (pow2int64 addressWidth)
            br []; br []
            Input.number [
                Input.Props [Style [Width "60px"] ; AutoFocus true]
                Input.DefaultValue (sprintf "%d" addressWidth)
                Input.OnChange (getIntEventValue >> fun newAddrWidth ->
                    Some (newAddrWidth, wordWidth, source, None) 
                    |> SetPopupDialogMemorySetup |> dispatch
                )
            ]
            br []
            br []
            str "How many bits should each memory word contain?"
            br []; br [];
            Input.number [
                Input.Props [Style [Width "60px"]]
                Input.DefaultValue (sprintf "%d" wordWidth)
                Input.OnChange (getIntEventValue >> fun newWordWidth ->
                    Some (addressWidth, newWordWidth, source, None) 
                    |> SetPopupDialogMemorySetup |> dispatch
                )
            ]
            br []
            br []
            makeSourceMenu dialogData dispatch
            br []
            br []
            str dataSetupMess
            br []
            match errorOpt with
            | Some msg -> div [Style [Color "red"]] [ str msg]
            | _ -> br []

        ]

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
let dialogPopup title body buttonText buttonAction isDisabled dispatch =
    let foot =
        fun (dialogData : PopupDialogData) ->
            Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsLight
                            Button.OnClick (fun _ -> 
                                dispatch ClosePopup
                                dispatch FinishUICmd) //In case user presses cancel on 'rename sheet' popup
                        ] [ str "Cancel" ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Disabled (isDisabled dialogData)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> buttonAction dialogData)
                        ] [ str buttonText ]
                    ]
                ]
            ]
    dynamicClosablePopup title body foot [] dispatch

/// A static confirmation popup.
let confirmationPopup title body buttonText buttonAction dispatch =
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick buttonAction
                    ] [ str buttonText ]
                ]
            ]
        ]
    closablePopup title body foot [] dispatch

/// A static choice dialog popup returning the popup function
let choicePopupFunc 
        title 
        (body:(Msg->Unit)->ReactElement) 
        buttonTrueText 
        buttonFalseText 
        (buttonAction: bool -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit) =
    let foot dispatch =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (buttonAction false dispatch)
                    ] [ str buttonFalseText ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (buttonAction true dispatch)
                    ] [ str buttonTrueText ]
                ]
            ]
        ]
    closablePopupFunc title body foot []

/// A static choice dialog popup.
let choicePopup title (body:ReactElement) buttonTrueText buttonFalseText (buttonAction: bool -> Browser.Types.MouseEvent -> Unit) dispatch =
    let popup = choicePopupFunc title (fun _ -> body) buttonTrueText buttonFalseText (fun bool dispatch-> buttonAction bool)
    dispatch <| ShowPopup popup

/// Display popup, if any is present.
let viewPopup model dispatch=
    match model.PopupViewFunc with
    | None -> div [] []
    | Some popup -> popup dispatch model.PopupDialogData

//===============//
// Notifications //
//===============//

let errorNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color IsDanger
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]
let successNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color  Color.IsSuccess
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]

let errorPropsNotification text = errorNotification text ClosePropertiesNotification
let errorFilesNotification text  = errorNotification text CloseFilesNotification
let successSimulationNotification text = successNotification text CloseSimulationNotification
let successPropertiesNotification text = successNotification text ClosePropertiesNotification

let warningNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color IsWarning
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]

let warningPropsNotification text = warningNotification text ClosePropertiesNotification
let warningSimNotification text = warningNotification text CloseSimulationNotification

let viewNotifications model dispatch =
    let sheetNotifications =
        match model.Sheet.GetNotifications with
        | Some msg -> Some <| errorNotification msg CloseDiagramNotification
        | None -> None
            
    [ //model.Notifications.FromDiagram
      sheetNotifications
      model.Notifications.FromSimulation
      model.Notifications.FromFiles
      model.Notifications.FromMemoryEditor
      model.Notifications.FromProperties ]
    |> List.tryPick id
    |> function
    | Some notification -> notification dispatch
    | None -> div [] []

let viewInfoPopup dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br[]]
    let title = "ISSIE: Interactive Schematic Simulator and Integrated Editor"

    let about = div [] [
        makeH "Version"
        str Version.VersionString
        br []; br []
        makeH "Acknowledgments"
        str "ISSIE was created by Marco Selvatici (EIE 3rd year) as his BEng final year project. \
             The waveform viewer was created \
             by Edoardo Santi (EEE 3rd year) during Summer UROP work. The new schematic editor \
             was written as 2021 coursework by HLP students in EEE, \
             and particularly Team 4. The new editor was integrated and the application enhanced \
             by Jo Merrick (EIE 3rd year) for her BEng final year project."
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
        str "Issie designs are made of one or more sheets. Each sheet contains components and Input and Output Connectors. \
        If you have a single sheet that is your complete design. Otherwise any \
        sheet can include the hardware defined in another sheet by adding a 'custom component' \
        from the My Project section of the Catalog. \
        Multiple copies of other sheets can be added." 
        br[]; br[]
        str "The Simulation Tab is used mainly for combinational logic and simple clocked logic: \
        the top 'Waveforms >>' button works with clocked circuits and displays waveforms." 
        br[]; br[];
        str "In Issie all clocked components use the same clock signal Clk. \
        Clk connections are not shown: all clk ports are
        automatically connected together. In the waveforms active clock edges are indicated \
        by vertical line through all the waveforms that separate clock cycles. The clock is not shown."
        br[]  ; br[];  
        button 
            [OnClick <| openInBrowser "https://github.com/tomcl/ISSIE"] 
            [ str "See the Issie Github Repo for more information"]
        br[] ; br[] ]

    let keys = div [] [
        makeH "Keyboard shortcuts - also available on top menus"
        span [Style [FontStyle "Italic"]] [str "On Mac use Command instead of Ctrl."]
        ul [] [
            li [] [str "Save: Ctrl + S"]
            li [] [str "Select all: Ctrl + A"]
            li [] [str "Copy selected diagram items: Ctrl + C"]
            li [] [str "Paste diagram items: Ctrl + V"]
            li [] [str "Undo last diagram action: Ctrl + Z"]
            li [] [str "Redo last diagram action: Ctrl + Y"]
            li [] [str "Zoom application in: Ctrl + Shift + ="]
            li [] [str "Zoom application out: Ctrl + Shift + -"]
            li [] [str "Zoom canvas in/out: Ctrl + MouseWheel"]
            li [] [str "Zoom canvas in: Shift + ="]
            li [] [str "Zoom canvas out: Shift + -"]
            li [] [str "Zoom circuit to fit screen: Ctrl + W"]
        ] ]
    let body (dialogData:PopupDialogData) =
        
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
                    [ str "Keyboard Shortcuts" ] ]
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 2) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 2)) ]
                    [ str "Introduction" ] ] ]
            match tab with
            | Some 0 -> about
            | Some 1 -> keys
            | Some 2 -> intro
            | _ -> dispatch <| SetPopupDialogInt (Some 0)
        ]

    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 800] dispatch

