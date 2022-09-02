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



//====================================================================//

open Fulma
open Fulma.Extensions.Wikiki
open VerilogTypes
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Browser.Types
open ElectronAPI

open JSHelpers
open Helpers
open ModelType
open CommonTypes
open EEExtensions
open Sheet.SheetInterface
open CodeEditorHelpers
open System


/////////////////////   CODE EDITOR React Component  /////////////////////////
// Basically: a code editor wrapped in Stateful React Component
// React Component is needed to keep track of the state -> mouse keeps track of its location on value change
// codeEditor react element is used to store the code into PopupDialogData.Code
// Code Highlighting is done automatically by PrismJS 

importSideEffects "../VerilogComponent/prism.css"

let inline codeEditor (props : CodeEditorProps list) (elems : ReactElement list) : ReactElement =
    ofImport "default" "react-simple-code-editor" (keyValueList CaseRules.LowerFirst props) elems

importSideEffects "prismjs/components/prism-clike"

type CERSCProps =
    { CurrentCode : string
      ReplaceCode : string Option
      Dispatch : (Msg -> unit)
      DialogData: PopupDialogData
      Compile: (PopupDialogData -> Unit)}
    //   Compile : (string -> PopupDialogData -> ReactElement)}
type CERSCState = { code: string; }

type CodeEditorReactStatefulComponent (props) =
    inherit Component<CERSCProps, CERSCState> (props)
    
    do base.setInitState({ code = "module NAME();\n  // Write your IO Port Declarations here\n  \n  \n  \n  // Write your Assignments here\n  \n  \n  \nendmodule" })


    // override this.shouldComponentUpdate (nextProps,nextState) =

    override this.componentDidUpdate (prevProps,prevState) =
        match (props.ReplaceCode <> None && prevProps.ReplaceCode = None) with
        |true -> 
            this.setState(fun s _-> {s with code = Option.get props.ReplaceCode} )
            props.Dispatch <| SetPopupDialogCode (props.ReplaceCode)
            props.Compile {props.DialogData with VerilogCode=props.ReplaceCode}
        |false -> ()

    override this.componentDidMount () =
        match props.ReplaceCode <> None with
        |true -> this.setState(fun s _-> {s with code = Option.get props.ReplaceCode} )
        |false -> ()

    override this.render () =
            div [Style [Position PositionOptions.Relative; CSSProp.Left "35px";Height "100%";]]
                [
                    codeEditor [
                        CodeEditorProps.Placeholder ("Start Writing your Verilog Code here..."); 
                        CodeEditorProps.Value ((sprintf "%s" this.state.code)); 
                        CodeEditorProps.Padding 5
                        OnValueChange (fun txt -> 
                            (this.setState (fun s p -> {s with code=txt}))
                            props.Dispatch <| SetPopupDialogCode (Some txt)
                            props.Compile {props.DialogData with VerilogCode=Some txt}
                        )             
                        Highlight (fun code -> Prism.highlight(code,language));]
                        []
                ]
//////////////////////////////////////////////////////////////////////


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
    | _ -> sprintf "%s(%d:%d)" (text'.ToUpper()) (width-1) 0
   

let formatLabelFromType compType (text:string) =
    let text' = extractLabelBase text
    match compType with
    | Input1 (1, _) | Output 1 -> text'
    | _ -> text'


let formatLabel (comp:Component) (text:string) =
    formatLabelFromType comp.Type (text:string)

// TODO: removed formatLabel for now
let setComponentLabel model (sheetDispatch) (comp:Component) (text:string) =
    // let label = formatLabel comp text
    let label = text.ToUpper() // TODO

    model.Sheet.ChangeLabel sheetDispatch (ComponentId comp.Id) label
    //model.Diagram.EditComponentLabel comp.Id label



//========//
// Popups //
//========//

let preventDefault (e: Browser.Types.ClipboardEvent) = e.preventDefault()

let getText (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text

let getCode (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.VerilogCode


let getErrorList (dialogData : PopupDialogData) =
    dialogData.VerilogErrors

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

/// Body and foot are functions that take a string of text and produce a
/// reactElement. The meaning of the input string to those functions is the
/// content of PopupDialogText (i.e. in a dialog popup, the string is the
/// current value of the input box.).
let dynamicClosablePopup title (body:PopupDialogData -> ReactElement) (foot: PopupDialogData -> ReactElement) (extraStyle: CSSProp list) (dispatch: Msg->Unit) =
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
        let goodLabel =
                getText dialogData
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> true | Some ch -> false | None -> true
        div [] [
            before dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]            

        ]

/// Create the body of a dialog Popup with only text.
let dialogPopupBodyOnlyTextWithDefaultValue before placeholder currDescr dispatch =
    fun (dialogData : PopupDialogData) ->
        let goodLabel =
                getText dialogData
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> true | Some ch -> false | None -> true
        let defaultValue = Option.defaultValue "" currDescr
        div [] [
            before dialogData
            Input.text [
                Input.DefaultValue defaultValue
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]            

        ]


/// Create the body of a Verilog Editor Popup.
let dialogVerilogCompBody before moduleName errorDiv errorList showExtraErrors codeToAdd compileButton addButton dispatch =
    fun (dialogData : PopupDialogData) ->
        let code = getCode dialogData
        let linesNo = code |> String.filter (fun ch->ch='\n') |> String.length
        let goodLabel =
                (Option.defaultValue "" moduleName)
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> true | Some ch -> false | None -> true
        
        let renderCERSC =
            ofType<CodeEditorReactStatefulComponent,_,_> {CurrentCode=code; ReplaceCode=codeToAdd; Dispatch=dispatch; DialogData=dialogData;Compile=compileButton} 
        
        let codeEditorWidth, errorWidth, hide = if showExtraErrors then "56%","38%",false else "96%","0%",true 

        let i = getHeight
        let editorheigth = ((float i)/2.9 |> int |> string)  + "px"
        let tableHeigth = ((float i)/1.9 |> int |> string)  + "px"
        div [Style [Width "100%"; Height "100%";Display DisplayOptions.Flex; FlexDirection FlexDirection.Row; JustifyContent "flex-start"]] [
            div [Style [Flex "2%"; Height "100%";]] []
            div [Style [Flex codeEditorWidth; Height "100%";]] [
                before dialogData
                Input.text [
                    Input.Props [AutoFocus true; SpellCheck false]
                    Input.Placeholder "Component name (equal to module name)"
                    Input.Value (Option.defaultValue "" moduleName)
                    Input.Disabled true
                    Input.OnChange (
                        getTextEventValue >> Some >> SetPopupDialogText >> dispatch
                        )
                ]
                span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]
                br []
                br []
                br []
                div [ Style [Position PositionOptions.Relative;]] [
                    p [] [b [] [str "Verilog Code:"]]
                    infoHoverableElement
                ]
                br []
                //BrowserWindowConstructorOptions
                div [ Style [Position PositionOptions.Relative; MinHeight "0px"; MaxHeight editorheigth; FontFamily ("ui-monospace,SFMono-Regular,SF Mono,Consolas,Liberation Mono,Menlo,monospace"); FontSize 16; BackgroundColor "#f5f5f5"; OutlineStyle "solid"; OutlineColor "Blue";OverflowY OverflowOptions.Auto;OverflowX OverflowOptions.Hidden]] 
                        [
                        getLineCounterDiv linesNo
                        errorDiv
                        renderCERSC Seq.empty
                        ]
                ]
            div [Style [Flex "2%"];] []
            div [Style [Flex "2%"]; Hidden hide] []
            div [Style [Position PositionOptions.Relative; Flex errorWidth; MinHeight "0px"; MaxHeight tableHeigth; OverflowY OverflowOptions.Auto;OverflowX OverflowOptions.Hidden]; Hidden hide] 
                [
                getErrorTable errorList (addButton dialogData)
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
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
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
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault1
                Input.OnChange (getIntEventValue >> int64 >> setPopupTwoInts (FirstInt,None))
            ]
            br []
            beforeInt2 dialogData
            br []
            Input.text [
                Input.Props [OnPaste preventDefault; Style [Width width2]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault2
                Input.OnChange (fun ev ->
                    let text = getTextEventValue ev
                    let n = getInt64EventValue ev
                    setPopupTwoInts(SecondInt, Some text) n)
            ]
        ]

/// Create the body of a dialog Popup with text and two ints.
let dialogPopupBodyTextAndTwoInts (beforeText, textPlaceholder) (beforeInt1,beforeInt2) (intDefault1,intDefault2) dispatch =

    let setPopupTwoInts (whichInt:IntMode, optText) =
        fun (n:int64) -> (Some n, whichInt, optText) |> SetPopupDialogTwoInts |> dispatch

    setPopupTwoInts (FirstInt,None) (int64 intDefault1)
    setPopupTwoInts (SecondInt, None) intDefault2 

    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeText dialogData
            br []
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder textPlaceholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]

            beforeInt1 dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault1
                Input.OnChange (getIntEventValue >> int64 >> setPopupTwoInts (FirstInt,None))
            ]
            br []
            beforeInt2 dialogData
            br []
            Input.text [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
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
        let goodLabel =
                getText dialogData
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> true | Some ch -> false | None -> true
        div [] [
            beforeText dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]            
            br []
            br []
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]]
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
                Input.Props [OnPaste preventDefault; Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
            br []
            br []
            beforeText dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
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
            not (System.Char.IsLetter (char text[0])))
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
                Input.Props [OnPaste preventDefault; Style [Width "60px"] ; AutoFocus true]
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
                Input.Props [OnPaste preventDefault; Style [Width "60px"]]
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
let dialogPopup title body buttonText buttonAction isDisabled extraStyle dispatch =
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
    dynamicClosablePopup title body foot extraStyle dispatch

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
let dialogVerilogPopup title body saveUpdateText noErrors showingExtraInfo saveButtonAction moreInfoButton isDisabled extraStyle dispatch =
    let foot =
        fun (dialogData : PopupDialogData) ->
            let compileButtonText = 
                if noErrors then "Compiled" 
                elif showingExtraInfo then "Hide Info"
                else "More Info" 
            let compileButtonColor = if noErrors then IsInfo else IsDanger
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
                            Button.Disabled (noErrors)
                            Button.Color compileButtonColor
                            Button.OnClick (fun _ -> moreInfoButton dialogData)
                        ] [ str compileButtonText ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Disabled (isDisabled dialogData)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> saveButtonAction dialogData)
                        ] [ str saveUpdateText ]
                    ]
                ]
            ]
    dynamicClosablePopup title body foot extraStyle dispatch

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

/// a popup diaplysing a progress bar
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
    buildPopup pp.Title body foot close extraStyle dispatch model.PopupDialogData
    

let simulationLegend (model:Model) (pp: PopupProgress) =
    match model.CurrentStepSimulationStep with
    | Some (Ok simData) ->
        let speed = pp.Speed
        str <| $"simulation speed: %6.0f{speed} component-clocks / ms"
    | _ -> div [] []


/// Display popup, if any is present.
/// A progress popup, if present, overrides any other active popup.
let viewPopup model dispatch =
    match model.PopupDialogData.Progress, model.PopupViewFunc with
    | None, None -> div [] []
    | Some amount, _ ->
        progressPopup simulationLegend model dispatch
    | None, Some popup -> popup dispatch model.PopupDialogData 



let viewInfoPopup dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br []]
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]

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
        sheet can include as a single component the hardware defined in another sheet by adding a 'custom component' \
        from the My Project section of the Catalog. \
        Multiple copies of other sheets can be added in this way. \
        Top-level sheets which are not used as subsheets are bolded on the sheet menu." 
        br []; br []
        str "Issie has two types of simulation: The Simulation Tab is used mainly for combinational logic and simple clocked logic: \
        the top 'Waveforms >>' button works with clocked circuits and displays waveforms. Use whichever works for you." 
        br []; br [];
        str "In Issie all clocked components use the same clock signal Clk. \
        Clk connections are not shown: all Clk ports are
        automatically connected together. In the waveform display active clock edges, 1 per clock cycle, are indicated \
        by vertical lines through the waveforms. The clock waveform has two edges for each clock cycle and is not shown."
        br []  ; br [];  
        button 
            [OnClick <| openInBrowser "https://github.com/tomcl/ISSIE"] 
            [ str "See the Issie Github Repo for more information"]
        br [] ; br [] ]

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
        makeH "Keyboard & mouse gesture shortcuts - also available on top menus"
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
            li [] [tSpan "Zoom canvas in: " ; keyOf2  "Shift" "="]
            li [] [tSpan "Zoom canvas out: " ; keyOf2  "Shift" "-"; rule]
            li [] [tSpan "Zoom circuit to fit screen: " ; keyOf2  "Ctrl" "W"]
            li [] [tSpan "Scroll (mouse): " ; keyOf2 "Shift" "Left-Click"; bSpan " on canvas and drag"]
            li [] [tSpan "Scroll (touch-pad): " ; bSpan "Two-finger scrolling on touchpad"]
            li [] [tSpan "Scroll (touch-screen): " ; bSpan "One-finger drag on screen"]
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
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 2) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 2)) ]
                    [ str "Introduction" ] ]  
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 1) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 1)) ]
                    [ str "Keyboard Shortcuts" ] ]
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 3) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 3)) ]
                    [ str "Bug Reports" ] ] ]

            match tab with
            | Some 0 -> about
            | Some 1 -> keys
            | Some 2 -> intro
            | Some 3 -> bugReport
            | _ -> dispatch <| SetPopupDialogInt (Some 0)
        ]

    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 800] dispatch

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

    let title = "How to Use the Waveform Simulator"

    let waveInfo = div [] [
        makeH "Wave and RAM Selection"
        str "Unlike the step simulator, the waveform simulator can view any port of any component on any sheet in the current design. There is redundancy: \
            one signal will normally have multiple components and ports connected."
        br []
        str "Use the Wave Select window to change which waveforms are viewed. The filter box allows components or ports to be selected by any part of name. \
            You can select or deselect ports, or groups of ports using the tick-boxes and expanding groups and components to get ports."
                    
        br [];
        str "You can view any number of waveforms. It is good practice to edit your waveforms \
            and keep only the ones you need at any time. This will also make it easier to find waveforms. \
            The waveforms you view can be changed at any time.";
        br []; 
        str "RAMs and ROMs can be viewed showing the memory contents in the current (cursor) cycle. The display also shows when memory locations \
            are written and read."
               
        br []; br [];

        makeH "Waveforms"
        str "If you hover the mouse over waveform names in the viewer the component and connection of the waveform will be highlighted. You can change sheet \
             to find a component. \
             Waveforms can be reordered by dragging names, and deleted using the delete icon to the right of the name."
        br []
        str "Use cursor and zoom controls at any time to show which cycles to display. This setting will be preserved from one simulation to the next. \
             The waveform simulator uses a cursor to select the cycle currently viewed. The cursor is greayed and can be moved by clicking the waveforms \
             or altering the number in the cursor box, or clicking arrows."
        br []; br [];
       

        makeH "Miscellaneous"

        str "During a simulation you can move to any sheet and view or edit the design. If the design changes a button will appear allowing you to simulate \
             the newer design, this will work even if you have edited a subsheet. You can move the grey bar to give the waveforms more or less room."
        br []
        str "You can set default values for inputs in properties boxes. The main sheet inputs to the simulation are given these values throughout the simulation. \
            Components can be edited during a simulation and the new values will appear when the simulation is refreshed."
        br []
        str "The waveform radix can be changed. When waveforms are too small to fit binary this will be automatically changed to hex: when there is still \
               not enough room in waveforms, numric values can still be viewed using the cursor and the righthand panel."
        ]

   
    let body (dialogData:PopupDialogData) =
        waveInfo
    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 1000] dispatch