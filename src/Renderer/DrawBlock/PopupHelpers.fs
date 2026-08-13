(*
    PopupHelpers.fs

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

Popus functions are called with (the latest) model and dispatch functions.

Make sure that if you need updated model contents you use the popup function argument, and not the
model argument to the popup build functions!

Popup state can use PopupDialogData which is shared between all popups but since only one popup
is present at any time that is OK. Or they can use model state directly.

*)

module PopupHelpers

open EEExtensions
open Fulma
open VerilogTypes
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open ElectronAPI
open JSHelpers
open Helpers
open ModelType
open CommonTypes
open CodeEditorHelpers
open System.Text.RegularExpressions

module Constants =
    let infoSignUnicode = "\U0001F6C8"


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

/// What the editor holds for a Verilog component that has not been written yet.
///
/// Named rather than written inline in setInitState below because the cancel path compares against
/// it: a new component whose code is still this has had nothing typed into it, so closing the
/// editor throws nothing away and need not ask. See CatalogueView.createVerilogPopup.
let verilogEditorTemplate =
    "module NAME(\n  // Write your IO Port Declarations here\n  \n);  \n  // Write your Assignments here\n  \n  \n  \nendmodule"

type CodeEditorReactStatefulComponent (props) =
    inherit Component<CERSCProps, CERSCState> (props)

    do base.setInitState({ code = verilogEditorTemplate })


    // override this.shouldComponentUpdate (nextProps,nextState) =

    override this.componentDidUpdate (prevProps,prevState) =
        match (props.ReplaceCode <> None && prevProps.ReplaceCode = None) with
        |true -> 
            this.setState(fun s _-> {code = Option.get props.ReplaceCode} )
            props.Dispatch <| SetPopupDialogCode (props.ReplaceCode)
            props.Compile {props.DialogData with VerilogCode=props.ReplaceCode}
        |false -> ()

    override this.componentDidMount () =
        match props.ReplaceCode <> None with
        |true -> this.setState(fun s _-> {code = Option.get props.ReplaceCode} )
        |false -> ()

    override this.render () =
            div [Style [Position PositionOptions.Relative; CSSProp.Left "35px";Height "100%";]]
                [
                    codeEditor [
                        CodeEditorProps.Placeholder ("Start Writing your Verilog Code here..."); 
                        CodeEditorProps.Value ((sprintf "%s" this.state.code)); 
                        CodeEditorProps.Padding 5
                        OnValueChange (fun txt -> 
                            (this.setState (fun s p -> {code=txt}))
                            props.Dispatch <| SetPopupDialogCode (Some txt)
                            props.Compile {props.DialogData with VerilogCode=Some txt}
                        )             
                        Highlight (fun code -> Prism.highlight(code,language));]
                        []
                ]



//---------------------------------------------------------------------------------------------//
//--------------------------------------Low-level HELPERS--------------------------------------//
//---------------------------------------------------------------------------------------------//

let openInBrowser url =
    (fun _ -> Bridge.openExternal url)

//------------------------------Popup-specific helper functions--------------------------------//

type DynamicElement = (Msg -> unit) -> Model -> ReactElement
type DynamicAction = (Msg -> unit) -> Model -> unit

let preventDefault (e: Browser.Types.ClipboardEvent) = e.preventDefault()

let getText (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text
let getText2 (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text2
let getImportDecisions (dialogData : PopupDialogData) =
    dialogData.ImportDecisions

let getCode (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.VerilogCode


let getErrorList (dialogData : PopupDialogData) =
    dialogData.VerilogErrors

let getInt (dialogData : PopupDialogData) =
    Option.defaultValue 1 dialogData.Int

let getInt2 (dialogData : PopupDialogData) : bigint =
    Option.defaultValue 0I dialogData.Int2

let getIntList (dialogData : PopupDialogData) (numInputsDefault : int ) (widthDefault)=
    Option.defaultValue [for _ in 1..numInputsDefault -> widthDefault] dialogData.IntList
let getIntList2 (dialogData : PopupDialogData) (numInputsDefault : int ) (lsbDefault)=
    Option.defaultValue [for _ in 1..numInputsDefault -> lsbDefault] dialogData.IntList2

let getMemorySetup (dialogData : PopupDialogData) wordWidthDefault =
    Option.defaultValue (4,wordWidthDefault,FromData,None) dialogData.MemorySetup

let getMemoryEditor (model: Model) =
    Option.defaultValue
        { Address = None; OnlyDiff = false; NumberBase = Hex ; Start = 0I}
        model.PopupDialogData.MemoryEditorData

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
    fun _ model->
        let memoryEditorData = getMemoryEditor model
        unclosablePopup maybeTitle (body memoryEditorData) maybeFoot extraStyle dispatch
    |> ShowPopup |> dispatch

/// Base popup builder function, used by other popup generators
let buildPopup title body foot close extraStyle =
    fun (dispatch:Msg->Unit) (model : Model) ->
        Modal.modal [ Modal.IsActive true; Modal.CustomClass "modal1"; Modal.Props [Style [ZIndex 20000]]] [
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
                Modal.Card.body [Props [Style [ OverflowY OverflowOptions.Visible ;OverflowX OverflowOptions.Visible]]] [ body dispatch model ]
                Modal.Card.foot [] [ foot dispatch model ]
            ]
        ]



/// Body and foot are functions that take a string of text and produce a
/// reactElement. The meaning of the input string to those functions is the
/// content of PopupDialogText (i.e. in a dialog popup, the string is the
/// current value of the input box.).
let dynamicClosablePopup title (body:Model -> ReactElement) (foot: Model -> ReactElement) (extraStyle: CSSProp list) (dispatch: Msg->Unit) =
    buildPopup title (fun _ -> body) (fun _ -> foot) (fun dispatch _ -> dispatch ClosePopup) extraStyle
    |> ShowPopup |> dispatch

/// As dynamicClosablePopup but accept functions of dispatch and return the popup function
let dynamicClosablePopupFunc title body foot extraStyle =
        buildPopup title body foot (fun dispatch _ -> dispatch ClosePopup) extraStyle

/// Popup to track progress of some long operation. Progress is captured via two dialog integers, current and max number.
/// Typically the number is number of steps.
/// The popup display is controlled by model.PopupDialog integers. Progress model updates must change these.
let dynamicProgressPopupFunc title (cancel: (Msg -> Unit) -> Unit) =
    let body (dispatch:Msg->Unit) (model: Model) =
        let dialog = model.PopupDialogData
        let n = Option.defaultValue 0 dialog.Int        
        Progress.progress
            [   Progress.Color IsSuccess
                Progress.Value n
                Progress.Max (int (Option.defaultValue 100I (dialog.Int2))) ]
            [ str $"{n}"]

    let foot (dispatch:Msg->Unit) (model:Model) =
        let dialog = model.PopupDialogData
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
    fun (model : Model) ->
        let dialogData = model.PopupDialogData
        let goodLabel =
                getText dialogData
                |> (fun s -> String.startsWithLetter s || s = "")
        div [] [
            before dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]            

        ]

/// Create the body of a dialog Popup with only text for sheet description (can have an initial value + allow empty).
let dialogPopupBodyOnlyTextWithDefaultValue before placeholder currDescr dispatch =
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let defaultValue = Option.defaultValue "" currDescr
        div [] [
            before dialogData
            Input.text [
                Input.DefaultValue defaultValue
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]


/// Create the body of a Verilog Editor Popup.
let dialogVerilogCompBody before moduleName errorDiv errorList showExtraErrors codeToAdd compileButton addButton dispatch =
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let code = getCode dialogData
        let linesNo = 
            match code,codeToAdd with
            | "", Some c -> c |> String.filter (fun ch->ch='\n') |> String.length
            | _ -> code |> String.filter (fun ch->ch='\n') |> String.length
        let goodLabel =
                (Option.defaultValue "" moduleName)
                |> (fun s -> String.startsWithLetter s || s = "")
        
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
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with only an int whose value is bounded
let dialogPopupBodyOnlyBoundedInt beforeInt intDefault minBound maxBound dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let errText =
            model.PopupDialogData.Int
            |> Option.map (fun i ->
                if i < minBound || i > maxBound then
                    sprintf $"Must have between {minBound} and {maxBound} outputs"
                else
                    "")
            |> Option.defaultValue ""
        div [] [
            beforeInt dialogData
            br []
            span
                [Style [Color Red; ]]
                [str errText]
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]

let dialogPopupBodyNInts beforeInt numOutputsDefault intDefault maxNumOutputs dispatch =
    numOutputsDefault |> Some |> SetPopupDialogInt |> dispatch
    [for _ in 1..numOutputsDefault -> intDefault] |> Some |> SetPopupDialogIntList |> dispatch
    [for x in 1..numOutputsDefault -> x-1] |> Some |> SetPopupDialogIntList2 |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let errText =
            model.PopupDialogData.Int
            |> Option.map (fun i ->
                if i < 2 || i > maxNumOutputs then
                    sprintf $"Must have between 2 and {maxNumOutputs} outputs"
                else
                    "")
            |> Option.defaultValue ""
        let newInt = 
            match getInt dialogData with
            | n when n < 2 -> 
                match dialogData.IntList with
                | None -> numOutputsDefault
                | Some widthList -> widthList.Length
            | n -> n
        
        let setupWidthList =
            match dialogData.IntList with 
            | None -> 
                let setup = getIntList dialogData numOutputsDefault intDefault
                dispatch <| SetPopupDialogIntList (Some setup)
                setup
            | Some outputWidthList -> 
                let newNumInputs = newInt
                match outputWidthList.Length with
                | n when n > newNumInputs -> 
                    outputWidthList[..(newNumInputs-1)]
                | n when n < newNumInputs ->
                    List.append outputWidthList (List.init (newNumInputs-n) (fun _ -> 1)) 
                | _ -> outputWidthList
        
        let setupLSBList =
            match dialogData.IntList2 with 
            | None -> 
                let setup = getIntList2 dialogData numOutputsDefault 0
                dispatch <| SetPopupDialogIntList2 (Some setup)
                setup
            | Some lsbList -> 
                let newNumInputs = newInt
                match lsbList.Length with
                | n when n > newNumInputs -> 
                    lsbList[..(newNumInputs-1)]
                | n when n < newNumInputs ->
                    match dialogData.IntList with 
                    | None -> failwith "No width list in dialogData"
                    | Some widths -> 
                        let msbs = List.map2 (fun lsb width -> lsb + width - 1) lsbList widths
                        List.append lsbList (List.init (newNumInputs-n) (fun x -> x + (List.max msbs) + 1)) 
                | _ -> lsbList
        
        if dialogData.IntList <> Some setupWidthList then
            dispatch <| SetPopupDialogIntList (Some setupWidthList)
        
        if dialogData.IntList2 <> Some setupLSBList then
            dispatch <| SetPopupDialogIntList2 (Some setupLSBList)

        div [] [
            beforeInt dialogData
            br []
            span
                [Style [Color Red; ]]
                [str errText]
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" numOutputsDefault
                Input.OnChange (
                    getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
            br []
            br []
            str $"What is the width and least significant bit (LSB) number of each output?"
            br []; br [];
            div [Style [Display DisplayOptions.Flex;]] [
                Label.label [Label.Props [Style [MarginLeft "120px"; MarginRight "15px"]]] [str "Width"]
                Label.label [Label.Props [Style [MarginLeft "15px"]]] [str "LSB"]
            ]
            List.mapi2 (fun index (defaultWidthValue : int) (defaultLSBValue : int) ->
                div [Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center]] [
                    label [Style [Width "105px"; MarginRight "10px";]] [str (sprintf "Output Port %d:" index)] 
                    Input.number [
                        Input.Props [OnPaste preventDefault; Style [Width "60px"]; ]
                        Input.DefaultValue <| sprintf "%d" defaultWidthValue
                        Input.OnChange (
                            let setWidth = 
                                fun newWidth -> 
                                    setupWidthList
                                    |> List.mapi (fun i x -> if i = index then newWidth else x)
                            getIntEventValue >> setWidth >> Some >> SetPopupDialogIntList >> dispatch)
                    ]
                    Input.number [
                        Input.Props [OnPaste preventDefault; Style [Width "60px"; MarginLeft "10px"]; ]
                        Input.DefaultValue <| sprintf "%d" defaultLSBValue
                        Input.OnChange (
                            let setLSB = 
                                fun newLSB -> 
                                    setupLSBList
                                    |> List.mapi (fun i x -> if i = index then newLSB else x)
                            getIntEventValue >> setLSB >> Some >> SetPopupDialogIntList2 >> dispatch)
                    ]
                    br []
                    hr []
                    br []
                ]
                ) setupWidthList setupLSBList
            |> div [] 
            br []
            br []

        ]
            
/// Create the body of a dialog Popup with two ints.
let dialogPopupBodyTwoInts (beforeInt1,beforeInt2) (intDefault1,intDefault2) (width2:string) dispatch =

    let setPopupTwoInts (whichInt:IntMode, optText) =
        fun (n:bigint) -> (Some n, whichInt, optText) |> SetPopupDialogTwoInts |> dispatch

    setPopupTwoInts (FirstInt,None) (intDefault1)
    setPopupTwoInts (SecondInt, None) (intDefault2)

    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        div [] [
            beforeInt1 dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%A" intDefault1
                Input.OnChange (getIntEventValue >> bigint >> setPopupTwoInts (FirstInt,None))
            ]
            br []
            beforeInt2 dialogData
            br []
            Input.text [
                Input.Props [OnPaste preventDefault; Style [Width width2]; AutoFocus true]
                Input.DefaultValue <| sprintf "%A" intDefault2
                Input.OnChange (fun ev ->
                    let text = getTextEventValue ev
                    let n = getBigintEventValue ev
                    setPopupTwoInts(SecondInt, Some text) n)
            ]
        ]

/// Create the body of a dialog Popup with text and two ints.
/// focus: which of the boxes has initial focus (= 1,2,3)
let dialogPopupBodyTextAndTwoInts (focus: int) (beforeText, textPlaceholder) (beforeInt1,beforeInt2) (intDefault1,intDefault2) dispatch =

    let setPopupTwoInts (whichInt:IntMode, optText) =
        fun (n:bigint) -> (Some n, whichInt, optText) |> SetPopupDialogTwoInts |> dispatch

    setPopupTwoInts (FirstInt,None) intDefault1
    setPopupTwoInts (SecondInt, None) intDefault2 

    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        div [] [
            beforeText dialogData
            br []
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus (focus = 1); SpellCheck false]
                Input.Placeholder textPlaceholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]

            beforeInt1 dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus (focus = 2)]
                Input.DefaultValue <| sprintf "%A" intDefault1
                Input.OnChange (getIntEventValue >> bigint >> setPopupTwoInts (FirstInt,None))
            ]
            br []
            beforeInt2 dialogData
            br []
            Input.text [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus (focus = 3)]
                Input.DefaultValue <| sprintf "%A" intDefault2
                Input.OnChange (fun ev ->
                    let text = getTextEventValue ev
                    let n = getBigintEventValue ev
                    setPopupTwoInts(SecondInt, Some text) n)
            ]
        ]

/// Create the body of a dialog Popup with two text fields, both required.
/// First goes to Text, second to Text2.
let dialogPopupBodyTwoTexts (beforeText1, placeholder1) (beforeText2, placeholder2) dispatch =
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        div [] [
            beforeText1 dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder1
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            br []
            br []
            beforeText2 dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; SpellCheck true]
                Input.Placeholder placeholder2
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText2 >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch =
    
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let goodLabelStart =
                getText dialogData
                |> (fun s -> String.startsWithLetter s || s = "")
        let goodLabelLetters =
                 getText dialogData
                 |> (fun s -> Regex.IsMatch(s, "^[a-zA-Z0-9]+$") || s = "")
        div [] [
            beforeText dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabelStart] [str "Name must start with a letter."]            
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden (goodLabelLetters || not goodLabelStart)] [str "Name can only contain letters and numbers."]  
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

/// A parameter's value, typed into a box.
///
/// A text box parsed as a bigint rather than an Input.number, because a parameter value is a
/// ParamInt and so may be larger than any int: a parameter that gives a wide Constant its value is
/// the case that needs it. The box is uncontrolled, so an unparseable entry stays on screen to be
/// corrected while Int2 goes to None, which is what the red text and the disabled button read.
let paramValueBox (valueDefault: ParameterTypes.ParamInt) dispatch =
    let onChange text =
        match System.Numerics.BigInteger.TryParse (text: string) with
        | true, value -> Some value
        | false, _ -> None
        |> SetPopupDialogInt2
        |> dispatch
    Input.text [
        Input.Props [OnPaste preventDefault; SpellCheck false; Style [Width "120px"]]
        Input.DefaultValue <| string valueDefault
        Input.OnChange (getTextEventValue >> onChange)
    ]

/// The red text under a parameter's value box: shown only once the box has been made unparseable,
/// since it opens holding a value that parses.
let paramValueError (dialogData: PopupDialogData) =
    span [Style [FontStyle "Italic"; Color "Red"]; Hidden dialogData.Int2.IsSome]
        [str "A property's value is a whole number."]

/// Create the body of a dialog Popup with a name, a compulsory description, and a value.
/// Used to declare a sheet parameter: the description is what a custom component instance of the
/// sheet shows the user when it asks them for a value, so a parameter cannot be declared without
/// one. Name goes to Text, description to Text2, value to Int2.
///
/// `nameIsTaken` is asked whether the sheet already declares a parameter of that name. Passed in
/// rather than worked out here, so that this file needs to know nothing about where a sheet keeps
/// its parameters.
let dialogPopupBodyTextDescriptionAndInt beforeText placeholder beforeDescription descriptionPlaceholder beforeInt intDefault nameIsTaken dispatch =

    intDefault |> Some |> SetPopupDialogInt2 |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let name = getText dialogData
        let goodLabelStart = String.startsWithLetter name || name = ""
        // ParameterTypes owns the rule, because it is also the parser's: a name this rejects
        // could be declared and then never written in an expression
        let goodLabelLetters = ParameterTypes.isValidParamName name || name = ""
        // Reported separately from the letters rule even though isValidParamName covers both: the
        // two are different problems needing different fixes, and "min can only contain letters
        // and numbers" is a sentence that helps nobody.
        let reserved = ParameterTypes.isReservedParamName name
        // Adding a parameter under a name the sheet already uses replaced the declaration and its
        // description, and rebound every instance to the new value, without saying anything.
        let taken = name <> "" && nameIsTaken name
        // an empty description is only flagged once the user has started naming the parameter,
        // so the popup does not open already showing an error
        let describedOrUnstarted = getText2 dialogData <> "" || name = ""
        div [] [
            beforeText dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabelStart] [str "Name must start with a letter."]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden (goodLabelLetters || reserved || not goodLabelStart)] [str "Name can only contain letters and numbers."]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden (not reserved)] [str $"{name} is a built-in function, so cannot be a property name."]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden (not taken)] [str $"This sheet already has a property called {name}. Edit that one to change its value."]
            br []
            br []
            beforeDescription dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; SpellCheck true]
                Input.Placeholder descriptionPlaceholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText2 >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden describedOrUnstarted] [str "A description is required: it is what users of this sheet read when asked for a value."]
            br []
            br []
            beforeInt dialogData
            br []
            paramValueBox intDefault dispatch
            paramValueError dialogData
        ]

/// Create the body of a dialog Popup with a compulsory description and a value, for editing an
/// existing sheet parameter whose name is fixed. The caller seeds Text2 with the current
/// description so that editing only the value leaves it intact.
///
/// `valueProblem` is asked what is wrong with the value now in the box, and its answer is shown.
/// The OK button is disabled when a slot of the sheet would break at the new value, and used to be
/// disabled in silence - leaving the user to guess which component objected and why, when the
/// constraint that failed carries a sentence written to explain exactly that.
let dialogPopupBodyDescriptionAndInt beforeDescription currentDescription beforeInt intDefault valueProblem dispatch =

    intDefault |> Some |> SetPopupDialogInt2 |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let problem = valueProblem model |> Option.defaultValue ""
        div [] [
            beforeDescription dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck true]
                Input.DefaultValue currentDescription
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText2 >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden (getText2 dialogData <> "")] [str "A description is required: it is what users of this sheet read when asked for a value."]
            br []
            br []
            beforeInt dialogData
            br []
            paramValueBox intDefault dispatch
            paramValueError dialogData
            p [Style [Color "Red"]] [str problem]
        ]

/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
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
            beforeText model
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]


/// Create the body of a memory dialog popup: asks for AddressWidth and
/// WordWidth, two integers.
let dialogPopupBodyMemorySetup intDefault dispatch =

    //Some (4, intDefault, FromData, None) 
    //|> SetPopupDialogMemorySetup |> dispatch
    fun (model: Model) ->
        let dialogData = model.PopupDialogData
        let setup =
            match dialogData.MemorySetup with 
            | None -> 
                let setup = getMemorySetup dialogData intDefault
                dispatch <| SetPopupDialogMemorySetup (Some setup)
                setup
            | Some (n1,n2,init, nameOpt) ->
                (n1,n2,FromData, None)
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
            str <| sprintf "%d bits yield %A memory locations." addressWidth (1I <<< addressWidth)
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
            str dataSetupMess
            br []
            match errorOpt with
            | Some msg -> div [Style [Color "red"]] [ str msg]
            | _ -> br []

        ]

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
///
/// Return confirms the dialog, as it does in any other dialog with one obvious action. It used to
/// reach KeyTypes.ScLeaveTextBox, whose job is to hand the keyboard back to the schematic: inside a
/// popup that blurred the box the user had just typed into and did nothing else, which read as the
/// entry having been thrown away. Swallowed here whether or not the action is available, so that
/// Return means one thing inside a dialog rather than confirming it or emptying it depending on
/// whether what has been typed so far is valid.
let dialogPopup title body buttonText buttonAction isDisabled extraStyle dispatch =
    /// The handler goes on a wrapper rather than on each input because a dialog body is built by
    /// its caller and may hold several boxes; keydown from any of them bubbles to here.
    let bodyWithReturn =
        fun (model: Model) ->
            div [ OnKeyDown(fun ev ->
                      if ev.key = "Enter" then
                          ev.preventDefault ()
                          // stops the document-level handler seeing it: see the comment above
                          ev.stopPropagation ()
                          if not (isDisabled model) then buttonAction model) ]
                [ body model ]
    let foot =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
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
                            Button.Disabled (isDisabled model)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> buttonAction model)
                        ] [ str buttonText ]
                    ]
                ]
            ]
    dynamicClosablePopup title bodyWithReturn foot extraStyle dispatch

// Used for refresh popup
let dialogPopupRefresh title body extraStyle dispatch =
    let foot =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
            Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsLight
                            Button.OnClick (fun _ -> 
                                dispatch ClosePopup
                                dispatch FinishUICmd) 
                        ] [ str "Cancel" ]
                    ]
                ]
            ]
    
    dynamicClosablePopup title body foot extraStyle dispatch

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
/// The Verilog editor. Unlike every other popup here it holds work that exists nowhere else until
/// it is saved, so leaving it is the one irrecoverable thing it can do.
///
/// `cancelAction` is therefore given all three ways out that are not Save - the Cancel button, the
/// X, and a click on the background - rather than each dispatching ClosePopup for itself. They
/// discard exactly the same edits, so a confirmation on one of them and not the others would be a
/// confirmation the user can miss by clicking somewhere else.
let dialogVerilogPopup title body saveUpdateText noErrors showingExtraInfo saveButtonAction moreInfoButton (cancelAction: PopupDialogData -> Unit) isDisabled extraStyle dispatch =
    /// buildPopup does not hand its close function a model, and what to do depends on what has been
    /// typed, so the current dialog data is fetched in a message.
    let close (dispatch: Msg -> Unit) =
        fun _ -> dispatch <| ExecFuncInMessage((fun m _ -> cancelAction m.PopupDialogData), dispatch)
    let foot =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
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
                            Button.OnClick (fun _ -> cancelAction dialogData)
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
    buildPopup title (fun _ -> body) (fun _ -> foot) close extraStyle
    |> ShowPopup
    |> dispatch

let staticButtonFoot buttonAction buttonText dispatch =
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


    

/// A static confirmation popup.
let confirmationPopup title buttonText body buttonAction dispatch =
    let foot = staticButtonFoot (fun _ -> buttonAction()) buttonText dispatch
    closablePopup title body foot [] dispatch

let dynamicConfirmationPopup title buttonText body buttonActionOpt dispatch =
    let buttonAction =
        buttonActionOpt
        |> Option.defaultValue (fun _ -> dispatch Msg.ClosePopup)
    let foot = staticButtonFoot (fun _ -> buttonAction()) buttonText dispatch
    dynamicClosablePopup title body (fun _ -> foot) [] dispatch

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


/// Base popup builder function, used by other popup generators: close function depends on (dynamic) model.
/// OK button is enabled based on return from body function
let newBuildPopup
    title
    (body: (Msg -> unit) -> Model -> ReactElement)
    (foot: DynamicElement)
    (close: DynamicAction)
    (extraStyle : CSSProp list) =
    fun (dispatch: Msg->Unit) (model : Model) ->
        Modal.modal [ Modal.IsActive true; Modal.CustomClass "modal1"; Modal.Props [Style [ZIndex 20000]]] [
            Modal.background [ Props [ OnClick (fun _ -> close dispatch model)]] []
            Modal.Card.card [ Props [
                Style ([
                    OverflowY OverflowOptions.Auto
                    OverflowX OverflowOptions.Visible
                    UserSelect UserSelectOptions.None
                    ] @ extraStyle)
                ] ] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick (fun _ -> close dispatch model) ] []
                ]
                Modal.Card.body [Props [Style [ OverflowY OverflowOptions.Visible ; OverflowX OverflowOptions.Visible]]] [ body dispatch model]
                Modal.Card.foot [] [ foot dispatch model ]
            ]
        ]


let newButtonFoot buttonAction buttonText closeAction enable: DynamicElement =
    fun dispatch model ->
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> closeAction dispatch model)
                    ] [ str "Cancel" ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.Disabled (enable model)
                        Button.OnClick (fun _ -> buttonAction dispatch model)
                    ] [ str buttonText ]
                ]
            ]
        ]

let newConfirmationPopup
        title
        (body : (Msg -> unit) -> Model -> ReactElement)
        (buttonActionOpt: DynamicAction option)
        (buttonEnable: Model -> bool)
        (close: DynamicAction)
            : DynamicElement =
    fun dispatch model ->
        let buttonAction: DynamicAction =
            buttonActionOpt
            |> Option.defaultValue (fun _ _  -> dispatch Msg.ClosePopup)
        let foot = newButtonFoot buttonAction "Ok" close buttonEnable
        newBuildPopup title body foot close [] dispatch model


