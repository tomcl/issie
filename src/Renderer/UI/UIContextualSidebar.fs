module UIContextualSideBar
open EEExtensions
open Fulma

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
open DrawModelType
open Browser
open PopupHelpers





/// A button on a contextual sidebar
/// Buttons take in a model and return a message
type SidebarButton = {
    ButtonClassNames: string; // for colours with fulma
    ButtonText: string; // for the text on the button
    ButtonAction: ModelType.Model -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit

}


type SidebarOptions = {
    ExtraStyle: CSSProp list;
    TitleText: string;
    SideBarButtons: SidebarButton list;
    Cancellable: bool;
}

/// Constructs a button component based on SidebarButton information
let createButton buttonInfo (dispatch: Msg -> Unit) (model: ModelType.Model)  : ReactElement =
    Button.button [
        Button.Option.Props[ClassName ("button " + buttonInfo.ButtonClassNames);];
        Button.Props[Style [ Margin "3px" ]];
        Button.OnClick (buttonInfo.ButtonAction model dispatch)
    ] [ str buttonInfo.ButtonText ]

/// CSS for the sidebar.
// It has absolute position so it can be placed on top of the rightbar. CSSProp.Left set to 2px so the dividerbar is visible
// PaddingTop set to 20px to look like it covers the tabbing bar
let sidebarDivCSS = [Position PositionOptions.Relative; ZIndex 100; Background "white"; Width "100%"; Padding "20px 0"; OverflowY OverflowOptions.Auto]

// let mutable sidebarDiv:Types.Element option = None

/// A sidebar with a title and a list of buttons, with a dynamic body
/// Creates the sidebar with a dynamic body
let buildSidebar (options: SidebarOptions) (body: (ModelType.Msg -> Unit) -> ModelType.Model ->  ReactElement) =
    fun (dispatch: Msg -> Unit) (model: ModelType.Model) ->
        let buttons = options.SideBarButtons |> List.map (fun buttonInfo -> createButton buttonInfo dispatch model )
        let maybeCancel =
            if options.Cancellable then
                // Some (Button.button [
                //     Button.Option.Props[ClassName "button is-light"];
                //     Button.Props[Style [ Margin "0px 3px"; Flex 1 ]];
                //     Button.Props[OnClick (fun _ -> dispatch (CloseContextualSidebar)) ]
                // ] [ str "Close" ])
                Some (Delete.delete [
                    Delete.Option.Props[OnClick (fun _ -> dispatch (CloseContextualSidebar)) ]
                ] [] )
            else None


        div [  Style (options.ExtraStyle @ sidebarDivCSS);] [
        div [ Style [Margin "5px 20px"]] [

            div [ Style [ MarginBottom "10px"; Display DisplayOptions.Flex; FlexDirection "row-reverse"] ] (maybeCancel |> Option.toList)
            Heading.h4 [] [ str options.TitleText ]
            div [ Style [ Margin "15px 0" ]] [ body dispatch model ]
            div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; FlexDirection FlexDirection.Row ] ] [
                div [ Style [  ] ] buttons
            ]
        ]
        ]

/// A react element that can be added to a sidebar body, which outputs an input text box that modifies the model's ContextualSidebarDialogData.Text
let textDialogSidebarElement (placeholder: string) (defaultVal: string option) (before: ContextualSidebarDialogData -> ReactElement) dispatch =
    fun (model : ModelType.Model) ->
    let dialogData = model.ContextualSidebarDialogData
    let inputValue =
        dialogData.Text
        |> Option.defaultValue (defaultVal |> Option.defaultValue "")
        |> string

    dispatch (SetContextualSidebarDialogText (Some inputValue))




    div [] [
        before dialogData
        div [Style [Margin "10px 0px"]] [
        Input.text [
            Input.Value inputValue
            Input.Props [AutoFocus true; SpellCheck false]
            Input.Placeholder placeholder
            Input.OnChange (getTextEventValue >> Some >>  SetContextualSidebarDialogText >> dispatch)
        ]]

    ]

/// A react element that can be added to a sidebar body, which outputs an input number box that modifies the model's ContextualSidebarDialogData.Int
let intDialogSidebarElement (defaultInt: int option) (before: ContextualSidebarDialogData -> ReactElement) dispatch =
    fun (model :  ModelType.Model) ->
    // 2147483647 is the max int size, so allow 8 digits max
    // (because we need a 1 digit padding for the next user entry, which gives us 9 digits, and 999,999,999 < 2,147,483,647. ).
    let overflowChecker i =
        if ( i < 0  || i > 99999999 ) then 0 else i
    let dialogData = model.ContextualSidebarDialogData
    let inputValue =
        dialogData.Int
        |> Option.defaultValue (defaultInt |> Option.defaultValue 0)
        // if the int is greater than the max int that F# can handle (aka overflow), set it to the max positive int
        |>  overflowChecker

    dispatch (SetContextualSidebarDialogInt (Some inputValue))

    div [] [
        before dialogData
        div [Style [Margin "10px 0px"]] [
        Input.number [
            Input.Value (string inputValue)
            Input.Props [AutoFocus true; SpellCheck false]
            Input.OnChange (getIntEventValue >> overflowChecker >> Some >>  SetContextualSidebarDialogInt >> dispatch)
        ]]

    ]

let boundedIntDialogSidebarElement(minBound: int) (maxBound: int) (defaultInt: int) (before: ContextualSidebarDialogData -> ReactElement) dispatch =
    // 2147483647 is the max int size, so allow 8 digits max
    // (because we need a 1 digit padding for the next user entry, which gives us 9 digits, and 999,999,999 < 2,147,483,647. ).
    fun (model :  ModelType.Model) ->
    let minBound = max 0 minBound
    let maxBound = min 999999999 maxBound
    let dialogData = model.ContextualSidebarDialogData
    let errText =
        model.PopupDialogData.Int
        |> Option.map (fun (i:int) ->
            if i < minBound || i > maxBound then
                $"Value must be between {minBound} and {maxBound}"
            else ""
        )
        |> Option.defaultValue ""
    let inputValue =
        dialogData.Int
        |> Option.defaultValue defaultInt
        |> fun i -> if i < minBound then minBound else if i > maxBound then maxBound else i

    dispatch (SetContextualSidebarDialogInt (Some inputValue))
    div [] [
        before dialogData
        br []
        span [Style [Color "red"]] [str errText]
        br []
        div [Style [Margin "10px 0px"]] [
        Input.number [
            Input.Value (string inputValue)
            Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
            Input.OnChange (getIntEventValue >> Some >> SetContextualSidebarDialogInt >> dispatch)
    ]]
    ]






/// To be called in MainView
let viewSidebar (model: ModelType.Model) dispatch : ReactElement option =
    match model.ContextualSidebarViewFunction with
    | Some contextualSidebar -> Some (contextualSidebar dispatch model)
    | None -> None








