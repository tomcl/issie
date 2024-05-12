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
open DrawModelType
open DrawModelType.SheetT
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DiagramStyle
open BlockHelpers
open SymbolUpdate
open CommonTypes
open CodeEditorHelpers
open DrawModelType
open Browser
open PopupHelpers
open Groups
open Groups.ColourGenerator


//------------------- TypeDefs -------------------//

/// A button on a contextual sidebar
/// Buttons take in a model and return a message
type SidebarButton = {
    ButtonClassNames: string; // for colours with fulma
    ButtonText: string; // for the text on the button
    ButtonAction: ModelType.Model -> (ModelType.Msg->Unit) -> Browser.Types.MouseEvent -> Unit
}

/// A type that defines whether sidebar can be cancelleable or not.
// The cancellability of a sidebar is defined by being either statically true or false, but sometimes, we would like dynamic behaviour.
// For example, consider the developer mode's non-cancellable group-selection sidebar, mandating the user to choose a nonzero amount of components for a group. You can call this from the red "Test Choose Group Sidebar" in the developer mode.
// What happens if the user decides to delete all the components while the sidebar is open? Or what if there are even no components left to make into a group?
// Then the user is stuck, as the sidebar cannot be dismissed as there are no valid actions to take.
// We would like the sidebar to be cancellable in this case.
// Unlike popups, the user can still modify some parts of the sheet while the sidebar is open, so we need to be able to dynamically change the cancellability of the sidebar.`
type Cancellable =
    | Bool of bool
    | Func of (ModelType.Model -> bool)

let isCancellable (cancellable : Cancellable) (model:ModelType.Model) : bool=
    match cancellable with
    | Bool b -> b
    | Func f -> f model


type SidebarOptions = {
    ExtraStyle: CSSProp list;
    TitleText: string;
    SideBarButtons: SidebarButton list;
    Cancellable: Cancellable;
}


//------------------- Core Functions -------------------//
/// Constructs a button component based on SidebarButton information
let createButton buttonInfo (dispatch: ModelType.Msg -> Unit) (model: ModelType.Model)  : ReactElement =
    Button.button [
        Button.Option.Props[ClassName ("button " + buttonInfo.ButtonClassNames);];
        Button.Props[Style [ Margin "3px" ]];
        Button.OnClick (buttonInfo.ButtonAction model dispatch)
    ] [ str buttonInfo.ButtonText ]

/// CSS for the sidebar
let sidebarDivCSS = [Position PositionOptions.Relative; ZIndex 100; Background "white"; Width "100%"; Padding "20px 0"; OverflowY OverflowOptions.Auto]

// let mutable sidebarDiv:Types.Element option = None

/// A sidebar with a title and a list of buttons, with a dynamic body
/// Creates the sidebar with a dynamic body
let buildSidebar (sidebarOptions: SidebarOptions) (body: (ModelType.Msg -> Unit) -> ModelType.Model ->  ReactElement) =
    fun (dispatch: ModelType.Msg -> Unit) (model: ModelType.Model) ->
        let buttons = sidebarOptions.SideBarButtons |> List.map (fun buttonInfo -> createButton buttonInfo dispatch model )
        // will be an empty div if the sidebar is not cancellable
        let cancelButton =
            if (isCancellable sidebarOptions.Cancellable model) then

                (Delete.delete [
                    Delete.Option.Props[OnClick (fun _ -> dispatch (CloseContextualSidebar)) ]
                ] [] )
            else div [Style [Height "20px"; ]] []


        div [  Style (sidebarOptions.ExtraStyle @ sidebarDivCSS);] [
        div [ Style [Margin "5px 20px"]] [

            div [ Style [ MarginBottom "10px"; Display DisplayOptions.Flex; FlexDirection "row-reverse"] ] [cancelButton]
            Heading.h4 [] [ str sidebarOptions.TitleText ]
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


//------------------- High Level -------------------//
/// To be called in MainView
let viewSidebar (model: ModelType.Model) dispatch : ReactElement option =
    match model.ContextualSidebarViewFunction with
    | Some contextualSidebar -> Some (contextualSidebar dispatch model)
    | None -> None


//------------------- Helpers  -------------------//
// Duplicate of DeveloperModeHelpers's getComponentTypeDescrFromSym.
// Unfortunately not available due to compile order

/// A helper printing function that returns a string of the symbol's component type description
let getComponentTypeDescrFromSym (symbol : SymbolT.Symbol)  =
    match symbol.Component.Type with
    | Input1 _ -> "Input1"
    | Output _ -> "Output"
    | Viewer _ -> "Viewer"
    | IOLabel -> "IOLabel"
    | NotConnected -> "NotConnected"
    | BusCompare1 _ -> "BusCompare1"
    | BusSelection _ -> "BusSelection"
    | Constant1 _ -> "Constant1"
    | Not -> "Not"
    | Decode4 -> "Decode4"
    | GateN _ -> "GateN"
    | Mux2 -> "Mux2"
    | Mux4 -> "Mux4"
    | Mux8 -> "Mux8"
    | Demux2 -> "Demux2"
    | Demux4 -> "Demux4"
    | Demux8 -> "Demux8"
    | NbitsAdder _ -> "NbitsAdder"
    | NbitsAdderNoCin _ -> "NbitsAdderNoCin"
    | NbitsAdderNoCout _ -> "NbitsAdderNoCout"
    | NbitsAdderNoCinCout _ -> "NbitsAdderNoCinCout"
    | NbitsXor _ -> "NbitsXor"
    | NbitsAnd _ -> "NbitsAnd"
    | NbitsNot _ -> "NbitsNot"
    | NbitsOr _ -> "NbitsOr"
    | NbitSpreader _ -> "NbitSpreader"
    | Custom customDetails -> $"Custom {customDetails.Name.ToUpper()}"
    | MergeWires -> "MergeWires"
    | SplitWire _ -> "SplitWire"
    | MergeN _ -> "MergeN"
    | SplitN _ -> "SplitN"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | Register _ -> "Register"
    | RegisterE _ -> "RegisterE"
    | Counter _ -> "Counter"
    | CounterNoLoad _ -> "CounterNoLoad"
    | CounterNoEnable _ -> "CounterNoEnable"
    | CounterNoEnableLoad _ -> "CounterNoEnableLoad"
    | AsyncROM1 _ -> "AsyncROM1"
    | ROM1 _ -> "ROM1"
    | RAM1 _ -> "RAM1"
    | AsyncRAM1 _ -> "Async RAM"
    | AsyncROM _ -> "AsyncROM"
    | ROM _ -> "ROM"
    | RAM _ -> "RAM"
    | Shift _ -> "Shift"
    | BusCompare _ -> "BusCompare"
    | Input _ -> "Input"
    | Constant _ -> "Constant"



//------------------- Sidebar for Group Selection -------------------//
/// Sidebar that allows the user to select symbols to add to a new group
let sidebarToCreateNewGroup : (ModelType.Msg -> Unit) -> ModelType.Model -> ReactElement =
    let sidebarOptions : SidebarOptions = {
        ExtraStyle = [];
        TitleText = "Choose Components for Group";
        Cancellable = (Cancellable.Func (fun model -> (getUngroupedSymbols model.Sheet).Length <= 0));
        SideBarButtons =[]; // we need a dynamic button whose colour/css depends on the model. so we will have to define the sidebar buttons in the sidebarBody
        }
    let sidebarBody =
        fun dispatch model ->
            let ungroupedSelectedSymbols = getUngroupedSelectedSymbols model.Sheet

            let selectedSymbolRows =
                ungroupedSelectedSymbols
                |> List.map (fun symbol ->
                    let compTypeDescr = getComponentTypeDescrFromSym symbol
                    tr
                        [Style [(*BackgroundColor (generateColourFromModel model.Sheet)*)]]
                        [ td [] [ str (symbol.Component.Label.ToString())  ];
                        td
                            []
                            [ code [] [ str ( compTypeDescr )] ];
                        ])
            let selectedSymbolsTable =
                if selectedSymbolRows.Length = 0 then
                    div [Style [MarginTop "10px"; MarginBottom "25px"; Border ""]] [str "No valid symbols selected."]
                else
                    div [Style [MarginTop "10px"; (*BackgroundColor (generateColourFromModel model.Sheet)*) ]] [
                    p [Style [Margin "10px 0"]] [ str "Currently selected components that are not in any group: "]
                    Table.table
                        [Table.IsFullWidth;Table.TableOption.Props[Style [MarginBottom "10px"]]]
                        [ tr
                            [Style [(*BackgroundColor (generateColourFromModel model.Sheet)*)]]
                            [ th [] [ str "Label" ];
                                th [] [ str "Type" ]; ];
                            yield! selectedSymbolRows ];
                        ]

            let confirmButton (model) : ReactElement =

                let ungroupedSelectedSymIds = ungroupedSelectedSymbols |> List.map (fun s -> s.Id)
                match ungroupedSelectedSymbols with
                | [] ->
                    Button.button [
                        Button.Option.Props[ClassName "button"];
                        Button.Disabled true;
                        Button.IsGhost;
                        Button.Props[Style [Margin "3px"]];
                    ] [ str "Confirm Group" ]
                | _ ->
                    Button.button [
                        Button.Option.Props[ClassName "button is-primary"];
                        Button.Props[Style [Margin "3px"]];
                        Button.OnClick (fun _ ->

                                let symbolDispatch symMsg =  symMsg |> Symbol |> Wire |> Sheet |> dispatch
                                symbolDispatch (DrawModelType.SymbolT.SetGroupMapAndInfo( createNewGroup model.Sheet ungroupedSelectedSymIds ))
                                dispatch CloseContextualSidebar
                        )
                    ] [ str "Confirm Group" ]




            div [] [  str "Choose symbols via Cmd/Ctrl + click, or clicking and dragging.";
                                        selectedSymbolsTable;
                                        div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; FlexDirection FlexDirection.Row ] ]
                                        [
                                            div [ Style [  ] ] [confirmButton model]
                                            ]]

    buildSidebar sidebarOptions sidebarBody

/// Sidebar that allows the user to select symbols to add to an existing group
let sidebarToAddToExistingGroup (groupId: GroupId) =
    let sidebarOptions : SidebarOptions = {
        ExtraStyle = [];
        TitleText = "Choose Components for Group";
        Cancellable = (Cancellable.Func (fun model -> (getUngroupedSymbols model.Sheet).Length <= 0));
        SideBarButtons =[]; // we need a dynamic button whose colour/css depends on the model. so we will have to define the sidebar buttons in the sidebarBody
        }
    let sidebarBody =
        fun dispatch model ->

            let groupedComponentIds =
                model.Sheet.Wire.Symbol.GroupMap
                |> Map.toList
                |> List.collect snd


            let ungroupedSelectedSymbols = getUngroupedSelectedSymbols model.Sheet

            let selectedSymbolRows =
                ungroupedSelectedSymbols
                |> List.map (fun symbol ->
                    let compTypeDescr = getComponentTypeDescrFromSym symbol
                    tr
                        [Style [(*BackgroundColor (generateColourFromModel model.Sheet)*)]]
                        [ td [] [ str (symbol.Component.Label.ToString())  ];
                        td
                            []
                            [ code [] [ str ( compTypeDescr )] ];
                        ])
            let selectedSymbolsTable =
                if selectedSymbolRows.Length = 0 then
                    div [Style [MarginTop "10px"; MarginBottom "25px"; Border ""]] [str "No valid symbols selected."]
                else
                    div [Style [MarginTop "10px"; (*BackgroundColor (generateColourFromModel model.Sheet)*) ]] [
                    p [Style [Margin "10px 0"]] [ str "Currently selected components that are not in any group: "]
                    Table.table
                        [Table.IsFullWidth;Table.TableOption.Props[Style [MarginBottom "10px"]]]
                        [ tr
                            [Style [(*BackgroundColor (generateColourFromModel model.Sheet)*)]]
                            [ th [] [ str "Label" ];
                                th [] [ str "Type" ]; ];
                            yield! selectedSymbolRows ];
                        ]

            let confirmButton (model) : ReactElement =

                let ungroupedSelectedSymIds = ungroupedSelectedSymbols |> List.map (fun s -> s.Id)
                match ungroupedSelectedSymbols with
                | [] ->
                    Button.button [
                        Button.Option.Props[ClassName "button"];
                        Button.Disabled true;
                        Button.IsGhost;
                        Button.Props[Style [Margin "3px"]];
                    ] [ str "Confirm Group" ]
                | _ ->
                    Button.button [
                        Button.Option.Props[ClassName "button is-primary"];
                        Button.Props[Style [Margin "3px"]];
                        Button.OnClick (fun _ ->

                                let symbolDispatch symMsg =  symMsg |> Symbol |> Wire |> Sheet |> dispatch
                                symbolDispatch (DrawModelType.SymbolT.SetGroupMap( addToGroup model.Sheet groupId ungroupedSelectedSymIds ))
                                dispatch CloseContextualSidebar
                        )
                    ] [ str "Confirm Group" ]




            div [] [  str "Choose symbols via Cmd/Ctrl + click, or clicking and dragging.";
                                        selectedSymbolsTable;
                                        div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; FlexDirection FlexDirection.Row ] ]
                                        [
                                            div [ Style [  ] ] [confirmButton model]
                                            ]]

    buildSidebar sidebarOptions sidebarBody


