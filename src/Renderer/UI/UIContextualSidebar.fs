module UIContextualSideBar
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

/// A button on a contextual sidebar
/// Buttons take in a model and return a message
type SideBarButton = {
    ButtonClassNames: string; // for colours with fulma
    ButtonText: string; // for the text on the button
    ButtonOnClick: Model -> (Msg -> Unit)

}
type SideBarOptions = {
    ExtraStyle: CSSProp list;
    TitleMessage: string;
    SideBarButtons: SideBarButton list;
    Cancellable: bool;
}



/// A simple sidebar with a title and a list of buttons, with a static body
let buildSimpleSideBar (options: SideBarOptions) (body: Model -> ReactElement) =
    fun (dispatch: Msg -> Unit)(model:Model) ->
        let buttons =
            options.SideBarButtons
            |> List.map (fun buttonInfo ->
                Button.button [ (*ClassName button.ButtonClassNames; OnClick (fun _ -> button.ButtonOnClick model dispatch)*) ] [ str buttonInfo.ButtonText ]
            )

        div [] [
        Heading.h3 [  ] [ str options.TitleMessage ]
        div [] [
            body model
        ]
        div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; Padding "0.5rem"; ] ] [


        ]
    ]



