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

module PopupView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

open JSHelpers
open DiagramMessageType
open DiagramModelType
open DiagramStyle

//========//
// Popups //
//========//

let getText (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text

let getInt (dialogData : PopupDialogData) =
    Option.defaultValue 1 dialogData.Int

/// Unclosable popup.
let stablePopup body =
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [] []
        Modal.Card.card [] [
            Modal.Card.body [] [ body ]
        ]
    ]

let private buildPopup title body foot close =
    fun (dialogData : PopupDialogData) ->
        Modal.modal [ Modal.IsActive true ] [
            Modal.background [ Props [ OnClick close ] ] []
            Modal.Card.card [] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick close ] []
                ]
                Modal.Card.body [] [ body dialogData ]
                Modal.Card.foot [] [ foot dialogData ]
            ]
        ]

/// Body and foot are functions that take a string of text and produce a
/// reactElement. The meaning of the input string to those functions is the
/// content of PopupDialogText (i.e. in a dialog popup, the string is the
/// current value of the input box.).
let private dynamicClosablePopup title body foot dispatch =
    buildPopup title body foot (fun _ -> dispatch ClosePopup)
    |> ShowPopup
    |> dispatch

/// Create a popup and add it to the page. Body and foot are static content.
/// Can be closed by the ClosePopup message.
let closablePopup title body foot dispatch =
    dynamicClosablePopup title (fun _ -> body) (fun _ -> foot) dispatch

/// Create the body of a dialog Popup with only text.
let dialogPopupBodyOnlyText before placeholder dispatch =
    fun (dialogData : PopupDialogData) ->
        div [] [
            before dialogData
            Input.text [
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with only an int.
let dialogPopupBodyOnlyInt beforeInt intDefault dispatch =
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch =
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeText dialogData
            Input.text [
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
                            Button.OnClick (fun _ -> dispatch ClosePopup)
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
    dynamicClosablePopup title body foot dispatch

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
    closablePopup title body foot dispatch

/// Display popup, if any is present.
let viewPopup model =
    match model.Popup with
    | None -> div [] []
    | Some popup -> popup model.PopupDialogData

//===============//
// Notifications //
//===============//

let errorNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        div [errorNotificationStyle] [
            Level.level [ Level.Level.Props [Style [Width "100%"] ] ] [
                Level.left [] [
                    Level.item [] [ str text ]
                ]
                Level.right [ Props [Style [MarginLeft "10px"] ] ] [
                    Level.item [] [ Delete.delete [ Delete.OnClick close ] [] ]
                ]
            ]
        ]

let viewNotifications model dispatch =
    match model.Notifications.FromDiagram, model.Notifications.FromSimulation with
    | None, None -> div [] []
    | Some notification, None -> notification dispatch
    | _, Some notification -> notification dispatch // Prioritise notifications from simulation.
