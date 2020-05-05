(*
    PopupView.fs

    This module provides a handy interface to create popups.
*)

module PopupView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

open JSHelpers
open DiagramMessageType
open DiagramModelType

/// Unclosable popup.
let stablePopup body =
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [] []
        Modal.Card.card [] [
            Modal.Card.body [] [ body ]
        ]
    ]

let private buildPopup title body foot close =
    fun model ->
        Modal.modal [ Modal.IsActive true ] [
            Modal.background [ Props [ OnClick close ] ] []
            Modal.Card.card [] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick close ] []
                ]
                Modal.Card.body [] [ body model ]
                Modal.Card.foot [] [ foot model ]
            ]
        ]

/// Body and foot are functions that take the model and get produced
/// dynamically. The input to those functions is a string (for example, in a
/// dialog popup, the string is the current value of the input box.).
let private dynamicClosablePopup title body foot dispatch =
    buildPopup title body foot (fun _ -> dispatch ClosePopup)
    |> ShowPopup
    |> dispatch

/// Create a popup and add it to the page. Body and foot are static content.
/// Can be closed by the ClosePopup message.
let closablePopup title body foot dispatch =
    dynamicClosablePopup title (fun _ -> body) (fun _ -> foot) (fun _ -> dispatch ClosePopup)

/// Get the value for a change event in an input textbox.
let private getEventValue (event: React.FormEvent) = 
    getFailIfNull event.currentTarget ["value"] |> unbox<string>  

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
let dialogPopup title before placeholder buttonText buttonAction isDisabled dispatch =
    let body =
        fun dialogText ->
            div [] [
                before dialogText
                Input.text [
                    Input.Placeholder placeholder
                    Input.OnChange (getEventValue >> Some >> SetPopupDialogText >> dispatch)
                ]
            ]
    let foot =
        fun dialogText ->
            Level.level [] [
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
                            Button.Disabled (isDisabled dialogText)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> buttonAction dialogText)
                        ] [ str buttonText ]
                    ]
                ]
            ]
    dynamicClosablePopup title body foot dispatch

/// Display popup, if any is present.
let viewPopup model =
    match model.Popup, model.PopupDialogText with
    | None, _ -> div [] []
    | Some popup, None -> popup ""
    | Some popup, Some text -> popup text
