(*
    PopupView.fs

    This module provides a handy interface to create popups.
*)

module PopupView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open DiagramMessageType
open DiagramModelType

let private buildPopup title body foot close =
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick close ] ] []
        Modal.Card.card [] [
            Modal.Card.head [] [
                Modal.Card.title [] [ str title ]
                Delete.delete [ Delete.OnClick close ] []
            ]
            Modal.Card.body [] [ body ]
            Modal.Card.foot [] [ foot ]
        ]
    ]

let displayPopup title body foot dispatch =
    buildPopup title body foot (fun _ -> dispatch ClosePopup)
    |> ShowPopup
    |> dispatch

let viewPopup model =
    match model.Popup with
    | None -> div [] []
    | Some popup -> popup
