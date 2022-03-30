module Notifications

//===============//
// Notifications //
//===============//

open Fulma
open Fulma.Extensions.Wikiki


open Fable.React

open DiagramStyle
open ModelType

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

let displayAlertOnError (dispatch: Msg -> Unit) res =
    match res with
    | Error e -> 
        dispatch <| SetFilesNotification (errorFilesNotification e)
    | _ -> ()

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

