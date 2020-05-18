(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open DiagramModelType
open DiagramMessageType
open DiagramTypes
open PopupView

let private menuItem label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick ] ]
        [ str label ]

let private makeCustom model loadedComponent =
    menuItem loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = loadedComponent.InputLabels
            OutputLabels = loadedComponent.OutputLabels
        }
        model.Diagram.CreateComponent custom loadedComponent.Name 100 100
        |> ignore
    )

let private makeCustomList model =
    match model.CurrProject with
    | None -> Menu.list [] []
    | Some project ->
        // Do no show the open component in the catalogue.
        Menu.list [] (project.LoadedComponents
                      |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
                      |> List.map (makeCustom model))

let private askLabelPopup typeStr compType model dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = 1
    let body = dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            model.Diagram.CreateComponent (compType inputInt) inputText 100 100 |> ignore
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let viewCatalogue model dispatch =
    Menu.menu [ ] [
            Menu.label [ ] [ str "Input / Output" ]
            Menu.list []
                [ menuItem "Input"  (fun _ -> askLabelPopup "input" Input model dispatch)
                  menuItem "Output" (fun _ -> askLabelPopup "output" Output model dispatch) ]
            Menu.label [] [ str "Buses" ]
            Menu.list []
                [ menuItem "MakeBus2"  (fun _ -> model.Diagram.CreateComponent MakeBus2 "" 100 100 |> ignore)
                  menuItem "SplitBus2" (fun _ -> model.Diagram.CreateComponent SplitBus2 "" 100 100 |> ignore)
                  menuItem "PushToBusFirst" (fun _ -> model.Diagram.CreateComponent PushToBusFirst "" 100 100 |> ignore)
                  menuItem "PushToBusLast" (fun _ -> model.Diagram.CreateComponent PushToBusLast "" 100 100 |> ignore)
                  menuItem "PopFirstFromBus" (fun _ -> model.Diagram.CreateComponent PopFirstFromBus "" 100 100 |> ignore)
                  menuItem "PopLastFromBus" (fun _ -> model.Diagram.CreateComponent PopLastFromBus "" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Gates" ]
            Menu.list []
                [ menuItem "Not"  (fun _ -> model.Diagram.CreateComponent Not "" 100 100 |> ignore)
                  menuItem "And"  (fun _ -> model.Diagram.CreateComponent And "" 100 100 |> ignore)
                  menuItem "Or"   (fun _ -> model.Diagram.CreateComponent Or "" 100 100 |> ignore)
                  menuItem "Xor"  (fun _ -> model.Diagram.CreateComponent Xor "" 100 100 |> ignore)
                  menuItem "Nand" (fun _ -> model.Diagram.CreateComponent Nand "" 100 100 |> ignore)
                  menuItem "Nor"  (fun _ -> model.Diagram.CreateComponent Nor "" 100 100 |> ignore)
                  menuItem "Xnor" (fun _ -> model.Diagram.CreateComponent Xnor "" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Mux / Demux" ]
            Menu.list []
                [ menuItem "Mux2" (fun _ -> model.Diagram.CreateComponent Mux2 "" 100 100 |> ignore)
                  menuItem "Demux2" (fun _ -> model.Diagram.CreateComponent Demux2 "" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Custom" ]
            makeCustomList model
        ]
