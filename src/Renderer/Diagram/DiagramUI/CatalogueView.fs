(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open DiagramStyle
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
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.map (makeCustom model)

let private createComponent comp label model dispatch =
    let offset = model.CreateComponentOffset
    model.Diagram.CreateComponent comp label (100+offset) (100+offset) |> ignore
    (offset + 50) % 200 |> SetCreateComponentOffset |> dispatch

let private createIOPopup typeStr compType model dispatch =
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
            createComponent (compType inputInt) inputText model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createSplitWirePopup model dispatch =
    let title = sprintf "Add SplitWire node" 
    let beforeInt =
        fun _ -> str "How many bits should the top wire have? The remaining bits will go in the bottom wire."
    let intDefault = 1
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            createComponent (SplitWire inputInt) "" model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createMemoryPopup model dispatch =
    let fake = {
        AddressWidth = 2
        WordWidth = 4
        Data = [0; 1; 4; 15] |> List.map int64
    }
    createComponent (ROM fake) "" model dispatch

let private makeMenuGroup title menuList =
    details [Open true] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let viewCatalogue model dispatch =
    Menu.menu [] [
            makeMenuGroup
                "Input / Output"
                [ menuItem "Input"  (fun _ -> createIOPopup "input" Input model dispatch)
                  menuItem "Output" (fun _ -> createIOPopup "output" Output model dispatch) ]
            makeMenuGroup
                "Buses"
                [ menuItem "MergeWires"  (fun _ -> createComponent MergeWires "" model dispatch)
                  menuItem "SplitWire" (fun _ -> createSplitWirePopup model dispatch) ]
            makeMenuGroup
                "Gates"
                [ menuItem "Not"  (fun _ -> createComponent Not "" model dispatch)
                  menuItem "And"  (fun _ -> createComponent And "" model dispatch)
                  menuItem "Or"   (fun _ -> createComponent Or "" model dispatch)
                  menuItem "Xor"  (fun _ -> createComponent Xor "" model dispatch)
                  menuItem "Nand" (fun _ -> createComponent Nand "" model dispatch)
                  menuItem "Nor"  (fun _ -> createComponent Nor "" model dispatch)
                  menuItem "Xnor" (fun _ -> createComponent Xnor "" model dispatch) ]
            makeMenuGroup
                "Mux / Demux"
                [ menuItem "Mux2" (fun _ -> createComponent Mux2 "" model dispatch)
                  menuItem "Demux2" (fun _ -> createComponent Demux2 "" model dispatch) ]
            makeMenuGroup
                "Flip Flops"
                [ menuItem "D-flip-flop" (fun _ -> createComponent DFF "" model dispatch) ]
            makeMenuGroup
                "Memories"
                [ menuItem "ROM (synchronous)" (fun _ -> createMemoryPopup model dispatch) ]
            makeMenuGroup
                "This project"
                (makeCustomList model)
        ]
