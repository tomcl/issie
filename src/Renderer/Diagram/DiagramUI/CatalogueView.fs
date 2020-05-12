(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open DiagramModelType
open DiagramTypes

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

let viewCatalogue model =
    Menu.menu [ ] [
            Menu.label [ ] [ str "Input / Output" ]
            Menu.list []
                [ menuItem "Input"  (fun _ -> model.Diagram.CreateComponent Input "input" 100 100 |> ignore)
                  menuItem "Output" (fun _ -> model.Diagram.CreateComponent Output "output" 100 100 |> ignore) ]
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
                [ menuItem "Mux2" (fun _ -> model.Diagram.CreateComponent Mux2 "mux2" 100 100 |> ignore) ]
            Menu.label [ ] [ str "Custom" ]
            makeCustomList model
        ]
