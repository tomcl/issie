(*
    BuildView.fs
    View for catalogue in the right tab.
*)

module BuildView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open DiagramStyle
open ModelType
open CommonTypes
open PopupView
open Sheet.SheetInterface
open DrawModelType


let private menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]


let private makeRowForCompilationStage (name: string) (stage: SheetT.CompilationStage) =
    tr [] [
        th [] [str name]
        match stage with
        | SheetT.Completed t ->
                th [ Style [ BackgroundColor "green"] ] [str $"{t} seconds"]
        | SheetT.InProgress t ->
                th [ Style [ BackgroundColor "yellow"] ] [str $"{t} seconds"]
        | SheetT.Failed ->
                th [ Style [ BackgroundColor "red"] ] [str "XX"]
        | SheetT.Queued ->
                th [ Style [ BackgroundColor "gray"] ] [str "--"]
    ]

let viewBuild model dispatch =
        let viewCatOfModel = fun model ->                 
            let styles = 
                match model.Sheet.Action with
                | SheetT.InitialisedCreateComponent _ -> [Cursor "grabbing"]
                | _ -> []

            let catTip1 name func (tip:string) = 
                let react = menuItem styles name func
                div [ HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                      Tooltip.dataTooltip tip
                      Style styles
                    ]
                    [ react ]
            Menu.menu [Props [Class "py-1"; Style styles]]  [
                    if (model.Sheet.Compiling) then
                        Button.button
                            [ 
                                Button.Color IsDanger;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.SetCompiling false) |> dispatch);
                            ]
                            [ str "Stop building" ]
                    else
                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.SetCompiling true) |> dispatch);
                            ]
                            [ str "Build and upload" ]

                    br []; br []
                    Table.table [
                        Table.IsFullWidth
                        Table.IsBordered
                    ] [
                        thead [] [ tr [] [
                            th [] [str "Stage"]
                            th [] [str "Progress"]
                        ] ]
                        tbody [] [
                            makeRowForCompilationStage "Synthesis" model.Sheet.CompilationStatus.Synthesis
                            makeRowForCompilationStage "Place And Route" model.Sheet.CompilationStatus.PlaceAndRoute
                            makeRowForCompilationStage "Generate" model.Sheet.CompilationStatus.Generate
                            makeRowForCompilationStage "Upload" model.Sheet.CompilationStatus.Upload
                        ]
                    ]

                ]

        (viewCatOfModel) model 