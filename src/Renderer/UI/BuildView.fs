(*
    BuildView.fs

    View for catalogue in the right tab.
*)

module BuildView

open System
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

open Node.ChildProcess
module node = Node.Api

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

type WaitForFinish =
    | Woken
    | Finished

[<AutoOpen>]
module AsyncEx = 
    type private SuccessException<'T>(value : 'T) =
        inherit Exception()
        member self.Value = value

    type Microsoft.FSharp.Control.Async with
        // efficient raise
        static member Raise (e : #exn) = Async.FromContinuations(fun (_,econt,_) -> econt e)
  
        static member CoolChoice<'T>(tasks : seq<Async<'T option>>) : Async<'T option> =
            let wrap task =
                async {
                    let! res = task
                    match res with
                    | None -> return None
                    | Some r -> return! Async.Raise <| SuccessException r
                }

            async {
                try
                    do!
                        tasks
                        |> Seq.map wrap
                        |> Async.Parallel
                        |> Async.Ignore

                    return None
                with 
                | :? SuccessException<'T> as ex -> return Some ex.Value
            }

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
                    Button.button
                        [ 
                            Button.Color IsPrimary;
                            Button.OnClick (fun _ ->
                                Async.StartImmediate(async {
                                    let sleep_task = async {
                                        do! Async.Sleep 1000
                                        return Some Woken
                                    }
                                    let a = Async.FromContinuations (fun (succ, _, _) ->
                                        let child = node.childProcess.spawn ("sh", ["-c"; "sleep 2 && echo hi"] |> ResizeArray);
                                        child.stdout.on("data", fun d -> printfn "stuff: %s" d) |> ignore
                                        child.stderr.on("data", fun s -> eprintfn "error: %s" s) |> ignore
                                        child.on("exit", fun d -> printfn "done"; succ <| Some Finished ) |> ignore
                                    )
                                    let delay (interval: int) result =
                                        async {
                                            do! Async.Sleep interval
                                            return! async {
                                                printfn "returning %A after %d ms." result interval
                                                return result }
                                        }

                                    let first_delay = delay 1500 (Some Finished)
                                    let a_executed = Async.StartChild a

                                    let get_a = async {
                                        let! res = a_executed
                                        return! res
                                    }

                                
                                    while true do
                                        let res =
                                            [ delay 600 (Some Woken) ; get_a ]
                                            |> Async.CoolChoice
                                            |> Async.RunSynchronously
                                            
                                        printfn "a_res = %A" res

                                        match res with
                                        | Some Finished -> return ()
                                        | Some Woken -> ()
                                        | _ -> return ()
                                })
                            );
                        ]
                        [ str "test" ]
                    if (model.Sheet.Compiling) then
                        Button.button
                            [ 
                                Button.Color IsDanger;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.StopCompilation) |> dispatch);
                            ]
                            [ str "Stop building" ]
                    else
                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.StartCompiling) |> dispatch);
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