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
open PopupHelpers
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open DrawModelType

open Node.ChildProcess
module node = Node.Api

let private menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]


let private makeRowForCompilationStage (name: string) (stage: SheetT.CompilationStage) =
    let minutesSeconds t =
        let minutes = t / 60
        let seconds = t % 60
        let zeroPadding = if seconds < 10 then "0" else ""
        $"{minutes}:{zeroPadding}{seconds}"
    tr [] [
        th [] [str name]
        match stage with
        | SheetT.Completed t ->
                th [ Style [ BackgroundColor "green"] ] [str (minutesSeconds t)]
        | SheetT.InProgress t ->
                th [ Style [ BackgroundColor "yellow"] ] [str (minutesSeconds t)]
        | SheetT.Failed ->
                th [ Style [ BackgroundColor "red"] ] [str "XX"]
        | SheetT.Queued ->
                th [ Style [ BackgroundColor "gray"] ] [str "--"]
    ]

let verilogOutput (vType: Verilog.VMode) (model: Model) (profile: Verilog.CompilationProfile) (dispatch: Msg -> Unit) =
    match FileMenuHelpers.updateProjectFromCanvas model dispatch, model.Sheet.GetCanvasState() with
        | Some proj, state ->
            match model.UIState with
            | Some _ -> () // do nothing if in middle of I/O operation
            | None ->
                match Simulator.startCircuitSimulation 2 proj.OpenFileName state proj.LoadedComponents with
                | Ok sim -> 
                    let path = FilesIO.pathJoin [| proj.ProjectPath; proj.OpenFileName + ".v" |]
                    printfn "should be compiling %s :: %s" proj.ProjectPath proj.OpenFileName
                    match tryCreateFolder <| pathJoin [| proj.ProjectPath; "/build" |] with
                    // TODO: No way to check for existence
                    //| Error e -> printfn "Couldn't make build folder: %s" e
                    | _ -> 
                        try
                            let verilog = Verilog.getVerilog vType sim.FastSim profile
                            let mappings =
                                sim.FastSim.FOrderedComps
                                |> Array.filter (fun fc -> match fc.FType with | Viewer _ -> true | _ -> false)
                                |> Array.map (fun fc -> fc.FullName, fc.OutputWidth 0)
                                |> Array.collect (function 
                                    | (_, 0) -> [||]
                                    | (name, width) -> [0 .. width - 1] |> List.toArray |> Array.map (fun i -> $"{name}"))
                            dispatch (Sheet (SheetT.Msg.DebugUpdateMapping mappings))
                            printfn "%s" verilog
                            FilesIO.writeFile path verilog
                        with
                        | e ->
                            printfn $"Error in Verilog output: {e.Message}"
                            Error e.Message
                        |> (function
                            | Ok () -> Sheet (SheetT.Msg.StartCompiling (proj.ProjectPath, proj.OpenFileName, profile)) |> dispatch
                            | Error e -> ()//oh no
                            )
                | Error simError ->
                   printfn $"Error in simulation prevents verilog output {(SimulatorTypes.errMsg simError.ErrType)}"
        | _ -> ()

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
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.StopCompilation) |> dispatch);
                            ]
                            [ str "Stop building" ]
                    else
                        Label.label [] [ str "Device Selection" ]
                        Label.label [ ]
                            [Select.select []
                            [ select [(OnChange(fun option -> 
                                printfn "Value is: %s" option.Value
                                Sheet (SheetT.Msg.SetDebugDevice option.Value) |> dispatch  ))]
                                
                                ([option [Value "";Selected true;Disabled true] [str ("Select Device")]] @ [option [Value "IceStick"] [str "IceStick"]] @ [option [Value "IssieStick-v0.1"] [str "IssieStick v0.1"] ] @ [option [Value "IssieStick-v1.0"] [str "IssieStick v1.0"] ])
                                ]
                            ]
                        br []
                        let isDisabled =
                            match model.Sheet.DebugDevice with
                            |None -> true
                            |_ -> false

                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.Disabled isDisabled
                                Button.OnClick (fun _ -> verilogOutput Verilog.VMode.ForSynthesis model Verilog.Release dispatch);
                                
                            ]
                            [ str "Build and upload" ]
                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.Disabled isDisabled
                                Button.OnClick (fun _ -> verilogOutput Verilog.VMode.ForSynthesis model Verilog.Debug dispatch);
                            ]
                            [ str "Build and Debug" ]

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
                    if model.Sheet.DebugState = SheetT.Paused then
                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.DebugSingleStep) |> dispatch);
                            ]
                            [ str "Step" ]
                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.DebugContinue) |> dispatch);
                            ]
                            [ str "Continue" ]
                    elif model.Sheet.DebugState = SheetT.Running then
                        Button.button
                            [ 
                                Button.Color IsSuccess;
                                Button.OnClick (fun _ -> Sheet (SheetT.Msg.DebugPause) |> dispatch);
                            ]
                            [ str "Pause" ]

                    if model.Sheet.DebugState <> SheetT.NotDebugging then
                        //br [];
                        //Button.button
                        //    [ 
                        //        Button.Color IsDanger;
                        //        Button.OnClick (fun _ -> Sheet (SheetT.Msg.DebugDisconnect) |> dispatch);
                        //    ]
                        //    [ str "Disconnect" ]
                        br []
                        br []
                        Table.table [
                            Table.IsFullWidth
                            Table.IsBordered
                        ] [
                            thead [] [ tr [] [
                                th [ Style [ BackgroundColor "lightgray"] ] [str "Viewer"]
                                th [ Style [ BackgroundColor "lightgray"] ] [str "Value"]
                            ] ]
                            tbody [] (
                                let mappings = Array.toList model.Sheet.DebugMappings
                                //let mappings = [ "debug_stuff"; "debug_stuff"; "v2"; "v2"; "v2"; "v1"  ]
                                let bits =
                                    model.Sheet.DebugData
                                    |> List.collect (fun byte -> 
                                        [0..7]
                                        |> List.rev
                                        |> List.map (fun i -> Some <| (byte / (pown 2 i)) % 2))
                                let bits =
                                    List.append bits (List.map (fun _ -> None) [1..256])

                                let values =
                                    List.zip mappings bits
                                    |> List.fold (fun s (name, bit) ->
                                        match List.tryHead s with
                                        | Some (n, bits) when n = name-> (n, bit :: bits) :: (List.tail s)
                                        | _ -> (name, [bit]) :: s
                                        ) []

                                
                                let numOrX nOpt =
                                    Option.map (fun b -> b.ToString()) nOpt
                                    |> Option.defaultValue "x"
                                    |> String.map( fun ch ->
                                        match model.Sheet.DebugIsConnected with
                                        |false -> 'X'
                                        |true -> ch
                                    )

                                values
                                |> List.map (fun (name, bits) ->
                                    tr [] [
                                        th [] [str (name + if List.length bits = 1 then "" else $"[{List.length bits - 1}:0]")]
                                        th [] [ str <| "0b" + (bits |> List.map numOrX |> String.concat "") ]
                                    ]))
                        ]
                ]

        (viewCatOfModel) model 
