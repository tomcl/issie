(*
    OnDiagramButtonsView.fs

    This module provides the views and the functions that are triggered when
    clicking the on-diagram buttons, such as undo/redo copy/paste.
*)

module OnDiagramButtonsView

open Fulma
open Elmish.React
open Fable.React
open Fable.React.Props

open DiagramMessageType
open DiagramModelType
open CommonTypes
open DiagramStyle
open Extractor
open Helpers
open Simulator

let private copyAction model dispatch =
    match model.Diagram.GetSelected () with
    | None -> ()
    | Some jsState -> extractState jsState |> SetClipboard |> dispatch

/// Map the port Ids of the old component to the equivalent Ports of the new
/// component. For example, if the component is a Not, the mapping will have two
/// entries, the input port and the output port.
let private mapPorts (oldComp : Component) (newComp : Component) : (string * Port) list =
    let mapPort oldPort newPort =
        assertThat (oldPort.PortNumber = newPort.PortNumber) <| "cloned components have different port numbers"
        assertThat (oldPort.PortType = newPort.PortType) <| "cloned components have different port types"
        oldPort.Id, newPort
    assertThat (oldComp.Id <> newComp.Id) "cloned component has same id as old one"
    assertThat (oldComp.Type = newComp.Type) "cloned components have different types"
    assertThat (oldComp.Label = newComp.Label) "cloned components have different labels"
    let inputs = (oldComp.InputPorts, newComp.InputPorts) ||> List.map2 mapPort
    let outputs = (oldComp.OutputPorts, newComp.OutputPorts) ||> List.map2 mapPort
    inputs @ outputs

/// Transform a connection replacing the ports using the provided mapping.
let private mapToNewConnection (portMappings : Map<string,Port>) oldConnection : Connection =
    let mapGet pId = match portMappings.TryFind pId with
                     | None -> failwithf "what? Could not find port Id %s while cloning" pId
                     | Some port -> port
    {
        Id = "" // This will be ignored and replaced with an actual new Id.
        Source = { (mapGet oldConnection.Source.Id) with PortNumber = None }
        Target = { (mapGet oldConnection.Target.Id) with PortNumber = None }
        Vertices = oldConnection.Vertices |> List.map (fun (x,y)->x+30.0,y+30.0)
    }

let pasteAction model =
    let oldComponents, oldConnections = model.Clipboard
    // Copy the old components and add them to the diagram.
    let newComponents =
        oldComponents
        |> List.map ((fun comp ->
            match model.Diagram.CreateComponent comp.Type comp.Label (comp.X+30) (comp.Y+30) with
            | None -> failwithf "what? Could not paste component %A" comp
            | Some jsComp -> jsComp) >> extractComponent)
    // The new (copied) components have been added to the diagram, now we need
    // to copy the connections among them.

    // Map the old components' ports to the new components' ports.
    let portMappings =
        (oldComponents, newComponents)
        ||> List.map2 mapPorts
        |> List.collect id
        |> Map.ofList
    // Iterate over the old connections replacing the ports to refer to the new
    // components, and add the newly created connections to the diagram.
    oldConnections
    |> List.map ((mapToNewConnection portMappings) >>
                 (model.Diagram.LoadConnection false))
    |> ignore

let simWireData2Wire wireData = 
    List.rev [0 .. List.length wireData - 1]
    |> List.zip wireData 
    |> List.map (fun (bit, weight) -> match bit with
                                      | SimulatorTypes.Bit.Zero -> bigint 0
                                      | SimulatorTypes.Bit.One -> bigint 2**weight ) //the way I use bigint might be wrong
    |> List.reduce (+)

let extractWaveData
        (simulationIOs : SimulatorTypes.SimulationIO list)
        (graph : SimulatorTypes.SimulationGraph)
        : Sample list =
    let extractWireData (inputs : Map<SimulatorTypes.InputPortNumber, SimulatorTypes.WireData>) : Sample =
        match inputs.TryFind <| SimulatorTypes.InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some wireData -> Wire { nBits = uint (List.length wireData)
                                  bitData = simWireData2Wire wireData }
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel, _) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> List.append result [ extractWireData comp.Inputs ]
    )

let simHighlighted (model: Model) dispatch = 
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ -> ()
    | _, None -> failwith "what? Cannot start a simulation without a project"
    | Some jsState, Some project ->
        let otherComponents =
            project.LoadedComponents
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> function
            | Ok simData -> 
                let clkAdvance (sD : SimulatorTypes.SimulationData) = 
                    feedClockTick sD.Graph
                    |> (fun graph -> { sD with Graph = graph
                                               ClockTickNumber = sD.ClockTickNumber+1 })
                let waveNames' = 
                    let extr = List.toArray >> Array.map (fun (_,b,_) -> match b with
                                                                         | SimulatorTypes.ComponentLabel name -> name)
                    (extr simData.Inputs, extr simData.Outputs) ||> Array.append 
                let waveData' : SimTime array =
                    match fst model.WaveSim.viewIndexes with 
                    | start when start = uint 0 -> simData
                    | start -> Array.fold (fun s _ -> clkAdvance s) simData [| 1..int start |]
                    |> (fun sD -> Array.mapFold (fun (s: SimulatorTypes.SimulationData) _ -> 
                            extractWaveData (List.append s.Inputs s.Outputs) s.Graph |> List.toArray, 
                            clkAdvance s) sD [| fst model.WaveSim.viewIndexes..snd model.WaveSim.viewIndexes |] )
                    |> fst

                { model.WaveSim with waveNames = Array.append model.WaveSim.waveNames waveNames'
                                     waveData = (Array.zip model.WaveSim.waveData waveData')
                                                |> Array.map (fun (a,b) -> Array.append a b)
                                     selected = Array.map (fun _ -> false) [| 1..Array.length waveNames' |]
                                                |> Array.append model.WaveSim.selected }
                |> StartWaveSim
                |> dispatch
            | Error _ -> ()

let viewOnDiagramButtons model dispatch =
    div [ canvasSmallMenuStyle ] [
        let canvasBut func label = 
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick func ] ] 
                          [ str label ]
        canvasBut (fun _ -> model.Diagram.Undo ()) "< undo"
        canvasBut (fun _ -> model.Diagram.Redo ()) "redo >"
        canvasBut (fun _ -> copyAction model dispatch) "copy"
        canvasBut (fun _ -> pasteAction model) "paste"
        canvasBut (fun _ -> simHighlighted model dispatch) "simulate"
    ]
