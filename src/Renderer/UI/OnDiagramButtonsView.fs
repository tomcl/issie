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
open SimulatorTypes

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


//simulate button functions
let simWireData2Wire wireData = 
    wireData
    |> List.mapFold (fun weight bit -> match bit with
                                       | SimulatorTypes.Bit.Zero -> bigint 0
                                       | SimulatorTypes.Bit.One -> weight 
                                       |> (fun r -> r, weight * (bigint 2)) ) (bigint 1) 
    |> fst |> List.sum

let getSelectedComps model =
    match model.Diagram.GetSelected () with
    | None -> []
    | Some jsState -> 
        fst jsState 
        |> List.map (extractComponent >> (fun c -> c.Id) >> SimulatorTypes.ComponentId)

let selected2portLst (simData : SimulatorTypes.SimulationData) (model: Model) =
    let graph = Map.toList simData.Graph

    let processInputs targetCompId inputs = 
        inputs
        |> Map.toArray
        |> Array.map fst
        |> Array.collect (fun portN -> 
            Array.fold (fun _ (compId, (simComp: SimulatorTypes.SimulationComponent)) -> 
                Array.tryFind (fun (_, lst) ->
                    not (List.forall (fun (pointId,pointPortN) -> 
                        pointId = targetCompId && pointPortN = portN ) lst) ) (Map.toArray simComp.Outputs)
                |> function 
                   | Some (outPN, _) -> [| compId,outPN |]
                   | None -> [||] ) [||] (List.toArray graph) )

    let processOutputs compId outputs =
        outputs 
        |> Map.toArray
        |> Array.map (fun (portNum, _) -> compId, portNum)   

    let lst' =
        List.toArray graph
        |> Array.filter (fun (compId, _) -> List.contains compId (getSelectedComps model))
        |> Array.collect (fun (compId, simComp) -> 
            Array.append (processInputs compId simComp.Inputs) (processOutputs compId simComp.Outputs))
        |> Array.distinct
        |> Array.map (fun el -> el, true)
        
    Array.fold (fun st port ->
                    match Array.contains (port, true) st with
                    | true -> st
                    | false -> Array.append st [| port, false |] ) lst' model.WaveSim.ports
    |> Array.unzip


let extractWaveData simData model =
    selected2portLst simData model
    |> fst
    |> Array.map (fun (compId, portN) ->
        match simData.Graph.[compId].Outputs.[portN] with 
        | [] -> StateSample [| "output not connected" |]
        | lst -> 
            match simData.Graph.[fst lst.[0]].Inputs.[snd lst.[0]] with
            | wD -> Wire { nBits = uint (List.length wD)
                           bitData = simWireData2Wire wD } )

let extractWaveNames simData model =
    selected2portLst simData model
    |> fst
    |> Array.map (fun (compId, portN) ->
        match simData.Graph.[compId].Label with
        | SimulatorTypes.ComponentLabel lbl -> lbl + "_" + string portN )
           

let simSelected (model: Model) dispatch = 
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

                let waveData' : SimTime [] = 
                    match fst model.WaveSim.viewIndexes with 
                    | start when start = uint 0 -> simData
                    | start -> Array.fold (fun s _ -> clkAdvance s) simData [| 1..int start |]
                    |> (fun sD -> 
                            Array.mapFold (fun (s: SimulatorTypes.SimulationData) _ -> 
                                                    extractWaveData s model, clkAdvance s) 
                                                    sD [| fst model.WaveSim.viewIndexes..snd model.WaveSim.viewIndexes |] )
                    |> fst

                let waveNames' = extractWaveNames simData model
                let ports', selected' = selected2portLst simData model
                    
                Ok { model.WaveSim with waveNames = waveNames'
                                        waveData = Some waveData'//this should already contain the some
                                        selected = selected'
                                        ports = ports'}
            | Error simError ->
                if simError.InDependency.IsNone then
                    // Highligh the affected components and connection only if
                    // the error is in the current diagram and not in a
                    // dependency.
                    (simError.ComponentsAffected, simError.ConnectionsAffected)
                    |> SetHighlighted |> dispatch
                Error simError
        |> StartWaveSim
        |> dispatch

let viewOnDiagramButtons model dispatch =
    div [ canvasSmallMenuStyle ] [
        let canvasBut func label = 
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick func ] ] 
                          [ str label ]
        canvasBut (fun _ -> model.Diagram.Undo ()) "< undo"
        canvasBut (fun _ -> model.Diagram.Redo ()) "redo >"
        canvasBut (fun _ -> copyAction model dispatch) "copy"
        canvasBut (fun _ -> pasteAction model) "paste"
        canvasBut (fun _ -> simSelected model dispatch) "simulate"
    ]
