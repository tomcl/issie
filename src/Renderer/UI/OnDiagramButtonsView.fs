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
        |> List.concat
        |> Map.ofList
    // Iterate over the old connections replacing the ports to refer to the new
    // components, and add the newly created connections to the diagram.
    oldConnections
    |> List.map ((mapToNewConnection portMappings) >>
                 (model.Diagram.LoadConnection false))
    |> ignore


//simulate button functions
let findDrivingOut simData targetCompId inPortN =
    Array.fold (fun st (compId, (simComp: SimulatorTypes.SimulationComponent)) -> 
        match st with
        | [||] ->
            Array.tryFind (fun (_, lst) ->
                (List.exists (fun (pointId,pointPortN) -> 
                    pointId = targetCompId && pointPortN = inPortN ) lst) ) (Map.toArray simComp.Outputs)
            |> function 
               | Some (outPN, _) -> [| compId,outPN |]
               | None -> [||] 
        | s -> s ) [||] (Map.toArray simData.Graph) 

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

    let processInputs inputs = 
        inputs
        |> Map.toArray
        |> Array.map fst
        |> Array.collect (fun portN -> 
            Array.fold (fun _ (compId, _) -> 
                findDrivingOut simData compId portN ) [||] (List.toArray graph) )

    let processOutputs compId outputs =
        outputs 
        |> Map.toArray
        |> Array.map (fun (portNum, _) -> compId, portNum)   

    let lst' =
        List.toArray graph
        |> Array.filter (fun (compId, _) -> List.contains compId (getSelectedComps model))
        |> Array.collect (fun (compId, simComp) -> 
            Array.append (processInputs simComp.Inputs) (processOutputs compId simComp.Outputs))
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

let limBits (name: string) : (int*int) option =
    match Seq.tryFind ((=)'[') name, Seq.tryFind ((=)':') name, Seq.tryFind ((=)']') name with
    | Some, Some, Some ->
       ( name.[Seq.findIndexBack ((=)'[') name + 1..Seq.findIndexBack ((=)':') name - 1 ], name.[Seq.findIndexBack ((=)':') name + 1..Seq.findIndexBack ((=)']') name - 1 ] )
       |> (fun (a,b) -> int a, int b)
       |> Some
    | _ -> None

let rec findName (simData: SimulatorTypes.SimulationData) compId outPortN = //fix underscores' logic
    let compLbl = match simData.Graph.[compId].Label with
                  | ComponentLabel lbl -> 
                    match Seq.tryFindIndex ( (=) '(' ) lbl with
                    | Some i -> lbl.[0..i - 1]
                    | None -> lbl 
    let outPortInt = match outPortN with
                     | OutputPortNumber pn -> pn
    match simData.Graph.[compId].Type with
    | Not | And | Or | Xor | Nand | Nor | Xnor | Mux2 -> 
        [ compLbl, (0,0) ]
    | Input w | Output w -> 
        [ compLbl, (w-1,0) ]
    | Demux2 -> 
        [ compLbl + "_" + string outPortInt, (0, 0) ]
    | NbitsAdder w -> 
        match outPortInt with 
        | 0 -> [ compLbl + "_sum", (w-1, 0) ]
        | _ -> [ compLbl + "Cout", (w-1, 0) ]
    | DFF | DFFE -> 
        [ compLbl + "_Q", (0, 0) ]
    | Register w | RegisterE w -> 
        [ compLbl + "_data-out", (w-1, 0) ]
    | RAM mem | AsyncROM mem | ROM mem  -> 
        [ compLbl + "_data-out", (mem.WordWidth-1, 0) ]
    | Custom c -> 
        [ c.Name + "_" + fst c.OutputLabels.[outPortInt], (snd c.OutputLabels.[outPortInt] - 1, 0) ]
    | IOLabel -> 
        match findDrivingOut simData compId (InputPortNumber 0) with
        | [| driveCompId, drivePortN |] -> 
            match findName simData driveCompId drivePortN with
            | hd::tl -> ("("+fst hd, snd hd)::tl
            | _ -> failwith "Error: IOLabel input names list is empty"
            |> List.rev
            |> function
               | hd::tl -> (fst hd+")", snd hd)::tl
               | _ -> failwith "Error: IOLabel input names list is empty"
            |> List.rev //this way seems very inefficient
        | _ -> ["", (0, 0)] //not sure what this should be 
    | MergeWires ->
        let mergeIn n =
            match findDrivingOut simData compId (InputPortNumber n) with
            | [| driveCompId, drivePortN |] -> findName simData driveCompId drivePortN
            | _ -> failwith "MergeWires input not connected" //these failwith should be something else maybe
        List.append (mergeIn 1) (mergeIn 0)
    | SplitWire w -> 
        match findDrivingOut simData compId (InputPortNumber 0) with
        | [| driveCompId, drivePortN |] -> findName simData driveCompId drivePortN
        | _ -> failwith "SplitWire input not connected"
        |> match outPortInt with
           | 0 -> 
                List.mapFold (fun count (name, (msb, lsb)) -> 
                    let nbits = msb - lsb + 1
                    match count < w, nbits + count - 1 < w with
                    | true, true -> (name, (msb, lsb)), count+nbits
                    | true, false -> (name, (msb, msb+w+count+1)), count+nbits
                    | false, _ -> ("", (-1, -1)), count+nbits ) 0
           | 1 -> 
                List.mapFold (fun count (name, (msb, lsb)) -> 
                    let nbits = msb - lsb + 1
                    match nbits + count - 1 >= w, count >= w with
                    | true, true -> (name, (msb, lsb)), count+nbits
                    | true, false -> (name, (msb+w+count, lsb)), count+nbits
                    | false, _ -> ("", (-1, -1)), count+nbits ) 0
           | _ -> failwith "Output port number of SplitWire can only be 0 or 1"
        |> fst
        |> List.filter ( fun (_,tup) -> tup <> (0, 0) )

    | BusSelection (w, oLSB) -> 
        let oMSB = oLSB + w - 1
        match findDrivingOut simData compId (InputPortNumber 0) with
        | [| driveCompId, drivePortN |] -> findName simData driveCompId drivePortN
        | _ -> failwith "BusSelection input not connected"
        |> List.rev
        |> List.mapFold (fun st (name, (msb, lsb)) -> (name, [lsb..msb], [st..st+msb-lsb]),st+msb-lsb+1) 0
        |> fst
        |> List.map (fun (name, lst, lstCumul) -> 
            List.zip lst lstCumul
            |> List.filter (fun (_, cumul) -> oLSB <= cumul && cumul <= oMSB)
            |> List.map fst
            |> (fun lst -> name, lst) )
        |> List.filter (fun (_, lst) -> lst <> [])
        |> List.map (fun (name, lst) -> (name, (List.max lst, List.min lst)))
        |> List.rev

let bitNums (a,b) = 
    match (a,b) with 
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb 
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let extractWaveNames simData model =
    selected2portLst simData model
    |> fst
    |> Array.map (fun (compId, portN) ->
        match findName simData compId portN with
        | [el] -> fst el + bitNums (snd el)
        | lst when List.length lst > 0 -> 
            List.fold (fun st (name, bitLims) -> st + name + bitNums bitLims + ", ") "{ " lst
            |> (fun lbl -> lbl.[0..String.length lbl - 3] + " }" )  
        | _ -> "Signal doesn't have a name source" )//Deal with this in better way
           

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
                                        waveData = waveData'//this should already contain the some
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
