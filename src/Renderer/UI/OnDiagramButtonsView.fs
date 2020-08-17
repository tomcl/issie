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

/// Returns a tuple option representing the output to which the target input is connected
let driveOut simData targetCompId inPortN =
    Map.toArray simData.Graph
    |> Array.tryPick ( fun (outCompId, (simComp: SimulatorTypes.SimulationComponent)) -> 
        Map.toArray simComp.Outputs
        |> Array.tryFind (fun (_, lst) ->
               (List.exists (fun t -> t = (targetCompId, inPortN)) lst)) 
        |> function 
           | Some (outPN, _) -> Some (outCompId, outPN)
           | None -> None )

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
    let processInputs compId inputs = 
        inputs
        |> Map.toArray
        |> Array.collect (fun (portN, _) -> 
            match simData.Graph.[compId].Type with
            | Input _ -> [||]
            | _ ->
                match driveOut simData compId portN with
                | Some tup -> [| tup |]
                | None -> failwith "Input is not connected" )

    let processOutputs compId outputs =
        outputs 
        |> Map.toArray
        |> Array.map (fun (portNum, _) -> compId, portNum)   
        
    //let lst' =
    simData.Graph
    |> Map.filter (fun compId _ -> List.contains compId (getSelectedComps model))
    |> Map.toArray
    |> Array.collect (fun (compId, simComp) -> 
        Array.append (processInputs compId simComp.Inputs) (processOutputs compId simComp.Outputs))
    |> Array.distinct
        // The commented lines keep the old waveforms when adding new ones
        //|> Array.map (fun el -> el, true)

    (*Array.fold (fun st port ->
                    match Array.contains (port, true) st with
                    | true -> st
                    | false -> Array.append st [| port, false |] ) lst' model.WaveSim.ports
    |> Array.unzip*)

let limBits (name: string) : (int*int) option =
    match Seq.tryFind ((=)'[') name, Seq.tryFind ((=)':') name, Seq.tryFind ((=)']') name with
    | Some, Some, Some ->
       ( name.[Seq.findIndexBack ((=)'[') name + 1..Seq.findIndexBack ((=)':') name - 1 ], name.[Seq.findIndexBack ((=)':') name + 1..Seq.findIndexBack ((=)']') name - 1 ] )
       |> (fun (a,b) -> int a, int b)
       |> Some
    | _ -> None

let rec findName (simData: SimulatorTypes.SimulationData) compId outPortN = 
    let compLbl =
        match Map.tryFind compId simData.Graph with
        | Some simComp ->
            match simComp.Label with 
            | ComponentLabel lbl -> 
                match Seq.tryFindIndexBack ( (=) '(' ) lbl with
                | Some i -> lbl.[0..i - 1]
                | None -> lbl //not robust!
        | None -> failwith "simData.Graph.[compId] doesn't exist"

    let outPortInt = match outPortN with
                     | OutputPortNumber pn -> pn

    let driveName n compTypeStr =
        match driveOut simData compId (InputPortNumber n) with
        | Some (driveCompId, drivePortN) -> findName simData driveCompId drivePortN
        | None -> failwith (compTypeStr + "input not connected")

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
        match driveOut simData compId (InputPortNumber 0) with
        | Some (driveCompId, drivePortN) -> 
            match findName simData driveCompId drivePortN with
            | hd::tl -> 
                ("("+fst hd, snd hd)::tl
                |> function
                   | hd::[] -> (fst hd + ")", snd hd)::[]
                   | lst -> List.append lst.[0 .. List.length lst - 2] [fst (List.last lst) + ")", snd (List.last lst)]
            | [] -> failwith "Error: IOLabel input names list is empty"
        | None -> failwith "IOLabel input not connected"
    | MergeWires ->
        List.append (driveName 1 "MergeWires") (driveName 0 "MergeWires")
    | SplitWire w -> 
        let predicate (_, b) =
            match outPortInt with
            | 0 -> b >= w
            | 1 -> b < w
            | _ -> failwith "SplitWire output port number greater than 1"
        let split name msb lsb st =
            List.zip [lsb .. msb] [st + msb - lsb .. -1 .. st]
            |> List.filter predicate
            |> List.unzip
            |> function
               | [],_ -> None
               | lst, _ -> Some (name, (List.max lst, List.min lst))
        (0, driveName 0 "SplitWire")
        ||> List.mapFold (fun st (name, (msb, lsb)) -> 
            split name msb lsb st, st + msb - lsb + 1 )
        |> fst
        |> List.choose id
    | BusSelection (w, oLSB) -> 
        let filtSelec name msb lsb st =
            List.zip [lsb .. msb] [st .. st + msb - lsb]
            |> List.filter (fun (_, b) ->  oLSB <= b && b <= oLSB + w - 1)
            |> List.unzip
            |> function
               | [],_ -> None
               | lst, _ -> Some (name, (List.max lst, List.min lst))
        (driveName 0 "BusSelection", 0)
        ||> List.mapFoldBack (fun (name, (msb, lsb)) st -> 
                filtSelec name msb lsb st, st + msb - lsb + 1 )
        |> fst
        |> List.choose id
        |> List.rev

let bitNums (a,b) = 
    match (a,b) with 
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb 
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let extractWaveNames simData model =
    selected2portLst simData model
    //|> fst
    |> Array.map (fun (compId, portN) ->
        match findName simData compId portN with
        | [el] -> fst el + bitNums (snd el)
        | lst when List.length lst > 0 -> 
            List.fold (fun st (name, bitLims) -> st + name + bitNums bitLims + ", ") "{ " lst
            |> (fun lbl -> lbl.[0..String.length lbl - 3] + " }" )  
        | _ -> "Signal doesn't have a name source" )//Deal with this in better way

let extractSimTime simData model =
    selected2portLst simData model
    //|> fst
    |> Array.map (fun (compId, portN) ->
        match simData.Graph.[compId].Outputs.[portN] with 
        | [] -> StateSample [| "output not connected" |]
        | lst -> 
            match simData.Graph.[fst lst.[0]].Inputs.[snd lst.[0]] with
            | wD -> Wire { nBits = uint (List.length wD)
                           bitData = simWireData2Wire wD } )

let extractWaveData simData model : SimTime [] = 
    let clkAdvance (sD : SimulatorTypes.SimulationData) = 
        feedClockTick sD.Graph
        |> (fun graph -> { sD with Graph = graph
                                   ClockTickNumber = sD.ClockTickNumber+1 })

    match fst model.WaveSim.viewIndexes with 
    | start when start = uint 0 -> simData
    | start -> Array.fold (fun s _ -> clkAdvance s) simData [| 1..int start |]
    |> (fun sD -> 
            Array.mapFold (fun (s: SimulatorTypes.SimulationData) _ -> 
                                    extractSimTime s model, clkAdvance s) 
                                    sD [| fst model.WaveSim.viewIndexes..snd model.WaveSim.viewIndexes |] )
    |> fst
           

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
                let ports' = selected2portLst simData model
                Ok { model.WaveSim with waveNames = extractWaveNames simData model
                                        waveData = extractWaveData simData model
                                        selected = Array.map (fun _ -> true) ports' 
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
