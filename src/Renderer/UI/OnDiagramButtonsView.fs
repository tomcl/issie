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

// types 
type DiagEl = | Comp of Component | Conn of Connection

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
let driveOut simGraph targetCompId inPortN =
    Map.toArray simGraph
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

let getSelected model : DiagEl list =
    match model.Diagram.GetSelected () with
    | None -> []
    | Some jsState -> 
        ( fst jsState |> List.map (extractComponent >> Comp), 
          snd jsState |> List.map (extractConnection >> Conn) )
        ||> List.append 

let procIns simData (compId: ComponentId) (inputs: InputPortNumber []) : WaveSimPort [] = 
    Array.collect (fun portN -> 
        match simData.Graph.[compId].Type, driveOut simData.Graph compId portN  with
        | Input _, _ -> [||]
        | Output _, Some (cId, oPN) ->  [| { CId = cId; OutPN = oPN; TrgtId = Some compId } |]
        | _, Some (cId, oPN) -> [| { CId = cId; OutPN = oPN; TrgtId = None } |]
        | _, None -> failwith "Input is not connected" ) inputs

let processComp simData cId : WaveSimPort [] =
    let procCompIns (compId: ComponentId) (inputs: Map<InputPortNumber,WireData>) : WaveSimPort [] = 
        Map.toArray inputs
        |> Array.map (fun (key, _) -> key)
        |> procIns simData compId 
        
    let procOuts compId outputs : WaveSimPort [] =
        Map.toArray outputs
        |> Array.map (fun (portNum, _) -> { CId = compId; OutPN = portNum; TrgtId = None })

    match Map.tryFind cId simData.Graph with
    | Some sC -> Array.append (procCompIns cId sC.Inputs) (procOuts cId sC.Outputs)
    | None -> failwith "Component Id is not in Simulation Data"

let remDuplicates arrWithDup =
    Array.groupBy (fun p -> p.CId, p.OutPN) arrWithDup
    |> Array.map (fun (_, ports) -> 
        { ports.[0] with TrgtId = Array.tryPick (fun p -> p.TrgtId) ports } )

let compsConns2portLst model (simData: SimulatorTypes.SimulationData) diagElLst : WaveSimPort []  =
    let portId2CIdInPN pId =
        match model.Diagram.GetCanvasState () with
        | Some s -> 
            List.map extractComponent (fst s)
            |> List.tryPick (fun c -> List.tryFindIndex (fun (p: Port) -> p.Id = pId) c.InputPorts
                                      |> function
                                         | Some i -> Some (c.Id, i)
                                         | None -> None )
        | None -> failwith "Called portId2cIdoutPN when Canvas State is None"

    diagElLst 
    |> List.toArray
    |> Array.collect (fun compEl -> 
            match compEl with
            | Comp c -> processComp simData (ComponentId c.Id)
            | Conn c -> match portId2CIdInPN c.Target.Id with 
                        | Some (cId, inPN) -> procIns simData (ComponentId cId) [| InputPortNumber inPN |]
                        | None -> [||] ) 
    |> remDuplicates

let selected2portLst model (simData: SimulatorTypes.SimulationData) : WaveSimPort [] =
    (simData, getSelected model) ||> compsConns2portLst model

let reloadablePorts (model: DiagramModelType.Model) (simData: SimulatorTypes.SimulationData) = 
    let inGraph port = Map.exists (fun key _ -> key = port.CId) simData.Graph
    Array.filter inGraph model.WaveSim.Ports
    |> Array.map (fun port -> 
        match port.TrgtId with
        | Some trgtId when Map.exists (fun key _ -> key = trgtId) simData.Graph ->   
            match List.tryFind (fun (cid,_) -> cid = trgtId) simData.Graph.[port.CId].Outputs.[port.OutPN] with
            | Some _ -> port
            | None -> {port with TrgtId = None}
        | _ -> {port with TrgtId = None} )

let limBits (name: string) : (int*int) option =
    match Seq.tryFind ((=)'[') name, Seq.tryFind ((=)':') name, Seq.tryFind ((=)']') name with
    | Some, Some, Some ->
       ( name.[Seq.findIndexBack ((=)'[') name + 1..Seq.findIndexBack ((=)':') name - 1 ], name.[Seq.findIndexBack ((=)':') name + 1..Seq.findIndexBack ((=)']') name - 1 ] )
       |> (fun (a,b) -> int a, int b)
       |> Some
    | _ -> None

let rec findName (simGraph: SimulatorTypes.SimulationGraph) ({ CId = compId; OutPN = outPortN; TrgtId = outputOpt }: WaveSimPort) = 
    let compLbl =
        match Map.tryFind compId simGraph with
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
        match driveOut simGraph compId (InputPortNumber n) with
        | Some (driveCompId, drivePortN) -> findName simGraph {CId = driveCompId; OutPN = drivePortN; TrgtId = None}
        | None -> failwith (compTypeStr + "input not connected")

    match simGraph.[compId].Type with
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
        match driveOut simGraph compId (InputPortNumber 0) with
        | Some (driveCompId, drivePortN) -> 
            match findName simGraph {CId = driveCompId; OutPN = drivePortN; TrgtId = None} with
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
    
    |> function
        | hd::tl -> 
            match outputOpt with
            | Some compId -> 
                 match simGraph.[compId].Label with
                 | ComponentLabel lbl -> (lbl + ": " + fst hd, snd hd)::tl
            | None -> hd::tl
        | [] -> failwith "empty (name, (msb*lsb)) list reached the end of findName"
        

let bitNums (a,b) = 
    match (a,b) with 
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb 
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let wSPort2Name simGraph p = 
    match findName simGraph p with
    | [el] -> fst el + bitNums (snd el)
    | lst when List.length lst > 0 -> 
        List.fold (fun st (name, bitLims) -> st + name + bitNums bitLims + ", ") "{ " lst
        |> (fun lbl -> lbl.[0..String.length lbl - 3] + " }" )  
    | _ -> failwith "Signal doesn't have a name source"

let extractWaveNames simData model (portFunc: Model -> SimulationData -> WaveSimPort []) =
    portFunc model simData
    |> Array.map (wSPort2Name simData.Graph)

let extractSimTime model portFunc simData =
    portFunc model simData 
    |> Array.map (fun {CId = compId; OutPN = portN; TrgtId = _} ->
        match Map.tryFind compId simData.Graph with 
        | Some simComp ->
            match Map.tryFind portN simComp.Outputs with
            | Some (hd::_) -> 
                let wD = simData.Graph.[fst hd].Inputs.[snd hd]
                Wire { NBits = uint (List.length wD)
                       BitData = simWireData2Wire wD } 
            | Some [] -> failwith "Output not connected" 
            | None -> failwith "Component doesn't have this output port number"
        | None -> failwith "ComponentId not in simulation graph")

let clkAdvance (sD : SimulatorTypes.SimulationData) = 
    feedClockTick sD.Graph
    |> (fun graph -> { sD with Graph = graph
                               ClockTickNumber = sD.ClockTickNumber + 1 })

let extractSimData simData nCycles = 
    (simData, [| 1u .. nCycles |])
    ||> Array.scan (fun s _ -> clkAdvance s) 

let extractWaveData model portFunc simDataArr : SimTime [] = 
    simDataArr
    |> Array.map (extractSimTime model portFunc)            

let makeSimData model = 
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ -> None, Some (Ok model.WaveSim)
    | _, None -> failwith "what? Cannot start a simulation without a project"
    | Some jsState, Some project ->
        let otherComponents =
            project.LoadedComponents
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> (fun x -> Some x, None)

(*let updateNames model portsFunc = 
    match makeSimData model with
    | Some (Ok simData), _ -> 
        Ok { model.WaveSim with WaveNames = extractWaveNames simData model portsFunc }
    | _, _ -> failwith "What? This case shouldn't happen"*)

let simLst model dispatch (portsFunc: Model -> SimulationData -> WaveSimPort []) = 
    match makeSimData model with
    | Some (Ok simData), _ -> 
        let ports' = portsFunc model simData
        let simData' = extractSimData simData model.WaveSim.LastClk
        Ok { model.WaveSim with SimData = simData'
                                WaveNames = extractWaveNames simData model portsFunc
                                WaveData = extractWaveData model portsFunc simData'
                                Selected = Array.map (fun _ -> true) ports' 
                                Ports = ports'
                                LastCanvasState = model.Diagram.GetCanvasState()}
    | Some (Error simError), _ ->
        if simError.InDependency.IsNone then
            // Highligh the affected components and connection only if
            // the error is in the current diagram and not in a
            // dependency.
            (simError.ComponentsAffected, simError.ConnectionsAffected)
            |> SetHighlighted |> dispatch
        Error simError
    | _, Some m -> m
    | _, _ -> failwith "What? This case shouldn't happen"

let viewOnDiagramButtons model dispatch =
    div [ canvasSmallMenuStyle ] [
        let canvasBut func label = 
            Button.button [ Button.Props [ canvasSmallButtonStyle; OnClick func ] ] 
                          [ str label ]
        canvasBut (fun _ -> model.Diagram.Undo ()) "< undo"
        canvasBut (fun _ -> model.Diagram.Redo ()) "redo >"
        canvasBut (fun _ -> copyAction model dispatch) "copy"
        canvasBut (fun _ -> pasteAction model) "paste"
        canvasBut (fun _ -> 
            simLst model dispatch selected2portLst |> StartWaveSim |> dispatch
            ChangeRightTab WaveSim |> dispatch) "simulate"
    ]
