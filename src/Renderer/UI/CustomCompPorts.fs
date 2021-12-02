module CustomCompPorts

(*
This module provides some functions that ensure consistency of instantiated custom components when changes are
made to ports in the underlying sheet. A dialiog is presented which allows instantiated components ports to
be updated correctly, with best efforts attempt to keep existing connections to each instance where ports remain
the same or where it can safely be deduced how ports have been renamed.

The code potentially makes chnages to every sheet in the project in the model, and writes out these chnages to disk.
*)

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open ModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open System


let printSheetNames (model:Model) =
    model.CurrentProj
    |> Option.map (fun p -> 
        printf $"SHEETS:{p.LoadedComponents |> List.map (fun ldc -> ldc.Name)}--->{p.OpenFileName}")
    |> ignore

//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//-------------------------------New-style project update and saving--------------------------//
//--------------------------------------------------------------------------------------------//



/// Save any changed sheets to disk in the project directory
let syncLoadedComponentsToDisk newProj oldProj =
    let needToSave ldc' ldc =
       (not <| compareCanvas 10. ldc'.CanvasState ldc.CanvasState) ||
       ldc'.WaveInfo <> ldc.WaveInfo
    let saveToDisk ldc =
        let state = ldc.CanvasState
        let waveInfo = ldc.WaveInfo
        saveStateToFile newProj.ProjectPath ldc.Name (state,waveInfo)
        |> ignore
        removeFileWithExtn ".dgmauto" oldProj.ProjectPath ldc.Name

    let nameOf sheet (ldc:LoadedComponent) = ldc.Name = sheet
    let oldLDCs = oldProj.LoadedComponents
    let newLDCs = newProj.LoadedComponents
    let sheets = List.distinct (List.map (fun ldc -> ldc.Name) (oldLDCs @ newLDCs))
    let sheetMap = 
        sheets
        |> List.map (fun sheet -> sheet, (List.tryFind (nameOf sheet) newLDCs, List.tryFind (nameOf sheet) oldLDCs))
        |> Map.ofList
    sheetMap
    |> Map.iter (fun name (optLdcNew, optLdcOld) ->
        match optLdcNew,optLdcOld with
        | Some ldcNew, Some ldcOld when needToSave ldcNew ldcOld ->
            saveToDisk ldcOld
        | Some _, Some _ -> ()
        | None, Some ldcOld -> 
            removeFileWithExtn ".dgm" oldProj.ProjectPath ldcOld.Name
        | Some ldcNew, None -> 
            saveToDisk ldcNew
        | None, None -> failwithf "What? Can't happen")

/// Return new model with project updated as per update function.
/// If p.LoadedComponents data is changed, for each sheet that is different
/// the sheet will be saved to disk.
/// This function should be used consistently to keep disk and project data
/// correct.
let updateProjectFiles (saveToDisk:bool) (update: Project -> Project) (model: Model) =
    match model.CurrentProj with
    | None -> model // do nothing in this case
    | Some p ->
        let p' = update p
        if saveToDisk then 
            syncLoadedComponentsToDisk p' p // write out to disk as needed
        {model with CurrentProj = Some p'}

//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//-------------------------------Custom Component Management----------------------------------//
//--------------------------------------------------------------------------------------------//


// Basic idea. When the I/Os of the current sheet are changed this may affect instances of the sheet embedded in other
// project sheets. Check for this whenever current sheet is saved (which will happen when Issie exits or the
// edited sheet is changed). Offer, in a dialog, to change all of the affected custom component instances to maintain
// compatibility. This will usually break the other sheets.
// all of this code uses the project data structure and (if needed) returns an updated structure.

type IODirection = InputIO | OutputIO



type Match = {
    MLabel: string
    MWidth: int
    MDir: IODirection
    }

type PortChange = {
    Direction: IODirection
    Old: (string * int) option
    New: (string * int) option
    Message: string
    }

/// Names and widths of ports, ordered. Input ports, Output ports.
type Signature = (string*int) list * (string*int) list

let getIOMatchFromSig (inputs, outputs)  =

    let makeSig dir (ios: (string * int) list) =
        ios
        |> List.map (fun (name,num) -> {MLabel = name; MWidth = num ; MDir = dir})
    (makeSig InputIO inputs) @
    (makeSig OutputIO outputs)


 


/// compare two I/O signature lists 
let ioCompareSigs (sig1: Signature) (sig2: Signature) =
    let map (sg: Signature) = 
        getIOMatchFromSig sg
        |> List.map (fun m -> (m.MDir, m.MLabel), m)
        |> Map.ofList

    let ioMap1 = sig1 |> map
    let ioMap2 = sig2 |> map
    let ioMap = mapUnion ioMap1 ioMap2
    let set1,set2 = set (mapKeys ioMap1), set (mapKeys ioMap2)
    let common = Set.intersect set1 set2
    let diff1 = set1 - set2
    let diff2 = set2 - set1
    mapKeys ioMap
    |> Array.map 
        (fun m -> 
            let getDetails m (ioMap: Map<IODirection*string,Match>) =
                let ma = Map.tryFind m ioMap
                let labWidth = ma |> Option.map (fun m -> m.MLabel, m.MWidth)
                labWidth
                    
            let newLW = getDetails m ioMap1
            let oldLW = getDetails m ioMap2
            let message =
                match newLW, oldLW with 
                |  Some (l1,w1), Some (l2,w2) when l1=l2 && w1=w2 -> "No Change"
                |  Some (l1,_), Some (l2,_) when l1 = l2 -> "Port width changed"
                | None, Some _ -> "Port and old connections deleted"
                | Some _, None -> "New Port will be added"
                | _ -> failwithf $"What? never happens: {newLW} {oldLW}"
            {
                Message = message
                Direction = fst m
                New = newLW
                Old = oldLW
            })

let guessAtRenamedPorts (matches: PortChange array)  : PortChange array =
    let matches = Array.toList matches
    let portsByWidthFiltered (ports: ((string*int) * PortChange) list) =
        ports
        |> List.groupBy (fst >> snd)
        |> List.collect (function |(width, [item]) -> [width,item] | _ -> [] )
        |> Map.ofList
  
    let additions = 
        matches
        |> List.collect (function | {New = Some (name,width); Old = None; Direction = dir } as m -> [(name,width), m] | _ -> [])
        |> portsByWidthFiltered

    let deletions = 
        matches
        |> List.collect (function | {Old = Some (name,width); New = None; Direction = dir } as m -> [(name,width), m] | _ -> [])
        |> portsByWidthFiltered

    let guessedRenames, deletedMatches =
        Set.intersect (Set (mapKeys additions)) (Set (mapKeys deletions))
        |> Set.toList
        |> List.map (fun n -> 
            {
                New = Some (fst additions[n])
                Old = Some (fst deletions[n])
                Direction = (additions[n] |> snd).Direction
                Message = "This appears to be a renamed port, connections will be kept"
            }, [snd additions[n]; snd deletions[n]])
        |> List.unzip

    let deletedMatches = List.concat deletedMatches
    // the final result
    Set matches - Set deletedMatches
    |> Set.union (Set guessedRenames)
    |> Set.toArray

        
    



let findInstancesOfCurrentSheet (project:Project) =
    let thisSheet = project.OpenFileName
    let ldcs = project.LoadedComponents
    let getInstance (comp:Component) =
        match comp.Type with
        | Custom ({Name=thisSheet} as cType) -> Some (ComponentId comp.Id, cType)
        | _ -> None

    let getSheetInstances (ldc:LoadedComponent) =
        fst ldc.CanvasState
        |> List.choose getInstance

    ldcs
    |> List.collect (fun ldc -> 
        getSheetInstances ldc
        |> List.map (fun ins -> ldc.Name, ins))


type Deps =
    | NoDependents
    | OneSig of ((string * int) list * (string * int) list) * (string * (ComponentId * CustomComponentType)) list
    | Mixed of (string * int) list

let getDependentsInfo (p: Project)  =
    let instances = findInstancesOfCurrentSheet p
    let gps = 
        instances
        |> List.groupBy (fun (_, (_,{InputLabels=ips; OutputLabels=ops})) -> (ips |> List.sort), (ops |> List.sort))
        |> List.sortByDescending (fun (tag,items) -> items.Length)

    match gps with
    | [] -> NoDependents // no dependencies - nothing to do
    | [sg, items] -> OneSig(sg, items) // normal case, all dependencies have same signature
    | _ -> // dependencies have mixed signatures
        instances
        |> List.groupBy fst
        |> List.map (fun (tag, lst) -> tag, lst.Length)
        |> Mixed          

let makePortName (nameWidth :(string*int) option) =
    match nameWidth with
    | None -> ""
    | Some (name,w) -> $"%s{name}({w-1}:{0})"
    |> str

/// returns IO signature of current sheet, and all its instances in other sheets
let getDependents (model:Model)  =
    mapOverProject None model <| fun p ->
         printfn "depcheck2a"
         let sheetName = p.OpenFileName
         let newSig = 
             p.LoadedComponents
             |> List.find (fun ldc -> ldc.Name = sheetName)
             |> (fun ldc -> parseDiagramSignature ldc.CanvasState)
         printfn "depcheck2b"
         let instances =
             p.LoadedComponents
             |> List.filter (fun ldc -> ldc.Name <> sheetName)
             |> List.collect (fun ldc -> 
                 fst ldc.CanvasState
                 |> List.collect (
                     function 
                         | {Type = Custom { Name=name; InputLabels=ins; OutputLabels=outs}
                            Id = cid} when name = sheetName-> [ldc.Name, cid,  (ins,outs)]
                         | _ -> []))
         printfn "depcheck2c"
         Some(newSig, instances)

let dependencyDoesNotMatchSignature newSig oldSig =
    let sortLists (a,b) = a, b // we now require signature order to match
    sortLists newSig <> sortLists oldSig


/// check whether any instance dependent on current sheet has different signature from current
let getOutOfDateDependents (model:Model) =
    match getDependents model with
    | None
    | Some(_, []) -> None
    | Some (newSig, (_,_,sg) :: _otherInstances) as deps when 
            dependencyDoesNotMatchSignature newSig sg-> deps
    | _ -> None


/// Return canvasState updated with bad connections that have lost either of their connecting components deleted
let deleteIncompleteConnections ((comps,conns): CanvasState) =
    let arrayOfIds (pL: Port list) =
        Array.ofList pL
        |> Array.map (fun p -> p.Id)
    let okPorts = 
        Array.ofList comps
        |> Array.collect (fun comp ->
            Array.append (arrayOfIds comp.InputPorts) (arrayOfIds comp.OutputPorts))
        |> Set
    let conns' = List.filter (fun (conn:Connection) -> 
        Set.contains conn.Source.Id okPorts && 
        Set.contains conn.Target.Id okPorts) conns
    if conns <> conns' then
        printfn "%d Connections deleted" (conns.Length - conns'.Length)
    comps,conns'

// Updating custom component instances changes the input and output port specifications.
// The CanvasState inside the custom component is always looked up from the corresponding sheet
// and not contained in the instance.
// Inputs and outputs ports have separate lists but work in the same way. A custom component instance
// is defined in TWO records: comp: Component and ct: CustomComponentType.
// ct = match comp.Type with | Custom ct -> ct (where for a custom component instance this match always succeeds)
// ct.InputLabels: (string * int) list -- associates an input port name with the port width. The position in the Inputlabels list is the port number
// comp.InputPorts: Port list -- input port list contains the input ports, each port record contains .PortNumber its number and .Id its (unique) id.
// Input port numbers are contiguous set of integers starting from 0.
// NB output port numbers are similar, thus a number does not uniquely identify a port on a component.
// Port names are likely to be unique but (maybe) do not have to be for the same reason.

/// Change the items x in lst where uPred x with x -> uFunc x.
/// Useful to replace one item of a list
let listUpdate (uPred: 'a -> bool) (uFunc: 'a -> 'a) (lst: 'a list) =
    lst
    |> List.map (fun item -> if uPred item then uFunc item else item)

type PortInfo = (string * int) list * Port list

/// Return updated custom component with ports changed as per change
/// If a port is deleted any corresponding connections must be deleted
/// to keep the CanvasState consistent. That is done elsewhere, since
/// deleting not fully connected connections is a straightforward operation
/// on CanvasState, as done by deleteIncompleteConnections
let changeInstance (comp:Component) (change: PortChange) =

    let updateInfo (dir: IODirection) (f: PortInfo -> PortInfo) (comp: Component)=
        match dir with
        | InputIO ->
            let labels,ct = 
                match comp.Type with 
                | Custom ct -> ct.InputLabels,ct
                | cType -> failwithf $"What? '{cType}' not allowed"
            let ports = comp.InputPorts
            let (labels,ports) = f (labels,ports)
            {comp with InputPorts = ports; Type = Custom {ct with InputLabels = labels }}
        | OutputIO ->
            let labels,ct = 
                match comp.Type with 
                | Custom ct -> ct.OutputLabels,ct 
                | cType -> failwithf $"What? '{cType}' not allowed"
            let ports = comp.OutputPorts
            let (labels, ports) = f (labels,ports)
            {comp with OutputPorts = ports; Type = Custom {ct with OutputLabels = labels }}

    /// To change a port width only InputLabels (or OutputLabels) need change
    let changePortWidth (dir: IODirection) (name: string) (newWidth:int) (comp: Component) = 
        let upf = fun (labels,ports) ->
            listUpdate (fun (s,_) -> s = name) (fun (s,_) -> (s,newWidth)) labels, ports
        updateInfo dir upf comp

    /// To rename a port width only InputLabels (or OutputLabels) need change
    let changePortName (dir: IODirection) (newName: string) (oldName:string) (comp: Component) = 
        let upf = fun (labels,ports) ->
            listUpdate (fun (s,_) -> s = oldName) (fun (_,w) -> (newName,w)) labels, ports
        updateInfo dir upf comp

    /// To add a port we add it to both lists, creating a new Port record with unique id, and using the next available port number
    let addPort (dir: IODirection) (name:string) (width:int) (comp:Component) =
        let upf = fun ((labels,ports):PortInfo) ->
            let labels = labels @ [name,width]
            let newPort:Port = 
                {
                    Id = EEEHelpers.uuid ()
                    PortNumber = Some ports.Length // next available number
                    HostId = comp.Id
                    PortType = match dir with | InputIO -> PortType.Input | OutputIO -> PortType.Output
                }
            let ports = ports @ [newPort]
            labels,ports
        updateInfo dir upf comp

    /// To delete a port we remove it from both lists but also must renumber all of the port records to keep numbers aligned
    let deletePort (dir: IODirection) (name:string) (comp:Component) =
        let upf = fun ((labels,ports):PortInfo) ->
            // first get the port number of the label we will delete
            let portNum = List.findIndex (fun (s,_) -> s = name) labels
            // delete the label we do not want
            let labels = List.filter (fun (s,_) -> s <> name) labels
            // renumber the ports contiguously, preserving order
            let ports = 
                List.filter (fun (port:Port) -> port.PortNumber <> Some portNum) ports
                |> List.sortBy (fun port -> port.PortNumber)
                |> List.mapi (fun i port -> {port with PortNumber = Some i})
            printfn $"deleteport:{labels.Length},{ports.Length}"
            labels,ports
        updateInfo dir upf comp

    let dir = change.Direction
    match change.New, change.Old with
    | x,y when x = y -> 
        comp // no change in this case
    | Some (newName,width'), Some(oldName,width) when width'=width ->
        // a guessed rename operation
        changePortName dir newName oldName comp
    | Some(name',newWidth), Some (name,oldWidth) when name=name' -> 
        changePortWidth dir name newWidth comp
    | Some newC, Some oldC -> 
        failwithf $"What? Change with new={newC} and old = {oldC} should not be possible"
    | Some(name,width), None -> 
        addPort dir name width comp
    | None, Some(name,width) -> 
        deletePort dir name comp
    | newC, oldC -> 
        failwithf $"What? Change with new={newC} and old = {oldC} should not be possible"
    
/// Make changes to ccomponent cid on sheet converting old ports oldSig to new ports newSig
let updateInstance (newSig: Signature) (sheet:string,cid:string,oldSig:Signature) (p: Project) =
    /// Assume that name and bit number changes have been made. Deal with any needed reordering.
    let reorderInstancePorts (newSig: Signature) (comp: Component) =
        let reorderPorts newNames oldNames (oldPorts: Port list) =
            newNames
            |> List.map (fun (name,_) -> List.findIndex (fun (name',_) -> name'=name) oldNames)
            |> List.map (fun n -> oldPorts[n])
            |> List.mapi (fun i p -> {p with PortNumber = Some i})
        match comp.Type with
        | Custom ct ->
                if oldSig = newSig then 
                    printfn "Order matches!"
                    comp
                elif mapPair List.sort newSig = mapPair List.sort oldSig then
                    printfn $"Reordering {comp.Label}"
                    let oldSig = ct.InputLabels, ct.OutputLabels
                    let newIn,newOut = newSig
                    let oldIn,oldOut = oldSig
                    let newInPorts = reorderPorts newIn oldIn comp.InputPorts
                    let newOutPorts = reorderPorts newOut oldOut comp.OutputPorts
                    let ct' = {ct with InputLabels = fst newSig; OutputLabels = snd newSig}
                    {comp with Type = Custom ct'; InputPorts=newInPorts; OutputPorts=newOutPorts}

                else
                    printfn "What? Signatures do not match after changes are made"
                    comp
        | _ -> comp // no change (should never happen?)
#if ASSERTS
    assertThat 
        (sheet <> p.OpenFileName)
        $"What? Instances to be changed in {sheet} must not be in custom \
        component sheet{p.OpenFileName}"
#endif
    let ldc =
        p.LoadedComponents
        |> List.find (fun ldc -> ldc.Name = sheet)
    let (comps,conns) = ldc.CanvasState
    let comp =
        comps |> List.find (fun comp -> comp.Id = cid)
    let changes = 
        ioCompareSigs newSig oldSig
        |> guessAtRenamedPorts
    let comp' =
        (comp, changes)
        ||> Array.fold (fun comp change ->
            let comp'' = changeInstance comp change
            comp''
            )
        |> reorderInstancePorts newSig
    let comps' =
        comps
        |> List.map (fun comp -> if comp.Id = cid then comp' else comp)
    let ldc' = {ldc with CanvasState = deleteIncompleteConnections (comps',conns)}
    let ldcLst = ldc' :: List.except [ldc] p.LoadedComponents
    {p with LoadedComponents = ldcLst}



            
let updateDependents (newSig: Signature) (instances:(string*string*Signature) list) model dispatch =
    match model.CurrentProj with
    | None -> ()
    | Some p ->
        (p,instances)
        ||> List.fold (fun p instance -> updateInstance newSig instance p)
        |> SetProject
        |> dispatch

let checkCanvasStateIsOk (model:Model) =
    mapOverProject false model (fun p ->
        let ldc = List.find (fun ldc -> ldc.Name = p.OpenFileName) p.LoadedComponents
        let comps,conns = ldc.CanvasState
        let ioNames =
            comps
            |> List.filter (fun comp -> match comp.Type with | Input _ | Output _ -> true | _ -> false)
            |> List.map (fun comp -> comp.Label)
        ioNames.Length = (List.distinct ioNames).Length
        )
    
/// returns a popup function to show the dependents update dialog if this is needed
/// this dialog drives all subsequent work changing custom component instances
let optCurrentSheetDependentsPopup (model: Model) =
        printfn "depcheck1"
        let sheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName)
        if not <| checkCanvasStateIsOk model then
            None // do nothing if IOs are not currently valid. Can this ever happen?
        else     
            printfn "depcheck2"
            match getOutOfDateDependents model  with
            | None -> None
            | Some (newSig, (((firstSheet,firstCid,firstSig) :: rest) as instances)) ->
                let depSheets = 
                    instances
                    |> List.map (fun (sheet,_,_) -> sheet)
                    |> List.distinct
                    |> String.concat ","
                let changes = 
                    ioCompareSigs newSig firstSig
                    |> guessAtRenamedPorts
                let whatChanged = 
                    match changes |> Array.exists (fun ch -> ch.Old <> ch.New) with
                    | false -> "the vertical order of inputs or outputs"
                    | true -> "the inputs or outputs"
                let headCell heading =  th [ ] [ str heading ]
                let makeRow (change:PortChange) = 
                    tr []
                        [
                       
                            td [] [str (if change.Direction = InputIO  then "Input" else "Output")]
                            td [] [makePortName change.New]
                            td [] [makePortName change.Old]
                            td [] [str change.Message]
                        ]
                let body = 
                    div [Style [ MarginTop "15px" ] ] 
                        [
                            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [str $"{sheet}"]
                            str $"You have changed the {whatChanged} of the current '{sheet}' sheet. "
                            br []
                            str "This dialog will automatically update all dependent sheets to match this. "
                            br []
                            str $"The '{sheet}' sheet is instantiated as a symbol {instances.Length} times in dependent sheets: '{depSheets}'. "
                            str $"If you do not automatically update the symbols you will need to delete and recreate each one."
                            br []
                            str "If you automatically update symbols wires that no longer match will be autorouted correctly when you next load each sheet"
                            br []
                            Table.table [
                                   Table.IsHoverable                               
                                   Table.IsBordered
                                   Table.IsNarrow
                                   Table.Props [Style [ MarginTop "15px" ]]]
                                [ 
                                    thead [] [ tr [] (List.map headCell ["Type" ;"New port"; "Old port" ; "Change"]) ]
                                    tbody []   (Array.map makeRow  changes) 
                                ]
                        ]

                let buttonAction isUpdate dispatch  _ =
                    if isUpdate then
                        updateDependents newSig instances model dispatch
                        mapOverProject () model  (fun p -> saveAllProjectFilesFromLoadedComponentsToDisk p)
                    dispatch <| ClosePopup
                choicePopupFunc 
                    "Update All Sheet Instances" 
                    (fun _ -> body)
                    "Update all instances" 
                    "Save the sheet without updating instances" 
                    buttonAction 
                |> Some
              

            | _ -> failwithf "What? Impossible"




       

 



