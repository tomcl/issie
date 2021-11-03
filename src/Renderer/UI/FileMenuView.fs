//(*
//    FileMenuView.fs
//
//    View for the top menu, and related functionalities.
//*)

module FileMenuView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open DiagramStyle
open ModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open System
open Electron

//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//-------------------------------New-style project update and saving--------------------------//
//--------------------------------------------------------------------------------------------//

let displayAlertOnError (dispatch: Msg -> Unit) res =
    match res with
    | Error e -> 
        dispatch <| SetFilesNotification (errorFilesNotification e)
    | _ -> ()

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
                New = Some (fst additions.[n])
                Old = Some (fst deletions.[n])
                Direction = (additions.[n] |> snd).Direction
                Message = "This appears to be a renamed port, connections will be kept"
            }, [snd additions.[n]; snd deletions.[n]])
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
    let sortLists (a,b) = List.sort a, List.sort b
    sortLists newSig <> sortLists oldSig



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
    

let updateInstance (newSig: Signature) (sheet:string,cid:string,oldSig:Signature) (p: Project) =
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
let optCurrentSheetDependentsPopup (model: Model) =
        printfn "depcheck1"
        let sheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName)
        if not <| checkCanvasStateIsOk model then
            None // do nothing if IOs are not currently valid
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
                printfn "depcheck3"
                let changes = 
                    ioCompareSigs newSig firstSig
                    |> guessAtRenamedPorts
                printfn "depcheck4"
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
                            str $"You have changed the inputs or outputs of the current '{sheet}' sheet. "
                            br []
                            str "This dialog will automatically update all dependent sheets to match this. "
                            br []
                            str $"The '{sheet}' sheet is instantiated as a component {instances.Length} times in dependent sheets: '{depSheets}'. "
                            str $"If you do not automatically update the instances you will need to delete and recreate each one."
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


       

 




//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//---------------------Code for CanvasState comparison and FILE BACKUP------------------------//
//--------------------------------------------------------------------------------------------//

/// Works out number of components and connections changed between two LoadedComponent circuits
/// a new ID => a change even if the circuit topology is identical. Layout differences do not
/// mean changes, as is implemented in the reduce functions which remove layout.
let quantifyChanges (ldc1:LoadedComponent) (ldc2:LoadedComponent) =
    let comps1,conns1 = ldc1.CanvasState
    let comps2,conns2 = ldc2.CanvasState
    let reduceComp comp1 =
        {comp1 with X=0;Y=0}
    let reduceConn conn1 =
        {conn1 with Vertices = []}
    /// Counts the number of unequal items in the two lists.
    /// Determine equality from whether reduce applied to each item is equal
    let unmatched reduce lst1 lst2 =
        let mapToSet = List.map reduce >> Set
        let rL1, rL2 = mapToSet lst1, mapToSet lst2
        Set.union (Set.difference rL1 rL2) (Set.difference rL2 rL1)
        |> Set.count
    unmatched reduceComp comps1 comps2, unmatched reduceConn conns1 conns2

////------------------------------------------Backup facility-------------------------------------------//

let writeComponentToFile comp =
    let data =  stateToJsonString (comp.CanvasState,comp.WaveInfo)
    writeFile comp.FilePath data

/// return an option containing sequence data and file name and directory of the latest
/// backup file for given component, if it exists.
let readLastBackup comp =
    let path = pathWithoutExtension comp.FilePath 
    let baseN = baseName path
    let backupDir = pathJoin [| dirName path ; "backup" |]
    latestBackupFileData backupDir baseN
    |> Option.map (fun (seq, fName) -> seq, fName, backupDir)
  
/// Write Loadedcomponent comp to a backup file if there has been any change.
/// Overwrite the existing backup file only if it is a small, and recent, change.
/// Parameters determine thresholds of smallness and recency
/// return () - display an error if the write goes wrong.
let writeComponentToBackupFile (numCircuitChanges: int) (numHours:float) comp (dispatch: Msg -> Unit)= 
    let nSeq, backupFileName, backFilePath =
        match readLastBackup comp with
        | Some( n, fp, path) -> n+1,fp, path
        | None -> 0, "", pathJoin [|comp.FilePath; "backup"|]
    //printfn "seq=%d,name=%s,path=%s" nSeq backupFileName backFilePath
    let wantToWrite, oldFile =
        if backupFileName = "" then
            true, None
        else
            let oldBackupFile = pathJoin [|backFilePath ; backupFileName|]
            match tryLoadComponentFromPath (oldBackupFile) with
            | Ok comp' ->
                if not (compareIOs comp comp') then
                    true, None // need to save, to a new backup file
                elif compareCanvas 10000. comp.CanvasState comp'.CanvasState then
                    false, None // no need for a new backup
                else
                    let nComps,nConns = quantifyChanges comp' comp
                    let interval = comp.TimeStamp - comp'.TimeStamp
                    if interval.TotalHours > numHours || nComps + nConns  > numCircuitChanges then
                        true, None
                    else
                        true, Some oldBackupFile
                        
            | err -> 
                printfn "Error: writeComponentToBackup\n%A" err
                true, None
    if wantToWrite then
        let timestamp = System.DateTime.Now
        let backupPath =
                // work out new path to write based on time.
                let path = pathWithoutExtension comp.FilePath
                let baseN = baseName path
                let ds = EEExtensions.String.replaceChar '/' '-' (timestamp.ToShortDateString())
                let suffix = EEExtensions.String.replaceChar ' ' '-' (sprintf "%s-%02dh-%02dm" ds timestamp.Hour timestamp.Minute)
                let backupDir = pathJoin [| dirName path ; "backup" |]
                ensureDirectory <| pathJoin [| dirName path ; "backup" |]
                pathJoin [| dirName path ; "backup" ; sprintf "%s-%03d-%s.dgm" baseN nSeq suffix |]
        // write the new backup file
        {comp with 
            TimeStamp = timestamp
            FilePath = backupPath}
        |> writeComponentToFile
        |> displayAlertOnError dispatch
        /// if necessary delete the old backup file
        match oldFile with
        | Some oldPath when oldPath <> backupPath ->
            if Node.Api.fs.existsSync (Fable.Core.U2.Case1 oldPath) then
                Node.Api.fs.unlink (Fable.Core.U2.Case1 oldPath, ignore) // Asynchronous.
            else
                ()
        | _ -> ()

/// returns a WaveSimModel option if a file is loaded, otherwise None
let currWaveSimModel (model: Model) =
    match getCurrFile model with
    | Some fileName when Map.containsKey fileName (fst model.WaveSim) -> Some (fst model.WaveSim).[fileName]
    | _ -> None

let private displayFileErrorNotification err dispatch =
    let note = errorFilesNotification err
    dispatch <| SetFilesNotification note

/// Send messages to change Diagram Canvas and specified sheet waveSim in model
let private loadStateIntoModel (compToSetup:LoadedComponent) waveSim ldComps model dispatch =
    // it seems still need this, however code has been deleted!
    //Sheet.checkForTopMenu () // A bit hacky, but need to call this once after everything has loaded to compensate mouse coordinates.
    
    let name = compToSetup.Name
    let components, connections = compToSetup.CanvasState
    //printfn "Loading..."
    let msgs = 
        [
            SetHighlighted([], []) // Remove current highlights.
    
            // Clear the canvas.
            Sheet Sheet.ResetModel
            Sheet (Sheet.Wire BusWire.ResetModel)
            Sheet (Sheet.Wire (BusWire.Symbol (Symbol.ResetModel ) ) )
    
            // Finally load the new state in the canvas.
            SetIsLoading true
            //printfn "Check 1..."
    
            //Load components
            Sheet (Sheet.Wire (BusWire.Symbol (Symbol.LoadComponents components )))
            Sheet Sheet.UpdateBoundingBoxes
    
            Sheet (Sheet.Wire (BusWire.LoadConnections connections))

            Sheet Sheet.FlushCommandStack // Discard all undo/redo.
            // Run the a connection widths inference.
            //printfn "Check 4..."
    
            Sheet (Sheet.Wire (BusWire.BusWidths))
            // JSdispatch <| InferWidths()
            //printfn "Check 5..."
            // Set no unsaved changes.
    
        
            JSDiagramMsg (SetHasUnsavedChanges false)
            // set waveSim data
            SetWaveSimModel(name, waveSim)
            (
                {
                    ProjectPath = dirName compToSetup.FilePath
                    OpenFileName =  compToSetup.Name
                    LoadedComponents = ldComps
                }
                |> SetProject) // this message actually changes the project in model
            SetWaveSimIsOutOfDate true
            SetIsLoading false 
        
            //printfn "Check 6..."
        ]

    //INFO - Currently the spinner will ALWAYS load after 'SetTopMenu x', probably it is the last command in a chain
    //Ideally it should happen before this, but it is not currently doing this despite the async call
    //This will set a spinner for both Open project and Change sheet which are the two most lengthly processes
    dispatch <| (Sheet (Sheet.SetSpinner true))
    dispatch <| SendSeqMsgAsynch msgs
    
/// Return LoadedComponents with sheet name updated according to setFun.
/// Do not update model. 
let updateLoadedComponents name (setFun: LoadedComponent -> LoadedComponent) (lcLst: LoadedComponent list) (dispatch: (Msg -> Unit))=
    let n = List.tryFindIndex (fun (lc: LoadedComponent) -> lc.Name = name) lcLst
    match n with
    | None -> 
        printf "In updateLoadedcomponents can't find name='%s' in components:%A" name lcLst
        lcLst
    | Some n ->
        let oldLc = lcLst.[n]
        let newLc = setFun oldLc
        writeComponentToBackupFile 0 1. oldLc dispatch
        List.mapi (fun i x -> if i = n then newLc else x) lcLst

/// return current project with current sheet updated from canvas if needed.
/// Do not update model.
let updateProjectFromCanvas (model:Model) (dispatch:Msg -> Unit) =
    match model.Sheet.GetCanvasState() with
    | ([], []) -> model.CurrentProj
    | canvasState ->  
        canvasState
        |> fun canvas ->
            let inputs, outputs = parseDiagramSignature canvas
            let setLc lc =
                { lc with
                    CanvasState = canvas
                    InputLabels = inputs
                    OutputLabels = outputs
                }
            model.CurrentProj
            |> Option.map (fun p -> 
                {   
                    p with LoadedComponents = updateLoadedComponents p.OpenFileName setLc p.LoadedComponents dispatch
                })


/// extract SavedwaveInfo from model to be saved
let getSavedWave (model:Model) : SavedWaveInfo option = 
    match currWaveSimModel model with
    | Some wSModel -> waveSimModel2SavedWaveInfo wSModel |> Some
    | None -> None

/// add waveInfo to model
let setSavedWave compIds (wave: SavedWaveInfo option) model : Model =
    match wave, getCurrFile model with
    | None, _ -> model
    | Some waveInfo, Some fileName -> 
        { model with WaveSim = Map.add fileName (savedWaveInfo2WaveSimModel waveInfo) 
                                                (fst model.WaveSim), 
                               snd model.WaveSim }
    | Some waveInfo, _ -> model

/// Save the sheet currently open, return  the new sheet's Loadedcomponent if this has changed.
/// Do not change model.
/// update Symbol model with new RAM contents.
let saveOpenFileAction isAuto model (dispatch: Msg -> Unit)=
    match model.Sheet.GetCanvasState (), model.CurrentProj with
    | _, None -> None
    | canvasState, Some project ->
        // "DEBUG: Saving Sheet"
        // printfn "DEBUG: %A" project.ProjectPath
        // printfn "DEBUG: %A" project.OpenFileName
                
        let savedState = canvasState, getSavedWave model
        if isAuto then
            failwithf "Auto saving is no longer used"
            None
        else 
            saveStateToFile project.ProjectPath project.OpenFileName savedState
            |> displayAlertOnError dispatch
            removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName
            let origLdComp =
                project.LoadedComponents
                |> List.find (fun lc -> lc.Name = project.OpenFileName)
            let savedWaveSim =
                Map.tryFind project.OpenFileName (fst model.WaveSim)
                |> Option.map waveSimModel2SavedWaveInfo
            let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData canvasState origLdComp.FilePath DateTime.Now savedWaveSim 
            let newState =
                canvasState
                |> (fun (comps, conns) -> 
                        comps
                        |> List.map (fun comp -> 
                            match List.tryFind (fun (c:Component) -> c.Id=comp.Id) ramCheck with
                            | Some newRam -> 
                                // TODO: create consistent helpers for messages
                                dispatch <| Sheet (Sheet.Wire (BusWire.Symbol (Symbol.WriteMemoryType (ComponentId comp.Id, newRam.Type))))
                                newRam
                            | _ -> comp), conns)
            writeComponentToBackupFile 4 1. newLdc dispatch
            Some (newLdc,newState)
        
/// save current open file, updating model etc, and returning the loaded component and the saved (unreduced) canvas state
let saveOpenFileActionWithModelUpdate (model: Model) (dispatch: Msg -> Unit) =
    let opt = saveOpenFileAction false model dispatch
    let ldcOpt = Option.map fst opt
    let state = Option.map snd opt |> Option.defaultValue ([],[])
    match model.CurrentProj with
    | None -> failwithf "What? Should never be able to save sheet when project=None"
    | Some p -> 
        // update loaded components for saved file
        updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
        |> (fun lc -> {p with LoadedComponents=lc})
        |> SetProject
        |> dispatch

    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch
    dispatch FinishUICmd
    opt

let private getFileInProject name project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name = name)

let private isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Create a new empty .dgm file and return corresponding loaded component.
let private createEmptyDiagramFile projectPath name =
    createEmptyDgmFile projectPath name |> ignore

    {   
        Name = name
        TimeStamp = System.DateTime.Now
        WaveInfo = None
        FilePath = pathJoin [| projectPath; name + ".dgm" |]
        CanvasState = [],[]
        InputLabels = []
        OutputLabels = []
    }


let createEmptyComponentAndFile (pPath:string)  (sheetName: string): LoadedComponent =
    createEmptyDgmFile pPath sheetName |> ignore
    {
        Name=sheetName
        WaveInfo = None
        TimeStamp = DateTime.Now
        FilePath= pathJoin [|pPath; sprintf "%s.dgm" sheetName|]
        CanvasState=([],[])
        InputLabels = []
        OutputLabels = []
    }
    

/// Load a new project as defined by parameters.
/// Ends any existing simulation
/// Closes WaveSim if this is being used
let setupProjectFromComponents (sheetName: string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
    let compToSetup =
        match ldComps with
        | [] -> failwithf "setupProjectComponents must be called with at least one LoadedComponent"
        | comps ->
            // load sheetName
            match comps |> List.tryFind (fun comp -> comp.Name = sheetName) with
            | None -> failwithf "What? can't find sheet %s in loaded sheets %A" sheetName (comps |> List.map (fun c -> c.Name))
            | Some comp -> comp
    match model.CurrentProj with
    | None -> ()
    | Some p ->
        dispatch EndSimulation // Message ends any running simulation.
        // TODO: make each sheet wavesim remember the list of waveforms.
    let waveSim = 
        compToSetup.WaveInfo
        |> Option.map savedWaveInfo2WaveSimModel 
        |> Option.defaultValue (ModelType.initWS [||] Map.empty)

    // TODO
    loadStateIntoModel compToSetup waveSim ldComps model dispatch
    {
        ProjectPath = dirName compToSetup.FilePath
        OpenFileName =  compToSetup.Name
        LoadedComponents = ldComps
    }
    |> SetProject // this message actually changes the project in model
    |> dispatch

/// Open the specified file, saving the current file if needed.
/// Creates messages sufficient to do all necessary model and diagram change
/// Terminates a simulation if one is running
/// Closes waveadder if it is open
let private openFileInProject' saveCurrent name project (model:Model) dispatch =
    let newModel = {model with CurrentProj = Some project}
    match getFileInProject name project with
    | None -> 
        log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some lc ->
        match updateProjectFromCanvas model dispatch with
        | None -> failwithf "What? current project cannot be None at this point in openFileInProject"
        | Some p ->
            let updatedModel = {model with CurrentProj = Some p}
            let ldcs =
                if saveCurrent then 
                    let opt = saveOpenFileAction false updatedModel dispatch
                    let ldcOpt = Option.map fst opt
                    let ldComps = updateLdCompsWithCompOpt ldcOpt project.LoadedComponents
                    let reducedState = Option.map snd opt |> Option.defaultValue ([],[])
                    SetHasUnsavedChanges false
                    |> JSDiagramMsg
                    |> dispatch
                    ldComps
                else
                    project.LoadedComponents
            setupProjectFromComponents name ldcs newModel dispatch

let openFileInProject name project (model:Model) dispatch =
    openFileInProject' true name project (model:Model) dispatch
    dispatch FinishUICmd


/// return a react warning message if name if not valid for a sheet Add or Rename, or else None
let maybeWarning dialogText project =
    let redText txt = Some <| div [ Style [ Color "red" ] ] [ str txt ]
    if isFileInProject dialogText project then
        redText "This sheet already exists." 
    elif dialogText.StartsWith " " || dialogText.EndsWith " " then
        redText "The name cannot start or end with a space."
    elif String.exists ((=) '.') dialogText then
        redText "The name cannot contain a file suffix."
    elif not <| String.forall (fun c -> Char.IsLetterOrDigit c || c = ' ') dialogText then
        redText "The name must be alphanumeric."
    elif ((dialogText |> Seq.tryItem 0) |> Option.map Char.IsDigit) = Some true then
        redText "The name must not start with a digit"
    else None


/// rename a sheet
let renameSheet oldName newName (model:Model) dispatch =

    let renameComps oldName newName (comps:Component list) : Component list = 
        comps
        |> List.map (fun comp -> 
            match comp with 
            | {Type= Custom ({Name = compName} as customType)} when compName = oldName-> 
                {comp with Type = Custom {customType with Name = newName} }
            | c -> c)

    let renameCustomComponents newName (ldComp:LoadedComponent) =
        let state = ldComp.CanvasState
        {ldComp with CanvasState = renameComps oldName newName (fst state), snd state}

    let renameSheetsInProject oldName newName proj =
        {proj with
            OpenFileName = if proj.OpenFileName = oldName then newName else proj.OpenFileName
            LoadedComponents =
                proj.LoadedComponents
                |> List.map (fun ldComp -> 
                    match ldComp with
                    | {Name = lcName} when lcName = oldName -> 
                        {ldComp with Name=newName}
                    | _ ->
                        renameCustomComponents newName ldComp )
        }
    match updateProjectFromCanvas model dispatch with
    | None -> 
        failwithf "What? current project cannot be None at this point in renamesheet"
    | Some p ->
        let updatedModel = {model with CurrentProj = Some p}
        let opt = saveOpenFileAction false updatedModel dispatch
        let ldcOpt = Option.map fst opt
        let ldComps = updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
        let reducedState = Option.map snd opt |> Option.defaultValue ([],[])
        SetHasUnsavedChanges false
        |> JSDiagramMsg
        |> dispatch
        [".dgm";".dgmauto"] |> List.iter (fun extn -> 
            renameFile extn p.ProjectPath oldName newName
            |> displayAlertOnError dispatch)
        let proj' = renameSheetsInProject oldName newName p
        setupProjectFromComponents proj'.OpenFileName proj'.LoadedComponents model dispatch
        /// save all the other files
        saveAllProjectFilesFromLoadedComponentsToDisk proj'
        dispatch FinishUICmd


/// rename file
let renameFileInProject name project model dispatch =
    match model.CurrentProj, getCurrentWSMod model with
    | None,_ -> log "Warning: renameFileInProject called when no project is currently open"
    | Some project, Some ws when ws.WSViewState<>WSClosed ->
        displayFileErrorNotification "Sorry, you must close the wave simulator before renaming design sheets!" dispatch
        switchToWaveEditor model dispatch
    | Some project, _ ->
        // Prepare dialog popup.
        let title = "Rename sheet in project"

        let before =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData

                div []
                    [ 
                      str <| "Warning: the current sheet will be saved during this operation."
                      br []
                      str <| "Names of existing components in other sheets that use the renamed sheet will still reflect the old sheet name.";
                      str <| " You may change names manually if you wish, operation does not depend on the name."
                      br []; br []
                      str <| sprintf "Sheet %s will be renamed as %s:" name dialogText
                      br []; br []
                      //str <| dialogText + ".dgm"
                      Option.defaultValue (div [] []) (maybeWarning dialogText project)]

        let placeholder = "New name for design sheet"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Rename"

        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                // Create empty file.
                let newName = (getText dialogData).ToLower()
                // rename the file in the project.
                renameSheet name newName model dispatch
                dispatch ClosePopup

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "")

        dialogPopup title body buttonText buttonAction isDisabled dispatch



/// Remove file.
let private removeFileInProject name project model dispatch =
    match getCurrentWSMod model with
    | Some ws when ws.WSViewState<>WSClosed ->
        displayFileErrorNotification "Sorry, you must close the wave simulator before removing design sheets!" dispatch
        switchToWaveEditor model dispatch
    | _ ->
        
        removeFile project.ProjectPath name
        removeFile project.ProjectPath (name + "auto")
        // Remove the file from the dependencies and update project.
        let newComponents = List.filter (fun (lc: LoadedComponent) -> lc.Name.ToLower() <> name.ToLower()) project.LoadedComponents
        // Make sure there is at least one file in the project.
        let project' = {project with LoadedComponents = newComponents}
        match newComponents, name = project.OpenFileName with
        | [],true -> 
            let newComponents = [ (createEmptyDiagramFile project.ProjectPath "main") ]
            openFileInProject' false project.LoadedComponents.[0].Name project' model dispatch
        | [], false -> 
            failwithf "What? - this cannot happen"
        | nc, true ->
            openFileInProject' false project'.LoadedComponents.[0].Name project' model dispatch
        | nc, false ->
            // nothing chnages except LoadedComponents
            dispatch <| SetProject project'
        dispatch FinishUICmd

                

/// Create a new file in this project and open it automatically.
let addFileToProject model dispatch =
    match model.CurrentProj with
    | None -> log "Warning: addFileToProject called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Add sheet to project"

        let before =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                let warn = maybeWarning dialogText project
                div []
                    [ str "A new sheet will be created at:"
                      br []
                      str <| pathJoin
                                 [| project.ProjectPath
                                    dialogText + ".dgm" |]
                      Option.defaultValue (div [] []) warn ]

        let placeholder = "Insert design sheet name"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Add"
        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                    // Create empty file.
                    let name = (getText dialogData).ToLower()
                    createEmptyDgmFile project.ProjectPath name
                    |> displayAlertOnError dispatch
                    // Add the file to the project.
                    let newComponent = {
                        Name = name
                        TimeStamp = System.DateTime.Now
                        WaveInfo = None
                        FilePath = pathJoin [|project.ProjectPath; name + ".dgm"|]
                        CanvasState = [],[]
                        InputLabels = []
                        OutputLabels = []
                    }
                    let updatedProject =
                        { project with
                              LoadedComponents = newComponent :: project.LoadedComponents
                              OpenFileName = name }
 
                    // Open the file, updating the project, saving current file
                    openFileInProject' true name updatedProject model dispatch
                    // Close the popup.
                    dispatch ClosePopup
                    dispatch FinishUICmd

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "") || (maybeWarning dialogText project <> None)

        dialogPopup title body buttonText buttonAction isDisabled dispatch

/// Close current project, if any.
let forceCloseProject model dispatch =
    dispatch (StartUICmd CloseProject)
    let sheetDispatch sMsg = dispatch (Sheet sMsg) 
    dispatch EndSimulation // End any running simulation.
    model.Sheet.ClearCanvas sheetDispatch
    dispatch FinishUICmd



/// force either save of current file before action, or abort (closeProject is special case of this)
let doActionWithSaveFileDialog (name: string) (nextAction: Msg)  model dispatch _ =
    let closeDialogButtons keepOpen _ =
        if keepOpen then
            dispatch ClosePopup
        else
            dispatch nextAction

    if model.SavedSheetIsOutOfDate then 
        choicePopup 
                $"{name}?" 
                (div [] [ str "The current sheet has unsaved changes."])
                "Go back to sheet" 
                $"{name} without saving changes"  
                closeDialogButtons 
                dispatch
    else
        dispatch nextAction

/// Create a new project.
let private newProject model dispatch  =
    match askForNewProjectPath() with
    | None -> () // User gave no path.
    | Some path ->
        match tryCreateFolder path with
        | Error err ->
            log err
            let errMsg = "Could not create a folder for the project."
            displayFileErrorNotification errMsg dispatch
        | Ok _ ->
            dispatch EndSimulation // End any running simulation.
            // Create empty placeholder projectFile.
            let projectFile = baseName path + ".dprj"
            writeFile (pathJoin [| path; projectFile |]) ""
            |> displayAlertOnError dispatch
            // Create empty initial diagram file.
            let initialComponent = createEmptyComponentAndFile path "main"
            setupProjectFromComponents "main" [initialComponent] model dispatch










/// work out what to do opening a file
let rec resolveComponentOpenPopup 
        (pPath:string)
        (components: LoadedComponent list)  
        (resolves: LoadStatus list) 
        (model: Model)
        (dispatch: Msg -> Unit) =
    let chooseWhichToOpen comps =
        (List.maxBy (fun comp -> comp.TimeStamp) comps).Name
    dispatch ClosePopup
    match resolves with
    | [] -> setupProjectFromComponents (chooseWhichToOpen components) components model dispatch
    | Resolve (ldComp,autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let compChanges, connChanges = quantifyChanges ldComp autoComp
        let buttonAction autoSave _ =
            let comp = {(if autoSave then autoComp else ldComp) with TimeStamp = DateTime.Now}
            writeComponentToFile comp
            |> displayAlertOnError dispatch
            if compChanges + connChanges > 0 then
                writeComponentToBackupFile 0 1. comp dispatch
            resolveComponentOpenPopup pPath (comp :: components) rLst  model dispatch   
        // special case when autosave data is most recent
        let title = "Warning!"
        let message, color =
            match compChanges + connChanges with
            | 0 -> 
                sprintf "There were layout but no circuit changes made in sheet %s after your last save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older Saved version?"  ldComp.Name, "green"  
            | n when n < 3 ->   
                sprintf "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version?"  compChanges connChanges ldComp.Name, "orange"
            | n -> 
                sprintf "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                         There is an automatically saved version which is \
                         more uptodate. Do you want to keep the newer AutoSaved version or \
                         the older saved version? This is a large change so the option you do not choose \
                         will be saved as file 'backup/%s.dgm'"  compChanges connChanges ldComp.Name ldComp.Name, "red"
        let body = 
            div [Style [Color color]] [str message] 
        choicePopup title body "Newer AutoSaved file" "Older Saved file" buttonAction dispatch
    | OkAuto autoComp :: rLst ->
         let errMsg = "Could not load saved project file '%s' - using autosave file instead"
         displayFileErrorNotification errMsg dispatch
         resolveComponentOpenPopup pPath (autoComp::components) rLst model dispatch
    | OkComp comp ::rLst -> 
        resolveComponentOpenPopup pPath (comp::components) rLst model dispatch
 

/// open an existing project
let private openProject model dispatch =
    //trying to force the spinner to load earlier
    //doesn't really work right now
    dispatch (Sheet (Sheet.SetSpinner true))
    match askForExistingProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        dispatch (ExecFuncAsynch <| fun () ->
            traceIf "project" (fun () -> "loading files")
            match loadAllComponentFiles path with
            | Error err ->
                log err
                displayFileErrorNotification err dispatch
            | Ok componentsToResolve ->
                traceIf "project" (fun () -> "resolving popups...")
            
                resolveComponentOpenPopup path [] componentsToResolve model dispatch
                traceIf "project" (fun () ->  "project successfully opened.")

            Elmish.Cmd.none)


    

/// Display the initial Open/Create Project menu at the beginning if no project
/// is open.
let viewNoProjectMenu model dispatch =
    let menuItem label action =
        Menu.Item.li
            [ Menu.Item.IsActive false
              Menu.Item.OnClick action ] [ str label ]

    let initialMenu =
        Menu.menu []
            [ Menu.list []
                  [ menuItem "New project" (fun _ -> newProject model dispatch)
                    menuItem "Open project" (fun _ -> openProject model dispatch) ]
            ]

    match model.CurrentProj with
    | Some _ -> div [] []
    | None -> unclosablePopup None initialMenu None [] dispatch

//These two functions deal with the fact that there is a type error otherwise..
let goBackToProject model dispatch _ =
    dispatch (SetExitDialog false)

let closeApp model dispatch _ =
    dispatch CloseApp


type SheetTree = {
    Node: string
    Size: int
    SubSheets: SheetTree list
    }


/// get the subsheet tree for aa sheets
let getSheetTrees (p:Project) =
    let ldcMap = 
        p.LoadedComponents
        |> List.map (fun ldc -> ldc.Name,ldc)
        |> Map.ofList
    let rec subSheets (path: string list) (sheet: string) : SheetTree=
        let ldc = Map.tryFind sheet ldcMap
        match ldc with
        | None -> {Node=sheet; Size = 1;SubSheets = []}
        | Some ldc ->
            let comps,_ = ldc.CanvasState
            comps
            |> List.collect (fun comp -> 
                    match comp.Type with 
                    | Custom ct when not <| List.contains ct.Name path -> 
                        [subSheets (ct.Name :: path) ct.Name]
                    | _ -> 
                        [])
            |> (fun subs -> {
                Node=sheet; 
                Size = List.sumBy (fun sub -> sub.Size) subs + 1; 
                SubSheets= subs
                })
    p.LoadedComponents
    |> List.map (fun ldc ->ldc.Name, subSheets []  ldc.Name)
    |> Map.ofList


    


    
    
    
        

/// Display top menu.
let viewTopMenu model messagesFunc simulateButtonFunc dispatch =
    let compIds = getComponentIds model
    
    messagesFunc model dispatch

    //printfn "FileView"
    let style = Style [ Width "100%" ; BorderBottom "2px solid lightgray"] //leftSectionWidth model
    let styleNoBorder = Style [Width "100%"]
    let projectPath, fileName =
        match model.CurrentProj with
        | None -> "no open project", "no open sheet"
        | Some project -> project.ProjectPath, project.OpenFileName

    let makeFileLine isSubSheet name project =
        let nameProps = 
            if isSubSheet then 
                [] else  
                [Props [Style [FontWeight "bold"]]]
        Navbar.Item.div [ Navbar.Item.Props [ styleNoBorder  ] ]
            [ Level.level [ Level.Level.Props [ styleNoBorder ]]
                  [ Level.left nameProps [ Level.item [] [ str name ] ]
                    Level.right [ Props [ Style [ MarginLeft "20px" ] ] ]
                        [ Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsPrimary
                                    Button.Disabled(name = project.OpenFileName)
                                    Button.OnClick(fun _ ->
                                        dispatch (StartUICmd ChangeSheet)
                                        openFileInProject name project model dispatch) ] [ str "open" ] 
                          ]
                          // Add option to rename?
                          Level.item [] [
                              Button.button [
                                  Button.Size IsSmall
                                  Button.IsOutlined
                                  Button.Color IsInfo
                                  Button.OnClick(fun _ ->
                                      dispatch (StartUICmd RenameSheet)
                                      renameFileInProject name project model dispatch) ] [ str "rename" ]
                          ]
                          Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsDanger
                                    Button.OnClick(fun _ ->
                                        let title = "Delete sheet"

                                        let body =
                                            div []
                                                [ str "Are you sure you want to delete the following design sheet?"
                                                  br []
                                                  str <| pathJoin
                                                             [| project.ProjectPath
                                                                name + ".dgm" |]
                                                  br []
                                                  str <| "This action is irreversible." ]

                                        let buttonText = "Delete"

                                        let buttonAction =
                                            fun _ ->
                                                dispatch (StartUICmd DeleteSheet)
                                                removeFileInProject name project model dispatch
                                                dispatch ClosePopup
                                        confirmationPopup title body buttonText buttonAction dispatch) ]
                                    [ str "delete" ] ] ] ] ]

    let fileTab =
        match model.CurrentProj with
        | None -> Navbar.Item.div [] []
        | Some project ->

            let sTrees = getSheetTrees project

            let rec subSheetsOf sh =
                match Map.tryFind sh sTrees with
                | Some tree -> tree.SubSheets
                | None -> []
                |> List.collect (fun ssh -> ssh.Node :: subSheetsOf ssh.Node)
                |> List.distinct

            let allSubSheets =
                mapKeys sTrees
                |> Array.toList
                |> List.collect subSheetsOf
                |> Set
            let isSubSheet sh = Set.contains sh allSubSheets

            let projectFiles = 
                project.LoadedComponents 
                |> List.map (fun comp -> 
                    let tree = sTrees.[comp.Name]
                    makeFileLine (isSubSheet tree.Node) comp.Name project, tree)
                |> List.sortBy (fun (line,tree) -> isSubSheet tree.Node, tree.Node, -tree.Size, tree.Node.ToLower())
                |> List.map fst
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          if model.TopMenuOpenState = Files then Closed else Files
                          |> SetTopMenu
                          |> dispatch) ] ]
                [ Navbar.Link.a [] [ str "Sheets" ]
                  Navbar.Dropdown.div
                      [ Navbar.Dropdown.Props
                          [ Style
                              [ Display
                                  (if (let b = model.TopMenuOpenState = Files
                                       b) then
                                      DisplayOptions.Block
                                   else
                                      DisplayOptions.None) ] ] ]
                      ([ Navbar.Item.a [ Navbar.Item.Props [ OnClick(fun _ -> 
                            dispatch (StartUICmd AddSheet)
                            addFileToProject model dispatch) ] ]
                             [ str "New Sheet" ]
                         Navbar.divider [] [] ]
                       @ projectFiles) ]

    div [   HTMLAttr.Id "TopMenu"
            leftSectionWidth model
            Style [ Position PositionOptions.Absolute
                    UserSelect UserSelectOptions.None

                    ]
        ]
        [ Navbar.navbar
            [ Navbar.Props
                [  Style
                    [ Height "100%"
                      Width "100%" 
                      BorderBottom "2px solid lightgray"] ] ]
            [ Navbar.Brand.div
                  [ Props
                      [ Style
                          [ Height "100%"
                            Width "100%" ] ] ]
                    [ Navbar.Item.div
                        [ Navbar.Item.HasDropdown
                          Navbar.Item.Props
                              [ OnClick(fun _ ->
                                  if model.TopMenuOpenState = Project then Closed else Project
                                  |> SetTopMenu
                                  |> dispatch) ] ]
                          [ Navbar.Link.a [] [ str "Project" ]
                            Navbar.Dropdown.div
                                [ Navbar.Dropdown.Props
                                    [ Style
                                        [ Display
                                            (if model.TopMenuOpenState = Project then
                                                DisplayOptions.Block
                                             else
                                                 DisplayOptions.None) ] ] ]
                                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick <| doActionWithSaveFileDialog "New project" (ExecFuncInMessage(newProject,dispatch)) model dispatch ] ]
                                      [ str "New project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| doActionWithSaveFileDialog "Open project" (ExecFuncInMessage(openProject,dispatch)) model dispatch ] ]
                                      [ str "Open project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| doActionWithSaveFileDialog "Close project" (ExecFuncInMessage(forceCloseProject,dispatch)) model dispatch ] ]
                                      [ str "Close project" ] ] ]

                      fileTab
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Breadcrumb.breadcrumb [ Breadcrumb.HasArrowSeparator ]
                                      [ Breadcrumb.item [] [ str <| cropToLength 30 false projectPath ]
                                        Breadcrumb.item [] [ span [ Style [ FontWeight "bold" ] ] [ str fileName ] ] ] ] ]
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Button.button
                                    ((if model.SavedSheetIsOutOfDate then 
                                        []
                                       else
                                        [ Button.Color IsLight ]) @
                                    [
                                      Button.Color IsSuccess  
                                      
                                      Button.OnClick(fun _ -> 
                                        dispatch (StartUICmd SaveSheet)
                                        saveOpenFileActionWithModelUpdate model dispatch |> ignore
                                        dispatch <| Sheet(Sheet.DoNothing) //To update the savedsheetisoutofdate send a sheet message
                                        ) ]) [ str "Save" ] ] ]
                      Navbar.End.div []
                          [ 
                            Navbar.Item.div [] 
                                [ simulateButtonFunc compIds model dispatch ] ]
                      Navbar.End.div []
                          [ Navbar.Item.div []
                                [ Button.button 
                                    [ Button.OnClick(fun _ -> PopupView.viewInfoPopup dispatch) 
                                      Button.Color IsInfo
                                    ] 
                                    [ str "Info" ] 
                                  // add space padding on RH of navbar to improve top bar formatting
                                  // this is a bit of a hack - but much easier than matching styles
                                  Text.div 
                                    [Props [Style [PaddingRight "7000px"]]] [str ""]
                                ] ] ] ] ]
