module CustomCompPorts

(*
This module provides some functions that ensure consistency of instantiated custom components when changes are
made to ports in the underlying sheet. A dialog is presented which allows instantiated components ports to
be updated correctly, with best efforts attempt to keep existing connections to each instance where ports remain
the same or where it can safely be deduced how ports have been renamed.

The code potentially makes changes to every sheet in the project in the model, and writes out these changes to disk.

A sheet that declares parameters has no single signature: it has a family of them, one per set of
bindings, and two instances of it may legitimately have different port widths. So "out of date"
cannot mean "differs from the sheet". It means differs from what THIS instance's own bindings give
it - CanvasExtractor.signatureOfInstance - and each instance is brought back to its own signature
rather than to a shared one. Testing against the sheet instead raised this dialog on every save of
any parameterised design, and accepting it forced every instance to the sheet's declared widths
while leaving its bindings alone, which is a design the simulator then rejects.

Widths are consequently absent from what the dialog reports. A port added, deleted or renamed is a
fact about the SHEET and is what the user needs to confirm; a width is a fact about each instance,
and every instance is updated to its own.

Port ORDER is not compared either - see instanceIsOutOfDate.
*)

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open ModelType
open ModelHelpers
open CommonTypes
open FilesIO
open CanvasExtractor
open PopupHelpers
open MenuHelpers
open Optics


let printSheetNames (model:Model) =
    model.CurrentProj
    |> Option.map (fun p -> 
        printf $"SHEETS:{p.LoadedComponents |> List.map (fun ldc -> ldc.Name)}--->{p.OpenFileName}")
    |> ignore


let getCorrectFileName (project:Project) = 
    match project.WorkingFileName with
    |Some name -> name
    |None -> project.OpenFileName


//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//-------------------------------New-style project update and saving--------------------------//
//--------------------------------------------------------------------------------------------//



/// Save any changed sheets to disk in the project directory
let syncLoadedComponentsToDisk newProj oldProj =
    let needToSave ldc' ldc =
       (not <| compareCanvas 10. ldc'.CanvasState ldc.CanvasState) ||
       ldc'.WaveInfo <> ldc.WaveInfo ||
       ldc'.LCParameterSlots <> ldc.LCParameterSlots ||
       ldc'.IsTopSheet <> ldc.IsTopSheet
    let saveToDisk ldc =
        let state = ldc.CanvasState
        let waveInfo = ldc.WaveInfo
        let sheetInfo: SheetInfo = {Form=ldc.Form;Description=ldc.Description; ParameterDefinitions=ldc.LCParameterSlots; IsTopSheet = Some ldc.IsTopSheet}
        saveStateToFile newProj.ProjectPath ldc.Name (state,waveInfo,Some sheetInfo)
        |> ignore

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
            saveToDisk ldcNew
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
/// CanvasExtractor owns this type, because it is what signatureOfInstance answers with.
type Signature = CanvasExtractor.Signature

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
    |> Seq.map 
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
            // printfn "message: %s" message
            {
                Message = message
                Direction = fst m
                New = newLW
                Old = oldLW
            })

let guessAtRenamedPorts (matches: PortChange seq)  : PortChange array =
    let matches = Seq.toList matches
    /// Candidates keyed by what a rename cannot change: which side of the component the port is on
    /// and how wide it is. A key held by more than one candidate is dropped, since there is then
    /// no single port it could have been renamed from.
    /// The direction belongs in the key: keyed on width alone, an INPUT added at width 8 was
    /// paired with an OUTPUT deleted at width 8, and the instance was left half-changed.
    let portsByKeyFiltered (ports: ((string*int) * PortChange) list) =
        ports
        |> List.groupBy (fun ((_, width), change) -> change.Direction, width)
        |> List.collect (function |(key, [item]) -> [key,item] | _ -> [] )
        |> Map.ofList

    let additions =
        matches
        |> List.collect (function | {New = Some (name,width); Old = None} as m -> [(name,width), m] | _ -> [])
        |> portsByKeyFiltered

    let deletions =
        matches
        |> List.collect (function | {Old = Some (name,width); New = None} as m -> [(name,width), m] | _ -> [])
        |> portsByKeyFiltered

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

        
    



/// One instance of the current sheet placed on another sheet: where it is, the signature it has,
/// and the signature it OUGHT to have - the sheet inside it resolved at this instance's own
/// parameter bindings. The two differ exactly when the instance needs updating.
type Instance = {
    /// the sheet the instance sits on
    Sheet: string
    CompId: string
    Label: string
    Old: Signature
    Expected: Signature
    }

/// Every instance of the current sheet, with the signature it has and the one it should have.
///
/// An instance's bindings are expressions in the parameters of the sheet it SITS ON - and may be
/// overridden there by a CustomCompParam slot, exactly as in elaboration - so the parent sheet is
/// where they are evaluated. ParameterAnalysis.instanceBindingExprs is the same merge the
/// properties pane and the simulator make.
let getInstancesOfCurrentSheet (model: Model) : Instance list =
    mapOverProject [] model <| fun p ->
        let sheetName = getCorrectFileName p
        p.LoadedComponents
        |> List.filter (fun ldc -> ldc.Name <> sheetName)
        |> List.collect (fun parentLdc ->
            let parentBindings = ParameterAnalysis.declaredParams parentLdc
            let parentSlots = ParameterAnalysis.sheetParamSlots parentLdc
            fst parentLdc.CanvasState
            |> List.choose (fun comp ->
                match comp.Type with
                | Custom cc when cc.Name = sheetName ->
                    ParameterAnalysis.instanceBindingExprs parentSlots comp cc
                    |> CanvasExtractor.signatureOfInstance p.LoadedComponents parentBindings sheetName
                    |> Option.map (fun expected ->
                        { Sheet = parentLdc.Name
                          CompId = comp.Id
                          Label = comp.Label
                          Old = cc.InputLabels, cc.OutputLabels
                          Expected = expected })
                | _ -> None))

/// Whether one instance is out of step with what its own bindings give it: it has a port the sheet
/// does not, lacks one the sheet has, or holds one at the wrong width.
///
/// The ORDER of the ports is deliberately not compared. An instance's ports are matched to the
/// sheet's Input and Output components by LABEL - by FastCreate when a simulation is elaborated, by
/// SynchronousUtils, and by CanvasStateAnalyser.checkCustomComponentForOkIOs, which compares sets
/// when it decides whether a design is legal - and each port is drawn where the symbol's own saved
/// PortOrder puts it, keyed by port id. So an instance whose ports are in a different order from
/// the sheet's is a correct instance, and reordering it would change nothing that can be seen or
/// simulated.
///
/// Requiring the orders to match raised this dialog on four of the five shipped demos, whose
/// instances were placed before the I/O components were moved about on their sheets, and described
/// the difference as a width change - which is what the dialog says when nothing structural
/// happened.
let instanceIsOutOfDate (inst: Instance) : bool =
    let samePorts (ports: (string * int) list) (ports': (string * int) list) = Set ports = Set ports'
    not (samePorts (fst inst.Old) (fst inst.Expected)
         && samePorts (snd inst.Old) (snd inst.Expected))

/// The instances of the current sheet that no longer match what their own bindings give them.
let getOutOfDateDependents (model: Model) : Instance list =
    getInstancesOfCurrentSheet model
    |> List.filter instanceIsOutOfDate


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
                    Id = JSHelpers.uuid ()
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
    
/// Bring one instance to the signature its own parameter bindings give it.
let updateInstance (inst: Instance) (p: Project) =
    let newSig = inst.Expected
    /// Assume that name and bit number changes have been made. Deal with any needed reordering.
    let reorderInstancePorts (comp: Component) =
        let reorderPorts newNames oldNames (oldPorts: Port list) =
            newNames
            |> List.map (fun (name,_) -> List.findIndex (fun (name',_) -> name'=name) oldNames)
            |> List.map (fun n -> oldPorts[n])
            |> List.mapi (fun i p -> {p with PortNumber = Some i})
        match comp.Type with
        | Custom ct -> //when ct.Form = Some User ->
                // What is left to do is judged from the signature the instance has NOW, after
                // changeInstance has added, deleted, renamed and rewidened its ports - not from
                // the one it started with. Comparing against the original made a change that both
                // added a port and reordered the rest fall through to the warning below, which
                // left the instance half-updated.
                let currentSig = ct.InputLabels, ct.OutputLabels
                if currentSig = newSig then
                    comp
                elif mapPair List.sort currentSig = mapPair List.sort newSig then
                    printfn $"Reordering {comp.Label}"
                    let newIn,newOut = newSig
                    let oldIn,oldOut = currentSig
                    let newInPorts = reorderPorts newIn oldIn comp.InputPorts
                    let newOutPorts = reorderPorts newOut oldOut comp.OutputPorts
                    let ct' = {ct with InputLabels = fst newSig; OutputLabels = snd newSig}
                    {comp with Type = Custom ct'; InputPorts=newInPorts; OutputPorts=newOutPorts}

                else
                    printfn "What? Signatures do not match after changes are made"
                    comp
        // | Custom ct when ct.Form = Some (Verilog _) ->

        | _ -> comp // no change (should never happen?)
#if ASSERTS
    assertThat
        (inst.Sheet <> (getCorrectFileName p))
        $"What? Instances to be changed in {inst.Sheet} must not be in custom \
        component sheet{p.OpenFileName}"
#endif
    let ldc =
        p.LoadedComponents
        |> List.find (fun ldc -> ldc.Name = inst.Sheet)
    let (comps,conns) = ldc.CanvasState
    let comp =
        comps |> List.find (fun comp -> comp.Id = inst.CompId)
    let changes =
        ioCompareSigs newSig inst.Old
        |> guessAtRenamedPorts
    let comp' =
        (comp, changes)
        ||> Array.fold (fun comp change ->
            let comp'' = changeInstance comp change
            comp''
            )
        |> reorderInstancePorts
    let comps' =
        comps
        |> List.map (fun comp ->
            if comp.Id = inst.CompId then
                comp'
            else
                comp)
    let ldc' =
        {ldc with
            CanvasState = deleteIncompleteConnections (comps',conns)
            // the sheet has changed in memory and must reach disk: it is not the open sheet, so
            // nothing else will notice
            LoadedComponentIsOutOfDate = true}
    let ldcLst = ldc' :: List.except [ldc] p.LoadedComponents
    {p with LoadedComponents = ldcLst}



/// Bring every one of these instances to the signature its own parameter bindings give it.
let updateDependents (instances: Instance list) (p: Project) : Project =
    (p, instances)
    ||> List.fold (fun p instance -> updateInstance instance p)

/// Every sheet has just been written to disk from memory, so none of them has unsaved changes.
///
/// updateInstance flags each sheet it changes so that the change reaches disk; once it has, the
/// flag has done its job. Left set it made every later "close without saving?" dialog name sheets
/// whose files were already correct, and left the user no way to clear it: the Save button follows
/// the OPEN sheet alone, and an instance is by construction never on that sheet.
let markProjectSaved (p: Project) : Project =
    p |> Optic.map loadedComponents_ (List.map (Optic.set loadedComponentIsOutOfDate_ false))

let checkCanvasStateIsOk (model:Model) =
    mapOverProject false model (fun p ->
        let ldc = List.find (fun ldc -> ldc.Name = (getCorrectFileName p)) p.LoadedComponents
        let comps,conns = ldc.CanvasState
        let ioNames =
            comps
            |> List.filter (fun comp -> match comp.Type with | Input1 _ | Output _ -> true | _ -> false)
            |> List.map (fun comp -> comp.Label)
        ioNames.Length = (List.distinct ioNames).Length
        )
    
/// A change to the sheet's ports as the dialog reports it: a port added, deleted or renamed.
/// Widths are deliberately left out - see the module comment - so that one row describes what
/// happened to every instance, however many different widths those instances have.
type ReportedChange = {
    Direction: IODirection
    OldName: string option
    NewName: string option
    Message: string
    }

/// What the user is being asked to confirm, gathered from every out-of-date instance: the ports
/// that came and went, and which instances change only in width.
///
/// The structural changes are taken over all instances rather than from a representative one.
/// Instances of a parameterised sheet may be at different widths, so a rename can be guessed for
/// one and not for another; the union is what the sheet's ports actually did.
let reportedChanges (instances: Instance list) : ReportedChange list * Instance list =
    let changesOf (inst: Instance) =
        ioCompareSigs inst.Expected inst.Old
        |> guessAtRenamedPorts
        |> Array.toList
    let structuralOf (change: PortChange) =
        let nameOf = Option.map fst
        match nameOf change.New, nameOf change.Old with
        | newName, oldName when newName = oldName -> None
        | newName, oldName ->
            Some {Direction = change.Direction; NewName = newName; OldName = oldName; Message = change.Message}
    let structural =
        instances
        |> List.collect (changesOf >> List.choose structuralOf)
        |> List.distinct
    let widthOnly =
        instances
        |> List.filter (fun inst -> changesOf inst |> List.forall (structuralOf >> Option.isNone))
    structural, widthOnly

/// returns a popup function to show the dependents update dialog if this is needed
/// this dialog drives all subsequent work changing custom component instances
let optCurrentSheetDependentsPopup (model: Model) =
        let sheet =
            model.CurrentProj
            |> Option.map (fun p ->
                match p.WorkingFileName with
                |Some z -> (Option.get p.WorkingFileName)
                |None -> p.OpenFileName
            )
        if not <| checkCanvasStateIsOk model then
            None // do nothing if IOs are not currently valid. Can this ever happen?
        else
            match getOutOfDateDependents model with
            | [] -> None
            | instances ->
                let depSheets =
                    instances
                    |> List.map (fun inst -> inst.Sheet)
                    |> List.distinct
                    |> String.concat ","
                let structural, widthOnly = reportedChanges instances
                let whatChanged =
                    match structural, widthOnly with
                    | [], [] -> ""
                    | [], _ -> "the width of a port on"
                    | _, _ -> "the inputs or outputs of"

                let headCell heading =  th [ ] [ str heading ]
                let makeRow (change: ReportedChange) =
                    let name = Option.defaultValue "" >> str
                    tr []
                        [
                            td [] [str (if change.Direction = InputIO  then "Input" else "Output")]
                            td [] [name change.NewName]
                            td [] [name change.OldName]
                            td [] [str change.Message]
                        ]
                let structuralTable =
                    match structural with
                    | [] -> div [] []
                    | _ ->
                        Table.table [
                                Table.IsHoverable
                                Table.IsBordered
                                Table.IsNarrow
                                Table.Props [Style [ MarginTop "15px" ]]]
                            [
                                thead [] [ tr [] (List.map headCell ["Type" ;"New port"; "Old port" ; "Change"]) ]
                                tbody []   (List.map makeRow structural)
                            ]
                // Instances whose widths alone change are NAMED rather than tabulated: two
                // instances of a parameterised sheet are meant to be at different widths, and
                // printing one instance's numbers as though they were the sheet's is what made
                // this dialog describe a parameterised design wrongly. Which instances are
                // affected is the part the user can act on.
                let widthNote =
                    match widthOnly with
                    | [] -> div [] []
                    | affected ->
                        let named =
                            affected
                            |> List.map (fun inst -> $"{inst.Label} on {inst.Sheet}")
                            |> String.concat ", "
                        let subject =
                            if affected.Length = 1 then "This instance is" else "These instances are"
                        div [] [str $"{subject} at port widths that no longer follow from \
                                      their own parameters, and will be corrected: {named}."]
                let body =
                    div [Style [ MarginTop "15px" ] ]
                        [
                            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [str $"{sheet}"]
                            str $"You have changed {whatChanged} the current '{sheet}' sheet. "
                            br []
                            str "This dialog will automatically update all dependent sheets to match this. "
                            br []
                            str $"The '{sheet}' sheet is instantiated as a symbol {instances.Length} times in dependent sheets: '{depSheets}'. "
                            str $"If you do not automatically update the symbols you will need to delete and recreate each one."
                            br []
                            str "If you automatically update symbols wires that no longer match will be autorouted correctly when you next load each sheet"
                            br []
                            str "Each instance is set to the port widths its own parameters give it, so instances at different widths stay at different widths."
                            br []
                            widthNote
                            structuralTable
                        ]

                let buttonAction isUpdate dispatch  _ =
                    match isUpdate, model.CurrentProj with
                    | true, Some p ->
                        let proj =
                            updateDependents instances p
                            |> (fun p ->
                                saveAllProjectFilesFromLoadedComponentsToDisk p
                                markProjectSaved p)
                        dispatch <| SetProject proj

                        if proj.OpenFileName <> (Option.defaultValue proj.OpenFileName proj.WorkingFileName) then
                            let model' = {model with CurrentProj = Some proj}
                            openFileInProject' false proj.OpenFileName proj model' dispatch
                    | _ -> ()

                    dispatch <| ClosePopup

                choicePopupFunc
                    "Update All Sheet Instances"
                    (fun _ -> body)
                    "Update all instances"
                    "Save the sheet without updating instances"
                    buttonAction
                |> Some




       

 



