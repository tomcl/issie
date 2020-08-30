(*
    FileMenuView.fs

    View for the top menu, and related functionalities.
*)

module FileMenuView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open DiagramStyle
open DiagramMessageType
open DiagramModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open Simulator
open SimulatorTypes

let getCurrFile (model: Model) =
    match model.CurrProject with
    | Some proj -> Some proj.OpenFileName
    | None -> None

let currWS (model: Model) =
    match getCurrFile model with
    | Some fileName when Map.containsKey fileName (fst model.WaveSim) -> Some (fst model.WaveSim).[fileName]
    | _ -> None

let private displayFileErrorNotification err dispatch =
    errorNotification err CloseFilesNotification
    |> SetFilesNotification
    |> dispatch

let private loadStateIntoCanvas state model dispatch =
    dispatch <| SetHighlighted([], []) // Remove current highlights.
    model.Diagram.ClearCanvas() // Clear the canvas.
    // Finally load the new state in the canvas.
    let components, connections = state
    List.map model.Diagram.LoadComponent components |> ignore
    List.map (model.Diagram.LoadConnection true) connections |> ignore
    model.Diagram.FlushCommandStack() // Discard all undo/redo.
    // Run the a connection widhts inference.
    InferWidths()
    |> JSDiagramMsg
    |> dispatch
    // Set no unsaved changes.
    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch



/// extract SavedwaveInfo from model to be saved
let getSavedWave (model:Model): SavedWaveInfo option = None

/// add waveInfo to model
let setSavedWave (wave: SavedWaveInfo option) =
    fun model ->
        match wave with
        | None -> model
        | Some waveInfo -> model // TODO: setup

/// Save the file currently open.


let saveOpenFileAction isAuto model =
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ | _, None -> ()
    | Some jsState, Some project ->
        extractState jsState
        |> (fun state -> 
                let savedState = state, getSavedWave model
                if isAuto then
                    saveAutoStateToFile project.ProjectPath project.OpenFileName savedState
                else 
                    saveStateToFile project.ProjectPath project.OpenFileName savedState
                    removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName)


let private getFileInProject name project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name = name)

let private isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Create a new empty .dgm file and return corresponding loaded component.
let private createEmptyDiagramFile projectPath name =
    createEmptyDgmFile projectPath name

    {   
        Name = name
        TimeStamp = System.DateTime.Now
        WaveInfo = None
        FilePath = pathJoin [| projectPath; name + ".dgm" |]
        CanvasState = [],[]
        InputLabels = []
        OutputLabels = []
    }

let setupProject (pPath:string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
    let openFileName, openFileState =
        match ldComps with
        | [] -> // No files in the project. Create one and open it.
            createEmptyDgmFile pPath "main"
            "main", ([],[])
        | comps ->
            // load the most recently saved file
            let comp = comps |> List.maxBy (fun comp -> comp.TimeStamp)
            comp.Name, comp.CanvasState
    dispatch EndSimulation // End any running simulation.
    loadStateIntoCanvas openFileState model dispatch
    {
        ProjectPath = pPath
        OpenFileName =  openFileName
        LoadedComponents = ldComps
    }
    |> SetProject |> dispatch

/// Open the specified file.
let private openFileInProject name project model dispatch =
    match getFileInProject name project with
    | None -> log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some loadedComponent ->
        saveOpenFileAction false model
        setupProject project.ProjectPath project.LoadedComponents model dispatch
        dispatch EndSimulation // End any running simulation.

/// Remove file.
let private removeFileInProject name project model dispatch =
    removeFile project.ProjectPath name
    // Remove the file from the dependencies and update project.
    let newComponents = List.filter (fun lc -> lc.Name <> name) project.LoadedComponents
    // Make sure there is at least one file in the project.
    let newComponents =
        match List.isEmpty newComponents with
        | false -> newComponents
        | true -> [ (createEmptyDiagramFile project.ProjectPath "main") ]

    let project = { project with LoadedComponents = newComponents }
    project
    |> SetProject
    |> dispatch
    // If the file was displayed, open and display another one instead.
    // It is safe to access position 0 as we are guaranteed that there is at
    // least one element in newComponents.
    assertThat (not <| List.isEmpty project.LoadedComponents) "removeFileInProject"
    match name = project.OpenFileName with
    | false -> ()
    | true -> openFileInProject project.LoadedComponents.[0].Name project model dispatch

/// Create a new file in this project and open it automatically.
let addFileToProject model dispatch =
    match model.CurrProject with
    | None -> log "Warning: addFileToProject called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Add file to project"

        let before =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData

                let maybeWarning =
                    if isFileInProject dialogText project
                    then div [ Style [ Color "red" ] ] [ str "This file already exists." ]
                    else div [] []
                div []
                    [ str "A new file will be created at:"
                      br []
                      str <| pathJoin
                                 [| project.ProjectPath
                                    dialogText + ".dgm" |]
                      maybeWarning ]

        let placeholder = "Insert module name"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Add"

        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                // Save current file.
                saveOpenFileAction false model
                // Create empty file.
                let name = getText dialogData
                createEmptyDgmFile project.ProjectPath name
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
                // Update the project.
                updatedProject
                |> SetProject
                |> dispatch
                // Open the file.
                openFileInProject name updatedProject model dispatch
                // Close the popup.
                dispatch ClosePopup
                dispatch EndSimulation // End any running simulation.

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "")

        dialogPopup title body buttonText buttonAction isDisabled dispatch

/// Close current project, if any.
let private closeProject model dispatch _ =
    dispatch EndSimulation // End any running simulation.
    dispatch CloseProject
    model.Diagram.ClearCanvas()

/// Create a new project.
let private newProject model dispatch _ =
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
            // Create empty initial diagram file.
            let initialDiagram = createEmptyDiagramFile path "main"
            // Load the diagram.
            loadStateIntoCanvas initialDiagram.CanvasState model dispatch
            // Add the file to the project.
            { ProjectPath = path
              OpenFileName = "main"
              LoadedComponents = [ initialDiagram ] }
            |> SetProject
            |> dispatch







/// work out what to do opening a file
let rec resolveComponentOpenPopup 
        (pPath:string)
        (components: LoadedComponent list)  
        (resolves: LoadStatus list) 
        (model: Model)
        (dispatch: Msg -> Unit) =
    dispatch ClosePopup
    match resolves with
    | [] -> setupProject pPath components model dispatch
    | Resolve (ldComp,autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let buttonAction autoSave _ =
            let comp = if autoSave then autoComp else ldComp

            resolveComponentOpenPopup pPath (comp :: components) rLst  model dispatch   
        // special case when autosave data is most recent
        let title = "Warning!"
        let body = str <|  sprintf "Warning: changes were made to sheet '%s' after your last Save. There ia an automatically saved version which is '%s \
                                more uptodate. Do you want to keep the newer AutoSaved version or \
                                the older saved version?"  ldComp.Name  ((autoComp.TimeStamp - ldComp.TimeStamp).ToString())
        choicePopup title body "Newer AutoSaved file" "Older Saved file" buttonAction dispatch
    | OkAuto autoComp :: rLst ->
         let errMsg = "Could not load saved project file '%s' - using autosave file instead"
         displayFileErrorNotification errMsg dispatch
         resolveComponentOpenPopup pPath (autoComp::components) rLst model dispatch
    | OkComp comp ::rLst -> 
        resolveComponentOpenPopup pPath (comp::components) rLst model dispatch
 

/// open an existing project
let private openProject model dispatch _ =
    match askForExistingProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match loadAllComponentFiles path with
        | Error err ->
            log err
            let errMsg = "Could not load diagrams files in the project. The files may be malformed."
            displayFileErrorNotification errMsg dispatch
        | Ok componentsToResolve ->
            resolveComponentOpenPopup path [] componentsToResolve model dispatch


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
                  [ menuItem "New project" (newProject model dispatch)
                    menuItem "Open project" (openProject model dispatch) ] ]

    match model.CurrProject with
    | Some _ -> div [] []
    | None -> unclosablePopup None initialMenu None []

let private viewInfoPopup dispatch =
    let makeH h =
        Text.span
            [ Modifiers
                [ Modifier.TextSize(Screen.Desktop, TextSize.Is5)
                  Modifier.TextWeight TextWeight.Bold ] ]
            [ str h
              br [] ]

    let title = "DEflow Info"

    let body =
        div []
            [ makeH "Version"
              str "v0.2"
              br []
              br []
              makeH "Acknowledgments"
              str "DEflow has been created by Marco Selvatici as his dissertation project."
              br []
              br []
              makeH "Keyboard shortcuts"
              str "On Mac use Command instead of Ctrl."
              ul []
                  [ li [] [ str "Save: Ctrl + S" ]
                    li [] [ str "Copy selected diagram items: Alt + C" ]
                    li [] [ str "Paste diagram items: Alt + V" ]
                    li [] [ str "Undo last diagram action: Alt + Z" ]
                    li [] [ str "Redo last diagram action: Alt + Shift + Z" ] ] ]

    let foot = div [] []
    closablePopup title body foot [] dispatch

/// Simulate function
//simulate button functions

/// Returns a tuple option representing the output to which the target input is connected
let driveOut simGraph targetCompId inPortN =
    Map.toArray simGraph
    |> Array.tryPick (fun (outCompId, simComp: SimulatorTypes.SimulationComponent) ->
        Map.toArray simComp.Outputs
        |> Array.tryFind (fun (_, lst) -> (List.exists (fun t -> t = (targetCompId, inPortN)) lst))
        |> function
        | Some(outPN, _) -> Some(outCompId, outPN)
        | None -> None)

let simWireData2Wire wireData =
    wireData
    |> List.mapFold (fun weight bit ->
        match bit with
        | SimulatorTypes.Bit.Zero -> bigint 0
        | SimulatorTypes.Bit.One -> weight
        |> (fun r -> r, weight * (bigint 2))) (bigint 1)
    |> fst
    |> List.sum

let getSelected model: DiagEl list =
    match model.Diagram.GetSelected() with
    | None -> []
    | Some jsState ->
        (fst jsState |> List.map (extractComponent >> Comp), snd jsState |> List.map (extractConnection >> Conn))
        ||> List.append

let procIns simData (compId: ComponentId) (inputs: InputPortNumber []): WaveSimPort [] =
    Array.collect (fun portN ->
        match simData.Graph.[compId].Type, driveOut simData.Graph compId portN with
        | Input _, _ -> [||]
        | Output _, Some(cId, oPN) ->
            [| { CId = cId
                 OutPN = oPN
                 TrgtId = Some compId } |]
        | _, Some(cId, oPN) ->
            [| { CId = cId
                 OutPN = oPN
                 TrgtId = None } |]
        | _, None -> failwith "Input is not connected") inputs

let processComp simData cId: WaveSimPort [] =
    let procCompIns (compId: ComponentId) (inputs: Map<InputPortNumber, WireData>): WaveSimPort [] =
        Map.toArray inputs
        |> Array.map (fun (key, _) -> key)
        |> procIns simData compId

    let procOuts compId outputs: WaveSimPort [] =
        Map.toArray outputs
        |> Array.map (fun (portNum, _) ->
            { CId = compId
              OutPN = portNum
              TrgtId = None })

    match Map.tryFind cId simData.Graph with
    | Some sC -> Array.append (procCompIns cId sC.Inputs) (procOuts cId sC.Outputs)
    | None -> failwith "Component Id is not in Simulation Data"

let remDuplicates arrWithDup =
    Array.groupBy (fun p -> p.CId, p.OutPN) arrWithDup
    |> Array.map (fun (_, ports) -> { ports.[0] with TrgtId = Array.tryPick (fun p -> p.TrgtId) ports })

let compsConns2portLst model (simData: SimulatorTypes.SimulationData) diagElLst: WaveSimPort [] =
    let portId2CIdInPN pId =
        match model.Diagram.GetCanvasState() with
        | Some s ->
            List.map extractComponent (fst s)
            |> List.tryPick (fun c ->
                List.tryFindIndex (fun (p: Port) -> p.Id = pId) c.InputPorts
                |> function
                | Some i -> Some(c.Id, i)
                | None -> None)
        | None -> failwith "Called portId2cIdoutPN when Canvas State is None"

    diagElLst
    |> List.toArray
    |> Array.collect (fun compEl ->
        match compEl with
        | Comp c -> processComp simData (ComponentId c.Id)
        | Conn c ->
            match portId2CIdInPN c.Target.Id with
            | Some(cId, inPN) -> procIns simData (ComponentId cId) [| InputPortNumber inPN |]
            | None -> [||])
    |> remDuplicates

let reloadablePorts (model: Model) (simData: SimulatorTypes.SimulationData) =
    let inGraph port = Map.exists (fun key _ -> key = port.CId) simData.Graph
    match currWS model with
    | Some wSMod ->
        Array.filter inGraph wSMod.Ports
        |> Array.map (fun port ->
            match port.TrgtId with
            | Some trgtId when Map.exists (fun key _ -> key = trgtId) simData.Graph ->
                match List.tryFind (fun (cid, _) -> cid = trgtId) simData.Graph.[port.CId].Outputs.[port.OutPN] with
                | Some _ -> port
                | None -> { port with TrgtId = None }
            | _ -> { port with TrgtId = None })
    | None -> [||]

let limBits (name: string): (int * int) option =
    match Seq.tryFind ((=) '[') name, Seq.tryFind ((=) ':') name, Seq.tryFind ((=) ']') name with
    | Some, Some, Some ->
        (name.[Seq.findIndexBack ((=) '[') name + 1..Seq.findIndexBack ((=) ':') name - 1],
         name.[Seq.findIndexBack ((=) ':') name + 1..Seq.findIndexBack ((=) ']') name - 1])
        |> (fun (a, b) -> int a, int b)
        |> Some
    | _ -> None

let rec findName
        (simGraph: SimulatorTypes.SimulationGraph)
        ({ CId = compId; OutPN = outPortN; TrgtId = outputOpt }: WaveSimPort)
    =
    let compLbl =
        match Map.tryFind compId simGraph with
        | Some simComp ->
            match simComp.Label with
            | ComponentLabel lbl ->
                match Seq.tryFindIndexBack ((=) '(') lbl with
                | Some i -> lbl.[0..i - 1]
                | None -> lbl //not robust!
        | None -> failwith "simData.Graph.[compId] doesn't exist"

    let outPortInt =
        match outPortN with
        | OutputPortNumber pn -> pn

    let driveName n compTypeStr =
        match driveOut simGraph compId (InputPortNumber n) with
        | Some(driveCompId, drivePortN) ->
            findName simGraph
                { CId = driveCompId
                  OutPN = drivePortN
                  TrgtId = None }
            |> snd
        | None -> failwith (compTypeStr + "input not connected")

    match simGraph.[compId].Type with
    | Not
    | And
    | Or
    | Xor
    | Nand
    | Nor
    | Xnor
    | Decode4
    | Mux2 -> [ compLbl, (0, 0) ]
    | Input w
    | Output w 
    | Constant(w, _) -> [ compLbl, (w - 1, 0) ]
    | Demux2 -> [ compLbl + "_" + string outPortInt, (0, 0) ]
    | NbitsAdder w ->
        match outPortInt with
        | 0 -> [ compLbl + "_sum", (w - 1, 0) ]
        | _ -> [ compLbl + "Cout", (w - 1, 0) ]
    | DFF
    | DFFE -> [ compLbl + "_Q", (0, 0) ]
    | Register w
    | RegisterE w -> [ compLbl + "_data-out", (w - 1, 0) ]
    | RAM mem
    | AsyncROM mem
    | ROM mem -> [ compLbl + "_data-out", (mem.WordWidth - 1, 0) ]
    | Custom c -> [ compLbl + "_" + fst c.OutputLabels.[outPortInt], (snd c.OutputLabels.[outPortInt] - 1, 0) ]
    | IOLabel ->
        match driveOut simGraph compId (InputPortNumber 0) with
        | Some(driveCompId, drivePortN) ->
            match findName simGraph
                      { CId = driveCompId
                        OutPN = drivePortN
                        TrgtId = None }
                  |> snd with
            | hd :: tl ->
                ("(" + fst hd, snd hd) :: tl
                |> function
                | hd :: [] -> (fst hd + ")", snd hd) :: []
                | lst ->
                    List.append lst.[0..List.length lst - 2] [ fst (List.last lst) + ")", snd (List.last lst) ]
            | [] -> failwith "Error: IOLabel input names list is empty"
        | None -> failwith "IOLabel input not connected"
    | MergeWires -> List.append (driveName 1 "MergeWires") (driveName 0 "MergeWires")
    | SplitWire w ->
        let predicate (_, b) =
            match outPortInt with
            | 0 -> b >= w
            | 1 -> b < w
            | _ -> failwith "SplitWire output port number greater than 1"

        let split name msb lsb st =
            List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
            |> List.filter predicate
            |> List.unzip
            |> function
            | [], _ -> None
            | lst, _ -> Some(name, (List.max lst, List.min lst))

        (0, driveName 0 "SplitWire")
        ||> List.mapFold (fun st (name, (msb, lsb)) -> split name msb lsb st, st + msb - lsb + 1)
        |> fst
        |> List.choose id
    | BusSelection(w, oLSB) ->
        let filtSelec name msb lsb st =
            List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
            |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
            |> List.unzip
            |> function
            | [], _ -> None
            | lst, _ -> Some(name, (List.max lst, List.min lst))
        (driveName 0 "BusSelection", 0)
        ||> List.mapFoldBack (fun (name, (msb, lsb)) st -> filtSelec name msb lsb st, st + msb - lsb + 1)
        |> fst
        |> List.choose id
        |> List.rev

    |> (fun lst ->
        match outputOpt with
        | Some compId ->
            match simGraph.[compId].Label with
            | ComponentLabel lbl -> Some(lbl + ": "), lst
        | None -> None, lst)


let bitNums (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let wSPort2Name simGraph p =
    let outNameOpt, nameLst = findName simGraph p

    let tl =
        match nameLst with
        | [ el ] -> fst el + bitNums (snd el)
        | lst when List.length lst > 0 ->
            let appendName st (name, bitLims) = st + name + bitNums bitLims + ", "
            List.fold appendName "{ " lst |> (fun lbl -> lbl.[0..String.length lbl - 3] + " }")
        | _ -> ""
    match outNameOpt with
    | Some outName -> outName + tl
    | None -> tl

let extractSimTime ports (simGraph: SimulationGraph) =
    ports
    |> Array.map (fun { CId = compId; OutPN = portN; TrgtId = _ } ->
        match Map.tryFind compId simGraph with
        | Some simComp ->
            match Map.tryFind portN simComp.Outputs with
            | Some(hd :: _) ->
                let wD = simGraph.[fst hd].Inputs.[snd hd]
                Wire
                    { NBits = uint (List.length wD)
                      BitData = simWireData2Wire wD }
            | Some [] -> failwith "Output not connected"
            | None -> failwith "Component doesn't have this output port number"
        | None -> failwith "ComponentId not in simulation graph")

let clkAdvance (sD: SimulatorTypes.SimulationData) =
    feedClockTick sD.Graph
    |> (fun graph ->
        { sD with
              Graph = graph
              ClockTickNumber = sD.ClockTickNumber + 1 })

let extractSimData simData nCycles =
    (simData, [| 1u .. nCycles |])
    ||> Array.mapFold (fun s _ -> clkAdvance s, clkAdvance s)
    |> fst

let makeSimData model =
    match model.Diagram.GetCanvasState(), model.CurrProject with
    | None, _ -> None
    | _, None -> failwith "what? Cannot start a simulation without a project"
    | Some jsState, Some project ->
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> Some

let initFileWS model dispatch =
    match getCurrFile model with
    | Some fileName ->
        (fileName, initWS)
        |> AddWaveSimFile
        |> dispatch
    | None -> ()

let avalPorts (model: Model) dispatch =
    match model.Diagram.GetCanvasState(), model.CurrProject with
    | None, _ -> [||]
    | _, None -> failwith "what? Cannot start a simulation without a project"
    | Some jsState, Some project ->
        let otherComponents = project.LoadedComponents |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> function
        | Ok simData ->
            List.map (extractComponent >> Comp) (fst jsState) 
            |> compsConns2portLst model simData
        | Error simError ->
            if simError.InDependency.IsNone then
                (simError.ComponentsAffected, simError.ConnectionsAffected)
                |> SetHighlighted
                |> dispatch
            [||]

let simulate model wSMod dispatch simData =
    SetViewerWidth minViewerWidth |> dispatch
    let simData' = 
        match currWS model with
        | Some wSMod -> wSMod.LastClk
        | None -> 
            initFileWS model dispatch
            initWS.LastClk
        |> extractSimData simData
        |> Array.append [| simData |] 

    let wA' =
        match avalPorts model dispatch with
        | wSPorts ->
            let names' = Array.map (wSPort2Name simData'.[0].Graph) wSPorts
            { Ports = wSPorts; WaveNames = names' }
    { wSMod with
          SimData = simData'
          WaveAdder = wA'
          LastCanvasState = model.Diagram.GetCanvasState() }
    |> SetCurrFileWSMod
    |> dispatch

let isSimulateActive (model: Model) dispatch =
    match currWS model with
    | Some wSMod ->
        if wSMod.LastCanvasState <> model.Diagram.GetCanvasState()
            then makeSimData model, Some wSMod 
            else None, Some wSMod
    | None -> 
        initFileWS model dispatch
        None, None

/// Display top menu.
let viewTopMenu model dispatch =
    //printfn "FileView"
    let style = Style [ Width "100%" ] //leftSectionWidth model

    let projectPath, fileName =
        match model.CurrProject with
        | None -> "no open project", "no open file"
        | Some project -> project.ProjectPath, project.OpenFileName

    let makeFileLine name project =
        Navbar.Item.div [ Navbar.Item.Props [ style ] ]
            [ Level.level [ Level.Level.Props [ style ] ]
                  [ Level.left [] [ Level.item [] [ str name ] ]
                    Level.right [ Props [ Style [ MarginLeft "20px" ] ] ]
                        [ Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsPrimary
                                    Button.Disabled(name = project.OpenFileName)
                                    Button.OnClick(fun _ ->
                                        saveOpenFileAction model // Save current file.
                                        openFileInProject name project model dispatch) ] [ str "open" ] ]
                          // Add option to rename?
                          //Level.item [] [
                          //    Button.button [
                          //        Button.Size IsSmall
                          //        Button.IsOutlined
                          //        Button.Color IsInfo
                          //    ] [ str "rename" ]
                          //]
                          Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsDanger
                                    Button.OnClick(fun _ ->
                                        let title = "Delete file"

                                        let body =
                                            div []
                                                [ str "Are you sure you want to delete the follwing file?"
                                                  br []
                                                  str <| pathJoin
                                                             [| project.ProjectPath
                                                                name + ".dgm" |]
                                                  br []
                                                  str <| "This action is irreversible." ]

                                        let buttonText = "Delete"

                                        let buttonAction =
                                            fun _ ->
                                                removeFileInProject name project model dispatch
                                                dispatch ClosePopup
                                        confirmationPopup title body buttonText buttonAction dispatch) ]
                                    [ str "delete" ] ] ] ] ]


    let fileTab =
        match model.CurrProject with
        | None -> Navbar.Item.div [] []
        | Some project ->
            let projectFiles = project.LoadedComponents |> List.map (fun comp -> makeFileLine comp.Name project)
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          if model.TopMenu = Files then Closed else Files
                          |> SetTopMenu
                          |> dispatch) ] ]
                [ Navbar.Link.a [] [ str "Files" ]
                  Navbar.Dropdown.div
                      [ Navbar.Dropdown.Props
                          [ Style
                              [ Display
                                  (if (let b = model.TopMenu = Files
                                       b) then
                                      DisplayOptions.Block
                                   else
                                       DisplayOptions.None) ] ] ]
                      ([ Navbar.Item.a [ Navbar.Item.Props [ OnClick(fun _ -> addFileToProject model dispatch) ] ]
                             [ str "New file" ]
                         Navbar.divider [] [] ]
                       @ projectFiles) ]

    div [ leftSectionWidth model ]
        [ Navbar.navbar
            [ Navbar.Props
                [ Style
                    [ Height "100%"
                      Width "100%" ] ] ]
              [ Navbar.Brand.div
                  [ Props
                      [ Style
                          [ Height "100%"
                            Width "100%" ] ] ]
                    [ Navbar.Item.div
                        [ Navbar.Item.HasDropdown
                          Navbar.Item.Props
                              [ OnClick(fun _ ->
                                  if model.TopMenu = Project then Closed else Project
                                  |> SetTopMenu
                                  |> dispatch) ] ]
                          [ Navbar.Link.a [] [ str "Project" ]
                            Navbar.Dropdown.div
                                [ Navbar.Dropdown.Props
                                    [ Style
                                        [ Display
                                            (if model.TopMenu = Project then
                                                DisplayOptions.Block
                                             else
                                                 DisplayOptions.None) ] ] ]
                                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick <| newProject model dispatch ] ]
                                      [ str "New project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| openProject model dispatch ] ]
                                      [ str "Open project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| closeProject model dispatch ] ]
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
                                    [ Button.Color(if model.HasUnsavedChanges then IsSuccess else IsWhite)
                                      Button.OnClick(fun _ ->
                                          saveOpenFileAction model
                                          SetHasUnsavedChanges false
                                          |> JSDiagramMsg
                                          |> dispatch) ] [ str "Save" ] ] ]
                      Navbar.End.div []
                          [ Navbar.Item.div []
                                [ match isSimulateActive model dispatch with
                                  | Some (Ok simData), Some wSMod ->
                                      Button.button
                                          [ Button.Color IsSuccess
                                            Button.OnClick(fun _ ->
                                                simulate model wSMod dispatch simData
                                                ChangeRightTab WaveSim |> dispatch) ]
                                  | Some (Error err), _ -> 
                                      Button.button
                                          [ Button.OnClick(fun _ ->
                                                Some err |> SetWSError |> dispatch
                                                ChangeRightTab WaveSim |> dispatch) ]
                                  | _ -> Button.button []
                                |> (fun but -> but [ str "Simulate >>" ]) ] ]
                      Navbar.End.div []
                          [ Navbar.Item.div []
                                [ Button.button [ Button.OnClick(fun _ -> viewInfoPopup dispatch) ] [ str "Info" ] ] ] ] ] ]

