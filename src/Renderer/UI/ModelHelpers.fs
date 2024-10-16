module ModelHelpers
open Fulma
open Fable.React
open Fable.React.Props
open CommonTypes
open Sheet.SheetInterface
open ModelType
open Elmish
open Optics
open Optics.Operators
open SimTypes



module Constants =
    /// Needed to prevent possible overrun of simulation arrays
    let multipliers = [1;2;5;10;20;50;100;200;500;1000]
    let maxStepsOverflow = 3
    let waveSimRequiredArraySize wsModel = wsModel.WSConfig.LastClock + maxStepsOverflow + List.last multipliers
    let defaultWSConfig = {
            LastClock = 2000; // Simulation array limit during wave simulation
            FirstClock = 0; // first clock accessible - limits scroll range. NOT IMPLEMENTED
            FontSize = 15; // size of text on waveforms
            FontWeight = 500 // weight of text on waveforms
            }
    let maxWarnSimulationSize = 100000
    let maxSimulationSize = 4000000
    let minScrollingWindow = 200

    let wsButtonHeight = 30
    let wsButtonWidth = 120
    let wsButtonFontSize = 16

    /// initial number of clock cycles navigated by the scrollbar.
    let scrollbarBkgRepCyclesInit = 100

/// type used for CSS grids in the UI to position an item on a grid
type CSSGridPos =
    | PosElement of int * int
    | PosAreaSpan of startX: int * startY: int * spanX: int * spanY: int
    | PosAreaAbsolute of startX: int * startY: int * spanX: int * spanY: int


let initWSModel  : WaveSimModel = {
    DefaultCursor = CursorType.Default
    TopSheet = ""
    WSConfig = Constants.defaultWSConfig
    WSConfigDialog = None
    Sheets = Map.empty
    State = Empty
    AllWaves = Map.empty
    SelectedWaves = List.empty
    StartCycle = 0
    ShownCycles = 5
    SamplingZoom = 1
    CursorDisplayCycle = 0
    CursorExactClkCycle = 0
    ClkCycleBoxIsEmpty = false
    Radix = Hex
    WaveformColumnWidth = Constants.initialWaveformColWidth
    WaveModalActive = false
    RamModalActive = false
    RamComps = []
    SelectedRams = Map.empty
    RamStartLocation = Map.empty
    SearchString = ""
    ShowComponentDetail = Set.empty
    ShowSheetDetail = Set.empty
    ShowGroupDetail = Set.empty
    HoveredLabel = None
    DraggedIndex = None
    PrevSelectedWaves = None
    
    ScrollbarTbWidth = 0.0 // overwritten when first rendered
    ScrollbarTbPos = 0.0 // overwritten when first rendered
    ScrollbarTbOffset = None // default value: not in scroll
    ScrollbarBkgWidth = 0.0 // overwritten when first rendered
    ScrollbarBkgRepCycs = Constants.scrollbarBkgRepCyclesInit // default value
    ScrollbarQueueIsEmpty = true // default value: empty scroll queue
}

/// This is needed because DrawBlock cannot directly access Issie Model.
/// can be replaced when all Model is placed at start of compile order and DB
/// model is refactored
let drawBlockModelToUserData (model: Model) (userData: UserData)=
    let bwModel =model.Sheet.Wire
    {userData with WireType = bwModel.Type; ArrowDisplay = bwModel.ArrowDisplay}

/// This is needed because DrawBlock cannot directly access Issie Model.
/// can be replaced when all Model is placed at start of compile order and DB
/// model is refactored
let userDataToDrawBlockModel (model: Model) =
    let userData = model.UserData
    {model with 
        Sheet = 
            {model.Sheet with 
                Wire = {
                    model.Sheet.Wire with 
                        Type = userData.WireType
                        ArrowDisplay = userData.ArrowDisplay
                        Symbol = {
                            model.Sheet.Wire.Symbol with Theme = userData.Theme
                        }}}}

let reduce (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         LastSimulatedCanvasState = this.LastSimulatedCanvasState
         LastSelectedIds = this.LastSelectedIds
         CurrentSelected = this.CurrentSelected
         LastUsedDialogWidth = this.LastUsedDialogWidth
         SelectedComponent= this.SelectedComponent
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         TopMenu = this.TopMenuOpenState
         DragMode = this.DividerDragMode
         ViewerWidth = this.WaveSimViewerWidth
         ConnsToBeHighlighted = this.ConnsOfSelectedWavesAreHighlighted

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Clipboard = this.Clipboard
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         PopupDialogData = this.PopupDialogData
         DragMode = this.DividerDragMode
         ViewerWidth = this.WaveSimViewerWidth
 |} 

let mapFst mapFn (model,cmd) = mapFn model, cmd

let mapOverProject defaultValue (model: Model) transform =
    match model.CurrentProj with
    | None -> defaultValue
    | Some p -> transform p

let getComponentIds (model: Model) =
    let extractIds ((comps,conns): Component list * Connection list) = 
        conns
        |> List.map (fun comp -> ComponentId comp.Id)
        
    model.Sheet.GetCanvasState()
    |> extractIds
    |> Set.ofList

//------------------------//
// Saving WaveSim Model   //
//------------------------//

/// Get saveable record of WaveSimModel
let getSavedWaveInfo (wsModel: WaveSimModel) : SavedWaveInfo =
    {
        SelectedWaves = Some wsModel.SelectedWaves
        Radix = Some wsModel.Radix
        WaveformColumnWidth = Some wsModel.WaveformColumnWidth
        SelectedFRams = Some wsModel.SelectedRams
        SelectedRams = None

        WSConfig = Some wsModel.WSConfig

        // The following fields are from the old waveform simulator.
        // They are no longer used.
        ClkWidth = None
        Cursor = None
        LastClk = None
        DisplayedPortIds = None
    }

/// Setup current WaveSimModel from saved record
/// NB: note that SavedWaveInfo can only be changed if code is added to make loading backwards compatible with
/// old designs
let loadWSModelFromSavedWaveInfo (swInfo: SavedWaveInfo) : WaveSimModel =
    {
        initWSModel with
            SelectedWaves = Option.defaultValue initWSModel.SelectedWaves swInfo.SelectedWaves
            Radix = Option.defaultValue initWSModel.Radix swInfo.Radix
            WaveformColumnWidth = Option.defaultValue initWSModel.WaveformColumnWidth swInfo.WaveformColumnWidth
            SelectedRams = Option.defaultValue initWSModel.SelectedRams swInfo.SelectedFRams
            WSConfig =Option.defaultValue initWSModel.WSConfig swInfo.WSConfig
    }

//----------------------Print functions-----------------------------//
//------------------------------------------------------------------//

let spComp (comp:Component) =
    match comp.Type with
    | Custom {Name=name; InputLabels=il; OutputLabels=ol} -> sprintf "Custom:%s(ins=%A:outs=%A)" name il ol
    | x -> sprintf "%A" x

let spConn (conn:Connection) = 
    sprintf "Conn:%A" conn.Vertices

let spState ((comps,conns):CanvasState) = 
    sprintf "Canvas<%A,%A>" (List.map spComp comps) (List.map spConn conns)

let spCanvas (model : Model) = 
    model.Sheet.GetCanvasState()
    |> spState

let spComps comps =  
    sprintf "Comps%A" (List.map spComp comps)

let spOpt f thingOpt = match thingOpt with |None -> "None" | Some x -> sprintf "Some %s" (f x)

let spLdComp (ldc: LoadedComponent) =
    sprintf "LDC<%s:%A:%s>" ldc.Name ldc.TimeStamp ((fst >>spComps) ldc.CanvasState)

let spProj (p:Project) =
    sprintf "PROJ||Sheet=%s\n%s||ENDP\n" p.OpenFileName (String.concat "\n" (List.map spLdComp p.LoadedComponents))

let pp model =
    printf "\n%s\n%s" (spCanvas model) (spOpt spProj model.CurrentProj)

let spMess msg =
    match msg with
    //| SetProject p -> sprintf "MSG<<SetProject:%s>>ENDM" (spProj p)
    //| SetLastSimulatedCanvasState canvasOpt-> sprintf "MSG<SetLastSimCanv:%s>>ENDM" (spOpt spState canvasOpt)
    | x -> sprintf "MSG<<%20A>>ENDM" x

let tryGetLoadedComponents model =
    match model.CurrentProj with
    | Some p -> p.LoadedComponents
    | _ -> []

let updateLdComps (name:string) (changeFun: LoadedComponent -> LoadedComponent)  (ldComps: LoadedComponent list)=
    ldComps
    |> List.map (fun ldc -> if ldc.Name=name then changeFun ldc else ldc)

let updateLdCompsWithCompOpt (newCompOpt:LoadedComponent option) (ldComps: LoadedComponent list) =
    match newCompOpt with 
    | None -> ldComps // no update
    | Some newComp -> 
        match List.tryFind (fun (ldc:LoadedComponent) -> ldc.Name = newComp.Name) ldComps with
        | None -> newComp :: ldComps
        | Some _ -> updateLdComps newComp.Name (fun _ -> newComp) ldComps

/// returns a string option representing the current file name if file is loaded, otherwise None
let getCurrFile (model: Model) =
    match model.CurrentProj with
    | Some proj -> Some proj.OpenFileName
    | None -> None

let getCurrSheets (model: Model) =
    match model.CurrentProj with
    | Some proj -> 
        proj.LoadedComponents
        |> List.map (fun lc -> lc.Name)
        |> Some
    | None -> None

/// For reasons of space efficiency, ensure that no non-empty unused FastSimulation records are kept
/// FastSimulation records can be very large and at most one should exist, it must be for the sheet referenced by
/// model.WaveSimSheet
let removeAllSimulationsFromModel (model:Model) = model


/// Get the current WaveSimModel used by the Model (index the map using the current wavesim sheet).
/// If no WaveSimModel for that sheet, return an empty wave sim model.
let rec getWSModel model : WaveSimModel =
    match model.WaveSimSheet with
    | Some sheet ->
        Map.tryFind sheet model.WaveSim
        |> function
            | Some wsModel ->
                // printf "Sheet %A found in model" model.WaveSimSheet
                wsModel
            | None ->
                // printf "Sheet %A not found in model" model.WaveSimSheet
                initWSModel
    | None ->
        match getCurrFile model with
        | None -> 
            initWSModel
        | Some sheet ->
            getWSModel {model with WaveSimSheet = Some sheet}        

/// Set WaveSimModel of current sheet.
let setWSModel (wsModel: WaveSimModel) (model: Model) =
    match getCurrSheets model, model.WaveSimOrCurrentSheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        { model with WaveSim = Map.add wsSheet wsModel model.WaveSim }
    | Some sheets, wsSheet ->
        failwithf $"What? can't find {wsSheet} in {sheets} to set WSModel"
    | None, _ ->
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, project is closed" model.WaveSimSheet
        model

/// This will - given a project is open - never fail. The getter returns the default WaveSimModel record if none
/// exists. The setter will add the WaveSimModel to the WaveSim map in the model.
let waveSimModel_ =
    let setter (wsr: WaveSimModel) (m: Model) =
        {m with WaveSim = Map.add m.WaveSimOrCurrentSheet wsr m.WaveSim}
    let getter (m: Model) =
        match Map.tryFind m.WaveSimOrCurrentSheet m.WaveSim with
        | Some wsm-> wsm
        | None -> initWSModel
    Lens.create getter setter

/// Update WaveSimModel of current sheet.
let updateWSModel (updateFn: WaveSimModel -> WaveSimModel) (model: Model) =
    match getCurrSheets model, model.WaveSimOrCurrentSheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        let ws = model.WaveSim[wsSheet]
        { model with WaveSim = Map.add wsSheet (updateFn ws) model.WaveSim }
    | Some sheets, wsSheet ->
        failwithf $"What? can't find {wsSheet} in {sheets} to set WSModel"
    | None, _ ->
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, project is closed" model.WaveSimSheet
        model

/// Update WaveSimModel of given sheet - if it does not exist do nothing
let updateWSModelOfSheet (sheet: string) (updateFn: WaveSimModel -> WaveSimModel) (model: Model) =
    match getCurrSheets model, sheet with
    | Some sheets, wsSheet when List.contains wsSheet sheets ->
        let ws = model.WaveSim[wsSheet]
        { model with WaveSim = Map.add wsSheet (updateFn ws) model.WaveSim }
    | None, _ ->
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, project is closed" model.WaveSimSheet
        model
    | Some sheets, wsSheet ->
        printfn "\n\n******* What? trying to set wsmod when WaveSimSheet '%A' is not valid, sheets=%A" wsSheet sheets
        //failwithf "Help"
        model

/// a long function to be executed in a message after the view function has run at least once
type ViewableJob = {
    JobWork: Model-> Model * Cmd<Msg>
    ViewHasExecuted: bool
    JobName: string
    }

/// list of jobs awaiting execution
let mutable asyncJobs: ViewableJob list = []

let runAfterView (jobName:string) ( workFn: Model -> Model * Cmd<Msg>) =
    let job = {JobWork=workFn; ViewHasExecuted = false; JobName = jobName}
    printfn $"scheduling {jobName}"
    asyncJobs <- List.append asyncJobs [job]

let setAsyncJobsRunnable dispatch =
    dispatch DoNothing
    if asyncJobs.Length > 0 then 
        printfn "setting asynch jobs to vieHasExecuted"
    asyncJobs <- 
        asyncJobs 
        |> List.map (fun job -> {job with ViewHasExecuted = true}); 

/// called from update function, it will execute outstanding async jobs.
/// each job modifies model.
let execOneAsyncJobIfPossible (model: Model,cmd: Cmd<Msg>)=
    asyncJobs
    |> List.filter (fun job -> job.ViewHasExecuted) 
    |> function 
        | [] -> (model,cmd)
        | job::_ -> 
            asyncJobs <- List.filter (fun job' -> job'.JobName <> job.JobName) asyncJobs 
            job.JobWork model
            |> (fun (model', cmd') -> model', Cmd.batch [cmd; cmd'])

/// Return the project with with open file contents in loadedcomponents updated according to
/// current Draw Block contents.
let getUpdatedLoadedComponents (project: Project) (model: Model) : Project =
    mapOverProject project model ( fun p ->
        p
        |> Optic.set (loadedComponentOf_ p.OpenFileName >-> canvasState_) (model.Sheet.GetCanvasState()))

/// Set the part of model specified by optic_ to initToSet: bounded by maxVal, minVal.
/// dispatch: the Elmihs dispatch function.
let setModelInt (optic_: Lens<Model,int>) (dispatch: Msg -> unit) maxVal minVal intToSet : unit =
    let intToSet = if intToSet > maxVal then maxVal else if intToSet < minVal then minVal else intToSet
    dispatch <| UpdateModel (Optics.Optic.set optic_ intToSet)

//--------------------------------------------------------------------------------------------//
//------------------------React Input Boxes for numeric Parsing with Elmish-------------------//
//--------------------------------------------------------------------------------------------//

// Code should be refactored to use these throughout

/// Both input text and its parsed numeric value must be stored in the model.
/// The two fields contain optics used to access these items in the model
type ModelLocations = {
     TextOptic_: Optics.Lens<Model,string>
     ValOptic_: Lens<Model,bigint>
}

/// <summary> Display an input box which is parsed as a bigint and written back to the Model using
/// textOptic (the text) and valOptic (the value). isValid must return true for the value to be written back to
/// the model</summary>
let inputBigint
        (props: IHTMLProp list)
        (placeholder:string)
        (locs: ModelLocations)
        (isValid: bigint -> Model -> bool)
        (dispatch: Msg -> unit) 
        (model:Model): ReactElement =

    let isNowValid bigNum =  isValid bigNum (Optic.set locs.ValOptic_ bigNum model)

    let parseInput (text:string) =
        dispatch <| UpdateModel (Optics.Optic.set locs.TextOptic_ text)
        match NumberHelpers.strToBigint text with
        | Ok big when isNowValid big ->
            dispatch <| UpdateModel (Optic.set locs.ValOptic_ big)
        | _ -> ()
            
    Input.text [
        Input.Props (props @ [OnPaste PopupHelpers.preventDefault; AutoFocus true; SpellCheck false])
        Input.Placeholder placeholder
        Input.DefaultValue (model |> Optics.Optic.get locs.TextOptic_)
        Input.OnChange (JSHelpers.getTextEventValue >> parseInput)
    ]


//---------------------------------------------------------------------------------------------//
//----------------------------View level simulation interface-----------------------------------//
//---------------------------------------------------------------------------------------------//
//
// Add-on to simulator.fs code. This is the interface to the simulator from the view level.
// it must be here because it references Model types.
//
open SimGraphTypes

let simReset dispatch =
    dispatch CloseSimulationNotification // Close error notifications.
    dispatch ClosePropertiesNotification
    dispatch <| Sheet (DrawModelType.SheetT.ResetSelection) // Remove highlights.
    dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.

/// Return the waveform simulation graph, or an error if simulation would fail.
/// This function does not create the simulation so is relatively fast.
let validateWaveModel (simulatedSheet: string option) openSheetCanvasState model =
    match openSheetCanvasState, model.CurrentProj with
    | _, None -> 
        Error (Simulator.makeDummySimulationError "What - Internal Simulation Error starting simulation - I don't think this can happen!")
    | canvasState, Some project ->
        let simSheet = Option.defaultValue project.OpenFileName simulatedSheet
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> Simulator.validateWaveSimulation project.OpenFileName simSheet
    

/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails.
/// Note that simulation is only redone if current canvas changes.
let simulateModel (isWaveSim: bool) (simulatedSheet: string option) (simulationArraySize: int) openSheetCanvasState model =
    let start = TimeHelpers.getTimeMs()
    match openSheetCanvasState, model.CurrentProj with
    | _, None -> 
        Error (Simulator.makeDummySimulationError "What - Internal Simulation Error starting simulation - I don't think this can happen!"), openSheetCanvasState
    | canvasState, Some project ->
        let simSheet = Option.defaultValue project.OpenFileName simulatedSheet
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> Simulator.prepareSimulationMemoized isWaveSim simulationArraySize project.OpenFileName simSheet 
        |> TimeHelpers.instrumentInterval "MakeSimData" start

let resimulateWaveSimForErrors (model: Model) : Result<SimulationData, SimulationError>  =
    let canv = model.Sheet.GetCanvasState()
    let ws = getWSModel model
    let simSize =
        match ws.State with
        | Success | Loading -> Constants.waveSimRequiredArraySize ws
        | _ -> 10 // small value does not matter what it is.
    simulateModel true model.WaveSimSheet simSize canv model
    |> fst
    
