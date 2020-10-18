(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It is not possible to put this type among the CommonTypes as it has to
    depend on Draw2dWrapper. Furthermore, non-UI modules should be agnostic of
    the FRP model.
*)

module rec ModelType

open CommonTypes
open MessageType
open SimulatorTypes
open Draw2dWrapper

//================================//
// Componenents loaded from files //
//================================//

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromSimulation : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromWaveSim : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromFiles : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromMemoryEditor : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromProperties : ((Msg -> unit) -> Fable.React.ReactElement) option
}


type AutoSaveT = Saving | Deleting | Inactive

type AsyncTasksT = {
    AutoSave: AutoSaveT
    LastAutoSave: Map<string,System.DateTime>
    LastAutoSaveCheck: System.DateTime
    LastSavedCanvasState: Map<string,CanvasState>
    RunningSimulation: bool // placeholder - not used yet
    }

[<CustomEquality;NoComparison>]
type Model = {
    AsyncActivity: AsyncTasksT
    Diagram : Draw2dWrapper
    SimulationIsStale: bool
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    LastDetailedState: CanvasState
    LastSelectedIds: string list * string list
    CurrentSelected: Component list * Connection list
    LastUsedDialogWidth: int
    SelectedComponent : Component option // None if no component is selected.
    Simulation : Result<SimulationData,SimulationError> option // None if no simulation is running.
    WaveSim : Map<string, WaveSimModel> * (SimulationError option)
    RightTab : RightTab
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    Clipboard : CanvasState // Components and connections that have been selected and copied.
    CreateComponent : Component option // Track the last added component
    HasUnsavedChanges : bool
    IsLoading: bool
    CurrProject : Project option
    Popup : (PopupDialogData -> Fable.React.ReactElement) option
    PopupDialogData : PopupDialogData
    Notifications : Notifications
    TopMenu : TopMenu
    DragMode: DragMode
    ViewerWidth: int // waveform viewer width in pixels
    SimulationInProgress:  Result<NetGroup array,{| LastClk: uint; Curs: uint; ClkW: float |}> option
    ConnsToBeHighlighted: bool
    CheckScrollPos: bool
} with
 
    override this.GetHashCode() =
        hash (reduce this)
        
    override this.Equals(x) = 
        match x with
        | :? Model as x' -> reduce this = reduce x'
        | _ -> false

let reduce (this: Model) = {|
         RightTab = this.RightTab
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         AsyncActivity = this.AsyncActivity
         SimulationIsStale = this.SimulationIsStale
         LastSimulatedCanvasState = this.LastSimulatedCanvasState
         LastSelectedIds = this.LastSelectedIds
         CurrentSelected = this.CurrentSelected
         LastUsedDialogWidth = this.LastUsedDialogWidth
         SelectedComponent= this.SelectedComponent
         CreateComponent = this.CreateComponent
         HasUnsavedChanges = false
         CurrProject = match this.Popup with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         TopMenu = this.TopMenu
         DragMode = this.DragMode
         ViewerWidth = this.ViewerWidth
         SimulationInProgress = this.SimulationInProgress
         ConnsToBeHighlighted = this.ConnsToBeHighlighted

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightTab
         Clipboard = this.Clipboard
         CurrProject = match this.Popup with None -> false | _ -> true
         SimulationIsStale = this.SimulationIsStale
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.CreateComponent
         HasUnsavedChanges = false
         CurrProject = match this.Popup with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         DragMode = this.DragMode
         ViewerWidth = this.ViewerWidth
         SimulationInProgress = this.SimulationInProgress
 |} 

/// Lens to facilitate changing AsyncActivity
let setActivity (f: AsyncTasksT -> AsyncTasksT) (model: Model) =
    {model with AsyncActivity = f model.AsyncActivity }

let getDetailedState (model:Model) =
    model.Diagram.GetCanvasState()
    |> Option.map Extractor.extractState
    |> Option.defaultValue ([],[])

let getReducedState (model:Model) =
    model.Diagram.GetCanvasState()
    |> Option.map Extractor.extractReducedState 

let addReducedState a name model =
    let lastState = a.LastSavedCanvasState
    match getReducedState model with
    | None -> lastState
    | Some state -> lastState.Add(name, state)


let changeSimulationIsStale (b:bool) (m:Model) = 
    { m with SimulationIsStale = b}

let getComponentIds (model: Model) =
    let extractIds (jsComps,jsConns) = 
        jsComps
        |> List.map Extractor.extractComponent
        |> List.map (fun comp -> ComponentId comp.Id)
    model.Diagram.GetCanvasState()
    |> Option.map extractIds
    |> Option.defaultValue []
    |> Set.ofList

////////////////////////////
/// Saving WaveSim Model ///
////////////////////////////

/// get saveable record of waveform setup
let waveSimModel2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    { 
        ClkWidth = wsMod.ClkWidth
        Cursor = wsMod.Cursor
        Radix = wsMod.Radix
        LastClk = wsMod.LastClk
        DisplayedPortIds = 
            wsMod.DispPorts 
            |> Array.map (fun p -> p.driverNet.[0].TargetCompId)
            |> Array.map (fun cId -> match cId with | ComponentId n -> n)
    }

/// setup current WaveSimModel from saved record
let savedWaveInfo2WaveSimModel (sWInfo: SavedWaveInfo) : WaveSimModel =
    {   
        SimDataCache = [||]
        DispWaveSVGCache = [||]
        DispWaveNames = [||]
        DispPorts = [||]
        ClkWidth = sWInfo.ClkWidth
        Cursor = sWInfo.Cursor
        CursorEmpty = false
        Radix = sWInfo.Radix
        LastClk = sWInfo.LastClk
        WaveSimState = false
        WaveData = None
        LastCanvasState = None           
    }

let getSheetWaveSimOpt (model:Model) : WaveSimModel option = 
    model.CurrProject
    |> Option.bind (fun p -> Map.tryFind p.OpenFileName (fst model.WaveSim))
    

let getSheetWaveSimErr (model:Model) =
    model.CurrProject
    |> Option.map (fun p -> snd model.WaveSim)
    |> Option.defaultValue None

let getSheetWaveCanvasState (model:Model) =
    getSheetWaveSimOpt model
    |> Option.map (fun (ws:WaveSimModel) -> ws.LastCanvasState)
    |> Option.defaultValue None

let getSheetWaveNetList (model:Model) =
    getSheetWaveCanvasState model
    |> Option.map Helpers.getNetList
   

//----------------------Print functions-----------------------------//
//------------------------------------------------------------------//


let spComp (comp:Component) =
    match comp.Type with
    | Custom {Name=name; InputLabels=il; OutputLabels=ol} -> sprintf "Custom:%s(ins=%A:outs=%A)" name il il
    | x -> sprintf "%A" x

let spConn (conn:Connection) = 
    sprintf "Conn:%A" conn.Vertices

let spState ((comps,conns):CanvasState) = 
    sprintf "Canvas<%A,%A>" (List.map spComp comps) (List.map spConn conns)

let spCanvas (model:Model) = 
    model.Diagram.GetCanvasState()
    |> Option.map Extractor.extractState
    |> Option.map spState
    |> Option.defaultValue "None"

let spComps comps =  
    sprintf "Comps%A" (List.map spComp comps)

let spOpt f thingOpt = match thingOpt with |None -> "None" | Some x -> sprintf "Some %s" (f x)

let spLdComp (ldc: LoadedComponent) =
    sprintf "LDC<%s:%A:%s>" ldc.Name ldc.TimeStamp ((fst >>spComps) ldc.CanvasState)

let spProj (p:Project) =
    sprintf "PROJ||Sheet=%s\n%s||ENDP\n" p.OpenFileName (String.concat "\n" (List.map spLdComp p.LoadedComponents))

let pp model =
    printf "\n%s\n%s" (spCanvas model) (spOpt spProj model.CurrProject)

let spMess msg =
    match msg with
    //| SetProject p -> sprintf "MSG<<SetProject:%s>>ENDM" (spProj p)
    //| SetLastSimulatedCanvasState canvasOpt-> sprintf "MSG<SetLastSimCanv:%s>>ENDM" (spOpt spState canvasOpt)
    | x -> sprintf "MSG<<%20A>>ENDM" x

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

