module MessageType

open CommonTypes
open JSTypes
open SimulatorTypes
open Fable.React

type RightTab =
    | Properties
    | Catalogue
    | Simulation
    | WaveSim

type MemoryEditorData = {
    OnlyDiff : bool // Only show diffs in Memory Diff Viewer.
    Address : int64 option // Only show the specified memory address.
    Start: int64
    NumberBase : NumberBase
}

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Int : int option;
    Int2: int option
    MemorySetup : (int * int) option // AddressWidth, WordWidth. 
    MemoryEditorData : MemoryEditorData option // For memory editor and viewer.
}

type TopMenu = | Closed | Project | Files

//==========//
// Messages //
//==========//

// Messages that will be sent from JS code.
type JSDiagramMsg =
    | InitCanvas of JSCanvas // Has to be dispatched only once.
    | SelectComponent of JSComponent
    | UnselectComponent of unit
    | InferWidths of unit
    | SetHasUnsavedChanges of bool

// Messages that will be triggered on key combinations.
type KeyboardShortcutMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

//---------------------------------------------------------------
//---------------------WaveSim types-----------------------------
//---------------------------------------------------------------

(*
WaveSim state.

Principles: 
1) at any time wavesim simulates a stored model.LastSimulatedCanvas circuit which is guaranteed working if it exists 
2) pressing "simulate button" updates this circuit
3) wavesim has two views: adder and waveforms. each waveform is the signal on a netGroup (set of connections from one driver to multiple sources).
4) waveforms display view based on: selected waveforms, zoom, cursor position, etc
5) simulation is rerun automatically as needed to generate current display, but previous simulation results are reused where possible
6) adder view interfaces with current circuit, colouring selected nets green, and allowing selection on nets to determine
selected waveforms. If current circuit has changed only driving components still on circuit can be used this way.
7) simulate button color determines status; if circuit has chnaged from that simulated it will be orange (errors) or green (OK to rerun simulation).
8) list of currently displayed ports is held in state and saved / restored with each sheet. LastSimulatedCanvas (and simulation data) are not saved/restored but
are recalculated when needed. List of possible to display ports used by waveadder

Data structures for internal state

LastSimulatedState: Record with canvas, netlist, ports, initial simulation etc all recreated whenever simulation button is pressed and absent initially and after simulating with errors.
WaveAdder: Record always present contains simulation parameters, including which ports are currently selected

WaveAdder is


*)



type WaveName = string

type Wire = {
    NBits: uint32
    BitData: bigint 
}

type StateSample = string array
type Sample = | Wire of Wire | StateSample of StateSample
type SimTime = Sample array
type Waveform = Sample array
type SVGCacheT = {
    Top: ReactElement []
    Waves: Map<string,ReactElement>
    Bottom: ReactElement []
    }

type SimParamsT = {
    /// radix for numbers on SVG waveforms display
    WaveViewerRadix: NumberBase
    /// last clock cycle (index) of the generated SVG
    LastClkTime: uint
    /// position of cursor (0 = first cycle)
    CursorTime: uint
    /// width of one clock in SVG units
    ClkSvgWidth: float
    /// names of NetGroups selected in wave editor and displayed in wave viewer
    DispNames: string array
    /// current scrolling position of waveform svg (used to possibly extend svgs if scrolling off screen)
    LastScrollPos: float option
}


type WSViewT = 
    | WSClosed 
    | WSInitEditorOpen
    | WSEditorOpen
    | WSViewerOpen

type SimActionT = 
    | MakeSVGs of NetGroup array 
    | ChangeParameters of SimParamsT


type WaveSimModel = {
    /// generate data using this 0 clock simulation, which comes from makeSimData
    /// TODO: get rid of this and use only SimDataCache, since this is SimDataCache[0]
    InitWaveSimGraph : SimulationData option
    
    /// parameters determining how and which viewer waves are displayed
    SimParams: SimParamsT

    /// NetGroup names shown in the editor
    AllWaveNames: string array
    /// Map of all the nets that exist in the currently simulated design
    AllNets: Map<string,NetGroup>

    /// react SVG for each waveform, indexed by name
    DispWaveSVGCache: SVGCacheT 
    /// Simulation output sample array of variable length
    SimDataCache: SimulatorTypes.SimulationData array
    
    /// Hack to detect when  cursor text box is empty and use 0.
    /// TODO - get rid of this - it should not be needed
    CursorBoxIsEmpty: bool
   
    WSViewState: WSViewT

    WSTransition: (SimParamsT * WSViewT) option
    /// the circuit that is being simulated - the canvas may have changed
    LastCanvasState: CanvasState option 
    } 

let setSimParams (setFn: SimParamsT -> SimParamsT) (wsm:WaveSimModel) =
    {wsm with SimParams = setFn wsm.SimParams}

let setDispNames names wsMod = 
    setSimParams (fun sp -> {sp with DispNames=names}) wsMod

let setEditorView view wsModel =
    {wsModel with WSViewState = view; WSTransition = None}


    
let setEditorNextView nView simParas wsModel =
    {wsModel with WSTransition = Some(simParas, nView)}


    

    

let inline getPort (ws:WaveSimModel) (name: string) = ws.AllNets.[name]

let inline getDispName (ws:WaveSimModel) (port:NetGroup) =
    Map.tryFindKey (fun k v -> v = port) ws.AllNets
    |> Option.defaultValue "name not found"
    

let inline dispPorts (ws: WaveSimModel) =
    ws.SimParams.DispNames
    |> Array.map (fun name -> ws.AllNets.[name])

let inline AllPorts (ws: WaveSimModel) =
    ws.AllWaveNames

let initWS (allNames:string array) (allPorts: Map<string,NetGroup>): WaveSimModel =
    { 
      InitWaveSimGraph = None
      AllNets = allPorts
      AllWaveNames = allNames
      SimDataCache = [||]
      DispWaveSVGCache = { Top = [||]; Waves = Map.empty; Bottom = [||]}
      SimParams = {
        DispNames = [||]
        ClkSvgWidth = 1.0
        CursorTime = 0u
        WaveViewerRadix = Bin
        LastClkTime = 9u 
        LastScrollPos = None
      }
      WSViewState = WSClosed
      WSTransition =None
      LastCanvasState = None 
      CursorBoxIsEmpty = false
    }


type DiagEl = | Comp of Component | Conn of Connection

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuNewFile
    | MenuZoom of float


/// Type for an open project which represents a complete design.
/// ProjectPath is directory containing project files.
/// OpenFileName is name of file from which current schematic sheet is loaded/saved, without extension or path
/// LoadedComponents contains the list of schematic sheets, each as a component, one per sheet.
type Project = {
    /// directory which contains the project files
    ProjectPath : string
    /// name of open sheet (without extension)
    OpenFileName : string
    /// componnets have one-one correspondence with files
    LoadedComponents : LoadedComponent list
}



type Msg =
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | StartSimulation of Result<SimulationData, SimulationError>
    | SetLastSavedCanvas of string * CanvasState
    | SetCurrFileWSMod of WaveSimModel
    | SetWSError of SimulationError option
    | AddWaveSimFile of string * WaveSimModel
    | SetSimulationGraph of SimulationGraph
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick
    | EndSimulation
    | EndWaveSim
    | ChangeRightTab of RightTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | CloseProject
    | ShowPopup of (PopupDialogData -> ReactElement)
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogInt of int option
    | SetPopupDialogTwoInts of (int option * IntMode)
    | SetPopupDialogMemorySetup of (int * int) option
    | SetPopupMemoryEditorData of MemoryEditorData option
    | SetSelectedComponentMemoryLocation of int64 * int64
    | CloseDiagramNotification
    | SetSimulationNotification of ((Msg -> unit) -> ReactElement)
    | CloseSimulationNotification
    | CloseWaveSimNotification
    | SetFilesNotification of ((Msg -> unit) -> ReactElement)
    | CloseFilesNotification
    | SetMemoryEditorNotification of ((Msg -> unit) -> ReactElement)
    | CloseMemoryEditorNotification
    | SetPropertiesNotification of ((Msg -> unit) -> ReactElement)
    | ClosePropertiesNotification
    | SetTopMenu of TopMenu
    | ReloadSelectedComponent of int
    | SetDragMode of DragMode
    | SetViewerWidth of int
    | MenuAction of MenuCommand * (Msg -> unit)
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetWaveSimIsStale of bool
    | SetIsLoading of bool
    | SetWaveSimModel of Sheet: string * WSModel: WaveSimModel
    | WaveSimulateNow
    | InitiateWaveSimulation of (WSViewT * SimParamsT)
    | SetLastSimulatedCanvasState of CanvasState option
 //   | StartNewWaveSimulation of CanvasState
    | UpdateScrollPos of bool
    | SetLastScrollPos of float option
