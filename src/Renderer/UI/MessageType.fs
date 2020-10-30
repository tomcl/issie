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

type WaveDispT = { Port: NetGroup; Name: string}

type WaveTempT = {
    /// generate data using this, which comes from makesimdata
    InitWaveSimGraph : SimulationData option
    /// all the nets that exist in the currently simulated design
    AllNetGroups : NetGroup array;
    /// names for all ports in currently simulated design
    AllWaveNames : WaveName array
}

type WaveSimModel = {
    /// array of variable length
    SimDataCache: SimulatorTypes.SimulationData array
    /// waveform names displayed, use findName
    DispWaveNames: string array
    /// react SVG for each waveform
    DispWaveSVGCache: ReactElement array
    /// NetGroups displayed, as in WaveNames
    DispPorts: NetGroup array
    /// width of one clock in SVG units
    ClkWidth: float
    /// position of cursor (0 = first cycle)
    Cursor: uint32 
    /// tracks when the cursor text box is empty string
    CursorEmpty: bool
    /// for waveforms display
    Radix: NumberBase
    /// last clock cycle (index) of the generated SVG
    LastClk: uint32
    /// if Adder window is currently open (changing tab does not effect it)
    WaveSimState: bool
    /// data needed by the waveform Edit window (adder)
    WaveData: WaveTempT option
    /// the circuit that is being simulated - the canvas may have changed
    LastCanvasState: CanvasState option 
}

let initWA netGroups= 
    { InitWaveSimGraph = None; AllNetGroups = netGroups; AllWaveNames = [||] }

let initWS: WaveSimModel =
    { SimDataCache = [||]
      DispWaveNames = [||]
      DispWaveSVGCache = [||]
      DispPorts = [||] 
      ClkWidth = 1.0
      Cursor = 0u
      CursorEmpty = false
      Radix = Bin
      LastClk = 9u 
      WaveSimState = false
      WaveData = None
      LastCanvasState = None 
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
    | SetSimIsStale of bool
    | SetIsLoading of bool
    | SetSimInProgress of Result<NetGroup array,{| LastClk: uint; Curs: uint; ClkW: float |}>
    | SetWaveSimModel of Sheet: string * WSModel: WaveSimModel
    | SimulateWhenInProgress of Result<NetGroup array,{| LastClk: uint; Curs: uint; ClkW: float |}>
    | SetSimNotInProgress
    | SetLastSimulatedCanvasState of CanvasState option
 //   | StartNewWaveSimulation of CanvasState
    | UpdateScrollPos of bool
