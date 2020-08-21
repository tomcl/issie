module DiagramMessageType

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
    Address : int option // Only show the specified memory address.
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
    | CtrlS | AltC | AltV | AltZ | AltShiftZ

// WaveSim types 

type WaveName = string

type Wire = {
    NBits: uint32
    BitData: bigint 
}

type StateSample = string array
type Sample = | Wire of Wire | StateSample of StateSample
type SimTime = Sample array
type Waveform = Sample array
type WaveSimPort = {
    CId : ComponentId;
    OutPN : OutputPortNumber;
    TrgtId : ComponentId option
}
type WaveAdderModel = {
    Ports : (WaveSimPort * bool) array;
    WaveNames : WaveName array;
    SimData : SimulatorTypes.SimulationData option
}

type WaveSimModel = {
    SimData: SimulatorTypes.SimulationData array
    WaveData: SimTime array
    //WaveNames: WaveName array
    Selected: bool array
    Ports: WaveSimPort array
    ClkWidth: float
    Cursor: uint32 
    Radix: NumberBase
    LastClk: uint32
    WaveAdder: WaveAdderModel
}

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuNewFile
    | MenuZoom of float



type Msg =
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | StartSimulation of Result<SimulationData, SimulationError>
    | StartWaveSim of Result<WaveSimModel, SimulationError>
    | SetSimulationGraph of SimulationGraph
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick
    | EndSimulation
    | ChangeRightTab of RightTab
    | SetHighlighted of ComponentId list * ConnectionId list
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
