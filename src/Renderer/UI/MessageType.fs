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
    nBits: uint32
    bitData: bigint 
}

type StateSample = string array

type Sample = | Wire of Wire | StateSample of StateSample

type SimTime = Sample array

type Waveform = Sample array

type PosParamsType = {
    sigHeight: float
    hPos: uint
    clkWidth: float
    labelWidth: uint
    sigThick: float
    boxWidth: uint
    boxHeight: uint
    spacing: float
    clkThick: float 
}

type WaveSimModel = {
    waveData: SimTime array
    waveNames: WaveName array
    selected: bool array
    posParams: PosParamsType
    cursor: uint32 
    radix: NumberBase
    viewIndexes: uint32*uint32
}

type Msg =
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | StartSimulation of Result<SimulationData, SimulationError>
    | StartWaveSim of WaveSimModel
    | SetSimulationGraph of SimulationGraph
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick
    | EndSimulation
    | ChangeRightTab of RightTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetClipboard of CanvasState
    | SetCreateComponentOffset of int
    | SetProject of Project
    | CloseProject
    | ShowPopup of (PopupDialogData -> ReactElement)
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogInt of int option
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
