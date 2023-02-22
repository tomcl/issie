type PortType = "Input" | "Output"

interface Port {
  Id: string
  // For example, an And would have input ports 0 and 1, and output port 0.
  // If the port is used in a Connection record as Source or Target, the Number is None.
  PortNumber?: number
  PortType: PortType
  HostId: string
}

type PortId = string

type CCForm = "User" | "Library" | "ProtectedTopLevel" | "ProtectedSubSheet" | "Verilog of string"

/// Name identifies the LoadedComponent used.
/// The labels define legends on symbol designating inputs or outputs: and are the names of the Input or Output components of the CC sheet.
/// Label strings are unique per CustomComponent.
/// Label position in list determines inputPortNumber or outputPortNumber of label.
/// Multiple CustomComponent instances are differentiated by Component data.
interface CustomComponentType {
  Name: string
  // Tuples with (label * connection width).
  InputLabels: [string, number][]
  OutputLabels: [string, number][]
  Form?: CCForm
  Description?: string
}

interface Memory {
  AddressWidth: number
  WordWidth: number
  Data: Map<number, number>
}

type InitMemData = "FromData" | { FromFile: string } | { ToFile: string } | { ToFileBadName: string } | "UnsignedMultiplier" | "SignedMultiplier"

interface Memory1 {
  Init: InitMemData
  AddressWidth: number
  WordWidth: number
  Data: Map<number, number>
}

type ShiftComponentType = "LSL" | "LSR" | "ASR"

type ComponentType =
  // Legacy component: to be deleted
  | { Input: number }
  | { Input1: [number, number] | [number] }
  | { Output: number }
  | { Viewer: number }
  | "IOLabel"
  | { BusCompare: [number, number] } // TODO: check
  | { BusCompare1: [number, number, string] }
  | { BusSelection: [number, number] }
  | { Constant: [number, number] }
  | { Constant1: [number, number, string] }
  | "Not"
  | "And"
  | "Or"
  | "Xor"
  | "Nand"
  | "Nor"
  | "Xnor"
  | "Decode4"
  | "Mux2"
  | "Mux4"
  | "Mux8"
  | "Demux2"
  | "Demux4"
  | "Demux8"
  | { NbitsAdder: number }
  | { NbitsAdderNoCin: number }
  | { NbitsAdderNoCout: number }
  | { NbitsAdderNoCinCout: number }
  | { NbitsXor: number }
  | { NbitsAnd: number }
  | { NbitsNot: number }
  | { NbitsOr: number }
  | { NbitsSpreader: number }
  | { Custom: string } // schematic sheet used as component
  | "MergeWires"
  | { SplitWire: number } // int is bus width
  // DFFE is a DFF with an enable signal.
  // No initial state for DFF or Register? Default 0.
  | "DFF"
  | "DFFE"
  | { Register: number }
  | { RegisterE: number }
  | { Counter: number }
  | { CounterNoLoad: number }
  | { CounterNoEnable: number }
  | { CounterNoEnableLoad: number }
  | { AsyncROM1: Memory1 }
  | { ROM1: Memory1 }
  | { RAM1: Memory1 }
  | { AsyncRAM1: Memory1 }
  // legacy components - to be deleted
  | { AsyncROM: Memory }
  | { ROM: Memory }
  | { RAM: Memory }
  | { Shift: [number, number, ShiftComponentType] }

interface XYPos {
  X: number
  Y: number
}

interface BoundingBox {
  /// Top left corner of the bounding box
  TopLeft: XYPos
  /// Width
  W: number
  /// Height
  H: number
}

type Rotation = "Degree0" | "Degree90" | "Degree180" | "Degree270"

/// Stores the rotation and the flip of the symbol, flipped false by default
type STransform = { Rotation: Rotation; flipped: boolean }

/// Represents the sides of a component
type Edge = "Top" | "Bottom" | "Left" | "Right"

interface SymbolInfo {
  LabelBoundingBox?: BoundingBox
  LabelRotation?: Rotation
  STransform: STransform
  ReversedInputPorts?: boolean
  PortOrientation: Map<string, Edge>
  PortOrder: Map<Edge, string[]>
  HScale?: number
  VScale?: number
}

type Component = {
  Id: string
  Type: ComponentType
  Label: string // All components have a label that may be empty.
  InputPorts: Port[] // position on this list determines inputPortNumber
  OutputPorts: Port[] // position in this lits determines OutputPortNumber
  X: number
  Y: number
  H: number
  W: number
  SymbolInfo?: SymbolInfo
}

type Connection = {
  Id: string
  Source: Port
  Target: Port
  Vertices: [number, number, boolean][]
}

type ComponentId = string

type FComponentId = [ComponentId, ComponentId[]]

interface WaveIndexT {
  SimArrayIndex: number
  Id: FComponentId
  PortType: PortType
  PortNumber: number
}

type NumberBase = "Hex" | "Dec" | "Bin" | "SDec"

interface SavedWaveInfo {
  /// Waves which are selected to be shown in the waveform viewer
  SelectedWaves?: WaveIndexT[]
  /// Radix in which values are displayed in the wave simulator
  Radix?: NumberBase
  /// Width of the waveform column
  WaveformColumnWidth?: number
  /// Number of visible cycles in the waveform column
  ShownCycles?: number
  /// RAMs which are selected to be shown in the RAM tables
  SelectedRams?: Map<ComponentId, string>
  SelectedFRams?: Map<FComponentId, string>

  /// The below fields are legacy values and no longer used.
  ClkWidth?: number
  Cursor?: number
  LastClk?: number
  DisplayedPortIds?: string[]
}

interface SheetInfo {
  Form?: CCForm
  Description?: string
}

type CanvasState = [Component[], Connection[]]
type NewCanvasWithFileWaveInfoAndNewConns = [CanvasState, SavedWaveInfo, Date] | [CanvasState, Date]
type NewCanvasWithFileWaveSheetInfoAndNewConns = [CanvasState, SavedWaveInfo, SheetInfo, Date] | [CanvasState, SavedWaveInfo, Date] | [CanvasState, SheetInfo, Date] | [CanvasState, Date]

type SavedInfo = NewCanvasWithFileWaveInfoAndNewConns | NewCanvasWithFileWaveSheetInfoAndNewConns
