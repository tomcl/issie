import * as fpts from "npm:fp-ts"
import * as t from "npm:io-ts"

const PortType = t.union([t.literal("Input"), t.literal("Output")])

const Port = t.type({
  Id: t.string,
  // For example, an And would have input ports 0 and 1, and output port 0.
  // If the port is used in a Connection record as Source or Target, the Number is None.
  PortNumber: t.union([t.number, t.null]),
  PortType: PortType,
  HostId: t.string,
})

const PortId = t.string

const CCForm = t.union([
  t.literal("User"),
  t.literal("Library"),
  t.literal("ProtectedTopLevel"),
  t.literal("ProtectedSubSheet"),
  t.string, // Verilog of string
])

const CustomComponentType = t.type({
  Name: t.string,
  // Tuples with (label * connection width).
  InputLabels: t.array(t.tuple([t.string, t.number])),
  OutputLabels: t.array(t.tuple([t.string, t.number])),
  Form: t.union([CCForm, t.null]),
  Description: t.union([t.string, t.null]),
})

const Memory = t.type({
  AddressWidth: t.number,
  WordWidth: t.number,
  Data: t.record(t.number, t.number),
})

const InitMemData = t.union([
  t.literal("FromData"),
  t.type({ FromFile: t.string }),
  t.type({ ToFile: t.string }),
  t.type({ ToFileBadName: t.string }),
  t.literal("UnsignedMultiplier"),
  t.literal("SignedMultiplier"),
])

const Memory1 = t.type({
  Init: InitMemData,
  AddressWidth: t.number,
  WordWidth: t.number,
  Data: t.record(t.number, t.number),
})

const ShiftComponentType = t.union([t.literal("LSL"), t.literal("LSR"), t.literal("ASR")])

const ComponentType = t.union([
  t.type({ Input: t.number }),
  t.type({ Input1: t.union([t.tuple([t.number, t.number]), t.tuple([t.number])]) }),
  t.type({ Output: t.number }),
  t.type({ Viewer: t.number }),
  t.literal("IOLabel"),
  t.type({ BusCompare: t.tuple([t.number, t.number]) }),
  t.type({ BusCompare1: t.tuple([t.number, t.number, t.string]) }),
  t.type({ BusSelection: t.tuple([t.number, t.number]) }),
  t.type({ Constant: t.tuple([t.number, t.number]) }),
  t.type({ Constant1: t.tuple([t.number, t.number, t.string]) }),
  t.literal("Not"),
  t.literal("And"),
  t.literal("Or"),
  t.literal("Xor"),
  t.literal("Nand"),
  t.literal("Nor"),
  t.literal("Xnor"),
  t.literal("Decode4"),
  t.literal("Mux2"),
  t.literal("Mux4"),
  t.literal("Mux8"),
  t.literal("Demux2"),
  t.literal("Demux4"),
  t.literal("Demux8"),
  t.type({ NbitsAdder: t.number }),
  t.type({ NbitsAdderNoCin: t.number }),
  t.type({ NbitsAdderNoCout: t.number }),
  t.type({ NbitsAdderNoCinCout: t.number }),
  t.type({ NbitsXor: t.number }),
  t.type({ NbitsAnd: t.number }),
  t.type({ NbitsNot: t.number }),
  t.type({ NbitsOr: t.number }),
  t.type({ NbitsSpreader: t.number }),
  t.type({ Custom: t.string }),
  t.literal("MergeWires"),
  t.type({ SplitWire: t.number }),
  t.literal("DFF"),
  t.literal("DFFE"),
  t.type({ Register: t.number }),
  t.type({ RegisterE: t.number }),
  t.type({ Counter: t.number }),
  t.type({ CounterNoLoad: t.number }),
  t.type({ CounterNoEnable: t.number }),
  t.type({ CounterNoEnableLoad: t.number }),
  t.type({ AsyncROM1: Memory1 }),
  t.type({ ROM1: Memory1 }),
  t.type({ RAM1: Memory1 }),
  t.type({ AsyncRAM1: Memory1 }),
  t.type({ AsyncROM: Memory }),
  t.type({ ROM: Memory }),
  t.type({ RAM: Memory }),
  t.type({ Shift: t.tuple([t.number, t.number, ShiftComponentType]) }),
])

const XYPos = t.type({
  X: t.number,
  Y: t.number,
})

const BoundingBox = t.type({
  TopLeft: XYPos,
  W: t.number,
  H: t.number,
})

const Rotation = t.union([t.literal("Degree0"), t.literal("Degree90"), t.literal("Degree180"), t.literal("Degree270")])

/// Stores the rotation and the flip of the symbol, flipped false by default
const STransform = t.type({
  Rotation: Rotation,
  flipped: t.boolean,
})

/// Represents the sides of a component
const Edge = t.union([t.literal("Top"), t.literal("Bottom"), t.literal("Left"), t.literal("Right")])

const SymbolInfo = t.type({
  LabelBoundingBox: t.union([BoundingBox, t.null]),
  LabelRotation: t.union([Rotation, t.null]),
  STransform: STransform,
  ReversedInputPorts: t.union([t.boolean, t.null]),
  PortOrientation: t.record(Edge, Edge),
  PortOrder: t.record(Edge, t.array(t.string)),
  HScale: t.union([t.number, t.null]),
  VScale: t.union([t.number, t.null]),
})

const Component = t.type({
  Id: t.string,
  Type: ComponentType,
  Label: t.string, // All components have a label that may be empty.
  InputPorts: t.array(Port), // position on this list determines inputPortNumber
  OutputPorts: t.array(Port), // position in this lits determines OutputPortNumber
  X: t.number,
  Y: t.number,
  H: t.number,
  W: t.number,
  SymbolInfo: t.union([SymbolInfo, t.null]),
})

const Connection = t.type({
  Id: t.string,
  Source: Port,
  Target: Port,
  Vertices: t.array(t.tuple([t.number, t.number, t.boolean])),
})

const ComponentId = t.string

const FComponentId = t.tuple([ComponentId, t.array(ComponentId)])

const WaveIndexT = t.type({
  SimArrayIndex: t.number,
  Id: FComponentId,
  PortType: PortType,
  PortNumber: t.number,
})

const NumberBase = t.union([t.literal("Hex"), t.literal("Dec"), t.literal("Bin"), t.literal("SDec")])

const SavedWaveInfo = t.type({
  SelectedWaves: t.union([t.array(WaveIndexT), t.null]),
  Radix: t.union([NumberBase, t.null]),
  WaveformColumnWidth: t.union([t.number, t.null]),
  ShownCycles: t.union([t.number, t.null]),
  SelectedRams: t.union([t.record(ComponentId, t.string), t.null]),
  SelectedFRams: t.union([t.record(FComponentId, t.string), t.null]),
  ClkWidth: t.union([t.number, t.null]),
  Cursor: t.union([t.number, t.null]),
  LastClk: t.union([t.number, t.null]),
  DisplayedPortIds: t.union([t.array(t.string), t.null]),
})

const SheetInfo = t.type({
  Form: t.union([CCForm, t.null]),
  Description: t.union([t.string, t.null]),
})

const CanvasState = t.tuple([t.array(Component), t.array(Connection)])

const Date = t.string

const NewCanvasWithFileWaveInfoAndNewConns = t.union([t.tuple([CanvasState, SavedWaveInfo, Date]), t.tuple([CanvasState, Date])])

const NewCanvasWithFileWaveSheetInfoAndNewConns = t.union([
  t.tuple([CanvasState, SavedWaveInfo, SheetInfo, Date]),
  t.tuple([CanvasState, SavedWaveInfo, Date]),
  t.tuple([CanvasState, SheetInfo, Date]),
  t.tuple([CanvasState, Date]),
])

const SavedInfo = t.union([NewCanvasWithFileWaveInfoAndNewConns, NewCanvasWithFileWaveSheetInfoAndNewConns])

export { SavedInfo }
