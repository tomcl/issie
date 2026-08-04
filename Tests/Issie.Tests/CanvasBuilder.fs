/// Programmatic construction of canvases and loaded components for tests.
/// Geometry fields are arbitrary: tests never render.
module CanvasBuilder

open CommonTypes

/// A component with explicit input/output port counts
let makeComp (id: string) (nInputs: int) (nOutputs: int) (compType: ComponentType) (label: string) : Component =
    let makePorts n portType =
        [ for i in 0 .. n - 1 ->
            { Id = $"{id}-{portType}-{i}"
              PortNumber = Some i
              PortType = portType
              HostId = id } ]
    { Id = id
      Type = compType
      Label = label
      InputPorts = makePorts nInputs PortType.Input
      OutputPorts = makePorts nOutputs PortType.Output
      X = 0.
      Y = 0.
      H = 30.
      W = 30.
      SymbolInfo = None
      SlotInfo = None }

/// Connect output port `srcPort` of `src` to input port `tgtPort` of `tgt`.
/// Connection-end ports carry no port number, as in saved .dgm files.
let conn (src: Component) (srcPort: int) (tgt: Component) (tgtPort: int) : Connection =
    { Id = $"conn-{src.Id}.{srcPort}-{tgt.Id}.{tgtPort}"
      Source = { src.OutputPorts[srcPort] with PortNumber = None }
      Target = { tgt.InputPorts[tgtPort] with PortNumber = None }
      Vertices = [] }

/// A LoadedComponent wrapping a canvas, with optional parameter definitions
let makeLdc (name: string) (paramDefs: ParameterTypes.ParameterDefs option) (canvas: CanvasState) : LoadedComponent =
    let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas
    { Name = name
      TimeStamp = System.DateTime.MinValue
      FilePath = ""
      WaveInfo = None
      CanvasState = canvas
      InputLabels = inputs
      OutputLabels = outputs
      LCParameterSlots = paramDefs
      Form = Some User
      LoadedComponentIsOutOfDate = false
      IsTopSheet = false
      Description = None }

/// A custom component type instantiating `ldc`, with explicit port widths (which must
/// be the widths the instance resolves to) and optional instance parameter bindings
let customOf
    (ldc: LoadedComponent)
    (inputLabels: (string * int) list)
    (outputLabels: (string * int) list)
    (bindings: ParameterTypes.ParamBindings option)
    : ComponentType =
    Custom
        { Name = ldc.Name
          InputLabels = inputLabels
          OutputLabels = outputLabels
          Form = ldc.Form
          ParameterBindings = bindings
          Description = None }

/// A minimal draw-block symbol wrapping a component. Only the fields the test reads are
/// meaningful; the rest are zeroed. Symbol.createNewSymbol cannot be used here because it mints a
/// uuid through a Fable import, which is dummy code under .NET.
let makeSymbol (comp: Component) : DrawModelType.SymbolT.Symbol =
    let zero: XYPos = { X = 0.; Y = 0. }
    { Pos = zero
      CentrePos = zero
      OffsetFromBBCentre = zero
      InWidth0 = None
      InWidth1 = None
      InWidths = None
      LabelBoundingBox = { TopLeft = zero; W = 0.; H = 0. }
      LabelHasDefaultPos = true
      LabelRotation = None
      Appearance =
        { ShowPorts = DrawModelType.SymbolT.ShowNone
          ShowCorners = DrawModelType.SymbolT.DontShow
          HighlightLabel = false
          Colour = "lightgray"
          Opacity = 1.0 }
      Id = ComponentId comp.Id
      Component = comp
      DeclaredSlots = Map.empty
      DeclaredPortLabels = None
      Annotation = None
      Moving = false
      IsClocked = false
      STransform = { Rotation = Degree0; flipped = false }
      ReversedInputPorts = None
      PortMaps = { Order = Map.empty; Orientation = Map.empty }
      HScale = None
      VScale = None
      MovingPort = None
      MovingPortTarget = None }
