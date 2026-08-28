/// SimpleDesign back to skeleton LoadedComponents that the existing simulation creation
/// accepts - the bridge that lets the dotnet sidecar run today's simulator on a design that
/// arrived over the wire, as a BASELINE against which rewrites (and the Electron simulator
/// itself) can be checked.
///
/// Deliberately not part of the wire contract: the Simple types stay minimal, and a future
/// .NET simulator consumes them directly with its own structures. This shim reconstructs only
/// what the current pipeline demands - components with port lists (ports synthesized from the
/// component type's arity; port ids never crossed the wire), connections resolved back from
/// (component, port number) endpoints, and sheet IO signatures recomputed by the same function
/// the loader uses. Geometry stays zeroed: the simulator never reads it.
module SimpleDesignShim

open CommonTypes

/// Port counts for a component type: custom components from their own label lists, everything
/// else from the same table symbol creation uses.
let portCounts (typeS: ComponentType) (label: string) =
    match typeS with
    | Custom custom -> custom.InputLabels.Length, custom.OutputLabels.Length
    | t ->
        let nIn, nOut, _, _ = Symbol.getComponentProperties t label
        nIn, nOut

let private makeComp (compId: int) (nInputs: int) (nOutputs: int) (compType: ComponentType) (label: string) : Component =
    let id = ComponentId compId

    let makePorts n portType =
        let offset = match portType with | PortType.Input -> 0 | PortType.Output -> 500
        [ for i in 0 .. n - 1 ->
            { Id = PortId(compId * 1000 + offset + i)
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

let sheetToLoadedComponent (sheet: SimpleSheet) : LoadedComponent =
    let comps =
        sheet.Components
        |> List.map (fun sc ->
            let nIn, nOut = portCounts sc.TypeS sc.Label
            makeComp sc.CompId nIn nOut sc.TypeS sc.Label)

    let compById = comps |> List.map (fun comp -> comp.Id, comp) |> Map.ofList

    let conns =
        sheet.Connections
        |> List.map (fun sc ->
            let src = compById[ComponentId sc.SrcComp]
            let tgt = compById[ComponentId sc.DestComp]

            { Id = ConnectionId sc.ConnId
              Source = { src.OutputPorts[sc.SrcPort] with PortNumber = None }
              Target = { tgt.InputPorts[sc.DestPort] with PortNumber = None }
              Vertices = [] })

    let canvas = comps, conns
    let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas

    { Name = sheet.SheetName
      TimeStamp = System.DateTime.MinValue
      FilePath = ""
      WaveInfo = None
      CanvasState = canvas
      InputLabels = inputs
      OutputLabels = outputs
      LCParameterSlots =
        if Map.isEmpty sheet.DefaultBindings && Map.isEmpty sheet.ParamSlots then
            None
        else
            Some { DefaultBindings = sheet.DefaultBindings; ParamSlots = sheet.ParamSlots }
      Form = Some User
      LoadedComponentIsOutOfDate = false
      IsTopSheet = false
      Description = None }

/// The whole design as skeleton LoadedComponents, ready for Simulator.startCircuitSimulation.
let designToLoadedComponents (design: SimpleDesign) : LoadedComponent list =
    design.Sheets |> List.map sheetToLoadedComponent
