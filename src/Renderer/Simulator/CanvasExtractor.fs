module CanvasExtractor

open CommonTypes
open ParameterTypes
open Fable.Core

let sortQBy (byFun: 'a -> 'b) (ids: 'a list) =
    let mutable isSorted = true

    for i = 0 to ids.Length - 2 do
        if byFun (ids[i]) > byFun (ids[i + 1]) then
            isSorted <- false

    if isSorted then ids else List.sortBy byFun ids

/// Formats a ConnectionId for logging purposes
let logConnId (ConnectionId id) = string id

/// Transform the CanvasState into an f# data structure, with layout data removed (for checking electrically significant changes).
/// Components and connections are sorted to make them order-invariant - selecting components alters order.
let extractReducedState (state: CanvasState) : ReducedCanvasState =
    let (components: Component list), (connections: Connection list) = state

    let comps =
        components
        |> List.map (fun comp ->
            if
                comp.SymbolInfo = None
                && comp.H = 0.
                && comp.W = 0.
                && comp.X = 0.
                && comp.Y = 0.
            then
                comp
            else
                { comp with H = 0.; W = 0.; X = 0.; Y = 0.; SymbolInfo = None })
        |> sortQBy (fun comp -> comp.Id)

    let conns =
        connections
        |> List.map (fun conn ->
            if conn.Vertices = [] then
                conn
            else
                { conn with Vertices = [] })
        |> sortQBy (fun conn -> conn.Id)

    ReducedCanvasState(comps, conns)

let inline connsAreEqual (conn1: Connection) (conn2: Connection) =
    let portsEqual (p1: Port) (p2: Port) =
        p1.Id = p2.Id
        && p1.HostId = p2.HostId
        && p1.PortNumber = p2.PortNumber
        && p1.PortType = p2.PortType

    portsEqual conn1.Source conn2.Source
    && portsEqual conn1.Target conn2.Target

// The label is part of the circuit, not of its presentation: wire labels are joined to each other
// by name (FastCreate.reLinkIOLabels keys on the label), so renaming one moves a connection from
// one net to another. Comparing it is also what stops a rename being invisible to the simulation
// cache, which hands back the simulation built from the old names. It comes before the type
// because it is a string compare and a type can carry a whole memory's contents.
let inline compsAreEqual (comp1: Component) (comp2: Component) =
    comp1.Id = comp2.Id
    && comp1.InputPorts = comp2.InputPorts
    && comp1.OutputPorts = comp2.OutputPorts
    && comp1.Label = comp2.Label
    && comp1.Type = comp2.Type

let inline compsAreEqualExInputDefault (comp1: Component) (comp2: Component) =
    match comp1.Type, comp2.Type with
    | Input1 _, Input1 _ ->
        comp1.Id = comp2.Id
        && comp1.InputPorts = comp2.InputPorts
        && comp1.OutputPorts = comp2.OutputPorts
        && comp1.Label = comp2.Label
    | _ ->
        comp1.Id = comp2.Id
        && comp1.InputPorts = comp2.InputPorts
        && comp1.OutputPorts = comp2.OutputPorts
        && comp1.Label = comp2.Label
        && comp1.Type = comp2.Type

/// Is circuit (not geometry) the same for two CanvasStates? fast comparison
let stateIsEqual (cs1: CanvasState) (cs2: CanvasState) =
    let comps1, conns1 = cs1
    let comps2, conns2 = cs2

    comps1.Length = comps2.Length
    && conns1.Length = conns2.Length
    && List.forall2 compsAreEqual comps1 comps2
    && List.forall2 connsAreEqual conns1 conns2

let stateIsEqualExInputDefault (cs1: CanvasState) (cs2: CanvasState) =
    let comps1, conns1 = cs1
    let comps2, conns2 = cs2

    comps1.Length = comps2.Length
    && conns1.Length = conns2.Length
    && List.forall2 compsAreEqualExInputDefault comps1 comps2
    && List.forall2 connsAreEqual conns1 conns2

/// Are two lists of vertices are very similar.
///
/// The lengths are compared FIRST, and not after the fold, because two wires of different lengths
/// are not similar however their common prefix compares - and folding them together first was how
/// this came to raise under .NET, where `List.iter2` is defined only for equal lengths, while
/// returning the right answer in the app, where Fable's stopped at the shorter list. `ListPairs`
/// has since settled that difference in favour of stopping, so the order here is about the answer
/// rather than about raising. Two versions of one connection differ in vertex count whenever the
/// wire has been rerouted, which is the ordinary case for a sheet being edited.
let verticesAreSame (fixedOffset:XYPos) tolerance (conns1: (float * float * bool) list) (conns2: (float * float * bool) list) =
    let diff m1 m2 = if m1 <> m2 then tolerance else 0.
    let sq x = x * x

    conns1.Length = conns2.Length
    && (let mutable errSum = 0.

        (conns1, conns2)
        ||> List.iter2 (fun (x1, y1, m1) (x2, y2, m2) ->
            errSum <- errSum + sq (x1 - x2 - fixedOffset.X) + sq (y1 - y2 - fixedOffset.Y) + diff m1 m2)

        errSum < tolerance)

/// evil mutable for debugging only
/// allows easy access to specific connections - so it can be highlighted in GUI
let mutable debugChangedConnections: ConnectionId list = []

/// Are two lists of connections identical
let compareConns tolerance conns1 conns2 =
    let connIdA (conns: Connection List) = conns |> sortQBy (fun conn -> conn.Id)
    let connsA1 = connIdA conns1
    let connsA2 = connIdA conns2
    /// if whole ckt has been translated this will be the offset.
    /// This offset for all vertices => connections still the same.
    ///
    /// A connection with no vertices has no position to measure that offset from, and there is no
    /// guarantee of one: extractReducedState strips vertices, and a .dgm need not have come from
    /// this version of Issie. No offset is the right answer there, as it is for a canvas with no
    /// connections at all - and it is a better one than indexing off the end of an empty list.
    let connFixedOffset =
        let xy (x,y,_) = {X=x;Y=y}
        match connsA1,connsA2 with
        | c1::_, c2::_ when not (List.isEmpty c1.Vertices || List.isEmpty c2.Vertices) ->
            xy c1.Vertices[0] - xy c2.Vertices[0]
        | _ ->
            {X=0.; Y=0.}
    match connsA1, connsA2 with
    | a,b when a.Length <> b.Length ->
        false
    | a,b when not <| List.forall2 (fun c1 c2 -> verticesAreSame connFixedOffset tolerance c1.Vertices c2.Vertices) a b ->
        List.zip a b
        |> List.filter (fun (c1,c2) -> not <| verticesAreSame connFixedOffset tolerance c1.Vertices c2.Vertices)
        |> List.map fst
        |> List.map (fun (badConn: Connection) -> badConn.Id)
        |> (fun lst ->
            debugChangedConnections <- lst)
        false
    | _ -> true
        
/// Are two lists of components identical
let compareComps tolerance comps1 comps2 =
    let byId (comps: Component List) = comps |> sortQBy (fun comp -> comp.Id)

    match byId comps1, byId comps2 with
    |  [], [] -> true
    | l1, l2 when l1.Length <> l2.Length -> false
    | (c1 :: _) as c1L, ((c2 :: _) as c2L) ->
        // if whole ckt has been translated this will be the offset.
        let dx = c1.X - c2.X
        let dy = c1.Y - c2.Y
        let isClose c1 c2 = (c1.X - c2.X - dx)**2 +  (c1.Y - c2.Y - dy)**2 < tolerance
        dx**2 + dy**2 < tolerance &&
        List.forall2 (fun (c1: Component) (c2: Component) -> isClose c1 c2 && (c1.isSame c2)) c1L c2L
    | _ -> false // NB this cannot happen


/// Robust comparison of two schematics. Tolerance determines how similar
/// counts as equal.
/// cannot use equality because float vertices may not be identical
/// use to detemine whether schematic needs to be saved
/// NB for electrical circuit comparison use extractReducedState.
let compareCanvas (tolerance: float) ((comps1, conns1): CanvasState) ((comps2, conns2): CanvasState) =
    let compsOk = compareComps tolerance comps1 comps2
    let connsOk = compareConns tolerance conns1 conns2
    let comparesEqual = compsOk && connsOk
    //if not comparesEqual then
    comparesEqual    

/// Compare the name and IOs of two sheets as loadedcomponents
/// For backups, if these change something major has happened
let compareIOs (ldc1: LoadedComponent) (ldc2: LoadedComponent) =
    Set(ldc1.InputLabels) = Set(ldc2.InputLabels)
    && ldc1.Name = ldc2.Name

(*
    The reference check below is what makes comparing a whole design cheap.

    Only the open sheet is rebuilt as the user edits: getUpdatedLoadedComponentState and
    addStateToLoadedComponents both mint a new LoadedComponent for it and pass every other sheet
    through as the object it already was. So in any comparison of a design against a stored copy of
    itself, all but one sheet are reference-identical, and without this they were still walked
    component by component - which is why comparing a stored simulation against the canvas cost the
    whole design rather than the one sheet that can have changed.

    It is a fast path and not a change of meaning: these compare four fields, and one object has
    the same four as itself. Nothing float-valued is among them - compsAreEqual excludes geometry
    and connsAreEqual compares only ports - so there is no value here that is unequal to itself.
*)

/// Is circuit (not geometry) the same for two LoadedComponents? They must also have the same name
let loadedComponentIsEqual (ldc1: LoadedComponent) (ldc2: LoadedComponent) =
    System.Object.ReferenceEquals(ldc1, ldc2)
    || (ldc1.InputLabels = ldc2.InputLabels
        && ldc1.OutputLabels = ldc2.OutputLabels
        && stateIsEqual ldc1.CanvasState ldc2.CanvasState
        && ldc1.Name = ldc2.Name)

/// Is circuit (not geometry) the same for two LoadedComponents? They must also have the same name
let loadedComponentIsEqualExInputDefault (ldc1: LoadedComponent) (ldc2: LoadedComponent) =
    System.Object.ReferenceEquals(ldc1, ldc2)
    || (ldc1.InputLabels = ldc2.InputLabels
        && ldc1.OutputLabels = ldc2.OutputLabels
        && stateIsEqualExInputDefault ldc1.CanvasState ldc2.CanvasState
        && ldc1.Name = ldc2.Name)

/// get sheet I/O labels in correct order based on position of components
let getOrderedCompLabels compType ((comps, _): CanvasState) =
    let result = 
        comps
        |> List.collect (fun comp ->
            let sortKey = comp.Y, comp.X

            match comp.Type, compType with
            | Input1(n, defaultVal), Input1 _ -> 
                [ sortKey, (comp.Label, n) ]
            | Output n, Output _ -> 
                [ sortKey, (comp.Label, n) ]
            | _ -> [])
        |> List.sortBy fst
        |> List.map snd
    
    result

/// Extract the labels and bus widths of the inputs and outputs nodes as a signature.
/// Form is inputs,outputs
let parseDiagramSignature canvasState : (string * int) list * (string * int) list =
    let inputs = getOrderedCompLabels (Input1(0, None)) canvasState
    let outputs = getOrderedCompLabels (Output 0) canvasState
    inputs, outputs

/// extract the fields compared to check circuit equality
let extractLoadedSimulatorComponent (canvas: CanvasState) (name: string) =
    let inputs, outputs = parseDiagramSignature canvas
    let ldc =
        { Name = name
          TimeStamp = System.DateTime.Now
          WaveInfo = None
          FilePath = ""
          CanvasState = canvas
          InputLabels = inputs
          OutputLabels = outputs
          Form = None
          Description = None
          LCParameterSlots = None // Parameter slots will be set by the sheet's parameter system
          LoadedComponentIsOutOfDate = false
          IsTopSheet = false
          }

    ldc

/// Put a sheet's parameter slots in order against its canvas, ready to be saved:
///
///   - drop slots naming components that are no longer on the sheet. Every parameter slot must
///     name a live component: a slot outliving its component cannot be displayed or edited, and
///     pushing a value into it would throw;
///   - re-point the label an `IO` slot carries at its component's current label. Nothing rewrites
///     that label when the component is renamed, and while no reader depends on it (see
///     ParameterTypes.sameSlotName) it is what the properties pane and the slot table print, so a
///     stale one describes the sheet wrongly.
let tidyParamSlots ((comps, _): CanvasState) (paramDefs: ParameterTypes.ParameterDefs option) =
    match paramDefs with
    | None -> None
    | Some defs ->
        let labelOf =
            comps
            |> List.map (fun comp -> comp.Id, comp.Label)
            |> Map.ofList
        defs.ParamSlots
        |> Map.toList
        |> List.choose (fun (slot, exprSpec) ->
            match Map.tryFind slot.CompId labelOf, slot.CompSlot with
            | None, _ -> None
            | Some label, IO _ -> Some ({slot with CompSlot = IO label}, exprSpec)
            | Some _, _ -> Some (slot, exprSpec))
        |> Map.ofList
        |> fun slots -> Some {defs with ParamSlots = slots}

//------------------------------------------------------------------------------------------------//
//------------------------- The signature of one custom component instance -----------------------//
//------------------------------------------------------------------------------------------------//

(*
    A sheet that declares parameters does not have one signature: it has a family of them, one per
    set of bindings. The port widths of a custom component instance are therefore a fact about the
    INSTANCE, worked out from the sheet inside it resolved at the bindings that instance gives.

    Everything that needs to know an instance's ports goes through signatureOfInstance: placing one
    (CatalogueView), redrawing one when a binding changes (ParameterView), checking one before
    simulation (CanvasStateAnalyser) and bringing instances back into step when the sheet changes
    (CustomCompPorts). Three copies of this calculation existed before, and they disagreed.
*)

/// A sheet's ports: input labels and widths, then output labels and widths, each in the order the
/// components appear on the sheet.
type Signature = (string * int) list * (string * int) list

/// The bindings a sheet is resolved with inside one instance of it.
///
/// An instance binding is an expression in the parameters of the sheet the instance SITS ON, so it
/// is evaluated there first and reaches the child sheet as a plain value; every parameter the
/// instance does not bind takes the child's own declared default. This is the same merge
/// GraphMerger.effectiveBindings makes for elaboration, so what is drawn and what is simulated
/// agree. A binding that cannot be evaluated falls back to the default rather than to nothing.
let effectiveInstanceBindings
        (childDefaults: ParamBindings)
        (parentBindings: ParamBindings)
        (instanceBindings: ParamBindings)
        : ParamBindings =
    childDefaults
    |> Map.map (fun name defExpr ->
        Map.tryFind name instanceBindings
        |> Option.bind (fun expr ->
            evaluateParamExpression parentBindings expr
            |> Result.toOption
            |> Option.map PInt)
        |> Option.defaultValue defExpr)

/// Whether the widths in an instance's signature can be believed: every value the instance binds
/// is one this caller can work out, and every parameterised slot of the child sheet then
/// evaluates. False where a binding is an expression the caller has no environment for - a
/// canvas checked on its own knows nothing of the sheet that contains it - so the widths that come
/// back are the child sheet's own and only the port NAMES mean anything.
let private signatureIsExact
        (childDefaults: ParamBindings)
        (parentBindings: ParamBindings)
        (instanceBindings: ParamBindings)
        (effective: ParamBindings)
        (slots: ComponentSlotExpr)
        : bool =
    let bindingsUsable =
        childDefaults
        |> Map.forall (fun name _ ->
            // a parameter the instance leaves alone takes the child's default, which is known
            match Map.tryFind name instanceBindings with
            | None -> true
            | Some expr -> evaluateParamExpression parentBindings expr |> Result.isOk)
    bindingsUsable
    && (slots |> Map.forall (fun _ exprSpec ->
            evaluateParamExpression effective exprSpec.Expression |> Result.isOk))

/// The signature an instance of `childSheet` has when it binds its parameters as given, and
/// whether its widths are exact. The bindings are read as expressions in `parentBindings` - the
/// parameters of the sheet the instance sits on.
/// Not exact (see signatureIsExact) means some width could not be worked out here, and only the
/// port NAMES may be relied on; the widths are then those the child sheet was saved with.
/// None when the child sheet is not in the project.
let signatureOfInstanceWithCertainty
        (ldcs: LoadedComponent list)
        (parentBindings: ParamBindings)
        (childSheet: string)
        (instanceBindings: ParamBindings)
        : (Signature * bool) option =
    ldcs
    |> List.tryFind (fun ldc -> ldc.Name = childSheet)
    |> Option.map (fun childLdc ->
        let defs =
            childLdc.LCParameterSlots
            |> Option.defaultValue {DefaultBindings = Map.empty; ParamSlots = Map.empty}
        let childDefaults = bindingsOf defs.DefaultBindings
        let effective = effectiveInstanceBindings childDefaults parentBindings instanceBindings
        let resolved = ComponentSlots.resolveCanvasAtBindings effective defs.ParamSlots childLdc.CanvasState
        (getOrderedCompLabels (Input1 (0, None)) resolved,
         getOrderedCompLabels (Output 0) resolved),
        signatureIsExact childDefaults parentBindings instanceBindings effective defs.ParamSlots)

/// The signature an instance of `childSheet` has when it binds its parameters as given.
/// None when the child sheet is not in the project.
let signatureOfInstance
        (ldcs: LoadedComponent list)
        (parentBindings: ParamBindings)
        (childSheet: string)
        (instanceBindings: ParamBindings)
        : Signature option =
    signatureOfInstanceWithCertainty ldcs parentBindings childSheet instanceBindings
    |> Option.map fst

/// A custom component instance holding the given port widths, matched to its ports by LABEL.
///
/// Widths only. The ORDER of an instance's ports, and which ports it has, are deliberately left
/// alone: an instance whose ports are in a different order from its sheet's is a correct instance
/// (see CustomCompPorts.instanceIsOutOfDate), and a port added to or removed from the sheet is a
/// change the user is asked to confirm rather than one to make behind them. A label the signature
/// says nothing about therefore keeps the width it had.
let withPortWidths (labelToWidth: Map<string, int>) (cc: CustomComponentType) : CustomComponentType =
    let resize labels =
        labels
        |> List.map (fun (label, width) -> label, Map.tryFind label labelToWidth |> Option.defaultValue width)
    {cc with InputLabels = resize cc.InputLabels; OutputLabels = resize cc.OutputLabels}

/// Every custom component instance in the project sized at its OWN bindings.
///
/// Resolving a sheet's parameterised slots does not do this. ComponentSlots.setSlotValue writes a
/// CustomCompParam slot into the instance's ParameterBindings, which is all it can reach: the port
/// widths follow from that binding by way of the CHILD sheet, and only signatureOfInstance knows
/// how. So a sheet whose parameter values changed came out of ParameterAnalysis.propagateParameterValues
/// holding instances whose bindings said one width and whose ports still said another.
///
/// The open sheet never showed it, because ParameterView.updateComponents pushes its slots through
/// ChangeCustom, which recomputes ports on the way. A CLOSED sheet was written to its file as it
/// stood, and opening it raised the instance-out-of-date error that the whole per-instance
/// signature apparatus exists to avoid.
///
/// Pure, and idempotent: it asks what each instance's ports should be and writes that, so running
/// it again finds nothing to change. A sheet whose canvas it rewrites is flagged as needing saving.
let syncInstancePorts (ldcs: LoadedComponent list) : LoadedComponent list =
    let parametersOf (ldc: LoadedComponent) =
        ldc.LCParameterSlots
        |> Option.defaultValue {DefaultBindings = Map.empty; ParamSlots = Map.empty}
    ldcs
    |> List.map (fun parentLdc ->
        let defs = parametersOf parentLdc
        let parentBindings = bindingsOf defs.DefaultBindings
        let comps, conns = parentLdc.CanvasState
        let resize (comp: Component) =
            match comp.Type with
            | Custom cc ->
                // the instance's bindings as elaboration reads them: a CustomCompParam slot of
                // this sheet overrides what is stored on the instance
                ParameterAnalysis.instanceBindingExprs defs.ParamSlots comp cc
                |> signatureOfInstance ldcs parentBindings cc.Name
                |> Option.map (fun (ins, outs) ->
                    {comp with Type = Custom (withPortWidths (ins @ outs |> Map.ofList) cc)})
                |> Option.defaultValue comp
            | _ -> comp
        match List.map resize comps with
        | resized when resized = comps -> parentLdc
        | resized -> {parentLdc with CanvasState = resized, conns; LoadedComponentIsOutOfDate = true})

/// Returns true if project exists and ldc is electrically identical to same sheet in project
/// canvasState must be the project currently open state.
let loadedComponentIsSameAsProject (canvasState: CanvasState) (ldc: LoadedComponent) (p: Project option) =
    let ldcIsEq ldc1 ldc2 =
        ldc1.InputLabels = ldc2.InputLabels
        && ldc1.OutputLabels = ldc2.OutputLabels
        && stateIsEqual ldc1.CanvasState ldc2.CanvasState

    match ldc.Name, p with
    | "", _
    | _, None -> false
    | name, Some p when name = p.OpenFileName ->
        let ins, outs = parseDiagramSignature canvasState
        let sort = List.sort

        stateIsEqual canvasState ldc.CanvasState
        && sort ins = sort ldc.InputLabels
        && sort outs = sort ldc.OutputLabels
    | name, Some p ->
        List.tryFind (fun ldc -> ldc.Name = name) p.LoadedComponents
        |> Option.map (fun ldc' -> ldcIsEq ldc' ldc)
        |> Option.defaultValue false

/// add given name,state to loadedcomponent list as a loaded component (overwriting existing if needed)
let addStateToLoadedComponents openFileName canvasState loadedComponents =
    let ins, outs = parseDiagramSignature canvasState
    let existingLdc = loadedComponents |> List.tryFind (fun ldc -> ldc.Name = openFileName)

    let ldc: LoadedComponent =
        { Name = openFileName
          LoadedComponentIsOutOfDate = false
          InputLabels = ins
          OutputLabels = outs
          CanvasState = canvasState
          Form = None
          Description = None
          WaveInfo = None
          FilePath = ""
          TimeStamp = System.DateTime.Now
          LCParameterSlots = existingLdc |> Option.map (fun e -> e.LCParameterSlots) |> Option.flatten
          IsTopSheet = existingLdc |> Option.map (fun e -> e.IsTopSheet) |> Option.defaultValue false
         }

    loadedComponents
    |> List.filter (fun ldc -> ldc.Name <> openFileName)
    |> (fun ldcs -> ldc :: ldcs)

/// the inverse of addStateToLoadedConponents
/// The loadedComponent list in the result does NOT include diagramName
///
/// A sheet that is not there is an Error rather than an exception because one of the callers runs
/// while rendering: the waveform viewer's buttons ask whether the sheet being simulated still
/// builds, and the sheet named by Model.WaveSimSheet can belong to a project that has since been
/// closed. Raising there threw out of the React render, which unmounted the UI and left every
/// later render throwing the same way - nothing short of reloading the app recovered.
let getStateAndDependencies
    (diagramName: string)
    (ldcs: LoadedComponent list)
    : Result<string * CanvasState * LoadedComponent list, string> =
    ldcs
    |> List.tryFind (fun ldc -> ldc.Name = diagramName)
    |> function
        | Some ldc ->
            Ok (diagramName, ldc.CanvasState, List.filter (fun ldc -> ldc.Name <> diagramName) ldcs)
        | None ->
            Error $"Error - can't find {diagramName} in dependencies"

// ---------------------------------------------------------------------------------------------
// Conversion to the Simple wire types (CommonTypes.SimpleDesign), the minimal electrical form a
// design crosses to the dotnet sidecar in. Now that canvas ids ARE integers this is a straight
// projection: geometry dropped, endpoints resolved to (component, port number).
// ---------------------------------------------------------------------------------------------

let simpleSheetOfLoadedComponent (ldc: LoadedComponent) : SimpleSheet =
    let comps, conns = ldc.CanvasState

    // Connection endpoints usually carry PortNumber = None; the components' own port lists
    // always carry Some, so endpoints resolve through a lookup built from those.
    let portInfo =
        comps
        |> List.collect (fun comp ->
            let entry (port: Port) =
                match port.PortNumber with
                | Some n -> port.Id, (cToInt comp.Id, n)
                | None ->
                    failwithf "simpleSheet: port %d on component %d of %s has no port number"
                        (pToInt port.Id) (cToInt comp.Id) ldc.Name

            List.map entry comp.InputPorts @ List.map entry comp.OutputPorts)
        |> Map.ofList

    let endpoint (which: string) (port: Port) =
        match Map.tryFind port.Id portInfo with
        | Some found -> found
        | None ->
            failwithf "simpleSheet: the %s of a connection on %s names port %d, which no component has"
                which ldc.Name (pToInt port.Id)

    let simpleComp (comp: Component) : SimpleComponent =
        { CompId = cToInt comp.Id
          TypeS = comp.Type
          Label = comp.Label }

    let simpleConn (conn: Connection) : SimpleConnection =
        let srcComp, srcPort = endpoint "source" conn.Source
        let destComp, destPort = endpoint "target" conn.Target

        let (ConnectionId connId) = conn.Id

        { ConnId = connId
          SrcComp = srcComp
          SrcPort = srcPort
          DestComp = destComp
          DestPort = destPort }

    let defaultBindings, paramSlots =
        match ldc.LCParameterSlots with
        | Some defs -> defs.DefaultBindings, defs.ParamSlots
        | None -> Map.empty, Map.empty

    { SheetName = ldc.Name
      Components = List.map simpleComp comps
      Connections = List.map simpleConn conns
      DefaultBindings = defaultBindings
      ParamSlots = paramSlots }

/// The whole design. TopSheet is the flagged top sheet when there is one; otherwise a root of
/// the instantiation forest (a sheet no other sheet instantiates), otherwise the first sheet -
/// the fallback order parameter propagation also uses.
let simpleDesignOfLoadedComponents (ldcs: LoadedComponent list) : SimpleDesign =
    let instantiated =
        ldcs
        |> List.collect (fun ldc ->
            fst ldc.CanvasState
            |> List.choose (fun comp ->
                match comp.Type with
                | Custom custom -> Some custom.Name
                | _ -> None))
        |> Set.ofList

    let top =
        ldcs
        |> List.tryFind (fun ldc -> ldc.IsTopSheet)
        |> Option.orElseWith (fun () -> ldcs |> List.tryFind (fun ldc -> not (Set.contains ldc.Name instantiated)))
        |> Option.orElseWith (fun () -> List.tryHead ldcs)
        |> Option.map (fun ldc -> ldc.Name)
        |> Option.defaultValue ""

    { TopSheet = top
      Sheets = List.map simpleSheetOfLoadedComponent ldcs }
