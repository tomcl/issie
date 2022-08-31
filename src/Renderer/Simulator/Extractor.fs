module Extractor
open CommonTypes
open Fable.Core

/// access javascript reference equality
[<Emit("($1 === $0)")>]
let referenceEquality (a1: obj) (a2:obj): bool = jsNative




let sortQBy (byFun: 'a -> 'b) (ids: 'a list) =
    let mutable isSorted = true
    for i = 0 to ids.Length - 2 do
        if byFun(ids[i]) > byFun(ids[i+1]) then isSorted <- false
    if isSorted then ids else List.sort ids


/// Transform the CanvasState into an f# data structure, with layout data removed (for checking electrically significant changes).
/// Components and connections are sorted to make them order-invariant - selecting components alters order.
let extractReducedState (state : CanvasState) : ReducedCanvasState =           
    let (components : Component list), (connections : Connection list) = state
    let comps = 
        components
        |> List.map (fun comp -> 
            if comp.SymbolInfo = None && comp.H=0. && comp.W=0. && comp.X=0. && comp.Y=0. then comp
            else {comp with H=0.;W=0.;X=0.;Y=0.; SymbolInfo = None})
        |> sortQBy (fun comp -> comp.Id)
                       
    let conns =                   
        connections
        |> List.map (fun conn -> if conn.Vertices = [] then conn else {conn with Vertices = []})
        |> sortQBy (fun conn -> conn.Id)
    ReducedCanvasState(comps, conns)

let inline connsAreEqual (conn1:Connection) (conn2:Connection) =
    let portsEqual (p1: Port) (p2:Port) =
        p1.Id = p2.Id &&
        p1.HostId = p2.HostId &&
        p1.PortNumber = p2.PortNumber &&
        p1.PortType = p2.PortType
    portsEqual conn1.Source conn2.Source && portsEqual conn1.Target conn2.Target

let inline compsAreEqual (comp1:Component) (comp2:Component) =
    comp1.Id = comp2.Id &&
    comp1.InputPorts = comp2.InputPorts &&
    comp1.OutputPorts = comp2.OutputPorts &&
    comp1.Type = comp2.Type

/// Is circuit (not geometry) the same for two CanvasStates? fast comparison
let stateIsEqual (cs1: CanvasState) (cs2:CanvasState) =
    let comps1,conns1 = cs1
    let comps2,conns2 = cs2
    comps1.Length = comps2.Length && 
    conns1.Length = conns2.Length &&
    List.forall2  compsAreEqual comps1 comps2 &&
    List.forall2 connsAreEqual conns1 conns2


/// Are two lists of vertices are very similar.
let verticesAreSame tolerance (conns1:(float*float*bool) list) (conns2: (float*float*bool) list) =
    let diff m1 m2 = if m1 <> m2 then tolerance else 0.
    let sq x = x*x
    conns1.Length = conns2.Length &&
    let mutable errSum = 0.
    (conns1,conns2)
    ||> List.iter2 (fun (x1,y1,m1) (x2,y2,m2) -> errSum <- errSum + sq(x1-x2) + sq(y1-y2) + diff m1 m2)
    errSum < tolerance
   

/// Are two lists of connections identical
let compareConns tolerance conns1 conns2 =
    let connIdA (conns:Connection List) =
        conns
        |> sortQBy (fun conn -> conn.Id)      
    let connsA1 = connIdA conns1
    let connsA2 = connIdA conns2
    connsA1.Length = connsA2.Length &&
        List.forall2 (fun c1 c2 ->
            verticesAreSame tolerance c1.Vertices c2.Vertices)  connsA1 connsA2

/// Are two lists of components identical
let compareComps tolerance comps1 comps2 =
    let isClose a b = float ((a-b)*(a-b)) < tolerance
    let compIdA (comps:Component List) =
        comps
        |> sortQBy (fun comp -> comp.Id)      
    let compsA1 = compIdA comps1
    let compsA2 = compIdA comps2
    compsA1.Length = compsA2.Length &&
        List.forall2 (fun (c1: Component) (c2:Component) -> isClose c1.X c2.X && isClose c1.Y c2.Y)  compsA1 compsA2

/// Robust comparison of two schematics. Tolerance determines how similar
/// counts as equal.
/// cannot use equality because float vertices may not be identical
/// use to detemine whether schematic needs to be saved
/// NB for electrical circuit comparison use extractReducedState.
let compareCanvas 
        (tolerance: float)
        ((comps1,conns1):CanvasState) 
        ((comps2,conns2):CanvasState) =
    let reduce (comps: Component list) =
        comps
        |> sortQBy (fun comp -> comp.Id)
    let compsOk = reduce comps1 = reduce comps2
    let compsSamePos = compareComps tolerance comps1 comps2
    let connsOk = compareConns tolerance conns1 conns2
    compsOk && compsSamePos && connsOk

/// Compare the name and IOs of two sheets as loadedcomponents
/// For backups, if these chnage something major has happened
let compareIOs (ldc1:LoadedComponent)  (ldc2:LoadedComponent) =
    Set(ldc1.InputLabels) = Set(ldc2.InputLabels) && ldc1.Name = ldc2.Name

/// Is circuit (not geometry) the same for two LoadedComponents? They must also have the same name
let loadedComponentIsEqual (ldc1: LoadedComponent) (ldc2: LoadedComponent) =
    ldc1.InputLabels = ldc2.InputLabels &&
    ldc1.OutputLabels = ldc2.OutputLabels &&
    stateIsEqual ldc1.CanvasState ldc2.CanvasState &&
    ldc1.Name = ldc2.Name


/// get sheet I/O labels in correct order based on position of components
let getOrderedCompLabels compType ((comps,_): CanvasState) =
    comps
    |> List.collect (fun comp -> 
        let sortKey = comp.Y,comp.X
        match comp.Type, compType with 
        | Input1 (n, defaultVal), Input1 _ -> [sortKey,(comp.Label, n)]
        | Output n, Output _ -> [sortKey, (comp.Label,n)] 
        | _ -> [])
    |> List.sortBy fst
    |> List.map snd
   

/// Extract the labels and bus widths of the inputs and outputs nodes as a signature.
/// Form is inputs,outputs
let parseDiagramSignature canvasState
        : (string * int) list * (string * int) list =
    let inputs = getOrderedCompLabels (Input1 (0, None)) canvasState
    let outputs = getOrderedCompLabels (Output 0) canvasState
    inputs, outputs

/// extract the fields compared to check circuit equality
let extractLoadedSimulatorComponent (canvas: CanvasState)(name: string)=
        let inputs, outputs = parseDiagramSignature canvas
        //printfn "parsed component"
        let ldc =
            {
                Name = name
                TimeStamp = System.DateTime.Now
                WaveInfo = None
                FilePath = ""
                CanvasState = canvas
                InputLabels = inputs
                OutputLabels = outputs
                Form = None
                Description = None
            }
        ldc

/// Returns true if project exists and ldc is electrically identical to same sheet in project
/// canvasState must be the project currently open state.
let loadedComponentIsSameAsProject (canvasState: CanvasState) (ldc: LoadedComponent) (p: Project option) =
    let ldcIsEq ldc1 ldc2 =
        ldc1.InputLabels = ldc2.InputLabels &&
        ldc1.OutputLabels = ldc2.OutputLabels &&
        stateIsEqual ldc1.CanvasState ldc2.CanvasState 

    match ldc.Name, p with
    | "", _ 
    | _, None -> false
    | name, Some p when name = p.OpenFileName ->
        let ins, outs = parseDiagramSignature canvasState
        let sort = List.sort
        stateIsEqual canvasState ldc.CanvasState && sort ins = sort ldc.InputLabels && sort outs = sort ldc.OutputLabels
    | name, Some p ->
        List.tryFind (fun ldc -> ldc.Name = name) p.LoadedComponents
        |> Option.map (fun ldc' -> ldcIsEq ldc' ldc)
        |> Option.defaultValue false
    | _ -> false

/// add given name,state to loadedcomponent lits as a loaded component (overwriting existing if needed)
let addStateToLoadedComponents openFileName canvasState loadedComponents =
    let ins, outs = parseDiagramSignature canvasState
    let ldc: LoadedComponent = 
        {
            Name = openFileName
            InputLabels = ins
            OutputLabels = outs
            CanvasState = canvasState
            Form = None
            Description = None
            WaveInfo = None
            FilePath = ""
            TimeStamp = System.DateTime.Now
        }
    loadedComponents
    |> List.filter (fun ldc -> ldc.Name <> openFileName) 
    |> (fun ldcs ->  ldc :: ldcs)

/// the inverse of addStateToLoadedConponents
/// The loadedComponent list does NOT include diagramName
let getStateAndDependencies (diagramName:string)  (ldcs:LoadedComponent list) =
    ldcs
    |> List.tryFind (fun ldc -> ldc.Name = diagramName)
    |> Option.map (fun ldc ->ldc.CanvasState)
    |> Option.map (fun cs -> diagramName, cs, List.filter (fun ldc -> ldc.Name <> diagramName) ldcs)
    |> Option.defaultWith (fun () -> failwithf $"Error - can't find {diagramName} in dependencies")



