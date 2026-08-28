module ParameterAnalysis

(*
    ParameterAnalysis.fs

    Design-time analysis of sheet parameters across the instance tree under a top sheet.

    Simulation elaboration (GraphMerger.resolveSheet) resolves each sheet with the bindings its
    instance gives it. The functions here perform the same binding walk over LoadedComponents,
    without building simulation graphs, to answer a different question: what values does each
    parameter of each sheet take across all of its instances under a chosen top sheet?

    The answers drive display only. Elaboration semantics are unchanged: only explicit
    per-instance bindings exist. Checking here is best effort and must produce no false
    positives, so a binding that cannot be evaluated makes a value unknown rather than wrong.
*)

open ParameterTypes
open CommonTypes

/// One link in an instance path: a custom component instance of ChildSheet placed on ParentSheet.
type InstancePathLink = {
    ParentSheet: string
    /// Component.Id of the custom component instance
    InstanceId: ComponentId
    InstanceLabel: string
    ChildSheet: string
}

/// One instance of a sheet in the tree under the top sheet.
type SheetInstance = {
    /// False on the record of the top sheet itself, which analyseUnderTop also produces so that
    /// "which sheets are in this design" can be answered - the top is an instance of nothing, and
    /// reading it as one made a top sheet's parameters look bound to their own declared values.
    IsInstance: bool
    /// The value of each parameter the sheet declares, inside this instance.
    /// None marks a value that could not be evaluated - unknown, never reported as a conflict.
    ParamValues: Map<ParamName, ParamInt option>
    /// The parameters this instance actually SETS, of those the sheet declares. A parameter the
    /// instance leaves alone still has a value in ParamValues - the sheet's own stored one - so
    /// without this the two are indistinguishable, and a sheet nothing sets looks settled.
    BoundParams: Set<ParamName>
}

/// Every instance of every sheet in the tree under one top sheet, keyed by sheet name.
/// A sheet absent from the map is not instantiated under the top (the top itself is present,
/// as its own single instance with an empty path).
type SheetInstances = Map<string, SheetInstance list>

/// The parameters a sheet declares, with their defaults and descriptions.
let declaredParamDefs (ldc: LoadedComponent) : ParamDefinitions =
    ldc.LCParameterSlots
    |> Option.map (fun ps -> ps.DefaultBindings)
    |> Option.defaultValue Map.empty

/// The parameters a sheet declares as an evaluation environment: its default bindings.
let declaredParams (ldc: LoadedComponent) : ParamBindings =
    declaredParamDefs ldc |> bindingsOf

/// The parameterised slots of a sheet.
let sheetParamSlots (ldc: LoadedComponent) : ComponentSlotExpr =
    ldc.LCParameterSlots
    |> Option.map (fun ps -> ps.ParamSlots)
    |> Option.defaultValue Map.empty

/// The custom component instances a sheet contains.
let customInstances (ldc: LoadedComponent) : (Component * CustomComponentType) list =
    fst ldc.CanvasState
    |> List.choose (fun comp ->
        match comp.Type with
        | Custom cc -> Some (comp, cc)
        | _ -> None)

/// The sheets of a project that are not instantiated inside any other sheet: the roots of the
/// instance forest, each the top of its own design. Ordered as the loaded components are.
let instanceForestRoots (ldcs: LoadedComponent list) : string list =
    let instantiated =
        ldcs
        |> List.collect (customInstances >> List.map (fun (_, cc) -> cc.Name))
        |> Set.ofList
    ldcs
    |> List.map (fun ldc -> ldc.Name)
    |> List.filter (fun name -> not (Set.contains name instantiated))

/// The sheet the user has flagged as the top, if any. Only for showing the flag; to find the
/// design a sheet belongs to use effectiveTopSheetFor, which is total.
let flaggedTopSheet (ldcs: LoadedComponent list) : string option =
    ldcs |> List.tryFind (fun ldc -> ldc.IsTopSheet) |> Option.map (fun ldc -> ldc.Name)

/// Bindings usable for evaluation, from values already computed: unknown values are left out,
/// so an expression referring to one fails to evaluate and stays unknown.
let private knownBindings (values: Map<ParamName, ParamInt option>) : ParamBindings =
    (Map.empty, values)
    ||> Map.fold (fun acc name value ->
        match value with
        | Some v -> Map.add name (PInt v) acc
        | None -> acc)

/// The value of every parameter a sheet declares when its defaults are evaluated in their own
/// environment - the values the sheet displays and checks with when nothing binds it.
let evaluatedDefaults (defaults: ParamBindings) : Map<ParamName, ParamInt option> =
    defaults
    |> Map.map (fun _ expr ->
        match evaluateParamExpression defaults expr with
        | Ok v -> Some v
        | Error _ -> None)

/// The expressions one custom component instance binds its sheet's parameters to, in the
/// parameters of the sheet the instance sits on. A CustomCompParam slot of the parent sheet
/// overrides the binding stored on the instance, exactly as in simulation elaboration.
let instanceBindingExprs (parentSlots: ComponentSlotExpr) (comp: Component) (cc: CustomComponentType) : ParamBindings =
    let stored = cc.ParameterBindings |> Option.defaultValue Map.empty
    (stored, parentSlots)
    ||> Map.fold (fun acc slot exprSpec ->
        match slot.CompSlot with
        | CustomCompParam p when slot.CompId = componentIdValue comp.Id ->
            Map.add (ParamName p) exprSpec.Expression acc
        | _ -> acc)

/// The values the child sheet's parameters take inside one instance of it.
/// As in simulation elaboration (GraphMerger.effectiveBindings): an instance binding is an
/// expression in the parent's parameters and becomes a plain value first; the merged bindings
/// (defaults overridden by those values) are then the environment every parameter evaluates in,
/// so a default expression referring to an instance-bound parameter agrees with elaboration.
/// A binding that cannot be evaluated makes its parameter unknown, never a default.
let private childParamValues
        (parentValues: Map<ParamName, ParamInt option>)
        (bindingExprs: ParamBindings)
        (childDefaults: ParamBindings)
        : Map<ParamName, ParamInt option> =
    let parentBindings = knownBindings parentValues
    let merged =
        childDefaults
        |> Map.map (fun name defExpr ->
            match Map.tryFind name bindingExprs with
            | Some expr ->
                match evaluateParamExpression parentBindings expr with
                | Ok v -> Some (PInt v)
                | Error _ -> None
            | None -> Some defExpr)
    let env =
        (Map.empty, merged)
        ||> Map.fold (fun acc name exprOpt ->
            match exprOpt with
            | Some expr -> Map.add name expr acc
            | None -> acc)
    childDefaults
    |> Map.map (fun name _ ->
        match Map.tryFind name merged |> Option.flatten with
        | None -> None
        | Some expr ->
            match evaluateParamExpression env expr with
            | Ok v -> Some v
            | Error _ -> None)

/// Walk the instance tree under `topSheet` and collect the instances of every sheet with the
/// parameter values they resolve to. This is the binding walk simulation elaboration performs,
/// without building graphs.
/// The walk is memoised on (sheet name, parameter values): the subtree below an instance depends
/// only on those, so a sheet tree recurring with the same values is descended once. The VALUE
/// SETS this produces are complete - a skipped subtree resolves identically to the one already
/// walked. Nothing here answers "by which route": that question is asked only by the bind-to-top
/// chain computation, which works on the sheet DAG so that no path can be missed.
let analyseUnderTop (ldcs: LoadedComponent list) (topSheet: string) : SheetInstances =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList

    // prepended, and reversed once at the end: appending each instance to the list it belongs to
    // made recording a sheet's instances quadratic in how many it has
    let record (acc: SheetInstances) sheetName instance =
        let existing = Map.tryFind sheetName acc |> Option.defaultValue []
        Map.add sheetName (instance :: existing) acc

    let rec walk
            (acc: SheetInstances, visited: Set<string * (ParamName * ParamInt option) list>)
            (isInstance: bool)
            (sheetsOnPath: Set<string>)
            (sheetName: string)
            (values: Map<ParamName, ParamInt option>)
            (boundParams: Set<ParamName>)
            : SheetInstances * Set<string * (ParamName * ParamInt option) list> =
        match Map.tryFind sheetName byName with
        | None -> acc, visited
        | Some ldc ->
            let acc = record acc sheetName { IsInstance = isInstance; ParamValues = values; BoundParams = boundParams }
            let key = sheetName, Map.toList values
            match Set.contains key visited with
            | true -> acc, visited
            | false ->
                let visited = Set.add key visited
                ((acc, visited), customInstances ldc)
                ||> List.fold (fun accVisited (comp, cc) ->
                    // a sheet containing itself is an illegal design; do not recurse for ever on it
                    match Set.contains cc.Name sheetsOnPath, Map.tryFind cc.Name byName with
                    | true, _ | _, None -> accVisited
                    | false, Some childLdc ->
                        let bindingExprs = instanceBindingExprs (sheetParamSlots ldc) comp cc
                        let childDefaults = declaredParams childLdc
                        let childValues = childParamValues values bindingExprs childDefaults
                        // a binding for a parameter the child no longer declares sets nothing, as
                        // in childParamValues and in elaboration
                        let childBound =
                            bindingExprs
                            |> Map.toList
                            |> List.map fst
                            |> List.filter (fun name -> Map.containsKey name childDefaults)
                            |> Set.ofList
                        walk accVisited true (Set.add cc.Name sheetsOnPath) cc.Name childValues childBound)

    match Map.tryFind topSheet byName with
    | None -> Map.empty
    | Some topLdc ->
        let topValues = evaluatedDefaults (declaredParams topLdc)
        // the top is an instance of nothing, so nothing sets its parameters
        walk (Map.empty, Set.empty) false (Set.singleton topSheet) topSheet topValues Set.empty
        |> fst
        |> Map.map (fun _ instances -> List.rev instances)

/// What the editor should display for one parameter of a sheet, computed from the value the
/// parameter takes in every instance of the sheet under the top.
type ParamDisplayValue =
    /// Nothing under the top instantiates the sheet, so nothing gives the parameter a value. The
    /// stored value stands in until the sheet is used, and the properties pane shows it greyed and
    /// italic so that a provisional number is never read as a fact about the design.
    | NotUsed of ParamInt
    /// The values the instances give it: distinct, descending, never empty. The head is the one
    /// the sheet is drawn at. Several arise when one design reaches the sheet by paths that bind
    /// it differently, which is allowed; the largest is taken so that the choice is definite and
    /// the recomputation stays idempotent.
    | Values of ParamInt list

/// The value the sheet is drawn at, whichever case produced it.
let shownValue (display: ParamDisplayValue) : ParamInt =
    match display with
    | NotUsed v -> v
    | Values values -> List.head values

/// The records of a sheet that are instances of it: see SheetInstance.IsInstance.
let private instancesOnly (instances: SheetInstance list) =
    instances |> List.filter (fun inst -> inst.IsInstance)

/// The display value of each parameter a sheet declares, given the sheet's records under the
/// top. Unknown (unevaluable) instance values are ignored: checking here must not accuse a
/// design that may be right.
let displayValuesOfSheet (ldc: LoadedComponent) (instances: SheetInstance list) : Map<ParamName, ParamDisplayValue> =
    let defaults = declaredParams ldc
    let defaultValues = evaluatedDefaults defaults
    let instances = instancesOnly instances
    defaults
    |> Map.map (fun name _ ->
        // 1, as every other fallback in this module: a declared value that will not evaluate has
        // to stand in as something, and a width of 0 is never one
        let storedValue =
            Map.tryFind name defaultValues |> Option.flatten |> Option.defaultValue 1I
        let values =
            instances
            // only instances that SET this parameter; one that leaves it alone is carrying the
            // sheet's own stored value back to it, which settles nothing
            |> List.filter (fun inst -> Set.contains name inst.BoundParams)
            |> List.choose (fun inst -> Map.tryFind name inst.ParamValues |> Option.flatten)
            |> List.distinct
            |> List.sortDescending
        match values with
        | [] -> NotUsed storedValue
        | _ -> Values values)

/// The display values of every parameter of `sheetName` under `topSheet`.
let displayValues (ldcs: LoadedComponent list) (topSheet: string) (sheetName: string) : Map<ParamName, ParamDisplayValue> =
    match ldcs |> List.tryFind (fun ldc -> ldc.Name = sheetName) with
    | None -> Map.empty
    | Some ldc ->
        let instances =
            analyseUnderTop ldcs topSheet
            |> Map.tryFind sheetName
            |> Option.defaultValue []
        displayValuesOfSheet ldc instances

/// The sheets in the instance tree under a top sheet (the top included).
let sheetsUnderTop (ldcs: LoadedComponent list) (topSheet: string) : Set<string> =
    analyseUnderTop ldcs topSheet |> Map.keys |> Set.ofSeq

/// The roots of the instance forest whose design contains the given sheet.
let rootsContaining (ldcs: LoadedComponent list) (sheetName: string) : string list =
    instanceForestRoots ldcs
    |> List.filter (fun root -> Set.contains sheetName (sheetsUnderTop ldcs root))

/// The design a sheet belongs to: the sheet whose parameter values everything below it is derived
/// from. TOTAL - there is always an answer, which is what lets every other parameter value be
/// derived rather than guessed.
///
/// This used to ask whether the PROJECT had a single design, by looking for exactly one
/// instance-forest root. One stray sheet that nothing instantiates - a scratch sheet, a half-built
/// block - is a second root, so that question answered "no" for the whole project, and every
/// parameter row in every sheet silently fell back to its stored value. The question that matters
/// is which design the sheet being looked at is part of, and a stray sheet elsewhere does not
/// change that.
///
/// Several roots reaching one sheet is the genuinely ambiguous case - two designs using the same
/// subsheet at different sizes, which is not the same thing as one design reaching it by two paths
/// - and is what the top-sheet popup asks about. Until it is answered the first root is taken, so
/// that a value is always defined.
let effectiveTopSheetFor (ldcs: LoadedComponent list) (sheetName: string) : string =
    match flaggedTopSheet ldcs with
    | Some flagged -> flagged
    | None ->
        match rootsContaining ldcs sheetName with
        | [ single ] -> single
        // nothing contains it, so it is a root itself and is the top of its own design
        | [] -> sheetName
        | first :: _ -> first

/// Every parameter environment a sheet is used in: one set of values per distinct way its design
/// binds it, and its own declared values where nothing uses it at all.
///
/// This is what makes "the width of that memory" a question with several answers. A sheet used
/// twice at different sizes has two environments, so a component on it has two sets of widths, and
/// anything that has to hold for the component - contents fitting a memory, above all - has to hold
/// in every one of them. displayValuesOfSheet answers the same question one parameter at a time,
/// which cannot be used here: two parameters' values must be taken from the SAME instance, and the
/// per-parameter answer has already forgotten which instance each value came from.
///
/// Unknown values are dropped rather than guessed, so an expression using one fails to evaluate and
/// the caller is left without an answer instead of with a wrong one.
let bindingEnvironmentsOf (ldcs: LoadedComponent list) (sheetName: string) : ParamBindings list =
    match ldcs |> List.tryFind (fun ldc -> ldc.Name = sheetName) with
    | None -> [ Map.empty ]
    | Some ldc ->
        let instances =
            analyseUnderTop ldcs (effectiveTopSheetFor ldcs sheetName)
            |> Map.tryFind sheetName
            |> Option.defaultValue []
            |> instancesOnly
        match instances with
        // nothing instantiates it, so the sheet's own declared values are the only ones it has -
        // the same fallback ParamDisplayValue.NotUsed stands for
        | [] -> [ declaredParams ldc ]
        | _ -> instances |> List.map (fun inst -> knownBindings inst.ParamValues) |> List.distinct

/// The address and word widths one memory component has across the whole design: one pair per
/// environment the sheet it sits on is used in, without duplicates.
///
/// A width that is not parameterised, or whose expression will not evaluate in some environment, is
/// the one the component is carrying - which is what the sheet is drawn at, and the only answer
/// there is. So this is never empty, and never invents a pairing: both widths of a pair come from
/// the same environment, and crossing the two lists would make sizes no instance has.
let memoryWidthsInDesign
        (ldcs: LoadedComponent list)
        (sheetName: string)
        (compId: int)
        (mem: Memory1)
        : (int * int) list =
    let slots =
        ldcs
        |> List.tryFind (fun ldc -> ldc.Name = sheetName)
        |> Option.map sheetParamSlots
        |> Option.defaultValue Map.empty
    let widthOf (slotName: CompSlotName) (stored: int) (bindings: ParamBindings) =
        slots
        |> Map.tryPick (fun slot exprSpec ->
            match sameSlot slot {CompId = compId; CompSlot = slotName} with
            | true -> Some exprSpec.Expression
            | false -> None)
        |> Option.bind (fun expr ->
            match evaluateParamExpression bindings expr with
            | Ok value -> tryIntOfParamInt value
            | Error _ -> None)
        |> Option.defaultValue stored
    bindingEnvironmentsOf ldcs sheetName
    |> List.map (fun bindings ->
        widthOf MemoryAddressWidth mem.AddressWidth bindings,
        widthOf MemoryWordWidth mem.WordWidth bindings)
    |> List.distinct

/// Every sheet brought into line with what its design sets its parameters to: the parameter values
/// a design settles are written into the sheet, and its parameterised slots are rewritten at them.
///
/// A pure recomputation from the primary state - each design's top-sheet values, and the bindings
/// on instances - rather than an incremental edit. That is what makes it safe to run after
/// anything: it is idempotent and order-independent, so undo need only restore the primary state
/// and run this again, and no change has to reason about which sheets a binding might reach.
///
/// A parameter nothing sets is left exactly as it is. Its stored value IS the primary state for
/// that sheet, and overwriting it would destroy the only copy.
let propagateParameterValues (ldcs: LoadedComponent list) : LoadedComponent list =
    // The instance tree is walked ONCE PER CANDIDATE TOP and the answers reused. Asking
    // effectiveTopSheetFor and then displayValues for each sheet separately walked it twice per
    // sheet per root, so a project with no flagged top paid `sheets x (roots + 1)` walks - and
    // this runs after every parameter edit and every change to an instance's bindings.
    let analysed =
        match flaggedTopSheet ldcs with
        // a flagged top governs every sheet, so there is one tree to walk
        | Some flagged -> [flagged, analyseUnderTop ldcs flagged]
        | None -> instanceForestRoots ldcs |> List.map (fun root -> root, analyseUnderTop ldcs root)

    /// The instances of `sheetName` under the design it belongs to. The same choice
    /// effectiveTopSheetFor makes - the flagged top, else the single root containing the sheet,
    /// else the first of several - made against the walks already done.
    let instancesOf (sheetName: string) =
        match analysed |> List.filter (fun (_, insts) -> Map.containsKey sheetName insts) with
        | (_, insts) :: _ -> Map.tryFind sheetName insts |> Option.defaultValue []
        // no design reaches it, so it is the top of its own: only possible where a cycle keeps it
        // out of every root's tree, which is an illegal design being displayed as best it can be
        | [] -> analyseUnderTop ldcs sheetName |> Map.tryFind sheetName |> Option.defaultValue []

    ldcs
    |> List.map (fun ldc ->
        match ldc.LCParameterSlots with
        | None -> ldc
        | Some defs ->
            let settled =
                displayValuesOfSheet ldc (instancesOf ldc.Name)
                |> Map.toList
                |> List.choose (fun (name, d) ->
                    match d with
                    | Values (largest :: _) -> Some (name, largest)
                    | Values [] | NotUsed _ -> None)
            let newBindings =
                (defs.DefaultBindings, settled)
                ||> List.fold (fun bindings (name, value) ->
                    match Map.tryFind name bindings with
                    | Some definition -> Map.add name {definition with Expression = PInt value} bindings
                    | None -> bindings)
            let newDefs = {defs with DefaultBindings = newBindings}
            {ldc with
                LCParameterSlots = Some newDefs
                CanvasState =
                    ComponentSlots.resolveCanvasAtBindings
                        (bindingsOf newBindings) newDefs.ParamSlots ldc.CanvasState})

//------------------------------------------------------------------------------------------------//
//------------------------------ How much of the feature to show ---------------------------------//
//------------------------------------------------------------------------------------------------//

(*
    Parameters are an advanced feature, and there are three levels of use. A user at one level must
    not have to understand the next, so the UI turns on in two stages rather than all at once:

      1. no parameters at all - library components do not count
      2. parameters with a single settled value throughout
      3. a parameter bound to different values in different instances

    Gate A separates 1 from 2-3, and governs whether the vocabulary appears anywhere. Gate B
    separates 2 from 3, and governs the top sheet and everything that hangs off it. They were
    previously one structural test, which was right for neither.
*)

/// Whether a sheet came from a component library. ComponentLibraries.isLibrarySheet says the same
/// thing, but that module is compiled after this one; the fact lives on ldc.Form either way.
let private isFromLibrary (ldc: LoadedComponent) =
    match ldc.Form with
    | Some (Library _) -> true
    | _ -> false

/// Gate A: does this project use parameters at all?
///
/// Library sheets do not count. Their parameters arrived with the library rather than being
/// declared by the user, and on a library component instance they are presented as ordinary
/// settings - so placing one must not turn the parameter vocabulary on across the whole project.
let projectDeclaresParams (ldcs: LoadedComponent list) : bool =
    ldcs
    |> List.filter (isFromLibrary >> not)
    |> List.exists (fun ldc -> not (Map.isEmpty (declaredParamDefs ldc)))

/// Give every instance of `sheetName` a binding for a parameter just declared on it.
///
/// Every instance binds every parameter its sheet declares. Placing one establishes that, but a
/// parameter added to a sheet that ALREADY has instances would leave all of them binding nothing,
/// and an unbound parameter is a state the design deliberately does not have: it elaborates at the
/// sheet's own declared value, which is a fact about the sheet rather than about the instance, and
/// it makes "default" into a concept the user has to reason about.
///
/// The value bound is the one just declared, so the design is unchanged - that is exactly what an
/// unbound parameter elaborated to. What changes is that the binding exists, can be seen and
/// edited, and, being a literal, is what findBindOffers fires on.
///
/// No slot is created: a literal needs none, as updateParamSlot has it. A sheet cannot instantiate
/// itself, so the sheet gaining the parameter is skipped.
let bindParamOnInstances
        (sheetName: string)
        (name: ParamName)
        (value: ParamInt)
        (ldcs: LoadedComponent list)
        : LoadedComponent list =
    let addToSheet (ldc: LoadedComponent) =
        let comps, conns = ldc.CanvasState
        let addBinding (comp: Component) =
            match comp.Type with
            | Custom custom when custom.Name = sheetName ->
                let bindings = custom.ParameterBindings |> Option.defaultValue Map.empty
                match Map.containsKey name bindings with
                | true -> comp
                | false ->
                    {comp with
                        Type = Custom {custom with ParameterBindings = Some (Map.add name (PInt value) bindings)}}
            | _ -> comp
        let ldc' = {ldc with CanvasState = List.map addBinding comps, conns}
        match ldc'.CanvasState = ldc.CanvasState with
        | true -> ldc
        | false -> {ldc' with LoadedComponentIsOutOfDate = true}
    ldcs
    |> List.map (fun ldc -> if ldc.Name = sheetName then ldc else addToSheet ldc)

/// Mark a sheet as differing from the file it was loaded from.
///
/// A change to what a sheet DECLARES, or to the expression filling one of its slots, need not
/// change its canvas at all: declaring a parameter, writing its description, deleting an unused
/// one, or entering an expression that works out to the width already shown all leave the canvas
/// identical. Issie decides whether the open sheet needs saving by comparing canvases
/// (UpdateHelpers.currentSheetIsOutOfDate), so a change of that kind is invisible to it: the save
/// button stays dark, switching sheets does not save, and the work is dropped.
///
/// This flag is the one other thing that comparison consults, and saving the sheet clears it. It
/// is what ParameterView.markSheetParamsChanged sets on every path that edits parameter data.
let markSheetOutOfDate (sheetName: string) (ldcs: LoadedComponent list) : LoadedComponent list =
    ldcs
    |> List.map (fun ldc ->
        match ldc.Name = sheetName with
        | true -> {ldc with LoadedComponentIsOutOfDate = true}
        | false -> ldc)

/// Whether every instance of every sheet binds every parameter that sheet declares.
/// The invariant bindParamOnInstances and the placement popup exist to keep; false only for a
/// project saved before it was required, or one edited by hand.
/// Give every instance a binding for every parameter its sheet declares, at that sheet's own
/// value, wherever one is missing.
///
/// Every instance binding every parameter is an invariant the rest of the parameter system is
/// written against: placing an instance asks for each value, and adding a parameter to a sheet
/// binds it on the instances that already exist. Only a project saved before that was required, or
/// one edited by hand, can arrive without it - so it is repaired on load rather than guarded
/// against everywhere. An invariant that almost always holds buys nothing: every reader still has
/// to handle the case it does not.
///
/// The value used is the child sheet's own, which is what such an instance was already resolving
/// to, so this changes no design - it only writes down what was already true.
let bindMissingInstanceParams (ldcs: LoadedComponent list) : LoadedComponent list =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList
    ldcs
    |> List.map (fun parentLdc ->
        let comps, conns = parentLdc.CanvasState
        let repair (comp: Component) =
            match comp.Type with
            | Custom cc ->
                match Map.tryFind cc.Name byName with
                | None -> comp
                | Some childLdc ->
                    let alreadyBound = instanceBindingExprs (sheetParamSlots parentLdc) comp cc
                    let stored = cc.ParameterBindings |> Option.defaultValue Map.empty
                    let missing =
                        declaredParams childLdc
                        |> Map.filter (fun name _ -> not (Map.containsKey name alreadyBound))
                    match Map.isEmpty missing with
                    | true -> comp
                    | false ->
                        let defaults = evaluatedDefaults (declaredParams childLdc)
                        let bindings =
                            (stored, missing)
                            ||> Map.fold (fun acc name _ ->
                                let value =
                                    Map.tryFind name defaults |> Option.flatten |> Option.defaultValue 1I
                                Map.add name (PInt value) acc)
                        {comp with Type = Custom {cc with ParameterBindings = Some bindings}}
            | _ -> comp
        match List.map repair comps with
        | repaired when repaired = comps -> parentLdc
        | repaired -> {parentLdc with CanvasState = repaired, conns; LoadedComponentIsOutOfDate = true})

let everyInstanceBindsEveryParam (ldcs: LoadedComponent list) : bool =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList
    ldcs
    |> List.forall (fun parentLdc ->
        customInstances parentLdc
        |> List.forall (fun (comp, cc) ->
            match Map.tryFind cc.Name byName with
            | None -> true
            | Some childLdc ->
                let bound = instanceBindingExprs (sheetParamSlots parentLdc) comp cc
                declaredParams childLdc
                |> Map.forall (fun name _ -> Map.containsKey name bound)))

/// Gate B: does this project need a top sheet?
///
/// A top sheet exists to settle WHICH VALUES a sheet is drawn at when its instances disagree. The
/// presence of a parameter is not that question: a sheet with one instance, or whose instances all
/// agree, or which is not instantiated at all, has nothing to settle, and none of the top-sheet
/// apparatus should appear for it.
///
/// Only sheets the user can open are asked the question. A library sheet is never displayed, so
/// there is no value to choose for it - but a library instance whose parameter is bound to an
/// expression rather than a literal takes its value from the parent sheet, so when the parent
/// varies the PARENT is ambiguous and is caught here on its own account. Nothing needs to
/// special-case libraries beyond not asking about them.
///
/// Ambiguity means two DESIGNS disagreeing about a sheet, not one design reaching it by two paths.
/// The second is ordinary - a sheet used at two sizes within one design - and is settled by taking
/// the largest, with the others shown beside it. The first cannot be settled by any rule: the two
/// designs are both right, and only the user can say which one they are looking at.
///
/// The values each root gives the sheet are therefore compared root by root. Merging the instances
/// of every root into one bag, as this used to, made the two indistinguishable and raised the
/// top-sheet question for designs that had nothing to decide.
let projectHasAmbiguousDisplay (ldcs: LoadedComponent list) : bool =
    let byRoot = instanceForestRoots ldcs |> List.map (analyseUnderTop ldcs)
    let valuesUnder (instancesBySheet: SheetInstances) (ldc: LoadedComponent) =
        Map.tryFind ldc.Name instancesBySheet
        |> Option.map (fun instances ->
            displayValuesOfSheet ldc instances |> Map.map (fun _ display -> shownValue display))
    ldcs
    |> List.filter (isFromLibrary >> not)
    |> List.exists (fun ldc ->
        byRoot
        |> List.choose (fun instancesBySheet -> valuesUnder instancesBySheet ldc)
        |> List.distinct
        |> List.length > 1)

//------------------------------------------------------------------------------------------------//
//----------------------------------- The bind-to-top offer --------------------------------------//
//------------------------------------------------------------------------------------------------//

/// One modification the bind-to-top offer would make if accepted.
type ChainAction =
    /// Declare parameter Param on Sheet with the given default value and description.
    /// The description is copied from the declaring ancestor: a pass-through parameter created
    /// here means the same thing as the one it is being chained to, and every parameter must
    /// carry a description.
    | AddSheetParam of Sheet: string * Param: ParamName * Default: ParamInt * Description: string
    /// Bind parameter Param of the instance InstanceId (of sheet ChildSheet, labelled
    /// InstanceLabel) on Sheet to the expression `Param`, i.e. to Sheet's own parameter.
    | BindInstance of Sheet: string * InstanceId: ComponentId * InstanceLabel: string * ChildSheet: string * Param: ParamName

/// An offer to bind one unbound instance parameter to a same-named parameter on an ancestor
/// sheet, by materialising parameters and bindings along every instance path from the ancestor
/// down to the instance.
type BindOffer = {
    /// The sheet the unbound instance sits on.
    OnSheet: string
    /// The unbound instance.
    InstanceId: ComponentId
    InstanceLabel: string
    /// The sheet inside the instance, which declares the parameter.
    ChildSheet: string
    Param: ParamName
    /// A representative declaring ancestor (the top sheet when it declares): what the offer
    /// describes itself as binding to.
    BindsTo: string
    /// Every ancestor sheet on a path from the top that already declares the parameter -
    /// the evidence the offer rests on.
    Declarers: string list
    /// Everything accepting would do, deduplicated across instance paths.
    Actions: ChainAction list
}

/// The sheets an offer's actions modify (parameters added or instance bindings created).
let sheetsModifiedByOffer (offer: BindOffer) : string list =
    offer.Actions
    |> List.map (function
        | AddSheetParam (sheet, _, _, _) -> sheet
        | BindInstance (sheet, _, _, _, _) -> sheet)
    |> List.distinct

/// The instance-link edges between the sheets in the tree under the top: every custom component
/// instance whose parent sheet and child sheet are both under the top.
let private edgesUnderTop (byName: Map<string, LoadedComponent>) (underTop: Set<string>) : InstancePathLink list =
    underTop
    |> Set.toList
    |> List.choose (fun name -> Map.tryFind name byName)
    |> List.collect (fun ldc ->
        customInstances ldc
        |> List.filter (fun (_, cc) -> Set.contains cc.Name underTop)
        |> List.map (fun (comp, cc) ->
            { ParentSheet = ldc.Name; InstanceId = comp.Id; InstanceLabel = comp.Label; ChildSheet = cc.Name }))

/// The sheets from which `target` can be reached through the given edges, target included.
let private sheetsReaching (edges: InstancePathLink list) (target: string) : Set<string> =
    let rec grow (reaching: Set<string>) =
        let more =
            edges
            |> List.filter (fun e -> Set.contains e.ChildSheet reaching && not (Set.contains e.ParentSheet reaching))
            |> List.map (fun e -> e.ParentSheet)
            |> Set.ofList
        match Set.isEmpty more with
        | true -> reaching
        | false -> grow (Set.union reaching more)
    grow (Set.singleton target)

/// The sheets reachable from `source` through the given edges, source included.
let private sheetsReachableFrom (edges: InstancePathLink list) (source: string) : Set<string> =
    let rec grow (reachable: Set<string>) =
        let more =
            edges
            |> List.filter (fun e -> Set.contains e.ParentSheet reachable && not (Set.contains e.ChildSheet reachable))
            |> List.map (fun e -> e.ChildSheet)
            |> Set.ofList
        match Set.isEmpty more with
        | true -> reachable
        | false -> grow (Set.union reachable more)
    grow (Set.singleton source)

/// The declaring ancestors and chain of actions binding parameter `name` of one unbound
/// instance (`instLink`, sitting on `instLink.ParentSheet`) up to those ancestors, over every
/// instance path from the top. Computed on the sheet DAG rather than by enumerating paths, so
/// duplicated subtrees neither blow up the computation nor cause any path's links to be missed.
/// None when no ancestor sheet on a path from the top declares the parameter - the evidence
/// gate: an unbound parameter alone is not evidence of a design constant.
/// What it would take to bind parameter `name` of an instance sitting on `onSheet` to a same-named
/// parameter of an ancestor, and the ancestors that make the offer worth making.
///
/// `finalLink` is the instance itself, where there is one. It is None when an instance is being
/// PLACED and so does not exist yet: everything above the instance depends only on which sheet it
/// sits on, and the caller binds the new instance directly. That is what lets the placement popup
/// and the properties pane offer the same thing.
let private chainActionsOnSheet
        (byName: Map<string, LoadedComponent>)
        (underTop: Set<string>)
        (edges: InstancePathLink list)
        (name: ParamName)
        (onSheet: string)
        (finalLink: InstancePathLink option)
        : (Set<string> * ChainAction list) option =
    let declares sheetName =
        Map.tryFind sheetName byName
        |> Option.map (declaredParams >> Map.containsKey name)
        |> Option.defaultValue false
    // ancestors of the instance: sheets on some path from the top down to its sheet
    let ancestors = Set.intersect underTop (sheetsReaching edges onSheet)
    let declarers = Set.filter declares ancestors
    match Set.isEmpty declarers with
    | true -> None
    | false ->
        // every sheet on some path from a declaring ancestor down to the instance's sheet:
        // each such sheet must carry the parameter, and each instance link between two of them
        // must bind it, for every path from the top to elaborate the instance at the top value
        let chainSheets =
            declarers
            |> Set.toList
            |> List.map (fun declarer ->
                Set.intersect (sheetsReachableFrom edges declarer) (sheetsReaching edges onSheet))
            |> Set.unionMany
        let defaultOf sheetName =
            Map.tryFind sheetName byName
            |> Option.map (declaredParams >> evaluatedDefaults)
            |> Option.bind (Map.tryFind name)
            |> Option.flatten
            |> Option.defaultValue 1I
        let descriptionOf sheetName =
            Map.tryFind sheetName byName
            |> Option.map declaredParamDefs
            |> Option.bind (Map.tryFind name)
            |> Option.map (fun def -> def.Description)
            |> Option.defaultValue ""
        /// Whether this link already passes the parameter down, rather than pinning it to a number.
        /// Merely HAVING a binding is not enough: every instance binds every parameter now, so a
        /// link bound to a literal is exactly the one that has to be rebound for the chain to carry
        /// a value from the ancestor. Testing for the binding's existence instead left every chain
        /// with no actions to take, and the offer was dropped as empty.
        let bindsToParameter (link: InstancePathLink) =
            match Map.tryFind link.ParentSheet byName with
            | None -> false
            | Some parentLdc ->
                fst parentLdc.CanvasState
                |> List.tryFind (fun comp -> comp.Id = link.InstanceId)
                |> Option.bind (fun comp ->
                    match comp.Type with
                    | Custom cc -> Some (instanceBindingExprs (sheetParamSlots parentLdc) comp cc)
                    | _ -> None)
                |> Option.bind (Map.tryFind name)
                |> Option.map exprContainsParams
                |> Option.defaultValue false
        // parameters created on intermediate sheets take the value and meaning of a declaring
        // ancestor, so those sheets remain viewable and simulatable standalone
        let rootDeclarer = Set.minElement declarers
        let rootDefault = defaultOf rootDeclarer
        let rootDescription = descriptionOf rootDeclarer
        let chainEdges =
            edges
            |> List.filter (fun e -> Set.contains e.ParentSheet chainSheets && Set.contains e.ChildSheet chainSheets)
            |> fun links -> links @ Option.toList finalLink
        let paramActions =
            chainSheets
            |> Set.toList
            |> List.filter (declares >> not)
            |> List.map (fun sheetName -> AddSheetParam (sheetName, name, rootDefault, rootDescription))
        let bindActions =
            chainEdges
            |> List.filter (bindsToParameter >> not)
            |> List.map (fun e -> BindInstance (e.ParentSheet, e.InstanceId, e.InstanceLabel, e.ChildSheet, name))
        Some (declarers, List.distinct (paramActions @ bindActions))

/// The bind-to-top offers that qualify under `topSheet`, optionally restricted to instances on
/// one sheet (`onSheet`). An offer exists for each unbound parameter of a custom component
/// instance in the tree under the top whose name is declared on an ancestor sheet along the
/// instance path - the evidence gate. Accepting materialises the chain along every instance
/// path from the ancestors to the instance.
let findBindOffers (ldcs: LoadedComponent list) (topSheet: string) (onSheet: string option) : BindOffer list =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList
    let underTop = analyseUnderTop ldcs topSheet |> Map.keys |> Set.ofSeq
    let edges = edgesUnderTop byName underTop

    /// The parameters of one instance that a chain could usefully be offered for: those bound to a
    /// plain number.
    ///
    /// This used to be the parameters bound to nothing at all. Every instance now binds every
    /// parameter its sheet declares - placing one asks for each, and so does adding a parameter to
    /// a sheet that already has instances - so that set is always empty and the offer would never
    /// fire again.
    ///
    /// A literal is the right trigger in its own right, not merely a replacement. The offer exists
    /// to help a user follow an outer parameter of the same name, and typing that name into the box
    /// by hand fails whenever a sheet in between does not declare it: parameter scoping is single
    /// level. Materialising the chain is the thing the user cannot easily do themselves, and an
    /// instance given a literal before the design-wide parameter existed needs it just as much as
    /// one that was never bound. The evidence gate below is what keeps this quiet: an ancestor must
    /// already declare the name.
    let literalBoundParams (parentLdc: LoadedComponent) (comp: Component) (cc: CustomComponentType) =
        match Map.tryFind cc.Name byName with
        | None -> []
        | Some childLdc ->
            let bound = instanceBindingExprs (sheetParamSlots parentLdc) comp cc
            declaredParams childLdc
            |> Map.toList
            |> List.map fst
            |> List.filter (fun name ->
                match Map.tryFind name bound with
                // still offered where nothing binds it: projects saved before totality was
                // required, and hand-edited files, can still reach this state
                | None -> true
                | Some expr -> not (exprContainsParams expr))

    underTop
    |> Set.toList
    |> List.filter (fun sheetName -> onSheet |> Option.forall (fun s -> s = sheetName))
    |> List.collect (fun sheetName ->
        match Map.tryFind sheetName byName with
        | None -> []
        | Some parentLdc ->
            customInstances parentLdc
            |> List.collect (fun (comp, cc) ->
                literalBoundParams parentLdc comp cc
                |> List.choose (fun name ->
                    let link = {
                        ParentSheet = sheetName
                        InstanceId = comp.Id
                        InstanceLabel = comp.Label
                        ChildSheet = cc.Name }
                    chainActionsOnSheet byName underTop edges name sheetName (Some link)
                    |> Option.bind (fun (declarers, actions) ->
                        match actions with
                        | [] -> None
                        | _ ->
                            let bindsTo =
                                match Set.contains topSheet declarers with
                                | true -> topSheet
                                | false -> Set.minElement declarers
                            Some {
                                OnSheet = sheetName
                                InstanceId = comp.Id
                                InstanceLabel = comp.Label
                                ChildSheet = cc.Name
                                Param = name
                                BindsTo = bindsTo
                                Declarers = Set.toList declarers
                                Actions = actions
                            }))))

/// The same offer, for an instance that has not been placed yet.
///
/// Placing an instance and editing a placed one ask the identical question - can this parameter
/// follow a same-named parameter of an ancestor, and what would that take - so they must give the
/// identical answer. The popup used to test only whether the IMMEDIATE parent declared the name,
/// which offered the chain in the one case the user did not need it (they could type the name)
/// and withheld it in the case they did: parameter scoping is single level, so a name declared
/// two sheets up cannot be reached by typing anything.
///
/// Returns the ancestor the offer describes itself as following, and everything accepting would
/// do APART from binding the new instance, which the caller does directly.
let bindOfferForPlacement
        (ldcs: LoadedComponent list)
        (topSheet: string)
        (onSheet: string)
        (name: ParamName)
        : (string * ChainAction list) option =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList
    let underTop = analyseUnderTop ldcs topSheet |> Map.keys |> Set.ofSeq
    let edges = edgesUnderTop byName underTop
    chainActionsOnSheet byName underTop edges name onSheet None
    |> Option.map (fun (declarers, actions) ->
        let bindsTo =
            match Set.contains topSheet declarers with
            | true -> topSheet
            | false -> Set.minElement declarers
        bindsTo, actions)
