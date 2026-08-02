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
    InstanceId: string
    InstanceLabel: string
    ChildSheet: string
}

/// One instance of a sheet in the tree under the top sheet.
type SheetInstance = {
    /// Links from the top sheet down to this instance; empty for the top sheet itself.
    Path: InstancePathLink list
    /// The value of each parameter the sheet declares, inside this instance.
    /// None marks a value that could not be evaluated - unknown, never reported as a conflict.
    ParamValues: Map<ParamName, ParamInt option>
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

/// The top sheet governing display for a project's loaded components.
/// The sheet the user has flagged wins; otherwise a single instance-forest root is inferred
/// silently, so a project whose sheets form one design never surfaces the concept.
/// None when several candidate tops exist and the user has not chosen between them.
let effectiveTopSheet (ldcs: LoadedComponent list) : string option =
    match ldcs |> List.tryFind (fun ldc -> ldc.IsTopSheet) with
    | Some flagged -> Some flagged.Name
    | None ->
        match instanceForestRoots ldcs with
        | [ single ] -> Some single
        | _ -> None

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
        | CustomCompParam p when slot.CompId = comp.Id -> Map.add (ParamName p) exprSpec.Expression acc
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
/// walked - but the recorded instance paths are exemplary, not exhaustive: descendants of a
/// repeated same-valued subtree keep only the paths of its first occurrence. Use the paths for
/// display examples; anything needing every path must work on the sheet DAG instead (as the
/// bind-to-top chain computation below does).
let analyseUnderTop (ldcs: LoadedComponent list) (topSheet: string) : SheetInstances =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList

    let record (acc: SheetInstances) sheetName instance =
        let existing = Map.tryFind sheetName acc |> Option.defaultValue []
        Map.add sheetName (existing @ [instance]) acc

    let rec walk
            (acc: SheetInstances, visited: Set<string * (ParamName * ParamInt option) list>)
            (path: InstancePathLink list)
            (sheetsOnPath: Set<string>)
            (sheetName: string)
            (values: Map<ParamName, ParamInt option>)
            : SheetInstances * Set<string * (ParamName * ParamInt option) list> =
        match Map.tryFind sheetName byName with
        | None -> acc, visited
        | Some ldc ->
            let acc = record acc sheetName { Path = path; ParamValues = values }
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
                        let link = {
                            ParentSheet = sheetName
                            InstanceId = comp.Id
                            InstanceLabel = comp.Label
                            ChildSheet = cc.Name }
                        let bindingExprs = instanceBindingExprs (sheetParamSlots ldc) comp cc
                        let childValues = childParamValues values bindingExprs (declaredParams childLdc)
                        walk accVisited (path @ [link]) (Set.add cc.Name sheetsOnPath) cc.Name childValues)

    match Map.tryFind topSheet byName with
    | None -> Map.empty
    | Some topLdc ->
        let topValues = evaluatedDefaults (declaredParams topLdc)
        walk (Map.empty, Set.empty) [] (Set.singleton topSheet) topSheet topValues
        |> fst

/// A human-readable instance path: the top sheet followed by the labels of the instances entered.
let renderInstancePath (topSheet: string) (path: InstancePathLink list) : string =
    topSheet :: (path |> List.map (fun link -> link.InstanceLabel))
    |> String.concat " > "

/// What the editor should display for one parameter of a sheet, computed from the value the
/// parameter takes in every instance of the sheet under the top.
type ParamDisplayValue =
    /// Every instance agrees on this value; it is exact and may differ from the default.
    | ExactValue of ParamInt
    /// The sheet is not instantiated under the top (or no instance value is known):
    /// the declared default is shown.
    | DefaultValue of ParamInt
    /// Instances disagree: the declared default is shown, and each value the parameter takes is
    /// listed with the instance paths that produce it.
    | MultipleValues of shownDefault: ParamInt * values: (ParamInt * InstancePathLink list list) list

/// The value shown for the parameter, whichever case produced it.
let shownValue (display: ParamDisplayValue) : ParamInt =
    match display with
    | ExactValue v | DefaultValue v | MultipleValues (v, _) -> v

/// The display value of each parameter a sheet declares, given the sheet's instances under the
/// top. Unknown (unevaluable) instance values are ignored: checking here must not accuse a
/// design that may be right.
let displayValuesOfSheet (ldc: LoadedComponent) (instances: SheetInstance list) : Map<ParamName, ParamDisplayValue> =
    let defaults = declaredParams ldc
    let defaultValues = evaluatedDefaults defaults
    defaults
    |> Map.map (fun name _ ->
        let defaultValue =
            Map.tryFind name defaultValues |> Option.flatten |> Option.defaultValue 0
        let valueGroups =
            instances
            |> List.choose (fun inst ->
                match Map.tryFind name inst.ParamValues with
                | Some (Some v) -> Some (v, inst.Path)
                | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (v, paths) -> v, List.map snd paths)
            |> List.sortBy fst
        match valueGroups with
        | [] -> DefaultValue defaultValue
        | [ (v, _) ] -> ExactValue v
        | many -> MultipleValues (defaultValue, many))

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

/// Whether every instance of every sheet binds every parameter that sheet declares.
/// The invariant bindParamOnInstances and the placement popup exist to keep; false only for a
/// project saved before it was required, or one edited by hand.
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
/// Computed over every root of the instance forest rather than one chosen top: a project may hold
/// several independent designs, and if two of them use a sheet at different widths then opening
/// that sheet is genuinely ambiguous.
let projectHasAmbiguousDisplay (ldcs: LoadedComponent list) : bool =
    let instancesBySheet =
        instanceForestRoots ldcs
        |> List.map (analyseUnderTop ldcs)
        |> List.fold
            (fun acc sheetInstances ->
                (acc, sheetInstances)
                ||> Map.fold (fun acc name instances ->
                    Map.tryFind name acc
                    |> Option.defaultValue []
                    |> (fun existing -> Map.add name (existing @ instances) acc)))
            Map.empty
    ldcs
    |> List.filter (isFromLibrary >> not)
    |> List.exists (fun ldc ->
        Map.tryFind ldc.Name instancesBySheet
        |> Option.defaultValue []
        |> displayValuesOfSheet ldc
        |> Map.exists (fun _ display ->
            match display with
            | MultipleValues _ -> true
            | ExactValue _ | DefaultValue _ -> false))

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
    | BindInstance of Sheet: string * InstanceId: string * InstanceLabel: string * ChildSheet: string * Param: ParamName

/// An offer to bind one unbound instance parameter to a same-named parameter on an ancestor
/// sheet, by materialising parameters and bindings along every instance path from the ancestor
/// down to the instance.
type BindOffer = {
    /// The sheet the unbound instance sits on.
    OnSheet: string
    /// The unbound instance.
    InstanceId: string
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
let private chainActionsForInstance
        (byName: Map<string, LoadedComponent>)
        (underTop: Set<string>)
        (edges: InstancePathLink list)
        (name: ParamName)
        (instLink: InstancePathLink)
        : (Set<string> * ChainAction list) option =
    let onSheet = instLink.ParentSheet
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
            |> Option.defaultValue 1
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
            |> fun links -> links @ [instLink]
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
                    chainActionsForInstance byName underTop edges name link
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
