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

/// The parameters a sheet declares: its default bindings.
let declaredParams (ldc: LoadedComponent) : ParamBindings =
    ldc.LCParameterSlots
    |> Option.map (fun ps -> ps.DefaultBindings)
    |> Option.defaultValue Map.empty

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
//----------------------------------- The bind-to-top offer --------------------------------------//
//------------------------------------------------------------------------------------------------//

/// One modification the bind-to-top offer would make if accepted.
type ChainAction =
    /// Declare parameter Param on Sheet with the given default value.
    | AddSheetParam of Sheet: string * Param: ParamName * Default: ParamInt
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
        | AddSheetParam (sheet, _, _) -> sheet
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
        let bindingExists (link: InstancePathLink) =
            match Map.tryFind link.ParentSheet byName with
            | None -> false
            | Some parentLdc ->
                fst parentLdc.CanvasState
                |> List.tryFind (fun comp -> comp.Id = link.InstanceId)
                |> Option.bind (fun comp ->
                    match comp.Type with
                    | Custom cc -> Some (instanceBindingExprs (sheetParamSlots parentLdc) comp cc)
                    | _ -> None)
                |> Option.map (Map.containsKey name)
                |> Option.defaultValue false
        // parameters created on intermediate sheets take the value of a declaring ancestor as
        // their default, so those sheets remain viewable and simulatable standalone
        let rootDefault = defaultOf (Set.minElement declarers)
        let chainEdges =
            edges
            |> List.filter (fun e -> Set.contains e.ParentSheet chainSheets && Set.contains e.ChildSheet chainSheets)
            |> fun links -> links @ [instLink]
        let paramActions =
            chainSheets
            |> Set.toList
            |> List.filter (declares >> not)
            |> List.map (fun sheetName -> AddSheetParam (sheetName, name, rootDefault))
        let bindActions =
            chainEdges
            |> List.filter (bindingExists >> not)
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

    /// unbound declared parameters of one instance
    let unboundParams (parentLdc: LoadedComponent) (comp: Component) (cc: CustomComponentType) =
        match Map.tryFind cc.Name byName with
        | None -> []
        | Some childLdc ->
            let bound = instanceBindingExprs (sheetParamSlots parentLdc) comp cc
            declaredParams childLdc
            |> Map.toList
            |> List.map fst
            |> List.filter (fun name -> not (Map.containsKey name bound))

    underTop
    |> Set.toList
    |> List.filter (fun sheetName -> onSheet |> Option.forall (fun s -> s = sheetName))
    |> List.collect (fun sheetName ->
        match Map.tryFind sheetName byName with
        | None -> []
        | Some parentLdc ->
            customInstances parentLdc
            |> List.collect (fun (comp, cc) ->
                unboundParams parentLdc comp cc
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

/// Which bind-to-top offers an event should surface. Offers are only ever raised at the events
/// that can bring a qualifying chain into existence, so declining is naturally one-shot: nothing
/// re-raises an offer whose circumstances have not changed.
type BindOfferScope =
    /// Custom component instances (by component id) just added to the named sheet: their own
    /// parameters and, because an added instance brings its whole subtree with it, anything
    /// unbound in the sheets below them.
    | NewInstances of OnSheet: string * InstanceIds: string list
    /// A parameter just declared on the named sheet: unbound same-named parameters below it.
    | NewParam of OnSheet: string * Param: ParamName
    /// The top sheet changed: everything under the new top. Re-offering something previously
    /// declined is correct here because the ancestor context genuinely changed.
    | WholeDesign

/// The offers an event with the given scope should surface, from the full offer list under the
/// current top.
let offersInScope (ldcs: LoadedComponent list) (scope: BindOfferScope) (offers: BindOffer list) : BindOffer list =
    match scope with
    | WholeDesign -> offers
    | NewParam (sheet, name) ->
        // offers this declaration is evidence for: the new parameter is one of their anchors
        offers |> List.filter (fun offer -> offer.Param = name && List.contains sheet offer.Declarers)
    | NewInstances (onSheet, ids) ->
        let idSet = Set.ofList ids
        let childSheets =
            ldcs
            |> List.tryFind (fun ldc -> ldc.Name = onSheet)
            |> Option.map (customInstances
                           >> List.filter (fun (comp, _) -> Set.contains comp.Id idSet)
                           >> List.map (fun (_, cc) -> cc.Name))
            |> Option.defaultValue []
        let subtreeSheets =
            childSheets
            |> List.collect (fun child -> analyseUnderTop ldcs child |> Map.keys |> List.ofSeq)
            |> Set.ofList
        offers
        |> List.filter (fun offer ->
            (offer.OnSheet = onSheet && Set.contains offer.InstanceId idSet)
            || Set.contains offer.OnSheet subtreeSheets)
