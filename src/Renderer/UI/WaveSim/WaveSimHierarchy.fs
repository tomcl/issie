/// The collapsed design hierarchy the wave selector shows.
///
/// A design whose sheets instantiate one another multiplies out: seven sheets of a few instances
/// each is tens of thousands of instances, and a selector with one entry per instance grows with
/// that rather than with the design somebody wrote. So the hierarchy shown has one node per SHEET
/// at each point in the tree, and the user says which instance of it they mean.
///
/// This module works out, once per render, everything both panes of the selector need: which nodes
/// there are, which instances each one could show, and which one it is showing. Both panes read the
/// same answer, so they cannot disagree about it.
module WaveSimHierarchy

open CommonTypes
open ModelType
open SimTypes
open MenuHelpers

/// One node of the collapsed hierarchy, resolved against the simulation.
///
/// The field names are prefixed because F# resolves an unannotated record field to the last type
/// declaring it, and every file compiled after this one is full of trees, keys and instances.
type SelectorNode = {
    /// design-time sheet names from the top sheet down to this node - the node's identity, and the
    /// key under which the model remembers what is open and which instance was chosen
    NodeKey: string list
    /// the sheet instances of this node's sheet that lie inside the instance chosen at its parent,
    /// alphabetically - so the head is what a node with nothing recorded about it shows
    NodeInstances: InstancePath list
    /// the instance on show. This is an InstancePath, which is what Wave.SheetId holds, so it is
    /// what selects the waves to list. None where the simulation has no instance here - a sheet
    /// with nothing on it, or a simulation of an earlier version of the design
    NodeInstance: InstancePath option
    /// whether more than one route from the top sheet reaches this node's sheet, so that it appears
    /// in the hierarchy more than once. Such a sheet has no place in the flat top level of the
    /// signal list - which of the routes would a single row stand for? - so its row is drawn inside
    /// each parent that instantiates it instead
    NodeMultiRoute: bool
    /// whether the user opens and closes this node. Not the same as NodeMultiRoute: a sheet with
    /// nothing inside it has nothing to reveal, so it gets no toggle, and a leaf reached two ways
    /// is exactly that case - which is why placement cannot be read off this
    NodeCollapsible: bool
    }

/// What the two panes of the wave selector draw, worked out together.
type SelectorHierarchy = {
    /// the visible tree, built only as far as it is drawn
    HierTree: SheetTree
    /// every visible node, by NodeKey
    HierNodes: Map<string list, SelectorNode>
    /// the visible nodes in the order the pills are drawn: a walk of HierTree, parents first
    HierOrder: SelectorNode list
    }

/// The hierarchy of a design with no project open: nothing to draw, and nothing to look up.
let emptyHierarchy = {
    HierTree =
        { SheetName = ""; BreadcrumbName = ""; LabelPath = []; SheetPath = []
          SheetAccessPath = []; Depth = 0; Size = 1; SubSheets = []; GridArea = None }
    HierNodes = Map.empty
    HierOrder = [] }

/// An instance path written as one string, for a place that can carry only one - the value behind
/// a DOM option. Ids, dot separated. NOT for showing anyone: see labelPathOf for that.
let pathKey (InstancePath ap) =
    ap |> List.map (fun (ComponentId i) -> string i) |> String.concat "."

/// The inverse, for reading a DOM option's value back.
let pathOfKey (key: string) =
    if key = "" then
        InstancePath []
    else
        key.Split '.'
        |> Array.toList
        |> List.choose (fun part ->
            match System.Int32.TryParse part with
            | true, n -> Some(ComponentId n)
            | _ -> None)
        |> InstancePath

/// An instance path as a person reads it - see `SimulatedDesign.LabelPathOfInstance`, which is
/// where this and the instance queries it is built from now live. Kept as a function here because
/// the selector calls it in several places and a `fs.Design.` prefix at each says nothing.
let labelPathOf (fs: FastSimulation) (instance: InstancePath) = fs.Design.LabelPathOfInstance instance

/// The top sheet's instance, which every other is reached from: the empty path, since nothing
/// contains it. None when there is no simulation to have a top sheet.
let private topInstance (fs: FastSimulation) =
    match fs.SimulatedTopSheet with
    | "" -> None
    | _ -> Some(InstancePath [])

/// What is inside each sheet of the SIMULATED design, and which of those sheets more than one route
/// from the top reaches.
///
/// The design the simulation was built from, not the one on the canvas now. Every instance, every
/// wave and every name in the selector comes from the simulation, so the tree they are drawn in has
/// to come from there too: reading the live project instead meant an edit to the schematic moved
/// half of the dialog and left the other half where it was, when an edit is meant to change nothing
/// until the simulation is restarted or refreshed. SimulatedCanvasState is exactly the sheets that
/// simulation needed, which is also what compareLoadedStates decides "needs refreshing" against.
///
/// Being a fact about the simulation, it is worked out once per simulation. It reads every
/// component of every sheet, and the dialog is rebuilt on every keystroke in its search boxes -
/// and it used to extract the whole open canvas from the draw block first, on each of them.
let private simulatedShapes: FastSimulation -> SheetShapes * Set<string> =
    Helpers.memoizeByIdentity (fun fs ->
        // A library component is opaque here whatever the Sheets menu is set to show: none of its
        // innards are offered as waves, so it must not appear in the hierarchy that selects them.
        let shapes = getSheetShapes (fun _ -> false) fs.SimulatedCanvasState
        shapes, multiPathSheets shapes fs.SimulatedTopSheet)

/// The hierarchy to draw: the sheets of the simulated design below its top sheet, collapsed so that
/// several instances of one sheet inside one parent are one node, cut off below any node the user
/// has not opened, and resolved against `fs` so each node names the instance it is showing.
let getSelectorHierarchy (fs: FastSimulation) (ws: WaveSimModel): SelectorHierarchy =
    let shapes, multiPath = simulatedShapes fs
    let root = fs.SimulatedTopSheet

    /// A sheet more than one route from the top reaches, and which is therefore in the hierarchy
    /// more than once.
    let multiRoute (key: string list) =
        match List.tryLast key with
        | Some sheet -> Set.contains sheet multiPath
        | None -> false

    /// A node the user opens and closes: reached more than one way, AND with something inside it
    /// to reveal. A leaf two routes reach is drawn inside each of its parents like any other
    /// multi-route sheet, but there is nothing under it to open.
    let collapsible (key: string list) =
        multiRoute key
        && (match List.tryLast key with
            | Some sheet -> not (Map.tryFind sheet shapes |> Option.defaultValue [] |> List.isEmpty)
            | None -> false)

    let tree =
        materialiseTree
            (fun key -> not (collapsible key) || Set.contains key ws.ShowSheetDetail)
            false
            shapes
            root


    /// Walk down, each node reading the instance its parent settled on. Validating the recorded
    /// choice against what is actually inside the parent's is what makes the chain hold: a choice
    /// higher up can change without anything below it having to be rewritten.
    let rec walk (node: SheetTree) (instances: InstancePath list) =
        let chosen =
            match Map.tryFind node.SheetPath ws.SelectedSheetInstance with
            | Some recorded when List.contains recorded instances -> Some recorded
            | _ -> List.tryHead instances
        let self = {
            NodeKey = node.SheetPath
            NodeInstances = instances
            NodeInstance = chosen
            NodeMultiRoute = multiRoute node.SheetPath
            NodeCollapsible = collapsible node.SheetPath
            }
        let below =
            node.SubSheets
            |> List.collect (fun sub ->
                match chosen with
                | Some parent -> walk sub (fs.Design.InstancesInside(parent, sub.SheetName))
                | None -> walk sub [])
        self :: below

    let order = walk tree (topInstance fs |> Option.toList)
    { HierTree = tree
      HierNodes = order |> List.map (fun node -> node.NodeKey, node) |> Map.ofList
      HierOrder = order }

/// The node a SheetTree node is drawn as, for the pills - which are handed SheetTree nodes by the
/// breadcrumb renderer and need what was worked out about them here.
let nodeOf (hierarchy: SelectorHierarchy) (sheet: SheetTree) =
    Map.tryFind sheet.SheetPath hierarchy.HierNodes
