/// Which waveforms the schematic's right-click menu offers for a component on the canvas.
///
/// A component on the canvas is not one thing in the simulation. A sheet instantiated twice holds
/// two of everything in it, and neither copy is the symbol that was clicked on - so nothing is
/// offered, and the wave selector is what serves that case. An Input or Output in a subsheet holds
/// no wave at all: its signal belongs to the port of the instance the sheet sits in, named after
/// that port, and has to be found there. WaveSimSelect.wavesOfComponent resolves both.
module WaveSelection

open Expecto
open CommonTypes
open SimTypes
open CanvasBuilder

let private maxArraySize = 100

/// A sheet holding an input, a NOT gate and an output, so that it has both a component with waves
/// of its own and I/O whose waves are on the instance above.
let private notSheet (idBase: int) (name: string) (inLabel: string) (outLabel: string) =
    let i = makeComp (idBase + 1) 0 1 (Input1(1, None)) inLabel
    let n = makeComp (idBase + 2) 1 1 Not "N"
    let o = makeComp (idBase + 3) 1 0 (Output 1) outLabel
    makeLdc name None ([ i; n; o ], [ conn i 0 n 0; conn n 0 o 0 ])

let private solo = notSheet 10 "solo" "A" "Y"
let private twin = notSheet 20 "twin" "B" "Z"

/// Top sheet: solo instantiated once and twin instantiated twice, chained from IN to OUT.
let private top =
    let inp = makeComp 1 0 1 (Input1(1, None)) "IN"
    let topNot = makeComp 2 1 1 Not "TOPNOT"
    let solo1 = makeComp 3 1 1 (customOf solo [ "A", 1 ] [ "Y", 1 ] None) "SOLO1"
    let twin1 = makeComp 4 1 1 (customOf twin [ "B", 1 ] [ "Z", 1 ] None) "TWIN1"
    let twin2 = makeComp 5 1 1 (customOf twin [ "B", 1 ] [ "Z", 1 ] None) "TWIN2"
    let out = makeComp 6 1 0 (Output 1) "OUT"
    makeLdc "top" None
        ([ inp; topNot; solo1; twin1; twin2; out ],
         [ conn inp 0 topNot 0
           conn topNot 0 solo1 0
           conn solo1 0 twin1 0
           conn twin1 0 twin2 0
           conn twin2 0 out 0 ])

//-------------------------------------------------------------------------------------------//
// A second design, for the fallback: a top sheet with no ports of its own.
//
// A whole CPU is often like this - a memory, some subsystem instances, wired to each other and to
// nothing outside - so there is nothing for a first start to select from the top sheet, and the
// Viewers in the design are what it falls back to. The `3cpu` demo's `eep1` is the real example.
//-------------------------------------------------------------------------------------------//

/// A subsheet with a Viewer on the net inside it, so the design has a signal somebody has said is
/// worth watching, and it is not on the top sheet.
let private probed =
    let i = makeComp 11 0 1 (Input1(1, None)) "A"
    let n = makeComp 12 1 1 Not "N"
    let v = makeComp 13 1 0 (Viewer 1) "PROBE"
    let o = makeComp 14 1 0 (Output 1) "Y"
    makeLdc "probed" None ([ i; n; v; o ], [ conn i 0 n 0; conn n 0 v 0; conn n 0 o 0 ])

/// Top sheet with no Input, Output or Viewer on it: a constant drives an instance, whose output is
/// terminated rather than brought out.
let private closed =
    let c = makeComp 1 0 1 (Constant1(1, 1I, "1")) "C1"
    let inst = makeComp 2 1 1 (customOf probed [ "A", 1 ] [ "Y", 1 ] None) "PROBED1"
    let nc = makeComp 3 1 0 NotConnected "NC1"
    makeLdc "closed" None ([ c; inst; nc ], [ conn c 0 inst 0; conn inst 0 nc 0 ])

let private closedSimulation =
    lazy
        (match Simulator.startCircuitSimulation maxArraySize "closed" closed.CanvasState [ closed; probed ] with
         | Error e -> failwith $"Simulation setup failed: %A{e}"
         | Ok simData ->
             simData.FastSim, WaveSimSVGs.getWaves Set.empty ModelHelpers.initWSModel simData.FastSim)

/// The simulation and every wave the wave simulator would offer from it. Built once: making a
/// FastSimulation is not cheap, and the tests only read it.
let private simulation =
    lazy
        (match Simulator.startCircuitSimulation maxArraySize "top" top.CanvasState [ top; solo; twin ] with
         | Error e -> failwith $"Simulation setup failed: %A{e}"
         | Ok simData -> simData.FastSim, WaveSimSVGs.getWaves Set.empty ModelHelpers.initWSModel simData.FastSim)

/// The waves offered for a canvas component, and the number of copies of it in the simulation
let private offered (compId: int) =
    let fs, allWaves = simulation.Force()
    WaveSimSelect.wavesOfComponent fs allWaves (ComponentId compId)

//-------------------------------------------------------------------------------------------//
// A third design, for the collapsed hierarchy the selector draws.
//
// Three levels, two instances at each: `deep` holds two `mid`, and each `mid` holds two `leaf`, so
// there are four instances of `leaf` in two groups of two. That is the shape the selector has to
// collapse - one node for `leaf`, standing for whichever two of the four lie inside the instance
// of `mid` currently chosen.
//-------------------------------------------------------------------------------------------//

let private leafSheet = notSheet 10 "leaf" "A" "Y"

let private midSheet =
    let i = makeComp 21 0 1 (Input1(1, None)) "B"
    let l1 = makeComp 22 1 1 (customOf leafSheet [ "A", 1 ] [ "Y", 1 ] None) "LEAF1"
    let l2 = makeComp 23 1 1 (customOf leafSheet [ "A", 1 ] [ "Y", 1 ] None) "LEAF2"
    let o = makeComp 24 1 0 (Output 1) "Z"
    makeLdc "mid" None ([ i; l1; l2; o ], [ conn i 0 l1 0; conn l1 0 l2 0; conn l2 0 o 0 ])

let private deepSheet =
    let i = makeComp 1 0 1 (Input1(1, None)) "IN"
    let m1 = makeComp 2 1 1 (customOf midSheet [ "B", 1 ] [ "Z", 1 ] None) "MID1"
    let m2 = makeComp 3 1 1 (customOf midSheet [ "B", 1 ] [ "Z", 1 ] None) "MID2"
    let o = makeComp 4 1 0 (Output 1) "OUT"
    makeLdc "deep" None ([ i; m1; m2; o ], [ conn i 0 m1 0; conn m1 0 m2 0; conn m2 0 o 0 ])

let private deepProject: Project =
    { ProjectPath = "."
      OpenFileName = "deep"
      WorkingFileName = Some "deep"
      LoadedComponents = [ deepSheet; midSheet; leafSheet ] }

let private deepSimulation =
    lazy
        (let sheets = [ deepSheet; midSheet; leafSheet ]
         match Simulator.startCircuitSimulation maxArraySize "deep" deepSheet.CanvasState sheets with
         | Error e -> failwith $"Simulation setup failed: %A{e}"
         | Ok simData -> simData.FastSim, WaveSimSVGs.getWaves Set.empty ModelHelpers.initWSModel simData.FastSim)

/// Every instance of the simulation: the top sheet, which nothing contains, and one per custom
/// component - each of which introduces the instance of the sheet it names.
let private allInstances (fs: FastSimulation) =
    InstancePath []
    :: (fs.FCustomComps |> Map.toList |> List.map (fun ((cid, ap), _) -> InstancePath(ap @ [ cid ])))

/// How an instance reads, which is how these tests name one. The identity is a path of component
/// ids; this is the path of LABELS it renders as, which is what a user sees and what the identity
/// used to be.
let private instanceNames (fs: FastSimulation) =
    allInstances fs
    |> List.map (fun path -> (WaveSimHierarchy.labelPathOf fs path).ToUpperInvariant())

/// The instance that reads like this.
let private instanceNamed (fs: FastSimulation) (name: string) =
    allInstances fs
    |> List.find (fun path -> (WaveSimHierarchy.labelPathOf fs path).ToUpperInvariant() = name)

/// The hierarchy the selector would draw with the given instances chosen.
let private hierarchyWith (chosen: (string list * InstancePath) list) =
    let fs, _ = deepSimulation.Force()
    let ws = { ModelHelpers.initWSModel with SelectedSheetInstance = Map.ofList chosen }
    fs, WaveSimHierarchy.getSelectorHierarchy fs ws

let private midKey = [ "deep"; "mid" ]
let private leafKey = [ "deep"; "mid"; "leaf" ]

//-------------------------------------------------------------------------------------------//
// A fourth design, for the rule that decides where a sheet's row is drawn in the signal list.
//
// `shared` is instantiated by both `left` and `right`, so two routes from the top reach it and it
// appears in the hierarchy twice. There is nothing at top level to say which of them a single row
// would stand for, so its row belongs inside each parent instead. `left` and `right` are each
// reached one way, so theirs belong at top level.
//
// `shared` is a LEAF, which is the case that separates the two flags: it is reached more than one
// way, so it is drawn inside its parents, but it has nothing inside it to open.
//-------------------------------------------------------------------------------------------//

let private sharedSheet = notSheet 30 "shared" "A" "Y"

/// A sheet whose whole content is one instance of `shared`.
let private wrapperSheet idBase name label =
    let i = makeComp (idBase + 1) 0 1 (Input1(1, None)) "B"
    let s = makeComp (idBase + 2) 1 1 (customOf sharedSheet [ "A", 1 ] [ "Y", 1 ] None) label
    let o = makeComp (idBase + 3) 1 0 (Output 1) "Z"
    makeLdc name None ([ i; s; o ], [ conn i 0 s 0; conn s 0 o 0 ])

let private leftSheet = wrapperSheet 10 "left" "S1"
let private rightSheet = wrapperSheet 20 "right" "S2"

let private forkSheet =
    let i = makeComp 1 0 1 (Input1(1, None)) "IN"
    let l = makeComp 2 1 1 (customOf leftSheet [ "B", 1 ] [ "Z", 1 ] None) "LEFT"
    let r = makeComp 3 1 1 (customOf rightSheet [ "B", 1 ] [ "Z", 1 ] None) "RIGHT"
    let o = makeComp 4 1 0 (Output 1) "OUT"
    makeLdc "fork" None ([ i; l; r; o ], [ conn i 0 l 0; conn l 0 r 0; conn r 0 o 0 ])

let private forkProject: Project =
    { ProjectPath = "."
      OpenFileName = "fork"
      WorkingFileName = Some "fork"
      LoadedComponents = [ forkSheet; leftSheet; rightSheet; sharedSheet ] }

let private forkHierarchy =
    lazy
        (match
            Simulator.startCircuitSimulation
                maxArraySize "fork" forkSheet.CanvasState forkProject.LoadedComponents
         with
         | Error e -> failwith $"Simulation setup failed: %A{e}"
         | Ok simData ->
             WaveSimHierarchy.getSelectorHierarchy simData.FastSim ModelHelpers.initWSModel)

//-------------------------------------------------------------------------------------------//
// A fifth design, for the sheets the selector must not show at all.
//
// A library component is one thing, not a sheet with innards: none of what is inside it is offered
// as a waveform, so it has no place in the hierarchy that chooses them. The hierarchy is built from
// the design the SIMULATION holds, so this also says that what marks a sheet as a library one
// survives the trip into the simulation.
//-------------------------------------------------------------------------------------------//

let private plainInnerSheet = notSheet 10 "lib" "A" "Y"
let private libSheet = { plainInnerSheet with Form = Some(Library("arithmetic", "lib")) }

/// A top sheet with one instance of `inner`, which differs between the two runs only in its Form.
let private usesSheet (inner: LoadedComponent) =
    let i = makeComp 1 0 1 (Input1(1, None)) "IN"
    let l = makeComp 2 1 1 (customOf inner [ "A", 1 ] [ "Y", 1 ] None) "LIB1"
    let o = makeComp 3 1 0 (Output 1) "OUT"
    makeLdc "useslib" None ([ i; l; o ], [ conn i 0 l 0; conn l 0 o 0 ])

/// The nodes the selector would draw for a design whose one subsheet is `inner`.
let private nodesUsing (inner: LoadedComponent) =
    let top = usesSheet inner
    match Simulator.startCircuitSimulation maxArraySize "useslib" top.CanvasState [ top; inner ] with
    | Error e -> failwith $"Simulation setup failed: %A{e}"
    | Ok simData ->
        WaveSimHierarchy.getSelectorHierarchy simData.FastSim ModelHelpers.initWSModel
        |> fun hierarchy -> hierarchy.HierOrder |> List.map (fun node -> node.NodeKey)

//-------------------------------------------------------------------------------------------//
// A sixth design, for the one identity that is not a path of labels.
//
// Every sheet instance is identified by the labels of the custom components reaching it. The top
// sheet has none, so it is identified by its own name instead - which puts it in the same namespace
// as a one-element path, and a custom component on the top sheet may be labelled anything the user
// likes, including that sheet's name.
//-------------------------------------------------------------------------------------------//

let private innerSheet = notSheet 10 "inner" "A" "Y"

/// A top sheet named `clash` holding an instance of another sheet labelled CLASH. Both the top
/// sheet and that instance would be identified by "CLASH".
let private clashSheet =
    let i = makeComp 1 0 1 (Input1(1, None)) "IN"
    let s = makeComp 2 1 1 (customOf innerSheet [ "A", 1 ] [ "Y", 1 ] None) "CLASH"
    let o = makeComp 3 1 0 (Output 1) "OUT"
    makeLdc "clash" None ([ i; s; o ], [ conn i 0 s 0; conn s 0 o 0 ])

let private clashSimulation =
    lazy
        (match
            Simulator.startCircuitSimulation
                maxArraySize "clash" clashSheet.CanvasState [ clashSheet; innerSheet ]
         with
         | Error e -> failwith $"Simulation setup failed: %A{e}"
         | Ok simData ->
             simData.FastSim, WaveSimSVGs.getWaves Set.empty ModelHelpers.initWSModel simData.FastSim)

let tests =
    testList
        "WaveSelection"
        [ testCase "a component on the top sheet is offered its own ports"
          <| fun () ->
              let waves, copies = offered 2
              Expect.equal copies 1 "one copy of a component on the simulated top sheet"
              Expect.isNonEmpty waves "the gate's ports are offered"
              Expect.all
                  waves
                  (fun w -> w.WaveId.Id = (ComponentId 2, []))
                  "the waves are the component's own, at the root of the simulation"

          testCase "a component in a sheet instantiated once is offered its own ports"
          <| fun () ->
              let waves, copies = offered 12
              Expect.equal copies 1 "one copy of a component in a singly instantiated sheet"
              Expect.all
                  waves
                  (fun w -> snd w.WaveId.Id = [ ComponentId 3 ])
                  "the waves are inside the one instance"

          testCase "a component in a sheet instantiated twice is offered nothing"
          <| fun () ->
              let waves, copies = offered 22
              Expect.equal copies 2 "one copy per instantiation of the sheet"
              Expect.isEmpty waves "nothing offered: the canvas symbol is neither copy"

          testCase "an Input in a subsheet is offered the instance port carrying its signal"
          <| fun () ->
              let waves, copies = offered 11
              Expect.equal copies 1 "one copy of the Input itself"
              match waves with
              | [ wave ] ->
                  Expect.equal
                      wave.WaveId.Id
                      (ComponentId 3, [])
                      "the wave is on the instance the sheet sits in, not on the Input"
                  Expect.equal wave.WaveId.PortType PortType.Input "an Input maps to an input port"
                  Expect.equal wave.WaveId.PortNumber 0 "the port A is the instance's first input"
                  Expect.equal wave.PortLabel "A" "and is named after the Input, as the viewer names it"
              | waves -> failtestf "expected one wave for the subsheet Input, got %d" waves.Length

          testCase "an Output in a subsheet is offered the instance port carrying its signal"
          <| fun () ->
              let waves, _ = offered 13
              match waves with
              | [ wave ] ->
                  Expect.equal wave.WaveId.Id (ComponentId 3, []) "the wave is on the instance"
                  Expect.equal wave.WaveId.PortType PortType.Output "an Output maps to an output port"
                  Expect.equal wave.PortLabel "Y" "named after the Output"
              | waves -> failtestf "expected one wave for the subsheet Output, got %d" waves.Length

          testCase "an Input in a sheet instantiated twice is offered nothing"
          <| fun () ->
              let waves, copies = offered 21
              Expect.equal copies 2 "one copy per instantiation"
              Expect.isEmpty waves "the redirect is not reached: there is no single instance to redirect to"

          testCase "a component outside the simulated design is offered nothing"
          <| fun () ->
              let waves, copies = offered 999
              Expect.equal copies 0 "no copies of a component the simulation does not hold"
              Expect.isEmpty waves "and so nothing to offer"

          // Hovering a wave's name highlights the wires carrying it, which are found by the sheet
          // the wave's component is on. A subsheet is reached through a custom component instance
          // whose LABEL is whatever the person who placed it chose - here SOLO1, for the sheet
          // named solo - so taking the sheet's name from that label finds no sheet, and no wires.
          testCase "a wave in a subsheet knows its sheet by the instance's type, not its label"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              match fst (WaveSimSelect.wavesOfComponent fs allWaves (ComponentId 12)) with
              | wave :: _ ->
                  Expect.equal
                      (WaveSimHelpers.sheetOfWave fs wave)
                      (Some "solo")
                      "the sheet is the instance's type, whose label is SOLO1"
                  Expect.isNonEmpty
                      (WaveSimHelpers.connsOfWave fs wave)
                      "so the wave's connections are found and its wires can be highlighted"
              | [] -> failtest "expected waves for the gate inside the subsheet"

          testCase "a wave on the top sheet knows its sheet"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              match fst (WaveSimSelect.wavesOfComponent fs allWaves (ComponentId 2)) with
              | wave :: _ ->
                  Expect.equal (WaveSimHelpers.sheetOfWave fs wave) (Some "top") "the simulated top sheet"
                  Expect.isNonEmpty (WaveSimHelpers.connsOfWave fs wave) "and its connections are found"
              | [] -> failtest "expected waves for the gate on the top sheet"

          //-----------------------------------------------------------------------------------//
          // What the viewer shows before the user has chosen anything
          //-----------------------------------------------------------------------------------//

          testCase "a first start shows the top sheet's own inputs and outputs"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              let chosen =
                  WaveSimSelect.defaultSelectedWaves fs allWaves
                  |> List.map (fun wi -> WaveSimSelect.getName wi fs)
              // IN before OUT: inputs are ranked before outputs, and each group is sorted by label
              Expect.equal chosen [ "IN"; "OUT" ] "the top sheet's ports, inputs first"

          testCase "a first start shows nothing from inside a subsheet"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              Expect.all
                  (WaveSimSelect.defaultSelectedWaves fs allWaves)
                  (fun wi -> snd wi.Id = [])
                  "every default wave is on the simulated top sheet, not inside an instance"

          testCase "a first start does not offer components that are not ports"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              Expect.all
                  (WaveSimSelect.defaultSelectedWaves fs allWaves)
                  (fun wi -> fst wi.Id <> ComponentId 2)
                  "TOPNOT is on the top sheet but is not one of its ports"

          testCase "a selection the user already has is left alone"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              let mine = [ (Map.toList allWaves |> List.head |> fst) ]
              let ws = { ModelHelpers.initWSModel with AllWaves = allWaves; SelectedWaves = mine }
              Expect.equal
                  (WaveSimSelect.withDefaultSelectionIfEmpty fs ws).SelectedWaves
                  mine
                  "a chosen selection is never added to"

          testCase "no waves but a chosen RAM counts as a selection"
          <| fun () ->
              // Otherwise a user who wants only RAM contents gets a screenful of waveforms back
              // every time the simulation is refreshed.
              let fs, allWaves = simulation.Force()
              let ws =
                  { ModelHelpers.initWSModel with
                      AllWaves = allWaves
                      SelectedWaves = []
                      SelectedRams = Map [ (ComponentId 999, []), "RAM1" ] }
              Expect.isEmpty
                  (WaveSimSelect.withDefaultSelectionIfEmpty fs ws).SelectedWaves
                  "nothing is added when the user has selected a RAM"

          testCase "an empty selection is filled in"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              let ws = { ModelHelpers.initWSModel with AllWaves = allWaves; SelectedWaves = [] }
              Expect.isNonEmpty
                  (WaveSimSelect.withDefaultSelectionIfEmpty fs ws).SelectedWaves
                  "the viewer never opens empty when the top sheet has ports"

          testCase "a top sheet with no ports of its own falls back to the design's Viewers"
          <| fun () ->
              let fs, allWaves = closedSimulation.Force()
              let chosen =
                  WaveSimSelect.defaultSelectedWaves fs allWaves
                  |> List.map (fun wi -> WaveSimSelect.getName wi fs)
              // the Viewer is inside the instance, which is exactly the point: it is the one signal
              // the author of the design said was worth watching, and nothing on the top sheet is
              Expect.equal chosen [ "PROBE" ] "the Viewer in the subsheet"

          testCase "the fallback is only a fallback"
          <| fun () ->
              // the first design has both top-level ports and no Viewers to be distracted by, so
              // this only checks that having ports stops the search there
              let fs, allWaves = simulation.Force()
              Expect.all
                  (WaveSimSelect.defaultSelectedWaves fs allWaves)
                  (fun wi ->
                      match Map.tryFind wi.Id fs.WaveComps with
                      | Some fc -> fc.AccessPath = []
                      | None -> false)
                  "a design with top-level ports never reaches into its subsheets"

          //---------------------------------------------------------------------------------//
          // The collapsed hierarchy.
          //---------------------------------------------------------------------------------//

          testCase "the hierarchy has one node per sheet, not one per instance"
          <| fun () ->
              let _, hierarchy = hierarchyWith []
              Expect.equal
                  (hierarchy.HierOrder |> List.map (fun node -> node.NodeKey))
                  [ [ "deep" ]; midKey; leafKey ]
                  "three sheets, three nodes - though there are two mids and four leaves"

          testCase "a node offers the instances inside the one chosen above it"
          <| fun () ->
              let _, hierarchy = hierarchyWith []
              let mid = hierarchy.HierNodes[midKey]
              Expect.equal mid.NodeInstances.Length 2 "both instances of mid are on offer"
              let leaf = hierarchy.HierNodes[leafKey]
              Expect.equal leaf.NodeInstances.Length 2
                  "two leaves and not four: the ones inside the mid that is chosen"

              // choosing the other mid puts the other pair of leaves on offer, which is what makes
              // the chain a chain: the choices always name one real path through the hierarchy
              let _, other = hierarchyWith [ midKey, mid.NodeInstances[1] ]
              Expect.isEmpty
                  (Set.intersect
                       (Set.ofList leaf.NodeInstances)
                       (Set.ofList other.HierNodes[leafKey].NodeInstances))
                  "no leaf is inside both instances of mid"

          testCase "every node shows the alphabetically first instance until it is told otherwise"
          <| fun () ->
              let _, hierarchy = hierarchyWith []
              Expect.all hierarchy.HierOrder (fun node -> node.NodeInstances = List.sort node.NodeInstances)
                  "instances are offered in order"
              Expect.all hierarchy.HierOrder (fun node -> node.NodeInstance = List.tryHead node.NodeInstances)
                  "and the first of them is the one shown"

              let leaf = hierarchy.HierNodes[leafKey]
              let _, chosen = hierarchyWith [ leafKey, leaf.NodeInstances[1] ]
              Expect.equal chosen.HierNodes[leafKey].NodeInstance (Some leaf.NodeInstances[1])
                  "a recorded choice is what is shown"

          testCase "a choice the parent's choice has invalidated is ignored, not obeyed"
          <| fun () ->
              let _, hierarchy = hierarchyWith []
              let otherMid = hierarchy.HierNodes[midKey].NodeInstances[1]
              let _, underOther = hierarchyWith [ midKey, otherMid ]
              let leafOfOther = underOther.HierNodes[leafKey].NodeInstances[0]

              // the leaf is recorded, but the mid it lives in is not the one now chosen
              let _, stale = hierarchyWith [ leafKey, leafOfOther ]
              let leaf = stale.HierNodes[leafKey]
              Expect.notEqual leaf.NodeInstance (Some leafOfOther)
                  "an instance outside the chosen parent is not shown"
              Expect.equal leaf.NodeInstance (List.tryHead leaf.NodeInstances)
                  "the default is used instead, without anything having had to rewrite the entry"

          testCase "a node names the instance whose waves it stands for"
          <| fun () ->
              let fs, hierarchy = hierarchyWith []
              let _, allWaves = deepSimulation.Force()
              Expect.all
                  hierarchy.HierOrder
                  (fun node ->
                      match node.NodeInstance with
                      | Some instance -> List.contains instance (allInstances fs)
                      | None -> false)
                  "every node resolves to a sheet instance the simulation has"
              let leaf = hierarchy.HierNodes[leafKey]
              Expect.isNonEmpty
                  (allWaves |> Map.toList |> List.filter (fun (_, w) -> Some w.SheetId = leaf.NodeInstance))
                  "and the waves keyed by it are the ones the selector would list under it"

          testCase "opening one occurrence of a sheet closes the others"
          <| fun () ->
              let opened keys ws = WaveSimStyle.setWaveSheetSelectionOpen ws keys true
              let ws =
                  ModelHelpers.initWSModel
                  |> opened [ [ "top"; "a"; "shared" ] ]
                  |> opened [ [ "top"; "b"; "shared" ] ]
              Expect.equal (Set.toList ws.ShowSheetDetail) [ [ "top"; "b"; "shared" ] ]
                  "the occurrence of `shared` under `a` closed when the one under `b` opened"
              let ws = ws |> opened [ [ "top"; "b"; "other" ] ]
              Expect.equal
                  (Set.toList ws.ShowSheetDetail)
                  [ [ "top"; "b"; "other" ]; [ "top"; "b"; "shared" ] ]
                  "nodes of different sheets are open at the same time, which is the ordinary case"

          testCase "the sheet filter matches the sheet's name, not the instance's identity"
          <| fun () ->
              // Clicking a pill fills the filter box, and the box shows what it holds. An identity
              // is the path of labels down to an instance - MID2.LEAF1 - which is unique and which
              // no user wants to read out of a text box. The panel says which instance by its own
              // shape, so the filter only has to say which sheet.
              let fs, allWaves = deepSimulation.Force()
              Simulator.simCacheWS <- { Simulator.simCacheInit () with FastSim = fs }
              let ws = { ModelHelpers.initWSModel with AllWaves = allWaves }
              let hierarchy = WaveSimHierarchy.getSelectorHierarchy fs ws
              let shown = hierarchy.HierOrder |> List.choose (fun node -> node.NodeInstance) |> Set.ofList

              let filtered =
                  WaveSimSelectHelpers.filterWaves (Some shown) { ws with SheetSearchString = "LEAF" }
              Expect.isNonEmpty filtered.OfSheet "the sheet is found by the name the user gave it"
              Expect.all
                  filtered.OfSheet
                  (fun wave -> fs.getSheetNameOfInstance wave.SheetId = "leaf")
                  "and only that sheet's waves come back"

              // the identity is no longer what the box is matched against
              let byIdentity =
                  WaveSimSelectHelpers.filterWaves (Some shown) { ws with SheetSearchString = "MID1.LEAF1" }
              Expect.isEmpty byIdentity.OfSheet "a path through the hierarchy is not what this box takes"

              Simulator.simCacheWS <- Simulator.simCacheInit ()

          testCase "Show Only Selected reaches a wave chosen in an instance no node is showing"
          <| fun () ->
              // The one mode the collapsed hierarchy cannot narrow: a wave already chosen inside an
              // instance no combo box is currently showing has to stay reachable, or it could never
              // be deselected. So it is bounded by the SELECTION instead, which is what keeps it off
              // the expansion - reading every wave in AllWaves and every key of SimSheetStructure
              // made one click on the checkbox cost the whole of a design that multiplies out.
              let fs, allWaves = deepSimulation.Force()
              Simulator.simCacheWS <- { Simulator.simCacheInit () with FastSim = fs }
              let ws = { ModelHelpers.initWSModel with AllWaves = allWaves }
              let hierarchy = WaveSimHierarchy.getSelectorHierarchy fs ws
              let shown = hierarchy.HierOrder |> List.choose (fun node -> node.NodeInstance) |> Set.ofList
              Expect.isFalse
                  (Set.contains (instanceNamed fs "MID2.LEAF2") shown)
                  "the node for `leaf` shows an instance under MID1, so this one is not on show"

              let hidden =
                  allWaves
                  |> Map.toList
                  |> List.map snd
                  |> List.filter (fun wave -> wave.SheetId = instanceNamed fs "MID2.LEAF2")
              Expect.isNonEmpty hidden "the design has waves in that instance"
              let chosen =
                  { ws with
                      SelectedWaves = hidden |> List.map (fun wave -> wave.WaveId)
                      ShowOnlySelected = true }

              Expect.isEmpty
                  (WaveSimSelectHelpers.filterWaves (Some shown) chosen).OfSheet
                  "no node is showing that instance, so its waves are not among the rows drawn from them"

              let filtered = WaveSimSelectHelpers.filterWaves None chosen
              Expect.equal
                  (filtered.OfSheet |> List.map (fun wave -> wave.WaveId) |> List.sort)
                  (hidden |> List.map (fun wave -> wave.WaveId) |> List.sort)
                  "Show Only Selected gives back exactly the waves that were chosen"
              Expect.equal
                  (Set.toList filtered.Sheets)
                  [ instanceNamed fs "MID2.LEAF2" ]
                  "and only the instances they are in, not every sheet instance in the design"

              Simulator.simCacheWS <- Simulator.simCacheInit ()

          testCase "the waves of one row all sit at one depth, which is what its checkbox selects"
          <| fun () ->
              // A row of the signal list is the waves of one component group within one sheet
              // INSTANCE, and a sheet's row is the waves of that instance - so every wave a checkbox
              // covers shares an access path, and ticking the row means taking all of it. The rows
              // could once hold a sheet's whole subtree, which is why the code that ticks one kept
              // only the waves at the shallowest depth it found.
              let fs, allWaves = deepSimulation.Force()
              let waves = allWaves |> Map.toList |> List.map snd
              let depthOf (wave: ModelType.Wave) = fs.WaveComps[wave.WaveId.Id].AccessPath.Length

              Expect.isGreaterThan
                  (waves |> List.map depthOf |> List.distinct |> List.length)
                  1
                  "the design nests, so its waves are not all at one depth to begin with"

              // How makeNodeRows and makeFlatGroupRow between them divide an instance up: the
              // instance's own row, and then one row per group of components in it.
              let rowsOfWaves =
                  waves
                  |> List.groupBy (fun wave -> wave.SheetId)
                  |> List.collect (fun (_, ofInstance) ->
                      ofInstance
                      :: (ofInstance
                          |> List.groupBy (fun w -> WaveSimHelpers.getCompGroup fs.WaveComps[w.WaveId.Id].FType)
                          |> List.map snd))
              Expect.isNonEmpty rowsOfWaves "the design has rows"
              Expect.all
                  rowsOfWaves
                  (fun row -> row |> List.map depthOf |> List.distinct |> List.length = 1)
                  "and the waves of each row are all at one depth"

          testCase "an instance labelled with the top sheet's name is still a separate instance"
          <| fun () ->
              // A custom component on the top sheet labelled with that sheet's own name. While an
              // instance was identified by the path of LABELS down to it, these two read as the
              // same thing, and sharing an identity made them one instance to everything
              // downstream - one group of waves, one node of the selector. It took a deliberate
              // case-folding hack to keep them apart.
              //
              // An instance is now the path of component IDS, which cannot collide: the top sheet
              // is the empty path and the component introduces a one-element one. What collides
              // now is only how they READ, which costs nothing.
              let fs, allWaves = clashSimulation.Force()
              let instances = allInstances fs
              Expect.equal (List.length instances) 2 "there are two instances"
              Expect.equal (List.length (List.distinct instances)) 2
                  "and they have identities of their own"
              Expect.equal (instanceNames fs |> List.distinct |> List.length) 1
                  "even though they read as the same thing"

              let instancesOfWaves =
                  allWaves |> Map.toList |> List.map (fun (_, wave) -> wave.SheetId) |> List.distinct
              Expect.equal (List.length (List.distinct instancesOfWaves)) 2
                  "and the waves of each are grouped under their own"
              Expect.all
                  instancesOfWaves
                  (fun instance -> fs.getSheetNameOfInstance instance = "clash"
                                   || fs.getSheetNameOfInstance instance = "inner")
                  "each still knows which sheet the user drew"

          testCase "a library sheet is not a node of the hierarchy the selector draws"
          <| fun () ->
              // Nothing inside a library component is offered as a waveform, so a node for it would
              // be a node with nothing to choose under it. The hierarchy is built from the design
              // the SIMULATION holds, so what marks a sheet as a library one has to survive into
              // it - it is read off the LoadedComponents saveStateInSimulation kept.
              //
              // The two designs differ in nothing but that mark, so the second says the first is
              // hiding the sheet rather than never having had it.
              Expect.equal
                  (nodesUsing plainInnerSheet)
                  [ [ "useslib" ]; [ "useslib"; "lib" ] ]
                  "an ordinary subsheet is a node of its own"
              Expect.equal
                  (nodesUsing libSheet)
                  [ [ "useslib" ] ]
                  "the same sheet marked as a library component is not"

          testCase "the viewer is sized by the rows it draws, not by the selection"
          <| fun () ->
              // A selected wave AllWaves no longer holds is dropped from every column, since all
              // three are built from selectedWaves. Anything sized or hit-tested by the selection's
              // own length therefore disagreed with them by a row - which is how the tooltip on a
              // waveform came to answer for the waveform above it.
              let _, allWaves = deepSimulation.Force()
              let chosen = allWaves |> Map.toList |> List.map fst |> List.truncate 3
              Expect.equal (List.length chosen) 3 "the design has waves to select"
              let stale = { chosen[0] with PortNumber = chosen[0].PortNumber + 100 }
              Expect.isFalse (Map.containsKey stale allWaves) "and the stale one is not among them"

              let ws =
                  { ModelHelpers.initWSModel with
                      AllWaves = allWaves
                      SelectedWaves = stale :: chosen }
              Expect.equal
                  (List.length (WaveSimStyle.selectedWaves ws))
                  3
                  "only the selected waves the simulation still has are drawn"
              Expect.equal
                  (WaveSimStyle.calcWaveformHeight ws)
                  (WaveSimStyle.calcWaveformHeight { ws with SelectedWaves = chosen })
                  "and the table is sized for those, not for a selection naming one it has not got"

          testCase "a sheet instance is identified by the labels down to it, and nothing invented"
          <| fun () ->
              // `deep` holds MID1 and MID2, each holding LEAF1 and LEAF2 - so four instances of
              // `leaf`, told apart by which mid they are in. The identity used to be the sheet name
              // with a disambiguator bolted on: `leaf:1`, or a bare ordinal where the labels shared
              // no distinguishing suffix. Nothing had to be invented for it.
              let fs, _ = deepSimulation.Force()
              Expect.equal
                  (instanceNames fs |> List.sort)
                  [ "DEEP"; "MID1"; "MID1.LEAF1"; "MID1.LEAF2"; "MID2"; "MID2.LEAF1"; "MID2.LEAF2" ]
                  "every instance reads as the path of custom component labels reaching it"
              Expect.all (instanceNames fs) (fun name -> not (name.Contains ":"))
                  "and none of them carries a disambiguator"

          testCase "a waveform is called sheet.component.port, with the sheet the user's name for it"
          <| fun () ->
              // The four leaves are four instances of one sheet, so each has its own identity -
              // MID1.LEAF1 and so on. That is which instance, not which sheet, and it was being
              // read out as the first component of every waveform's name.
              let _, allWaves = deepSimulation.Force()
              let names = allWaves |> Map.toList |> List.map (fun (_, w) -> w.ViewerDisplayName)
              Expect.isNonEmpty names "the design has waves"
              Expect.all names (fun name -> not (name.Contains ":"))
                  "no waveform is named by the disambiguation FastCreate needed to tell instances apart"
              Expect.isNonEmpty
                  (names |> List.filter (fun name -> name.ToUpper().StartsWith "LEAF."))
                  "a wave inside a leaf is named for the sheet the user drew, whichever instance it is in"
              // A top sheet Input or Output is its own port, so it is named sheet.port and has no
              // middle part; anything inside a sheet carries all three.
              Expect.all names (fun name -> name.Split('.').Length >= 2)
                  "every name starts with the sheet it is in"
              Expect.isNonEmpty
                  (names |> List.filter (fun name -> name.Split('.').Length >= 3))
                  "and a component inside a sheet is named sheet, component and port"

          testCase "an instance is offered by the label on the canvas, not by its run-time name"
          <| fun () ->
              // FastCreate names an instance by whatever suffix of its label tells it apart from
              // every other instance of that sheet in the design - so the two `mid` here, labelled
              // MID1 and MID2, are named "1" and "2", and a design whose labels share no
              // distinguishing suffix gets bare ordinals. Neither is something to show a user.
              let fs, _ = deepSimulation.Force()
              let labels = WaveSimHierarchy.instanceLabels fs
              let _, hierarchy = hierarchyWith []
              let labelsOf (node: WaveSimHierarchy.SelectorNode) =
                  node.NodeInstances
                  |> List.map (fun instance ->
                      Map.tryFind instance labels
                      |> Option.defaultValue (WaveSimHierarchy.pathKey instance))
                  |> List.sort
              Expect.equal (labelsOf hierarchy.HierNodes[midKey]) [ "MID1"; "MID2" ]
                  "the choice between the two mids is offered as what the user drew"
              Expect.equal (labelsOf hierarchy.HierNodes[leafKey]) [ "LEAF1"; "LEAF2" ]
                  "and so is the choice between the leaves inside whichever mid is chosen"

          testCase "a sheet several routes reach is drawn inside its parents, not at top level"
          <| fun () ->
              let hierarchy = forkHierarchy.Force()
              let nodeAt key = hierarchy.HierNodes[key]
              // both parents hold it, so it is in the hierarchy under each of them
              Expect.equal
                  (hierarchy.HierOrder
                   |> List.map (fun node -> node.NodeKey)
                   |> List.filter (fun key -> List.tryLast key = Some "shared"))
                  [ [ "fork"; "left"; "shared" ]; [ "fork"; "right"; "shared" ] ]
                  "one node per place `shared` is reached, and no more"
              Expect.all
                  (hierarchy.HierOrder |> List.filter (fun node -> List.tryLast node.NodeKey = Some "shared"))
                  (fun node -> node.NodeMultiRoute)
                  "each is drawn inside the parent that instantiates it"

          testCase "a sheet one route reaches is drawn at top level"
          <| fun () ->
              let hierarchy = forkHierarchy.Force()
              Expect.all
                  (hierarchy.HierOrder |> List.filter (fun node -> List.tryLast node.NodeKey <> Some "shared"))
                  (fun node -> not node.NodeMultiRoute)
                  "fork, left and right are each reached one way, so each is a flat top-level row"

          testCase "a leaf several routes reach is placed inside its parents but has no toggle"
          <| fun () ->
              // The case that separates the two flags. Reading placement off NodeCollapsible put a
              // shared leaf back at top level, once per parent, which is the duplication the flat
              // list exists to avoid.
              let hierarchy = forkHierarchy.Force()
              let shared = hierarchy.HierNodes[[ "fork"; "left"; "shared" ]]
              Expect.isTrue shared.NodeMultiRoute "two routes reach it, so its row is drawn nested"
              Expect.isFalse shared.NodeCollapsible "and it holds nothing, so there is nothing to open"

          testCase "the selector reads only the waves of the instances it is drawing"
          <| fun () ->
              // What makes the dialog affordable on a design that expands. `deep` has seven sheet
              // instances and the collapsed hierarchy draws three of them, so a filter that read
              // every wave would read more than twice what it can use - and the designs this was
              // written for are four orders of magnitude worse.
              let fs, allWaves = deepSimulation.Force()
              // filterWaves reaches the simulation through the wave-sim cache, as the dialog does
              Simulator.simCacheWS <- { Simulator.simCacheInit () with FastSim = fs }
              let ws = { ModelHelpers.initWSModel with AllWaves = allWaves }
              let hierarchy = WaveSimHierarchy.getSelectorHierarchy fs ws
              let shown = hierarchy.HierOrder |> List.choose (fun node -> node.NodeInstance) |> Set.ofList
              let filtered = WaveSimSelectHelpers.filterWaves (Some shown) ws

              let everyInstance =
                  allWaves |> Map.toList |> List.map (fun (_, w) -> w.SheetId) |> Set.ofList
              Expect.isTrue (Set.count shown < Set.count everyInstance)
                  "the design holds more instances than the dialog draws, which is what makes this matter"
              Expect.isNonEmpty filtered.All "the instances on show have waves"
              Expect.all filtered.All (fun wave -> Set.contains wave.SheetId shown)
                  "and no wave of any other instance is read"

              // Show Only Selected is the exception: a wave chosen inside an instance no combo box
              // is showing has to stay reachable, or it could never be deselected.
              let chosen = allWaves |> Map.toList |> List.map fst |> List.truncate 1
              let unrestricted =
                  WaveSimSelectHelpers.filterWaves
                      None { ws with ShowOnlySelected = true; SelectedWaves = chosen }
              Expect.equal (List.length unrestricted.All) 1
                  "the selected wave is found wherever in the design it was chosen"

              Simulator.simCacheWS <- Simulator.simCacheInit ()

          testCase "too many waveforms warns, and far too many is refused"
          <| fun () ->
              let verdicts = [ 0; 25; 50; 51; 100; 101 ] |> List.map WaveSimSelectHelpers.selectionVerdict
              Expect.equal
                  verdicts
                  [ WaveSimSelectHelpers.SelectionOk
                    WaveSimSelectHelpers.SelectionOk
                    WaveSimSelectHelpers.SelectionOk
                    WaveSimSelectHelpers.SelectionWarn
                    WaveSimSelectHelpers.SelectionWarn
                    WaveSimSelectHelpers.SelectionRefuse ]
                  "both limits are the number that may be selected, not the first that may not" ]
