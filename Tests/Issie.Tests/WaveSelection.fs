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
open CanvasBuilder

let private maxArraySize = 100

/// A sheet holding an input, a NOT gate and an output, so that it has both a component with waves
/// of its own and I/O whose waves are on the instance above.
let private notSheet (name: string) (inLabel: string) (outLabel: string) =
    let i = makeComp $"{name}-in" 0 1 (Input1(1, None)) inLabel
    let n = makeComp $"{name}-not" 1 1 Not "N"
    let o = makeComp $"{name}-out" 1 0 (Output 1) outLabel
    makeLdc name None ([ i; n; o ], [ conn i 0 n 0; conn n 0 o 0 ])

let private solo = notSheet "solo" "A" "Y"
let private twin = notSheet "twin" "B" "Z"

/// Top sheet: solo instantiated once and twin instantiated twice, chained from IN to OUT.
let private top =
    let inp = makeComp "top-in" 0 1 (Input1(1, None)) "IN"
    let topNot = makeComp "top-not" 1 1 Not "TOPNOT"
    let solo1 = makeComp "top-solo1" 1 1 (customOf solo [ "A", 1 ] [ "Y", 1 ] None) "SOLO1"
    let twin1 = makeComp "top-twin1" 1 1 (customOf twin [ "B", 1 ] [ "Z", 1 ] None) "TWIN1"
    let twin2 = makeComp "top-twin2" 1 1 (customOf twin [ "B", 1 ] [ "Z", 1 ] None) "TWIN2"
    let out = makeComp "top-out" 1 0 (Output 1) "OUT"
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
    let i = makeComp "probed-in" 0 1 (Input1(1, None)) "A"
    let n = makeComp "probed-not" 1 1 Not "N"
    let v = makeComp "probed-view" 1 0 (Viewer 1) "PROBE"
    let o = makeComp "probed-out" 1 0 (Output 1) "Y"
    makeLdc "probed" None ([ i; n; v; o ], [ conn i 0 n 0; conn n 0 v 0; conn n 0 o 0 ])

/// Top sheet with no Input, Output or Viewer on it: a constant drives an instance, whose output is
/// terminated rather than brought out.
let private closed =
    let c = makeComp "closed-const" 0 1 (Constant1(1, 1I, "1")) "C1"
    let inst = makeComp "closed-inst" 1 1 (customOf probed [ "A", 1 ] [ "Y", 1 ] None) "PROBED1"
    let nc = makeComp "closed-nc" 1 0 NotConnected "NC1"
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
let private offered (compId: string) =
    let fs, allWaves = simulation.Force()
    WaveSimSelect.wavesOfComponent fs allWaves (ComponentId compId)

let tests =
    testList
        "WaveSelection"
        [ testCase "a component on the top sheet is offered its own ports"
          <| fun () ->
              let waves, copies = offered "top-not"
              Expect.equal copies 1 "one copy of a component on the simulated top sheet"
              Expect.isNonEmpty waves "the gate's ports are offered"
              Expect.all
                  waves
                  (fun w -> w.WaveId.Id = (ComponentId "top-not", []))
                  "the waves are the component's own, at the root of the simulation"

          testCase "a component in a sheet instantiated once is offered its own ports"
          <| fun () ->
              let waves, copies = offered "solo-not"
              Expect.equal copies 1 "one copy of a component in a singly instantiated sheet"
              Expect.all
                  waves
                  (fun w -> snd w.WaveId.Id = [ ComponentId "top-solo1" ])
                  "the waves are inside the one instance"

          testCase "a component in a sheet instantiated twice is offered nothing"
          <| fun () ->
              let waves, copies = offered "twin-not"
              Expect.equal copies 2 "one copy per instantiation of the sheet"
              Expect.isEmpty waves "nothing offered: the canvas symbol is neither copy"

          testCase "an Input in a subsheet is offered the instance port carrying its signal"
          <| fun () ->
              let waves, copies = offered "solo-in"
              Expect.equal copies 1 "one copy of the Input itself"
              match waves with
              | [ wave ] ->
                  Expect.equal
                      wave.WaveId.Id
                      (ComponentId "top-solo1", [])
                      "the wave is on the instance the sheet sits in, not on the Input"
                  Expect.equal wave.WaveId.PortType PortType.Input "an Input maps to an input port"
                  Expect.equal wave.WaveId.PortNumber 0 "the port A is the instance's first input"
                  Expect.equal wave.PortLabel "A" "and is named after the Input, as the viewer names it"
              | waves -> failtestf "expected one wave for the subsheet Input, got %d" waves.Length

          testCase "an Output in a subsheet is offered the instance port carrying its signal"
          <| fun () ->
              let waves, _ = offered "solo-out"
              match waves with
              | [ wave ] ->
                  Expect.equal wave.WaveId.Id (ComponentId "top-solo1", []) "the wave is on the instance"
                  Expect.equal wave.WaveId.PortType PortType.Output "an Output maps to an output port"
                  Expect.equal wave.PortLabel "Y" "named after the Output"
              | waves -> failtestf "expected one wave for the subsheet Output, got %d" waves.Length

          testCase "an Input in a sheet instantiated twice is offered nothing"
          <| fun () ->
              let waves, copies = offered "twin-in"
              Expect.equal copies 2 "one copy per instantiation"
              Expect.isEmpty waves "the redirect is not reached: there is no single instance to redirect to"

          testCase "a component outside the simulated design is offered nothing"
          <| fun () ->
              let waves, copies = offered "not-in-this-simulation"
              Expect.equal copies 0 "no copies of a component the simulation does not hold"
              Expect.isEmpty waves "and so nothing to offer"

          // Hovering a wave's name highlights the wires carrying it, which are found by the sheet
          // the wave's component is on. A subsheet is reached through a custom component instance
          // whose LABEL is whatever the person who placed it chose - here SOLO1, for the sheet
          // named solo - so taking the sheet's name from that label finds no sheet, and no wires.
          testCase "a wave in a subsheet knows its sheet by the instance's type, not its label"
          <| fun () ->
              let fs, allWaves = simulation.Force()
              match fst (WaveSimSelect.wavesOfComponent fs allWaves (ComponentId "solo-not")) with
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
              match fst (WaveSimSelect.wavesOfComponent fs allWaves (ComponentId "top-not")) with
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
                  (fun wi -> fst wi.Id <> ComponentId "top-not")
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
                      SelectedRams = Map [ (ComponentId "someRam", []), "RAM1" ] }
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
                  "a design with top-level ports never reaches into its subsheets" ]
