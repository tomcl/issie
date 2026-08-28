/// Comments written against locations in a .ram file, and their appearance on the waveform of a
/// ROM reading those locations.
///
/// A comment says what a word of memory is for. It is of no use in the memory viewer alone, where
/// it is only seen if the RAM display happens to be open at the right cycle - so it is shown when
/// hovering the ROM's data output, beside the value when that value is too narrow to be printed.
module RomComments

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasBuilder

let private maxArraySize = 100

/// Comments against two of the four locations, so that a location with none can be told apart from
/// one whose comment happens to be empty.
let private comments = Map [ 0I, "reset vector"; 2I, "loop target" ]

let private romMemory =
    { Init = FromData
      AddressWidth = 2
      WordWidth = 8
      Data = Map [ 0I, 10I; 1I, 20I; 2I, 30I; 3I, 40I ]
      Comments = Some comments }

/// A sheet holding one ROM of the given type, its address driven by an input and its data read by
/// an output, so that the ROM has a data output wave to hover.
let private romSheet (name: string) (romType: ComponentType) =
    let addr = makeComp 1 0 1 (Input1(2, None)) "ADDR"
    let rom = makeComp 2 1 1 romType "MEM"
    let out = makeComp 3 1 0 (Output 8) "DOUT"
    makeLdc name None ([ addr; rom; out ], [ conn addr 0 rom 0; conn rom 0 out 0 ])

/// Run a design for a few cycles with the address input held at `address`, and return the fast
/// simulation together with the ROM's data output wave.
let private simulateAt (ldc: LoadedComponent) (romId: int) (address: bigint) (ticks: int) =
    match Simulator.startCircuitSimulation maxArraySize ldc.Name ldc.CanvasState [ ldc ] with
    | Error e -> failwith $"Simulation setup failed: %A{e}"
    | Ok simData ->
        let fs = simData.FastSim
        simData.Inputs
        |> List.iter (fun (cid, _, width) ->
            FastExtract.changeInput cid (IData(NumberHelpers.convertBigintToFastData width address)) 0 fs)
        FastRun.runFastSimulation None ticks fs |> ignore

        // The comment is looked up from the address the ROM is reading, and that address is read
        // through WaveData like every other value the wave simulator shows - so the test wires it
        // to this simulation exactly as WaveProvider.selectSimulator does in the app. Reading the
        // FastSimulation directly, as this used to, is the one thing the code under test must not
        // do: it is what made these comments impossible to show when the simulator is the .NET one.
        WaveData.setLocal
            (fun (SignalHandle(DriverIndex i)) ->
                Array.tryItem i fs.Drivers
                |> Option.flatten
                |> Option.map (fun driver -> driver.DriverData))
            (fun () -> fs.ClockTick)

        let waves = TestFixtures.allWavesOf ModelHelpers.initWSModel fs
        let wave =
            waves
            |> Map.toList
            |> List.map snd
            |> List.find (fun w ->
                fst w.WaveId.Id = ComponentId romId && w.WaveId.PortType = PortType.Output)
        fs, wave

let tests =
    testList
        "RomComments"
        [ testCase "an asynchronous ROM reads the comment for the address it is given"
          <| fun () ->
              let ldc = romSheet "asyncrom" (AsyncROM1 romMemory)
              let fs, wave = simulateAt ldc 2 2I 3
              Expect.equal
                  (EvilHoverCache.getRomCommentAtStep fs 2 wave)
                  "loop target"
                  "an asynchronous ROM reads at the step being shown"

          testCase "a synchronous ROM reads the comment for the address of the step before"
          <| fun () ->
              // its output at a step is the word addressed at the step before, so that is the
              // location whose comment belongs beside it
              let ldc = romSheet "syncrom" (ROM1 romMemory)
              let fs, wave = simulateAt ldc 2 0I 3
              Expect.equal
                  (EvilHoverCache.getRomCommentAtStep fs 2 wave)
                  "reset vector"
                  "read one step behind"
              Expect.equal
                  (EvilHoverCache.getRomCommentAtStep fs 0 wave)
                  ""
                  "and nothing at all at step 0, which has no step before it"

          testCase "a location with no comment gives none"
          <| fun () ->
              let ldc = romSheet "asyncrom" (AsyncROM1 romMemory)
              let fs, wave = simulateAt ldc 2 1I 3
              Expect.equal (EvilHoverCache.getRomCommentAtStep fs 2 wave) "" "address 1 is not commented"

          testCase "a ROM whose file had no comments gives none"
          <| fun () ->
              let ldc = romSheet "asyncrom" (AsyncROM1 { romMemory with Comments = None })
              let fs, wave = simulateAt ldc 2 0I 3
              Expect.equal (EvilHoverCache.getRomCommentAtStep fs 2 wave) "" "no comments map at all"

          testCase "the address input's own wave carries no comment"
          <| fun () ->
              // only the data output is read from the memory, so only it has anything to say
              let ldc = romSheet "asyncrom" (AsyncROM1 romMemory)
              let fs, _ = simulateAt ldc 2 0I 3
              let inputWave =
                  TestFixtures.allWavesOf ModelHelpers.initWSModel fs
                  |> Map.toList
                  |> List.map snd
                  |> List.find (fun w ->
                      fst w.WaveId.Id = ComponentId 2 && w.WaveId.PortType = PortType.Input)
              Expect.equal (EvilHoverCache.getRomCommentAtStep fs 2 inputWave) "" "an input port is not a read" ]
