module Main

open Expecto

/// Errors the suite is supposed to produce: `ExceptionBuffer` logs some on purpose, because
/// what it tests is the machinery that records them. Everything else is a finding.
let private expectedErrors = [ ExceptionBuffer.tag ]

[<EntryPoint>]
let main argv =
    // **A test run that logs an error fails, whether or not any assertion noticed.**
    //
    // `Log.error` means Issie was asked to do something and did not - the same claim in a test
    // run as in the application, and the same thing to do about it. Without this a test could
    // pass its assertions while the code under it quietly reported a failure, which is exactly
    // the "occasional errors nobody understands" this is all meant to prevent. It uses the hook
    // the renderer uses to put the error on screen, so the two cannot drift apart.
    //
    // An uncaught exception needs nothing here: Expecto already fails the test it escapes from.
    let logged = ResizeArray<string>()

    Log.onErrorLogged <-
        Some(fun text ->
            if not (expectedErrors |> List.exists text.Contains) then
                logged.Add text)

    // Sequenced: building a FastSimulation is not re-entrant (FastCreate.stepArrayIndex is
    // a module-level mutable), so tests that simulate cannot run in parallel
    testList "Issie" [
        PathHelperTests.tests
        RecentProjects.tests
        Properties.tests
        AlgebraTests.tests
        NumberHelpersTests.tests
        TruthTableSimTests.tests
        ParameterScenarios.tests
        ArraySheets.tests
        SheetIdentity.tests
        ComponentSemantics.tests
        GoldenModel.tests
        LookupArrayTests.tests
        ListPairsTests.tests
        WidthInferenceTests.tests
        PersistenceTests.tests
        SimpleDesignTests.tests
        SheetDescriptionTests.tests
        DrawBlockTests.tests
        WireQuality.tests
        PasteArrayGeometry.tests
        MarkdownTests.tests
        LibraryTests.tests
        SheetHierarchy.tests
        ReadOnlySheetTests.tests
        ParameterUI.tests
        MemoryParameters.tests
        ComponentSlotTests.tests
        InstanceSignatures.tests
        WaveSelection.tests
        CustomOutputExtraction.tests
        SimulationBudget.tests
        StaleSheetName.tests
        RomComments.tests
        RamStoreTests.tests
        RamBenchmark.tests
        SimBenchmark.tests
        KeyBindingTests.tests
        VerilogOutput.tests
        ExceptionBuffer.tests
        SourceHygiene.tests
        CarrierTests.tests
        // The VerilogCompiler group spawns node for every parse (the real nearley parser) and
        // takes most of the suite's runtime, so it runs locally only, not on CI runners.
        // GitHub Actions (and most CI systems) set CI=true.
        if System.String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable "CI") then
            VerilogCompiler.tests
    ]
    |> runTestsWithCLIArgs [ Sequenced ] argv
    |> fun exitCode ->
        // after the run, so the list is complete and so this cannot be mistaken for one test
        // failing - a logged error is a fact about the whole run
        if logged.Count = 0 then
            exitCode
        else
            printfn ""
            printfn "%d error(s) were logged during this run. Every one is a bug:" logged.Count
            logged |> Seq.iter (printfn "  %s")
            printfn ""
            printfn "Log.error means Issie did not do what it was asked. Fix the cause, or - if the"
            printfn "outcome was right after all - the call should have been Log.warn."
            max exitCode 1
