module Main

open Expecto

[<EntryPoint>]
let main argv =
    // Sequenced: building a FastSimulation is not re-entrant (FastCreate.stepArrayIndex is
    // a module-level mutable), so tests that simulate cannot run in parallel
    testList "Issie" [
        PathHelperTests.tests
        Properties.tests
        AlgebraTests.tests
        NumberHelpersTests.tests
        TruthTableSimTests.tests
        ParameterScenarios.tests
        ComponentSemantics.tests
        GoldenModel.tests
        ListPairsTests.tests
        WidthInferenceTests.tests
        PersistenceTests.tests
        SheetDescriptionTests.tests
        DrawBlockTests.tests
        MarkdownTests.tests
        LibraryTests.tests
        SheetHierarchy.tests
        ReadOnlySheetTests.tests
        ParameterUI.tests
        MemoryParameters.tests
        InstanceSignatures.tests
        WaveSelection.tests
        CustomOutputExtraction.tests
        SimulationBudget.tests
        RomComments.tests
        RamStoreTests.tests
        RamBenchmark.tests
        KeyBindingTests.tests
        VerilogOutput.tests
        SourceHygiene.tests
        // The VerilogCompiler group spawns node for every parse (the real nearley parser) and
        // takes most of the suite's runtime, so it runs locally only, not on CI runners.
        // GitHub Actions (and most CI systems) set CI=true.
        if System.String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable "CI") then
            VerilogCompiler.tests
    ]
    |> runTestsWithCLIArgs [ Sequenced ] argv
