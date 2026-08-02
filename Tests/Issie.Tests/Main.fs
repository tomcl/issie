module Main

open Expecto

[<EntryPoint>]
let main argv =
    // Sequenced: building a FastSimulation is not re-entrant (FastCreate.stepArrayIndex is
    // a module-level mutable), so tests that simulate cannot run in parallel
    testList "Issie" [
        Properties.tests
        AlgebraTests.tests
        ParameterScenarios.tests
        ComponentSemantics.tests
        GoldenModel.tests
        PersistenceTests.tests
        SheetDescriptionTests.tests
        DrawBlockTests.tests
        LibraryTests.tests
    ]
    |> runTestsWithCLIArgs [ Sequenced ] argv
