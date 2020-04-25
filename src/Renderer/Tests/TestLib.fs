module TestLib

open Expecto

/// Create a testcase for unit testing a function.
let testFunction f testData =
    let descr, inp, expectedOut = testData
    testCase descr <| fun () ->
        let actual = f inp
        Expect.equal actual expectedOut ""

/// Create an expecto testList from a list of testCases.
let createTestList descr f testCases =
    testList descr <| List.map (testFunction f) testCases

/// Run all the previously defined tests.
let runTests () =
    runTestsInAssembly defaultConfig [||] |> ignore
