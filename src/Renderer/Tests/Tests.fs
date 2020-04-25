open TestLib
open Expecto
open Analyser
open AnalyserTests

[<Tests>]
let portsTest =
    // The idea is that the checks in checkPortTypesAreConsistent should never
    // fail, since the user should not be able to make them fail.
    createTestList "checkPortsAreConnectedProperly"
        (fun inp -> match checkPortTypesAreConsistent inp,
                          checkPortsAreConnectedProperly inp with
                    | Some err, _ | _, Some err -> Some err
                    | None, None -> None
        )
        testCasesCheckPortsAreConnectedProperly

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
