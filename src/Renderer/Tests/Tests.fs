open TestLib
open Expecto
open Analyser
open AnalyserTests

[<Tests>]
let portChecksTest =
    // The idea is that the checks in checkPortTypesAreConsistent should never
    // fail, since the user should not be able to make them fail.
    createTestList "portChecks"
        (fun inp -> match checkPortTypesAreConsistent inp,
                          checkPortsAreConnectedProperly inp with
                    | Some err, _ | _, Some err -> Some err
                    | None, None -> None
        )
        testCasesCheckPortsAreConnectedProperly

[<Tests>]
let cycleChecksTest =
    createTestList "cycleCheks"
        (fun (graph, connections) -> analyseGraph graph connections)
        testCasesAnalyseGraph

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
