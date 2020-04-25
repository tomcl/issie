open TestLib
open Expecto

let testCasesAnalyser = []

[<Tests>]
let preprocessorTest = createTestList "Analyser Tests" id testCasesAnalyser

[<EntryPoint>]
let main argv =
    TestLib.runTests ()
    0
