module TestDrawBlockD4
open Elmish

open GenerateData
open TestDrawBlockD1.Circuit
open TestDrawBlockD1.Builder
open TestDrawBlockD2
open TestDrawBlock
open TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests
open SheetBeautifyHelpers
open SheetBeautifyD1
open SheetBeautifyD4

open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open BlockHelpers



module Circuit =
    
    /// generates random circuit without overlap
    let randCircuit ((comps: ComponentType list), (posGen: XYPos list)) =
        // let genRand = System.Random(seed)
        printfn $"comps len = {List.length comps}"
        printfn $"pos len = {List.length posGen}"
        if (List.length comps <> List.length posGen) 
        then failwithf "needs 1-1 mapping for component's pos in Random Circuit"

        /// Returns function to add a symbol to sheet
        let addSingleSym (num:int) comp (pos:XYPos) =
            addSym $"SYM{num}" comp pos.X pos.Y

        /// Adds all input symbols to sheet
        let placeSyms (sheet : Result<SheetT.Model, string>) : Result<SheetT.Model, string> =
            List.mapi2 addSingleSym comps posGen
            |> List.fold (fun s x -> x s) sheet

        let addWireToModel (sheet: SheetT.Model) (inId, outId)  =
            let wModel = sheet.Wire
            let newWire = 
                wModel
                |> BusWireUpdate.makeNewWire (InputPortId inId) (OutputPortId outId)
            Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire sheet
        

        /// Adds random wires input symbols to sheet
        let placeWires (sheet : SheetT.Model) : Result<SheetT.Model, string> =
            let portIdByType (portType: PortType) (sheet: SheetT.Model) : string array =
                sheet.Wire.Symbol.Ports
                |> Map.filter (fun _ port -> port.PortType = portType)
                |> mapKeys
            
            let inPorts = portIdByType PortType.Input sheet
            let outPorts = portIdByType PortType.Output sheet |> shuffleA
            let minLen = min (Array.length inPorts) (Array.length outPorts)
            
            
            Array.fold addWireToModel sheet (Array.zip (Array.take minLen inPorts) (Array.take minLen outPorts))
            |> Ok
            
        
        // Place all symbols
        // Connect with random wiring
        // random flips / rotates
        Ok initSheetModel
        |> placeSyms
        |> Result.bind placeWires
        |> getOkOrFail


module TestData =
    
    /// All components available for random selection
    let allComps : ComponentType array =
        [|   
            Input1 (1, None);
            Output 1;
            Viewer 1;
            IOLabel;
            NotConnected;
            BusCompare1 (1, 0u, "test");
            BusSelection (1, 0);
            Constant1 (1, 0, "test");
            Not;
            Decode4;
            GateN (GateComponentType.And, 2);
            GateN (GateComponentType.And, 10);
            Mux2;
            Mux4;
            Mux8;
            Demux2;
            Demux4;
            Demux8;
            NbitsAdder 1;
            NbitsAdderNoCin 1;
            NbitsAdderNoCout 1;
            NbitsAdderNoCinCout 1;
            NbitsXor (1, None);
            NbitsAnd 1;
            NbitsNot 1;
            NbitsOr 1;
            NbitSpreader 4;
            ctrlPathCC;  // Custom
            dataPathCC; // Custom
            MergeWires;
            SplitWire 1;
            MergeN 1;
            SplitN (1, [1], [0]);
            DFF;
            DFFE;
            Register 1;
            RegisterE 1;
            Counter 1;
            CounterNoLoad 1;
            CounterNoEnable 1;
            CounterNoEnableLoad 1;
            AsyncROM1 (exampleMem 1);
            ROM1 (exampleMem 1);
            AsyncRAM1 (exampleMem 1);
            RAM1 (exampleMem 1);
        |]

    /// Returns a random element from an array
    let randElem (arr: 'a array) =
        let len = Array.length arr
        let randIdx = random.Next(len)
        arr.[randIdx]

    /// Gets all combinations of length n from a list
    let rec combinations n list =
        match n, list with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> 
            let withX = combinations (k-1) xs |> List.map (fun ys -> x::ys)
            let withoutX = combinations k xs
            withX @ withoutX

    ///
    let nRandComps n =  
        combinations n (Array.toList allComps)
        |> fromList

    // let nRandPos n = 
    //     let posGen = randXY {
    //                             min = -400
    //                             step = 100
    //                             max = 400
    //                         }
    //     toList posGen
    let getTestRand n =
        (nRandComps n, randomPos)
        ||> product (fun compL posL -> (compL, posL))
        |> toArray
        |> shuffleA
        |> fromArray


        

    // /// Generates a list of n random components
    // let nRandComps (n:int) : ComponentType list =
    //     let getRandComp _ = randElem allComps

    //     [1..n]
    //     |> List.map getRandComp

    // let nRandXY (n:int) (xyMin, xyStep, xyMax) =
    //     let getRandXY _ = randXY {  
    //                                 min  = xyMin
    //                                 step = xyStep
    //                                 max  = xyMax
    //                              }   

    //     [1..n]
    //     |> List.map getRandXY


    // /// Generator of random number of random components.
    // /// `@param` minimum & maximum number of comps (inclusive)
    // let randCircuitData (cMin, cMax) (xyMin, xyStep, xyMax) =
    //     let n = random.Next(cMin, cMax+1)
    //     let compGen = 
    //         nRandComps n
    //         |> fromList
    //     let posGen =
    //         nRandXY n (xyMin, xyStep, xyMax)


    //     product (fun comp xy -> (comp, xy)) compGen posGen




module Tests =
    open Circuit
    open TestData
    open TestDrawBlockD3

    let genLC = randXY {min=(0.5); step=0.5; max=1}
    let testD1Circuit testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "Large circuit"
            firstSample
            genLC
            showTargetSheet
            (Some beautifySheetBasic)
            makeLargeCircuit
            (AssertFunc failOnAllTests)
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch
    
    let testD3circuit testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "General Test on complex circuit"
            firstSample
            test2Builder
            showTargetSheet
            (Some beautifySheetBasic)
            makeTest3Circuit
            (AssertFunc failOnAllTests)
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch


    let genTestRand = getTestRand 2
    let testRand testNum firstSample showTargetSheet dispatch =
        runTestOnSheets
            "random circuit"
            firstSample
            genTestRand
            showTargetSheet
            (Some beautifySheetBasic)
            randCircuit
            (AssertFunc failOnAllTests)
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum showTargetSheet dispatch

    /// List of tests available which can be run ftom Issie File Menu.
    /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
    let testsToRunFromSheetMenu : (string * (int -> int -> bool -> Dispatch<Msg> -> Unit)) list =
        // Change names and test functions as required
        // delete unused tests from list
        [
            "D1 - Random", testRand // RANDOM TEST (D1 beautify)
            "D2 - Random", fun _ _ _ _ -> printf "Test2" // RANDOM TEST (D2 beautify)
            "D3 - Random", fun _ _ _ _ -> printf "Test3" // RANDOM TEST (D3 beautify)
            "All - D1's test", testD1Circuit // Standard D1 TEST (all beautifies)
            "All - D2's test", fun _ _ _ _ -> printf "Test5" // Standard D2 TEST (all beautifies)
            "All - D3's test", testD3circuit // Standard D3 TEST (all beautifies)
            "All - Random", fun _ _ _ _ -> printf "Test5" // RANDOM TEST (all beautifies)
            "Toggle Beautify", fun _ _ _ _ -> printf "Beautify Toggled"
            "Next Test Error", fun _ _ _ _ -> printf "Next Error:" // Go to the nexterror in a test
        ]

    /// Display the next error in a previously started test
    let nextError (testName, testFunc) firstSampleToTest showTargetSheet dispatch =
        let testNum =
            testsToRunFromSheetMenu
            |> List.tryFindIndex (fun (name,_) -> name = testName)
            |> Option.defaultValue 0
        testFunc testNum firstSampleToTest showTargetSheet dispatch

    /// common function to execute any test.
    /// testIndex: index of test in testsToRunFromSheetMenu
    let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
        let name,func = testsToRunFromSheetMenu[testIndex] 
        printf "%s" name
        match name, model.DrawBlockTestState with
        | "Next Test Error", Some state ->
            nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) (state.TargetFunctionApplied) dispatch
        | "Next Test Error", None ->
            printf "Test Finished"
            ()
        | "Toggle Beautify", Some state -> 
            nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex) (not state.TargetFunctionApplied) dispatch
        | "Toggle Beautify", None ->
            printf "No test started"
            ()
        | _ ->
            func testIndex 0 true dispatch
    




