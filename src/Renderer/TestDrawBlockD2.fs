module TestDrawBlockD2
open GenerateData
open Elmish
open RotateScale
open CommonTypes
open SheetBeautifyHelpers
open SheetBeautifyD2

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Test Circuits---------------//
//--------------------------------------------------------------------------------------------------//

module D2 =
    open EEExtensions
    open Optics
    open Optics.Operators
    open DrawHelpers
    open Helpers
    open CommonTypes
    open ModelType
    open DrawModelType
    open Sheet.SheetInterface
    open GenerateData
    open TestDrawBlock.HLPTick3.Builder
    open TestDrawBlock.TestLib
    open TestDrawBlock.HLPTick3
    let random = System.Random()

    let reRouteWires (sheet: SheetT.Model) =
        let wireModel = sheet.Wire
        let updatedWireModel = 
            sheet.Wire.Wires 
            |> Map.toList
            |> List.map snd
            |> List.fold 
                (fun model wire -> 
                    let updatedWire = (BusWireRoute.smartAutoroute model wire)
                    let updatedWires = Map.add updatedWire.WId updatedWire model.Wires
                    { model with Wires = updatedWires }) 
                wireModel
        {sheet with Wire = updatedWireModel}
        

    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    let swapMuxInput (symLabel: string) (swap: bool) (sheet: SheetT.Model) = 
        let componentMap = sheet.Wire.Symbol.Symbols
        let findSymbolID map = 
                map
                |> Map.filter(fun _ (value:DrawModelType.SymbolT.Symbol) -> value.Component.Label = symLabel)
                |> Map.toList
        let compId = fst (findSymbolID componentMap).[0]
        let compMap = Map.add compId (Optic.set reversedInputPorts_ (Some swap) (Map.find compId componentMap)) componentMap
        Optic.set SheetT.symbols_ compMap sheet

    let randomComponentPosition (topLeft: XYPos) (bottomRight:XYPos) = 
        let randomCoordinate max min = 
            random.NextDouble() * (max-min) + min

        let X = randomCoordinate bottomRight.X topLeft.X
        let Y = randomCoordinate bottomRight.Y topLeft.Y
        {X = X; Y = Y}

    let D2StarterPositions = 
        let mux1PossibleSwaps = [true; false]
        let mux1PossibleFlips: SymbolT.FlipType list = [SymbolT.FlipType.FlipHorizontal; SymbolT.FlipType.FlipVertical]
        let mux2PossibleSwaps = [true; false]
        let mux2PossibleFlips = [SymbolT.FlipType.FlipHorizontal; SymbolT.FlipType.FlipVertical]
        let gateFlips = [SymbolT.FlipType.FlipHorizontal; SymbolT.FlipType.FlipVertical]

        mux1PossibleSwaps
        |> List.allPairs mux1PossibleFlips
        |> List.allPairs mux2PossibleSwaps
        |> List.map( fun (s2,(f1,s1)) -> (s2,f1,s1))
        |> List.allPairs mux2PossibleFlips
        |> List.map(fun (f2,(s2,f1,s1)) -> (f2,s2,f1,s1))
        |> List.allPairs gateFlips 
        |> List.map (fun (g,(f2,s2,f1,s1)) -> (g,f2,s2,f1,s1))
        |> fromList

    let getTestMetrics (sheet: SheetT.Model) : SheetT.Model=
        printf $"Pre correction metrics, wire crossings:{rightAngleSegCount sheet}"
        let updatedSheet = reRouteWires (sheetOrderFlip sheet)
        printf $"Post correction metrics, wires straightened:{(visibleWireRightAngles updatedSheet) - (visibleWireRightAngles sheet)},\
         wire crossings:{rightAngleSegCount updatedSheet}"
        updatedSheet

    /// demo test circuit consisting of all components neede from D2
    let makeD2StarterCircuit (data :SymbolT.FlipType * SymbolT.FlipType * bool * SymbolT.FlipType * bool) =
        let gateFlip, mux2Flip, mux2Swap, mux1Flip,mux1Swap = data
        let tmpFlip = flipSymbol 
        let tmpFlipResult label  flip sheet = if true then Ok (tmpFlip label flip sheet) else Error "Won't happen"
        let tmpSwap = swapMuxInput
        let tmpSwapResult symLabel sheet swap = if true then Ok (tmpSwap symLabel sheet swap) else Error "Won't happen"
        initSheetModel
        |> placeSymbol "MUX1" Mux2 {middleOfSheet with X = middleOfSheet.X - 100.0 ; Y = middleOfSheet.Y - 200.0} 
        |> Result.bind (placeSymbol "S1" (Input1((1,Some 1))) {middleOfSheet with X = middleOfSheet.X - 200.0 ; Y = middleOfSheet.Y - 30.0})
        |> Result.bind (placeSymbol "S2" (Input1(1,Some 1)) {middleOfSheet with X = middleOfSheet.X - 200.0 ; Y = middleOfSheet.Y + 46.0})
        |> Result.bind (placeSymbol "MUX2" Mux2 middleOfSheet)
        |> Result.bind (placeSymbol "G1" (GateN(And,2)) {middleOfSheet with X = middleOfSheet.X + 200.0 ; Y = middleOfSheet.Y - 200.0})
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2) )
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1) )
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0) )
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0) )
        |> Result.bind(tmpFlipResult "G1" gateFlip)
        |> Result.bind(tmpFlipResult "MUX2" mux2Flip)
        |> Result.bind(tmpSwapResult "MUX2" mux2Swap)
        |> Result.bind(tmpFlipResult "MUX1" mux1Flip)
        |> Result.bind(tmpSwapResult "MUX2" mux2Swap)
        |> Result.map (reRouteWires)
        |> Result.map(getTestMetrics)
        |> Result.map (reRouteWires)
        |> getOkOrFail


    let getOptionStarterPosition (topLeft: XYPos) (bottomRight: XYPos) (comb: SymbolT.FlipType * SymbolT.FlipType * bool * SymbolT.FlipType * bool) =
        let mux1 = randomComponentPosition topLeft bottomRight
        let mux2 = randomComponentPosition topLeft bottomRight
        let s1 = randomComponentPosition topLeft bottomRight
        let s2 = randomComponentPosition topLeft bottomRight
        let g1 = randomComponentPosition topLeft bottomRight

        // Combine the original comb values with the new values
        let newComb = 
            // Extract existing values for readability
            let (a, b, c, d, e) = comb
            // Create the new tuple with all values
            (a, b, c, d, e, mux1, mux2, s1, s2, g1)

        newComb
        


    let makeD2OptionCircuit (data :SymbolT.FlipType * SymbolT.FlipType * bool * SymbolT.FlipType * bool * XYPos * XYPos * XYPos * XYPos * XYPos) =
        let gateFlip, mux2Flip, mux2Swap, mux1Flip, mux1Swap, mux1, mux2, s1, s2, g1 = data
        let tmpFlip = flipSymbol 
        let tmpFlipResult label  flip sheet = if true then Ok (tmpFlip label flip sheet) else Error "Won't happen"
        let tmpSwap = swapMuxInput
        let tmpSwapResult symLabel sheet swap = if true then Ok (tmpSwap symLabel sheet swap) else Error "Won't happen"
        initSheetModel
        |> placeSymbol "MUX1" Mux2  mux1 
        |> Result.bind (placeSymbol "S1" (Input1((1,Some 1))) s1)
        |> Result.bind (placeSymbol "S2" (Input1(1,Some 1)) s2)
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2)
        |> Result.bind (placeSymbol "G1" (GateN(And,2)) g1)
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2) )
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1) )
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0) )
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0) )
        |> Result.bind(tmpFlipResult "G1" gateFlip)
        |> Result.bind(tmpFlipResult "MUX2" mux2Flip)
        |> Result.bind(tmpSwapResult "MUX2" mux2Swap)
        |> Result.bind(tmpFlipResult "MUX1" mux1Flip)
        |> Result.bind(tmpSwapResult "MUX2" mux2Swap)
        |> Result.map (reRouteWires)
        |> Result.map(getTestMetrics)
        |> Result.map (reRouteWires)
        |> getOkOrFail

    
    let D2OptionStarterPositions = 
        let topLeft = {middleOfSheet with X = middleOfSheet.X - 100.0 ; Y = middleOfSheet.Y - 200.0}
        let bottomRight = {middleOfSheet with X = middleOfSheet.X + 100.0 ; Y = middleOfSheet.Y + 200.0}
        D2StarterPositions
        |> toList
        |> List.map (getOptionStarterPosition topLeft bottomRight)
        |> List.filter(fun d -> (numOfIntersectedSymPairs (makeD2OptionCircuit d)) <= 0)
        |> fromList



//---------------------------------------------------------------------------------------//
//-----------------------------Tests-----------------------------------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

    /// Allow test errors to be viewed in sequence by recording the current error
    /// in the Issie Model (field DrawblockTestState). This contains all Issie persistent state.
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber=testNumber; LastTestSampleIndex= numb})
            
        /// Example test: Horizontally positioned AND + DFF: fail on sample 0
        let test1 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 0"
                firstSample
                D2StarterPositions
                makeD2StarterCircuit
                (Asserts.failOnSampleNumber 10)
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on sample 10
        let test2 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 10"
                firstSample
                D2StarterPositions
                makeD2StarterCircuit
                (Asserts.failOnSampleNumber 10)
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let test3 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on symbols intersect"
                firstSample
                D2StarterPositions
                makeD2StarterCircuit
                Asserts.failOnSymbolIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail all tests
        let test4 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail all tests"
                firstSample
                D2StarterPositions
                makeD2StarterCircuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let testD2Starter testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail all tests"
                firstSample
                D2OptionStarterPositions
                makeD2OptionCircuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch



        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testDropdown : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1", test1 // example
                "Test2", test2
                "Test3", test3
                "Test4", test4
                "Test5", testD2Starter// dummy test - delete line or replace by real test as needed
                "Test6", fun _ _ _ -> printf "Test6" 
                "Test7", fun _ _ _ -> printf "Test7"
                "Test8", fun _ _ _ -> printf "Test8"
                "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

            ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testDropdown
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testDropdown
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testDropdown[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testDropdown[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | _ ->
                func testIndex 0 dispatch



    


