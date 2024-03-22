module TestDrawBlock
open TestConfiguration
open System.Text.RegularExpressions
open TestDrawBlockHelpers.SimpleSymbol
open Elmish

module TestingFramwork =
    open Optics
    open CommonTypes
    open ModelType
    open DrawModelType

    module SimpleSymbolTestBuilders =
        open System
        open DrawModelType.SymbolT
        open DrawModelType.SheetT
        open Operators
        open SheetBeautifyHelpers

        let rand = Random()

        let createRandomCustomComponent numInputs =
            {
               Name = "Custom"
               InputLabels = List.init numInputs (fun i -> sprintf "In%d" i, 1)
               OutputLabels = ["Out", 1]
               Form = None
               Description = None
            }


        let portInfoByComponentType (compType: ComponentType) : (int * int) = // (numInputs, numOutputs)
            match compType with
            | GateN(_, numInputs) -> (numInputs, 1)
            | Mux2 -> (2, 1)
            | Mux4 -> (4, 1)
            | Mux8 -> (8, 1)
            | Demux2 -> (1, 2)
            | Demux4 -> (1, 4)
            | Demux8 -> (1, 8)
            | DFF
            | DFFE -> (1, 1) // Simplification !!
            | _ -> (0, 0)

        let randomGateComponentType () =
            [| And; Or; Xor; Nand; Nor; Xnor |]
            |> Array.item (Random().Next(6))

        let randomMuxDemuxType () =
            let types = [| Mux2; Mux4; Mux8; Demux2; Demux4; Demux8 |]
            // Mux8; Demux2; Demux4; Demux8
            types.[Random().Next(types.Length)]

        let randomFlipFlopType () =
            [| DFF; DFFE |] |> Array.item (Random().Next(2))

        let randomComponentType () : ComponentType =
            let choice = Random().Next(100)
            match choice with
            | n when n < 50 -> // 50% for GateN
                GateN(randomGateComponentType (), Random().Next(1, 5)) // Randomly selecting between 1 to 4 inputs
            | n when n < 80 -> // 30% for Mux/Demux
                randomMuxDemuxType ()
            | n when n < 90 -> // 10% for FlipFlop
                randomFlipFlopType ()
            | _ -> // 10% for FlipFlop
                // randomFlipFlopType ()
                Custom (createRandomCustomComponent (Random().Next(1, 4)))

        let randomFlipType () =
            let flips = [| FlipHorizontal; FlipVertical |]
            flips.[rand.Next(flips.Length)]

        let defineGridParameters (gridLength: float) (numberOfColumns: int) : float list =
            let columnWidth = gridLength / float numberOfColumns
            List.init numberOfColumns (fun i -> (float i + 0.5) * columnWidth)

        let randomPositionInColumn (gridLength: float) (columnXPositions: float list) : XYPos =
            let columnIndex = rand.Next(columnXPositions.Length)
            let xPosition = columnXPositions.[columnIndex]
            let yPosition = rand.NextDouble() * gridLength
            { X = xPosition; Y = yPosition }

        let generateRandomPositionInColumn (maxCoord: float) (numberOfColumns: int) : XYPos =
            let columnXPositions = defineGridParameters maxCoord numberOfColumns
            randomPositionInColumn maxCoord columnXPositions

        let deviatePos (minDev: float) (maxDev: float) (pos: XYPos) =
            let rand = System.Random()
            let deviate =  minDev + (maxDev - minDev) * rand.NextDouble()
            { X = pos.X + deviate; Y = pos.Y + deviate }


        let getGridPositions (gridDimension: XYPos) (numberOfColumns: int) (numberOfRows: int) =
            let xPositions = defineGridParameters gridDimension.X numberOfColumns
            let yPositions = defineGridParameters gridDimension.Y numberOfRows
            List.collect (fun y -> List.map (fun x -> { X = x; Y = y }) xPositions) yPositions


        let randomSTransform () =
            { Rotation =
                match Random().Next(4) with
                | 0 -> Degree0
                | 1 -> Degree90
                | 2 -> Degree180
                | _ -> Degree270
              Flipped =
                Random().Next(2) = 0 }


        let createRandomSimpleSymbol (id: int) (maxCoord: float) (numberOfColumns: int) : SimpleSymbol =
            let compType = randomComponentType ()
            let pos = (generateRandomPositionInColumn maxCoord numberOfColumns)
            createSimpleSymbol (sprintf "Comp%d" id) compType pos (randomSTransform ())


        let getRandomPort (comp: ComponentType) (portType: PortType) : int =
            let inputs, outputs = portInfoByComponentType comp
            match portType with
            | PortType.Output -> rand.Next(outputs)
            | PortType.Input -> rand.Next(inputs)

        let createRandomSimpleConnection source target =
            { Source = { Label = source.SymLabel; PortNumber = getRandomPort source.CompType PortType.Output }
              Target = { Label = string target.SymLabel; PortNumber = getRandomPort target.CompType PortType.Input } }

        let makeConnections (components: SimpleSymbol list) =
            List.zip components (List.tail components)
            |> List.map (fun (source, target) -> createRandomSimpleConnection source target)


        let rec generateAndConnectComponents (numComponents: int) (maxCoord: float) (numberOfColumns: int) : TestModel =
            let components =
                List.init numComponents (fun id -> createRandomSimpleSymbol id maxCoord numberOfColumns)
            let connections = makeConnections components
            let testModel = { SimpleSymbols = components; Connections = connections }

            let sheetModel =
                try Builder.placeTestModel testModel
                with
                | _ -> failwith "Error placing test model on sheet."

            if numOfIntersectedSymPairs sheetModel > 0 then
                generateAndConnectComponents numComponents maxCoord numberOfColumns
            else
                testModel


        let getRandomSymbol (simSymbols: SimpleSymbol list) =
            List.item (rand.Next(simSymbols.Length)) simSymbols

        let createRandomConnection (simSymbols: SimpleSymbol list) =
            let source = getRandomSymbol simSymbols
            let target = getRandomSymbol simSymbols
            createRandomSimpleConnection source target

        let createNRandomConnections (simSymbols: SimpleSymbol list) (n: int) =
            List.init n (fun _ -> createRandomConnection simSymbols)


        let updateConnections (newConnections: SimpleConnection list) (model: TestModel)  =
            let removeIllegalConnections (connections: SimpleConnection list) (newConnections: SimpleConnection list) =
                connections @ newConnections
                |> List.distinctBy (fun c -> (c.Target))

            Optic.map connections_ (fun connections -> removeIllegalConnections connections newConnections) model

        let rec buildConstrainedCircuit (minDev: float) (maxDev: float) (numberOfRows: int) (numberOfColumns: int) (gridDimension: XYPos) : Model =
            let gridPositions = getGridPositions gridDimension numberOfColumns numberOfRows
            let components =
                gridPositions
                |> List.map (deviatePos minDev maxDev)
                |> List.mapi (fun i pos -> createSimpleSymbol (sprintf "Comp%d" i) (randomComponentType ()) pos {Rotation = Degree0; Flipped = false})

            components
            |> (fun components -> createTestModel components (makeConnections components))
            |> updateConnections (createNRandomConnections components (components.Length / 2))
            |> Builder.placeTestModel
            |> (fun sheet ->
                match SheetBeautifyHelpers.numOfIntersectedSymPairs sheet  with
                | 0 -> sheet
                | _ -> buildConstrainedCircuit minDev maxDev numberOfRows numberOfColumns gridDimension
            )

        let buildTestCircuit (numComponents: int) (maxCoord: float) (numberOfColumns: int) : SheetT.Model =
            let testModel = generateAndConnectComponents numComponents maxCoord numberOfColumns
            let sheetModel = Builder.placeTestModel testModel
            sheetModel


    //------------------------------------------------------------------------------------------------------------------------//
    //------------------------------functions to build issue schematics programmatically--------------------------------------//
    //------------------------------------------------------------------------------------------------------------------------//
    module Builder =


        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map
                busWireModel_
                (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch
            <| UpdateModel(Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
            sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.


        let runTestsWithBeautify
            (sheetModel: SheetT.Model)
            (beautifyFunc: SheetT.Model -> SheetT.Model)
            (testMetrics: (SheetT.Model -> int) list)
            (dispatch: Dispatch<Msg>)
            =

            let beautifiedSheetModel = beautifyFunc sheetModel

            testMetrics
            |> List.iter (fun metric ->
                let before = metric sheetModel
                let after = metric beautifiedSheetModel
                printfn
                    "Metric result before beautify: %d, after beautify: %d, Difference: %d"
                    before
                    after
                    (after - before)
                )

            showSheetInIssieSchematic {beautifiedSheetModel with UndoList= SheetUpdateHelpers.appendUndoList sheetModel.UndoList sheetModel} dispatch

    module Tests =
        //////////////////////////////////////////////////////////////////
        ////////////////// UNIT TESTING PIPELINE SKELETON ////////////////
        //////////////////////////////////////////////////////////////////
        open Builder

        let modelsUnderTest = modelsToTest
        let testMetricsInUse = unitTestMetrics
        let beautifyFunc = beautifyFunction
        let showUnitTestOnSheet (model: Model) (testModels: TestModel list) (dispatch: Dispatch<Msg>) =
            let currentState: int option = model.UnitTestState
            match currentState with
            | Some testModelIndex when testModelIndex < List.length testModels ->
                printfn "Showing state of test %d" testModelIndex
                let testModelToShow = testModels.[testModelIndex]
                let sheetModel =
                    try
                        placeTestModel testModelToShow
                    with _ ->
                        failwith "Error placing test model on sheet."
                showSheetInIssieSchematic sheetModel dispatch
                dispatch (UpdateUnitTestIndex( (fun _ -> Some(testModelIndex + 1))))
            | Some _ ->
                printfn "No more tests to show. Resetting to first test."
                dispatch (UpdateUnitTestIndex (fun _ -> (Some 0)) )
            | None ->
                printfn "Initializing test state to first test."
                dispatch (UpdateUnitTestIndex (fun _ -> (Some 0)))


        let showUnitTestOnSheetWrapper (model: Model) (dispatch: Dispatch<Msg>) =
            showUnitTestOnSheet model modelsUnderTest dispatch

        let runUnitTestOnSheet
            (model: Model)
            (testMetrics: list<(SheetT.Model -> int)>)
            (beautifyFunction: SheetT.Model -> SheetT.Model)
            (dispatch: Dispatch<Msg>)
            =
            let modelToRun = model.Sheet
            printfn "Running test on current sheet"
            // add to undo list
            runTestsWithBeautify modelToRun beautifyFunction testMetrics dispatch

        let runUnitTestOnSheetWrapper (model: Model) (dispatch: Dispatch<Msg>) =
            runUnitTestOnSheet model testMetricsInUse beautifyFunc dispatch


        let testRandom
            (randomSheet: SheetT.Model)
            (beautifyFunction: SheetT.Model -> SheetT.Model)
            (testAssert: SheetT.Model -> SheetT.Model -> option<string>)
            (dispatch: Dispatch<Msg>)
            =

            let beautifiedSheet = beautifyFunction randomSheet

            testAssert randomSheet beautifiedSheet
            |> function
             | Some fail ->
                    printfn $"{fail}"
                    printfn "Showing sheet before beautify..."
                    showSheetInIssieSchematic randomSheet dispatch
             | None ->
                    printfn "Showing beautified sheet"
                    showSheetInIssieSchematic beautifiedSheet dispatch
                    printfn "Test Succeeded!"


        let runRandomTest (model: Model) (dispatch: Dispatch<Msg>) =
            let randomModel = SimpleSymbolTestBuilders.buildTestCircuit 12 1000.0 10

            randomTestAsserts
            |> List.map (fun curAssert ->
                testRandom
                    randomModel
                    id
                    curAssert
                    dispatch )
            |> ignore

        let runRandomConstrainedTest (model: Model) (dispatch: Dispatch<Msg>) =
            let randomModel = SimpleSymbolTestBuilders.buildConstrainedCircuit -10 10 4 4 { X = 1000.0; Y = 1000.0 }

            randomTestAsserts
                |> List.map (fun curAssert ->
                    testRandom
                        randomModel
                        id
                        curAssert
                        dispatch )
                |> ignore

        let correctFormat (data: string) : string =
            let replaceFirstGroupToUpper (m: Match) : string =
                m.Groups.[1].Value + m.Groups.[2].Value.ToUpper()

            let addSemicolonToEnd (line: string) : string =
                if not (line.EndsWith(";")) && line <> "" then line + ";"
                else line

            let data = Regex.Replace(data, @"(GateN \()([a-z])", new MatchEvaluator(replaceFirstGroupToUpper))
            let data = Regex.Replace(data, @"(Mux)(\d)", "$1$2")
            let data = Regex.Replace(data, @"(Label = )([^\s]+)", "$1\"$2\"")

            let lines = data.Split([|'\n'|])
            let processedLines = lines |> Array.map addSemicolonToEnd
            let data = System.String.Join("\n", processedLines)

            if data.Length > 2 then
                data.Substring(0, data.Length - 1)
            else
                data


        let printTestModel (model: Model) (dispatch: Dispatch<Msg>) =
            let testModel = getTestModel model.Sheet
            let testModelString = correctFormat(sprintf "%A" testModel)
            printfn $"{testModelString}"
