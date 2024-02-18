module Playground

module TestFonts =
    open PopupHelpers
    open ModelType
    open EEExtensions
    open Fulma
    open Fable.React
    open Fable.React.Props
    open Browser.Types
    open JSHelpers
    open Helpers


    let testCanvas = Browser.Dom.document.createElement("canvas") :?> HTMLCanvasElement
    let canvasWidthContext = testCanvas.getContext_2d()

    let fontString (font:DrawHelpers.Text) = String.concat " " [ font.FontWeight; font.FontSize; font.FontFamily]

    let textMeasureWidth (font:DrawHelpers.Text) (txt:string) =
        let fontStr = fontString font
        canvasWidthContext.font <- fontStr
        printf $"font = {font}\n\n '{fontStr}' -> {canvasWidthContext.font}\n\n"
        //canvasWidthContext.textAlign <- font.TextAnchor
        canvasWidthContext.measureText(txt).width

    // all lower case values
    let testedFonts = [
        "arial"
        "helvetica"
        "verdana"
        "tahoma"
        "600 tahoma"
        "trebuchet ms"
        "times"
        "georgia"
        "impact"
        ]

    let nextFontFamily fontFamily =
        fontFamily
        |> (fun f -> List.tryFindIndex ((=) f) testedFonts)
        |> Option.defaultValue -1
        |> (+) 1
        |> (fun n -> testedFonts[n % testedFonts.Length] )

    /// Create the body of a dialog Popup with both text and int.
    let dialogPopupBody  dispatch =
        let fontStyleDefault = "times"
        let textToTestDefault = "iiiimmmmyyyy0123456789"
        fontStyleDefault |> Some |> SetPopupConstraintErrorMsg |> TruthTableMsg |> dispatch
        textToTestDefault |> Some |> SetPopupDialogText  |> dispatch
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
            let fontSpec =
                match dialogData.ConstraintErrorMsg with
                | None -> fontStyleDefault
                | Some fs -> fs
            let textToTest = Option.defaultValue textToTestDefault dialogData.Text
            let fontSize = 20
            let fontWeight,fontFamily =
                match fontSpec.Split (" ",System.StringSplitOptions.RemoveEmptyEntries) with
                | [|family|] -> "",family
                | [|weight;family|] -> weight, family
                | _ -> "", ""
            let font = {
                DrawHelpers.defaultText with
                    FontFamily = fontFamily // arial,times,consolas,georgia,helvetica, verdana, trebuchet ms, impact, tahoma
                    FontSize = $"{fontSize}px"
                    TextAnchor = "left" // left, right, middle
                    FontWeight = fontWeight // "bold", ""
                }
            let text = (Option.defaultValue textToTestDefault dialogData.Text)
            let width =  textMeasureWidth font text
            let height = float fontSize
            let textEl = DrawHelpers.makeText 50. 100.  text font
            div [] [
                br []
                br []
                str "Font Family: enter here or click button for known fonts" 
                Input.text [
                    Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false; HTMLAttr.Value fontFamily]
                    Input.OnChange (getTextEventValue >> Some >> SetPopupConstraintErrorMsg >> TruthTableMsg >> dispatch)
                ]
                br []
                br []
                str "Text to show:"
                Input.text [
                    Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false; HTMLAttr.Value textToTest]
                    Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
                ]
                br []
                br []
                svg
                    [ Style
                        [
                            Height 200
                            Width 800
                        ]
                    ]
                    [
                        let pts = $"50 100 {50. + width} 100  {50. + width} {100. + height} 50 {100. + height}"
                        textEl
                        g [] (SymbolView.createPolygon pts "red" 0.2)
                    ]

            ]

    let makeTextPopup (dispatch: Msg -> Unit) =
        let body = dialogPopupBody dispatch
        dialogPopup
            "Font test: pink box shows measured width"
                body
            "Change Font"
            (fun dd ->
                Option.defaultValue "" dd.PopupDialogData.ConstraintErrorMsg
                |> nextFontFamily
                |> Some
                |> SetPopupConstraintErrorMsg
                |> TruthTableMsg
                |> dispatch)
            (fun _ -> false)
            []
            dispatch
        
module MiscTests =

    /// static assets should theoretically be put under ./static in Issie repo
    /// but appear on file system under staticDir() when Issie is run. The exact poistion on disk
    /// will vary between production and dev runs, but staticDir()
    /// should always work
    let testAssets() =
        let staticD = FilesIO.staticDir()
        printfn "Static Asset Directory = %s" staticD
        printfn "%A" (FilesIO.readdir staticD)

    let testMaps() =
        let modMap =
            [0..1000]
            |> List.map (fun n -> n, (n*256+1) % 1001)
            |> Map.ofList


        let iterMap count =
            let mutable x: int = 1
            let mutable i:int = 0
            while i < count do
                x <- modMap[x]
                i <- i + 1

        let count = 1000000
        let start = TimeHelpers.getTimeMs()
        let result = iterMap count
        let interval = TimeHelpers.getTimeMs() - start
        printfn "%d iterations of iterMap took %.1fms" count interval

    let displayPerformance n m = TimeHelpers.checkPerformance n m JSHelpers.startTimer JSHelpers.stopAndLogTimer


module Breadcrumbs =
    open Fable.React
    open Fable.React.Props
    open Browser.Types

    let config = MiscMenuView.Constants.defaultConfig

    let testBreadcrumbs model dispatch =
        let action _ _ = ()
        PopupHelpers.closablePopup
            "Design Hierarchy of current sheet"
            (MiscMenuView.hierarchyBreadcrumbs config dispatch model)
            (div [] []) []
            dispatch

    let testAllHierarchiesBreadcrumbs model dispatch =
        let action _ _ = ()
        PopupHelpers.closablePopup
            "Design Hierarchy of all sheets"
            (MiscMenuView.allRootHierarchiesFromProjectBreadcrumbs config dispatch model)
            (div [] [])
            []
            dispatch

module WebWorker =
    open WorkerInterface

    type WorkerPerfTestConfig = {
        OverheadRuns: int
        OverheadWWs: int
        ConcurrencyTestWWs: int list
        NumRuns: int // number of times all tests are run
    }

    module Constants =
        let workerTestConfig = {
            OverheadRuns = 5
            OverheadWWs = 100
            ConcurrencyTestWWs = [2;4;6;8;10]
            NumRuns = 3
        }
    
    let geoMean (vals: float array) =
        vals
        |> Array.reduce ( * )
        |> fun x -> x ** (1./(float vals.Length))


    let runTestNTimes n testPromise =
        promise {
            let mutable result = 1.0
            let! discardPromise = testPromise // discard first test
            for _ in [1..n] do
                let! testVal = testPromise
                result <- result * testVal
            return result
        }
        |> Promise.map (fun result -> result ** (1./(float n)))

    let workerPromise (t: string) =
        Promise.create (fun resolve reject ->
            let start = TimeHelpers.getTimeMs()
            let worker = newWorkerUrl("./TestWorker.fs.js")
            worker
            |> setWorkerOnMsg (fun (msg: {|data: float|}) -> resolve ((TimeHelpers.getInterval start)/1000.))
            sendWorkerMsg t worker
        )
    
    let nWorkerPromise (t: string) (n: int) =
        List.init n (fun _ -> workerPromise t)
        |> Promise.all

    let testWorkerConcurrency n =
        promise {
            let! worker1Time = workerPromise "long"

            let! workers = nWorkerPromise "long" n
            let parallelism = (float n) * (worker1Time / (geoMean workers))
            return parallelism
        }
    
    let testWorkerOverhead runs =
        promise {
            let mutable totalOverhead = 1.0;
            for _ in [1..runs] do
                let! overhead = workerPromise "short"
                totalOverhead <- totalOverhead * overhead
            return totalOverhead
        }
        |> Promise.map (fun total ->
            total ** (1./(float runs)))

    let testWorkerCPUOverhead numWorkers =
        promise {
            let start = TimeHelpers.getTimeMs()
            let! nWorkers = nWorkerPromise "short" numWorkers
            let timeTaken = (TimeHelpers.getInterval start)/1000.
            return ((float numWorkers)/timeTaken)
        }
        

    let testWorkers (conf: WorkerPerfTestConfig) =
        promise {
            let! overheadRes = runTestNTimes conf.NumRuns <| testWorkerOverhead conf.OverheadRuns
            printfn "Average elapsed time overhead: %.2f seconds" overheadRes
            let! cpuOverheadRes = runTestNTimes conf.NumRuns <| testWorkerCPUOverhead conf.OverheadWWs
            printfn "Can start %.1f workers/second" cpuOverheadRes
            for i in conf.ConcurrencyTestWWs do
                let! parallelism = runTestNTimes conf.NumRuns <| testWorkerConcurrency i
                printfn "Parallelism with %d workers: %.2f" i parallelism
        } |> ignore

            

module Misc =
    open ModelType
    open DrawModelType


    let highLightChangedConnections dispatch =
        dispatch (Sheet (SheetT.Msg.SelectWires Extractor.debugChangedConnections))
        Extractor.debugChangedConnections <- []


module TestDraw =
    open TestDrawBlock
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
    open TestLib
    open TestDrawBlock.HLPTick3.Builder
    open TestDrawBlock.HLPTick3

    /// create an initial empty Sheet Model 
    //let initSheetModel = DiagramMainView.init().Sheet



    /// demo test circuit consisting of a DFF & And gate
    /// andPos - controls posn of AND gate
    /// TODO - this function is unsafe - it should return a result
    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail

    /// Demo circuit with flips and rotations of DFF and AND gate.
    /// Parameters control flip and rotation of DFF and AND gate.
    /// andPos controls position of AND gate.
    /// TODO - this function is unsafe - it should return a result
    let makeAdvTestCircuit sample =
        let (flipDFF, (flipAnd, (rotateDFF, (rotateAnd, andPos)))) = sample
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.map (rotateSymbol "G1" rotateAnd)
        |> match flipAnd with
            | Some f -> Result.map (flipSymbol "G1" f)
            | None -> id
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.map (rotateSymbol "FF1" rotateDFF)
        |> match flipDFF with
            | Some f -> Result.map (flipSymbol "FF1" f)
            | None -> id
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail

    /// Same as makeAdvtestCircuit but uses anonymous record for sample.
    /// not clear whetehr this is a good idea in this case.
    /// Demo circuit with flips and rotations of DFF and AND gate.
    /// Parameters control flip and rotation of DFF and AND gate.
    /// andPos controls position of AND gate.
    /// TODO - this function is unsafe - it should return a result
    let makeAdvTestCircuit1 (
                sample: {|
                    FlipDFF: SymbolT.FlipType option;
                    FlipAnd: SymbolT.FlipType option;
                    RotateDFF: Rotation;
                    RotateAnd: Rotation;
                    AndPos: XYPos
                |}) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) sample.AndPos
        |> Result.map (rotateSymbol "G1" sample.RotateAnd)
        |> match sample.FlipAnd with
            | Some f -> Result.map (flipSymbol "G1" f)
            | None -> id
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.map (rotateSymbol "FF1" sample.RotateDFF)
        |> match sample.FlipDFF with
            | Some f -> Result.map (flipSymbol "FF1" f)
            | None -> id
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail

    /// Generates a Gen<> of XYPos samples on a XY grid.
    /// m: number of grid points on each side.
    /// grid pitch is 5 pixels (fixed).
    /// Generated samples are filtered so that makeTest1Circuit does not
    /// have overlapping (illegal) symbols.
    /// TODO - make this a Constant in TestDrawblokc.Constants
    /// TODO - makeTest1circuit should be a parameter so this can be generalised.
    /// TODO makeTest1Circuit should return a result with error returns filtered
    /// out from the grid.
    let gridMaker m =
        let coords =
            fromList [-m..5..m]
            |> map (fun n -> float n)
        product (fun x y -> middleOfSheet + {X=x; Y=y}) coords coords
        |> filter (fun pos ->
            let sheet = makeTest1Circuit pos
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> not
        )

    /// Sample data based on a grid of points around the sheet center,
    /// filtered to remove samples which cause symbol-symbol intersections.
    /// (for this test, we are only interested in symbol-segment intersections).
    let grid70 = gridMaker 70
    let grid100 = gridMaker 100

    let makeTuple a b = (a, b)

    /// Gen samples incorporating two sets of rotations, and two of flips
    let advSamples =
        let rots = fromList [Rotation.Degree0; Rotation.Degree90; Rotation.Degree180; Rotation.Degree270]
        let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; Some SymbolT.FlipType.FlipVertical; None]
        product makeTuple rots grid100
        |> product makeTuple rots
        |> product makeTuple flips
        |> product makeTuple flips

    /// as advSamples but uses an anonymous record. That should be
    /// better but as you can see if used only once it is not obviously better!
    let advSamples1 =
        advSamples // convert to anonymous record using Gen.map
                   // to map over the samples
        |> map (fun (flipDFF, (flipAnd, (rotateDFF, (rotateAnd, andPos)))) ->
            {|
                FlipDFF=flipDFF;
                FlipAnd=flipAnd;
                RotateDFF= rotateDFF;
                RotateAnd=rotateAnd;
                AndPos=andPos
             |})
 





 
 
