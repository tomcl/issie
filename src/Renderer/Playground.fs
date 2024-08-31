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
        //printf $"font = {font}\n\n '{fontStr}' -> {canvasWidthContext.font}\n\n"
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
            let success, (num:bigint) = System.Numerics.BigInteger.TryParse(text)
            let numi = uint32 num
            // uncomment to see the parsed number
            printfn $"BigInteger Parse: num={num}, success={success} hex= {NumberHelpers.hexBignum num}"
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
    open ModelType



    let highLightChangedConnections dispatch =
        dispatch (Sheet (SheetT.Msg.SelectWires CanvasExtractor.debugChangedConnections))
        CanvasExtractor.debugChangedConnections <- []

module Memory =
    open Fable.Core
    open Fable.Core.JsInterop
    open ElectronAPI
    open ModelType

    let webframe = renderer.webFrame

    let printProcessMemory() : unit =
        let memInfo:JS.Promise<string>  = Node.Api.``process``?getProcessMemoryInfo()
        promise {
            return! memInfo
            }
        |> Promise.iter (
            fun info ->
                printfn $"mem info: private= {info?``private``/1000}, resident={info?``resident``}")
        

                


    let printMemory() =
        let toMB (f:float) = $"%10.1f{f / 1000000.}"
        let printDetails (name: string, d: MemoryUsageDetails option) =
            match d with
            | None ->
                $"""%20s{"object"} %10s{"count"} %10s{"livesize"} %10s{"size"}"""
            | Some d ->
                $"%20s{name} %10s{toMB d.count} %10s{toMB d.liveSize} %10s{toMB d.size}"
            
        let usage = webframe.getResourceUsage()
        let details =
            [
                "images", usage.images
                "other", usage.other
                "cssStyleSheets", usage.cssStyleSheets
                "xslstylesheets", usage.xslStyleSheets
                "fonts", usage.fonts
                "scripts", usage.scripts
            ] |> List.map (fun (s, r) -> s, Some r)
        String.concat "\n" (printDetails ("",None) :: List.map printDetails details)
        |> printfn "%s"
        webframe.clearCache()

    let printListeners() =
        let listeners = renderer.ipcRenderer.listeners
        renderer.ipcRenderer.eventNames()
        |> Array.iter (fun name ->
            let ls = listeners name
            printfn $"{name} -> {ls.Length}")

    let mutable modelCopy: Model option = None



