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


    // The same measurement the draw block itself uses, so what this page reports is what
    // symbol sizing will get. It used to keep a private canvas here, which meant this module
    // touched the DOM as soon as it loaded.
    let textMeasureWidth = DrawHelpers.getTextWidthInPixels

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

            

module Sidecar =

    /// What the latency test sends. Sizes are payload bytes; each size is measured in all three
    /// directions (echo both ways, upload, download), warmed up untimed first.
    type SidecarLatencyTestConfig = {
        PayloadSizes: int list
        /// untimed round trips before each series, so connection setup is not in the numbers
        Warmup: int
        /// timed round trips per size and direction
        Runs: int
        /// as Runs, for sizes at or over BigThreshold bytes - big transfers take long enough
        /// that fewer runs still settle
        BigRuns: int
        BigThreshold: int
    }

    module Constants =
        let latencyTestConfig = {
            PayloadSizes = [ 0; 1024; 65536; 1_048_576; 16_777_216 ]
            Warmup = 2
            Runs = 10
            BigRuns = 3
            BigThreshold = 1_048_576
        }

    /// Mean round-trip milliseconds over `runs` timed requests, after `warmup` untimed ones.
    let private runSeries (warmup: int) (runs: int) (cmd: int) (payload: obj) =
        promise {
            for _ in [ 1 .. warmup ] do
                let! _ = SidecarClient.request cmd payload
                ()

            let mutable total = 0.0

            for _ in [ 1 .. runs ] do
                let start = TimeHelpers.getTimeMs ()
                let! _ = SidecarClient.request cmd payload
                total <- total + TimeHelpers.getInterval start

            return total / float runs
        }

    /// MB/s for `bytes` moved in `ms` - 0 when the time is too small to divide by.
    let private throughput (bytes: float) (ms: float) =
        if ms <= 0.0 then 0.0 else bytes / 1.0e6 / (ms / 1000.0)

    /// Development > Play > Test Sidecar Latency. Output goes to Log.out because it is what was
    /// asked for; errors (sidecar not up yet, connection dropped) land on Log.error.
    let testLatency (conf: SidecarLatencyTestConfig) =
        promise {
            do! SidecarClient.connect ()
            Log.out "sidecar latency: mean round trip, renderer <-> dotnet sidecar over a loopback WebSocket"

            for size in conf.PayloadSizes do
                let runs = if size >= conf.BigThreshold then conf.BigRuns else conf.Runs

                // echo and upload carry the payload up; download asks for that many bytes back
                let sized () = SidecarClient.makeBytes size

                let downRequest () =
                    let request = SidecarClient.makeBytes 4
                    SidecarClient.writeUint32At request 0 (float size)
                    request

                let! echoMs = runSeries conf.Warmup runs SidecarClient.Constants.echoCmd (sized ())
                let! upMs = runSeries conf.Warmup runs SidecarClient.Constants.uploadCmd (sized ())
                let! downMs = runSeries conf.Warmup runs SidecarClient.Constants.downloadCmd (downRequest ())

                let mbs bytes ms = throughput (float (bytes: int)) ms

                Log.out (
                    $"%9d{size}B x%2d{runs}:  "
                    + $"echo %8.3f{echoMs}ms (%.1f{mbs (2 * size) echoMs}MB/s)  "
                    + $"up %8.3f{upMs}ms (%.1f{mbs size upMs}MB/s)  "
                    + $"down %8.3f{downMs}ms (%.1f{mbs size downMs}MB/s)"
                )
        }
        |> Promise.catch (fun e -> Log.error $"sidecar latency test: {e.Message}")
        |> ignore

module Misc =
    open DrawModelType
    open ModelType
    open Editor
    open PopupHelpers
    open Fable.React
    open Fable.React.Props

    let makeEditorPopup (dispatch: Msg -> Unit) =
        let body = fun model ->
                //div [ Style [] ]
                    match model.CodeEditorState with
                    | Some codeModel -> Editor.renderEditor  codeModel dispatch
                    | None -> div [] [] // should not happen?
        dispatch <| CodeEditorMsg (UpdateCodeEditorState (fun _ -> Editor.testEditorModel))
        dynamicClosablePopup
            "Editor Demo"
            body
            (fun _ -> div [] [])
            [Height "auto" ; Width "auto"]
            //[Height "calc(50vh + 175px)" ; Width "calc(50vw + 50px)"]
            dispatch


    let highLightChangedConnections dispatch =
        dispatch (Sheet (SheetT.Msg.SelectWires CanvasExtractor.debugChangedConnections))
        CanvasExtractor.debugChangedConnections <- []

module Memory =
    open Fable.Core
    open Fable.Core.JsInterop
    open ElectronAPI
    open ModelType

    let printProcessMemory() : unit =
        let memInfo = Bridge.processMemory ()
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
            
        let usage: Electron.ResourceUsage = unbox (Bridge.resourceUsage ())
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
        Bridge.clearCache()

    /// The renderer has no ipcRenderer of its own any more, so what this counts is what the preload
    /// holds on its behalf - which is what leaks if anything does, and what this was always after.
    let printListeners() =
        let counts = Bridge.ipcListenerCounts ()
        JS.Constructors.Object.keys counts
        |> Seq.iter (fun name -> printfn $"{name} -> {counts?(name)}")

    let mutable modelCopy: Model option = None

    
