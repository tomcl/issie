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

    let fontString (font:DrawHelpers.Text) = String.concat " " [font.FontWeight; font.FontSize; font.FontFamily]

    let textMeasureWidth (font:DrawHelpers.Text) (txt:string) =
        let fontStr = fontString font
        canvasWidthContext.font <- fontStr
        printf $"font = {font} -> {canvasWidthContext.font}"
        //canvasWidthContext.textAlign <- font.TextAnchor
        canvasWidthContext.measureText(txt).width

    // all lower case values
    let testedFonts = [
        "arial"
        "helvetica"
        "verdana"
        "tahoma"
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
            let fontFamily =
                match dialogData.ConstraintErrorMsg with
                | None -> fontStyleDefault
                | Some fs -> fs
            let textToTest = Option.defaultValue textToTestDefault dialogData.Text
            let fontSize = 20
            let font = {
                DrawHelpers.defaultText with
                    FontFamily = fontFamily // arial,times,consolas,georgia,helvetica, verdana, trebuchet ms, impact, tahoma
                    FontSize = $"{fontSize}px"
                    TextAnchor = "left" // left, right, middle
                    FontWeight = "" // "bold", ""
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

