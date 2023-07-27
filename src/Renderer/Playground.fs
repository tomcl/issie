module Playground

module TestFonts =
    open PopupView
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
        fontStyleDefault |> Some |> SetPopupConstraintErrorMsg |> dispatch
        textToTestDefault |> Some |> SetPopupDialogText  |> dispatch
        fun (dialogData : PopupDialogData) ->
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
                    Input.OnChange (getTextEventValue >> Some >> SetPopupConstraintErrorMsg >> dispatch)
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
                Option.defaultValue "" dd.ConstraintErrorMsg
                |> nextFontFamily
                |> Some
                |> SetPopupConstraintErrorMsg
                |> dispatch)
            (fun _ -> false)
            []
            dispatch
        

        
