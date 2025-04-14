module Editor

open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open CommonTypes

/// a text position interval in the code editor.
/// This is used to represent the start and end of a selection or an error.
/// all characters in raster scan order are included.
type Interval =
    {
        Start: XYPos
        End: XYPos
    }

/// Intersection of two intervals
let intersects (other: Interval) (this: Interval) =
    let startPos =
        match System.Math.Sign(this.Start.Y - other.Start.Y) with
        | 0 -> // same line
            { X= max this.Start.X other.Start.X; Y=this.Start.Y }
        | -1 -> this.Start
        | _ -> other.Start
    let endPos =
        match System.Math.Sign(this.End.Y - other.End.Y) with
        | 0 -> // same line
            { X= min this.End.X other.End.X; Y=this.End.Y }
        | -1 -> this.End
        | _ -> other.End
    if startPos >= endPos then
        None
    else
        Some { Start = startPos; End = endPos}


type CodeColor =
    | Comment
    | Keyword
    | Identifier
    | Literal


   

type Code =
    {
        CodeText: char
        Color: CodeColor
    }

/// Elmish Model type for a rich text code editor

type CodeEditorModel =
    {
        /// the characters in the code editor as a list of lines
        HighlightedCode: Code list list
        /// The errored code positions
        Errors: Interval list
        /// The current cursor position
        CursorPos: XYPos
    }

    with member this.tryGetChar(x: int, y: int) =
                    List.tryItem y this.HighlightedCode
                    |> Option.map (fun line ->
                        line |> List.tryItem x
                        |> Option.map (fun code -> code.CodeText)
                    )

         member this.tryGetColor(x: int, y: int) =
                    List.tryItem y this.HighlightedCode
                    |> Option.map (fun line ->
                        line |> List.tryItem x
                        |> Option.map (fun code -> code.Color)
                    )

module Constants =
    let codeColors = 
        [
            Comment, "green"
            Keyword,"Blue"
            Identifier,"Black"
            Literal,"Red"
        ] 
        |> Map.ofList

let colorStyle (code: Code) = [ Color Constants.codeColors[code.Color] ]
let errorStyle = [ TextDecoration "underline wavy red"]


let renderCursor (posn: float) =
        span [
            Style [
                Display DisplayOptions.Block
                Float FloatOptions.Left
                MarginLeft posn
                ZIndex 10000
                Width "2px" // Adjust as needed for the thickness of the I-beam
                Height "1em" // Adjust for desired height */
                //BackgroundColor "black" // Or any color you want for the cursor
                Animation "editorblink 1s steps(2, start) infinite" // Adjust blink duration and steps as needed
            ]   
        ] [str "\u2336"] // Unicode character for I-beam cursor

let makeSegments (line: ReactElement list) (errorPositions: Interval list) =

    let errors = errorPositions |> List.map (fun ep -> int ep.Start.X,int ep.Start.X)

    let makeLineSegment styles (startPos, endPos) =
        if startPos <= endPos then
            [ startPos, span [ Style styles ] line[startPos..endPos] ]
        else
            []

    let normalSegments =
        (0,-1) :: errors @ [line.Length, line.Length - 1]
        |> List.pairwise
        |> List.collect (fun ((start1, end1),(start2,end2)) -> makeLineSegment [] (end1, start2))

    let errorSegments =
        errors
        |> List.collect (makeLineSegment [ TextDecoration "underline wavy red"])

    (normalSegments @ errorSegments)
    |> List.sortBy fst
    |> List.map snd
   
    

/// Renders the code editor with syntax highlighting
let renderEditor (model: CodeEditorModel) (dispatch: 'msg -> unit)  = 
    model.HighlightedCode
    |> List.mapi (fun lineIndex line ->
        let text = line |> List.map (fun code -> code.CodeText)
        model.Errors
        |> List.collect (
            intersects {Start={X = 0; Y = lineIndex}; End={X = float (line.Length - 1) ; Y = lineIndex}}
            >> Option.map (fun interval -> int interval.Start.X, int interval.End.X)
            >> Option.toList)
        |> fun errs -> errs @ [line.Length, line.Length-1]
        |> (fun errorsOnLine ->
            ((0,[]), errorsOnLine )
            ||> List.fold (fun (start,segsSoFar) (errStart, errEnd) ->
                    let okSeg =  if errStart - 1 < start then None 
                                 else Some <| (false, start, errStart-1)
                    let errSeg = if errStart > errEnd then None
                                 else Some <| (true, errStart, errEnd)
                    errEnd + 1, List.concat [
                                    Option.toList okSeg;
                                    Option.toList errSeg;
                                    segsSoFar ])
            |> (fun x -> printfn $"Segments: %A{x}"; snd x))
        |> List.map (fun (isError, startP, endP) ->
            printfn "Line %d isError: %b, startP: %d, endP: %d" lineIndex isError startP endP
            [startP..endP]    
            |> List.map (fun charIndex ->
                let code = line[charIndex]
                let styleProps = colorStyle code
                span [ 
                    Key (sprintf "char-%d-%d" lineIndex charIndex)
                    Style styleProps
                ] [ str (string code.CodeText) ])
            |> span (if isError then [ Style errorStyle ] else []) )
        |> (fun lineParts ->
            span [
                    Key (sprintf "line-%d" lineIndex)
                    //Class "code-line"
                    Style [
                        //CSSProp.MinHeight "1.2em"
                        //CSSProp.Position PositionOptions.Relative
                    ]
                ] lineParts)
        |> (fun lineReact -> 
                if lineIndex = int model.CursorPos.Y then
                    div [] [ lineReact; renderCursor (model.CursorPos.X * 10.) ]
                else div [] [lineReact]))
    |> (fun lines -> 
        div [        
            Class "code-editor-container"
            Style [
                CSSProp.Width "100%"
                CSSProp.Height "100%"
                CSSProp.FontFamily "monospace"
                CSSProp.FontSize "30px"
                //CSSProp.Position PositionOptions.Relative
                ]
            ]  lines)



let testEditorModel =
    let word (txt:string) =
        txt
        |> Seq.toList
        |> List.map (fun c ->
            {
                CodeText = c
                Color = Identifier
            }
        )
    let addColor (color: CodeColor) (textChars: Code list)  =
        textChars |> List.map (fun textChar -> { textChar with Color = color })

    let errorPosns =
        [
            { Start = { X = 0; Y = 0 }; End = { X = 4; Y = 0 } }
            //{ Start = { X = 4; Y = 2 }; End = { X = 6; Y = 2 } }
        ]
    let codeLines =
        [
            (word "module" |> addColor Keyword)
            (word " test" |> addColor Identifier)
            (word ";")
        ]
    {   HighlightedCode = codeLines
        Errors = errorPosns
        CursorPos = { X = 3; Y = 0 }}
        



