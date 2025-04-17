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
        | -1 -> other.Start
        | _ -> this.Start
    let endPos =
        match System.Math.Sign(this.End.Y - other.End.Y) with
        | 0 -> // same line
            { X= min this.End.X other.End.X; Y=this.End.Y }
        | -1 -> this.End
        | _ -> other.End
    if startPos.Y > endPos.Y  || (startPos.Y  = endPos.Y && startPos.X > endPos.X) then
        None
    else
        printfn $"Intersecting {this} with {other} gives {startPos} to {endPos}"
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
let errorStyle = [ TextDecoration "underline wavy red 5px" ]


let renderCursor (posn: float) =
        div [
            Style [
                Position PositionOptions.Absolute
                CSSProp.Left 0
                CSSProp.Top 0
                MarginLeft posn
                ZIndex 10002
                Width "2px" // Adjust as needed for the thickness of the I-beam
                Height "1em" // Adjust for desired height */
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

        let errorReact =
            model.Errors
            |> List.collect (
                intersects {Start={X = 0; Y = lineIndex}; End={X = float (line.Length - 1) ; Y = lineIndex}}
                >> Option.map (fun interval -> int interval.Start.X, int interval.End.X)
                >> Option.toList)
            |> List.sort
            |> (fun ints -> printfn $"Line {lineIndex} error intervals: %A{ints}"; ints)
            |> function
                | [] -> []
                | lineErrors ->                    
                    ((0,[]), lineErrors )
                    ||> List.fold (fun (start,segsSoFar) (errStart, errEnd) ->
                            let okSeg =  if errStart - 1 < start then None 
                                         else Some <| (false, errStart - start)
                            let errSeg = if errStart > errEnd then None
                                         else Some <| (true, errEnd - errStart)
                            errEnd + 1, List.concat [
                                            Option.toList errSeg;
                                            Option.toList okSeg;
                                            segsSoFar ])
                    |> (fun (_endIndex, segL) ->
                        //printfn $"Segments: %A{segL} lineIndex {lineIndex} lineErrors {lineErrors}"
                        segL
                        |> List.rev
                        |> List.map (fun (isError, segLength) ->
                            let spaceChars = str <| String.replicate segLength " "
                            if isError then
                                span [Style [
                                        TextDecoration "underline wavy red";
                                        CSSProp.Custom("textUnderlineOffset","15%")
                                        WhiteSpace WhiteSpaceOptions.PreWrap
                                        ZIndex 10001
                                        Position PositionOptions.Relative
                                        Background "transparent"]] [spaceChars]
                            else
                                span [ Style [
                                        WhiteSpace WhiteSpaceOptions.PreWrap

                                        Background "transparent"]] [ spaceChars ] )
                        |> (fun lineReactL ->
                            [ div [
                                    Style [
                                        Position PositionOptions.Absolute
                                        CSSProp.Left 0
                                        CSSProp.Top 0
                                        ZIndex 10001
                                        Width "2px" // Adjust as needed for the thickness of the I-beam
                                        Height "1em" // Adjust for desired height */
                                        //BackgroundColor "black" // Or any color you want for the cursor
                                    ]
                               ] lineReactL
                            ]
                           ))


        let codeReact =
            line
            |> List.mapi (fun codeIndex code ->
                //printfn "Line %d length %d charIndex: %d" lineIndex line.Length codeIndex
                let styleProps = colorStyle code
                span [ 
                        Key (sprintf "char-%d-%d" lineIndex codeIndex)
                        Style (WhiteSpace WhiteSpaceOptions.PreWrap :: Background "transparent" :: styleProps)
                    ]
                    [ str (string code.CodeText) ] )
            |> (fun reactElements ->
                    [div [Id $"line-{lineIndex}"] reactElements])

        let cursorReact =
                if lineIndex = int model.CursorPos.Y then
                    [ renderCursor (model.CursorPos.X * 18.03+ 2.) ]
                else []
        List.concat [
            codeReact
            errorReact
            cursorReact
        ]
        |> div [Style [Position PositionOptions.Relative]])
        
    |> (fun lines -> 
        div [        
            Class "code-editor-container"
            Style [
                CSSProp.Width "70vw"
                CSSProp.Height "70vh"
                CSSProp.FontFamily "monospace"
                CSSProp.FontSize "30px"
                CSSProp.Position PositionOptions.Relative
                ]
            ]  [div [Class "code-editor-lines"] [div [] lines]])



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
            { Start = { X = 3; Y = 1 }; End = { X = 4; Y = 1 } }
            { Start = { X = 7; Y = 0 }; End = { X = 10; Y = 0 } }
            { Start = { X = 4; Y = 2 }; End = { X = 6; Y = 2 } }
        ]
    let codeLines =
        [
            (word "module two or else" |> addColor Keyword)
            (word "a  tyst 5 " |> addColor Identifier)
            (word ";   ;")
        ] @ List.replicate 2000 (word "a  test bbbbbbbbbbbbbbbb " |> addColor Identifier)
    {   HighlightedCode = codeLines
        Errors = errorPosns
        CursorPos = { X = 10; Y = 0 }}
        



