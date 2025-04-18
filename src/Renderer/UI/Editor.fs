module Editor

open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open CommonTypes
open ModelType



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
        //printfn $"Intersecting {this} with {other} gives {startPos} to {endPos}"
        Some { Start = startPos; End = endPos}




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
                Animation "codeEditorBlink 1s steps(2, start) infinite" // Adjust blink duration and steps as needed
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
   
let actionMouseEvent (evType:string) ((x,y): float * float) (dispatch: 'msg -> unit) =
    let x = x
    let y = y
    printfn $"Mouse '{evType}' event  line:{int y} char:{x}"   

/// Renders the code editor with syntax highlighting
let renderEditor (model: CodeEditorModel) (dispatch: 'msg -> unit)  =

    let mouseEventHandler evType (ev:Browser.Types.MouseEvent) =
        let x = ev.clientX 
        let y = ev.clientY
        let el = Browser.Dom.document.getElementById("codeEditorContainer")
        let sx = el.scrollLeft
        let sy = el.scrollTop
        let bb = el.getBoundingClientRect()
        actionMouseEvent evType ((x + sx - bb.left) / 18.01, ((y + sy - bb.top) / 46.75)) dispatch

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
            |> (fun ints -> (*printfn $"Line {lineIndex} error intervals: %A{ints}";*) ints)
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
                                        WhiteSpace WhiteSpaceOptions.Pre
                                        ZIndex 10001
                                        Position PositionOptions.Relative
                                        Background "transparent"]] [spaceChars]
                            else
                                span [ Style [
                                        WhiteSpace WhiteSpaceOptions.Pre
                                        ZIndex 10001
                                        Position PositionOptions.Relative
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
                            ] ))

        let codeReact =
            line
            |> List.mapi (fun codeIndex code ->
                //printfn "Line %d length %d charIndex: %d" lineIndex line.Length codeIndex
                let styleProps = colorStyle code
                span [ 
                        Key (sprintf "char-%d-%d" lineIndex codeIndex)
                        Style (VerticalAlign "middle" :: WhiteSpace WhiteSpaceOptions.Pre :: Background "transparent" :: styleProps)
                    ]
                    [ str (string code.CodeText) ] )
            |> (fun reactElements ->
                    [span [Id $"line-{lineIndex}" ] reactElements])

        let cursorReact =
                if lineIndex = int model.CursorPos.Y then
                    [ renderCursor (model.CursorPos.X * 18.03 + 2.) ]
                else []
        List.concat [
            codeReact
            errorReact
            cursorReact
        ]
        |> div [
                Id  "codeEditorTop"
                OnClick (mouseEventHandler "click")
                OnMouseMove (mouseEventHandler "mousemove")           
                Style [
                    Position PositionOptions.Relative
                    BorderTop "0px"
                    BorderBottom "0px"
                    ]
               ])
        
    |> (fun lines -> 
        div [        
            Id "codeEditorContainer"
            Style [
                CSSProp.FontFamily "monospace"
                CSSProp.FontSize "30px"
                CSSProp.Height 1.5
                CSSProp.Position PositionOptions.Relative
                CSSProp.Overflow OverflowOptions.Auto
                CSSProp.Height "80vh"
                CSSProp.Width "80vw"
                ]
            ]   lines)

let updateCodeEditor codeMsg model =
    failwithf "Not implemented yet: %A" codeMsg

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
            (word "   module two or else" |> addColor Keyword)
            (word "a  tyst 5 " |> addColor Identifier)
            (word "     ;   ;")
        ] @ List.replicate 2000 (word "a  test bbbbbbbbbbbbbbbb " |> addColor Identifier)
    {   HighlightedCode = codeLines
        Errors = errorPosns
        CursorPos = { X = 10; Y = 0 }}
        



