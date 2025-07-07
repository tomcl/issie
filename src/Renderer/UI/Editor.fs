module Editor

open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open Fable.React.HookBindings
open Fable.Core.JsInterop
open CommonTypes
open ModelType
open Elmish


/// <summary>
/// Extends the editor to support wrapped lines—logical lines of text that may span 
/// multiple visual rows due to space or tab breaks.
/// </summary>
/// 
/// <remarks>
/// Introducing wrapped lines adds complexity because the mapping between logical text 
/// line numbers and visual row numbers is no longer 1-to-1. This makes downstream processing 
/// and rendering more difficult unless the problem is broken into simple steps.
///
/// The solution involves defining a new type:
///
/// - A new record type `WrappedLine` is added to `editor.fs`.
/// - The editor model's text is changed from `string list` to `WrappedLine list`.
///
/// <para>
/// Each `WrappedLine`:
/// - Corresponds 1:1 with a logical line (`string`)
/// - Contains metadata about how the line is wrapped into visual rows
/// - Stores the starting row number it occupies on the screen, which allows independent
///   rendering of lines even when wrapping occurs
/// </para>
///
/// <para>
/// The editor update process uses the following transformation steps:
/// 1. `unwrap` – Converts `WrappedLine list` back to `string list`
/// 2. Apply existing editor logic (insertions, deletions, etc.) on the `string list`
/// 3. `wrap` – Reconstructs the `WrappedLine list` from the updated `string list`
/// </para>
///
/// Additional changes include:
/// - Modifying the `ClickHandler` to work with `WrappedLine`
/// - Updating the rendering logic to operate on `WrappedLine` instead of `string`
/// </remarks>


/// constants used by code editor
module Constants =
    let leftMargin = 100.0 // left margin for code editor text
    let leftGutter = 20.0 // distance between code numbers and text

let initCodeEditorState =
    {
        Lines = [{Txt="";LineBreaks=[]}] // code must have at least one (possibly zero length) line.
        Errors = []
        CursorPos = {X = 0; Y = 0}
    }

/// Intersection of two intervals each defining the end points
/// of a raster scan (ascending order X first then Y).
let intersectionOpt (other: Interval) (this: Interval) =
    let startPos =
        match System.Math.Sign(this.Start.YLine - other.Start.YLine) with
        | 0 -> // same line
            { XChar = max this.Start.XChar other.Start.XChar; YLine = this.Start.YLine }
        | -1 -> other.Start
        | _ -> this.Start
    let endPos =
        match System.Math.Sign(this.End.YLine - other.End.YLine) with
        | 0 -> // same line
            { XChar = min this.End.XChar other.End.XChar; YLine = this.End.YLine }
        | -1 -> this.End
        | _ -> other.End
    if
        startPos.YLine > endPos.YLine
        || (startPos.YLine = endPos.YLine && startPos.XChar > endPos.XChar)
    then
        None
    else
        //printfn $"Intersecting {this} with {other} gives {startPos} to {endPos}"
        Some { Start = startPos; End = endPos }

/// TODO: implement line breaks in lines that need wrapping
let toWrappedLine (txt: string) = {
    Txt = txt
    LineBreaks = []
}

//--------------------------------------------------------------------------------//
//---------------Types and Active Patterns for parsing and highlighting-----------//
//--------------------------------------------------------------------------------//

/// the type of a code editors text string associated with a given highlight color
type ElementType =
    | Normal
    | Keyword
    | Identifier
    | Number
    | String

/// The type of the input to the highlighter.
/// This is a list of characters, since the input to the highlighter is a string.
/// Matching operations have type: MatchStream -> (string * ElementType) * MatchStream.
/// TODO:
/// MatchStream could have state added to make the highlighting context-sensitive.
/// E.g. to remember if the string being highlighted starts in a comment or not: MatchStream = bool * char list.
/// More generally MatchStream = StateT * char list where StateT is a discriminated union.
type MatchStream = char list

/// Returns true if the characters in str, interpreted as a string, start prefix.
let charsStartWith (prefix: string) (str: char list) : bool =
    prefix.Length <= str.Length
    && prefix
       |> Seq.indexed
       |> Seq.forall (fun (index, c) -> str[index] = c)

/// Active pattern helper that matches if a list of chars starts with startP.
/// F it matches the return value is a pair:
/// the first character and the string of characters matching inMatchP
/// the list of chars from the first character that does not match inMatchP.
/// or None if the first character does not match startP.
/// NB - this cannot itself be an active pattern since startP and inMatchP are not literals.
let makeAPMatch (startP: char -> bool) (inMatchP: char -> bool) (str: MatchStream) : (string * MatchStream) option =
    match str with
    | [] -> None
    | start :: rest when startP start ->
        let unMatchedPart = List.skipWhile inMatchP rest
        let matchedString =
            str[0 .. str.Length - unMatchedPart.Length - 1]
            |> System.String.Concat
        Some(matchedString, unMatchedPart)
    | _ -> None

let (|NormalMatch|_|) (str: MatchStream) : (string * MatchStream) option =
    let isNormal c =
        c <> '"' && not (System.Char.IsLetterOrDigit c)
    makeAPMatch isNormal isNormal str

/// Active Pattern that matches if a char list starts with any of the prefixes in a list.
/// Returns the prefix and the remaining characters if it does.
/// NB - prefixL must be a literal for this to work
let (|StringLP|_|) (prefixL: string list) (str: MatchStream) : (string * MatchStream) option =
    prefixL
    |> List.tryPick (fun prefix ->
        if charsStartWith prefix str then
            Some(prefix, str.[prefix.Length ..])
        else
            None)

/// Active Pattern that matches if str starts with a given character.
/// Returns the remaining string if it does.
/// NB - prefix must be a literal for this to work
let (|CharP|_|) (prefix: char) (str: MatchStream) : MatchStream option =
    if str.Length > 0 && str[0] = prefix then
        Some str.[1..]
    else
        None

/// Active Pattern that matches if str starts with a string literal.
/// Returns the string literal and the remaining string if it does.
let (|StringLiteralStart|_|) (str: MatchStream) : (string * MatchStream) option =
    let isStringStart c = c = '"'
    let isStringPart c = c <> '"'
    makeAPMatch isStringStart isStringPart str

/// Active Pattern that matches if str starts with an identifier.
/// Returns the identifier and the remaining string if it does.
let (|IdentifierP|_|) (str: MatchStream) : (string * MatchStream) option =
    let isIdentifierStart c = System.Char.IsLetter c || c = '_'
    let isIdentifierPart c =
        System.Char.IsLetterOrDigit c || c = '_'
    makeAPMatch isIdentifierStart isIdentifierPart str

/// Active Pattern that matches if str starts with a number.
/// Returns the number and the remaining string if it does.
let (|NumberP|_|) (str: MatchStream) : (string * MatchStream) option =
    let isNumberStart c = System.Char.IsDigit c
    let isNumberPart c = System.Char.IsDigit c
    makeAPMatch isNumberStart isNumberPart str

/// Return chars segmented as a list of tuples (string, ElementType).
/// The string is the text to be highlighted and the ElementType is the type of highlighting.
let rec highlight (chars: MatchStream) : (string * ElementType) list =
    match chars with
    | [] -> []
    | StringLP [ "let"; "if"; "then"; "else" ] (keyword, rest) -> (keyword, Keyword) :: highlight rest
    | IdentifierP(identifier, rest) -> (identifier, Identifier) :: highlight rest
    | NumberP(number, rest) -> (number, Number) :: highlight rest
    | StringLiteralStart(quote, CharP '"' rest) -> (quote + "\"", String) :: highlight rest
    | StringLiteralStart(quote, []) -> (quote, String) :: []
    | NormalMatch(normalPart, rest) -> (normalPart, Normal) :: highlight rest
    | c :: rest -> // this should not happen
        printfn $"Warning: character '{c}' is not recognised by highlighter, default to Normal"
        (c.ToString(), Normal) :: highlight rest


//----------------------------------------------------------------------------------------------//
//----------------------------Mouse Event Handling & Cursor-------------------------------------//
//----------------------------------------------------------------------------------------------//

/// handles editor mouse events.
/// evtype is the type of event (e.g., click, mousemove).
/// dispatch is the function to call to update the model.
/// ev is the mouse event from the browser.
// NB - maybe use a union type for the event type instead of a string?
let mouseEventHandler evType dispatch (ev: Browser.Types.MouseEvent) =
    let x = ev.clientX
    let y = ev.clientY
    let el = Browser.Dom.document.getElementById "codeEditorContainer"
    let sx = el.scrollLeft
    let sy = el.scrollTop
    let bb = el.getBoundingClientRect ()
    let x = (x + sx - bb.left - Constants.leftMargin) / 11. // 11 one char width
    let y = (y + sy - bb.top) / 30. // 30 is line height
    match evType with
    | "click" ->
        // SetCursor message calls updateEditorCursor
        dispatch <| CodeEditorMsg(SetCursor(max (int (x + 0.5)) 0, int y)) // offset by 0.5 chars
    | _ -> ()

/// Updates the Editor model with the new cursor position based on mouse coordinates.
/// xMouse and yMouse are the mouse coordinates.
/// xMouse specifies the column position in the line at which new chars are inserted
/// xMouse - 1 is the column that is deleted on backspace.
/// Model is the whole Issie Model - of which CodeEditorModel is one field.
let updateEditorCursor (xMouse: int) (yMouse: int) (model: Model) =
    let state = model.CodeEditorState |> Option.defaultValue initCodeEditorState
    let y =
        yMouse
        |> min (state.Lines.Length - 1)
        |> max 0
    let x =
        xMouse
        |> min (state.Lines[y].Txt.Length)
        |> max 0
    { model with CodeEditorState = Some {state with CursorPos ={ X = x; Y = y } } }

//------------------------------------------------------------------------------------------//
//------------------------------Key Press Handling------------------------------------------//
//------------------------------------------------------------------------------------------//

/// Implements a model update for a key press event.
/// The keyCode is the code of the key pressed.
/// Normally, this is a character code, but it can also be a special key code (e.g., backspace).
/// Character codes are inserted. Special codes are handled differently.
/// Lines and CursorPos are updated accordingly.
let updateEditorOnKeyPress (keyPress: KeyPressInfo) (model: Model) =
    let state = model.CodeEditorState |> Option.defaultValue initCodeEditorState
    let key = keyPress.KeyString
    let modifiers =
        Set
        <| seq {
            if keyPress.ShiftKey then
                yield "Shift"
            if keyPress.ControlKey then
                yield "Control"
            if keyPress.AltKey then
                yield "Alt"
            if keyPress.MetaKey then
                yield "Meta"
        }
    /// insert point is on the line indexed by cursorY
    let cursorY =
        int state.CursorPos.Y
        |> min (state.Lines.Length - 1)
        |> max 0
    // split up lines before doing processing
    let beforeLines, currentLine, afterLines =
        match state.Lines.Length with
        | 0 -> [], toWrappedLine "", [] // if there are no lines make one empty line.
        | n ->
            List.splitAt (min cursorY n) state.Lines
            |> fun (before, after) -> before, after[0], after[1..]

    /// insert point is before the character at cursorX
    let cursorX =
        int state.CursorPos.X
        |> min currentLine.Txt.Length
    /// the output values here come from 5 separate items
    /// newLines is beforeLines, newLine, afterLines concatenated.
    /// The items all have default values.
    /// It would be better to use a record type to hold these values and update the fields
    /// of a default record value as needed. That would make the code clearer.
    let newCursorX, newCursorY, newLines =
        let insertIndex = min cursorX currentLine.Txt.Length
        let noChange = beforeLines @ [ currentLine ] @ afterLines
        match modifiers = Set.empty, key with
        | true, "Backspace" -> // backspace
            match currentLine with
            | _ when cursorX = 0 && cursorY = 0 ->
                cursorX, cursorY, noChange
            | line when cursorX = 0 ->
                beforeLines[cursorY - 1].Txt.Length,
                cursorY - 1,
                beforeLines[.. cursorY - 2]
                @ [ toWrappedLine <| beforeLines[cursorY - 1].Txt + line.Txt ]
                @ afterLines
            | line ->
                cursorX - 1,
                cursorY,
                beforeLines
                @ [ Seq.removeAt (cursorX - 1) line.Txt
                    |> System.String.Concat 
                    |> toWrappedLine ]
                @ afterLines
        | true, "Enter" -> // enter
            List.splitAt insertIndex (currentLine.Txt |> Seq.toList)
            |> fun (before, after) ->
                0,
                cursorY + 1,
                beforeLines
                @ [ before |> System.String.Concat |> toWrappedLine; after |> System.String.Concat |> toWrappedLine ]
                @ afterLines
        | _, key when
                    key.Length = 1
                    && not (
                        keyPress.AltKey
                        || keyPress.ControlKey
                        || keyPress.MetaKey
                    ) ->
            cursorX + 1,
            cursorY,
            beforeLines
            @ [ currentLine.Txt.Insert(insertIndex, key) |> toWrappedLine ]
            @ afterLines
        | _, key ->
            //printfn $"Key {key} with modifiers {modifiers} not handled"
            cursorX, cursorY, noChange
    //printfn $"Updating model with cursor = char:{newCursorX} line:{newCursorY}"
    let state = model.CodeEditorState |> Option.defaultValue initCodeEditorState
    { model with
        CodeEditorState =
            Some { state with Lines = newLines; CursorPos = { X = newCursorX; Y = newCursorY } }},
    Cmd.none

//------------------------------------------------------------------------------------//
//--------------------------------Editor Rendering------------------------------------//
//------------------------------------------------------------------------------------//

(*
[<ReactComponent>]
let Counter () =
    let count, setCount = React.useState (0)

    (React.useEffect 
         (fun _ -> printfn $"The count is now {count}"),
         [| box count |]
    ) |> ignore

    Html.div [
        Html.h1 count
        Html.button [
            prop.onClick (fun _ -> setCount (count + 1))
            prop.text "Increment"
        ]
    ]
*)

/// Render the left margin - that does not scroll horizontally
let renderLineNumbers xPosition (model: CodeEditorModel) =
    
    div
        [ Id "lineNumberColumn"
          Style
              [ CSSProp.Position PositionOptions.Absolute
                CSSProp.Left (xPosition)
                CSSProp.Width (Constants.leftMargin - Constants.leftGutter)
                CSSProp.Top 0
                CSSProp.LineHeight 1.5
                CSSProp.OverflowY OverflowOptions.Visible
                CSSProp.OverflowX OverflowOptions.Visible
                CSSProp.ZIndex 10000
                BackgroundColor "#f0f0f0" // light grey background for left margin
                BorderRight "1px solid #ccc" // light grey border on right side of left margin
                ] ]
        ([1..max model.Lines.Length 1]
        |> List.map (fun i ->
            div
                [ Id $"lineNumber-{i}"
                  Style
                      [ CSSProp.Position PositionOptions.Absolute
                        CSSProp.Left 0
                        CSSProp.Top (float (i - 1) * 30.) // 30 is line height
                        CSSProp.Width (Constants.leftMargin - Constants.leftGutter)
                        CSSProp.Height "30px" // line height
                        CSSProp.TextAlign TextAlignOptions.Right
                        CSSProp.MarginRight "10px"
                        CSSProp.BackgroundColor "#f0f0f0" // light grey background for left margin
                        CSSProp.PaddingRight "5px" 
                        ] ]
                [ str (sprintf "%d" i) ]))
     

/// Renders the code editor with syntax highlighting and error indication.
/// TODO: Add Fable.React.FunctionComponent.Of caching to each line of test and associated errors
/// so that the lines are not normally re-rendered when the model changes.
//  NB - performance without this on 1000 lines is OK.

let renderEditor (model: CodeEditorModel) (dispatch: Msg -> unit) =
    // for debug printout of editor Lines text.
    let toText (code: string list) = code |> String.concat "\n"
    //printfn $"-----Rendering-----\n%A{model.Lines |> toText}\n------------------\n"
    model.Lines
    |> List.mapi (fun lineIndex text ->
        /// the line error indications as a list of react elements
        /// each comprising a string of invisible characters that is highlighted
        /// or not.
        let errorLineReact =
            model.Errors
            |> List.collect (
                intersectionOpt
                    { Start = { XChar = 0; YLine = lineIndex }
                      End = { XChar = text.Txt.Length + 1; YLine = lineIndex } }
                >> Option.map (fun interval -> int interval.Start.XChar, int interval.End.XChar)
                >> Option.toList
            )
            |> List.sort
            |> function
                | [] -> []
                | lineErrors ->
                    ((0, []), lineErrors)
                    ||> List.fold (fun (start, segsSoFar) (errStart, errEnd) ->
                        let okSeg =
                            if errStart - 1 < start then
                                None
                            else
                                Some <| (false, errStart - start)
                        let errSeg =
                            if errStart > errEnd then
                                None
                            else
                                Some <| (true, errEnd - errStart)
                        errEnd + 1, List.concat [ Option.toList errSeg; Option.toList okSeg; segsSoFar ])
                    |> (fun (_endIndex, segL) ->
                        segL
                        |> List.rev
                        |> List.mapi (fun i (isError, segLength) ->
                            /// space chars do not render underlines correctly
                            /// so we use a non-space with a Color = transparent instead to make an
                            /// underline overlay
                            let spaceChars = str <| String.replicate segLength "*"
                            if isError then
                                span
                                    [ Id $"errorseg-{lineIndex}-{i}"
                                      Style
                                          [ TextDecoration "underline wavy red"
                                            // negative offset to make text above wavy line readable
                                            // requires enough space between lines (LineHeight =1.5 is fine)
                                            CSSProp.Custom("textUnderlineOffset", "15%") 
                                            Position PositionOptions.Relative
                                            ] ]
                                    [ spaceChars ]
                            else
                                span
                                    [ Id $"okseg-{lineIndex}-{i}"
                                      Style
                                          [ Position PositionOptions.Relative
                                          ]
                                    ]
                                    [ spaceChars ])
                        |> (fun errorLineReactL ->
                            [ div
                                  [ Id $"errorSegLine-{lineIndex}"
                                    Style
                                        [ Position PositionOptions.Absolute // from CodeEditorLine
                                          CSSProp.Left 0
                                          CSSProp.Top 0
                                          WhiteSpace WhiteSpaceOptions.Pre
                                          VerticalAlign "middle"
                                          Background "transparent"
                                          Color "transparent"                                          
                                          LineHeight 1.5
                                          ] ]
                                  errorLineReactL ]))
        /// the line text elements as highlighted by highlighter
        let codeLineReact =
            let reactOfText =
                match text.Txt with
                | "" -> [ br [] ]
                | text ->
                    highlight (text |> Seq.toList) // use highlighter to get highlighted text
                    |> List.map (fun (text, elementType) ->
                        // better to implement this using a Map under Constants?
                        match elementType with
                        | Normal -> str text
                        | Keyword -> span [ Style [ Color "green" ] ] [ str text ]
                        | Identifier -> span [ Style [ Color "blue" ] ] [ str text ]
                        | Number -> span [ Style [ Color "orange" ] ] [ str text ]
                        | String -> span [ Style [ Color "red" ] ] [ str text ])
            [ span
                  [ Id (sprintf "code-text-line-%d" lineIndex)
                    Style [
                        Position PositionOptions.Absolute // from codeEditorLine
                        VerticalAlign "middle"
                        CSSProp.Custom("scrollSnapAlign", "start")
                        LineHeight 1.5
                        CSSProp.Left 0
                        CSSProp.Top 0
                        WhiteSpace WhiteSpaceOptions.Pre
                        Background "transparent"
                        ]
                     ]

                  reactOfText]
        /// the line cursor element - if needed
        let cursorReact =
            /// Renders the cursor in react at the given position as a single character padded to the left with a margin.
            /// The cursor is a vertical line that blinks to indicate the current insert position.
            /// The returned react element has a ZIndex chosen so it lies above the text.
            let renderCursor (charIndex: int) =
                let xPosition = (float charIndex) * 11. - 9.5 // Small adjustment to make cursor before character
                div
                    [ Id "cursor-div"; Style
                          [ Position PositionOptions.Absolute  // from codeEditorLine
                            CSSProp.Left xPosition
                            CSSProp.Top 0
                            LineHeight 1.5 // line height is 1.5 X Font size.
                            Background "transparent"
                            //ZIndex 10002
                            //Width "2px" // Adjust as needed for the thickness of the I-beam
                            //Height "20px" // Adjust for desired height */
                            // animation uses codeEditorBlink frame info defined in extra.css
                            // TODO?: define this programmatically in the DOM. Is this possible?
                            Animation "codeEditorBlink 1s steps(2, start) infinite" ] ] // Adjust blink duration and steps as needed
                    [ str "\u2336" ] // Unicode character for I-beam cursor
            if lineIndex = int model.CursorPos.Y then
                // cursor displays before the character at cursorX
                [ renderCursor (int model.CursorPos.X) ]
            else
                []
        List.concat [ codeLineReact; errorLineReact; cursorReact ]
        |> div
            [ Id "codeEditorLine"
              Style // codeEditorLine is positioned absolute offset from codeEditorContainer
                  [ Position PositionOptions.Absolute
                    CSSProp.Left Constants.leftMargin
                    CSSProp.Top (float lineIndex * 30.) // 30 is line height
                    LineHeight 0 // the real LineHeight is defined in the inner div
                    WhiteSpace WhiteSpaceOptions.Pre
                  ] ])

    |> fun lines ->
        let scrollContainer = Browser.Dom.document.getElementById "codeEditorContainer"
        let hasXScroll = scrollContainer <> null && scrollContainer.scrollWidth > scrollContainer.clientWidth
        /// the vertical height taken up by a horizontal scrollbar
        let xScrollbarHeightPx =
            if scrollContainer = null || not hasXScroll then 0.
            else scrollContainer.offsetHeight - scrollContainer.clientHeight
        let xScrollAmount =
            if scrollContainer = null then 0.
            else scrollContainer.scrollLeft
        div
            // This is the main container for the code editor.
            // It contains all the editor content that scrolls.
            // It is sized to fit an integral number of editor lines
            [ Id "codeEditorContainer" 
              OnClick(mouseEventHandler "click" dispatch) // used to position cursor on click
              OnMouseMove(mouseEventHandler "mousemove" dispatch) // not currently used
              OnScroll (fun ev -> dispatch <| CodeEditorMsg(EditorMsg.UpdateCodeEditorState id))

              Style
                  [ CSSProp.FontFamily "monospace" // don't change this - dimensions depend on it.
                    // for Monospace 30px, in dev tools, char size is 16.50 W X 35.05 H
                    // Lineheight 1.5 (used for inner divs and spans) sets Line height to 45
                    CSSProp.FontSize "20px" // scales all dimensions. Line height is 1.5 X this.
                    LineHeight "0" // real line height is defined in inner div
                    BorderTop "0px"
                    PaddingTop "0px"
                    PaddingBottom "0px"                    
                    BorderBottom "0px"
                    CSSProp.Position PositionOptions.Relative
                    CSSProp.OverflowY OverflowOptions.Auto
                    CSSProp.OverflowX OverflowOptions.Auto
                    CSSProp.ScrollSnapType "y mandatory" // snap to nearest line when vertical scrolling
                    // Work out correct height for integral number of lines.
                    // This is larger if the editor requires a horizontal scrollbar.
                    // 50vh is the approx desired window height. 45px is the line height.
                    // exact window height is varied to ensure editor holds exact number of lines
                    CSSProp.Height  $"calc( round(50vh , 45px) + {xScrollbarHeightPx}px)"
                    // 50vw is the required editor width.
                    // NB - this may not be an exact number of chars.
                    // TODO - change this as per height to make it exact?
                    // This might cause oscillation due to interation between heights widths?
                    CSSProp.Width "50vw" ] ]
            (renderLineNumbers xScrollAmount model :: lines)

//---------------------------------------Top Level--------------------------------------------------//


/// Main editor model update function, called from Elmish update when a CodeEditorMsg Message is received.
let updateCodeEditor codeMsg model =
    let state = model.CodeEditorState |> Option.defaultValue initCodeEditorState
    match codeMsg with
    | SetCursor(xMouse, yMouse) -> updateEditorCursor xMouse yMouse model
    | UpdateCode updateFn -> { model with CodeEditorState = Some {state with Lines = updateFn state.Lines }}
    | SetErrors errors -> { model with CodeEditorState = Some {state with Errors = errors } }
    | UpdateCodeEditorState updateFn -> { model with CodeEditorState = Some (updateFn state)}
    |> fun model -> model, Cmd.none

/// Some ad hoc test data for the code editor
let testEditorModel =

    let errorPosns =
        [ { Start = { XChar = 3; YLine = 1 }; End = { XChar = 4; YLine = 1 } }
          { Start = { XChar = 0; YLine = 0 }; End = { XChar = 4; YLine = 0 } }
          { Start = { XChar = 4; YLine = 40 }; End = { XChar = 6; YLine = 41 } } ]

    let codeLines =
        [ "   if then else module 123 yellow Big!"; "a  test 5 "; "     ;   ;" ]
        @ List.replicate 1000 "test2 bbbb 1234 () \"abcdef\" 6754 (Ta123) "

    { Lines = codeLines |> List.map toWrappedLine; Errors = errorPosns; CursorPos = { X = 10; Y = 0 } }
