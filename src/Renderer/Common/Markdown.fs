/// A small markdown renderer for Issie's in-app help.
///
/// Issie's longer help text - the Info window, the waveform simulator's help panels, the bodies of
/// confirmation popups - used to be written as React element trees: `str` and `bSpan` and `li`
/// interleaved with the words, several hundred lines of it. Read as prose it was unreadable, which
/// is a problem for text whose whole job is to read well. It is now written as markdown in
/// AppMessages and rendered here.
///
/// The subset is not a guess: it is what that text was already doing, counted. Headings, bold,
/// italic, inline code, links, bullet and numbered lists, and tables - which is core GitHub
/// markdown with nothing left over. There are no images and no nested lists, so there is no
/// support for them; a line that tries will render as its own text rather than silently vanish.
///
/// TWO THINGS THIS DELIBERATELY DOES NOT DO.
///
/// It does not produce an HTML string. Rendering markdown by handing HTML to
/// `dangerouslySetInnerHTML` is the usual shortcut, and in a renderer with node integration, for
/// messages that interpolate sheet names the user chose, it is a bad trade. `render` builds React
/// elements, so there is no path from message text to executed markup.
///
/// It does not open links itself. A markdown link becomes an anchor with an `onClick` supplied by
/// the caller, because in Electron an ordinary `href` navigates the application window away from
/// Issie. Keeping the handler out here also keeps this module free of Electron and of anything
/// above it in compile order.
///
/// `parse` is pure and returns data, so the tests can read every message in AppMessages under
/// plain .NET without a browser. `render` is the only part that needs one.
module Markdown

open Fable.React
open Fable.React.Props

/// Text with emphasis, within one paragraph, heading, list item or table cell.
type Inline =
    | Text of string
    | Bold of string
    | Italic of string
    | Code of string
    /// [shown](url). The url is handed to the caller's handler; nothing here opens it.
    | Link of shown: string * url: string

type Block =
    /// `#`, `##`, `###` - level is 1, 2 or 3.
    | Heading of level: int * Inline list
    | Para of Inline list
    | Bullets of Inline list list
    | Numbered of Inline list list
    /// A GitHub table: the header row, then the body rows. Column count is the header's.
    | Table of header: Inline list list * rows: Inline list list list

//---------------------------------------------------------------------------------------------//
//--------------------------------------------PARSING-------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// Split a line into text and emphasis.
///
/// Written as a scan rather than with a regular expression per mark so that the marks cannot
/// interleave wrongly: at each position at most one of them can start, and each runs to its own
/// closing mark. An unclosed mark is not an error - it is left as literal text, which is what a
/// reader of the source would expect and what stops one stray asterisk swallowing a paragraph.
let parseInlines (line: string) : Inline list =
    let n = line.Length

    /// The index just past `mark` starting at `from`, or None if it does not close.
    let closes (mark: string) (from: int) =
        match line.IndexOf(mark, from) with
        | -1 -> None
        | i -> Some i

    let rec go (i: int) (plain: System.Text.StringBuilder) (acc: Inline list) =
        /// everything accumulated as plain text so far, emitted before an emphasis run
        let flush acc =
            if plain.Length = 0 then acc
            else
                let t = plain.ToString()
                plain.Clear() |> ignore
                Text t :: acc

        if i >= n then
            flush acc |> List.rev
        else

        let starts (mark: string) = i + mark.Length <= n && line.Substring(i, mark.Length) = mark

        // **bold** before *italic*, or the first asterisk of a bold mark opens an italic run
        if starts "**" then
            match closes "**" (i + 2) with
            | Some j -> go (j + 2) plain (Bold (line.Substring(i + 2, j - i - 2)) :: flush acc)
            | None -> plain.Append line[i] |> ignore; go (i + 1) plain acc
        elif starts "*" then
            match closes "*" (i + 1) with
            | Some j -> go (j + 1) plain (Italic (line.Substring(i + 1, j - i - 1)) :: flush acc)
            | None -> plain.Append line[i] |> ignore; go (i + 1) plain acc
        elif starts "`" then
            match closes "`" (i + 1) with
            | Some j -> go (j + 1) plain (Code (line.Substring(i + 1, j - i - 1)) :: flush acc)
            | None -> plain.Append line[i] |> ignore; go (i + 1) plain acc
        elif starts "[" then
            // [shown](url), and only in that shape: a bare [ is ordinary text
            match closes "]" (i + 1) with
            | Some close when close + 1 < n && line[close + 1] = '(' ->
                match closes ")" (close + 2) with
                | Some rp ->
                    let shown = line.Substring(i + 1, close - i - 1)
                    let url = line.Substring(close + 2, rp - close - 2)
                    go (rp + 1) plain (Link (shown, url) :: flush acc)
                | None -> plain.Append line[i] |> ignore; go (i + 1) plain acc
            | _ -> plain.Append line[i] |> ignore; go (i + 1) plain acc
        else
            plain.Append line[i] |> ignore
            go (i + 1) plain acc

    go 0 (System.Text.StringBuilder()) []

/// The longest prefix of `lst` satisfying `pred`, and what is left after it. List.partition would
/// take matching lines from anywhere, which for a block of markdown is not the same thing.
let private spanWhile pred lst =
    let taken = List.takeWhile pred lst
    taken, List.skip taken.Length lst

/// The cells of a table row, without the leading and trailing pipes.
let private tableCells (line: string) =
    let t = line.Trim()
    let t = if t.StartsWith "|" then t.Substring 1 else t
    let t = if t.EndsWith "|" then t.Substring(0, t.Length - 1) else t
    t.Split '|' |> Array.toList |> List.map (fun c -> parseInlines (c.Trim()))

/// Whether a line is a table's `|---|---|` rule, which is what marks the line above it as a header.
let private isTableRule (line: string) =
    let t = line.Trim()
    t.StartsWith "|" && t |> Seq.forall (fun c -> c = '|' || c = '-' || c = ':' || c = ' ')

let private isTableRow (line: string) = line.Trim().StartsWith "|"

let private bulletText (line: string) =
    let t = line.TrimStart()
    if t.StartsWith "- " then Some (t.Substring 2)
    elif t.StartsWith "* " then Some (t.Substring 2)
    else None

let private numberedText (line: string) =
    let t = line.TrimStart()
    let digits = t |> Seq.takeWhile System.Char.IsDigit |> Seq.length
    if digits > 0 && t.Length > digits + 1 && t[digits] = '.' && t[digits + 1] = ' '
    then Some (t.Substring(digits + 2))
    else None

/// Whether a line begins a block of its own, and so cannot be the continuation of the one above.
let private isBlockStart (line: string) =
    line.Trim() = ""
    || line.TrimStart().StartsWith "#"
    || isTableRow line
    || (bulletText line).IsSome
    || (numberedText line).IsSome

/// The items of a list, each gathered with the lines it wraps onto, and what follows the list.
///
/// A list item is written over as many source lines as it needs, like everything else here, so an
/// item is its marker line plus the lines under it that begin no block of their own. Without this
/// a wrapped item would lose everything after its first line - and worse, quietly: emphasis that
/// spanned the wrap would be left unclosed and render as asterisks.
let private listItems (marker: string -> string option) (lines: string list) =
    let rec go lines (current: string list) (items: string list list) =
        let close items = if List.isEmpty current then items else List.rev current :: items
        match lines with
        | line :: rest when (marker line).IsSome ->
            go rest [ (marker line).Value ] (close items)
        | line :: rest when not (List.isEmpty current) && not (isBlockStart line) ->
            go rest (line.Trim() :: current) items
        | _ -> List.rev (close items), lines
    go lines [] []

/// Markdown to blocks.
///
/// Paragraphs are separated by blank lines and a wrapped paragraph is joined back into one line,
/// so the source can be wrapped to the column width the rest of the codebase uses without that
/// wrapping reaching the screen.
let parse (text: string) : Block list =
    let lines =
        text.Replace("\r\n", "\n").Split '\n'
        |> Array.toList
        // a leading blank line is what you get from writing a message as a triple-quoted string
        |> List.skipWhile (fun l -> l.Trim() = "")

    let rec blocks (lines: string list) (acc: Block list) =
        match lines with
        | [] -> List.rev acc

        | line :: rest when line.Trim() = "" -> blocks rest acc

        | line :: rest when line.TrimStart().StartsWith "#" ->
            let t = line.TrimStart()
            let level = t |> Seq.takeWhile ((=) '#') |> Seq.length
            blocks rest (Heading (min level 3, parseInlines (t.Substring(level).Trim())) :: acc)

        | header :: rule :: rest when isTableRow header && isTableRule rule ->
            let rows, after = rest |> spanWhile isTableRow
            blocks after (Table (tableCells header, rows |> List.map tableCells) :: acc)

        | line :: _ when (bulletText line).IsSome ->
            let items, after = listItems bulletText lines
            blocks after (Bullets (items |> List.map (String.concat " " >> parseInlines)) :: acc)

        | line :: _ when (numberedText line).IsSome ->
            let items, after = listItems numberedText lines
            blocks after (Numbered (items |> List.map (String.concat " " >> parseInlines)) :: acc)

        | first :: rest ->
            // A paragraph runs to the next blank line or the start of any other kind of block.
            //
            // The first line is taken unconditionally, and only the rest is tested. Otherwise a
            // line that reaches here while failing the test below - a `|` row with no rule under
            // it, which is not a table and so falls through to here - would consume nothing, and
            // this would recurse for ever on the same list.
            let isParaLine (l: string) =
                l.Trim() <> ""
                && not (l.TrimStart().StartsWith "#")
                && not (isTableRow l)
                && (bulletText l).IsNone
                && (numberedText l).IsNone
            let para, after = rest |> spanWhile isParaLine
            let joined = first :: para |> List.map (fun l -> l.Trim()) |> String.concat " "
            blocks after (Para (parseInlines joined) :: acc)

    blocks lines []

//---------------------------------------------------------------------------------------------//
//-------------------------------------------RENDERING-----------------------------------------//
//---------------------------------------------------------------------------------------------//

/// Named Styles, not Style: `Style` is also a case of Fable.React.Props, and a module of that name
/// resolves against the union case under Fable even though dotnet build accepts it.
module private Styles =
    let heading level =
        let size = match level with | 1 -> "17px" | 2 -> "15px" | _ -> "14px"
        [ FontWeight "bold"; FontSize size; MarginTop "14px"; MarginBottom "6px" ]
    let para = [ MarginBottom "10px"; LineHeight "1.45" ]
    let list = [ MarginBottom "10px"; MarginLeft "22px"; LineHeight "1.45" ]
    let item = [ MarginBottom "4px" ]
    let code =
        [ FontFamily "monospace"; BackgroundColor "rgba(0,0,0,0.06)"
          Padding "1px 4px"; BorderRadius "3px" ]
    let cell = [ BorderBottom "1px solid #e0e0e0"; Padding "4px 10px 4px 0"; VerticalAlign "top" ]

/// One line's worth of inlines. `onLink` is given a link's url when it is clicked - see the note
/// at the top about why this module does not open it itself.
let renderInlines (onLink: string -> unit) (inlines: Inline list) : ReactElement list =
    inlines
    |> List.map (function
        | Text t -> str t
        | Bold t -> b [] [ str t ]
        | Italic t -> i [] [ str t ]
        | Code t -> span [ Style Styles.code ] [ str t ]
        | Link (shown, url) -> a [ OnClick (fun _ -> onLink url) ] [ str shown ])

/// Rendered markdown, as one element ready to drop into a popup body.
let renderBlocks (onLink: string -> unit) (blocks: Block list) : ReactElement =
    let inl = renderInlines onLink
    let items style xs = xs |> List.map (fun x -> li [ Style Styles.item ] (inl x))
    blocks
    |> List.map (function
        | Heading (level, xs) -> div [ Style (Styles.heading level) ] (inl xs)
        | Para xs -> div [ Style Styles.para ] (inl xs)
        | Bullets xs -> ul [ Style (Styles.list @ [ ListStyle "disc" ]) ] (items Styles.item xs)
        | Numbered xs -> ol [ Style (Styles.list @ [ ListStyle "decimal" ]) ] (items Styles.item xs)
        | Table (header, rows) ->
            // Markdown has no table without a header row, but a two-column table of term and
            // explanation has nothing to head its columns with. Written `| | |`, it is dropped
            // here rather than drawn as an empty band above the first row.
            let headerIsBlank = header |> List.forall List.isEmpty
            table [ Style [ BorderCollapse "collapse"; MarginBottom "12px"; Width "100%" ] ] [
                if not headerIsBlank then
                    thead [] [
                        tr [] (header |> List.map (fun c ->
                            th [ Style (Styles.cell @ [ TextAlign TextAlignOptions.Left ]) ] (inl c))) ]
                tbody [] (rows |> List.map (fun r ->
                    tr [] (r |> List.map (fun c -> td [ Style Styles.cell ] (inl c))))) ])
    |> div []

/// Markdown text, rendered. The usual entry point.
let render (onLink: string -> unit) (text: string) : ReactElement =
    text |> parse |> renderBlocks onLink
