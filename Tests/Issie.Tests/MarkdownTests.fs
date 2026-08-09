/// The markdown used for Issie's in-app help, and the help itself.
///
/// Markdown.parse is pure and returns data, so every message in AppMessages can be read here under
/// plain .NET - no browser, no React. That matters more than the parser tests: the risk with
/// moving text out of the view functions is not that the parser is wrong, it is that a message
/// acquires a malformed table or an unclosed link and renders as something odd months later, with
/// nothing to catch it. The compiler used to catch that, because the markup was F#.
module MarkdownTests

open Expecto
open Markdown

let private textOf (inlines: Inline list) =
    inlines
    |> List.map (function
        | Text t -> t
        | Bold t -> t
        | Italic t -> t
        | Code t -> t
        | Link (shown, _) -> shown)
    |> String.concat ""

/// every message AppMessages holds, with a name to report failures against
let private allMessages: (string * string) list = [
    "Info.gettingStarted", AppMessages.Info.gettingStarted
    "Info.tips", AppMessages.Info.tips
    "Info.bugReport", AppMessages.Info.bugReport
    "Info.about", AppMessages.Info.about "v0.0.0"
    "WaveHelp.gettingStarted", AppMessages.WaveHelp.gettingStarted
    "WaveHelp.viewingWaveforms", AppMessages.WaveHelp.viewingWaveforms
    "WaveHelp.selection", AppMessages.WaveHelp.selection
    "WaveHelp.instructions", AppMessages.WaveHelp.instructions
    "WaveHelp.miscellaneous", AppMessages.WaveHelp.miscellaneous
    "WaveHelp.noHelpFor", AppMessages.WaveHelp.noHelpFor "Some Feature"
    "Memories.help", AppMessages.Memories.help
    "Confirm.usingParameters", AppMessages.Confirm.usingParameters
    "Confirm.duplicateSheet", AppMessages.Confirm.duplicateSheet
    "Confirm.missingProjectFile", AppMessages.Confirm.missingProjectFile "SomeFolder"
]

/// The Properties pane's field explanations. Plain text, not markdown - they are drawn by CSS from
/// a data-tooltip attribute, which holds characters and nothing else.
let private fieldTips = AppMessages.Fields.tips

let tests =
    testList "Markdown" [

        test "emphasis, code and links are picked out of a line" {
            Expect.equal (parseInlines "plain **bold** and *italic* and `code`")
                [ Text "plain "; Bold "bold"; Text " and "; Italic "italic"; Text " and "; Code "code" ]
                "each mark runs to its own closing mark"
            Expect.equal (parseInlines "see [F#](https://fsharp.org/) for more")
                [ Text "see "; Link ("F#", "https://fsharp.org/"); Text " for more" ]
                "a link keeps its shown text and its url apart"
        }

        test "an unclosed mark is left as text rather than swallowing the line" {
            // the failure that matters: one stray asterisk must not italicise the rest of a page
            Expect.equal (parseInlines "2 * 3 = 6") [ Text "2 * 3 = 6" ] "a lone asterisk is a lone asterisk"
            Expect.equal (parseInlines "an [unfinished link") [ Text "an [unfinished link" ] "as is a lone bracket"
        }

        test "a wrapped paragraph becomes one line" {
            // the source is wrapped to the width the rest of the codebase uses; that must not reach
            // the screen as line breaks
            let blocks = parse "one two\nthree four\n\nsecond para"
            Expect.equal blocks.Length 2 "two paragraphs, from the blank line between them"
            match blocks with
            | [ Para a; Para b ] ->
                Expect.equal (textOf a) "one two three four" "the wrapped lines are joined with a space"
                Expect.equal (textOf b) "second para" ""
            | _ -> failtest $"expected two paragraphs, got {blocks}"
        }

        test "headings, bullets and numbered lists" {
            match parse "# Title\n\n- one\n- two\n\n1. first\n2. second" with
            | [ Heading (1, h); Bullets bs; Numbered ns ] ->
                Expect.equal (textOf h) "Title" ""
                Expect.equal (List.map textOf bs) [ "one"; "two" ] ""
                Expect.equal (List.map textOf ns) [ "first"; "second" ] ""
            | other -> failtest $"expected heading, bullets, numbered; got {other}"
        }

        test "a table takes its header from the row above the rule" {
            match parse "| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |" with
            | [ Table (header, rows) ] ->
                Expect.equal (List.map textOf header) [ "a"; "b" ] ""
                Expect.equal (rows |> List.map (List.map textOf)) [ [ "1"; "2" ]; [ "3"; "4" ] ] ""
            | other -> failtest $"expected one table, got {other}"
        }

        test "a pipe line with no rule under it is not a table" {
            // otherwise a sentence containing a pipe would silently become a one-cell table
            match parse "| not a table" with
            | [ Para p ] -> Expect.equal (textOf p) "| not a table" "it stays a paragraph"
            | other -> failtest $"expected a paragraph, got {other}"
        }

        testList "every message in AppMessages" [
            for name, text in allMessages ->
                test name {
                    let blocks = parse text
                    Expect.isNonEmpty blocks "the message parses to at least one block"

                    // A table whose rows do not match its header renders with cells missing, and
                    // nothing else would notice.
                    blocks
                    |> List.iter (function
                        | Table (header, rows) ->
                            rows
                            |> List.iteri (fun i r ->
                                Expect.equal r.Length header.Length
                                    $"row {i} of a table in {name} has {r.Length} cells, header has {header.Length}")
                        | _ -> ())

                    // Markup left showing means a mark that did not close, or one this subset does
                    // not support. Either renders as literal punctuation in the middle of prose.
                    let rendered =
                        blocks
                        |> List.collect (function
                            | Heading (_, xs) | Para xs -> [ textOf xs ]
                            | Bullets xs | Numbered xs -> List.map textOf xs
                            | Table (h, rs) -> List.map textOf h @ (rs |> List.collect (List.map textOf)))
                        |> String.concat " "
                    Expect.isFalse (rendered.Contains "**") $"{name} has an unclosed bold mark"
                    Expect.isFalse (rendered.Contains "](") $"{name} has a link this subset did not parse"
                }
        ]

        test "field explanations carry no markup, because a tooltip cannot render it" {
            // The one place markdown must NOT be used: these reach the screen through a
            // data-tooltip attribute, so any mark written here would show as punctuation.
            fieldTips
            |> Map.iter (fun label tip ->
                Expect.isFalse (tip.Contains "**") $"the explanation of '{label}' has a bold mark"
                Expect.isFalse (tip.Contains "](") $"the explanation of '{label}' has a link"
                Expect.isFalse (tip.Contains "\n") $"the explanation of '{label}' has a line break")
        }

        test "every field explanation is a sentence, not a label" {
            // These exist because the pane's labels were too terse to be understood. One that is
            // itself terse has not done the job.
            fieldTips
            |> Map.iter (fun label tip ->
                Expect.isGreaterThan tip.Length 40 $"the explanation of '{label}' is too short to explain anything"
                Expect.stringEnds (tip.Trim()) "." $"the explanation of '{label}' is not a full sentence")
        }
    ]
