/// What the user sees of everything that has gone wrong this session.
///
/// Two things use this. The Bug Reports tab shows the panel below, so that somebody who has been
/// asked for details can get them without opening dev tools - which the checklist beside it asks
/// them to do, and which most people reporting a bug will not manage. And the renderer's
/// exception boundary shows `crashPage` when the view itself throws, which is the one case where
/// the Info window cannot be opened to read the panel.
///
/// **The text is the deliverable.** Everything here exists to get `Log.problemReport ()` into a
/// forum post or an email, so the panel is a block of text and a button that copies it, and not a
/// table of exceptions with columns. The same text is on `window.issieLog.report()`, so a
/// developer sitting at the machine reads exactly what a user would have sent.
module ExceptionReport

open Fable.React
open Fable.React.Props
open Fulma
open ModelType
open PopupHelpers

module Constants =
    /// How tall the text block may get before it scrolls. It sits under the bug report checklist
    /// and must not push it off the screen.
    let maxBlockHeight = "20em"

    /// How long the "copied" confirmation stays up. As ErrorDisplay, which this sits beside.
    let copiedNotificationMs = 3000

//-------------------------------------------------------------------------------------------------//
//------------------------------------- WHAT IS AUTO-COLLECTED ------------------------------------//
//-------------------------------------------------------------------------------------------------//

// **Everything a machine can answer, Issie answers.** These used to be questions on the checklist,
// and they are the ones a person answers wrongly from memory or not at all: which version, which
// platform, what was open. The checklist now asks only what Issie cannot know.

/// The build and the machine it is running on.
let private environmentLines () =
    let build = if Bridge.isDev then "development" else "installed"

    let debug =
        if JSHelpers.debugLevel > 0 then $", debug level {JSHelpers.debugLevel}" else ""

    [ $"Issie {Version.VersionString} ({build} build{debug})"
      $"{Bridge.platform} {Bridge.osRelease} {Bridge.arch}, Electron {Bridge.electronVersion}, "
      + $"Chrome {Bridge.chromeVersion}"
      $"logging: {Log.namesOfMask Log.enabled}" ]

/// What was open and how big it was, which is most of what makes an Issie bug reproducible - and
/// none of which the user is asked for. The crash page has no model it can trust, so this is
/// optional rather than assumed.
///
/// The project's NAME, never its path: a path is the user's home directory, which a bug report
/// does not need and they may not want to publish.
let private sessionLines (model: Model option) =
    match model with
    | None -> []
    | Some m ->
        let simulator =
            if m.SimulateInRenderer then "renderer (development only)" else ".NET sidecar"

        let heap =
            try
                $", heap {JSHelpers.usedHeap () / 1000000}MB of {JSHelpers.heapLimit () / 1000000}MB"
            with _ ->
                ""

        [ match m.CurrentProj with
          | None -> "no project open"
          | Some p ->
              let name =
                  p.ProjectPath.Split([| '/'; '\\' |])
                  |> Array.filter (fun part -> part <> "")
                  |> Array.tryLast
                  |> Option.defaultValue "?"

              // the top sheet as well as the one on screen: they are often different, and which
              // one a design is rooted at decides what a simulation even contains
              let top =
                  p.LoadedComponents
                  |> List.tryFind (fun ldc -> ldc.IsTopSheet)
                  |> Option.map (fun ldc -> ldc.Name)
                  |> Option.defaultValue "none marked"

              $"project '{name}', {p.LoadedComponents.Length} sheets, top sheet '{top}', "
              + $"showing '{p.OpenFileName}' "
              + $"({Map.count m.Sheet.Wire.Symbol.Symbols} symbols, {Map.count m.Sheet.Wire.Wires} wires)"
          $"simulating in the {simulator}{heap}" ]

/// What the panel shows: everything Issie knows and nothing to fill in. The questions are on the
/// tab beside it to be read, and in the copied text to be answered - putting them here as well
/// would be the same words three times on one screen.
let viewText (model: Model option) =
    [ yield! environmentLines ()
      yield! sessionLines model
      ""
      Log.problemReport () ]
    |> String.concat "\n"

/// What the Copy button puts on the clipboard: the same, and the questions as a form.
///
/// **The questions travel with the report because that is where they get answered.** A checklist
/// read on one screen and typed into an email on another is a checklist half of which is
/// forgotten; one that arrives already in the email, above the evidence, is answered in place.
/// They come from `AppMessages.Info.bugReportQuestions`, the same list the tab renders.
let copyText (model: Model option) =
    let questions =
        AppMessages.Info.bugReportQuestions
        |> List.mapi (fun i q -> $"{i + 1}. {q}" + "\n")
        |> String.concat "\n"

    [ "=== Issie bug report ==="
      ""
      yield! environmentLines ()
      yield! sessionLines model
      ""
      "--- please answer these, then send the whole of this ---"
      ""
      questions
      Log.problemReport () ]
    |> String.concat "\n"

/// One line saying whether there is anything to send, which is the part somebody skims.
let private summaryLine () =
    let exceptions = Log.recentExceptions () |> List.length
    let problems = Log.recentProblems () |> Array.length

    match exceptions, problems with
    | 0, 0 -> "Nothing has gone wrong this session."
    | 0, n -> $"{n} error or warning message(s) this session, and no uncaught exceptions."
    | 1, n -> $"1 uncaught exception and {n} error or warning message(s) this session."
    | e, n -> $"{e} uncaught exceptions and {n} error or warning message(s) this session."

/// The block of text itself, styled as ErrorDisplay styles an error message - not monospace, on a
/// pale field, selectable by hand as well as by button, and scrolling rather than growing.
let private textBlock (text: string) =
    div
        [ Style
            [ WhiteSpace WhiteSpaceOptions.PreWrap
              UserSelect UserSelectOptions.Text
              Background "#f5f5f5"
              Border "1px solid #dbdbdb"
              BorderRadius "4px"
              Padding "8px"
              MarginTop "8px"
              MarginBottom "8px"
              MaxHeight Constants.maxBlockHeight
              OverflowY OverflowOptions.Auto ] ]
        [ str text ]

/// Copy the report, and say so. The notification is the whole confirmation: the clipboard gives
/// no other sign that a click did anything, and a user who is not sure clicks again.
let private copyButton (model: Model option) (dispatch: Msg -> unit) (label: string) =
    let copy _ =
        Bridge.clipboardWrite (copyText model)

        [ SetSimulationNotification(
              Notifications.successNotification
                  "Report copied - paste it into an email or a GitHub issue, and answer the questions in it"
                  CloseSimulationNotification)
          DispatchDelayed(Constants.copiedNotificationMs, CloseSimulationNotification) ]
        |> List.iter dispatch

    Button.button [ Button.Size IsSmall; Button.Color IsInfo; Button.OnClick copy ] [ str label ]

/// The Bug Reports tab's panel: what has gone wrong, foldable so that it does not bury the
/// checklist above it, and a button that puts the lot on the clipboard.
///
/// `details` rather than a model field, because whether a user has this open is not something the
/// application needs to know or to remember - and holding it in the model would mean a message,
/// an update case and a render of the whole application for a disclosure triangle.
let panel (model: Model) (dispatch: Msg -> unit) : ReactElement =
    div [ Style [ MarginTop "1em" ] ] [
        Heading.h5 [] [ str "This session" ]
        div [] [ str (summaryLine ()) ]
        details [] [
            summary [ Style [ Cursor "pointer"; MarginTop "8px" ] ] [ str "Show what will be sent" ]
            textBlock (viewText (Some model))
        ]
        copyButton (Some model) dispatch "Copy error report"
    ]

//-------------------------------------------------------------------------------------------------//
//------------------------------------ THE DEBUG-BUILD POPUP --------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// The most recent thing to have gone wrong, as one line for the top of the popup - which is what
/// somebody reads before deciding whether to care.
let private mostRecent () =
    match Log.recentExceptions (), Log.recentProblems () with
    | latest :: _, _ -> $"{latest.Source}: {latest.Message}"
    | [], lines when lines.Length > 0 -> lines[lines.Length - 1]
    | _ -> "(nothing recorded)"

/// The popup a debug build puts up when anything is logged as an error, or when an exception
/// reaches the boundary.
///
/// **This exists because a developer does not read the console.** Every error already reached the
/// buffer and the Bug Reports tab before this was written, and that was enough for a user being
/// asked for a report and no use at all to the person who could have fixed it: it says nothing at
/// the moment the mistake is made, which is the only moment at which anybody remembers what they
/// just did. So a debug build interrupts, and a release build does not.
///
/// The tickbox is the other half. Interrupting is only tolerable if it can be stopped, and an
/// error that repeats - a failing sidecar, a handler that throws on every drag - would otherwise
/// make the app unusable in exactly the build being used to debug it.
let popup: (Msg -> unit) -> Model -> ReactElement =
    let body (dispatch: Msg -> unit) (model: Model) =
        div [] [
            div [ Style [ MarginBottom "8px" ] ] [ str (mostRecent ()) ]
            div [ Style [ FontSize "0.85em"; Color "#7a7a7a" ] ] [
                str "Issie is not meant to produce these. Everything below is also on \
                     Info > Bug Reports, and this popup is in debug builds only."
            ]
            textBlock (viewText (Some model))
            Checkbox.checkbox [] [
                Checkbox.input [ Props [
                    Style [ MarginRight "5px" ]
                    Checked model.SuppressErrorPopups
                    OnChange(fun _ -> dispatch (SetSuppressErrorPopups(not model.SuppressErrorPopups)))
                ] ]
                str "Stop showing this for the rest of this session"
            ]
        ]

    let foot (dispatch: Msg -> unit) (model: Model) =
        div [] [
            copyButton (Some model) dispatch "Copy error report"
            Button.button
                [ Button.Size IsSmall
                  Button.Props [ Style [ MarginLeft "8px" ] ]
                  Button.OnClick(fun _ -> dispatch ClosePopup) ]
                [ str "Close" ]
        ]

    dynamicClosablePopupFunc "Issie logged an error" body foot [ Width 800 ]

/// What is drawn when drawing anything else threw.
///
/// Deliberately does not try to be the application: there is no dispatch it can trust, because
/// whatever the view choked on is still in the model, and a button that dispatches would run the
/// same render again. What it can do is say what happened, show it, and offer the clipboard - so
/// that a session which is over anyway still produces a usable bug report.
let crashPage (message: string) : ReactElement =
    div [ Style [ Padding "2em"; MaxWidth "60em" ] ] [
        Heading.h4 [] [ str "Issie could not draw the screen" ]
        div [] [
            str "This is a bug in Issie, not in your design. Your project files have not been \
                 changed by it. Please copy the report below and send it to us, then restart \
                 Issie."
        ]
        textBlock (viewText None)
        Button.button
            [ Button.Color IsInfo; Button.OnClick(fun _ -> Bridge.clipboardWrite (copyText None)) ]
            [ str "Copy error report" ]
        div [ Style [ MarginTop "1em"; FontSize "0.85em"; Color "#7a7a7a" ] ] [ str message ]
    ]
