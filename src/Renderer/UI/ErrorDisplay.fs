/// How an error message is put on screen when the user may need to send it to us.
///
/// Issie has two kinds of error. Most are about the design - a port with nothing driving it, two
/// labels with one name - and the user reads them, fixes the design and never thinks about them
/// again. A few are about ISSIE: a sheet file that cannot have been produced by the editor, a
/// simulator that hit a state it says is impossible, a check that admits it should not have
/// fired. Those say "please report it (Info -> Bug Reports)", and a report is only useful if it
/// carries the message.
///
/// **Which is which is not written down anywhere, and this deliberately does not ask.** Classifying
/// `SimulationErrorType` case by case would be a second list to keep in step with the first, and it
/// would go stale the moment a case was added. Length decides instead, and it decides correctly by
/// construction: every message that asks to be reported is long, because it has to explain that the
/// fault is Issie's and say what to do about it. A one-line message about a missing connection stays
/// exactly as it was.
///
/// Being long is also, on its own, a reason to make a message copyable - a user quoting a
/// two-hundred-character explanation in a forum question retypes it or drops half of it - so the
/// rule needs no exception for the design errors it also catches.
module ErrorDisplay

open Fable.React
open Fable.React.Props
open Fulma
open ModelType

module Constants =
    /// The longest message shown as plain text, and so the longest one a user is expected to retype
    /// or quote by hand.
    ///
    /// **It cannot be set to separate "report this" from "fix your design", and does not try.**
    /// Measured over the messages that exist: the one-line design errors are 73 characters, the
    /// longest piece of pure user advice is 182 ("You can't connect two Net Labels with a wire..."),
    /// and the shortest message that asks to be reported is 188 ("...if you are seeing this please
    /// report it"). Six characters apart. Any threshold drawn between those two would be decided by
    /// a rewording, so the threshold goes well below both and some design errors get a Copy button
    /// they do not strictly need.
    ///
    /// That is the right way round. A user quoting a long error in a forum question wants to copy it
    /// whoever it is addressed to, and the cost of an extra button is a button; the cost of the
    /// other mistake is a bug report with the message retyped from memory, or missing.
    let longestPlainMessage = 150

    /// How tall the block may get before it scrolls, so that a stack trace cannot push the rest of
    /// the error pane - the affected components, the Fix button - off the screen.
    let maxBlockHeight = "16em"

    /// How long the "copied" confirmation stays up.
    let copiedNotificationMs = 3000

/// Whether this message is short enough to leave as plain text.
///
/// A line break is disqualifying whatever the length: HTML collapses one, so a message written in
/// paragraphs arrives as a single run-on line - which is how a stack trace used to be shown, and
/// why it was unreadable as well as uncopyable.
let private isShort (text: string) =
    text.Length <= Constants.longestPlainMessage
    && not (text.Contains "\n")

/// One error message, as text the user can read and - when it is long enough to be worth it - copy.
///
/// The block preserves the line breaks the message was written with, which is the half of this that
/// is not about copying at all: `InternalError` puts a stack trace in the message, and `str` inside
/// a `div` rendered the whole of it as one line.
let errorMessage (dispatch: Msg -> unit) (text: string) : ReactElement =
    if isShort text then
        str text
    else
        let copy _ =
            Bridge.clipboardWrite text

            [ SetSimulationNotification(
                  Notifications.successNotification "Error message copied - please paste it into your bug report" CloseSimulationNotification)
              DispatchDelayed(Constants.copiedNotificationMs, CloseSimulationNotification) ]
            |> List.iter dispatch

        div [] [
            div
                [ Style
                    [ WhiteSpace WhiteSpaceOptions.PreWrap
                      // said explicitly: this is the text the button is about, and a message the
                      // user is asked to send us must be selectable by hand as well as by button
                      UserSelect UserSelectOptions.Text
                      // Not monospace. The block has to hold a stack trace, which invites it, but
                      // most of what lands here is prose - a long explanation of what is wrong with
                      // a sheet - and prose set in monospace on a grey field reads as a crash dump
                      // the user is not meant to understand. A stack trace is lines of text rather
                      // than columns, so it loses nothing.
                      Background "#f5f5f5"
                      Border "1px solid #dbdbdb"
                      BorderRadius "4px"
                      Padding "8px"
                      MarginBottom "8px"
                      MaxHeight Constants.maxBlockHeight
                      OverflowY OverflowOptions.Auto ] ]
                [ str text ]
            Button.button
                [ Button.Size IsSmall
                  Button.Color IsInfo
                  Button.OnClick copy ]
                [ str "Copy error message" ]
        ]
