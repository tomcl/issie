/// What the fields in the Properties pane mean, and the label element that says so.
///
/// The Catalogue explains every component before you place it, in a tooltip written as a sentence.
/// The pane where you then configure the component you placed explained nothing at all: it showed
/// "Width (bits)", "Optional Ports", "LSB" and left the user to work out the rest. This closes that
/// gap without adding prose to the pane itself, which is short on room.
///
/// The text is keyed by the label the field displays, so a field acquires its explanation simply by
/// being labelled - no call site has to pass one, and the same label used in two places is
/// explained the same way in both. A label with no entry here renders exactly as it did before.
///
/// Keep each entry to what the field does and, where there is one, the thing people get wrong.
module PropertiesHelp

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

/// The explanations live in AppMessages.Fields, with the rest of Issie's longer text, so that
/// they can be read and reviewed as writing rather than found one at a time in the pane.

/// A field label carrying its explanation, where there is one to give.
///
/// The help cursor is the only marking: a dotted underline under every explained label put a row
/// of broken rules down a pane that is mostly labels, which read as structure that was not there.
///
/// The tooltip goes on a span INSIDE the label rather than on the label itself, because it needs
/// an element that shrinks to the width of the words - a tooltip anchored to a full-width box
/// points at nothing in particular - and a label made inline-block to achieve that stops being a
/// line of its own, so the field's input box comes up beside it and covers the words.
let fieldLabel (name: string) : ReactElement =
    match Map.tryFind name AppMessages.Fields.tips with
    | None -> Label.label [] [ str name ]
    | Some tip ->
        Label.label [] [
            span [
                // bottom rather than the default top: the pane is tall and narrow, and a field
                // near its top edge would put a tooltip above it off the screen. field-tip opens
                // it to the right rather than centred - see extra.css.
                HTMLAttr.ClassName
                    $"field-tip {Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsTooltipBottom}"
                Tooltip.dataTooltip tip
                Style [
                    Cursor "help"
                    Display DisplayOptions.InlineBlock
                ]
            ] [ str name ]
        ]
