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

/// Explanations, keyed by the exact label text the field shows.
///
/// Labels are written once, in the module that builds the field, and matched here verbatim: a
/// label that is reworded loses its tooltip rather than showing the wrong one, which is the safe
/// direction for a mismatch to fail in.
let private fieldTips: Map<string, string> =
    Map [
        // ---- identity ----
        "Name",
            "The label drawn on the symbol and used for this component everywhere else: in error \
             messages, in the waveform viewer, and in generated Verilog. It must be unique on this \
             sheet."
        "Instance name",
            "This copy's own label. The sheet it is an instance of keeps its own name - renaming \
             here renames only this copy."

        // ---- widths ----
        "Width (bits)",
            "How many bits wide this component's bus is. Widths must agree at both ends of every \
             wire, so changing this here is usually the fix for a 'wrong wire width' error. It can \
             be an expression in the sheet's parameters, such as WIDTH or WIDTH+1."
        "Output width (bits)",
            "How many bits the output bus has. The single input bit is copied onto every one of \
             them."
        "Top (LSB) output width (bits)",
            "How many of the input's bits go to the top output. The rest go to the bottom one, so \
             the two together always add up to the input width. Flip the component vertically \
             (Ctrl+Down) if you want the least significant bits at the bottom instead."
        "Width",
            "How many bits this output takes from the input bus."
        "LSB",
            "The bit of the input bus this output starts at, counting from 0 at the least \
             significant end."
        "Least Significant Bit number selected: lsb",
            "The bit this selection starts at, counting from 0 at the least significant end. With \
             a width of 4 and an LSB of 8 the output is bits 11 down to 8 of the input."
        "Compare with",
            "The output is 1 when the input bus equals this value and 0 otherwise. Write it in \
             decimal, or with an 0x or 0b prefix for hex or binary."
        "Default value if input is undriven",
            "The value this input takes in simulation when nothing drives it - which is the case \
             for the top sheet's own inputs in the waveform viewer. Both simulators use it, so it \
             is the place to set the input values a waveform simulation should start from."

        // ---- shape ----
        "Number of inputs",
            "How many input ports this component has. Reducing it deletes the wires on the ports \
             that go away, so this is a change to the schematic and cannot be set by a parameter."
        "Number of outputs",
            "How many output ports this component has. Each gets its own width and starting bit \
             below. Reducing it deletes the wires on the ports that go away."
        "Optional Ports",
            "Ports you do not need can be removed rather than tied off. An unticked Cin behaves as \
             0; an unticked Cout simply is not there, so nothing has to be connected to it."
        "Optional Inputs",
            "Ports you do not need can be removed rather than tied off. Without Load the counter \
             only counts; without Enable it counts every clock cycle."
        "Ports",
            "The inputs and outputs this instance has, with their widths, taken from the sheet it \
             is an instance of. Hold Ctrl and drag a port to move it to another edge of the symbol."

        // ---- appearance ----
        "Width Scale",
            "Stretches the symbol horizontally. Issie sizes a custom component to fit its port \
             labels; set this only when you want a particular shape."
        "Height Scale",
            "Stretches the symbol vertically. Issie sizes a custom component to fit its ports; set \
             this only when you want a particular shape."

        // ---- values ----
        "Enter constant value in decimal, hex, or binary:",
            "The value this component drives, written however is clearest: 42, 0x2a or 0b101010. \
             It is redisplayed in the form you typed it."
        "Enter bus compare value in decimal, hex, or binary:",
            "The output is 1 when the input bus equals this value and 0 otherwise. Write it \
             however is clearest: 42, 0x2a or 0b101010."

        // ---- the sheet itself ----
        "Sheet Description",
            "A sentence about what this sheet does. It is shown against the sheet in the Sheet \
             menu, and in this pane wherever the sheet is used as a custom component - so it is \
             read by whoever uses your sheet, not only by you."
    ]

/// A field label carrying its explanation, where there is one to give.
///
/// Marked with a dotted underline and a help cursor, so that a label with something to say looks
/// different from one without: an invisible tooltip is one nobody hovers.
let fieldLabel (name: string) : ReactElement =
    match Map.tryFind name fieldTips with
    | None -> Label.label [] [ str name ]
    | Some tip ->
        Label.label [
            Label.Props [
                // bottom rather than the default top: the pane is tall and narrow, and a field
                // near its top edge would put a tooltip above it off the screen
                HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsTooltipBottom}"
                Tooltip.dataTooltip tip
                Style [
                    Cursor "help"
                    BorderBottom "1px dotted #9a9a9a"
                    Display DisplayOptions.InlineBlock
                ]
            ]
        ] [ str name ]
