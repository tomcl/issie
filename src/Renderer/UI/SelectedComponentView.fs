(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open Helpers
open DiagramModelType
open DiagramMessageType
open CommonTypes
open MemoryEditorView
open PopupView

let extractLabelBase (text:string) : string =
    text.ToUpper()
    |> Seq.takeWhile (fun ch -> ch <> '(')
    |> Seq.filter System.Char.IsLetterOrDigit
    |> Seq.map (fun ch -> ch.ToString())
    |> String.concat ""

let formatLabelAsBus (width:int) (text:string) =
    let text' = extractLabelBase text
    match width with
    | 1 -> text'
    | _ -> sprintf "%s(%d:%d)" text' (width-1) 0
   

let formatLabel comp (text:string) =
    let text' = extractLabelBase text
    match comp.Type with
    | Input 1 | Output 1 -> text'
    | Input width | Output width -> sprintf "%s(%d:%d)" text' (width-1) 0
    | _ -> text'

let setComponentLabel model comp text =
    let label = formatLabel comp text
    printf "Setting label %s" label
    model.Diagram.EditComponentLabel comp.Id label

let setComponentLabelFromText model (comp:Component) text =
    printf "Setting label %s" text
    model.Diagram.EditComponentLabel comp.Id text

let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]

let private textFormField name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Control.div [] [ Input.text [
            Input.Props [ Name name; ]
            Input.DefaultValue defaultValue
            Input.OnChange (getTextEventValue >> onChange)
        ] ]
    ]

let private intFormField name defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width "60px"]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private makeMemoryInfo descr mem compId model dispatch =
    div [] [
        str descr
        br []; br []
        str <| sprintf "Address width: %d bit(s)" mem.AddressWidth
        br []
        str <| sprintf "Number of elements: %d" (pow2 mem.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" mem.WordWidth
        br []; br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> openMemoryEditor mem compId model dispatch)
        ] [str "View/Edit memory content"]
    ]

let private makeNumberOfBitsField model comp text setter dispatch =
    let title, width =
        match comp.Type with
        | Input w | Output w | NbitsAdder w | Register w -> "Number of bits", w
        | SplitWire w -> "Number of bits in the top wire", w
        | c -> failwithf "makeNumberOfBitsField called with invalid component: %A" c
    intFormField title width 1 (
        fun newWidth ->
            if newWidth < 1
            then
                errorNotification "Invalid number of bits." ClosePropertiesNotification
                |> SetPropertiesNotification |> dispatch
            else
                setter comp.Id newWidth // change the JS component
                printfn "new width = %d" newWidth
                let text' = formatLabelAsBus newWidth text
                setComponentLabelFromText model comp text' // change the JS component label
                dispatch ReloadSelectedComponent // reload the new component
                dispatch ClosePropertiesNotification
    )

let private makeDescription comp model dispatch =
    match comp.Type with
    | Input _ -> str "Input."
    | Output _ -> str "Output."
    | IOLabel -> str "Label on Wire or Bus. Labels with the same name connect wires or busses."
    | Not | And | Or | Xor | Nand | Nor | Xnor ->
        div [] [ str <| sprintf "%A gate." comp.Type ]
    | Mux2 -> div [] [ str "Multiplexer with two inputs and one output." ]
    | Demux2 -> div [] [ str "Demultiplexer with one input and two outputs." ]
    | MergeWires -> div [] [ str "Merge two wires of width n and m into a single wire of width n+m." ]
    | SplitWire _ -> div [] [ str "Split a wire of width n+m into two wires of width n and m."]
    | NbitsAdder numberOfBits -> div [] [ str <| sprintf "%d bit(s) adder." numberOfBits ]
    | Custom custom ->
        let toHTMLList =
            List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
        div [] [
            str <| sprintf "%s: user defined component." custom.Name
            br []
            span [Style [FontStyle "italic"]] [str <| "Inputs"]
            ul [] (toHTMLList custom.InputLabels)
            span [Style [FontStyle "italic"]] [str <| "Outputs"]
            ul [] (toHTMLList custom.OutputLabels)
        ]
    | DFF -> div [] [ str "D-flip-flop. The component is implicitly connected to the global clock." ]
    | DFFE -> div [] [
        str "D-flip-flop with enable. If the enable signal is high the state of
             the D-flip-flop will be updated at the next clock cycle.
             The component is implicitly connected to the global clock." ]
    | Register _  -> div [] [ str "Register. The component is implicitly connected to the global clock." ]
    | RegisterE _ ->
        div [] [ str "Register with enable. If the enable signal is high the
                      state of the Register will be updated at the next clock
                      cycle. The component is implicitly connected to the global
                      clock." ]
    | AsyncROM mem ->
        let descr = "Asynchronous ROM: the output is updated as soon as the address changes."
        makeMemoryInfo descr mem comp.Id model dispatch
    | ROM mem ->
        let descr = "Synchronous ROM: the output is updated only after a clock tick. The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem comp.Id model dispatch
    | RAM mem ->
        let descr =
            "RAM memory. At every clock tick, the RAM can either read or write
            the content of the memory location selected by the address. If the
            write signal is high, the content of the selected memory location
            is set to the value of data-in. This value will also be propagated
            to data-out immediately. The component is implicitly connected to
            the global clock."
        makeMemoryInfo descr mem comp.Id model dispatch

let private makeExtraInfo model comp text dispatch =
    match comp.Type with
    | Input _ | Output _ | NbitsAdder _ ->
        makeNumberOfBitsField model comp text model.Diagram.SetNumberOfBits dispatch
    | SplitWire _ ->
        makeNumberOfBitsField model comp text model.Diagram.SetTopOutputWidth dispatch
    | Register _ ->
        makeNumberOfBitsField model comp text model.Diagram.SetRegisterWidth dispatch
    | _ -> div [] []

let viewSelectedComponent model dispatch =
    match model.SelectedComponent with
    | None -> div [] [ str "Select a component in the diagram to view/edit its properties." ]
    | Some comp ->
        printfn "Comp=%A %A" comp.Type comp.Label

        div [Key comp.Id] [
            let label' = extractLabelBase comp.Label
            readOnlyFormField "Description" <| makeDescription comp model dispatch
            makeExtraInfo model comp label' dispatch
            textFormField "Label" label' (fun text -> 
                printfn "dispatch text=%A, compType=%A" text comp.Type
                setComponentLabel model comp (formatLabel comp text)
                dispatch ReloadSelectedComponent // reload the new component
                )
        ]
