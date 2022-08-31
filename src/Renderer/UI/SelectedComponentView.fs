(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open VerilogTypes
open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupView
open Notifications
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open CatalogueView
open FileMenuView


let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]




let private textFormField isRequired name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ OnPaste preventDefault; SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.CustomClass "www"
            Input.Placeholder (if isRequired then "Name (required)" else "Name (optional)")
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]

let private textFormFieldSimple name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ OnPaste preventDefault; SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]


let private intFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private int64FormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getInt64EventValue >> onChange)
        ]
    ]

let private intFormFieldNoMin name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width "60px"]]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private int64FormFieldNoMin name (defaultValue:int64) (currentText:string option) onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [Style [Width "180px"]]
            Input.DefaultValue <| Option.defaultValue $"{defaultValue}" currentText
            Input.OnChange (getTextEventValue >> onChange)
        ]
    ]


let getInitSource (mem: Memory1) =
    let a = mem.AddressWidth
    match mem.Init with
    | SignedMultiplier -> 
        $"Dout = (signed) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | UnsignedMultiplier ->
        $"Dout = (unsigned) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | FromData ->
        "See Memory Viewer"
    | FromFile name | ToFile name | ToFileBadName name ->
        $"From '{name}.ram' file"
   

let private makeMemoryInfo descr mem compId cType model dispatch =
    let projectPath = (Option.get model.CurrentProj).ProjectPath
    div [] [
        str descr
        br []; br []
        str <| sprintf "Address width: %d bit(s)" mem.AddressWidth
        br []
        str <| sprintf "Number of elements: %d" (1UL <<< mem.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" mem.WordWidth
        br []
        str <| sprintf "%sData: %s"  
                (match cType with 
                 | RAM1 _ | AsyncRAM1 _ -> "Initial "
                 | ROM1 _ | AsyncROM1 _ -> ""
                 | _ -> failwithf $"What - wrong component type ({cType} here")
                (getInitSource mem)
        br []
        //makeSourceMenu model (Option.get model.CurrentProj) mem dispatch
        br []; br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> openMemoryEditor mem compId model dispatch)
        ] [str "View/Edit memory content"]
        br []; br [];
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> 
                FilesIO.openWriteDialogAndWriteMemory mem projectPath
                |> (function
                        | None -> ()
                        | Some path ->
                            let note = successPropertiesNotification $"Memory content written to '{path}'"
                            dispatch <| SetPropertiesNotification note)
                )
        ] [str "Write content to file"]

    ]

let makeVerilogEditButton model (custom:CustomComponentType) dispatch : ReactElement = 
    
    let openCodeEditor code name = 
        let project' =
            match model.CurrentProj with
            |Some proj -> {proj with WorkingFileName = Some (custom.Name)}
            |None -> failwithf "Can't happen!"
        let model'= {model with CurrentProj = Some project'} 
        createVerilogPopup model false (Some code) (Some name) (UpdateVerilogFile name)
    
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        match custom.Form with
        |Some (Verilog name) ->
            let folderPath = project.ProjectPath
            let path = pathJoin [| folderPath; name + ".v" |]
            let code = 
                match tryReadFileSync path with
                |Ok text -> text
                |Error _ -> sprintf "Error: file {%s.v} has been deleted from the project directory" name
            Button.button [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> 
                    dispatch (StartUICmd SaveSheet)
                    saveOpenFileActionWithModelUpdate model dispatch |> ignore
                    dispatch <| Sheet(SheetT.DoNothing)
                    openCodeEditor code name dispatch)
            ] [str "View/Edit Verilog code"]
        |_ -> null

let makeVerilogDeleteButton (model:Model) (custom:CustomComponentType) dispatch : ReactElement = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        match custom.Form with
        |Some (Verilog name) ->
            let title = "Delete sheet"

            let body =
                div []
                    [ 
                        str "Are you sure you want to delete the following Verilog component?"
                        br []
                        str <| pathJoin
                                    [| project.ProjectPath;
                                    name + ".v" |]
                        br []
                        str <| "This action is irreversible." ]

            let buttonText = "Delete"

            let buttonAction =
                fun _ ->
                    dispatch (StartUICmd DeleteSheet)
                    dispatch <| ExecFuncInMessage(removeFileInProject name project,dispatch)
                    (removeFileWithExtn ".v" project.ProjectPath name)
                    dispatch ClosePopup
            Button.button
                [ 
                Button.IsOutlined
                Button.Color IsDanger
                Button.OnClick(fun _ -> confirmationPopup title body buttonText buttonAction dispatch) ]
                [ str "Delete" ]
        | _ -> null

let private changeAdderType model (comp:Component) dispatch = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
        let colourCin,colourCout =
            match comp.Type with
            |NbitsAdder w ->
                IsPrimary, IsPrimary 
            |NbitsAdderNoCout w ->
                IsPrimary, IsDanger
            |NbitsAdderNoCin w ->
                IsDanger, IsPrimary 
            |NbitsAdderNoCinCout w ->
                IsDanger, IsDanger
            | _ -> failwithf "Cannot change adder type from non-adder component"

        let buttonActionCin =
            fun _ ->
                match comp.Type with
                |NbitsAdder w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCin w)
                |NbitsAdderNoCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCinCout w)
                |NbitsAdderNoCin w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdder w)
                |NbitsAdderNoCinCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCout w)
                | _ -> failwithf "Cannot change adder type from non-adder component"
        let buttonActionCout =
            fun _ ->
                match comp.Type with
                |NbitsAdder w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCout w)
                |NbitsAdderNoCin w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCinCout w)
                |NbitsAdderNoCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdder w)
                |NbitsAdderNoCinCout w ->
                    model.Sheet.ChangeAdderComp sheetDispatch (ComponentId comp.Id) (NbitsAdderNoCin w)
                |_ -> failwithf "Cannot change adder type from non-adder component"

        div [] [
            Button.button
                [ 
                Button.IsOutlined
                Button.Color colourCin
                Button.OnClick(buttonActionCin)
                ]
                [ str "Cin" ]
            Button.button
                [ 
                Button.IsOutlined
                Button.Color colourCout
                Button.OnClick(buttonActionCout)
                ]
                [ str "Cout" ]
            ]

let private makeNumberOfBitsField model (comp:Component) text dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let title, width =
        match comp.Type with
        | Input1 (w, _) | Output w | NbitsAdder w  | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w 
        | NbitsXor w | NbitsAnd w | NbitsOr w |NbitsNot w |NbitSpreader w 
        | Register w | RegisterE w | Viewer w -> "Number of bits", w
        | SplitWire w -> "Number of bits in the top (LSB) wire", w
        | BusSelection( w, _) -> "Number of bits selected: width", w
        | BusCompare( w, _) -> "Bus width", w
        | Constant1(w, _,_) -> "Number of bits in the wire", w
        | c -> failwithf "makeNumberOfBitsField called with invalid component: %A" c
    intFormField title "60px" width 1 (
        fun newWidth ->
            if newWidth < 1
            then
                let props = errorPropsNotification "Invalid number of bits."
                dispatch <| SetPropertiesNotification props
            else
                model.Sheet.ChangeWidth sheetDispatch (ComponentId comp.Id) newWidth
                let text' = match comp.Type with | BusSelection _ -> text | _ -> formatLabelAsBus newWidth text
                //SetComponentLabelFromText model comp text' // change the JS component label
                let lastUsedWidth = 
                    match comp.Type with 
                    | SplitWire _ | BusSelection _ | Constant1 _ -> 
                        model.LastUsedDialogWidth 
                    | _ ->  
                        newWidth
                dispatch (ReloadSelectedComponent (lastUsedWidth)) // reload the new component
                dispatch <| SetPopupDialogInt (Some newWidth)
                dispatch ClosePropertiesNotification
    )

/// Used for Input1 Component types. Make field for users to enter a default value for
/// Input1 Components when they are undriven.
let makeDefaultValueField (model: Model) (comp: Component) dispatch: ReactElement =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let title = "Default value if input is undriven"

    let width, defValue =
        match comp.Type with
        | Input1 (w, defValue) ->
            match defValue with
            | Some defValue -> w, defValue
            | None -> w, 0
        | _ -> failwithf "Other component types should not call this function."
    
    intFormField title "60px" defValue 0 (
        fun newValue ->
            // Check if value is within bit range
            match NumberHelpers.checkWidth width (int64 newValue) with
            | Some msg ->
                let props = errorPropsNotification msg
                dispatch <| SetPropertiesNotification props
            | None ->
                model.Sheet.ChangeInputValue sheetDispatch (ComponentId comp.Id) newValue
                // reload the new component
                dispatch (ReloadSelectedComponent (model.LastUsedDialogWidth))
                dispatch <| SetPopupDialogInt (Some newValue)
                dispatch ClosePropertiesNotification
    )

let mockDispatchS msgFun msg =
    match msg with
    | Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol sMsg)) ->
        msgFun msg
    | _ -> ()



let msgToS = 
    BusWireT.Msg.Symbol >> SheetT.Msg.Wire >> Msg.Sheet
  
/// Return dialog fileds used by constant, or default values
let constantDialogWithDefault (w,cText) dialog =
    let w = Option.defaultValue w dialog.Int
    let cText = Option.defaultValue cText dialog.Text
    w, cText

/// Create react to chnage constant properties
let makeConstantDialog (model:Model) (comp: Component) (text:string) (dispatch: Msg -> Unit): ReactElement =
        let symbolDispatch msg = dispatch <| msgToS msg
        let wComp, txtComp =
            match comp.Type with | Constant1( w,_,txt) -> w,txt | _ -> failwithf "What? impossible" 
        let w = Option.defaultValue wComp model.PopupDialogData.Int
        let cText = Option.defaultValue txtComp model.PopupDialogData.Text
        let reactMsg, compTOpt = CatalogueView.parseConstant w cText
        match compTOpt with
        | None -> ()
        | Some (Constant1(w,cVal,cText) as compT) ->
            if compT <> comp.Type then
                model.Sheet.ChangeWidth (Sheet >> dispatch) (ComponentId comp.Id) w
                symbolDispatch <| SymbolT.ChangeConstant (ComponentId comp.Id, cVal, cText)
                dispatch (ReloadSelectedComponent w)
                dispatch ClosePropertiesNotification
        | _ -> failwithf "What? impossible"

        div [] [
                makeNumberOfBitsField model comp text dispatch
                br []
                reactMsg
                br []
                textFormFieldSimple 
                    "Enter constant value in decimal, hex, or binary:" 
                    cText 
                    (fun txt -> 
                        printfn $"Setting {txt}"
                        dispatch <| SetPopupDialogText (Some txt))
                
            ]              

let private makeLsbBitNumberField model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let lsbPos, infoText =
        match comp.Type with 
        | BusSelection(width,lsb) -> uint32 lsb, "Least Significant Bit number selected: lsb"
        | BusCompare(width,cVal) -> cVal, "Compare with"
        | _ -> failwithf "makeLsbBitNumberfield called from %A" comp.Type

    match comp.Type with
    | BusCompare(width, _) -> 
        intFormField infoText "120px"  (int lsbPos) 1  (
            fun cVal ->
                if cVal < 0 || uint32 cVal > uint32 ((1 <<< width) - 1)
                then
                    let note = errorPropsNotification <| sprintf "Invalid Comparison Value for bus of width %d" width
                    dispatch <| SetPropertiesNotification note
                else
                    model.Sheet.ChangeLSB sheetDispatch (ComponentId comp.Id) (int64 cVal)
                    dispatch (ReloadSelectedComponent (width)) // reload the new component
                    dispatch ClosePropertiesNotification
        )
    | BusSelection(width, _) -> 
        intFormField infoText "60px" (int lsbPos) 1 (
            fun newLsb ->
                if newLsb < 0
                then
                    let note = errorPropsNotification "Invalid LSB bit position"
                    dispatch <| SetPropertiesNotification note
                else
                    model.Sheet.ChangeLSB sheetDispatch (ComponentId comp.Id) (int64 newLsb)
                    dispatch (ReloadSelectedComponent (width)) // reload the new component
                    dispatch ClosePropertiesNotification
        )
    | _ -> failwithf "What? invalid component for lsbpos in properties"



let private makeDescription (comp:Component) model dispatch =
    match comp.Type with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | Input1 _ -> str "Input."
    | Constant1 _ | Constant _ -> str "Constant Wire."
    | Output _ -> str "Output."
    | Viewer _ -> str "Viewer."
    | BusCompare _ -> str "The output is one if the bus unsigned binary value is equal to the integer specified. This will display in hex on the design sheet, and decimal in this dialog. Busses of greater than 32 bits are not supported"
    | BusSelection _ -> div [] [
                str "Bus Selection."
                br []
                str "The output is the subrange [width+lsb-1..lsb] of the input bits. If width = 1 this selects one bit. Error if the input has less than width + lsb bits."
                br []
                br []
                str "Note that the output bit(s) are numbered from 0 even if the input range has LS bit number > 0. \
                     The input bits connected are displayed in the schematic symbol"
        ]
    | IOLabel -> div [] [
        str "Label on Wire or Bus. Labels with the same name connect wires. Each label has input on left and output on right. \
            No output connection is required from a set of labels. Since a set represents one wire of bus, exactly one input connection is required. \
            Labels can be used:"  
        br [] ;
        str "To name wires and document designs."; br []
        str "To join inputs and outputs without wires."; br []
        str "To prevent an unused output from giving an error."
        ]
    | Not | And | Or | Xor | Nand | Nor | Xnor ->
        div [] [ str <| sprintf "%A gate." comp.Type ]
    | Mux2 -> div [] [ 
        str "Multiplexer with two inputs and one output." 
        br []
        br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> model.Sheet.ChangeReversedInputs (Sheet >> dispatch) (ComponentId comp.Id))
            ] 
            [str "Reverse Inputs"]
        ]
    | Mux4 -> div [] [ str "Multiplexer with four inputs and one output." ]
    | Mux8 -> div [] [ str "Multiplexer with eight inputs and one output." ]
    | Demux2 -> div [] [ str "Demultiplexer with one input and two outputs." ]
    | Demux4 -> div [] [ str "Demultiplexer with one input and four outputs." ]
    | Demux8 -> div [] [ str "Demultiplexer with one input and eight outputs." ]
    | MergeWires -> div [] [ str "Merge two wires of width n and m into a single wire of width n+m." ]
    | SplitWire _ -> div [] [ str "Split a wire of width n+m into two wires of width n and m."]
    | NbitsAdder numberOfBits 
    | NbitsAdderNoCin numberOfBits 
    | NbitsAdderNoCout numberOfBits 
    | NbitsAdderNoCinCout numberOfBits 
        -> div [] [ str <| sprintf "%d bit(s) adder." numberOfBits ]
    | NbitsXor numberOfBits  -> div [] [ str <| sprintf "%d XOR gates with %d outputs." numberOfBits numberOfBits]
    | NbitsAnd numberOfBits  -> div [] [ str <| sprintf "%d AND gates with %d outputs." numberOfBits numberOfBits]
    | NbitsOr numberOfBits  -> div [] [ str <| sprintf "%d OR gates with %d outputs." numberOfBits numberOfBits]
    | NbitsNot numberOfBits  -> div [] [ str <| sprintf "%d NOT gates with %d outputs." numberOfBits numberOfBits]
    | NbitSpreader numberOfBits  -> div [] [ str <| sprintf "One input Bit Spreader with one %d-bit output." numberOfBits]
    | Decode4 -> div [] [ str <| "4 bit decoder: Data is output on the Sel output, all other outputs are 0."]
    | Custom custom ->
        let styledSpan styles txt = span [Style styles] [str <| txt]
        let boldSpan txt = styledSpan [FontWeight "bold"] txt
        let italicSpan txt = styledSpan [FontStyle "italic"] txt

        let toHTMLList =
            List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
        
        let symbolExplanation =
            match custom.Form with
            |Some (Verilog _) -> ": Verilog Component."
            |_ -> ": user defined (custom) component."
            //TODO: remaining

        let portOrderExplanation =
            match custom.Form with
            |Some (Verilog _) -> $"Input or Output ports are displayed on the '{custom.Name}' symbol sorted by the \
                    port definition order in the original Verilog file."
            |_ -> $"Input or Output ports are displayed on the '{custom.Name}' symbol sorted by the \
                    vertical position on the design sheet of the Input or Output components at the time the symbol is added."
            //TODO: remaining

        div [] [
            boldSpan $"{custom.Name}"
            span [] [str <| symbolExplanation]
            br []
            br []
            makeVerilogEditButton model custom dispatch
            br []
            br []
            makeVerilogDeleteButton model custom dispatch
            br []
            br []
            p [  Style [ FontStyle "italic"; FontSize "12px"; LineHeight "1.1"]] [
                str <| portOrderExplanation]
            
            span [Style [FontWeight "bold"; FontSize "15px"]] [str <| "Inputs"]
            ul [] (toHTMLList custom.InputLabels)
            br []
            span [Style [FontWeight "bold"; FontSize "15px"]] [str <| "Outputs"]
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
    | AsyncROM1 mem ->
        let descr = "Asynchronous ROM: the output is updated as soon as the address changes."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | ROM1 mem ->
        let descr = "Synchronous ROM: the output is updated only after a clock tick. The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | RAM1 mem ->
        let descr =
            "synchronous read and write RAM memory. 
            At every clock tick, the RAM can read and optionally write
            the content of the memory location selected by the address. If the
            write signal is high, the content of the selected memory location
            is set to the value of data-in. In cycle 0 data-out is 0, otherwise
            data-out is the contents of the memory location addressed in the
            previous cycle, before any optional write.
            The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch
    | AsyncRAM1 mem ->
        let descr =
            "Asynchronous read, synchronous write RAM memory. 
            At every clock tick, optionally write
            the content of the memory location selected by the address. If the
            write signal is high, the content of the selected memory location
            is set to the value of data-in. data-out is the contents of the memory 
            location addressed by the current cycle addres.
            The component is implicitly connected to the global clock."
        makeMemoryInfo descr mem (ComponentId comp.Id) comp.Type model dispatch

let private makeExtraInfo model (comp:Component) text dispatch : ReactElement =
    match comp.Type with
    | Input1 _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                makeDefaultValueField model comp dispatch
            ]
    | Output _ |NbitsAnd _ |NbitsOr _ |NbitsNot _ |NbitSpreader _ | NbitsXor _ | Viewer _ ->
        makeNumberOfBitsField model comp text dispatch
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                changeAdderType model comp dispatch
            ]
    | SplitWire _ ->
        makeNumberOfBitsField model comp text dispatch
    | Register _ | RegisterE _ ->
        makeNumberOfBitsField model comp text dispatch
    | BusSelection _ -> 
        div [] [
            makeNumberOfBitsField model comp text dispatch
            makeLsbBitNumberField model comp dispatch
            ]
    | BusCompare _ -> 
        div [] [
            makeNumberOfBitsField model comp text dispatch
            makeLsbBitNumberField model comp dispatch
            ]

    | Constant1 _ ->         
             makeConstantDialog model comp text dispatch
    | _ -> div [] []


let viewSelectedComponent (model: ModelType.Model) dispatch =
    let allowNoLabel =
        let symbols = model.Sheet.Wire.Symbol.Symbols
        match model.Sheet.SelectedComponents with
        | [cid] ->
            match Map.tryFind cid symbols with
            | Some {Component ={Type=MergeWires | SplitWire _ | BusSelection _}} -> true
            | _ -> false
        | _ -> false
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let formatLabelText (txt: string) =
        txt.ToUpper()
        |> Seq.filter (function | ch when System.Char.IsLetterOrDigit ch -> true 
                                | '.' -> true 
                                | '_' -> true 
                                | _ -> false)
        |> Seq.skipWhile (System.Char.IsLetter >> not)
        |> Seq.map string
        |> String.concat ""
        |> (fun chars -> 
            match String.length chars with 
            | 0 when allowNoLabel -> 
                Some ""
            | 0 -> 
                None 
            | _ -> 
                Some chars)
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [
            // let label' = extractLabelBase comp.Label
            // TODO: normalise labels so they only contain allowed chars all uppercase
            let label' = Option.defaultValue "" (formatLabelText comp.Label) // No formatting atm
            readOnlyFormField "Description" <| makeDescription comp model dispatch
            makeExtraInfo model comp label' dispatch
            let required = 
                match comp.Type with 
                | SplitWire _ | MergeWires | BusSelection _ -> false | _ -> true
            textFormField required "Component Name" label' (fun text ->
                // TODO: removed formatLabel for now
                //setComponentLabel model sheetDispatch comp (formatLabel comp text)
                match formatLabelText text with
                | Some label -> 
                    setComponentLabel model sheetDispatch comp label
                    dispatch <| SetPopupDialogText (Some label)
                | None -> ()
                //updateNames model (fun _ _ -> model.WaveSim.Ports) |> StartWaveSim |> dispatch
                dispatch (ReloadSelectedComponent model.LastUsedDialogWidth) // reload the new component
                )
        ]    
    | _ -> div [] [ str "Select a component in the diagram to view or change its properties, for example number of bits." ]

