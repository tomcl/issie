(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView
open EEExtensions
open VerilogTypes
open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open NumberHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupHelpers
open UIPopups
open Notifications
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open CatalogueView
open TopMenuView
open MenuHelpers
open ParameterTypes

open CatalogueView.Constants

module Constants =
    let labelUniqueMess = "Components must have a unique label within one sheet"
    let dropDownHeightFraction = 2.5

    


/// The name box.
///
/// Delete pressed with the caret at the end of this box used to delete the selected component,
/// because there was no other way to get the keyboard back to the schematic without the mouse.
/// Return now does that properly - see KeyTypes.ScLeaveTextBox - so one key no longer means two
/// things depending on where the caret happens to be.
let private textFormField isRequired name defaultValue isBad onChange =
    Field.div [] [
        PropertiesHelp.fieldLabel name
        Input.text [
            Input.Props [
                Id "labelInputElement";
                OnPaste preventDefault;
                SpellCheck false;
                Name name;
                Style [ Width "200px"] ]
            Input.DefaultValue defaultValue
            // the field is labelled, so the placeholder says the one thing the label does not
            Input.Placeholder (if isRequired then "required" else "optional")
            Input.OnChange (getTextEventValue >> onChange)
        ]
        br []
        span [Style [FontStyle "Italic"; Color "Red"]; Hidden (isBad = None)] [str <| Option.defaultValue "" isBad]
    ]

let private textFormFieldSimple name defaultValue onChange =
    Field.div [] [
        PropertiesHelp.fieldLabel name
        Input.text [
            // no AutoFocus: selecting a component must leave the keyboard on the schematic, or
            // every canvas shortcut dies the moment you click a Constant. You click into a box to
            // type in it, and press Return to leave.
            Input.Props [ OnPaste preventDefault; SpellCheck false; Name name; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]


let private intFormField name (width:string) (defaultValue: bigint) (minValue: int)  onChange =
    Field.div [] [
        PropertiesHelp.fieldLabel name
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%A" defaultValue
            Input.OnChange (getBigintEventValue >> onChange)
        ]
    ]

let private intFormField2 name bits (width: string) defaultValue1 defaultValue2 minValue1 minValue2 onChange1 onChange2 =
    Field.div [Field.Props[Style [Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center]]] [
        Label.label [Label.Props [Style [Width "115px"; Flex "0 0 auto";]]] [str name]
        Label.label [Label.Props [Style [TextAlign TextAlignOptions.Center; Width "50px"; MarginRight "8px"]]] [str bits] 
        Input.number [
            Input.Props [Style [Width width;]; Min minValue1] 
            Input.DefaultValue <| sprintf "%d" defaultValue1
            Input.OnChange (getIntEventValue >> onChange1)
        ]
        Input.number [
            Input.Props [Style [Width width; MarginLeft "15px"]; Min minValue2] 
            Input.DefaultValue <| sprintf "%d" defaultValue2
            Input.OnChange (getIntEventValue >> onChange2)
        ]
        hr []
    ]

let private floatFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        PropertiesHelp.fieldLabel name
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%A" defaultValue
            Input.OnChange (getFloatEventValue >> onChange)
        ]
    ]

/// The one sentence eight clocked components used to repeat word for word.
let private clockNote = "Implicitly connected to the global clock."

/// The kind of component this is, in the words a user would use for it.
///
/// Not Symbol.getComponentLegend, which is the text drawn inside the symbol: that bakes in bit
/// counts and rotation, and the pane shows the width in its own box right below.
/// Legacy types are named rather than refused. This renders above everything else in the pane, so
/// a component that cannot be named must not take the whole pane down with it.
let componentTypeName (ct: ComponentType) : string =
    match ct with
    | Input1 _ | Input _ -> "Input"
    | Output _ -> "Output"
    | Viewer _ -> "Viewer"
    | IOLabel -> "Net label"
    | NotConnected -> "Not connected"
    | Constant1 _ | Constant _ -> "Constant"
    | BusSelection _ -> "Bus selection"
    | BusCompare _ | BusCompare1 _ -> "Bus compare"
    | Not -> "Not gate"
    | GateN (gateType, n) ->
        let gate = $"{gateType}"
        $"{n}-input {gate[0..0].ToUpper() + gate[1..]} gate"
    | Decode4 -> "Decoder"
    | Mux2 | Mux4 | Mux8 -> "Multiplexer"
    | Demux2 | Demux4 | Demux8 -> "Demultiplexer"
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ -> "Adder"
    | NbitsXor (_, Some Multiply) -> "Multiplier"
    | NbitsXor _ -> "Xor-N block"
    | NbitsAnd _ -> "And-N block"
    | NbitsOr _ -> "Or-N block"
    | NbitsNot _ -> "Not-N block"
    | NbitSpreader _ -> "Bus spreader"
    | MergeWires | MergeN _ -> "Merge"
    | SplitWire _ | SplitN _ -> "Split"
    | DFF -> "D flip-flop"
    | DFFE -> "D flip-flop with enable"
    | Register _ -> "Register"
    | RegisterE _ -> "Register with enable"
    | Counter _ | CounterNoLoad _ | CounterNoEnable _ | CounterNoEnableLoad _ -> "Counter"
    | AsyncROM1 _ | AsyncROM _ -> "Asynchronous ROM"
    | ROM1 _ | ROM _ -> "Synchronous ROM"
    | RAM1 _ | RAM _ -> "Synchronous RAM"
    | AsyncRAM1 _ -> "Asynchronous-read RAM"
    | Shift (_, _, LSL) -> "Shift left"
    | Shift (_, _, LSR) -> "Shift right"
    | Shift (_, _, ASR) -> "Arithmetic shift right"
    | Custom custom -> custom.Name

/// A sheet or component name written as a heading.
///
/// Issie lowercases a sheet name when the sheet is created, so a custom component is called "alu"
/// and a library component "reg16x8". That is the right name to match on and the wrong one to head
/// a pane with, where it sits between two capitalised headings. Only the first letter is touched:
/// the rest of the name is the author's, and "regFile8" must not become "Regfile8".
let private asHeading (name: string) =
    match name with
    | "" -> ""
    | _ -> string (System.Char.ToUpper name[0]) + name[1..]

/// The identity line at the top of the pane: what this component is, and where it came from.
///
/// Sized between the pane's own heading and the field labels below it, because that is where it
/// belongs in the hierarchy: it names the thing whose fields follow.
///
/// A library component is named by the component and library its author gave it, not by the
/// L<n>_ sheet the project keeps it in. That sheet is not part of the design the user navigates -
/// it can only be reached by asking to view it, read-only - so its name is an implementation
/// detail rather than the name of the thing in front of them.
let private makeComponentHeader (comp: Component) =
    let name, origin =
        match comp.Type with
        | Custom custom ->
            match custom.Form with
            | Some (Library (libName, compName)) -> asHeading compName, Some $"library · {libName}"
            | Some (Verilog vName) -> asHeading vName, Some "Verilog"
            | _ -> asHeading custom.Name, Some "instance of sheet"
        | ct -> componentTypeName ct, None
    div [Style [
            Display DisplayOptions.Flex
            AlignItems AlignItemsOptions.Baseline
            JustifyContent "space-between"
            MarginBottom "12px"]] [
        // between the pane heading (Bulma title is-4, 1.5rem) and a field label (1rem)
        span [Style [FontWeight "bold"; FontSize "1.125rem"]] [str name]
        (match origin with
         | None -> null
         | Some tag ->
            span [Style [FontSize "11px"; Color "grey"; MarginLeft "8px"; WhiteSpace WhiteSpaceOptions.Nowrap]]
                 [str tag])
    ]


let getInitSource (mem: Memory1) (model:Model)=
    let a = mem.AddressWidth
    let path = match model.CurrentProj with | Some p -> p.ProjectPath | None -> ""
    match mem.Init with
    | SignedMultiplier -> 
        Ok $"Dout = (signed) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | UnsignedMultiplier ->
        Ok $"Dout = (unsigned) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | FromData ->
        Ok "Memory Viewer/Editor"
    | FromFile name | ToFile name | ToFileBadName name ->
        if FilesIO.fileExistsWithExtn ".ram" path name then 
            match initialiseMem mem path with
            | Ok _ -> Ok $"From '{name}.ram' file"
            | Error s -> Error $"From '{name}.ram' file. WARNING - this file exists but has a read error: {s}"
        else
            Error $"From '{name}.ram' file in folder '{path}'. WARNING - this file does not exist"
   

let getDialogMemorySetup (mem: Memory1) =
    mem.AddressWidth,mem.WordWidth,mem.Init, match mem.Init with | FromFile n -> Some n | _ -> None

/// What a memory component holds, as facts: shape, and where its initial contents come from.
/// Prose and figures only - everything here belongs behind the About disclosure, and nothing in it
/// does anything.
let private memoryFacts (mem1: Memory1) cType model : ReactElement list =
    let sourceLine =
        let isError, msgEnd =
            match getInitSource mem1 model with Error txt -> true, txt | Ok txt -> false, txt
        let msgStart =
            match cType with
            | RAM1 _ | AsyncRAM1 _ -> "Initial "
            | ROM1 _ | AsyncROM1 _ -> ""
            | _ -> failwithf $"What - wrong component type ({cType}) here"
        let msg = $"{msgStart}data source: {msgEnd}"
        match isError with
        | true -> Fulma.Label.label [Label.Size IsSmall; Label.Modifiers [Modifier.TextColor IsDanger]] [str msg]
        | false -> Fulma.Label.label [] [str msg]
    [
        bSpan $"Address width: {mem1.AddressWidth} bit(s)"
        br []
        bSpan $"Number of elements: {1UL <<< mem1.AddressWidth}"
        br []
        bSpan $"Word width: {mem1.WordWidth} bit(s)"
        br []
        sourceLine
    ]

/// The things a memory component can be asked to DO. These are the reason for selecting a RAM, so
/// they stay in the body of the pane rather than going behind the About disclosure with the prose.
let private memoryActions mem compId cType model dispatch =
    let setup = getDialogMemorySetup mem
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let mem1 = match cType with | Memory mem -> mem | _ -> failwithf "What? memoryActions called with non-memory"
    if model.PopupDialogData.MemorySetup = None then
        dispatch <| SetPopupDialogMemorySetup (Some setup)
    let projectPath = (Option.get model.CurrentProj).ProjectPath
    div [] [
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch <| Msg.ExecFuncInMessage(
                    (fun model _ -> openMemoryEditor mem1 compId model dispatch), dispatch))
        ] [str "View/Edit memory content"]
        (memPropsInfoButton dispatch)
        br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ ->
                FilesIO.openWriteDialogAndWriteMemory mem1 projectPath
                |> (function
                        | None -> ()
                        | Some path ->
                            let note = successPropertiesNotification $"Memory content written to '{path}'"
                            dispatch <| SetPropertiesNotification note))
        ] [str "Export memory initial data to file"]
        br []
        (makePopupButton
            "Memory Initial Data Source"
            (MenuHelpers.makeSourceMenu
                model
                (model.Sheet.UpdateMemory sheetDispatch)
                compId
                dispatch)
            "Change data source"
            dispatch)
    ]


let makeVerilogEditButton model (custom:CustomComponentType) dispatch : ReactElement = 
    
    let openCodeEditor code name = 
        let project' =
            match model.CurrentProj with
            |Some proj -> {proj with WorkingFileName = Some (custom.Name)}
            |None -> failwithf "Can't happen!"
        let model'= {model with CurrentProj = Some project'} 
        createVerilogPopup model true (Some code) (Some name) (UpdateVerilogFile name)
    
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
            div []
                [
                    br []
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> 
                            dispatch (StartUICmd SaveSheet)
                            dispatch (FileCommand (FileSaveOpenFile, dispatch))
                            dispatch <| Sheet(SheetT.DoNothing)
                            openCodeEditor code name dispatch)
                    ] [str "View/Edit Verilog code"]
                    br []
                ]
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
            div []
                [
                    br []
                    Button.button
                        [ 
                        Button.IsOutlined
                        Button.Color IsDanger
                        Button.OnClick(fun _ -> confirmationPopup title buttonText body  buttonAction dispatch) ]
                        [ str "Delete" ]
                    br []
                ]
        | _ -> null

let private changeAdderType model (comp:Component) dispatch = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
        
        let checkedCin,checkedCout =
            match comp.Type with
            |NbitsAdder w ->
                Checked true, Checked true
            |NbitsAdderNoCout w ->
                Checked true, Checked false
            |NbitsAdderNoCin w ->
                Checked false, Checked true 
            |NbitsAdderNoCinCout w ->
                Checked false, Checked false
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
            PropertiesHelp.fieldLabel "Optional Ports"
            
            Table.table [] [
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Cin"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionCin); Value "Cin"; Id "Cin-button"; Name "Cin-button"; checkedCin; Style [Height "15px"; Width "15px"]]]]
                ]
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Cout"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionCout); Value "Cout"; Id "Cout-button"; Name "Cout-button"; checkedCout; Style [Height "15px"; Width "15px"]]]]
                ]
            ]
            br []
            ]


let private changeCounterType model (comp:Component) dispatch = 
    match model.CurrentProj with
    | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
    | Some project ->
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
        let checkedLoad,checkedEnable=
            match comp.Type with
            |Counter _ ->
                Checked true, Checked true 
            |CounterNoEnable _ ->
                Checked true, Checked false
            |CounterNoLoad _ ->
                Checked false, Checked true 
            |CounterNoEnableLoad _ ->
                Checked false, Checked false
            | _ -> failwithf "Cannot change counter type from non-counter component"

        let buttonActionLoad =
            fun _ ->
                match comp.Type with
                |Counter w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoLoad w)
                |CounterNoEnable w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnableLoad w)
                |CounterNoLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (Counter w)
                |CounterNoEnableLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnable w)
                | _ -> failwithf "Cannot change adder type from non-adder component"
        let buttonActionEnable =
            fun _ ->
                match comp.Type with
                |Counter w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnable w)
                |CounterNoLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoEnableLoad w)
                |CounterNoEnable w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (Counter w)
                |CounterNoEnableLoad w ->
                    model.Sheet.ChangeCounterComp sheetDispatch (ComponentId comp.Id) (CounterNoLoad w)
                |_ -> failwithf "Cannot change adder type from non-adder component"

        div [] [
            PropertiesHelp.fieldLabel "Optional Inputs"
            Table.table [] [
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Load"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionLoad); Value "Load"; Id "Load-button"; Name "Load-button"; checkedLoad; Style [Height "15px"; Width "15px"]]]]
                ]
                tr [] [
                    td [Style [BorderStyle "solid"]] [str "Enable"]
                    td [Style [BorderStyle "solid"]] [Checkbox.input [Props [OnChange (buttonActionEnable); Value "Enable"; Id "Enable-button"; Name "Enable-button"; checkedEnable; Style [Height "15px"; Width "15px"]]]]
                ]
            ]
            br []
            ]

let private makeScaleAdjustmentField model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let textw =  
        match comp.SymbolInfo with
        |Some si ->
            match si.HScale with
            |Some no -> no
            |None -> 1.0
        |None -> 1.0
    let texth =  
        match comp.SymbolInfo with
        |Some si ->
            match si.VScale with
            |Some no -> no
            |None -> 1.0
        |None -> 1.0

    // Side by side: two boxes of the same kind, sixty pixels wide, that are nearly always set
    // together - stacked they read as two unrelated fields and cost two lines of a short pane.
    div [Style [Display DisplayOptions.Flex; Flex "0 0 auto"; ColumnGap "12px"]] [
        floatFormField "Width Scale" "60px" textw 0.0 (
            fun (newWidth) ->
                if newWidth < 0.0
                then
                    let props = errorPropsNotification "A scale factor cannot be negative."
                    dispatch <| SetPropertiesNotification props
                else
                    model.Sheet.ChangeScale sheetDispatch (ComponentId comp.Id) newWidth Horizontal
                    dispatch ClosePropertiesNotification
        )
        floatFormField "Height Scale" "60px" texth 0.0 (
            fun (newWidth) ->
                if newWidth < 0.0
                then
                    let props = errorPropsNotification "A scale factor cannot be negative."
                    dispatch <| SetPropertiesNotification props
                else
                    model.Sheet.ChangeScale sheetDispatch (ComponentId comp.Id) newWidth Vertical
                    dispatch ClosePropertiesNotification
        )
    ]


let private makeNumberOfBitsField model (comp: Component) text dispatch =    
    // One phrasing, not seven. These prompts said "Number of bits", "Bus width", "Width of output
    // bus", "Number of bits in the wire", "Number of bits selected: width" and two more, all for
    // the same idea; only the two that qualify WHICH width still differ.
    let title, width, slot =
        match comp.Type with
        | Input1 (w, _) | Output w -> "Width (bits)", w, IO comp.Label
        | NbitsAdder w | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w
        | NbitsXor (w, _) | NbitsAnd w | NbitsOr w |NbitsNot w
        | Register w | RegisterE w
        | Counter w | CounterNoEnable w | CounterNoEnableLoad w |CounterNoLoad w
        | Viewer w -> "Width (bits)", w, Buswidth
        | NbitSpreader w -> "Output width (bits)", w, Buswidth
        | SplitWire w -> "Top (LSB) output width (bits)", w, Buswidth
        | BusSelection (w, _) -> "Width (bits)", w, Buswidth
        | BusCompare (w, _) -> "Width (bits)", w, Buswidth
        | BusCompare1 (w, _, _) -> "Width (bits)", w, Buswidth
        | Constant1 (w, _, _) -> "Width (bits)", w, Buswidth
        // the SHIFT input width follows the bus width (see shifterWidthFor)
        | Shift (w, _, _) -> "Width (bits)", w, Buswidth
        | c -> failwithf $"makeNumberOfBitsField called with invalid component: {c}"

    // The upper bound is the one Issie already enforces on a constant's or a bus comparison's
    // width, in CatalogueView.parseConstant. Widths reached through a parameter expression had no
    // upper bound at all, so a parameter could make a bus of any size at all and the first thing to
    // object was the simulator running out of memory.
    let constraints = [
        MinVal (PInt 1I, $"{title} must be positive")
        MaxVal (PInt (bigint NumberHelpers.Constants.maxIssieBusWidth),
                $"{title} cannot exceed {NumberHelpers.Constants.maxIssieBusWidth}")
    ]

    ParameterView.paramInputField model title 1I (Some (bigint width)) None constraints (Some comp) slot dispatch


let private makeNumberOfInputsField model (comp: Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let prompt, gateType, nInp =
        match comp.Type with
        | GateN (gt, n) -> "Number of inputs", gt, n
        | c -> failwithf $"makeNumberOfInputsField called with invalid component: {c}"

    // Plain numeric input, not a parameter box: the number of inputs sets how many ports the
    // component has, and changing it deletes the wires on any port that goes away. Parameter
    // slots record a value, not a change of topology, so this cannot be parameterised.
    Field.div [] [
        PropertiesHelp.fieldLabel prompt
        Input.number [
            Input.Props [ Style [ Width "60px" ]; Min 2; Max maxGateInputs ]
            Input.DefaultValue (string nInp)
            Input.OnChange (getIntEventValue >> fun newNum ->
                match newNum >= 2 && newNum <= maxGateInputs with
                | true -> model.Sheet.ChangeGate sheetDispatch (ComponentId comp.Id) gateType newNum
                | false -> ())
        ]
    ]


let private changeMergeN model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let errText =
        model.PopupDialogData.Int
        |> Option.map (fun i ->
            if i < 2 || i > Constants.maxSplitMergeBranches then
                sprintf $"Must have between 2 and {Constants.maxSplitMergeBranches} inputs"
            else
                "")
        |> Option.defaultValue ""

    let title, nInp =
        match comp.Type with
        | MergeN n -> "Number of inputs", n
        | c -> failwithf "changeMergeN called with invalid component: %A" c

    div [] [
        span
            [Style [Color Red]]
            [str errText]

        // Plain numeric input, not a parameter box: the number of inputs sets how many ports the
        // component has, and changing it deletes the wires on any port that goes away. Parameter
        // slots record a value, not a change of topology, so this cannot be parameterised.
        Field.div [] [
            PropertiesHelp.fieldLabel title
            Input.number [
                Input.Props [ Style [ Width "60px" ]; Min 2; Max Constants.maxSplitMergeBranches ]
                Input.DefaultValue (string nInp)
                Input.OnChange (getIntEventValue >> fun newNum ->
                    match newNum >= 2 && newNum <= Constants.maxSplitMergeBranches with
                    | true -> model.Sheet.ChangeMergeN sheetDispatch (ComponentId comp.Id) newNum
                    | false -> ())
            ]
        ]
    ]


let private changeSplitN model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let errText =
        model.PopupDialogData.Int
        |> Option.map (fun i ->
            if i < 2 || i > Constants.maxSplitMergeBranches then
                sprintf $"Must have between 2 and {Constants.maxSplitMergeBranches} outputs"
            else
                "")
        |> Option.defaultValue ""

    let title, nInp, widths, lsbs =
        match comp.Type with
        | SplitN (nInputs, widths, lsblist) -> "Number of outputs", nInputs, widths, lsblist
        | c -> failwithf "changeSplitN called with invalid component: %A" c
    
    let changeWidths = fun (widths: int list) (newNumInputs: int ) (defaultVal : int)-> 
        match widths.Length with
        | n when n > newNumInputs -> 
            widths[..(newNumInputs-1)]
        | n when n < newNumInputs ->
            List.append widths (List.init (newNumInputs-n) (fun _ -> defaultVal)) 
        | _ -> widths

    let changeLsbs = fun (lsbs: int list) (widths: int list) (newNumInputs: int ) -> 
        match lsbs.Length with
        | n when n > newNumInputs -> 
            lsbs[..(newNumInputs-1)]
        | n when n < newNumInputs ->
            let msbs = List.map2 (fun lsb width -> lsb + width - 1) lsbs widths
            List.append lsbs (List.init (newNumInputs-n) (fun x -> x + (List.max msbs) + 1)) 
        | _ -> lsbs

    div [] [
        span
            [Style [Color Red]]
            [str errText]

        // Plain numeric input for number of outputs (no parameter box)
        Field.div [] [
            PropertiesHelp.fieldLabel title
            Input.number [
                Input.Props [ Style [ Width "80px" ]; Min 2; Max Constants.maxSplitMergeBranches ]
                Input.DefaultValue (string nInp)
                Input.OnChange (getIntEventValue >> fun newNum ->
                    let newWidths = changeWidths widths newNum 1
                    let newLsbs = changeLsbs lsbs newWidths newNum
                    model.Sheet.ChangeSplitN sheetDispatch (ComponentId comp.Id) newNum newWidths newLsbs)
            ]
        ]

        // Parameter boxes for each output's Width and LSB
        widths
        |> List.mapi (fun index defaultWidth ->
            let defaultLsb = lsbs.[index]
            div [ Style [ MarginLeft "10px"; MarginBottom "10px" ] ] [
                Label.label [] [ str (sprintf "Output Port %d" index) ]
                // Width parameter
                ParameterView.paramInputField
                    model
                    "Width"
                    1I
                    (Some (bigint defaultWidth))
                    None
                    [ MinVal (PInt 1I, "Width must be at least 1")
                      MaxVal (PInt (bigint NumberHelpers.Constants.maxIssieBusWidth),
                              $"Width cannot exceed {NumberHelpers.Constants.maxIssieBusWidth}") ]
                    (Some comp)
                    (SplitNWidth index)
                    dispatch
                // LSB parameter
                ParameterView.paramInputField
                    model
                    "LSB"
                    0I
                    (Some (bigint defaultLsb))
                    None
                    [ MinVal (PInt 0I, "LSB must be non-negative")
                      MaxVal (PInt (bigint NumberHelpers.Constants.maxIssieBusWidth),
                              $"LSB cannot exceed {NumberHelpers.Constants.maxIssieBusWidth}") ]
                    (Some comp)
                    (SplitNLSB index)
                    dispatch
            ]
        )
        |> div [Style [MarginBottom "20px"]]
    ]


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
            | None -> w, 0I
        | _ -> failwithf "Other component types should not call this function."
    
    // The bound holds at every width because parameter values are bigint: it used to be computed
    // as `1 <<< width`, which wraps at 32, so it had to be left off above width 30 and a wide
    // input's default went unchecked. Nothing downstream checked it either - FastRun assigns the
    // default to the step array as it stands.
    let constraints = [
        MinVal (PInt 0I, "Default value must be non-negative")
        MaxVal (PInt ((1I <<< width) - 1I), $"Default value must fit in {width} bits")
    ]

    // DefaultValue, not IO: IO is this input's width, and two fields of one component cannot
    // share a parameter slot
    ParameterView.paramInputField model title 0I (Some defValue) None constraints (Some comp) InputDefault dispatch

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

/// Create react to change constant properties
let makeConstantDialog (model:Model) (comp: Component) (text:string) (dispatch: Msg -> Unit): ReactElement =
        let symbolDispatch msg = dispatch <| msgToS msg
        let wComp, txtComp =
            match comp.Type with | Constant1( w,_,txt) -> w,txt | _ -> failwithf "What? impossible" 
        let w = Option.defaultValue wComp model.PopupDialogData.Int
        let cText = Option.defaultValue txtComp model.PopupDialogData.Text
        let reactMsg, compTOpt = CatalogueView.parseConstant Constants.maxIssieBusWidth w cText
        match compTOpt with
        | None -> ()
        // The dispatch below happens while rendering, whenever the dialog's value differs from the
        // component's. On a sheet held at what it loaded with, the change would be undone, the
        // pane would render again, and it would dispatch again: not a dead control but a loop. So
        // the test is here, not on the input box.
        | Some _ when ModelHelpers.openSheetIsReadOnly model -> ()
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
                        dispatch <| SetPopupDialogText (Some txt))
                
            ]              

/// Create react to change constant properties
let makeBusCompareDialog (model:Model) (comp: Component) (text:string) (dispatch: Msg -> Unit): ReactElement =
        let symbolDispatch msg = dispatch <| msgToS msg
        let wComp, txtComp =
            match comp.Type with | BusCompare1( w,_,txt) -> w,txt | _ -> failwithf "What? impossible1" 
        let w = Option.defaultValue wComp model.PopupDialogData.Int
        let cText = Option.defaultValue txtComp model.PopupDialogData.Text
        let reactMsg, compTOpt = CatalogueView.parseBusCompareValue Constants.maxIssieBusWidth w cText
        match compTOpt with
        | None -> ()
        // as makeConstantDialog: this dispatches while rendering, so on a read-only sheet it would
        // dispatch, be undone, render, and dispatch again
        | Some _ when ModelHelpers.openSheetIsReadOnly model -> ()
        | Some (BusCompare1(w,cVal,cText) as compT) ->
            if compT <> comp.Type then
                model.Sheet.ChangeWidth (Sheet >> dispatch) (ComponentId comp.Id) w
                symbolDispatch <| SymbolT.ChangeBusCompare (ComponentId comp.Id, cVal, cText)
                dispatch (ReloadSelectedComponent w)
                dispatch ClosePropertiesNotification
        | _ -> failwithf "What? impossible"

        div [] [
                makeNumberOfBitsField model comp text dispatch
                br []
                reactMsg
                br []
                textFormFieldSimple 
                    "Enter bus compare value in decimal, hex, or binary:" 
                    cText 
                    (fun txt -> 
                        dispatch <| SetPopupDialogText (Some txt))
                
            ] 

/// The properties field for the SECOND integer of a BusSelection or a BusCompare: where the
/// selection starts, and what the comparison is against. Both go in an IO parameter slot, because
/// Buswidth is already taken by the component's own width.
///
/// Only those two component types have such a field. BusCompare1 does not belong here even though
/// it also has a comparison value: makeBusCompareDialog edits that one. This used to accept it
/// when working out the prompt and then fail on a second match when building the field, so a
/// caller that believed the first match would have crashed the properties pane. One match now, so
/// the accepted types cannot drift apart again.
let private makeLsbBitNumberField model (comp:Component) dispatch =
    let infoText, value, constraints =
        match comp.Type with
        | BusSelection (_, lsb) ->
            "Least Significant Bit number selected: lsb",
            bigint lsb,
            [ MinVal (PInt 0I, "LSB position must be non-negative") ]
        | BusCompare (width, cVal) ->
            "Compare with",
            cVal,
            // as in makeDefaultValueField, the bound holds at every width now that it is computed
            // in bigint rather than wrapping at 32
            [ MinVal (PInt 0I, "Comparison value must be non-negative")
              MaxVal (PInt ((1I <<< width) - 1I), $"Comparison value must fit in {width} bits") ]
        | _ ->
            failwithf $"makeLsbBitNumberField called on {comp.Type}, which has no such field"
    ParameterView.paramInputField model infoText 0I (Some value) None constraints (Some comp) (IO comp.Label) dispatch



/// Prose about a component: what is left of the old Description field once everything the header,
/// the value boxes and the symbol itself already say has been taken out of it.
///
/// Empty for most components. A one-word gloss restating the type name was the pane's most
/// prominent text while saying nothing, and the catalogue already carries a full tooltip for every
/// built-in (CatalogueView.catTip1), so this is a second copy of that text and not the only one.
/// Widths are never quoted here: the value is in the box below and, when it comes from an
/// expression, beside it as well.
let private describeComponent (comp:Component) model : ReactElement list =
    let nGateNote (gateName: string) =
        $"One {gateName} gate per bit of the input and output busses."
    match comp.Type with
    | ROM _ | RAM _ | AsyncROM _ ->
        failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    // the header names these, and there is nothing further worth saying
    | Input1 _ | Output _ | Viewer _ | NotConnected | Constant1 _ | Constant _
    | Not | GateN _ | Decode4
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ -> []
    // No longer says that busses of more than 32 bits are unsupported: they are. The simulator
    // compares wide busses as bigint, and a comparison value is checked against the bus width it
    // is compared with, up to NumberHelpers.Constants.maxIssieBusWidth.
    | BusCompare _ | BusCompare1 _ ->
        [str "The output is one if the bus unsigned binary value is equal to the integer specified. \
              This will display in hex on the design sheet."]
    | BusSelection _ ->
        [ str "The output is the subrange [width+lsb-1..lsb] of the input bits. If width = 1 this \
               selects one bit. Error if the input has less than width + lsb bits."
          br []; br []
          str "Note that the output bit(s) are numbered from 0 even if the input range has LS bit \
               number > 0. The input range selected for output is displayed in brackets on the \
               symbol." ]
    | IOLabel ->
        [ str "Net labels with the same name are one net. Each label has input on left and output \
               on right. No output connection is required from a net. Since a net represents one \
               wire or bus, exactly one input connection is required. Net labels can be used:"
          br []
          str "To name wires and document designs."; br []
          str "To join inputs and outputs without wires, in place of a long connection or one \
               driving many inputs."; br []
          str "To prevent an unused output from giving an error." ]
    | MergeWires ->
        [str "Merge two busses of width n and m into a single bus of width n+m. The bit numbers of \
              the whole and each branch are shown when the component is connected."]
    | MergeN _ ->
        [str "Merge N busses of various widths into a single bus. The bit numbers of the whole and \
              each branch are shown when the component is connected."]
    | SplitWire _ ->
        [str "Split a bus of width n+m exactly into two non-overlapping busses of width n and m. \
              The bit numbers of the whole and each branch are shown when the component is \
              connected."]
    | SplitN _ ->
        [str "Split a bus into N output busses of various widths. The output busses may overlap. \
              The output busses need not include all of the input bits"]
    | NbitsXor (_, Some Multiply) ->
        [str "Returns the low bits of the product - as many as the busses are wide. For those bits, \
              signed and unsigned multiplication are identical."]
    | NbitsXor _ -> [str (nGateNote "Xor")]
    | NbitsAnd _ -> [str (nGateNote "And")]
    | NbitsOr _ -> [str (nGateNote "Or")]
    | NbitsNot _ -> [str (nGateNote "Not")]
    | NbitSpreader _ ->
        [str "Every bit of the output bus is the same as the 1-bit input. Used to implement sign \
              extension and shift operations."]
    | DFF -> [str clockNote]
    | DFFE ->
        [str $"If the enable signal is high the state of the D-flip-flop will be updated at the \
               next clock cycle. {clockNote}"]
    | Register _ -> [str clockNote]
    | RegisterE _ ->
        [str $"If the enable signal is high the state of the Register will be updated at the next \
               clock cycle. {clockNote}"]
    | Counter _ | CounterNoEnable _ | CounterNoEnableLoad _ | CounterNoLoad _ ->
        [str $"If the enable signal is high the state of the counter will be updated at the next \
               clock cycle, taking either the value of input d (when load is enabled) or the value \
               of out+1 (if load is disabled). {clockNote}"]
    | AsyncROM1 mem ->
        str "The output is updated as soon as the address changes." :: br [] :: br []
        :: memoryFacts mem comp.Type model
    | ROM1 mem ->
        str $"The output is updated only after a clock tick. {clockNote}" :: br [] :: br []
        :: memoryFacts mem comp.Type model
    | RAM1 mem ->
        str $"At every clock tick the RAM can read and optionally write the content of the memory \
              location selected by the address. If the write signal is high, the content of the \
              selected location is set to the value of data-in. In cycle 0 data-out is 0, otherwise \
              data-out is the contents of the location addressed in the previous cycle, before any \
              optional write. {clockNote}"
        :: br [] :: br [] :: memoryFacts mem comp.Type model
    | AsyncRAM1 mem ->
        str $"At every clock tick, optionally write the content of the memory location selected by \
              the address. If the write signal is high, the content of the selected location is set \
              to the value of data-in. data-out is the contents of the location addressed by the \
              current cycle address. {clockNote}"
        :: br [] :: br [] :: memoryFacts mem comp.Type model
    | Shift (_, shifterWidth, shiftType) ->
        let kindName, kind =
            match shiftType with
            | LSL -> "LSL (logical shift left)", "bits shifted in are zero."
            | LSR -> "LSR (logical shift right)", "bits shifted in are zero."
            | ASR -> "ASR (arithmetic shift right)", "the sign bit is replicated into the bits shifted in."
        [ b [] [str $"Shift kind: {kindName}."]
          br []; br []
          str $"Shifts the IN bus by the number of positions given on the {shifterWidth} bit SHIFT \
                input: {kind} Shifting by the bus width or more clears the output, or fills it with \
                the sign bit for an arithmetic shift." ]
    | Custom custom ->
        let sheetDescription =
            match custom.Description with
            // The description belongs to the SHEET and was copied onto the instance when it was
            // placed, so say whose it is rather than letting it read as the instance's own.
            | Some description ->
                [ p [Style [Color "grey"; FontSize "11px"]] [str $"Description of sheet {custom.Name}:"]
                  p [] [str description] ]
            | None -> []
        // What decided the port order when the symbol was first drawn used to be explained here.
        // It is not worth a paragraph in a pane this short: it tells the user nothing they can act
        // on, and it stops being true the moment they reorder the ports themselves.
        sheetDescription

/// The ports of a custom component instance, and their widths.
/// Kept in the body of the pane rather than behind the About disclosure: on a parameterised
/// component these widths move whenever a value moves, so they are the first thing to check after
/// an edit.
let private makePortSummary (custom:CustomComponentType) =
    /// A port, written as it would be in Verilog or on a wire label: a single wire is just its
    /// name, and a bus carries the range of bit numbers it actually has. This was "NAME 3", a
    /// number whose meaning had to be known rather than read - and on the common single-wire port
    /// it put a "1" beside every name for no information at all.
    let portText (label: string) (width: int) =
        match width with
        | w when w <= 1 -> label
        | w -> $"{label}({w - 1}:0)"
    let group name (labels: (string * int) list) =
        match labels with
        | [] -> null
        | _ ->
            div [Style [Display DisplayOptions.Flex; MarginBottom "2px"]] [
                span [Style [Width "34px"; Color "grey"; FontSize "11px"; Flex "0 0 auto"]] [str name]
                span [Style [FontSize "12px"]]
                     [str (labels |> List.map (fun (l, w) -> portText l w) |> String.concat "   ")]
            ]
    match custom.InputLabels, custom.OutputLabels with
    | [], [] -> null
    | ins, outs ->
        div [Style [MarginBottom "10px"]] [
            PropertiesHelp.fieldLabel "Ports"
            group "in" ins
            group "out" outs
        ]

/// The editable fields and the actions of one component: everything in the pane that DOES
/// something, as against the prose that explains it.
let private makeExtraInfo model (comp:Component) text dispatch : ReactElement =
    match comp.Type with
    | Input1 _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                makeDefaultValueField model comp dispatch
            ]
    | GateN _ ->
        div []
            [
                makeNumberOfInputsField model comp dispatch
            ]
    | MergeN _ ->
        div []
            [
                changeMergeN model comp dispatch
            ]
    | Output _ |NbitsAnd _ |NbitsOr _ |NbitsNot _ |NbitSpreader _ | NbitsXor _ | Viewer _ | Shift _ ->
        makeNumberOfBitsField model comp text dispatch
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                changeAdderType model comp dispatch
            ]
    | SplitWire _ ->
        makeNumberOfBitsField model comp text dispatch
    | SplitN _ ->
        div []
            [
                changeSplitN model comp dispatch
            ]
    | Register _ | RegisterE _ ->
        makeNumberOfBitsField model comp text dispatch
    |Counter _ |CounterNoEnable _ |CounterNoEnableLoad _ |CounterNoLoad _ ->
        div []
            [
                makeNumberOfBitsField model comp text dispatch
                changeCounterType model comp dispatch
            ]
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
    |BusCompare1 _ ->
        makeBusCompareDialog model comp text dispatch
    | Constant1 _ ->
             makeConstantDialog model comp text dispatch
    // Reversing the inputs is an action, so it belongs here with the other controls rather than
    // buried in the read-only prose that used to carry it.
    | Mux2 ->
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(
                (fun model _ -> model.Sheet.ChangeReversedInputs (Sheet >> dispatch) (ComponentId comp.Id)),dispatch))
            ] [str "Reverse Inputs"]
    // The memory buttons are the reason for selecting a RAM, so they stay in the body of the pane
    // while the shape and data source of the memory go to About with the rest of the prose.
    | AsyncROM1 mem | ROM1 mem | RAM1 mem | AsyncRAM1 mem ->
        memoryActions mem (ComponentId comp.Id) comp.Type model dispatch
    // A custom component had no case here at all: everything it could be asked to do lived inside
    // the read-only description. Its parameter values are now edited exactly as a built-in width is.
    | Custom custom ->
        // Being able to go to the sheet is the plainest way of saying that this component IS an
        // instance of one. Absent for a library component, which is meant to be one thing: its
        // sheet can be looked at, but only from the right-click menu, which keeps that the
        // deliberate and uncommon act it should be.
        let openSheetButton =
            match custom.Form with
            | Some (Library _) -> null
            | _ ->
                Button.button [
                    Button.Color IsInfo
                    Button.IsLight
                    Button.OnClick (fun _ ->
                        dispatch <| ExecFuncInMessage((fun model dispatch ->
                            match model.CurrentProj with
                            | Some proj ->
                                MenuHelpers.openFileInProject custom.Name proj model dispatch
                                dispatch <| UpdateUISheetTrail (fun trail -> proj.OpenFileName :: trail)
                            | None -> ()), dispatch))
                ] [str $"Open sheet {custom.Name}"]
        div [] [
            ParameterView.makeParamBindingEntryBoxes model comp custom dispatch
            makePortSummary custom
            openSheetButton
            makeVerilogEditButton model custom dispatch
            makeVerilogDeleteButton model custom dispatch
        ]
    | _ -> div [] []

/// The collapsed "About this component" disclosure at the foot of the pane: the prose, and the
/// appearance controls - neither of which is needed in order to use the component.
/// Renders nothing at all when there would be nothing in it.
let private makeAboutSection model (comp:Component) dispatch =
    let appearance =
        match comp.Type with
        // A library component's dimensions are settled by whoever authored it, so scaling an
        // instance is not something to offer: the fields are absent rather than disabled.
        | Custom {Form = Some (Library _)} -> []
        | Custom _ -> [makeScaleAdjustmentField model comp dispatch]
        | _ -> []
    match describeComponent comp model @ appearance with
    | [] -> null
    | content ->
        details [Open false; Style [MarginTop "15px"]] [
            summary [DiagramStyle.menuLabelStyle] [str "About this component"]
            div [Style [FontSize "12px"; LineHeight "1.35"]] content
        ]


let viewSelectedComponent (model: ModelType.Model) dispatch =
    let dispatchWithModel (func: Model -> (Msg -> Unit) -> Unit) =
        dispatch <|ExecFuncInMessage(func, dispatch)

    let checkIfLabelIsUnique chars (symbols: SymbolT.Symbol list)  =
        match List.exists (fun (s:SymbolT.Symbol) -> s.Component.Label = chars) symbols with
        |true -> Error Constants.labelUniqueMess
        |false -> Ok chars

    let allowNoLabel =
        let symbols = model.Sheet.Wire.Symbol.Symbols
        match model.Sheet.SelectedComponents with
        | [cid] ->
            match Map.tryFind cid symbols with
            | Some {Component ={Type=MergeWires | SplitWire _ | BusSelection _ | NotConnected}} -> true
            | _ -> false
        | _ -> false

    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    /// return an OK label text, or an error message
    let formatLabelText (txt: string) compId =
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        let allowedDotPos =
            match comp.Type with
            | Custom {Name = name} -> name.Length
            | _ -> -1
        txt.ToUpper()
        |> (fun chars -> 
            let symbols = model.Sheet.Wire.Symbol.Symbols |> Map.toList |> List.filter (fun (i,s) -> i <> compId) |> List.map snd
            let badChars = 
                chars 
                |> Seq.indexed
                |> Seq.filter (fun (i,ch) -> not (Char.IsLetterOrDigitOrUnderscore ch) && (ch <> '.'  || i <> allowedDotPos))
                |> Seq.map snd
                |> Seq.map string |> String.concat ""
            match String.length chars with 
            | 0 when allowNoLabel -> 
                Ok ""
            | 0 -> 
                Error "Empty label is not allowed for this component"
            | _ when not (String.startsWithLetter chars) ->
                Error "Labels must start with a character"
            | _ when badChars.Contains "." && allowedDotPos > 0 ->
                Error $"Custom Component labels can only contain a '.' immediately after the name"
            | _ when badChars.Contains "."  ->
                Error $"Labels of normal components can only contain letters and digits and underscores, not '.'"
            | _ when badChars <> "" ->
                Error $"Labels can only contain letters and digits, not '{badChars}'"
            | _ -> 
                let currSymbol = model.Sheet.Wire.Symbol.Symbols[compId]
                match currSymbol.Component.Type with
                |IOLabel ->
                    let allSymbolsNotWireLabel = symbols |> List.filter(fun s -> s.Component.Type <> IOLabel)
                    checkIfLabelIsUnique chars allSymbolsNotWireLabel
                |_ ->
                    checkIfLabelIsUnique chars symbols           
            )
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.displayedComponent model.Sheet.Wire.Symbol compId
        // let label' = extractLabelBase comp.Label
        // TODO: normalise labels so they only contain allowed chars all uppercase
        let defaultText =
            match model.PopupDialogData.Text with
            | None -> comp.Label
            | Some text -> text
        let label' = formatLabelText defaultText compId // No formatting atm
        let labelText = match label' with Ok s -> s | Error e -> defaultText
        let required =
            match comp.Type with
            | SplitWire _ | MergeWires | BusSelection _ | NotConnected -> false | _ -> true
        let isBad =
            if model.PopupDialogData.BadLabel then
                match label' with
                | Ok _ -> None
                | Error msg -> Some msg
            else    None
        // Identity first, then the things that can be changed, then - closed - the things that
        // only explain. The name used to sit at the bottom, below a wall of prose and every value
        // box, although it is what the user most often came here to change.
        let nameLabel =
            match comp.Type with
            // naming it an instance says, where the user can act on it, that the sheet and this
            // thing on the canvas are different objects. A library component is not presented as
            // an instance of anything, so it keeps the plain word.
            | Custom {Form = Some (Library _)} -> "Name"
            | Custom _ -> "Instance name"
            | _ -> "Name"
        div [Key comp.Id] [
            makeComponentHeader comp
            textFormField
                required
                nameLabel
                defaultText
                isBad
                (fun text -> // onChange
                    match formatLabelText text compId with
                    | Error errorMess ->
                        dispatch <| SetPopupDialogBadLabel (true)
                        dispatch <| SetPopupDialogText (Some text)
                    | Ok label ->
                        MenuHelpers.setComponentLabel model sheetDispatch comp label
                        dispatch <| SetPopupDialogText (Some label)
                        dispatch <| SetPopupDialogBadLabel (false)
                    dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)) // reload the new component
            makeExtraInfo model comp labelText dispatch
            makeAboutSection model comp dispatch
        ]
    // Nothing selected: this is the OPEN SHEET's properties, not a component's. The two used to be
    // written out twice over, differing only by whether a description existed.
    | _ ->
        match model.CurrentProj with
        |Some proj ->
            let sheetName = proj.OpenFileName
            let sheetLdc = proj.LoadedComponents |> List.find (fun ldc -> ldc.Name = sheetName)
            let sheetDescription = sheetLdc.Description
            let descriptionButton =
                match sheetDescription with
                | None ->
                    Button.button
                        [ Button.Color IsSuccess
                          Button.OnClick (fun _ ->
                            dispatchWithModel (fun model _ -> createSheetDescriptionPopup model None sheetName dispatch)) ]
                        [str "Add Description"]
                | Some _ ->
                    Button.button
                        [ Button.Color IsInfo
                          Button.OnClick (fun _ ->
                            dispatchWithModel (fun model _ -> createSheetDescriptionPopup model sheetDescription sheetName dispatch)) ]
                        [str "Edit Description"]
            div [] [
                PropertiesHelp.fieldLabel "Sheet Description"
                (match sheetDescription with
                 | None -> null
                 | Some descr -> p [] [str descr])
                br []
                descriptionButton
                br []
                br []
                ParameterView.viewParameters model dispatch
                ]
        |None -> null
    // Below whichever of the two the pane is showing, because it explains the boxes in both: a
    // component's width and a sheet property's value are typed into the same kind of box. One
    // placement rather than one per box - a grammar repeated beside every field is noise.
    |> (fun react -> div [] [react; ParameterView.expressionSyntaxHelp model])
    // A library component's sheet can be looked at but not changed, and the pane is most of the
    // reason to look: it is where the widths, constants, property values and memory contents
    // are. So the values stay visible and readable, and only the controls go dead.
    //
    // A disabled fieldset does that natively and in one place - it disables every input, select
    // and button below it, whatever renders them, and needs no flag threaded through the thirty
    // or so fields the pane can show. Text stays selectable, which pointer-events would have cost.
    |> (fun react ->
            if ModelHelpers.openSheetIsReadOnly model then
                fieldset
                    [ HTMLAttr.Disabled true
                      Style [Border "none"; Padding "0"; Margin "0"; MinWidth "0"; Opacity "0.85"] ]
                    [react]
            else react)
    |> (fun react -> div [Style [Height "calc(100vh - 150px)"; OverflowY OverflowOptions.Auto]] [react])
