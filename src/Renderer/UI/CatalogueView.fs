(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fulma.Extensions.Wikiki

open Fulma
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

open Helpers
open DiagramStyle
open ModelType
open CommonTypes
open PopupView
open Extractor
open System

let private menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]

let private createComponent compType label model dispatch =
    Sheet (Sheet.InitialiseCreateComponent (compType, label)) |> dispatch

//Anything requiring a standard label should be checked and updated with the correct number suffix in Symbol/Sheet, so give the label ""
let createCompStdLabel comp model dispatch =
    createComponent comp "" model dispatch

let private makeCustom model dispatch (loadedComponent: LoadedComponent)  =
    menuItem [] loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = loadedComponent.InputLabels
            OutputLabels = loadedComponent.OutputLabels
        }
        
        Sheet (Sheet.InitialiseCreateComponent (custom, "")) |> dispatch
    )

let private makeCustomList model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.map (makeCustom model dispatch)

let private createIOPopup hasInt typeStr compType (model:Model) dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = model.LastUsedDialogWidth
    let body = 
        match hasInt with
        | true -> dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
        | false -> dialogPopupBodyOnlyText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            createComponent (compType inputInt) (formatLabelFromType (compType inputInt) inputText) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createNbitsAdderPopup (model:Model) dispatch =
    let title = sprintf "Add N bits adder"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            //printfn "creating adder %d" inputInt
            createCompStdLabel (NbitsAdder inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch


let private createNbitsXorPopup (model:Model) dispatch =
    let title = sprintf "Add N bits XOR gates"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            //printfn "creating XOR %d" inputInt
            createCompStdLabel (NbitsXor inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch


let private createSplitWirePopup model dispatch =
    let title = sprintf "Add SplitWire node" 
    let beforeInt =
        fun _ -> str "How many bits should go to the top (LSB) wire? The remaining bits will go to the bottom (MSB) wire."
    let intDefault = 1
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            createCompStdLabel (SplitWire inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createConstantPopup model dispatch =
    let title = sprintf "Add Constant" 
    let beforeInt2 =
        fun _ -> str "What is the decimal value of the constant?"
    let beforeInt =
        fun _ -> str "How many bits has wire carrying the constant?"
    let intDefault = 1
    let intDefault2 = 0
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, intDefault2) "120px" dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let width = getInt dialogData
            let constant = getInt2 dialogData
            createCompStdLabel (Constant(width,constant)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1 || getInt dialogData > 32
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createBusSelectPopup model dispatch =
    let title = sprintf "Add Bus Selection node" 
    let beforeInt2 =
        fun _ -> str "Which input bit is the least significant output bit?"
    let beforeInt =
        fun _ -> str "How many bits width is the output bus?"
    let intDefault = 1
    let intDefault2 = 0
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, intDefault2) "60px" dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let width = getInt dialogData
            let lsb = getInt2 dialogData
            createCompStdLabel (BusSelection(width,lsb)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1 || getInt2 dialogData < 0
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createBusComparePopup (model:Model) dispatch =
    let title = sprintf "Add Bus Compare node" 
    let beforeInt2 =
        fun _ -> str "What is the decimal value to compare the input with?"
    let beforeInt =
        fun _ -> str "How many bits width is the input bus?"
    let intDefault = model.LastUsedDialogWidth
    let intDefault2 = 0
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, intDefault2) "120px" dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let width = getInt dialogData
            let cVal = getInt2 dialogData
            createCompStdLabel (BusCompare(width, uint32 cVal)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> 
            let w = getInt dialogData
            let cVal = getInt2 dialogData |> uint32
            w > 32 || w < 1 || cVal > (1u <<< w) - 1u
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createRegisterPopup regType (model:Model) dispatch =
    let title = sprintf "Add Register" 
    let beforeInt =
        fun _ -> str "How wide should the register be (in bits)?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            createCompStdLabel (regType inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch


 
    

let private createMemoryPopup memType model (dispatch: Msg -> Unit) =
    let title = "Create memory"
    let intDefault = model.LastUsedDialogWidth
    let addError errorOpt (memSetup:(int*int*InitMemData*string option) option) : (int*int*InitMemData*string option) option =
        match memSetup with
        | Some (n1,n2,mem, _) ->
            Some (n1,n2,mem,errorOpt)
        | _ -> None
    let body = dialogPopupBodyMemorySetup intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let addressWidth, wordWidth, source, msgOpt = getMemorySetup dialogData intDefault
            let initMem = {
                AddressWidth = addressWidth
                WordWidth = wordWidth
                Init = source
                Data = Map.empty
                }

            let memory = FilesIO.initialiseMem  initMem dialogData.ProjectPath
            match memory with
            | Ok mem ->
                createCompStdLabel (memType mem)  model dispatch
                dispatch ClosePopup
            | Error mess ->
                dispatch <| SetPopupDialogMemorySetup (addError (Some mess) dialogData.MemorySetup)
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            let addressWidth, wordWidth, source,_ = getMemorySetup dialogData 1
            let error = 
                match dialogData.MemorySetup with 
                | Some(_,_,ToFileBadName _,_) ->
                    Some "File name must be alphanumeric without prefix"
                | None -> 
                    Some ""
                | Some (_,_,SignedMultiplier,_) 
                | Some(_,_,UnsignedMultiplier,_) ->
                    if addressWidth % 2 <> 0 then
                        Some "The address width must be even for a multiplier"
                    elif addressWidth > 16 then
                        Some "The maximum multiplier size is 8X8 - 16 address bits"
                    else None
                | _ -> 
                    None
            match error with
            | Some msg when msg <> "" ->
                match dialogData.MemorySetup with
                | Some (_,_,_,e) when e = Some msg -> ()
                | _ -> dispatch <| SetPopupDialogMemorySetup (addError (Some msg) dialogData.MemorySetup)
            | _ -> ()
            addressWidth < 1 || wordWidth < 1 || error <> None
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private makeMenuGroup title menuList =
    details [Open false] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]


let mutable firstTip = true

let mutable tippyNodes: Browser.Types.Element list = []

let private makeMenuGroupWithTip  title tip menuList =
    let addTip (el: Browser.Types.Element) =
        if not (isNull el) && firstTip then 
            el.setAttribute("data-tippy-content",tip)
            tippyNodes <- el :: tippyNodes
    details [Open false; Ref addTip] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let compareModelsApprox (m1:Model) (m2:Model) =
    let initActivity = {
        AutoSave = Inactive
        LastSavedCanvasState = Map.empty
        LastAutoSaveCheck = System.DateTime.MinValue
        LastAutoSave = Map.empty
        RunningSimulation = false
        }
    let m1r = reduceApprox m1
    let m2r = reduceApprox m2
    let b = m1r = m2r
    //printfn "Model equality:%A" b
    //if b = false then printfn "\n\n%A\n\n%A\n\n" m1r m2r
    b

let viewCatalogue model dispatch =
        let viewCatOfModel = fun model ->                 
            let styles = 
                match model.Sheet.Action with
                | Sheet.InitialisedCreateComponent _ -> [Cursor "grabbing"]
                | _ -> []

            let catTip1 name func (tip:string) = 
                let react = menuItem styles name func
                div [ HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                      Tooltip.dataTooltip tip
                      Style styles
                    ]
                    [ react ]
            Menu.menu [Props [Class "py-1"; Style styles]]  [
                // TODO
                    makeMenuGroup
                        "Input / Output"
                        [ catTip1 "Input"  (fun _ -> createIOPopup true "input" Input model dispatch) "Input connection to current sheet: one or more bits"
                          catTip1 "Output" (fun _ -> createIOPopup true "output" Output model dispatch) "Output connection from current sheet: one or more bits"
                          catTip1 "Viewer" (fun _ -> createIOPopup true "viewer" Viewer model dispatch) "Viewer of current sheet to expose value in simulation"
                          catTip1 "Constant" (fun _ -> createConstantPopup model dispatch) "Define a one or more bit constant value, \
                                                                                            e.g. 0 or 1 to drive an unused input"
                          catTip1 "Wire Label" (fun _ -> createIOPopup false "label" (fun _ -> IOLabel) model dispatch) "Labels with the same name connect \
                                                                                                                         together wires or busses"]
                    makeMenuGroup
                        "Buses"
                        [ catTip1 "MergeWires"  (fun _ -> createComponent MergeWires "" model dispatch) "Use Mergewire when you want to \
                                                                                       join the bits of a two busses to make a wider bus"
                          catTip1 "SplitWire" (fun _ -> createSplitWirePopup model dispatch) "Use Splitwire when you want to split the \
                                                                                             bits of a bus into two sets"
                          catTip1 "Bus Select" (fun _ -> createBusSelectPopup model dispatch) "Bus Select output connects to one or \
                                                                                                more selected bits of its input"
                          catTip1 "Bus Compare" (fun _ -> createBusComparePopup model dispatch) "Bus compare outputs 1 if the input bus \
                                                                                                 matches a constant value" ]
                    makeMenuGroup
                        "Gates"
                        [ catTip1 "Not"  (fun _ -> createCompStdLabel Not model dispatch) "Invertor: output is negation of input"
                          catTip1 "And"  (fun _ -> createCompStdLabel And model dispatch) "Output is 1 if both the two inputs are 1"
                          catTip1 "Or"   (fun _ -> createCompStdLabel Or model dispatch) "Output is 1 if either of the two inputs are 1"
                          catTip1 "Xor"  (fun _ -> createCompStdLabel Xor model dispatch) "Output is 1 if the two inputs have different values"
                          catTip1 "Nand" (fun _ -> createCompStdLabel Nand model dispatch) "Output is 0 if both the two inputs are 1"
                          catTip1 "Nor"  (fun _ -> createCompStdLabel Nor model dispatch) "Output is 0 if either of the two inputs are 1"
                          catTip1 "Xnor" (fun _ -> createCompStdLabel Xnor model dispatch) "Output is 1 if the two inputs have the same values"]
                    makeMenuGroup
                        "Mux / Demux"
                        [ catTip1 "Mux2" (fun _ -> createCompStdLabel Mux2 model dispatch) "Selects the one of its two input busses numbered by the value of the select input
                                                                                to be the output. Adjusts bus width to match."
                          catTip1 "Demux2" (fun _ -> createCompStdLabel Demux2 model dispatch)  "The output is equal to the input, the other is 0"
                          catTip1 "Decode4" (fun _ -> createCompStdLabel Decode4 model dispatch) "The output numbered by the binary value 
                                                                                                of the 2 bit sel input is equal to Data, the others are 0"]
                    makeMenuGroup
                        "Arithmetic"
                        [ catTip1 "N bits adder" (fun _ -> createNbitsAdderPopup model dispatch) "N bit Binary adder with carry in to bit 0 and carry out from bit N-1"
                          catTip1 "N bits XOR" (fun _ -> createNbitsXorPopup model dispatch) "N bit XOR gates - use to make subtractor or comparator"]

                    makeMenuGroup
                        "Flip Flops and Registers"
                        [ catTip1 "D-flip-flop" (fun _ -> createCompStdLabel DFF model dispatch) "D flip-flop - note that clock is assumed always connected to a global clock, \
                                                                                                   so ripple counters cannot be implemented in Issie"
                          catTip1 "D-flip-flop with enable" (fun _ -> createCompStdLabel DFFE model dispatch) "D flip-flop: output will remain unchanged when En is 0"
                          catTip1 "Register" (fun _ -> createRegisterPopup Register model dispatch) "N D flip-flops with inputs and outputs combined into single N bit busses"
                          catTip1 "Register with enable" (fun _ -> createRegisterPopup RegisterE model dispatch) "As register but outputs stay the same if En is 0"]
                    makeMenuGroup
                        "Memories"
                        [ catTip1 "ROM (asynchronous)" (fun _ -> createMemoryPopup AsyncROM1 model dispatch) "This is combinational: \
                                                    the output is available in the same clock cycle that the address is presented"
                          catTip1 "ROM (synchronous)" (fun _ -> createMemoryPopup ROM1 model dispatch) "A ROM whose output contains \
                                                    the addressed data in the clock cycle after the address is presented"
                          catTip1 "RAM" (fun _ -> createMemoryPopup RAM1 model dispatch)  "A RAM whose output contains the addressed \
                                                   data in the clock cycle after the address is presented" ]
                    makeMenuGroupWithTip 
                        "This project"
                        "Every design sheet is available for use in other sheets as a custom component: \
                        it can be added any number of times, each instance replicating the sheet logic"
                        (makeCustomList model dispatch)
                ]

        (viewCatOfModel) model // This prevented the model from automatically updating which lead to label generation not being 
                                                            // updated unless the catalogue viewer was reloaded.
                                                            // viewCatOfModel model
