(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView
open EEExtensions
open VerilogTypes
open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open DiagramStyle
open ModelType
open ModelHelpers
open CommonTypes
open PopupHelpers
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open NearleyBindings
open ErrorCheck
open CodeEditorHelpers
open Fable.SimpleJson
open Fable.Core.JsInterop
open System
open TopMenuView
open MenuHelpers
open SheetCreator

NearleyBindings.importGrammar
NearleyBindings.importFix
NearleyBindings.importParser

module Constants =
    let maxGateInputs = 4

let menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]

let private createComponent compType label model dispatch =
    Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, compType, label)) |> dispatch

// Anything requiring a standard label should be checked and updated with the correct number suffix in Symbol/Sheet, 
// so give the label ""
let createCompStdLabel comp model dispatch =
    createComponent comp "" model dispatch



let private makeCustom styles model dispatch (loadedComponent: LoadedComponent)  =
    let canvas = loadedComponent.CanvasState
    menuItem styles loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
            OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
            Form = loadedComponent.Form
            Description = loadedComponent.Description
        }
        
        Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, custom, "")) |> dispatch
    )

let private makeCustomList styles model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.filter (fun comp -> 
            match JSHelpers.debugLevel <> 0 with
            |true -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel || comp.Form = Some ProtectedSubSheet)
            |false -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel)
        )
        |> List.map (makeCustom styles model dispatch)

let private makeVerilog styles model dispatch (loadedComponent: LoadedComponent)  =
    let canvas = loadedComponent.CanvasState
    menuItem styles loadedComponent.Name (fun _ ->
        let verilog = Custom {
            Name = loadedComponent.Name
            InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
            OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
            Form = loadedComponent.Form
            Description = loadedComponent.Description
        }
        
        Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, verilog, "")) |> dispatch
    )

let private makeVerilogList styles model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.filter (fun comp -> match comp.Form with Some (Verilog _)-> true |_ -> false)
        |> List.map (makeVerilog styles model dispatch)




let private createInputPopup typeStr (compType: int * int option -> ComponentType) (model:Model) (dispatch: Msg -> unit) =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let beforeDefaultValue = fun _ -> str <| sprintf "If the input is undriven, what should the default value be?"
    let intDefault = model.LastUsedDialogWidth
    let body =
        dialogPopupBodyTextAndTwoInts 1 (beforeText, placeholder) (beforeInt, beforeDefaultValue) (intDefault, 0) dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let widthInt = getInt dialogData
            let defaultValueInt = int (getInt2 dialogData)
            createComponent (compType (widthInt, Some defaultValueInt)) (MenuHelpers.formatLabelFromType (compType (widthInt, Some defaultValueInt)) inputText) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let notGoodLabel =
                getText dialogData
                |> (fun s -> s = "" || not (String.startsWithLetter s))
            (getInt dialogData < 1) || notGoodLabel
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

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
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            createComponent (compType inputInt) (MenuHelpers.formatLabelFromType (compType inputInt) inputText) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let notGoodLabel =
                getText dialogData
                |> (fun s -> s = "" || not (String.startsWithLetter s))
            (getInt dialogData < 1) || notGoodLabel
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let createSheetDescriptionPopup (model:Model) previousDescr sheetName dispatch =
    let title = sprintf "Sheet Description"
    let beforeText =
        fun _ -> str <| sprintf "Add description for sheet '%s'" sheetName
    let body =  dialogPopupBodyOnlyTextWithDefaultValue beforeText "Description" previousDescr dispatch
    let buttonText = "Save"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let descr = getText dialogData
            let descrToSave =
                match descr with
                |"" -> None
                |_ -> Some descr
            
            match model.CurrentProj with
            |None -> failwithf "Can't happen"
            |Some p ->
                let target_ldc = p.LoadedComponents |> List.find (fun x -> x.Name = sheetName)
                let other_ldc = p.LoadedComponents |> List.filter (fun x -> x.Name <> sheetName)
                let target_ldc' = {target_ldc with Description=descrToSave}  //add description to ldc
                
                let other_ldc' =  //find all custom comps originating from that sheet and update their description
                    other_ldc 
                    |> List.map (fun ldc -> 
                        let newComps = 
                            ldc.CanvasState
                            |> fst
                            |> List.map (fun comp ->
                                match comp.Type with
                                |Custom x when x.Name = sheetName -> 
                                    let newCompType = Custom {x with Description = descrToSave} 
                                    {comp with Type = newCompType}
                                |_ -> comp
                        )
                        let newCS = newComps,(ldc.CanvasState |> snd)
                        {ldc with CanvasState = newCS}
                    )

                let fixed_ldcs = other_ldc'@[target_ldc'] 
                let p' = {p with LoadedComponents=fixed_ldcs}
                let model' = {model with CurrentProj = Some p'}
                dispatch <| SetProject p'
                saveOpenFileActionWithModelUpdate model' dispatch |> ignore
            dispatch ClosePopup
    let isDisabled =
        fun (model: Model) ->
            false  //allow all
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createGateNPopup (gateType: GateComponentType) (model: Model) dispatch =
    let title = $"Add N input {gateType} gate"
    let beforeInt =
        fun _ -> str "How many inputs should the gate have?"
    let intDefault = 2
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (GateN (gateType, inputInt)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let intIn = getInt model'.PopupDialogData
            intIn < 2 || intIn > Constants.maxGateInputs
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createMergeNPopup (model: Model) dispatch =
    let title = $"Add N input merge"
    let beforeInt =
        fun _ -> str "How many inputs should the merge component have?"
    let intDefault = 2
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (MergeN inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let intIn = getInt model'.PopupDialogData
            intIn < 2
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createSplitNPopup (model: Model) dispatch =
    let title = $"Add N output split"
    let beforeInt =
        fun _ -> str "How many outputs should the split component have?"
    let numInputsDefault = 2
    let body = dialogPopupBodyNInts beforeInt numInputsDefault 1 dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let outputInt = getInt dialogData 
            let outputWidthList = getIntList dialogData numInputsDefault 1
            let outputLSBList = getIntList2 dialogData numInputsDefault 0
            createCompStdLabel (SplitN (outputInt, outputWidthList, outputLSBList)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) ->
            let intIn = getInt model'.PopupDialogData
            intIn < 2
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createNbitsAdderPopup (model:Model) dispatch =
    let title = sprintf "Add N bits adder"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            //printfn "creating adder %d" inputInt
            createCompStdLabel (NbitsAdder inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createNbitsXorPopup (model:Model) dispatch =
    let title = sprintf "Add N bits XOR gates"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            //printfn "creating XOR %d" inputInt
            createCompStdLabel (NbitsXor(inputInt,None)) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model: Model) -> getInt model.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createNbitsAndPopup (model:Model) dispatch =
    let title = sprintf "Add N bits AND gates"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            //printfn "creating XOR %d" inputInt
            createCompStdLabel (NbitsAnd inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createNbitsOrPopup (model:Model) dispatch =
    let title = sprintf "Add N bits OR gates"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            //printfn "creating XOR %d" inputInt
            createCompStdLabel (NbitsOr inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createNbitsNotPopup (model:Model) dispatch =
    let title = sprintf "Add N bits NOT gates"
    let beforeInt =
        fun _ -> str "How many bits should the input/output have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            //printfn "creating XOR %d" inputInt
            createCompStdLabel (NbitsNot inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createNbitSpreaderPopup (model:Model) dispatch =
    let title = sprintf "Add 1-to-N bit spreader"
    let beforeInt =
        fun _ -> str "How many bits should the output have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            //printfn "creating XOR %d" inputInt
            createCompStdLabel (NbitSpreader inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createSplitWirePopup model dispatch =
    let title = sprintf "Add SplitWire node" 
    let beforeInt =
        fun _ -> str "How many bits should go to the top (LSB) wire? The remaining bits will go to the bottom (MSB) wire. \
                      Use Edit -> Flip Vertically after placing component to swap top and bottom"
    let intDefault = 1
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (SplitWire inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// two react text lines in red
let private twoErrorLines errMsg1 errMsg2 =
    span [Style [Color Red]] [str errMsg1; br []; str errMsg2; br [] ]

/// two line message giving constant value
let private constantValueMessage w (cVal:int64) =
    let mask = 
        if w = 64 then 
            0xffffffffffffffffUL 
        else 
            (1UL <<< w) - 1UL
    let uVal = (uint64 cVal) &&& mask
    let sVal = ((int64 uVal) <<< 64 - w) >>> 64 - w
    let hVal = NumberHelpers.fillHex64 w (int64 uVal)
    let line1 = $"Decimal value: %d{uVal} (%d{sVal} signed)"
    let line2 = $"Hex value: %s{hVal}"
    span [] [str line1; br [] ; str line2; br [] ]

/// two line message giving constant value
let private busCompareValueMessage w (cVal:uint32) =
    let mask = 
        if w = 32 then 
            0xffffffffu
        else 
            (1u <<< w) - 1u
    let uVal = cVal &&& mask
    let sVal = ((int32 uVal) <<< 32 - w) >>> 32 - w
    let hVal = NumberHelpers.hex(int uVal)
    let line1 = $"Decimal value: %d{uVal} (%d{sVal} signed)"
    let line2 = $"Hex value: %s{hVal}"
    span [] [str line1; br [] ; str line2; br [] ]

/// check constant parameters and return two react lines with
/// error message or value details
let parseConstant wMax w cText =
    if w < 1 || w > wMax then
            twoErrorLines $"Constant width must be in the range 1..{wMax}" "", None
    else
        match NumberHelpers.strToIntCheckWidth w cText with
        | Ok n ->
            constantValueMessage w n, Some (Constant1 (w,n,cText))
        | Error msg ->
            twoErrorLines msg "", None

let parseBusCompareValue wMax w cText =
    if w < 1 || w > wMax then
            twoErrorLines $"Bus Compare width must be in the range 1..{wMax}" "", None
    else
        match NumberHelpers.strToIntCheckWidth w cText with
        | Ok n ->
            let n' =
                if n >= 0 then n |> uint32
                else
                    let mask = 
                        if w = 32 then 
                            0xffffffffu
                        else 
                            (1u <<< w) - 1u
                    let uVal = (uint32 n) &&& mask
                    uVal
            busCompareValueMessage w (uint32 n), Some (BusCompare1 (w,(n'),cText))
        | Error msg ->
            twoErrorLines msg "", None

/// create react popup to set a constant
let private createConstantPopup model dispatch =
    let title = sprintf "Add Constant" 
    let beforeInt =
        fun _ -> str "How many bits has the wire carrying the constant?"
    let intDefault = 1
    let parseConstantDialog model' =
        let dialog = model'.PopupDialogData
        parseConstant 64
            (Option.defaultValue intDefault dialog.Int)
            (Option.defaultValue "" dialog.Text)
    let beforeText = (fun d -> div [] [d |> parseConstantDialog |> fst; br [] ])
    let placeholder = "Value: decimal, 0x... hex, 0b... binary"   
    let body = dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model: Model) ->
            let dialogData = model.PopupDialogData
            let width = getInt dialogData
            let text = Option.defaultValue "" dialogData.Text
            let constant = 
                match NumberHelpers.strToIntCheckWidth width text with
                | Ok n -> n
                | Error _ -> 0L // should never happen?
            let text' = if text = "" then "0" else text
            createCompStdLabel (Constant1(width,constant,text')) model dispatch
            dispatch ClosePopup
    let isDisabled = parseConstantDialog >> snd >> Option.isNone
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createBusSelectPopup model dispatch =
    let title = sprintf "Add Bus Selection node" 
    let beforeInt2 =
        fun _ -> str "Which input bit is the least significant output bit?"
    let beforeInt =
        fun _ -> str "How many bits width is the output bus?"
    let intDefault = 1
    let intDefault2 = 0L
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, int64 intDefault2) "60px" dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let width = getInt dialogData
            let lsb = int32 (getInt2 dialogData)
            createCompStdLabel (BusSelection(width,lsb)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1 || getInt2 model'.PopupDialogData < 0L
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createBusComparePopup (model:Model) dispatch =
    let title = sprintf "Add Bus Compare node" 
    let beforeInt =
        fun _ -> str "How many bits width is the input bus?"
    let intDefault = 1
    let parseBusCompDialog model' =
        let dialog = model'.PopupDialogData
        parseBusCompareValue 32 (Option.defaultValue intDefault dialog.Int) (Option.defaultValue "" dialog.Text)
    let beforeText = (fun d -> div [] [d |> parseBusCompDialog |> fst; br [] ])
    let placeholder = "Value: decimal, 0x... hex, 0b... binary"   
    let body = dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let width = getInt dialogData
            let text = Option.defaultValue "" dialogData.Text
            let constant = 
                match NumberHelpers.strToIntCheckWidth width text with
                | Ok n -> 
                    if n >= 0 then n |> uint32
                    else
                        let mask = 
                            if width = 32 then 
                                0xffffffffu
                            else 
                                (1u <<< width) - 1u
                        let uVal = (uint32 n) &&& mask
                        uVal
                | Error _ -> 0u // should never happen?
            let text' = if text = "" then "0" else text
            createCompStdLabel (BusCompare1(width,constant,text')) model dispatch
            dispatch ClosePopup
    let isDisabled = parseBusCompDialog >> snd >> Option.isNone
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch

let private createRegisterPopup regType (model:Model) dispatch =
    let title = sprintf "Add Register" 
    let beforeInt =
        fun _ -> str "How wide should the register be (in bits)?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
            let inputInt = getInt dialogData
            createCompStdLabel (regType inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (model': Model) -> getInt model'.PopupDialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


 
    

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
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
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
        fun (model': Model) ->
            let dialogData = model'.PopupDialogData
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
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch



let rec createVerilogPopup model showExtraErrors correctedCode moduleName (origin:CodeEditorOpen) dispatch =
    let title = sprintf "Create Combinational Logic Components using Verilog" 
    let beforeText =
        fun _ -> str <| sprintf "ISSIE Component Name"
    let noErrors = List.isEmpty model.PopupDialogData.VerilogErrors
    let errorDiv = if noErrors then null else getErrorDiv model.PopupDialogData.VerilogErrors
    let errorList = if showExtraErrors then model.PopupDialogData.VerilogErrors else [] 

    let saveButtonAction =
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
            | Some project ->
                let name = (Option.get moduleName)
                let folderPath = project.ProjectPath
                let path = pathJoin [| folderPath; name + ".v" |]
                let path2 = pathJoin [| folderPath; name + ".dgm" |]
                let code = getCode dialogData
                
                match writeFile path code with
                | Ok _ -> ()
                | Error _ -> failwithf "Writing verilog file FAILED"

                let parsedCodeNearley = parseFromFile(code)
                printfn $"{parsedCodeNearley}"
                let output = Json.parseAs<ParserOutput> parsedCodeNearley
                let result = Option.get output.Result
                let fixedAST = fix result
                printfn "fixed %A" fixedAST
                let parsedAST = fixedAST |> Json.parseAs<VerilogInput>

                let cs = SheetCreator.createSheet parsedAST project
                let toSaveCanvasState = Helpers.JsonHelpers.stateToJsonString (cs, None, Some {Form = Some (Verilog name);Description=None})

                match writeFile path2 toSaveCanvasState with
                | Ok _ -> 
                    let newComponent = 
                        match tryLoadComponentFromPath path2 with
                        |Ok comp -> comp
                        |Error _ -> failwithf "failed to load the created Verilog file"
                    let updatedProject =
                        {project with LoadedComponents = newComponent :: project.LoadedComponents}
                    openFileInProject project.OpenFileName updatedProject model dispatch
                | Error _ -> failwithf "Writing .dgm file FAILED"
            dispatch ClosePopup

    let updateButton = 
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in writing Verilog Component"
            | Some project ->
                let name = (Option.get moduleName)
                let folderPath = project.ProjectPath
                let path = pathJoin [| folderPath; name + ".v" |]
                let code = getCode dialogData
                match writeFile path code with
                | Ok _ -> ()
                | Error _ -> failwithf "Writing verilog file FAILED"
                
                let parsedCodeNearley = parseFromFile(code)
                let output = Json.parseAs<ParserOutput> parsedCodeNearley
                let result = Option.get output.Result
                let fixedAST = fix result
                let parsedAST = fixedAST |> Json.parseAs<VerilogInput>
                let newCS = SheetCreator.createSheet parsedAST project

                dispatch (StartUICmd SaveSheet)               
                updateVerilogFileActionWithModelUpdate newCS name model dispatch |> ignore
                dispatch <| Sheet(SheetT.DoNothing)


    let compile =
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in compiling Verilog Component"
            | Some project ->
                let code = getCode dialogData
                let parsedCodeNearley = parseFromFile(code)
                let output = Json.parseAs<ParserOutput> parsedCodeNearley
                if isNullOrUndefined output.Error then
                    match Option.isNone output.Result with 
                    |true-> () //do nothing if parser failed for some reason
                    |false ->
                        let result = Option.get output.Result
                        let fixedAST = fix result
                        let linesIndex = Option.get output.NewLinesIndex |> Array.toList
                        let parsedAST = fixedAST |> Json.parseAs<VerilogInput>                        
                        let moduleName = parsedAST.Module.ModuleName.Name
                        let errorList = ErrorCheck.getSemanticErrors parsedAST linesIndex origin project
                        let dataUpdated = {dialogData with VerilogErrors = errorList; VerilogCode=Some code}
                        let showErrors' = 
                            match List.isEmpty errorList with
                            | true -> showExtraErrors
                            | false -> showExtraErrors 
                        createVerilogPopup {model with PopupDialogData = dataUpdated } showErrors' None (Some moduleName) origin dispatch
                else
                    let error = Option.get output.Error
                    let error'= CodeEditorHelpers.getSyntaxErrorInfo error
                    let dataUpdated = {dialogData with VerilogErrors = [error'] }
                    createVerilogPopup {model with PopupDialogData = dataUpdated } showExtraErrors None moduleName origin dispatch
 

    let addButton =
        fun (dialogData : PopupDialogData) ->
            fun (suggestion,replaceType,line, col) -> 
                
                let findLastIOAndAssignment (oldCode:string) =
                    let isSmallerThan x y = y <= x

                    let parsedCodeNearley = parseFromFile(oldCode)
                    let output = Json.parseAs<ParserOutput> parsedCodeNearley
                    let result = Option.get output.Result
                    let fixedAST = fix result
                    let linesIndex = Option.get output.NewLinesIndex |> Array.toList
                    let parsedAST = fixedAST |> Json.parseAs<VerilogInput>      
                    let ioDecls = parsedAST.Module.ModuleItems.ItemList |> Array.filter (fun item -> Option.isSome item.IODecl)
                    let lastIODecl = Array.tryLast ioDecls
                    let lastIOLocation =
                        match lastIODecl with
                        |Some d -> d.Location
                        |None -> 1
                    let prevIndexIO = List.findIndexBack (fun x -> isSmallerThan lastIOLocation x) linesIndex
                    
                    let assignments = parsedAST.Module.ModuleItems.ItemList |> Array.filter (fun item -> Option.isSome item.Statement)
                    let lastAssignment = Array.tryLast assignments
                    let lastAssignmentLocation =
                        match lastAssignment with
                        |Some d -> d.Location
                        |None -> lastIOLocation
                    let prevIndexA = List.findIndexBack (fun x -> isSmallerThan lastAssignmentLocation x) linesIndex                    
                    
                    (prevIndexIO,prevIndexA)
                let replaceSubstringAtLocation (originalString: string) (replacement: string) (startIndex: int) (length: int) =
                    let prefix = originalString.[..(startIndex-1)]
                    let suffix = originalString.[(startIndex+length)..]
                    prefix + replacement + suffix

                // need to update the variable at the specific location
                let putToCorrectPlace (oldCode:string) suggestion replaceType line =
                    let sepCode = oldCode.Split([|"\n"|],StringSplitOptions.RemoveEmptyEntries)
                    let linesList = Seq.toList sepCode
                    match replaceType with
                    |IODeclaration |Assignment ->
                        let untilError = (linesList[0],[2..line+1])||> List.fold (fun s v -> s+"\n"+linesList[v-1])
                        let fixedError = untilError+"\n  "+ suggestion
                        (fixedError,[line+1..(List.length linesList)-1])||> List.fold (fun s v -> s+"\n"+linesList[v])
                    |Variable error ->
                        let untilError = ("",[1..line-1])||> List.fold (fun s v -> 
                            match v with
                            |1 -> linesList[v-1]
                            |_ -> s+"\n"+linesList[v-1]
                        )
                        let fixedLine = replaceSubstringAtLocation linesList[line-1] suggestion (col-1) error.Length
                        printfn $"fixed: {fixedLine}"
                        let fixedError = 
                            match line with
                            |1 -> fixedLine
                            |_ -> untilError+"\n"+ fixedLine
                        (fixedError,[line..(List.length linesList)-1])||> List.fold (fun s v -> s+"\n"+linesList[v])
                    |NoReplace ->
                        oldCode

                let lineToPut =
                    match replaceType with
                    |IODeclaration -> fst <| findLastIOAndAssignment (Option.defaultValue "" dialogData.VerilogCode)
                    |Assignment -> snd <| findLastIOAndAssignment (Option.defaultValue "" dialogData.VerilogCode)
                    |_ -> line
                let replacedCode = putToCorrectPlace (Option.defaultValue "" dialogData.VerilogCode) suggestion replaceType lineToPut
                createVerilogPopup model showExtraErrors (Some replacedCode) moduleName origin dispatch
    
    let moreInfoButton = 
        fun (dialogData : PopupDialogData) ->
            match model.CurrentProj with
            | None -> failwithf "What? current project cannot be None at this point in compiling Verilog Component"
            | Some project ->
                let errors = dialogData.VerilogErrors
                createVerilogPopup model (not showExtraErrors) None moduleName origin dispatch
    
    let body= dialogVerilogCompBody beforeText moduleName errorDiv errorList showExtraErrors correctedCode compile addButton dispatch

    let isDisabled =
        fun (dialogData : PopupDialogData) ->   //OverflowX OverflowOptions.Hidden;OverflowY OverflowOptions.Hidden
            not noErrors
    
    let extra = if showExtraErrors then [Width "80%";Height "75%";OverflowX OverflowOptions.Hidden;Position PositionOptions.Fixed;] else [Width "50%";Height "75%";OverflowX OverflowOptions.Hidden;Position PositionOptions.Fixed;]
    let saveUpdateText = match origin with |NewVerilogFile -> "Save" |UpdateVerilogFile _ -> "Update"
    let saveUpdateButton = match origin with |NewVerilogFile -> saveButtonAction |UpdateVerilogFile _ -> updateButton
    dialogVerilogPopup title body saveUpdateText noErrors showExtraErrors saveUpdateButton moreInfoButton isDisabled extra dispatch


let private makeMenuGroup title menuList =
    details [Open false] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]


let mutable firstTip = true

let mutable tippyNodes: Browser.Types.Element list = []

let private makeMenuGroupWithTip styles  title tip menuList =
    details [
        Open false;
        HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
        Tooltip.dataTooltip tip
        Style styles
    ] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let compareModelsApprox (m1:Model) (m2:Model) =

    let m1r = reduceApprox m1
    let m2r = reduceApprox m2
    let b = m1r = m2r
    //printfn "Model equality:%A" b
    //if b = false then printfn "\n\n%A\n\n%A\n\n" m1r m2r
    b




let viewCatalogue model dispatch =

        let muxTipMessage (numBusses:string) = $"Selects the one of its {numBusses} input busses numbered by the value of the select input 
                                to be the output. Adjusts bus width to match"

        let deMuxTipMessage (numBits:string) = $"The output numbered by the binary value 
        of the {numBits} sel inputs is equal to Data, the others are 0"

        let viewCatOfModel = fun model ->                 
            let styles = 
                match model.Sheet.Action with
                | SheetT.InitialisedCreateComponent _ -> [Cursor "grabbing"]
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
                        [ catTip1 "Input"  (fun _ -> createInputPopup "input" Input1 model dispatch) "Input connection to current sheet: one or more bits"
                          catTip1 "Output" (fun _ -> createIOPopup true "output" Output model dispatch) "Output connection from current sheet: one or more bits"
                          catTip1 "Viewer" (fun _ -> createIOPopup true "viewer" Viewer model dispatch) "Viewer to expose value in step simulation: works in subsheets. \
                                                                                                         Can also be used to terminate an unused output."
                          catTip1 "Constant" (fun _ -> createConstantPopup model dispatch) "Define a one or more bit constant value of specified width, \
                                                                                            e.g. 0 or 1, to drive an input. Values can be written \
                                                                                            in hex, decimal, or binary."
                          catTip1 "Wire Label" (fun _ -> createIOPopup false "label" (fun _ -> IOLabel) model dispatch) "Labels with the same name connect \
                                                                                                                         together wires within a sheet. Each set of labels \
                                                                                                                         muts have exactly one driving input. \
                                                                                                                         A label can also be used to terminate an unused output"
                          catTip1 "Not Connected" (fun _ -> createComponent (NotConnected) "" model dispatch) "Not connected component to terminate unused output."]                          
                    makeMenuGroup
                        "Buses"
                        [ 
                        catTip1 "MergeWires"  (fun _ -> createComponent MergeWires "" model dispatch) "Use Mergewires when you want to \
                                                                                       join the bits of a two busses to make a wider bus. \
                                                                                       Default has LS bits connected to top arm. Use Edit -> Flip Vertically \
                                                                                       after placing component to change this."
                            
                        catTip1 "MergeN"  (fun _ -> createMergeNPopup model dispatch) "Use Mergewires when you want to \
                                                                                       join the bits of N busses to make a wider bus."
                        catTip1 "SplitWire" (fun _ -> createSplitWirePopup model dispatch) "Use Splitwire when you want to split the \
                                                                                             bits of a bus into two sets. \
                                                                                             Default has LS bits connected to top arm. Use Edit -> Flip Vertically \
                                                                                             after placing component to change this."
                        catTip1 "SplitN" (fun _ -> createSplitNPopup model dispatch) "Use Splitwire when you want to split the \
                                                                                             bits of a bus into multiple sets."                                                                          
                        catTip1 "Bus Select" (fun _ -> createBusSelectPopup model dispatch) "Bus Select output connects to one or \
                                                                                                more selected bits of its input"
                        catTip1 "Bus Compare" (fun _ -> createBusComparePopup model dispatch) "Bus compare outputs 1 if the input bus \
                                                                                                 matches a constant value as written in decimal, hex, or binary." 
                        catTip1 "N bits spreader" (fun _ -> createNbitSpreaderPopup model dispatch) "1-to-N bits spreader"]
                    makeMenuGroup
                        "Gates"
                        [ catTip1 "Not"  (fun _ -> createCompStdLabel Not model dispatch) "Invertor: output is negation of input"
                          catTip1 "And"  (fun _ -> createCompStdLabel (GateN (And, 2)) model dispatch) "Output is 1 if both the two inputs are 1"
                          catTip1 "Or"   (fun _ -> createCompStdLabel (GateN (Or, 2)) model dispatch) "Output is 1 if either of the two inputs are 1"
                          catTip1 "Xor"  (fun _ -> createCompStdLabel (GateN (Xor, 2)) model dispatch) "Output is 1 if the two inputs have different values"
                          catTip1 "Nand" (fun _ -> createCompStdLabel (GateN (Nand, 2)) model dispatch) "Output is 0 if both the two inputs are 1"
                          catTip1 "Nor"  (fun _ -> createCompStdLabel (GateN (Nor, 2)) model dispatch) "Output is 0 if either of the two inputs are 1"
                          catTip1 "Xnor" (fun _ -> createCompStdLabel (GateN (Xnor, 2)) model dispatch) "Output is 1 if the two inputs have the same values"]
                    makeMenuGroup
                        "Mux / Demux"
                        [ catTip1 "2-Mux" (fun _ -> createCompStdLabel Mux2 model dispatch) <| muxTipMessage "two"
                          catTip1 "4-Mux" (fun _ -> createCompStdLabel Mux4 model dispatch) <| muxTipMessage "four"
                          catTip1 "8-Mux" (fun _ -> createCompStdLabel Mux8 model dispatch) <| muxTipMessage "eight"                                                             
                          catTip1 "2-Demux" (fun _ -> createCompStdLabel Demux2 model dispatch)  <| deMuxTipMessage "two"  
                          catTip1 "4-Demux" (fun _ -> createCompStdLabel Demux4 model dispatch)  <| deMuxTipMessage "four"
                          catTip1 "8-Demux" (fun _ -> createCompStdLabel Demux8 model dispatch)  <| deMuxTipMessage "eight" ]
                    makeMenuGroup
                        "Arithmetic"
                        [ catTip1 "N bits adder" (fun _ -> createNbitsAdderPopup model dispatch) "N bit Binary adder with carry in to bit 0 and carry out from bit N-1"
                          catTip1 "N bits XOR" (fun _ -> createNbitsXorPopup model dispatch) "N bit XOR gates - use to make subtractor or comparator"
                          catTip1 "N bits AND" (fun _ -> createNbitsAndPopup model dispatch) "N bit AND gates"
                          catTip1 "N bits OR" (fun _ -> createNbitsOrPopup model dispatch) "N bit OR gates"
                          catTip1 "N bits NOT" (fun _ -> createNbitsNotPopup model dispatch) "N bit NOT gates"]

                    makeMenuGroup
                        "Flip Flops and Registers"
                        [ catTip1 "D-flip-flop" (fun _ -> createCompStdLabel DFF model dispatch) "D flip-flop - note that clock is assumed always connected to a global clock, \
                                                                                                   so ripple counters cannot be implemented in Issie"
                          catTip1 "D-flip-flop with enable" (fun _ -> createCompStdLabel DFFE model dispatch) "D flip-flop: output will remain unchanged when En is 0"
                          catTip1 "Register" (fun _ -> createRegisterPopup Register model dispatch) "N D flip-flops with inputs and outputs combined into single N bit busses"
                          catTip1 "Register with enable" (fun _ -> createRegisterPopup RegisterE model dispatch) "As register but outputs stay the same if En is 0"
                          catTip1 "Counter" (fun _ -> createRegisterPopup Counter model dispatch) "N-bits counter with customisable enable and load inputs"]
                    makeMenuGroup
                        "Memories"
                        [ catTip1 "ROM (asynchronous)" (fun _ -> createMemoryPopup AsyncROM1 model dispatch) "This is combinational: \
                                                    the output is available in the same clock cycle that the address is presented"
                          catTip1 "ROM (synchronous)" (fun _ -> createMemoryPopup ROM1 model dispatch) "A ROM whose output contains \
                                                    the addressed data in the clock cycle after the address is presented"
                          catTip1 "RAM (synchronous)" (fun _ -> createMemoryPopup RAM1 model dispatch)  "A RAM whose output contains the addressed \
                                                   data in the clock cycle after the address is presented" 
                          catTip1 "RAM (async read)" (fun _ -> createMemoryPopup AsyncRAM1 model dispatch)  "A RAM whose output contains the addressed \
                                                   data in the same clock cycle as address is presented" ]

                    makeMenuGroupWithTip styles
                        "This project"
                        "Every design sheet is available for use in other sheets as a custom component: \
                        it can be added any number of times, each instance replicating the sheet logic"
                        (makeCustomList styles model dispatch)

                    makeMenuGroupWithTip 
                        styles
                        "Verilog"
                        "Write combinational logic in Verilog and use it as a Custom Component. 
                         To edit/delete a verilog component add it in a sheet and click on 'properties'"
                        (List.append 
                            [menuItem styles "New Verilog Component" (fun _ -> createVerilogPopup model true None None NewVerilogFile dispatch) ]
                            (makeVerilogList styles model dispatch))
                          
                ]
        (viewCatOfModel) model 
