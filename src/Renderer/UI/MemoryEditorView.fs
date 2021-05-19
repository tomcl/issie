//(*
//    MemoryEditorView.fs
//
//    A simple Popup editor to view and change the content of a memory.
//*)
//
module MemoryEditorView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open NumberHelpers
open JSHelpers
open CommonTypes
open ModelType
open PopupView

let private popupExtraStyle = [ Width "65%"; Height "80%" ]
let private headerHeight = 60;
let private headerStyle = Style [
    Position PositionOptions.Fixed
    MarginTop (string (-headerHeight-20) + "px")
    PaddingTop "20px"
    PaddingBottom "60px"
    BackgroundColor "white"
    Width "61%"
    Height headerHeight
    CSSProp.ZIndex 32
]
let private bodyStyle = Style [
    MarginTop (string headerHeight + "px")
]

let private showError msg dispatch : unit =
    errorNotification msg CloseMemoryEditorNotification
    |> SetMemoryEditorNotification |> dispatch

let private closeError dispatch : unit =
    CloseMemoryEditorNotification |> dispatch

let private showRowWithAdrr memoryEditorData addr =
    match memoryEditorData.Address with
    | None -> true
    | Some a when a = addr -> true
    | _ -> false

let viewNum numBase =
    match numBase with | Hex -> hex64 | Dec -> dec64 | Bin -> bin64 | SDec -> sDec64

let viewFilledNum width numBase =
    match numBase with | Hex -> fillHex64 width | Dec -> dec64 | Bin -> fillBin64 width | SDec -> sDec64

// let private baseToStr b = match b with | Hex -> "hex" | Dec -> "dec" | Bin -> "bin"


let mutable dynamicMem: Memory1 = { 
    Init = FromData; 
    WordWidth = 0; 
    AddressWidth = 0; 
    Data = Map.empty 
    } // Need to use a mutable dynamic memory and update it locally so that the shown values are correct since the model is not immediately updated

let baseSelector numBase changeBase =
    Level.item [ Level.Item.HasTextCentered ] [
        Field.div [ Field.HasAddonsCentered ] [
            Control.div [] [ Button.button [
                Button.Color (if numBase = Hex then IsPrimary else NoColor)
                Button.OnClick (fun _ -> changeBase Hex)
            ] [ str "hex" ] ]
            Control.div [] [ Button.button [
                Button.Color (if numBase = Dec then IsPrimary else NoColor)
                Button.OnClick (fun _ -> changeBase Dec)
            ] [ str "dec" ] ]
            Control.div [] [ Button.button [
                Button.Color (if numBase = Bin then IsPrimary else NoColor)
                Button.OnClick (fun _ -> changeBase Bin)
            ] [ str "bin" ] ]
        ]
    ]

let changeBase memoryEditorData dispatch numBase =
    { memoryEditorData with NumberBase = numBase }
    |> Some |> SetPopupMemoryEditorData |> dispatch

//========//
// Editor //
//========//
/// Function creating react input box for memory address for use in WaveSim code (could also be used here).
/// setMemoryAddress dispatches a message to set the memory address to that typed in the input box.
/// numberBase, addressWidth configure the view.
/// TODO: make box width variable according to memory width?
let reactMemoryAddressInputBox numberBase addressWidth setMemoryAddress dispatch =
    Input.text [
        Input.Props [ Style [ MarginLeft "10px"; Width "80px" ] ]
        Input.DefaultValue ""
        Input.Placeholder <| viewNum numberBase (int64 0)
        Input.OnChange (getTextEventValue >> fun text ->
            match text with
            | "" -> closeError dispatch
                    dispatch <| setMemoryAddress None
            | t ->
                match strToInt t with
                | Error err -> showError err dispatch
                | Ok addr ->
                    let addr = uint64 addr
                    let w = addressWidth
                    if w < 64 && addr >= (1UL <<< w)
                    then showError "Address out of bounds." dispatch
                    else closeError dispatch
                         dispatch <| setMemoryAddress (Some (int64 addr))
                         
        )
    ]

let private makeEditorHeader memory isDiff memoryEditorData dispatch =
    div [headerStyle; SpellCheck false] [
        Level.level [] ([
            Level.item [ Level.Item.HasTextCentered ] [
                str <| sprintf "Number of elements: %d" (pow2int64 memory.AddressWidth)
                br []
                str <| sprintf "Word width: %d bit(s)" memory.WordWidth
            ]
            Level.item [ Level.Item.HasTextCentered ] [
                str <| "First Location Displayed"
                Input.text [
                    Input.Props [ Style [ MarginLeft "10px"; Width "80px" ] ]
                    Input.DefaultValue ""
                    Input.Placeholder <| viewNum memoryEditorData.NumberBase (int64 0)
                    Input.OnChange (getTextEventValue >> fun text ->
                        match text with
                        | "" -> closeError dispatch
                                { memoryEditorData with Address = None }
                                |> Some |> SetPopupMemoryEditorData |> dispatch
                        | t ->
                            match strToInt t with
                            | Error err -> showError err dispatch
                            | Ok addr ->
                                let addr = uint64 addr
                                let w = memory.AddressWidth
                                if w < 64 && addr >= (1UL <<< w)
                                then showError "Address out of bounds." dispatch
                                else closeError dispatch
                                     { memoryEditorData with Address = Some (int64 addr) }
                                     |> Some |> SetPopupMemoryEditorData |> dispatch
                    )
                ]
            ]
            baseSelector memoryEditorData.NumberBase (changeBase memoryEditorData dispatch)
        ] @ (if isDiff // Add extra filter.
            then [
                Level.item [ Level.Item.HasTextCentered ] [
                    Checkbox.checkbox [] [
                        Checkbox.input [ Props [
                            Style [ MarginRight "5px" ]
                            Checked memoryEditorData.OnlyDiff
                            OnChange (fun _ ->
                                { memoryEditorData with OnlyDiff = not memoryEditorData.OnlyDiff }
                                |> Some |> SetPopupMemoryEditorData |> dispatch
                            )
                        ] ]
                        str "Show only if changed"
                    ]
                ]
            ]
            else []
        ))
    ]

let private makeEditorBody memory compId memoryEditorData model (dispatch: Msg -> unit) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let source = memory.Init
    let isReadOnly =
        match source with
        | SignedMultiplier
        | UnsignedMultiplier ->
            $"Fixed multiplier blocks cannot have initial value edited"
        | FromFile fName -> 
            $"This memory takes initial values from {fName}.ram, edit the file to change them"
        | ToFile fName->
            $"This memory is linked to File {fName}.ram, changes made here will be save dto that file"
        | _ -> ""
    let showRow = showRowWithAdrr memoryEditorData
    let viewNumD = viewFilledNum memory.WordWidth memoryEditorData.NumberBase
    let viewNumA = viewFilledNum memory.AddressWidth memoryEditorData.NumberBase
    let numLocsToDisplay = 16UL
    let maxLocAddr = ((1UL <<< memory.AddressWidth) - 1UL)
    let startLoc, endLoc =
        match memoryEditorData.Address with
        | None -> 0UL, min maxLocAddr numLocsToDisplay
        | Some a -> 
            let a = uint64 a
            let maxDispLocWrapped = a + numLocsToDisplay - 1UL
            let maxDispLoc = if maxDispLocWrapped > a then maxDispLocWrapped else uint64 (-1L)
            a, min  maxDispLoc maxLocAddr
    //printfn "makeEditorBody called"

    //printfn "Making body with data=%A, dynamic %A" memory.Data dynamicMem
    // let memory = dynamicMem
    let makeRow isReadOnly (memData: Map<int64,int64>) (addr: uint64) =
        let addr = int64 addr
        let content =  // Need to keep the changes locally as well, since the model does not immediately get updated
            Map.tryFind (int64 addr) memData
            |> Option.defaultValue 0L
        //printfn "load"
        tr [ SpellCheck false; Style [  Display (if true then DisplayOptions.TableRow else DisplayOptions.None)] ] [
            td [] [ str <| viewNumA (int64 addr) ]
            td [] [
                let handleInput  (ev: Browser.Types.FocusEvent) =
                    let text = getTextEventValue ev
                    //printfn "change"
                    match strToIntCheckWidth memory.WordWidth text with
                    | Ok value ->
                        // Close error notification.
                        closeError dispatch
                        // Write new value.
                        let oldData =
                            let comp = model.Sheet.GetComponentById compId
                            comp.Type |> (function | (RAM1 d) | (ROM1 d) | (AsyncROM1 d) -> d
                                                    | _ ->
                                                       printfn "Should not be here"
                                                       memory)

                        dynamicMem <- { dynamicMem with Data = Map.add addr value dynamicMem.Data }
                        model.Sheet.WriteMemoryLine sheetDispatch compId addr value // Only update one row
                        
                        dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)
                        //printfn "setting value=%d, addr=%d" value addr                       
                    | Error err -> 
                        showError err dispatch
                
                Input.text [
                    Input.Props [ 
                        OnBlur handleInput ; 
                        Key <| ( memoryEditorData.NumberBase.ToString() + (addr,content).ToString())
                        ] 
                    Input.Disabled isReadOnly
                    Input.DefaultValue <| viewNumD content
                    Input.Option.OnChange <| (getTextEventValue >> (fun text ->
                        match strToIntCheckWidth memory.WordWidth text with
                        | Error err -> showError err dispatch
                        | Ok _ -> closeError dispatch))                    
                ]
            ]
        ]
        
    div [bodyStyle] [
        str isReadOnly
        br []
        Table.table [ Table.IsFullWidth ] [
            thead [] [ tr [] [
                th [] [str "Address"]
                th [] [str "Content"]
            ] ]
            tbody [] ( [startLoc..endLoc] |> List.map (makeRow (isReadOnly <> "") memory.Data))
        ]
    ]

let private makeFoot editMode dispatch (model: Model)=
    let action =
        fun _ -> dispatch CloseMemoryEditorNotification
                 dispatch ClosePopup
                 // Diff mode is triggered by a simulationView, not by a
                 // selected component.
                 match editMode with
                 | Some (ToFile fName) ->
                    match model.CurrentProj, model.SelectedComponent with
                    | Some p, Some comp ->
                        let mem =
                            match comp.Type with
                            | RAM1 mem | ROM1 mem | AsyncROM1 mem -> mem
                            | _ -> failwithf $"Unexpected non-memory component {comp.Type}"
                        match FilesIO.initialiseMem mem p.ProjectPath with
                        | Error msg ->
                            let note = 
                                errorNotification 
                                    "Error writing chnaged memory contents to {fName}.ram" 
                                    CloseMemoryEditorNotification
                            dispatch <| SetMemoryEditorNotification note
                        | _ -> ()
                        dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)
                    | p,comp -> failwithf "What? expecting component {comp} and project {p}"
                 | Some FromData ->
                    dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)
                 | _ -> ()
    Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
        Level.left [] []
        Level.right [] [ Level.item [] [ Button.button [
            Button.Color IsPrimary
            Button.OnClick action
        ] [ str "Done" ] ] ]
    ]

let private makeEditor memory compId model dispatch =
    // printfn "makeEditor called"
    dynamicMem <- // Need to use a mutable dynamic memory and update it locally so that the shown values are correct since the model is not immediately updated
        match model.SelectedComponent with
        | Some {Type=RAM1 mem} | Some {Type=ROM1 mem} |Some {Type = AsyncROM1 mem} -> mem
        | _ -> memory
    fun memoryEditorData ->
        div [] [
            makeEditorHeader dynamicMem false memoryEditorData dispatch
            makeEditorBody dynamicMem compId memoryEditorData model dispatch
        ]

/// Open a popup to view and edit the content of a memory.
let openMemoryEditor memory compId model dispatch : unit =
    // Build editor.
    let title = "Memory editor"
    let body = makeEditor memory compId model dispatch
    let foot = makeFoot (Some memory.Init) dispatch model
    showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch

//=============//
// Diff viewer //
//=============//

let private makeDiffViewerBody memory1 memory2 memoryEditorData =
    let getData addr memData =
        Map.tryFind addr memData
        |> Option.defaultValue 0L
    let viewNum = viewNum memoryEditorData.NumberBase
    let makeRow content1 content2 addr =
        let hasChanged = content1 <> content2
        let showRow addr =
            showRowWithAdrr memoryEditorData addr && (
                not memoryEditorData.OnlyDiff ||
                memoryEditorData.OnlyDiff && hasChanged)
        tr [ Style [ Display (if showRow addr then DisplayOptions.TableRow else DisplayOptions.None)] ] [
            td [] [ str <| viewNum (int64 addr) ]
            td [
                Style [BackgroundColor (if hasChanged then "#ffc6d3" else "auto") ]
            ] [ str <| viewNum content1 ]
            td [
                Style [BackgroundColor (if hasChanged then "#baffd3" else "auto") ]
            ] [ str <| viewNum content2 ]
        ]
    let addr = Option.defaultValue 0L memoryEditorData.Address
    let addr2 = addr + 15L

    div [bodyStyle] [
        Table.table [ Table.IsFullWidth ] [
            thead [] [ tr [] [
                th [] [str "Address"]
                th [] [str "Initial content"]
                th [] [str "Current content"]
            ] ]
            tbody [] (
                [addr..addr2] |> List.map (fun a -> (makeRow (getData a memory1.Data) (getData a memory2.Data) a))
            )
        ]
    ]

let private makeDiffViewer memory1 memory2 dispatch =
    fun memoryEditorData ->
        div [] [
            makeEditorHeader memory1 true memoryEditorData dispatch
            makeDiffViewerBody memory1 memory2 memoryEditorData
        ]

let openMemoryDiffViewer memory1 memory2 model dispatch : unit =
#if DEBUG
    assertThat (memory1.AddressWidth = memory2.AddressWidth &&
                memory1.WordWidth = memory2.WordWidth)
    <| sprintf "Memories in diffViewer do not match: %A\n%A" memory1 memory2
#endif
    // Build editor.
    let title = "Memory diff viewer"
    let body = makeDiffViewer memory1 memory2 dispatch
    let foot = makeFoot None dispatch model
    showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch
