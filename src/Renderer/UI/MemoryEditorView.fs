(*
    MemoryEditorView.fs

    A simple Popup editor to view and change the content of a memory.
*)

module MemoryEditorView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open NumberHelpers
open JSHelpers
open CommonTypes
open MessageType
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

let private makeEditorBody memory compId memoryEditorData model dispatch =
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
    let dynamicMem =
        match model.Diagram.GetComponentById compId |> Result.map Extractor.extractComponent with
        | Ok {Type=RAM mem} | Ok {Type=ROM mem} | Ok {Type = AsyncROM mem} -> mem
        | _ -> memory

    printfn "Making body with data=%A, dynamic %A" memory.Data dynamicMem
    let memory = dynamicMem
    let makeRow (memData: Map<int64,int64>) (addr: uint64) =
        let addr = int64 addr
        let content = 
            Map.tryFind (int64 addr) memData
            |> Option.defaultValue 0L
        printfn "load"
        tr [ SpellCheck false; Style [  Display (if true then DisplayOptions.TableRow else DisplayOptions.None)] ] [
            td [] [ str <| viewNumA (int64 addr) ]
            td [] [
                let handleInput  (ev: Browser.Types.FocusEvent) =
                    let text = getTextEventValue ev
                    printfn "change"
                    match strToIntCheckWidth text memory.WordWidth with
                    | Ok value ->
                        // Close error notification.
                        closeError dispatch
                        // Write new value.
                        let oldData = 
                            model.Diagram.GetComponentById compId
                            |> Result.map Extractor.extractComponentType
                            |> (function | Ok (RAM d) | Ok (ROM d) | Ok (AsyncROM d) -> d | _ -> memory)
                        oldData.Data
                        |> Map.add addr value
                        |> Map.filter (fun k v -> v <> 0L)
                        |> Map.toList
                        |> model.Diagram.WriteMemoryLine compId
                        dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)
                        printfn "setting value=%d, addr=%d" value addr                       
                    | Error err -> 
                        showError err dispatch
                
                Input.text [
                    Input.Props [ 
                        OnBlur handleInput ; 
                        Key <| ( memoryEditorData.NumberBase.ToString() + (addr,content).ToString())
                        ] 
                    Input.DefaultValue <| viewNumD content
                    Input.Option.OnChange <| (getTextEventValue >> (fun text ->
                        match strToIntCheckWidth text memory.WordWidth with
                        | Error err -> showError err dispatch
                        | Ok _ -> closeError dispatch))                    
                ]
            ]
        ]
        
    div [bodyStyle] [
        Table.table [ Table.IsFullWidth ] [
            thead [] [ tr [] [
                th [] [str "Address"]
                th [] [str "Content"]
            ] ]
            tbody [] ( [startLoc..endLoc] |> List.map (makeRow memory.Data))
        ]
    ]

let private makeFoot isDiffMode dispatch (model: Model)=
    let action =
        fun _ -> dispatch CloseMemoryEditorNotification
                 dispatch ClosePopup
                 // Diff mode is triggered by a simulationView, not by a
                 // selected component.
                 if not isDiffMode then dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)
    Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
        Level.left [] []
        Level.right [] [ Level.item [] [ Button.button [
            Button.Color IsPrimary
            Button.OnClick action
        ] [ str "Done" ] ] ]
    ]

let private makeEditor memory compId model dispatch =
    let dynamicMem =
        match model.SelectedComponent with
        | Some {Type=RAM mem} | Some {Type=ROM mem} |Some {Type = AsyncROM mem} -> mem
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
    let foot = makeFoot false dispatch model
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
                [addr..addr2] |> List.map (makeRow (getData addr memory1.Data) (getData addr memory2.Data))
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
    assertThat (memory1.AddressWidth = memory2.AddressWidth &&
                memory1.WordWidth = memory2.WordWidth)
    <| sprintf "Memories in diffViewer do not match: %A\n%A" memory1 memory2
    // Build editor.
    let title = "Memory diff viewer"
    let body = makeDiffViewer memory1 memory2 dispatch
    let foot = makeFoot true dispatch model
    showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch
