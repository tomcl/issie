(*
    MemoryEditorView.fs

    A simple Popup editor to view and change the content of a memory.
*)

module MemoryEditorView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Helpers
open NumberHelpers
open JSHelpers
open DiagramTypes
open DiagramMessageType
open DiagramModelType
open PopupView

let private popupExtraStyle = [ Width "65%"; Height "80%" ]
let private headerHeight = 60;
let private headerStyle = Style [
    Position "fixed"
    MarginTop (string (-headerHeight-20) + "px")
    PaddingTop "20px"
    PaddingBottom "60px"
    BackgroundColor "white"
    Width "61%"
    Height headerHeight
    ZIndex 10
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

//========//
// Editor //
//========//

let private makeEditorHeader memory isDiff memoryEditorData dispatch =
    div [headerStyle] [
        Level.level [] ([
            Level.item [ Level.Item.HasTextCentered ] [
                str <| sprintf "Number of elements: %d" (pow2int64 memory.AddressWidth)
                br []
                str <| sprintf "Word width: %d bit(s)" memory.WordWidth
            ]
            Level.item [ Level.Item.HasTextCentered ] [
                str <| "Find address"
                Input.text [
                    Input.Props [ Style [ MarginLeft "10px"; Width "80px" ] ]
                    Input.DefaultValue ""
                    Input.Placeholder "0x0"
                    Input.OnChange (getTextEventValue >> fun text ->
                        match text with
                        | "" -> closeError dispatch
                                { memoryEditorData with Address = None }
                                |> Some |> SetPopupMemoryEditorData |> dispatch
                        | t ->
                            match strToInt t with
                            | Error err -> showError err dispatch
                            | Ok addr ->
                                let addr = int addr
                                if addr < 0 || addr >= pow2(memory.AddressWidth)
                                then showError "Address out of bounds." dispatch
                                else closeError dispatch
                                     { memoryEditorData with Address = Some addr }
                                     |> Some |> SetPopupMemoryEditorData |> dispatch
                    )
                ]
            ]
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
    let makeRow addr content =
        tr [ Style [ Display (if showRow addr then "table-row" else "none")] ] [
            td [] [ str <| hex addr ]
            td [] [
                Input.text [
                    Input.DefaultValue <| hex64 content
                    Input.OnChange (getTextEventValue >> fun text ->
                        match strToIntCheckWidth text memory.WordWidth with
                        | Ok value ->
                            // Close error notification.
                            closeError dispatch
                            // Write new value.
                            model.Diagram.WriteMemoryLine compId addr value
                        | Error err -> showError err dispatch
                    )
                ]
            ]
        ]
    div [bodyStyle] [
        Table.table [ Table.IsFullWidth ] [
            thead [] [ tr [] [
                th [] [str "Address"]
                th [] [str "Content"]
            ] ]
            tbody [] ( memory.Data |> List.mapi makeRow )
        ]
    ]

let private makeFoot isDiffMode dispatch =
    let action =
        fun _ -> dispatch CloseMemoryEditorNotification
                 dispatch ClosePopup
                 // Diff mode is triggered by a simulationView, not by a
                 // selected component.
                 if not isDiffMode then dispatch ReloadSelectedComponent
    Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
        Level.left [] []
        Level.right [] [ Level.item [] [ Button.button [
            Button.Color IsPrimary
            Button.OnClick action
        ] [ str "Done" ] ] ]
    ]

let private makeEditor memory compId model dispatch =
    fun memoryEditorData ->
        div [] [
            makeEditorHeader memory false memoryEditorData dispatch
            makeEditorBody memory compId memoryEditorData model dispatch
        ]

/// Open a popup to view and edit the content of a memory.
let openMemoryEditor memory compId model dispatch : unit =
    // Build editor.
    let title = "Memory editor"
    let body = makeEditor memory compId model dispatch
    let foot = makeFoot false dispatch
    showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch

//=============//
// Diff viewer //
//=============//

let private makeDiffViewerBody memory1 memory2 memoryEditorData =
    let makeRow addr content1 content2 =
        let hasChanged = content1 <> content2
        let showRow addr =
            showRowWithAdrr memoryEditorData addr && (
                not memoryEditorData.OnlyDiff ||
                memoryEditorData.OnlyDiff && hasChanged)
        tr [ Style [ Display (if showRow addr then "table-row" else "none")] ] [
            td [] [ str <| hex addr ]
            td [
                Style [BackgroundColor (if hasChanged then "#ffc6d3" else "auto") ]
            ] [ str <| hex64 content1 ]
            td [
                Style [BackgroundColor (if hasChanged then "#baffd3" else "auto") ]
            ] [ str <| hex64 content2 ]
        ]
    div [bodyStyle] [
        Table.table [ Table.IsFullWidth ] [
            thead [] [ tr [] [
                th [] [str "Address"]
                th [] [str "Initial content"]
                th [] [str "Current content"]
            ] ]
            tbody [] (
                (memory1.Data, memory2.Data) ||> List.mapi2 makeRow
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
    let foot = makeFoot true dispatch
    showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch
