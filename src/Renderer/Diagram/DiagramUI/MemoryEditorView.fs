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
let private headerHeight = 100;
let private headerStyle = Style [
    Position "fixed"
    MarginTop (string (-headerHeight-20) + "px")
    PaddingTop "20px"
    PaddingBottom "40px"
    BackgroundColor "white"
    Width "61%"
    Height headerHeight
    ZIndex 10
]
let private bodyStyle = Style [
    MarginTop (string headerHeight + "px")
]

let showError private msg dispatch =
    errorNotification msg CloseMemoryEditorNotification
    |> SetMemoryEditorNotification |> dispatch

//========//
// Editor //
//========//

let private makeEditorHeader memory =
    div [headerStyle] [
        str <| sprintf "Number of elements: %d" (pow2int64 memory.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" memory.WordWidth
    ]

let private makeEditorBody memory compId model dispatch =
    let makeRow addr content =
        tr [] [
            td [] [ str <| hex addr ]
            td [] [
                Input.text [
                    Input.DefaultValue <| hex64 content
                    Input.OnChange (getTextEventValue >> fun text ->
                        match strToIntCheckWidth text memory.WordWidth with
                        | Ok value ->
                            // Close error notification.
                            CloseMemoryEditorNotification |> dispatch
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
    div [] [
        makeEditorHeader memory
        makeEditorBody memory compId model dispatch
    ]

/// Open a popup to view and edit the content of a memory.
let openMemoryEditor memory compId model dispatch : unit =
    let title = "Memory editor"
    let body = makeEditor memory compId model dispatch
    let foot = makeFoot false dispatch
    showUnclosablePopup (Some title) body (Some foot) popupExtraStyle dispatch

//=============//
// Diff viewer //
//=============//

let private makeDiffViewerBody memory1 memory2 dispatch =
    let makeRow addr content1 content2 =
        let hasChanged = content1 <> content2
        tr [] [
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
    div [] [
        makeEditorHeader memory1
        makeDiffViewerBody memory1 memory2 dispatch
    ]

let openMemoryDiffViewer memory1 memory2 dispatch : unit =
    assertThat (memory1.AddressWidth = memory2.AddressWidth &&
                memory1.WordWidth = memory2.WordWidth)
    <| sprintf "Memories in diffViewer do not match: %A\n%A" memory1 memory2
    let title = "Memory diff viewer"
    let body = makeDiffViewer memory1 memory2 dispatch
    let foot = makeFoot true dispatch
    showUnclosablePopup (Some title) body (Some foot) popupExtraStyle dispatch
