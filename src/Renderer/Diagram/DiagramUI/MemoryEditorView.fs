(*
    MemoryEditorView.fs

    A simple Popup editor to view and change the content of a memory.
*)

module MemoryEditorView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Helpers
open DiagramTypes
open DiagramMessageType
open PopupView

// TODO: reload selected component when leaving memory editor.

let private makeEditorHeader memory =
    div [] [
        str <| sprintf "Number of elements: %d" (pow2int64 memory.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" memory.WordWidth
    ]

let private makeEditorBody memory =
    let makeRow idx content =
        tr [] [
            td [] [ str <| sprintf "%d" idx ]
            td [] [ str <| sprintf "%d" content ] // TODO: make modifiable.
        ]
    Table.table [] [
        thead [] [
            tr [] [
                th [] [str "Address"]
                th [] [str "Content"]
            ]
        ]
        tbody [] (
            memory.Data |> List.mapi makeRow
        )
    ]

let private makeEditor memory model =
    div [] [
        makeEditorHeader memory
        makeEditorBody memory
    ]

let openMemoryEditor (memory : Memory) model dispatch : unit =
    let title = "Memory editor"
    let body = makeEditor memory model
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> dispatch ClosePopup)
                    ] [ str "Done" ]
                ]
            ]
        ]
    let extraStyle = [
        //Width "80%"
        Height "80%"
    ]
    closablePopup title body foot extraStyle dispatch
