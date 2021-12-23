//(*
//    TruthTableView.fs
//
//    View for Truth Table in the right tab.
//*)
//

module TruthTableView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open NumberHelpers
open Helpers
open TimeHelpers
open JSHelpers
open DiagramStyle
open Notifications
open PopupView
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open Extractor
open Simulator
open TruthTableCreate

let viewTruthTable model dispatch =
    printf "Viewing Truth Table"
    let simRes = SimulationView.makeSimData model
    let buttonColor, buttonText =
        match simRes with
        | None -> IColor.IsWhite, ""
        | Some (Ok sd,_) -> 
            if sd.IsSynchronous = false then 
                IColor.IsSuccess, "Generate Truth Table" 
            else 
                IColor.IsInfo, "Combinational Only!"
        | Some (Error _,_) -> IColor.IsWarning, "See Problems"
    div [] [
        str "Generate Truth Tables for the whole sheet using this tab."
        br []
        str "Please note that Truth Tables can only be generated for Combinational Logic Circuits"
        br []; br []
        Button.button
            [ 
                Button.Color buttonColor; 
                Button.OnClick (fun _ -> ()) ; 
            ]
            [ str buttonText ]
    ]