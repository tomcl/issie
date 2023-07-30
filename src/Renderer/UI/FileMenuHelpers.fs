module FileMenuHelpers
open EEExtensions
open Fulma
open Fable.React
open Fable.React.Props
open Fulma.Extensions.Wikiki

open Helpers
open JSHelpers
open DiagramStyle
open ModelType
open ModelHelpers
open CommonTypes
open FilesIO
open Extractor
open Notifications
open PopupView
open DrawModelType
open Sheet.SheetInterface
open Optics
open Optics.Operators
open System

let updateSymbolRAMs (ramCheck: Component list) (sModel: SymbolT.Model) =
    (sModel, ramCheck)
    ||> List.fold (fun sModel comp ->
            let cId = (ComponentId comp.Id)
            if Map.containsKey cId sModel.Symbols then 
                SymbolUpdate.writeMemoryType sModel cId comp.Type
            else
               sModel)


let loadComponentWithRAMChanges newCS savedWaveSim ldc model =
        let sheetInfo = {Form = ldc.Form; Description = ldc.Description} //only user defined sheets are editable and thus saveable
        let filePath = ldc.FilePath
        let (newLdc, ramCheck) = makeLoadedComponentFromCanvasData newCS filePath DateTime.Now savedWaveSim (Some sheetInfo)
        model
        |> Optic.map (sheet_ >-> SheetT.symbol_) (updateSymbolRAMs ramCheck)

/// temporary shim for compatibility while dispatch is still being used.
let raiseFileNotification  (dispatch : Msg -> unit) (msg: string option) =
    match msg with
    | Some err -> dispatch <| SetFilesNotification (errorFilesNotification err)
    | None -> ()
