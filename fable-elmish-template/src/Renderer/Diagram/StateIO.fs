module StateIO

open DiagramTypes
open Draw2dWrapper

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Fable.SimpleJson
open Node.Exports

let private stateToJsonString (state : CanvasState) : string =
    Json.stringify state

let private jsonStringToState (jsonString : string) : CanvasState =
    Json.parseAs<CanvasState> jsonString

let private fileFilterOpts =
    ResizeArray [
        createObj [
            "name" ==> "Diagram"
            "extensions" ==> ResizeArray [ "dgm" ]
        ]
    ] |> Some

let saveStateToFile (state : CanvasState) : unit =
    let writeToFile str path =
        fs.writeFile (path, str, ignore)
    let stateStr = stateToJsonString state
    // The next three lines open the window to the save the file.
    let options = createEmpty<SaveDialogOptions>
    options.filters <- fileFilterOpts
    let path = electron.remote.dialog.showSaveDialog (options)
    writeToFile stateStr path

let loadStateFromFile (diagramWrapper : Draw2dWrapper) : unit =
    let loadFromPath path =
        fs.readFile (path, (fun err dataBuffer ->
            let data = dataBuffer.toString ("utf8")
            let components, connections = jsonStringToState data
            diagramWrapper.ClearCanvas()
            List.map diagramWrapper.LoadComponent components |> ignore
            List.map diagramWrapper.LoadConnection connections |> ignore
        ))
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openFile" ]) |> Some
    options.filters <- fileFilterOpts
    electron.remote.dialog.showOpenDialog(options)
    |> Seq.toList
    |> List.map loadFromPath
    |> ignore

// TODO: move into a test.
//let comp = {
//    Id = "test"
//    Type = Not
//    Label = Some "label-test"
//    InputPorts = [
//        {
//            Id = "port-id"
//            Label = None
//            PortType = PortType.Input
//        }
//    ]
//    OutputPorts = []
//}
//
//let conn = {
//    Id = "pppport"
//    Source = {
//            Id = "port-id"
//            Label = Some "aaa"
//            PortType = PortType.Input
//        }
//    Target = {
//            Id = "port-id"
//            Label = None
//            PortType = PortType.Input
//        }
//}
//
//// serialize record into JSON
//let jsonStr = stateToJSONString <| ([comp], [conn])
//let data = JSONStringToState jsonStr
