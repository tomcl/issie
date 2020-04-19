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

/// Save the state to a file and return the path where it was saved.
/// TODO: handle errors?
let saveStateToFile maybePath state =
    let writeToFile str path =
        fs.writeFile (path, str, ignore)
    let stateStr = stateToJsonString state
    let path =
        match maybePath with
        | Some path -> path
        | Option.None -> // Open dialog window to obtain the path.
            let options = createEmpty<SaveDialogOptions>
            options.filters <- fileFilterOpts
            electron.remote.dialog.showSaveDialog(options)
    match isNull path with
    | true -> Option.None // User did not completed the save dialog interaction.
    | false -> writeToFile stateStr path
               Some path

/// Load the state from a file and return the path from where it is loaded.
/// TODO: handle errors?
let loadStateFromFile (diagramWrapper : Draw2dWrapper) =
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
    let paths = electron.remote.dialog.showOpenDialog(options)
    match isNull paths with
    | true -> Option.None // User did not completed the load dialog interaction.
    | false -> match Seq.toList paths with
               | [path] -> loadFromPath path
                           Some path
               | _ -> Option.None

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
