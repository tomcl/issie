module StateIO

open DiagramTypes
open Draw2dWrapper

open Fable.SimpleJson

type private State = Component list * Connection list

let private stateToJsonString (state : State) : string =
    Json.stringify state

let private jsonStringToState (jsonString : string) : State =
    Json.parseAs<State> jsonString

let saveStateToFile (state : State) : unit =
    // TODO
    log "saving"
    log <| stateToJsonString state

let loadStateFromFile (diagramWrapper : Draw2dWrapper) : unit =
    // TODO
    log "loading"

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
