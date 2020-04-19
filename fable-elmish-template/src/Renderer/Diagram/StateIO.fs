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
    let data = """
    [[{"Id":"b132a5c3-d136-ee93-0cea-8c1d9cd5567a","Type":"And","Label":"","InputPorts":[{"Id":"c3092d92-1ef3-4aac-b166-2929e3987123","PortType":"Input"},{"Id":"d634c0e2-a897-e919-5aeb-04359ab56a08","PortType":"Input"}],"OutputPorts":[{"Id":"05af7f83-6fad-4a75-28f1-8ff4720d4d34","PortType":"Output"}],"X":100,"Y":100},{"Id":"19e7099d-a9b1-485f-d361-7a5804e90dac","Type":"Xnor","Label":"hello world","InputPorts":[{"Id":"496be2e5-405a-5805-c155-d46b548e1c21","PortType":"Input"},{"Id":"7bf63951-cfc3-7792-7852-c4cc5fb8718d","PortType":"Input"}],"OutputPorts":[{"Id":"79d1f932-1292-1bd1-d997-4d89c73b32d2","PortType":"Output"}],"X":318,"Y":272},{"Id":"226225f6-ba6d-b209-2013-228b2e716291","Type":"Mux2","Label":"mux2","InputPorts":[{"Id":"d00ca0c2-3307-3055-872d-38350ef3b248","PortType":"Input"},{"Id":"6673f69a-9d52-3fe2-2b2e-971c14036c73","PortType":"Input"},{"Id":"4c929fa9-46a3-013f-c629-4494f7253436","PortType":"Input"}],"OutputPorts":[{"Id":"34b58845-fa4a-9e1a-4f48-e0ae4c1bb1cd","PortType":"Output"}],"X":399,"Y":105}],[]]
    """
    let components, connections = jsonStringToState data
    List.map diagramWrapper.LoadComponent components |> ignore
    // TODO add connections
    log connections

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
