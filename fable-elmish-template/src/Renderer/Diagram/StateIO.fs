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
    log <| stateToJsonString state

let loadStateFromFile (diagramWrapper : Draw2dWrapper) : unit =
    let data = """
    [[{"Id":"a7aed4c1-23f6-6a83-2eb1-a8b673837685","Type":"Or","Label":"hello","InputPorts":[{"Id":"26943173-e9bd-ca8d-3bec-26afa4923936","PortType":"Input","HostId":"a7aed4c1-23f6-6a83-2eb1-a8b673837685"},{"Id":"afe96ac0-b3b6-179f-1c9e-24dd50040d22","PortType":"Input","HostId":"a7aed4c1-23f6-6a83-2eb1-a8b673837685"}],"OutputPorts":[{"Id":"19ddc9cb-6531-cf1e-06ac-5b17bd1694d4","PortType":"Output","HostId":"a7aed4c1-23f6-6a83-2eb1-a8b673837685"}],"X":402,"Y":190},{"Id":"203dd1b9-b4b8-2b30-3c06-635753d84e30","Type":"Or","Label":"","InputPorts":[{"Id":"35a53006-c1db-068e-97da-a5e4f7722863","PortType":"Input","HostId":"203dd1b9-b4b8-2b30-3c06-635753d84e30"},{"Id":"d2cafe14-995c-22db-a99f-cf58b797d766","PortType":"Input","HostId":"203dd1b9-b4b8-2b30-3c06-635753d84e30"}],"OutputPorts":[{"Id":"689ebafe-6043-7f1f-dcba-997f37ebc48f","PortType":"Output","HostId":"203dd1b9-b4b8-2b30-3c06-635753d84e30"}],"X":578,"Y":335},{"Id":"654ae989-43b7-cd24-541e-11e28fad48b7","Type":"Mux2","Label":"mux2","InputPorts":[{"Id":"d1287f4b-b2bd-f889-4e1b-dc0514a76e35","PortType":"Input","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"},{"Id":"2b10930e-187d-6516-9e08-9e88a6bd2bac","PortType":"Input","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"},{"Id":"2c1bced8-0a5f-3466-c155-945570e269c9","PortType":"Input","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"}],"OutputPorts":[{"Id":"25628225-0e1b-db8b-e609-a8201b5ee821","PortType":"Output","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"}],"X":153,"Y":103}],[{"Id":"c87dc351-a94d-4528-043c-87be346d32fa","Source":{"Id":"2c1bced8-0a5f-3466-c155-945570e269c9","PortType":"Input","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"},"Target":{"Id":"689ebafe-6043-7f1f-dcba-997f37ebc48f","PortType":"Output","HostId":"203dd1b9-b4b8-2b30-3c06-635753d84e30"}},{"Id":"3ba1d562-acd2-084f-a42e-00329b97458a","Source":{"Id":"26943173-e9bd-ca8d-3bec-26afa4923936","PortType":"Input","HostId":"a7aed4c1-23f6-6a83-2eb1-a8b673837685"},"Target":{"Id":"25628225-0e1b-db8b-e609-a8201b5ee821","PortType":"Output","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"}},{"Id":"44409c33-402a-157e-243c-f6f1bdda702a","Source":{"Id":"19ddc9cb-6531-cf1e-06ac-5b17bd1694d4","PortType":"Output","HostId":"a7aed4c1-23f6-6a83-2eb1-a8b673837685"},"Target":{"Id":"35a53006-c1db-068e-97da-a5e4f7722863","PortType":"Input","HostId":"203dd1b9-b4b8-2b30-3c06-635753d84e30"}},{"Id":"e2b51695-5b3c-f320-27d8-48d81dd104a6","Source":{"Id":"d1287f4b-b2bd-f889-4e1b-dc0514a76e35","PortType":"Input","HostId":"654ae989-43b7-cd24-541e-11e28fad48b7"},"Target":{"Id":"19ddc9cb-6531-cf1e-06ac-5b17bd1694d4","PortType":"Output","HostId":"a7aed4c1-23f6-6a83-2eb1-a8b673837685"}}]]
    """
    let components, connections = jsonStringToState data
    List.map diagramWrapper.LoadComponent components |> ignore
    List.map diagramWrapper.LoadConnection connections |> ignore

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
