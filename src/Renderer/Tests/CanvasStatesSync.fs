module CanvasStatesSync

open DiagramTypes

/// Simple DFF connected to an input and an output.
let stateSync1 : CanvasState =
    [
        {Id = "067434c0-3bf4-f4c4-551b-1001d73b024f";Type = DFF;Label = "";InputPorts = [{Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = Some 0;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];OutputPorts = [{Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = Some 0;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];X = 409;Y = 232};
        {Id = "6e7a2000-439c-108e-df6d-93cff7a41266";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "62496e2e-fa0d-3e2b-46f2-9ee99a141de6";PortNumber = Some 0;PortType = PortType.Output;HostId = "6e7a2000-439c-108e-df6d-93cff7a41266"}];X = 171;Y = 247};
        {Id = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd";Type = Output 1;Label = "out";InputPorts = [{Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"}];OutputPorts = [];X = 671;Y = 247}
    ],
    [
        {Id = "fe93a564-b9be-2ce5-61a0-89a57d1f4c30";Source = {Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = None;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Target = {Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = None;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"};Vertices = [459.0,257.0; 671.0,257.0]};
        {Id = "e1da6926-1493-ffb0-c85b-c58b00506cd6";Source = {Id = "62496e2e-fa0d-3e2b-46f2-9ee99a141de6";PortNumber = None;PortType = PortType.Output;HostId = "6e7a2000-439c-108e-df6d-93cff7a41266"};Target = {Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = None;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Vertices = [201.0,257.0; 409.0,257.0]}
    ]

/// stateSync1 loaded as a dependency.
let stateSync1Dependency : LoadedComponent = {
    Name = "single-dff"
    FilePath = ""
    CanvasState = stateSync1
    InputLabels = ["in", 1]
    OutputLabels = ["out", 1]
}

/// stateSync1 custom component.
let stateSync1CustomComponent : CustomComponentType = 
    CanvasStates.makeCustomComponent stateSync1Dependency

/// Two DFF connected in series.
let stateSync2 : CanvasState =
    [
        {Id = "067434c0-3bf4-f4c4-551b-1001d73b024f";Type = DFF;Label = "";InputPorts = [{Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = Some 0;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];OutputPorts = [{Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = Some 0;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];X = 351;Y = 185};
        {Id = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd";Type = Output 1;Label = "out";InputPorts = [{Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"}];OutputPorts = [];X = 752;Y = 200};
        {Id = "3739e54a-fd21-bf60-8fc2-a3d10108c947";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = Some 0;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"}];X = 200;Y = 200};
        {Id = "edda6ee1-8b6f-8f37-8177-4c073299a362";Type = DFF;Label = "";InputPorts = [{Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = Some 0;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];OutputPorts = [{Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = Some 0;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];X = 574;Y = 185}
    ],
    [
        {Id = "9e2ded72-08fd-c5d2-5bd7-f191c02f0d4d";Source = {Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = None;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"};Target = {Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = None;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"};Vertices = [624.0,210.0; 752.0,210.0]};
        {Id = "bc1e6ca7-6f71-697d-c81e-53598fcd5b4e";Source = {Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = None;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Target = {Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = None;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"};Vertices = [401.0,210.0; 574.0,210.0]};
        {Id = "9b5af8de-2de5-3a95-7347-316c3f9cfa8d";Source = {Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = None;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"};Target = {Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = None;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Vertices = [230.0,210.0; 351.0,210.0]}
    ]

/// Three DFF connected in series.
let stateSync3 : CanvasState =
    [
        {Id = "067434c0-3bf4-f4c4-551b-1001d73b024f";Type = DFF;Label = "";InputPorts = [{Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = Some 0;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];OutputPorts = [{Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = Some 0;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];X = 351;Y = 185};
        {Id = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd";Type = Output 1;Label = "out";InputPorts = [{Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"}];OutputPorts = [];X = 752;Y = 200};
        {Id = "3739e54a-fd21-bf60-8fc2-a3d10108c947";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = Some 0;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"}];X = 200;Y = 200};
        {Id = "edda6ee1-8b6f-8f37-8177-4c073299a362";Type = DFF;Label = "";InputPorts = [{Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = Some 0;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];OutputPorts = [{Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = Some 0;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];X = 489;Y = 185};
        {Id = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650";Type = DFF;Label = "";InputPorts = [{Id = "2e1f0297-f4c6-c266-7dbb-d56fc63e386a";PortNumber = Some 0;PortType = PortType.Input;HostId = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650"}];OutputPorts = [{Id = "e0df158f-e186-c205-8693-9fa92c62d154";PortNumber = Some 0;PortType = PortType.Output;HostId = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650"}];X = 613;Y = 185}
    ],
    [
        {Id = "9e2ded72-08fd-c5d2-5bd7-f191c02f0d4d";Source = {Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = None;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"};Target = {Id = "2e1f0297-f4c6-c266-7dbb-d56fc63e386a";PortNumber = None;PortType = PortType.Input;HostId = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650"};Vertices = [539.0,210.0; 613.0,210.0]};
        {Id = "9b5af8de-2de5-3a95-7347-316c3f9cfa8d";Source = {Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = None;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"};Target = {Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = None;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Vertices = [230.0,210.0; 351.0,210.0]};
        {Id = "bc1e6ca7-6f71-697d-c81e-53598fcd5b4e";Source = {Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = None;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Target = {Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = None;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"};Vertices = [401.0,210.0; 489.0,210.0]};
        {Id = "56b29fa5-a6e0-45d1-d2c0-4d3f4d82d94c";Source = {Id = "e0df158f-e186-c205-8693-9fa92c62d154";PortNumber = None;PortType = PortType.Output;HostId = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650"};Target = {Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = None;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"};Vertices = [663.0,210.0; 752.0,210.0]}
    ]

/// StateSync1 custom component followed by a DFF.
let stateSync4 : CanvasState =
    [
        {Id = "03e4c81a-4703-d9f5-dfaf-301de006610f";Type = Input 1;Label = "a";InputPorts = [];OutputPorts = [{Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = Some 0;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"}];X = 100;Y = 100};
        {Id = "781e7d9d-b18c-d614-dbc0-23bac9e617b7";Type = Output 1;Label = "b";InputPorts = [{Id = "a8e8da74-7bd2-f3a8-9ee5-9e8caa1ee0e5";PortNumber = Some 0;PortType = PortType.Input;HostId = "781e7d9d-b18c-d614-dbc0-23bac9e617b7"}];OutputPorts = [];X = 749;Y = 100};
        {Id = "547051c6-38f6-27ec-7c33-9c3bbee47e6e";Type = DFF;Label = "";InputPorts = [{Id = "7ec9e434-b066-daf3-3308-0a13a97c8880";PortNumber = Some 0;PortType = PortType.Input;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"}];OutputPorts = [{Id = "252c237c-1323-a291-eccb-f94392a37743";PortNumber = Some 0;PortType = PortType.Output;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"}];X = 500;Y = 85};
        {Id = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b";Type = Custom stateSync1CustomComponent;Label = "main";InputPorts = [{Id = "4c5a5a1a-e7e3-f53b-8059-0dfd34d796b1";PortNumber = Some 0;PortType = PortType.Input;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"}];OutputPorts = [{Id = "05578981-7a96-a101-b726-df761e234abb";PortNumber = Some 0;PortType = PortType.Output;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"}];X = 285;Y = 95}
    ],
    [
        {Id = "75474538-c7cb-de96-9fd2-5f55af7af631";Source = {Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = None;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"};Target = {Id = "4c5a5a1a-e7e3-f53b-8059-0dfd34d796b1";PortNumber = None;PortType = PortType.Input;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"};Vertices = [130.0,110.0; 285.0,110.0]};
        {Id = "4c146e97-db61-4848-5073-01c3785d2571";Source = {Id = "252c237c-1323-a291-eccb-f94392a37743";PortNumber = None;PortType = PortType.Output;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"};Target = {Id = "a8e8da74-7bd2-f3a8-9ee5-9e8caa1ee0e5";PortNumber = None;PortType = PortType.Input;HostId = "781e7d9d-b18c-d614-dbc0-23bac9e617b7"};Vertices = [550.0,110.0; 749.0,110.0]};
        {Id = "50982a96-2ea6-7db7-2eda-6c38456697c4";Source = {Id = "05578981-7a96-a101-b726-df761e234abb";PortNumber = None;PortType = PortType.Output;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"};Target = {Id = "7ec9e434-b066-daf3-3308-0a13a97c8880";PortNumber = None;PortType = PortType.Input;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"};Vertices = [345.0,110.0; 500.0,110.0]}
    ]
