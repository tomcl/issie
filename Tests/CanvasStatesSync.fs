module CanvasStatesSync

open CommonTypes

/// Simple DFF connected to an input and an output.
let stateSync1 : CanvasState =
    [
        {H=50; W=40;Id = "067434c0-3bf4-f4c4-551b-1001d73b024f";Type = DFF;Label = "";InputPorts = [{Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = Some 0;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];OutputPorts = [{Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = Some 0;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];X = 409;Y = 232};
        {H=50; W=40;Id = "6e7a2000-439c-108e-df6d-93cff7a41266";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "62496e2e-fa0d-3e2b-46f2-9ee99a141de6";PortNumber = Some 0;PortType = PortType.Output;HostId = "6e7a2000-439c-108e-df6d-93cff7a41266"}];X = 171;Y = 247};
        {H=50; W=40;Id = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd";Type = Output 1;Label = "out";InputPorts = [{Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"}];OutputPorts = [];X = 671;Y = 247}
    ],
    [
        {Id = "fe93a564-b9be-2ce5-61a0-89a57d1f4c30";Source = {Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = None;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Target = {Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = None;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"};Vertices = [459.0,257.0; 671.0,257.0]};
        {Id = "e1da6926-1493-ffb0-c85b-c58b00506cd6";Source = {Id = "62496e2e-fa0d-3e2b-46f2-9ee99a141de6";PortNumber = None;PortType = PortType.Output;HostId = "6e7a2000-439c-108e-df6d-93cff7a41266"};Target = {Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = None;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Vertices = [201.0,257.0; 409.0,257.0]}
    ]

/// stateSync1 loaded as a dependency.
let stateSync1Dependency : LoadedComponent = {
    Name = "single-dff"
    TimeStamp = System.DateTime.MinValue
    WaveInfo = None
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
        {H=50; W=40;Id = "067434c0-3bf4-f4c4-551b-1001d73b024f";Type = DFF;Label = "";InputPorts = [{Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = Some 0;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];OutputPorts = [{Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = Some 0;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];X = 351;Y = 185};
        {H=50; W=40;Id = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd";Type = Output 1;Label = "out";InputPorts = [{Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"}];OutputPorts = [];X = 752;Y = 200};
        {H=50; W=40;Id = "3739e54a-fd21-bf60-8fc2-a3d10108c947";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = Some 0;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"}];X = 200;Y = 200};
        {H=50; W=40;Id = "edda6ee1-8b6f-8f37-8177-4c073299a362";Type = DFF;Label = "";InputPorts = [{Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = Some 0;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];OutputPorts = [{Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = Some 0;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];X = 574;Y = 185}
    ],
    [
        {Id = "9e2ded72-08fd-c5d2-5bd7-f191c02f0d4d";Source = {Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = None;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"};Target = {Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = None;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"};Vertices = [624.0,210.0; 752.0,210.0]};
        {Id = "bc1e6ca7-6f71-697d-c81e-53598fcd5b4e";Source = {Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = None;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Target = {Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = None;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"};Vertices = [401.0,210.0; 574.0,210.0]};
        {Id = "9b5af8de-2de5-3a95-7347-316c3f9cfa8d";Source = {Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = None;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"};Target = {Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = None;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"};Vertices = [230.0,210.0; 351.0,210.0]}
    ]

/// Three DFF connected in series.
let stateSync3 : CanvasState =
    [
        {H=50; W=40;Id = "067434c0-3bf4-f4c4-551b-1001d73b024f";Type = DFF;Label = "";InputPorts = [{Id = "b595ca8e-027e-0024-2855-1169330c53f4";PortNumber = Some 0;PortType = PortType.Input;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];OutputPorts = [{Id = "547be6ee-118d-5bd6-2b3c-37ed3250302f";PortNumber = Some 0;PortType = PortType.Output;HostId = "067434c0-3bf4-f4c4-551b-1001d73b024f"}];X = 351;Y = 185};
        {H=50; W=40;Id = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd";Type = Output 1;Label = "out";InputPorts = [{Id = "5fd06e1a-90ba-64e8-24bf-d69f190a6b2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "a5d52bcd-0a6d-d123-7313-61d0b8b367fd"}];OutputPorts = [];X = 752;Y = 200};
        {H=50; W=40;Id = "3739e54a-fd21-bf60-8fc2-a3d10108c947";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "5c467246-0f2a-0da7-6e92-9f041d1a9fd8";PortNumber = Some 0;PortType = PortType.Output;HostId = "3739e54a-fd21-bf60-8fc2-a3d10108c947"}];X = 200;Y = 200};
        {H=50; W=40;Id = "edda6ee1-8b6f-8f37-8177-4c073299a362";Type = DFF;Label = "";InputPorts = [{Id = "7888376e-2242-39ac-9299-aef7e30f4465";PortNumber = Some 0;PortType = PortType.Input;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];OutputPorts = [{Id = "272e61d9-786c-f609-274c-9990997892cf";PortNumber = Some 0;PortType = PortType.Output;HostId = "edda6ee1-8b6f-8f37-8177-4c073299a362"}];X = 489;Y = 185};
        {H=50; W=40;Id = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650";Type = DFF;Label = "";InputPorts = [{Id = "2e1f0297-f4c6-c266-7dbb-d56fc63e386a";PortNumber = Some 0;PortType = PortType.Input;HostId = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650"}];OutputPorts = [{Id = "e0df158f-e186-c205-8693-9fa92c62d154";PortNumber = Some 0;PortType = PortType.Output;HostId = "36fd5a8f-ca4c-eb7e-e0f4-3b89109ad650"}];X = 613;Y = 185}
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
        {H=50; W=40;Id = "03e4c81a-4703-d9f5-dfaf-301de006610f";Type = Input 1;Label = "a";InputPorts = [];OutputPorts = [{Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = Some 0;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"}];X = 100;Y = 100};
        {H=50; W=40;Id = "781e7d9d-b18c-d614-dbc0-23bac9e617b7";Type = Output 1;Label = "b";InputPorts = [{Id = "a8e8da74-7bd2-f3a8-9ee5-9e8caa1ee0e5";PortNumber = Some 0;PortType = PortType.Input;HostId = "781e7d9d-b18c-d614-dbc0-23bac9e617b7"}];OutputPorts = [];X = 749;Y = 100};
        {H=50; W=40;Id = "547051c6-38f6-27ec-7c33-9c3bbee47e6e";Type = DFF;Label = "";InputPorts = [{Id = "7ec9e434-b066-daf3-3308-0a13a97c8880";PortNumber = Some 0;PortType = PortType.Input;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"}];OutputPorts = [{Id = "252c237c-1323-a291-eccb-f94392a37743";PortNumber = Some 0;PortType = PortType.Output;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"}];X = 500;Y = 85};
        {H=50; W=40;Id = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b";Type = Custom stateSync1CustomComponent;Label = "main";InputPorts = [{Id = "4c5a5a1a-e7e3-f53b-8059-0dfd34d796b1";PortNumber = Some 0;PortType = PortType.Input;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"}];OutputPorts = [{Id = "05578981-7a96-a101-b726-df761e234abb";PortNumber = Some 0;PortType = PortType.Output;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"}];X = 285;Y = 95}
    ],
    [
        {Id = "75474538-c7cb-de96-9fd2-5f55af7af631";Source = {Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = None;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"};Target = {Id = "4c5a5a1a-e7e3-f53b-8059-0dfd34d796b1";PortNumber = None;PortType = PortType.Input;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"};Vertices = [130.0,110.0; 285.0,110.0]};
        {Id = "4c146e97-db61-4848-5073-01c3785d2571";Source = {Id = "252c237c-1323-a291-eccb-f94392a37743";PortNumber = None;PortType = PortType.Output;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"};Target = {Id = "a8e8da74-7bd2-f3a8-9ee5-9e8caa1ee0e5";PortNumber = None;PortType = PortType.Input;HostId = "781e7d9d-b18c-d614-dbc0-23bac9e617b7"};Vertices = [550.0,110.0; 749.0,110.0]};
        {Id = "50982a96-2ea6-7db7-2eda-6c38456697c4";Source = {Id = "05578981-7a96-a101-b726-df761e234abb";PortNumber = None;PortType = PortType.Output;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"};Target = {Id = "7ec9e434-b066-daf3-3308-0a13a97c8880";PortNumber = None;PortType = PortType.Input;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"};Vertices = [345.0,110.0; 500.0,110.0]}
    ]

/// A DFF looping to itself via a Not gate. Two output nodes to probe the wires
/// before and after the Not gate. No inputs.
let stateSync5 : CanvasState =
    [
        {H=50; W=40;Id = "62a3108e-1198-502b-e338-e677815aead3";Type = Output 1;Label = "out1";InputPorts = [{Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = Some 0;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"}];OutputPorts = [];X = 417;Y = 117};
        {H=50; W=40;Id = "fc72c05a-5174-0334-aa74-be3ce27c3657";Type = Not;Label = "";InputPorts = [{Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = Some 0;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];OutputPorts = [{Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = Some 0;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];X = 169;Y = 184};
        {H=50; W=40;Id = "023094a0-9787-47ce-26af-03086cdc4b15";Type = Output 1;Label = "out2";InputPorts = [{Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = Some 0;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"}];OutputPorts = [];X = 229;Y = 138};
        {H=50; W=40;Id = "1a772449-853d-2171-1e47-fb9783b99556";Type = DFF;Label = "";InputPorts = [{Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = Some 0;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];OutputPorts = [{Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = Some 0;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];X = 273;Y = 343}
    ],
    [
        {Id = "5f42f1e8-0ccd-24e1-04a2-635d957c991b";Source = {Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = None;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Target = {Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = None;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"};Vertices = [199.0,199.0; 214.0,199.0; 214.0,148.0; 229.0,148.0]};
        {Id = "dc4cbb8e-e7a8-a008-97a8-0200f8720a81";Source = {Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = None;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Target = {Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = None;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Vertices = [199.0,199.0; 236.0,199.0; 236.0,368.0; 273.0,368.0]};
        {Id = "4810ec6b-a9f9-a562-58cf-82f9071755ad";Source = {Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = None;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Target = {Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = None;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Vertices = [323.0,368.0; 343.0,368.0; 343.0,283.5; 149.0,283.5; 149.0,199.0; 169.0,199.0]};
        {Id = "3831ffa7-549f-da3b-ab73-20c72e69b5b8";Source = {Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = None;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Target = {Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = None;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"};Vertices = [323.0,368.0; 370.0,368.0; 370.0,127.0; 417.0,127.0]}
    ]


/// Similar to stateSync5, but with a stateSync1 custom component instead of a
/// DFF. Loop with a synchronous custom component.
let stateSync6 : CanvasState =
    [
        {H=50; W=40;Id = "62a3108e-1198-502b-e338-e677815aead3";Type = Output 1;Label = "out1";InputPorts = [{Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = Some 0;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"}];OutputPorts = [];X = 417;Y = 117};
        {H=50; W=40;Id = "fc72c05a-5174-0334-aa74-be3ce27c3657";Type = Not;Label = "";InputPorts = [{Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = Some 0;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];OutputPorts = [{Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = Some 0;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];X = 169;Y = 184};
        {H=50; W=40;Id = "023094a0-9787-47ce-26af-03086cdc4b15";Type = Output 1;Label = "out2";InputPorts = [{Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = Some 0;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"}];OutputPorts = [];X = 229;Y = 138};
        {H=50; W=40;Id = "1a772449-853d-2171-1e47-fb9783b99556";Type = Custom stateSync1CustomComponent;Label = "";InputPorts = [{Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = Some 0;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];OutputPorts = [{Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = Some 0;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];X = 273;Y = 343}
    ],
    [
        {Id = "5f42f1e8-0ccd-24e1-04a2-635d957c991b";Source = {Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = None;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Target = {Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = None;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"};Vertices = [199.0,199.0; 214.0,199.0; 214.0,148.0; 229.0,148.0]};
        {Id = "dc4cbb8e-e7a8-a008-97a8-0200f8720a81";Source = {Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = None;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Target = {Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = None;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Vertices = [199.0,199.0; 236.0,199.0; 236.0,368.0; 273.0,368.0]};
        {Id = "4810ec6b-a9f9-a562-58cf-82f9071755ad";Source = {Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = None;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Target = {Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = None;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Vertices = [323.0,368.0; 343.0,368.0; 343.0,283.5; 149.0,283.5; 149.0,199.0; 169.0,199.0]};
        {Id = "3831ffa7-549f-da3b-ab73-20c72e69b5b8";Source = {Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = None;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Target = {Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = None;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"};Vertices = [323.0,368.0; 370.0,368.0; 370.0,127.0; 417.0,127.0]}
    ]

/// Input connected to two outputs. One of the two paths has a DFF in between.
/// Therefore, A to B-Comb is combinatorial path. A to B-Sync is a synchronous
/// path.
let stateSync7 : CanvasState =
    [
        {H=50; W=40;Id = "ff10125a-601f-e1d5-e379-7eb7c65eb91f";Type = Input 1;Label = "A";InputPorts = [];OutputPorts = [{Id = "32b40ca6-c21f-5b84-ca05-38d88b35b567";PortNumber = Some 0;PortType = PortType.Output;HostId = "ff10125a-601f-e1d5-e379-7eb7c65eb91f"}];X = 150;Y = 150};
        {H=50; W=40;Id = "2947473e-2eef-864c-217a-dd5c1daaae44";Type = DFF;Label = "";InputPorts = [{Id = "45a6e6f6-ed52-8de9-2fed-9f7a0bbb86dc";PortNumber = Some 0;PortType = PortType.Input;HostId = "2947473e-2eef-864c-217a-dd5c1daaae44"}];OutputPorts = [{Id = "25dc0c5b-e047-090b-5c2a-7ac216d80122";PortNumber = Some 0;PortType = PortType.Output;HostId = "2947473e-2eef-864c-217a-dd5c1daaae44"}];X = 230;Y = 210};
        {H=50; W=40;Id = "794d5154-6969-3f4e-9c8b-4bc17927c28f";Type = Output 1;Label = "B-Comb";InputPorts = [{Id = "dbdd44c2-14e4-449c-4d69-6dd78af61e2c";PortNumber = Some 0;PortType = PortType.Input;HostId = "794d5154-6969-3f4e-9c8b-4bc17927c28f"}];OutputPorts = [];X = 328;Y = 150};
        {H=50; W=40;Id = "95452292-b507-ab43-f082-85152d3e4cf2";Type = Output 1;Label = "B-Sync";InputPorts = [{Id = "05a19e53-570c-f02f-0af2-7ba35c4a4b71";PortNumber = Some 0;PortType = PortType.Input;HostId = "95452292-b507-ab43-f082-85152d3e4cf2"}];OutputPorts = [];X = 328;Y = 225}
    ],
    [
        {Id = "79418b3f-7ee3-997a-a433-32a9fbb49f18";Source = {Id = "32b40ca6-c21f-5b84-ca05-38d88b35b567";PortNumber = None;PortType = PortType.Output;HostId = "ff10125a-601f-e1d5-e379-7eb7c65eb91f"};Target = {Id = "dbdd44c2-14e4-449c-4d69-6dd78af61e2c";PortNumber = None;PortType = PortType.Input;HostId = "794d5154-6969-3f4e-9c8b-4bc17927c28f"};Vertices = [180.0,160.0; 328.0,160.0]};
        {Id = "51efab13-f633-205b-8cd4-c3c858ae25d8";Source = {Id = "32b40ca6-c21f-5b84-ca05-38d88b35b567";PortNumber = None;PortType = PortType.Output;HostId = "ff10125a-601f-e1d5-e379-7eb7c65eb91f"};Target = {Id = "45a6e6f6-ed52-8de9-2fed-9f7a0bbb86dc";PortNumber = None;PortType = PortType.Input;HostId = "2947473e-2eef-864c-217a-dd5c1daaae44"};Vertices = [180.0,160.0; 205.0,160.0; 205.0,235.0; 230.0,235.0]};
        {Id = "10c6a066-34dd-e0e2-d53d-32c34fcddde6";Source = {Id = "25dc0c5b-e047-090b-5c2a-7ac216d80122";PortNumber = None;PortType = PortType.Output;HostId = "2947473e-2eef-864c-217a-dd5c1daaae44"};Target = {Id = "05a19e53-570c-f02f-0af2-7ba35c4a4b71";PortNumber = None;PortType = PortType.Input;HostId = "95452292-b507-ab43-f082-85152d3e4cf2"};Vertices = [280.0,235.0; 328.0,235.0]}
    ]

/// stateSync7 loaded as a dependency.
let stateSync7Dependency : LoadedComponent = {
    Name = "combinatorial-sync"
    TimeStamp = System.DateTime.MinValue
    WaveInfo = None
    FilePath = ""
    CanvasState = stateSync7
    InputLabels = ["A", 1]
    OutputLabels = ["B-Comb", 1; "B-Sync", 1]
}

/// stateSync7 custom component.
let stateSync7CustomComponent : CustomComponentType =
    CanvasStates.makeCustomComponent stateSync7Dependency

/// stateSync7 Not-ed self looped in the synchronous branch. This is a
/// legitimate synchronous circuit and should pass. It is the same showed in
/// the report for the examples about combinatorial loops detection.
let stateSync8 : CanvasState =
    [
        {H=50; W=40;Id = "eb5353fd-fac6-3b2a-4de8-8949046671d2";Type = Custom stateSync7CustomComponent;Label = "";InputPorts = [{Id = "04f9c8fe-06c2-b485-e181-73aa36617622";PortNumber = Some 0;PortType = PortType.Input;HostId = "eb5353fd-fac6-3b2a-4de8-8949046671d2"}];OutputPorts = [{Id = "b9f547be-e285-cb90-1803-b8d3a4c0d351";PortNumber = Some 0;PortType = PortType.Output;HostId = "eb5353fd-fac6-3b2a-4de8-8949046671d2"}; {Id = "9935af7b-9671-60a2-a76a-f00d3a372d6c";PortNumber = Some 1;PortType = PortType.Output;HostId = "eb5353fd-fac6-3b2a-4de8-8949046671d2"}];X = 302;Y = 256};
        {H=50; W=40;Id = "66030e1a-4a97-244a-f0bb-d9e5fd25627f";Type = Output 1;Label = "B";InputPorts = [{Id = "8bfcfe42-be19-e2fa-42ae-25e929cab003";PortNumber = Some 0;PortType = PortType.Input;HostId = "66030e1a-4a97-244a-f0bb-d9e5fd25627f"}];OutputPorts = [];X = 488;Y = 266};
        {H=50; W=40;Id = "825b172a-9355-8bf1-10fc-b18f06d4e76b";Type = Not;Label = "";InputPorts = [{Id = "69688e66-1a74-e959-f58a-6085a5ae5c57";PortNumber = Some 0;PortType = PortType.Input;HostId = "825b172a-9355-8bf1-10fc-b18f06d4e76b"}];OutputPorts = [{Id = "7f763373-0473-16b8-75da-f80541b220ec";PortNumber = Some 0;PortType = PortType.Output;HostId = "825b172a-9355-8bf1-10fc-b18f06d4e76b"}];X = 536;Y = 281}
    ],
    [
        {Id = "c1ba620c-e715-5f47-d05b-1a6937750f13";Source = {Id = "7f763373-0473-16b8-75da-f80541b220ec";PortNumber = None;PortType = PortType.Output;HostId = "825b172a-9355-8bf1-10fc-b18f06d4e76b"};Target = {Id = "04f9c8fe-06c2-b485-e181-73aa36617622";PortNumber = None;PortType = PortType.Input;HostId = "eb5353fd-fac6-3b2a-4de8-8949046671d2"};Vertices = [566.0,296.0; 586.0,296.0; 586.0,395.0; 282.0,395.0; 282.0,286.0; 302.0,286.0]};
        {Id = "b1a62f29-11d6-957d-da22-f780a247ad24";Source = {Id = "9935af7b-9671-60a2-a76a-f00d3a372d6c";PortNumber = None;PortType = PortType.Output;HostId = "eb5353fd-fac6-3b2a-4de8-8949046671d2"};Target = {Id = "69688e66-1a74-e959-f58a-6085a5ae5c57";PortNumber = None;PortType = PortType.Input;HostId = "825b172a-9355-8bf1-10fc-b18f06d4e76b"};Vertices = [374.0,296.0; 536.0,296.0]};
        {Id = "ee6ffc42-4a79-edff-3eb2-7008eec7a649";Source = {Id = "b9f547be-e285-cb90-1803-b8d3a4c0d351";PortNumber = None;PortType = PortType.Output;HostId = "eb5353fd-fac6-3b2a-4de8-8949046671d2"};Target = {Id = "8bfcfe42-be19-e2fa-42ae-25e929cab003";PortNumber = None;PortType = PortType.Input;HostId = "66030e1a-4a97-244a-f0bb-d9e5fd25627f"};Vertices = [374.0,276.0; 488.0,276.0]}
    ]

/// stateSync8 loaded as a dependency.
let stateSync8Dependency : LoadedComponent = {
    Name = "fake-combinatorial-loop"
    TimeStamp = System.DateTime.MinValue
    WaveInfo = None
    FilePath = ""
    CanvasState = stateSync8
    InputLabels = []
    OutputLabels = ["B", 1;]
}

/// stateSync8 custom component.
let stateSync8CustomComponent : CustomComponentType =
    CanvasStates.makeCustomComponent stateSync8Dependency

/// StateSync8 connected to an output. Legitmate ciruit that behaves precisely
/// like stateSync8.
let stateSync9 : CanvasState =
    [
        {H=50; W=40;Id = "16e7b03d-6504-a148-e207-ce99634ee5c5";Type = Custom stateSync8CustomComponent;Label = "loop2";InputPorts = [];OutputPorts = [{Id = "1db2fde2-c823-9ac8-1b7f-5187166934e9";PortNumber = Some 0;PortType = PortType.Output;HostId = "16e7b03d-6504-a148-e207-ce99634ee5c5"}];X = 568;Y = 310};
        {H=50; W=40;Id = "a100bada-b27f-15ca-accb-153e717a31f1";Type = Output 1;Label = "B";InputPorts = [{Id = "b2c8d4ef-bb50-3b2b-259b-e6381ca798a4";PortNumber = Some 0;PortType = PortType.Input;HostId = "a100bada-b27f-15ca-accb-153e717a31f1"}];OutputPorts = [];X = 715;Y = 315}
    ],
    [
        {Id = "2b617de7-dfcc-0287-1d9e-0f6a3b40de5b";Source = {Id = "1db2fde2-c823-9ac8-1b7f-5187166934e9";PortNumber = None;PortType = PortType.Output;HostId = "16e7b03d-6504-a148-e207-ce99634ee5c5"};Target = {Id = "b2c8d4ef-bb50-3b2b-259b-e6381ca798a4";PortNumber = None;PortType = PortType.Input;HostId = "a100bada-b27f-15ca-accb-153e717a31f1"};Vertices = [618.0,325.0; 715.0,325.0]}
    ]

/// A fully connected DFFE.
let stateSync10 : CanvasState =
    [
        {H=50; W=40;Id = "5916f1cf-408e-4186-a839-c80926bfddf0";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "e894fb40-804a-36d1-b2e8-6033afb61dfa";PortNumber = Some 0;PortType = PortType.Output;HostId = "5916f1cf-408e-4186-a839-c80926bfddf0"}];X = 109;Y = 95};
        {H=50; W=40;Id = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf";Type = DFFE;Label = "";InputPorts = [{Id = "c766cb8d-a762-04b9-f021-4a9175fdbc01";PortNumber = Some 0;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"}; {Id = "2b80cebe-c0af-4905-d443-fd99796ec8df";PortNumber = Some 1;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"}];OutputPorts = [{Id = "ecee9ad7-925f-c0b9-26d5-4cf8b96a9be1";PortNumber = Some 0;PortType = PortType.Output;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"}];X = 273;Y = 80};
        {H=50; W=40;Id = "cab54371-5e07-9586-eb9b-be8cc417e610";Type = Input 1;Label = "en";InputPorts = [];OutputPorts = [{Id = "4c83ef6d-7267-2732-d828-c508fff7f889";PortNumber = Some 0;PortType = PortType.Output;HostId = "cab54371-5e07-9586-eb9b-be8cc417e610"}];X = 109;Y = 270};
        {H=50; W=40;Id = "a2c874bb-eaeb-d62d-8a72-5eeae48db694";Type = Output 1;Label = "out";InputPorts = [{Id = "df5f625f-43c4-28bb-c931-eae047edaa14";PortNumber = Some 0;PortType = PortType.Input;HostId = "a2c874bb-eaeb-d62d-8a72-5eeae48db694"}];OutputPorts = [];X = 495;Y = 95}
    ],
    [
        {Id = "7ab86f0a-1b2c-7901-0a12-cbe447f1f06d";Source = {Id = "e894fb40-804a-36d1-b2e8-6033afb61dfa";PortNumber = None;PortType = PortType.Output;HostId = "5916f1cf-408e-4186-a839-c80926bfddf0"};Target = {Id = "c766cb8d-a762-04b9-f021-4a9175fdbc01";PortNumber = None;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"};Vertices = [139.0,105.0; 273.0,105.0]};
        {Id = "04bb7d64-1899-a4f3-6c50-f53ccd84cd49";Source = {Id = "ecee9ad7-925f-c0b9-26d5-4cf8b96a9be1";PortNumber = None;PortType = PortType.Output;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"};Target = {Id = "df5f625f-43c4-28bb-c931-eae047edaa14";PortNumber = None;PortType = PortType.Input;HostId = "a2c874bb-eaeb-d62d-8a72-5eeae48db694"};Vertices = [353.0,105.0; 495.0,105.0]};
        {Id = "de4f03d1-580c-eabb-7439-38b664b89aba";Source = {Id = "4c83ef6d-7267-2732-d828-c508fff7f889";PortNumber = None;PortType = PortType.Output;HostId = "cab54371-5e07-9586-eb9b-be8cc417e610"};Target = {Id = "2b80cebe-c0af-4905-d443-fd99796ec8df";PortNumber = None;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"};Vertices = [139.0,280.938; 313.0,280.938; 313.0,130.0]}
    ]

/// stateSync7 Not-ed self looped in the combinatorial branch. This is NOT a
/// legitimate circuit, has combinatorial cycle. Similar to stateSync8 but loops
/// with the other output of the custom component.
let stateSync11 : CanvasState =
    [
        {H=50; W=40;Id = "c9d9659a-4476-de3a-a838-eeab15496c99";Type = Custom stateSync7CustomComponent;Label = "loop1";InputPorts = [{Id = "c52782db-1ac3-f0d5-28ca-02c8410eb78d";PortNumber = Some 0;PortType = PortType.Input;HostId = "c9d9659a-4476-de3a-a838-eeab15496c99"}];OutputPorts = [{Id = "eda8e4f2-f21b-12fc-b209-ad315b99a851";PortNumber = Some 0;PortType = PortType.Output;HostId = "c9d9659a-4476-de3a-a838-eeab15496c99"}; {Id = "627b5ddf-e4ef-5629-70d4-af674cf37b96";PortNumber = Some 1;PortType = PortType.Output;HostId = "c9d9659a-4476-de3a-a838-eeab15496c99"}];X = 508;Y = 245};
        {H=50; W=40;Id = "5c24921a-88e9-7bc2-ee89-97fefb694902";Type = Not;Label = "";InputPorts = [{Id = "e16cf7e3-d01f-29e5-8d0d-921194f2f909";PortNumber = Some 0;PortType = PortType.Input;HostId = "5c24921a-88e9-7bc2-ee89-97fefb694902"}];OutputPorts = [{Id = "50f7aed7-49de-5472-defa-cbf07c2c4f56";PortNumber = Some 0;PortType = PortType.Output;HostId = "5c24921a-88e9-7bc2-ee89-97fefb694902"}];X = 664;Y = 245};
        {H=50; W=40;Id = "68bfdbd5-b91f-9f34-0a32-2f8730856d49";Type = Output 1;Label = "B-Sync";InputPorts = [{Id = "ce17f39f-f79e-d9c5-78dd-379d4be0d3a4";PortNumber = Some 0;PortType = PortType.Input;HostId = "68bfdbd5-b91f-9f34-0a32-2f8730856d49"}];OutputPorts = [];X = 701;Y = 273}
    ],
    [
        {Id = "24e0a9c6-5a3a-e5e9-5034-0f831dcbe0f9";Source = {Id = "627b5ddf-e4ef-5629-70d4-af674cf37b96";PortNumber = None;PortType = PortType.Output;HostId = "c9d9659a-4476-de3a-a838-eeab15496c99"};Target = {Id = "ce17f39f-f79e-d9c5-78dd-379d4be0d3a4";PortNumber = None;PortType = PortType.Input;HostId = "68bfdbd5-b91f-9f34-0a32-2f8730856d49"};Vertices = [580.0,285.0; 640.5,285.0; 640.5,283.0; 701.0,283.0]};
        {Id = "bc1f9a51-5ca8-1aa0-b4bd-b86bc9646c20";Source = {Id = "eda8e4f2-f21b-12fc-b209-ad315b99a851";PortNumber = None;PortType = PortType.Output;HostId = "c9d9659a-4476-de3a-a838-eeab15496c99"};Target = {Id = "e16cf7e3-d01f-29e5-8d0d-921194f2f909";PortNumber = None;PortType = PortType.Input;HostId = "5c24921a-88e9-7bc2-ee89-97fefb694902"};Vertices = [580.0,265.0; 622.0,265.0; 622.0,260.0; 664.0,260.0]};
        {Id = "cc7a9da2-f78a-f3e1-1c2f-7323f2b43d15";Source = {Id = "50f7aed7-49de-5472-defa-cbf07c2c4f56";PortNumber = None;PortType = PortType.Output;HostId = "5c24921a-88e9-7bc2-ee89-97fefb694902"};Target = {Id = "c52782db-1ac3-f0d5-28ca-02c8410eb78d";PortNumber = None;PortType = PortType.Input;HostId = "c9d9659a-4476-de3a-a838-eeab15496c99"};Vertices = [694.0,260.0; 714.0,260.0; 714.0,195.5; 488.0,195.5; 488.0,275.0; 508.0,275.0]}
    ]

/// stateSync11 loaded as a dependency.
let stateSync11Dependency : LoadedComponent = {
    Name = "combinatorial-loop"
    TimeStamp = System.DateTime.MinValue
    WaveInfo = None
    FilePath = ""
    CanvasState = stateSync11
    InputLabels = []
    OutputLabels = ["B-Sync", 1;]
}

/// stateSync11 custom component.
let stateSync11CustomComponent : CustomComponentType =
    CanvasStates.makeCustomComponent stateSync11Dependency

/// StateSync11 connected to an output. Should spot cycle in the dependency.
let stateSync12 : CanvasState =
    [
        {H=50; W=40;Id = "eab95b08-cf95-15b7-ad8f-4eaffcefff6f";Type = Custom stateSync11CustomComponent;Label = "loop2-comb";InputPorts = [];OutputPorts = [{Id = "685a78e3-c891-d0c3-f973-1814b01edd28";PortNumber = Some 0;PortType = PortType.Output;HostId = "eab95b08-cf95-15b7-ad8f-4eaffcefff6f"}];X = 354;Y = 215};
        {H=50; W=40;Id = "638304a8-99e8-fe4f-4503-b50c64f45756";Type = Output 1;Label = "B-Comb";InputPorts = [{Id = "3b0171fa-dffe-f4c5-5f8c-e365112bae2f";PortNumber = Some 0;PortType = PortType.Input;HostId = "638304a8-99e8-fe4f-4503-b50c64f45756"}];OutputPorts = [];X = 532;Y = 220}
    ],
    [
        {Id = "f7c55a63-955e-6edd-0db6-fe320dc02c02";Source = {Id = "685a78e3-c891-d0c3-f973-1814b01edd28";PortNumber = None;PortType = PortType.Output;HostId = "eab95b08-cf95-15b7-ad8f-4eaffcefff6f"};Target = {Id = "3b0171fa-dffe-f4c5-5f8c-e365112bae2f";PortNumber = None;PortType = PortType.Input;HostId = "638304a8-99e8-fe4f-4503-b50c64f45756"};Vertices = [420.0,230.0; 532.0,230.0]}
    ]
