module CanvasStatesSync

open CommonTypes

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

/// A DFF looping to itself via a Not gate. Two output nodes to probe the wires
/// before and after the Not gate. No inputs.
let stateSync5 : CanvasState =
    [
        {Id = "62a3108e-1198-502b-e338-e677815aead3";Type = Output 1;Label = "out1";InputPorts = [{Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = Some 0;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"}];OutputPorts = [];X = 417;Y = 117};
        {Id = "fc72c05a-5174-0334-aa74-be3ce27c3657";Type = Not;Label = "";InputPorts = [{Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = Some 0;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];OutputPorts = [{Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = Some 0;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];X = 169;Y = 184};
        {Id = "023094a0-9787-47ce-26af-03086cdc4b15";Type = Output 1;Label = "out2";InputPorts = [{Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = Some 0;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"}];OutputPorts = [];X = 229;Y = 138};
        {Id = "1a772449-853d-2171-1e47-fb9783b99556";Type = DFF;Label = "";InputPorts = [{Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = Some 0;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];OutputPorts = [{Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = Some 0;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];X = 273;Y = 343}
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
        {Id = "62a3108e-1198-502b-e338-e677815aead3";Type = Output 1;Label = "out1";InputPorts = [{Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = Some 0;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"}];OutputPorts = [];X = 417;Y = 117};
        {Id = "fc72c05a-5174-0334-aa74-be3ce27c3657";Type = Not;Label = "";InputPorts = [{Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = Some 0;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];OutputPorts = [{Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = Some 0;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"}];X = 169;Y = 184};
        {Id = "023094a0-9787-47ce-26af-03086cdc4b15";Type = Output 1;Label = "out2";InputPorts = [{Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = Some 0;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"}];OutputPorts = [];X = 229;Y = 138};
        {Id = "1a772449-853d-2171-1e47-fb9783b99556";Type = Custom stateSync1CustomComponent;Label = "";InputPorts = [{Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = Some 0;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];OutputPorts = [{Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = Some 0;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"}];X = 273;Y = 343}
    ],
    [
        {Id = "5f42f1e8-0ccd-24e1-04a2-635d957c991b";Source = {Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = None;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Target = {Id = "0ed71ded-67ad-cd1b-0ff0-a1e167900051";PortNumber = None;PortType = PortType.Input;HostId = "023094a0-9787-47ce-26af-03086cdc4b15"};Vertices = [199.0,199.0; 214.0,199.0; 214.0,148.0; 229.0,148.0]};
        {Id = "dc4cbb8e-e7a8-a008-97a8-0200f8720a81";Source = {Id = "63ca8512-31ee-88e6-9d25-8fdc727d2418";PortNumber = None;PortType = PortType.Output;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Target = {Id = "8b78d961-4203-c0ae-3229-c3a02c42065a";PortNumber = None;PortType = PortType.Input;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Vertices = [199.0,199.0; 236.0,199.0; 236.0,368.0; 273.0,368.0]};
        {Id = "4810ec6b-a9f9-a562-58cf-82f9071755ad";Source = {Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = None;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Target = {Id = "add99bad-5b00-ab97-4f13-2903a14b2f4e";PortNumber = None;PortType = PortType.Input;HostId = "fc72c05a-5174-0334-aa74-be3ce27c3657"};Vertices = [323.0,368.0; 343.0,368.0; 343.0,283.5; 149.0,283.5; 149.0,199.0; 169.0,199.0]};
        {Id = "3831ffa7-549f-da3b-ab73-20c72e69b5b8";Source = {Id = "abd2d9ec-a752-cb9d-b97e-afd7e2eea26d";PortNumber = None;PortType = PortType.Output;HostId = "1a772449-853d-2171-1e47-fb9783b99556"};Target = {Id = "3a7fdd4d-59d0-60cf-9eaf-cd2c7fd16264";PortNumber = None;PortType = PortType.Input;HostId = "62a3108e-1198-502b-e338-e677815aead3"};Vertices = [323.0,368.0; 370.0,368.0; 370.0,127.0; 417.0,127.0]}
    ]

/// Similar to stateSync6, but with an extra connection input to output.
/// This component is therefore cosnidered combinatorial, not synchronous.
/// b receives the input after 2 Clock ticks, b1 receives it immediately.
let stateSync7 : CanvasState =
    [
        {Id = "03e4c81a-4703-d9f5-dfaf-301de006610f";Type = Input 1;Label = "a";InputPorts = [];OutputPorts = [{Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = Some 0;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"}];X = 100;Y = 100};
        {Id = "781e7d9d-b18c-d614-dbc0-23bac9e617b7";Type = Output 1;Label = "b";InputPorts = [{Id = "a8e8da74-7bd2-f3a8-9ee5-9e8caa1ee0e5";PortNumber = Some 0;PortType = PortType.Input;HostId = "781e7d9d-b18c-d614-dbc0-23bac9e617b7"}];OutputPorts = [];X = 749;Y = 100};
        {Id = "547051c6-38f6-27ec-7c33-9c3bbee47e6e";Type = DFF;Label = "";InputPorts = [{Id = "7ec9e434-b066-daf3-3308-0a13a97c8880";PortNumber = Some 0;PortType = PortType.Input;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"}];OutputPorts = [{Id = "252c237c-1323-a291-eccb-f94392a37743";PortNumber = Some 0;PortType = PortType.Output;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"}];X = 500;Y = 85};
        {Id = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b";Type = Custom stateSync1CustomComponent;Label = "main";InputPorts = [{Id = "4c5a5a1a-e7e3-f53b-8059-0dfd34d796b1";PortNumber = Some 0;PortType = PortType.Input;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"}];OutputPorts = [{Id = "05578981-7a96-a101-b726-df761e234abb";PortNumber = Some 0;PortType = PortType.Output;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"}];X = 285;Y = 95};
        {Id = "fc5099db-d220-b42f-2add-b8c057164cb1";Type = Output 1;Label = "b1";InputPorts = [{Id = "64af4f69-11a9-ae0f-6b4d-48452658249e";PortNumber = Some 0;PortType = PortType.Input;HostId = "fc5099db-d220-b42f-2add-b8c057164cb1"}];OutputPorts = [];X = 749;Y = 240}
    ],
    [
        {Id = "75474538-c7cb-de96-9fd2-5f55af7af631";Source = {Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = None;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"};Target = {Id = "4c5a5a1a-e7e3-f53b-8059-0dfd34d796b1";PortNumber = None;PortType = PortType.Input;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"};Vertices = [130.0,110.0; 285.0,110.0]};
        {Id = "d2622b0e-7b0a-3028-c91a-a02ef95b1b4f";Source = {Id = "0caf419c-354b-78b8-a12b-e5fa450d4fc0";PortNumber = None;PortType = PortType.Output;HostId = "03e4c81a-4703-d9f5-dfaf-301de006610f"};Target = {Id = "64af4f69-11a9-ae0f-6b4d-48452658249e";PortNumber = None;PortType = PortType.Input;HostId = "fc5099db-d220-b42f-2add-b8c057164cb1"};Vertices = [130.0,110.0; 201.5,110.0; 201.5,250.0; 749.0,250.0]};
        {Id = "50982a96-2ea6-7db7-2eda-6c38456697c4";Source = {Id = "05578981-7a96-a101-b726-df761e234abb";PortNumber = None;PortType = PortType.Output;HostId = "423bb75b-e791-14f9-ecc2-1ac1c73fc55b"};Target = {Id = "7ec9e434-b066-daf3-3308-0a13a97c8880";PortNumber = None;PortType = PortType.Input;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"};Vertices = [345.0,110.0; 500.0,110.0]};
        {Id = "4c146e97-db61-4848-5073-01c3785d2571";Source = {Id = "252c237c-1323-a291-eccb-f94392a37743";PortNumber = None;PortType = PortType.Output;HostId = "547051c6-38f6-27ec-7c33-9c3bbee47e6e"};Target = {Id = "a8e8da74-7bd2-f3a8-9ee5-9e8caa1ee0e5";PortNumber = None;PortType = PortType.Input;HostId = "781e7d9d-b18c-d614-dbc0-23bac9e617b7"};Vertices = [550.0,110.0; 749.0,110.0]}
    ]

/// stateSync7 loaded as a dependency.
let stateSync7Dependency : LoadedComponent = {
    Name = "combinatorial-sync"
    FilePath = ""
    CanvasState = stateSync7
    InputLabels = ["a", 1]
    OutputLabels = ["b", 1; "b1", 1]
}

/// stateSync7 custom component.
let stateSync7CustomComponent : CustomComponentType =
    CanvasStates.makeCustomComponent stateSync7Dependency

/// Create a Not-ed self loop with the custom component of stateSync7. Connect
/// both outputs of stateSync7 to outputs. This would be a legitimate circuit,
/// but stateSync7 has a combinatorial connection input to output, hence it is
/// considered combinatorial, and it is part of a combinatorial loop.
let stateSync8 : CanvasState =
    [
        {Id = "44beb433-285e-8199-2b3a-d18d7bce1c6f";Type = Output 1;Label = "dn-out";InputPorts = [{Id = "b2909f77-bbc1-4428-1bd3-f5b37ce4ef9b";PortNumber = Some 0;PortType = PortType.Input;HostId = "44beb433-285e-8199-2b3a-d18d7bce1c6f"}];OutputPorts = [];X = 633;Y = 182};
        {Id = "5baefd71-8841-6e27-5930-ce3c4530fc4d";Type = Not;Label = "";InputPorts = [{Id = "16f663d6-34a1-1895-938a-aba5cc88cc84";PortNumber = Some 0;PortType = PortType.Input;HostId = "5baefd71-8841-6e27-5930-ce3c4530fc4d"}];OutputPorts = [{Id = "c09b810b-d411-b803-5495-d95230c22ae6";PortNumber = Some 0;PortType = PortType.Output;HostId = "5baefd71-8841-6e27-5930-ce3c4530fc4d"}];X = 182;Y = 76};
        {Id = "5339d358-0ac2-f907-4b2c-ba52b1a090b6";Type = Custom stateSync7CustomComponent;Label = "nested";InputPorts = [{Id = "782db954-6514-eafd-7829-7446feb3ca81";PortNumber = Some 0;PortType = PortType.Input;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"}];OutputPorts = [{Id = "3875c8c1-ca03-c13a-b360-97b8eceefb36";PortNumber = Some 0;PortType = PortType.Output;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"}; {Id = "dfd35e38-4b9c-ad0e-4f28-3c5e9d497b0a";PortNumber = Some 1;PortType = PortType.Output;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"}];X = 253;Y = 148};
        {Id = "e0745d45-ab87-81ca-89a4-33629ff2c470";Type = Output 1;Label = "b1";InputPorts = [{Id = "942d1cb6-0a8f-43bd-cfad-db8f97bce9b4";PortNumber = Some 0;PortType = PortType.Input;HostId = "e0745d45-ab87-81ca-89a4-33629ff2c470"}];OutputPorts = [];X = 431;Y = 158}
    ],
    [
        {Id = "9979884a-3c5c-0d33-33af-057b4594daef";Source = {Id = "dfd35e38-4b9c-ad0e-4f28-3c5e9d497b0a";PortNumber = None;PortType = PortType.Output;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"};Target = {Id = "b2909f77-bbc1-4428-1bd3-f5b37ce4ef9b";PortNumber = None;PortType = PortType.Input;HostId = "44beb433-285e-8199-2b3a-d18d7bce1c6f"};Vertices = [303.0,188.0; 468.0,188.0; 468.0,192.0; 633.0,192.0]};
        {Id = "d424a273-3637-84bd-e8b1-b4cba64f19ae";Source = {Id = "c09b810b-d411-b803-5495-d95230c22ae6";PortNumber = None;PortType = PortType.Output;HostId = "5baefd71-8841-6e27-5930-ce3c4530fc4d"};Target = {Id = "782db954-6514-eafd-7829-7446feb3ca81";PortNumber = None;PortType = PortType.Input;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"};Vertices = [212.0,91.0; 232.5,91.0; 232.5,178.0; 253.0,178.0]};
        {Id = "a1d8be49-12df-11ea-e0fc-03f882516cb9";Source = {Id = "dfd35e38-4b9c-ad0e-4f28-3c5e9d497b0a";PortNumber = None;PortType = PortType.Output;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"};Target = {Id = "16f663d6-34a1-1895-938a-aba5cc88cc84";PortNumber = None;PortType = PortType.Input;HostId = "5baefd71-8841-6e27-5930-ce3c4530fc4d"};Vertices = [303.0,188.0; 323.0,188.0; 323.0,139.5; 162.0,139.5; 162.0,91.0; 182.0,91.0]};
        {Id = "90df8d13-cf14-6d13-ddf0-1b45741a3781";Source = {Id = "3875c8c1-ca03-c13a-b360-97b8eceefb36";PortNumber = None;PortType = PortType.Output;HostId = "5339d358-0ac2-f907-4b2c-ba52b1a090b6"};Target = {Id = "942d1cb6-0a8f-43bd-cfad-db8f97bce9b4";PortNumber = None;PortType = PortType.Input;HostId = "e0745d45-ab87-81ca-89a4-33629ff2c470"};Vertices = [303.0,168.0; 431.0,168.0]}
    ]

/// stateSync8 loaded as a dependency.
let stateSync8Dependency : LoadedComponent = {
    Name = "fake-combinatorial-loop"
    FilePath = ""
    CanvasState = stateSync8
    InputLabels = []
    OutputLabels = ["b1", 1; "dn-out", 1]
}

/// stateSync8 custom component.
let stateSync8CustomComponent : CustomComponentType =
    CanvasStates.makeCustomComponent stateSync8Dependency

/// StateSync8 connected to two outputs. Should fail as stateSync8 has a
/// "combinational" loop in it.
let stateSync9 : CanvasState =
    [
        {Id = "be508ddb-3f61-39f3-0f76-bbdf5380d3cb";Type = Custom stateSync8CustomComponent;Label = "doubly nested";InputPorts = [];OutputPorts = [{Id = "eda6a896-ee77-fa35-697e-10a7553266b4";PortNumber = Some 0;PortType = PortType.Output;HostId = "be508ddb-3f61-39f3-0f76-bbdf5380d3cb"}; {Id = "b6a36a53-fe71-d1aa-5a45-208cdc752e08";PortNumber = Some 1;PortType = PortType.Output;HostId = "be508ddb-3f61-39f3-0f76-bbdf5380d3cb"}];X = 298;Y = 158};
        {Id = "641641c5-f4f0-734c-d0fe-9445dc783e9d";Type = Output 1;Label = "b1";InputPorts = [{Id = "c2853aba-a183-0609-4400-e0f7085ba4c9";PortNumber = Some 0;PortType = PortType.Input;HostId = "641641c5-f4f0-734c-d0fe-9445dc783e9d"}];OutputPorts = [];X = 519;Y = 158};
        {Id = "e7fd87fc-c9a7-29b9-4cab-87eb73d45093";Type = Output 1;Label = "dn-out";InputPorts = [{Id = "5454f9bd-f6ef-6fa0-b6cf-837b048dfa13";PortNumber = Some 0;PortType = PortType.Input;HostId = "e7fd87fc-c9a7-29b9-4cab-87eb73d45093"}];OutputPorts = [];X = 519;Y = 228}
    ],
    [
        {Id = "37bd862a-b2ea-dd5c-4867-5a20f9ce4009";Source = {Id = "eda6a896-ee77-fa35-697e-10a7553266b4";PortNumber = None;PortType = PortType.Output;HostId = "be508ddb-3f61-39f3-0f76-bbdf5380d3cb"};Target = {Id = "c2853aba-a183-0609-4400-e0f7085ba4c9";PortNumber = None;PortType = PortType.Input;HostId = "641641c5-f4f0-734c-d0fe-9445dc783e9d"};Vertices = [364.0,178.0; 441.5,178.0; 441.5,168.0; 519.0,168.0]};
        {Id = "b471b89a-6ac7-7e22-c804-9a3b071f190a";Source = {Id = "b6a36a53-fe71-d1aa-5a45-208cdc752e08";PortNumber = None;PortType = PortType.Output;HostId = "be508ddb-3f61-39f3-0f76-bbdf5380d3cb"};Target = {Id = "5454f9bd-f6ef-6fa0-b6cf-837b048dfa13";PortNumber = None;PortType = PortType.Input;HostId = "e7fd87fc-c9a7-29b9-4cab-87eb73d45093"};Vertices = [364.0,198.0; 441.5,198.0; 441.5,238.0; 519.0,238.0]}
    ]

/// A fully connected DFFE.
let stateSync10 : CanvasState =
    [
        {Id = "5916f1cf-408e-4186-a839-c80926bfddf0";Type = Input 1;Label = "in";InputPorts = [];OutputPorts = [{Id = "e894fb40-804a-36d1-b2e8-6033afb61dfa";PortNumber = Some 0;PortType = PortType.Output;HostId = "5916f1cf-408e-4186-a839-c80926bfddf0"}];X = 109;Y = 95};
        {Id = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf";Type = DFFE;Label = "";InputPorts = [{Id = "c766cb8d-a762-04b9-f021-4a9175fdbc01";PortNumber = Some 0;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"}; {Id = "2b80cebe-c0af-4905-d443-fd99796ec8df";PortNumber = Some 1;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"}];OutputPorts = [{Id = "ecee9ad7-925f-c0b9-26d5-4cf8b96a9be1";PortNumber = Some 0;PortType = PortType.Output;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"}];X = 273;Y = 80};
        {Id = "cab54371-5e07-9586-eb9b-be8cc417e610";Type = Input 1;Label = "en";InputPorts = [];OutputPorts = [{Id = "4c83ef6d-7267-2732-d828-c508fff7f889";PortNumber = Some 0;PortType = PortType.Output;HostId = "cab54371-5e07-9586-eb9b-be8cc417e610"}];X = 109;Y = 270};
        {Id = "a2c874bb-eaeb-d62d-8a72-5eeae48db694";Type = Output 1;Label = "out";InputPorts = [{Id = "df5f625f-43c4-28bb-c931-eae047edaa14";PortNumber = Some 0;PortType = PortType.Input;HostId = "a2c874bb-eaeb-d62d-8a72-5eeae48db694"}];OutputPorts = [];X = 495;Y = 95}
    ],
    [
        {Id = "7ab86f0a-1b2c-7901-0a12-cbe447f1f06d";Source = {Id = "e894fb40-804a-36d1-b2e8-6033afb61dfa";PortNumber = None;PortType = PortType.Output;HostId = "5916f1cf-408e-4186-a839-c80926bfddf0"};Target = {Id = "c766cb8d-a762-04b9-f021-4a9175fdbc01";PortNumber = None;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"};Vertices = [139.0,105.0; 273.0,105.0]};
        {Id = "04bb7d64-1899-a4f3-6c50-f53ccd84cd49";Source = {Id = "ecee9ad7-925f-c0b9-26d5-4cf8b96a9be1";PortNumber = None;PortType = PortType.Output;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"};Target = {Id = "df5f625f-43c4-28bb-c931-eae047edaa14";PortNumber = None;PortType = PortType.Input;HostId = "a2c874bb-eaeb-d62d-8a72-5eeae48db694"};Vertices = [353.0,105.0; 495.0,105.0]};
        {Id = "de4f03d1-580c-eabb-7439-38b664b89aba";Source = {Id = "4c83ef6d-7267-2732-d828-c508fff7f889";PortNumber = None;PortType = PortType.Output;HostId = "cab54371-5e07-9586-eb9b-be8cc417e610"};Target = {Id = "2b80cebe-c0af-4905-d443-fd99796ec8df";PortNumber = None;PortType = PortType.Input;HostId = "4f90dab4-e43d-3c75-cd2e-27a5ef66ccaf"};Vertices = [139.0,280.938; 313.0,280.938; 313.0,130.0]}
    ]
