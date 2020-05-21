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
