module CanvasStatesWithBuses

open DiagramTypes
open CanvasStates

/// Two inputs connected to a MakeBus2 component. No other connections.
let stateBus1 : CanvasState =
    [
        {Id = "08de9671-756c-44e0-905c-cde5b9a98aa9";Type = Input;Label = "a";InputPorts = [];OutputPorts = [{Id = "528c3ccc-9554-6a5b-129f-c88e55478ae2";PortNumber = Some 0;PortType = PortType.Output;HostId = "08de9671-756c-44e0-905c-cde5b9a98aa9"}];X = 100;Y = 100};
        {Id = "0b7b0ae5-dcca-f0af-3ba6-68231fdf80fc";Type = Input;Label = "b";InputPorts = [];OutputPorts = [{Id = "9de10edf-96be-beb0-2926-45f98b5dccb2";PortNumber = Some 0;PortType = PortType.Output;HostId = "0b7b0ae5-dcca-f0af-3ba6-68231fdf80fc"}];X = 100;Y = 169};
        {Id = "8caa65e2-97eb-ed9c-cd34-220b1fde3add";Type = MakeBus2;Label = "";InputPorts = [{Id = "6d525499-76f1-001b-3cca-726c4aa9b2ee";PortNumber = Some 0;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}; {Id = "0d6f9b2f-d511-c08b-3b72-4c23975adeda";PortNumber = Some 1;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];OutputPorts = [{Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = Some 0;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];X = 230;Y = 138}
    ], 
    [    
        {Id = "conn0";Source = {Id = "528c3ccc-9554-6a5b-129f-c88e55478ae2";PortNumber = None;PortType = PortType.Output;HostId = "08de9671-756c-44e0-905c-cde5b9a98aa9"};Target = {Id = "6d525499-76f1-001b-3cca-726c4aa9b2ee";PortNumber = None;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Vertices = [130.0,110.0; 180.0,110.0; 180.0,138.0; 230.0,138.0]};
        {Id = "conn1";Source = {Id = "9de10edf-96be-beb0-2926-45f98b5dccb2";PortNumber = None;PortType = PortType.Output;HostId = "0b7b0ae5-dcca-f0af-3ba6-68231fdf80fc"};Target = {Id = "0d6f9b2f-d511-c08b-3b72-4c23975adeda";PortNumber = None;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Vertices = [130.0,179.0; 180.0,179.0; 180.0,158.0; 230.0,158.0]}
    ]

/// A MakeBus2 connected to a SplitBus2.
let stateBus2 : CanvasState =
    [
        {Id = "8caa65e2-97eb-ed9c-cd34-220b1fde3add";Type = MakeBus2;Label = "";InputPorts = [{Id = "6d525499-76f1-001b-3cca-726c4aa9b2ee";PortNumber = Some 0;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}; {Id = "0d6f9b2f-d511-c08b-3b72-4c23975adeda";PortNumber = Some 1;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];OutputPorts = [{Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = Some 0;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];X = 230;Y = 138};
        {Id = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2";Type = SplitBus2;Label = "";InputPorts = [{Id = "ba539631-cbee-1c40-e2ad-755e3c4893ab";PortNumber = Some 0;PortType = PortType.Input;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}];OutputPorts = [{Id = "9f93cb8f-ad50-a556-57fb-3f1ec4cbdaba";PortNumber = Some 0;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}; {Id = "58dc5768-a64d-e6d6-36cc-fecdd57b57d7";PortNumber = Some 1;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}];X = 336;Y = 138}
    ],
    [
        {Id = "conn0";Source = {Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = None;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Target = {Id = "ba539631-cbee-1c40-e2ad-755e3c4893ab";PortNumber = None;PortType = PortType.Input;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"};Vertices = [270.0,148.0; 336.0,148.0]}
    ]

/// A MakeBus2 connected to a SplitBus2 and a single-bit output node.
let stateBus3 : CanvasState =
    [
        {Id = "8caa65e2-97eb-ed9c-cd34-220b1fde3add";Type = MakeBus2;Label = "";InputPorts = [{Id = "6d525499-76f1-001b-3cca-726c4aa9b2ee";PortNumber = Some 0;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}; {Id = "0d6f9b2f-d511-c08b-3b72-4c23975adeda";PortNumber = Some 1;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];OutputPorts = [{Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = Some 0;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];X = 230;Y = 138};
        {Id = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2";Type = SplitBus2;Label = "";InputPorts = [{Id = "ba539631-cbee-1c40-e2ad-755e3c4893ab";PortNumber = Some 0;PortType = PortType.Input;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}];OutputPorts = [{Id = "9f93cb8f-ad50-a556-57fb-3f1ec4cbdaba";PortNumber = Some 0;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}; {Id = "58dc5768-a64d-e6d6-36cc-fecdd57b57d7";PortNumber = Some 1;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}];X = 336;Y = 138}
        {Id = "7af6a475-990d-015f-e9e0-4a680cc84173";Type = Output;Label = "a";InputPorts = [{Id = "cefbb411-481e-d04d-c5a0-02f3921e423f";PortNumber = Some 0;PortType = PortType.Input;HostId = "7af6a475-990d-015f-e9e0-4a680cc84173"}];OutputPorts = [];X = 346;Y = 63}
    ],
    [
        {Id = "conn0";Source = {Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = None;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Target = {Id = "ba539631-cbee-1c40-e2ad-755e3c4893ab";PortNumber = None;PortType = PortType.Input;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"};Vertices = [270.0,148.0; 336.0,148.0]}
        {Id = "conn1";Source = {Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = None;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Target = {Id = "cefbb411-481e-d04d-c5a0-02f3921e423f";PortNumber = None;PortType = PortType.Input;HostId = "7af6a475-990d-015f-e9e0-4a680cc84173"};Vertices = [270.0,148.0; 308.0,148.0; 308.0,73.0; 346.0,73.0]}
    ]

/// Like stateBus2 but with a loop.
let stateBus4 : CanvasState =
    [
        {Id = "8caa65e2-97eb-ed9c-cd34-220b1fde3add";Type = MakeBus2;Label = "";InputPorts = [{Id = "6d525499-76f1-001b-3cca-726c4aa9b2ee";PortNumber = Some 0;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}; {Id = "0d6f9b2f-d511-c08b-3b72-4c23975adeda";PortNumber = Some 1;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];OutputPorts = [{Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = Some 0;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"}];X = 230;Y = 138};
        {Id = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2";Type = SplitBus2;Label = "";InputPorts = [{Id = "ba539631-cbee-1c40-e2ad-755e3c4893ab";PortNumber = Some 0;PortType = PortType.Input;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}];OutputPorts = [{Id = "9f93cb8f-ad50-a556-57fb-3f1ec4cbdaba";PortNumber = Some 0;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}; {Id = "58dc5768-a64d-e6d6-36cc-fecdd57b57d7";PortNumber = Some 1;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"}];X = 336;Y = 138}
    ],
    [
        {Id = "conn0";Source = {Id = "f8a73708-ed54-cc87-d4d5-0a72745e82c7";PortNumber = None;PortType = PortType.Output;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Target = {Id = "ba539631-cbee-1c40-e2ad-755e3c4893ab";PortNumber = None;PortType = PortType.Input;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"};Vertices = [270.0,148.0; 336.0,148.0]}
        {Id = "conn1";Source = {Id = "9f93cb8f-ad50-a556-57fb-3f1ec4cbdaba";PortNumber = None;PortType = PortType.Output;HostId = "6060cff2-0e4f-d3ac-a8f2-40557eca62b2"};Target = {Id = "6d525499-76f1-001b-3cca-726c4aa9b2ee";PortNumber = None;PortType = PortType.Input;HostId = "8caa65e2-97eb-ed9c-cd34-220b1fde3add"};Vertices = [376.0,138.0; 396.0,138.0; 396.0,138.0; 210.0,138.0; 210.0,138.0; 230.0,138.0]}
    ]

/// Two inputs connected to a PushToBusFirst component. No other connections.
let stateBus5 : CanvasState =
    [
        {Id = "08de9671-756c-44e0-905c-cde5b9a98aa9";Type = Input;Label = "a";InputPorts = [];OutputPorts = [{Id = "528c3ccc-9554-6a5b-129f-c88e55478ae2";PortNumber = Some 0;PortType = PortType.Output;HostId = "08de9671-756c-44e0-905c-cde5b9a98aa9"}];X = 100;Y = 100};
        {Id = "06fdc230-aa76-c8be-f5f8-acc445b80945";Type = PushToBusFirst;Label = "";InputPorts = [{Id = "d07af1bd-ff97-4fd2-16ca-b51521c6c032";PortNumber = Some 0;PortType = PortType.Input;HostId = "06fdc230-aa76-c8be-f5f8-acc445b80945"}; {Id = "c1671202-233c-30a4-2d3a-026476d5be43";PortNumber = Some 1;PortType = PortType.Input;HostId = "06fdc230-aa76-c8be-f5f8-acc445b80945"}];OutputPorts = [{Id = "7f247eed-a686-cab9-2e93-72b4ba8b7311";PortNumber = Some 0;PortType = PortType.Output;HostId = "06fdc230-aa76-c8be-f5f8-acc445b80945"}];X = 282;Y = 127};
        {Id = "0b7b0ae5-dcca-f0af-3ba6-68231fdf80fc";Type = Input;Label = "b";InputPorts = [];OutputPorts = [{Id = "9de10edf-96be-beb0-2926-45f98b5dccb2";PortNumber = Some 0;PortType = PortType.Output;HostId = "0b7b0ae5-dcca-f0af-3ba6-68231fdf80fc"}];X = 100;Y = 169}
    ],
    [
        {Id = "conn0";Source = {Id = "528c3ccc-9554-6a5b-129f-c88e55478ae2";PortNumber = None;PortType = PortType.Output;HostId = "08de9671-756c-44e0-905c-cde5b9a98aa9"};Target = {Id = "d07af1bd-ff97-4fd2-16ca-b51521c6c032";PortNumber = None;PortType = PortType.Input;HostId = "06fdc230-aa76-c8be-f5f8-acc445b80945"};Vertices = [130.0,110.0; 206.0,110.0; 206.0,127.0; 282.0,127.0]}
        {Id = "conn1";Source = {Id = "9de10edf-96be-beb0-2926-45f98b5dccb2";PortNumber = None;PortType = PortType.Output;HostId = "0b7b0ae5-dcca-f0af-3ba6-68231fdf80fc"};Target = {Id = "c1671202-233c-30a4-2d3a-026476d5be43";PortNumber = None;PortType = PortType.Input;HostId = "06fdc230-aa76-c8be-f5f8-acc445b80945"};Vertices = [130.0,179.0; 206.0,179.0; 206.0,147.0; 282.0,147.0]};
    ]

/// All the bus components in series, properly connected. No other components.
let stateBus6 : CanvasState =
    [
        {Id = "52a4b421-c919-6177-c66c-ec2a77379373";Type = SplitBus2;Label = "";InputPorts = [{Id = "07e65ee3-58a7-a5e0-724a-0f56f2e528c5";PortNumber = Some 0;PortType = PortType.Input;HostId = "52a4b421-c919-6177-c66c-ec2a77379373"}];OutputPorts = [{Id = "8e27936c-75e6-5ad1-777e-6aa111b2d584";PortNumber = Some 0;PortType = PortType.Output;HostId = "52a4b421-c919-6177-c66c-ec2a77379373"}; {Id = "1385e5ca-c8e7-b209-402f-b85c42023bdb";PortNumber = Some 1;PortType = PortType.Output;HostId = "52a4b421-c919-6177-c66c-ec2a77379373"}];X = 614;Y = 258};
        {Id = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d";Type = PushToBusFirst;Label = "";InputPorts = [{Id = "94d62be4-60ae-4e5f-add7-3715341617aa";PortNumber = Some 0;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}; {Id = "c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce";PortNumber = Some 1;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}];OutputPorts = [{Id = "cabe29d9-74ea-06b9-8421-8547090727f9";PortNumber = Some 0;PortType = PortType.Output;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}];X = 241;Y = 248};
        {Id = "66dd284a-9199-fda1-867b-4e6b837a7ae5";Type = MakeBus2;Label = "";InputPorts = [{Id = "2ee75313-e4e3-4ab8-9e36-a5089e49278d";PortNumber = Some 0;PortType = PortType.Input;HostId = "66dd284a-9199-fda1-867b-4e6b837a7ae5"}; {Id = "a5d69516-6a23-a363-e86f-2fe2a062e722";PortNumber = Some 1;PortType = PortType.Input;HostId = "66dd284a-9199-fda1-867b-4e6b837a7ae5"}];OutputPorts = [{Id = "caf8086a-b657-bc0a-5684-a25556b95845";PortNumber = Some 0;PortType = PortType.Output;HostId = "66dd284a-9199-fda1-867b-4e6b837a7ae5"}];X = 153;Y = 258};
        {Id = "02e33421-bea8-657b-1e17-6e0df31696d5";Type = PopFirstFromBus;Label = "";InputPorts = [{Id = "ed9727f2-373b-9f23-9293-dacea5a5476b";PortNumber = Some 0;PortType = PortType.Input;HostId = "02e33421-bea8-657b-1e17-6e0df31696d5"}];OutputPorts = [{Id = "7beda885-b4f4-f5be-ada5-476643c2cec3";PortNumber = Some 0;PortType = PortType.Output;HostId = "02e33421-bea8-657b-1e17-6e0df31696d5"}; {Id = "853f0e05-b301-db44-d437-d50abef0065b";PortNumber = Some 1;PortType = PortType.Output;HostId = "02e33421-bea8-657b-1e17-6e0df31696d5"}];X = 531;Y = 248};
        {Id = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67";Type = PushToBusLast;Label = "";InputPorts = [{Id = "b87776d8-4588-4dc1-3251-342683ee4ba8";PortNumber = Some 0;PortType = PortType.Input;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"}; {Id = "b26122bb-0ce1-8e3e-1021-a9aec7c4c0ca";PortNumber = Some 1;PortType = PortType.Input;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"}];OutputPorts = [{Id = "211fd204-86fe-e959-b4fa-129d1b4accb4";PortNumber = Some 0;PortType = PortType.Output;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"}];X = 329;Y = 258};
        {Id = "a7db69d3-5a34-e5d2-46d1-19fe8879b625";Type = PopLastFromBus;Label = "";InputPorts = [{Id = "075ab754-bff4-ce21-0384-1f46f1ae986a";PortNumber = Some 0;PortType = PortType.Input;HostId = "a7db69d3-5a34-e5d2-46d1-19fe8879b625"}];OutputPorts = [{Id = "673c4ac0-9c68-6f3c-dfd4-6b129b956357";PortNumber = Some 0;PortType = PortType.Output;HostId = "a7db69d3-5a34-e5d2-46d1-19fe8879b625"}; {Id = "3a314510-a373-0c2c-50c1-ac3ce9d5f4f7";PortNumber = Some 1;PortType = PortType.Output;HostId = "a7db69d3-5a34-e5d2-46d1-19fe8879b625"}];X = 438;Y = 258}
    ],
    [
        {Id = "conn0";Source = {Id = "caf8086a-b657-bc0a-5684-a25556b95845";PortNumber = None;PortType = PortType.Output;HostId = "66dd284a-9199-fda1-867b-4e6b837a7ae5"};Target = {Id = "c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce";PortNumber = None;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"};Vertices = [193.0,268.0; 230.0,268.0; 230.0,268.0; 241.0,268.0]};
        {Id = "conn1";Source = {Id = "cabe29d9-74ea-06b9-8421-8547090727f9";PortNumber = None;PortType = PortType.Output;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"};Target = {Id = "b87776d8-4588-4dc1-3251-342683ee4ba8";PortNumber = None;PortType = PortType.Input;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"};Vertices = [281.0,258.0; 329.0,258.0]}
        {Id = "conn2";Source = {Id = "211fd204-86fe-e959-b4fa-129d1b4accb4";PortNumber = None;PortType = PortType.Output;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"};Target = {Id = "075ab754-bff4-ce21-0384-1f46f1ae986a";PortNumber = None;PortType = PortType.Input;HostId = "a7db69d3-5a34-e5d2-46d1-19fe8879b625"};Vertices = [369.0,268.0; 438.0,268.0]};
        {Id = "conn3";Source = {Id = "673c4ac0-9c68-6f3c-dfd4-6b129b956357";PortNumber = None;PortType = PortType.Output;HostId = "a7db69d3-5a34-e5d2-46d1-19fe8879b625"};Target = {Id = "ed9727f2-373b-9f23-9293-dacea5a5476b";PortNumber = None;PortType = PortType.Input;HostId = "02e33421-bea8-657b-1e17-6e0df31696d5"};Vertices = [478.0,258.0; 531.0,258.0]};
        {Id = "conn4";Source = {Id = "853f0e05-b301-db44-d437-d50abef0065b";PortNumber = None;PortType = PortType.Output;HostId = "02e33421-bea8-657b-1e17-6e0df31696d5"};Target = {Id = "07e65ee3-58a7-a5e0-724a-0f56f2e528c5";PortNumber = None;PortType = PortType.Input;HostId = "52a4b421-c919-6177-c66c-ec2a77379373"};Vertices = [571.0,268.0; 614.0,268.0]};
    ]

/// Non-inferrable loop: PushToBusFirst connected to PushToBusLast and loop back.
let stateBus7 : CanvasState =
    [
        {Id = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d";Type = PushToBusFirst;Label = "";InputPorts = [{Id = "94d62be4-60ae-4e5f-add7-3715341617aa";PortNumber = Some 0;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}; {Id = "c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce";PortNumber = Some 1;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}];OutputPorts = [{Id = "cabe29d9-74ea-06b9-8421-8547090727f9";PortNumber = Some 0;PortType = PortType.Output;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}];X = 241;Y = 248};
        {Id = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67";Type = PushToBusLast;Label = "";InputPorts = [{Id = "b87776d8-4588-4dc1-3251-342683ee4ba8";PortNumber = Some 0;PortType = PortType.Input;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"}; {Id = "b26122bb-0ce1-8e3e-1021-a9aec7c4c0ca";PortNumber = Some 1;PortType = PortType.Input;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"}];OutputPorts = [{Id = "211fd204-86fe-e959-b4fa-129d1b4accb4";PortNumber = Some 0;PortType = PortType.Output;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"}];X = 325;Y = 258}
    ],
    [
        {Id = "conn0";Source = {Id = "211fd204-86fe-e959-b4fa-129d1b4accb4";PortNumber = None;PortType = PortType.Output;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"};Target = {Id = "c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce";PortNumber = None;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"};Vertices = [365.0,268.0; 385.0,268.0; 385.0,307.0; 221.0,307.0; 221.0,268.0; 241.0,268.0]};
        {Id = "conn1";Source = {Id = "cabe29d9-74ea-06b9-8421-8547090727f9";PortNumber = None;PortType = PortType.Output;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"};Target = {Id = "b87776d8-4588-4dc1-3251-342683ee4ba8";PortNumber = None;PortType = PortType.Input;HostId = "eb13d9a8-162b-3ffc-a1d8-e6666edf7a67"};Vertices = [281.0,258.0; 325.0,258.0]}
    ]

/// Mux connected to two PushBusFirst. Width not inferrable.
let stateBus8 : CanvasState =
    [
        {Id = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d";Type = PushToBusFirst;Label = "";InputPorts = [{Id = "94d62be4-60ae-4e5f-add7-3715341617aa";PortNumber = Some 0;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}; {Id = "c8e1b3ee-d1c7-e576-6e8e-eca93bb33fce";PortNumber = Some 1;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}];OutputPorts = [{Id = "cabe29d9-74ea-06b9-8421-8547090727f9";PortNumber = Some 0;PortType = PortType.Output;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"}];X = 241;Y = 248};
        {Id = "3875f58b-8744-291d-1f4f-bca5b1a1eda5";Type = Mux2;Label = "mux2";InputPorts = [{Id = "6758d0e5-2618-afe4-473b-43c5002abf74";PortNumber = Some 0;PortType = PortType.Input;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}; {Id = "401a0856-683d-6ed2-c193-53d1761ad7a4";PortNumber = Some 1;PortType = PortType.Input;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}; {Id = "cf514335-b50d-eb19-c6f5-18303cda0b13";PortNumber = Some 2;PortType = PortType.Input;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}];OutputPorts = [{Id = "6bf59da3-ea8a-30c3-163d-37dbefd821bc";PortNumber = Some 0;PortType = PortType.Output;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}];X = 145;Y = 190};
        {Id = "536f5ede-5b98-ce3f-db3e-e96de247a89b";Type = PushToBusFirst;Label = "";InputPorts = [{Id = "b871e19d-2bec-0cc1-8c71-0fbee278bf51";PortNumber = Some 0;PortType = PortType.Input;HostId = "536f5ede-5b98-ce3f-db3e-e96de247a89b"}; {Id = "69b5caec-1da0-9ecb-d7fc-fbd9af66fc23";PortNumber = Some 1;PortType = PortType.Input;HostId = "536f5ede-5b98-ce3f-db3e-e96de247a89b"}];OutputPorts = [{Id = "6b2d64d3-7697-a97f-ed51-f0991d77b6b9";PortNumber = Some 0;PortType = PortType.Output;HostId = "536f5ede-5b98-ce3f-db3e-e96de247a89b"}];X = 336;Y = 237}
    ],
    [
        {Id = "conn0";Source = {Id = "6bf59da3-ea8a-30c3-163d-37dbefd821bc";PortNumber = None;PortType = PortType.Output;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"};Target = {Id = "94d62be4-60ae-4e5f-add7-3715341617aa";PortNumber = None;PortType = PortType.Input;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"};Vertices = [175.0,215.0; 208.0,215.0; 208.0,248.0; 241.0,248.0]};
        {Id = "conn1";Source = {Id = "cabe29d9-74ea-06b9-8421-8547090727f9";PortNumber = None;PortType = PortType.Output;HostId = "37dd0853-0d7e-ab38-82e7-4e5d6d43ee9d"};Target = {Id = "69b5caec-1da0-9ecb-d7fc-fbd9af66fc23";PortNumber = None;PortType = PortType.Input;HostId = "536f5ede-5b98-ce3f-db3e-e96de247a89b"};Vertices = [281.0,258.0; 308.5,258.0; 308.5,257.0; 336.0,257.0]}
    ]

/// Mux connected to a splitBus2.
let stateBus9 : CanvasState =
    [
        {Id = "3875f58b-8744-291d-1f4f-bca5b1a1eda5";Type = Mux2;Label = "mux2";InputPorts = [{Id = "6758d0e5-2618-afe4-473b-43c5002abf74";PortNumber = Some 0;PortType = PortType.Input;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}; {Id = "401a0856-683d-6ed2-c193-53d1761ad7a4";PortNumber = Some 1;PortType = PortType.Input;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}; {Id = "cf514335-b50d-eb19-c6f5-18303cda0b13";PortNumber = Some 2;PortType = PortType.Input;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}];OutputPorts = [{Id = "6bf59da3-ea8a-30c3-163d-37dbefd821bc";PortNumber = Some 0;PortType = PortType.Output;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"}];X = 145;Y = 190};
        {Id = "772307ab-2941-3343-28a2-faacaa0efc96";Type = SplitBus2;Label = "";InputPorts = [{Id = "2332109c-b48a-be65-70fd-662fc00cd0c7";PortNumber = Some 0;PortType = PortType.Input;HostId = "772307ab-2941-3343-28a2-faacaa0efc96"}];OutputPorts = [{Id = "6df8212b-0639-3d53-61cd-297e74d6bbad";PortNumber = Some 0;PortType = PortType.Output;HostId = "772307ab-2941-3343-28a2-faacaa0efc96"}; {Id = "5a95b665-510e-848c-c178-e0356b48940a";PortNumber = Some 1;PortType = PortType.Output;HostId = "772307ab-2941-3343-28a2-faacaa0efc96"}];X = 262;Y = 205}
    ],
    [
        {Id = "conn0";Source = {Id = "6bf59da3-ea8a-30c3-163d-37dbefd821bc";PortNumber = None;PortType = PortType.Output;HostId = "3875f58b-8744-291d-1f4f-bca5b1a1eda5"};Target = {Id = "2332109c-b48a-be65-70fd-662fc00cd0c7";PortNumber = None;PortType = PortType.Input;HostId = "772307ab-2941-3343-28a2-faacaa0efc96"};Vertices = [175.0,215.0; 262.0,215.0]}
    ]
