module CanvasStatesMemories

open CommonTypes
open Helpers

let makeData (lst: int list) =
    lst 
    |> List.indexed
    |> List.map (fun (a,d) -> int64 a, int64 d)
    |> Map

let mem1 = {
    AddressWidth = 2
    WordWidth = 4
    Data = [0; 1; 4; 15] |> makeData
}

/// Synchronous ROM connected to address and output.
let stateMem1 : CanvasState =
    [
        {H=50;W=40; Id = "322715aa-a1d4-b314-91c7-670d33bca00a";Type = ROM mem1;Label = "";InputPorts = [{Id = "3ff66cbf-7662-f666-2823-6459005239f2";PortNumber = Some 0;PortType = PortType.Input;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"}];OutputPorts = [{Id = "81f27eec-4619-cb35-ce9a-ac7149a52da8";PortNumber = Some 0;PortType = PortType.Output;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"}];X = 252;Y = 240};
        {H=50;W=40; Id = "4f65afe9-f03c-2b97-fde9-c657ca32f246";Type = Input 2;Label = "addr";InputPorts = [];OutputPorts = [{Id = "453dbdf9-8a50-792d-c6a9-27396ef5b036";PortNumber = Some 0;PortType = PortType.Output;HostId = "4f65afe9-f03c-2b97-fde9-c657ca32f246"}];X = 88;Y = 280};
        {H=50;W=40; Id = "fbb5aa79-a471-ac24-4201-56ae39d537c6";Type = Output 4;Label = "data";InputPorts = [{Id = "d3339319-afbc-c196-6a23-475704cff3ae";PortNumber = Some 0;PortType = PortType.Input;HostId = "fbb5aa79-a471-ac24-4201-56ae39d537c6"}];OutputPorts = [];X = 467;Y = 280}
    ],
    [
        {Id = "b5675376-6921-1909-5838-b43891f88fee";Source = {Id = "453dbdf9-8a50-792d-c6a9-27396ef5b036";PortNumber = None;PortType = PortType.Output;HostId = "4f65afe9-f03c-2b97-fde9-c657ca32f246"};Target = {Id = "3ff66cbf-7662-f666-2823-6459005239f2";PortNumber = None;PortType = PortType.Input;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"};Vertices = [118.0,290.0; 252.0,290.0]};
        {Id = "4720848e-5a2e-7203-d4ac-22f9d9d74af3";Source = {Id = "81f27eec-4619-cb35-ce9a-ac7149a52da8";PortNumber = None;PortType = PortType.Output;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"};Target = {Id = "d3339319-afbc-c196-6a23-475704cff3ae";PortNumber = None;PortType = PortType.Input;HostId = "fbb5aa79-a471-ac24-4201-56ae39d537c6"};Vertices = [332.0,290.0; 467.0,290.0]}
    ]

/// Numbers from 0 to 255.
let mem2 = {
    AddressWidth = 8
    WordWidth = 8
    Data = [0..pow2(8)-1] |> makeData
}

/// Synchronous ROM connected to address and output. ROM is big.
let stateMem2 : CanvasState =
    [
        {H=50;W=40; Id = "322715aa-a1d4-b314-91c7-670d33bca00a";Type = ROM mem2;Label = "";InputPorts = [{Id = "3ff66cbf-7662-f666-2823-6459005239f2";PortNumber = Some 0;PortType = PortType.Input;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"}];OutputPorts = [{Id = "81f27eec-4619-cb35-ce9a-ac7149a52da8";PortNumber = Some 0;PortType = PortType.Output;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"}];X = 252;Y = 240};
        {H=50;W=40; Id = "4f65afe9-f03c-2b97-fde9-c657ca32f246";Type = Input 8;Label = "addr";InputPorts = [];OutputPorts = [{Id = "453dbdf9-8a50-792d-c6a9-27396ef5b036";PortNumber = Some 0;PortType = PortType.Output;HostId = "4f65afe9-f03c-2b97-fde9-c657ca32f246"}];X = 88;Y = 280};
        {H=50;W=40; Id = "fbb5aa79-a471-ac24-4201-56ae39d537c6";Type = Output 8;Label = "data";InputPorts = [{Id = "d3339319-afbc-c196-6a23-475704cff3ae";PortNumber = Some 0;PortType = PortType.Input;HostId = "fbb5aa79-a471-ac24-4201-56ae39d537c6"}];OutputPorts = [];X = 467;Y = 280}
    ],
    [
        {Id = "b5675376-6921-1909-5838-b43891f88fee";Source = {Id = "453dbdf9-8a50-792d-c6a9-27396ef5b036";PortNumber = None;PortType = PortType.Output;HostId = "4f65afe9-f03c-2b97-fde9-c657ca32f246"};Target = {Id = "3ff66cbf-7662-f666-2823-6459005239f2";PortNumber = None;PortType = PortType.Input;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"};Vertices = [118.0,290.0; 252.0,290.0]};
        {Id = "4720848e-5a2e-7203-d4ac-22f9d9d74af3";Source = {Id = "81f27eec-4619-cb35-ce9a-ac7149a52da8";PortNumber = None;PortType = PortType.Output;HostId = "322715aa-a1d4-b314-91c7-670d33bca00a"};Target = {Id = "d3339319-afbc-c196-6a23-475704cff3ae";PortNumber = None;PortType = PortType.Input;HostId = "fbb5aa79-a471-ac24-4201-56ae39d537c6"};Vertices = [332.0,290.0; 467.0,290.0]}
    ]

let mem3 = {
    AddressWidth = 3
    WordWidth = 4
    Data = [0; 0; 0; 0; 0; 0; 0; 0] |> makeData
}

/// Fully connected RAM.
let stateMem3 : CanvasState =
    [
        {H=50;W=40; Id = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60";Type = RAM mem3;Label = "";InputPorts = [{Id = "83ff4d8a-a28e-ebc8-8311-77def91ca935";PortNumber = Some 0;PortType = PortType.Input;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"}; {Id = "0c116a9b-78cd-79a5-c080-dfdc1445334d";PortNumber = Some 1;PortType = PortType.Input;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"}; {Id = "86e5f7cf-657c-9383-54b3-65c156fdf867";PortNumber = Some 2;PortType = PortType.Input;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"}];OutputPorts = [{Id = "c9e983ab-b832-156a-36fd-9ce1d03810d7";PortNumber = Some 0;PortType = PortType.Output;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"}];X = 318;Y = 190};
        {H=50;W=40; Id = "1f9704f9-88fc-124c-3ff8-21c36cfb7328";Type = Output 4;Label = "data-out";InputPorts = [{Id = "0af79035-101f-b683-f39c-68a36a7dc33f";PortNumber = Some 0;PortType = PortType.Input;HostId = "1f9704f9-88fc-124c-3ff8-21c36cfb7328"}];OutputPorts = [];X = 647;Y = 230};
        {H=50;W=40; Id = "266d8763-6fbf-37c2-6825-d3487153053b";Type = Input 4;Label = "data-in";InputPorts = [];OutputPorts = [{Id = "b155889c-3d6d-8ebf-2b3b-771f1ada3578";PortNumber = Some 0;PortType = PortType.Output;HostId = "266d8763-6fbf-37c2-6825-d3487153053b"}];X = 116;Y = 230};
        {H=50;W=40; Id = "81030fc6-2471-a568-160f-922709edeb2e";Type = Input 3;Label = "addr";InputPorts = [];OutputPorts = [{Id = "ceb4382f-d670-912f-87c2-755315a89c87";PortNumber = Some 0;PortType = PortType.Output;HostId = "81030fc6-2471-a568-160f-922709edeb2e"}];X = 116;Y = 138};
        {H=50;W=40; Id = "f0769200-24e3-5c7b-3591-c5c3711d9336";Type = Input 1;Label = "write";InputPorts = [];OutputPorts = [{Id = "b8286beb-9cc1-1128-cc1e-813388ca16fc";PortNumber = Some 0;PortType = PortType.Output;HostId = "f0769200-24e3-5c7b-3591-c5c3711d9336"}];X = 116;Y = 322}
    ],
    [
        {Id = "04957b14-81b7-83c0-7669-c9c6fc363e00";Source = {Id = "b155889c-3d6d-8ebf-2b3b-771f1ada3578";PortNumber = None;PortType = PortType.Output;HostId = "266d8763-6fbf-37c2-6825-d3487153053b"};Target = {Id = "0c116a9b-78cd-79a5-c080-dfdc1445334d";PortNumber = None;PortType = PortType.Input;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"};Vertices = [146.0,240.0; 318.0,240.0]};
        {Id = "d24c03fd-7b01-e68f-0d6f-8ff153791809";Source = {Id = "c9e983ab-b832-156a-36fd-9ce1d03810d7";PortNumber = None;PortType = PortType.Output;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"};Target = {Id = "0af79035-101f-b683-f39c-68a36a7dc33f";PortNumber = None;PortType = PortType.Input;HostId = "1f9704f9-88fc-124c-3ff8-21c36cfb7328"};Vertices = [448.0,240.0; 647.0,240.0]};
        {Id = "63851c98-6945-0718-29c9-94f738246fd3";Source = {Id = "b8286beb-9cc1-1128-cc1e-813388ca16fc";PortNumber = None;PortType = PortType.Output;HostId = "f0769200-24e3-5c7b-3591-c5c3711d9336"};Target = {Id = "86e5f7cf-657c-9383-54b3-65c156fdf867";PortNumber = None;PortType = PortType.Input;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"};Vertices = [146.0,332.0; 232.0,332.0; 232.0,265.0; 318.0,265.0]};
        {Id = "69b302c0-11e5-bb4d-291c-077318321d91";Source = {Id = "ceb4382f-d670-912f-87c2-755315a89c87";PortNumber = None;PortType = PortType.Output;HostId = "81030fc6-2471-a568-160f-922709edeb2e"};Target = {Id = "83ff4d8a-a28e-ebc8-8311-77def91ca935";PortNumber = None;PortType = PortType.Input;HostId = "33d42057-a42d-3fc5-5b6c-a9f31e18bb60"};Vertices = [146.0,148.0; 232.0,148.0; 232.0,215.0; 318.0,215.0]}
    ]
