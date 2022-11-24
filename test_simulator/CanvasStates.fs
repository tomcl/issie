module CanvasStates

open CommonTypes
open SimulatorTypes

let registor: CanvasState =
    ([ { Id = "4d634a9c-8b0a-4a97-ab3e-2041566c3096"
         Type = Register 1
         Label = "REG1"
         InputPorts =
           [ { Id = "6fe79c06-0646-4308-a005-9e342010d535"
               PortNumber = Some 0
               PortType = PortType.Input
               HostId = "4d634a9c-8b0a-4a97-ab3e-2041566c3096" } ]
         OutputPorts =
           [ { Id = "dffdca0c-d8d4-4e30-b365-ed7db5dfbf00"
               PortNumber = Some 0
               PortType = PortType.Output
               HostId = "4d634a9c-8b0a-4a97-ab3e-2041566c3096" } ]
         X = 1700.25
         Y = 1735
         H = 60
         W = 120
         SymbolInfo =
           Some
               { LabelBoundingBox = None
                 LabelRotation = None
                 STransform = { Rotation = Degree0; flipped = false }
                 ReversedInputPorts = Some false
                 PortOrientation =
                   Map
                       [ ("6fe79c06-0646-4308-a005-9e342010d535", Left)
                         ("dffdca0c-d8d4-4e30-b365-ed7db5dfbf00", Right) ]
                 PortOrder =
                   Map
                       [ (Top, [])
                         (Bottom, [])
                         (Left, [ "6fe79c06-0646-4308-a005-9e342010d535" ])
                         (Right, [ "dffdca0c-d8d4-4e30-b365-ed7db5dfbf00" ]) ]
                 HScale = None
                 VScale = None } }
       { Id = "6458692b-d240-4168-b60f-b7c623f5eba4"
         Type = Output 1
         Label = "B"
         InputPorts =
           [ { Id = "0e6ab83b-1a81-4656-9c9f-558030ae7936"
               PortNumber = Some 0
               PortType = PortType.Input
               HostId = "6458692b-d240-4168-b60f-b7c623f5eba4" } ]
         OutputPorts = []
         X = 1902.75
         Y = 1750
         H = 30
         W = 60
         SymbolInfo =
           Some
               { LabelBoundingBox = None
                 LabelRotation = None
                 STransform = { Rotation = Degree0; flipped = false }
                 ReversedInputPorts = Some false
                 PortOrientation = Map [ ("0e6ab83b-1a81-4656-9c9f-558030ae7936", Left) ]
                 PortOrder =
                   Map
                       [ (Top, [])
                         (Bottom, [])
                         (Left, [ "0e6ab83b-1a81-4656-9c9f-558030ae7936" ])
                         (Right, []) ]
                 HScale = None
                 VScale = None } }
       { Id = "a4f8b9a2-0fc9-4844-8552-5568babe236d"
         Type = Input1(1, Some 0)
         Label = "A"
         InputPorts = []
         OutputPorts =
           [ { Id = "e6f9257b-c749-47ca-8481-0c9ffd71f8de"
               PortNumber = Some 0
               PortType = PortType.Output
               HostId = "a4f8b9a2-0fc9-4844-8552-5568babe236d" } ]
         X = 1537.25
         Y = 1750
         H = 30
         W = 60
         SymbolInfo =
           Some
               { LabelBoundingBox = None
                 LabelRotation = None
                 STransform = { Rotation = Degree0; flipped = false }
                 ReversedInputPorts = Some false
                 PortOrientation = Map [ ("e6f9257b-c749-47ca-8481-0c9ffd71f8de", Right) ]
                 PortOrder =
                   Map
                       [ (Top, [])
                         (Bottom, [])
                         (Left, [])
                         (Right, [ "e6f9257b-c749-47ca-8481-0c9ffd71f8de" ]) ]
                 HScale = None
                 VScale = None } } ],
     [ { Id = "056c4806-a2e1-4061-a75f-95c064ea9531"
         Source =
           { Id = "e6f9257b-c749-47ca-8481-0c9ffd71f8de"
             PortNumber = None
             PortType = PortType.Output
             HostId = "a4f8b9a2-0fc9-4844-8552-5568babe236d" }
         Target =
           { Id = "6fe79c06-0646-4308-a005-9e342010d535"
             PortNumber = None
             PortType = PortType.Input
             HostId = "4d634a9c-8b0a-4a97-ab3e-2041566c3096" }
         Vertices =
           [ 1597.25, 1765, false
             1605.25, 1765, false
             1605.25, 1765, false
             1648.75, 1765, false
             1648.75, 1765, false
             1692.25, 1765, false
             1692.25, 1765, false
             1700.25, 1765, false ] }
       { Id = "6fec660b-c1e6-4da1-89f1-ed9129e4f89f"
         Source =
           { Id = "dffdca0c-d8d4-4e30-b365-ed7db5dfbf00"
             PortNumber = None
             PortType = PortType.Output
             HostId = "4d634a9c-8b0a-4a97-ab3e-2041566c3096" }
         Target =
           { Id = "0e6ab83b-1a81-4656-9c9f-558030ae7936"
             PortNumber = None
             PortType = PortType.Input
             HostId = "6458692b-d240-4168-b60f-b7c623f5eba4" }
         Vertices =
           [ 1820.25, 1765, false
             1828.25, 1765, false
             1828.25, 1765, false
             1861.5, 1765, false
             1861.5, 1765, false
             1894.75, 1765, false
             1894.75, 1765, false
             1902.75, 1765, false ] } ])
