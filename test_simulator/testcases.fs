module TestCases

open CommonTypes
open SimulatorTypes

let canvasStates: (CanvasState * string) list =
    [ (([ { Id = "5965a50b-0a0f-4ae6-8853-fee429a55d63"
            Type = NbitsAnd(128)
            Label = "AND1"
            InputPorts =
              [ { Id = "9ff0b77b-3a4b-4a0a-9316-80e344cd7a23"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "5965a50b-0a0f-4ae6-8853-fee429a55d63" }
                { Id = "8cff242e-2750-4190-90f2-ecc451a139fa"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "5965a50b-0a0f-4ae6-8853-fee429a55d63" } ]
            OutputPorts =
              [ { Id = "76c4ead7-c406-42a7-b48c-6778c40afe99"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "5965a50b-0a0f-4ae6-8853-fee429a55d63" } ]
            X = 1688.5
            Y = 1700
            H = 120
            W = 120
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("76c4ead7-c406-42a7-b48c-6778c40afe99", Right)
                            ("8cff242e-2750-4190-90f2-ecc451a139fa", Left)
                            ("9ff0b77b-3a4b-4a0a-9316-80e344cd7a23", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "9ff0b77b-3a4b-4a0a-9316-80e344cd7a23"
                               "8cff242e-2750-4190-90f2-ecc451a139fa" ])
                            (Right, [ "76c4ead7-c406-42a7-b48c-6778c40afe99" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "77e204ad-dc00-4fa5-9f29-a3cb1059976d"
            Type = Input1(128, Some 0)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "ca4befb9-0e15-4266-901d-4876f64e505a"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "77e204ad-dc00-4fa5-9f29-a3cb1059976d" } ]
            X = 1520.5
            Y = 1720
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("ca4befb9-0e15-4266-901d-4876f64e505a", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "ca4befb9-0e15-4266-901d-4876f64e505a" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "919e1991-ce1b-456f-9e0e-324297f71a49"
            Type = Output(128)
            Label = "C"
            InputPorts =
              [ { Id = "89fefc98-97b2-4ee5-848e-10bf6c0d579b"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "919e1991-ce1b-456f-9e0e-324297f71a49" } ]
            OutputPorts = []
            X = 1919.5
            Y = 1745
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("89fefc98-97b2-4ee5-848e-10bf6c0d579b", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "89fefc98-97b2-4ee5-848e-10bf6c0d579b" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "a1044287-c02e-4a71-9ca0-feadfa1620c4"
            Type = Input1(128, Some 0)
            Label = "B"
            InputPorts = []
            OutputPorts =
              [ { Id = "908a9fab-02ef-4cf3-b6f2-a08c2d4dec9a"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "a1044287-c02e-4a71-9ca0-feadfa1620c4" } ]
            X = 1520.5
            Y = 1770
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree180; flipped = true }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("908a9fab-02ef-4cf3-b6f2-a08c2d4dec9a", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "908a9fab-02ef-4cf3-b6f2-a08c2d4dec9a" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "516c3e0c-3898-426a-bb4c-cb4b4427f1e1"
            Source =
              { Id = "908a9fab-02ef-4cf3-b6f2-a08c2d4dec9a"
                PortNumber = None
                PortType = PortType.Output
                HostId = "a1044287-c02e-4a71-9ca0-feadfa1620c4" }
            Target =
              { Id = "8cff242e-2750-4190-90f2-ecc451a139fa"
                PortNumber = None
                PortType = PortType.Input
                HostId = "5965a50b-0a0f-4ae6-8853-fee429a55d63" }
            Vertices =
              [ 1580.5, 1785, false
                1588.5, 1785, false
                1588.5, 1785, false
                1688.5, 1785, false ] }
          { Id = "7d3730ea-b969-43c2-af61-af0a7f4e33a2"
            Source =
              { Id = "ca4befb9-0e15-4266-901d-4876f64e505a"
                PortNumber = None
                PortType = PortType.Output
                HostId = "77e204ad-dc00-4fa5-9f29-a3cb1059976d" }
            Target =
              { Id = "9ff0b77b-3a4b-4a0a-9316-80e344cd7a23"
                PortNumber = None
                PortType = PortType.Input
                HostId = "5965a50b-0a0f-4ae6-8853-fee429a55d63" }
            Vertices =
              [ 1580.5, 1735, false
                1588.5, 1735, false
                1588.5, 1735, false
                1688.5, 1735, false ] }
          { Id = "a385d9ef-4148-4872-825c-e598f00f6e49"
            Source =
              { Id = "76c4ead7-c406-42a7-b48c-6778c40afe99"
                PortNumber = None
                PortType = PortType.Output
                HostId = "5965a50b-0a0f-4ae6-8853-fee429a55d63" }
            Target =
              { Id = "89fefc98-97b2-4ee5-848e-10bf6c0d579b"
                PortNumber = None
                PortType = PortType.Input
                HostId = "919e1991-ce1b-456f-9e0e-324297f71a49" }
            Vertices =
              [ 1808.5, 1760, false
                1816.5, 1760, false
                1816.5, 1760, false
                1919.5, 1760, false ] } ]),
       "and128")
      (([ { Id = "2fde2dce-f155-4a89-be05-e65d07bb943f"
            Type = Input1(1, Some 0)
            Label = "B"
            InputPorts = []
            OutputPorts =
              [ { Id = "3c5dc0e1-3112-4048-b378-e200ec2b82d5"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "2fde2dce-f155-4a89-be05-e65d07bb943f" } ]
            X = 1520.5
            Y = 1770
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree180; flipped = true }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("3c5dc0e1-3112-4048-b378-e200ec2b82d5", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "3c5dc0e1-3112-4048-b378-e200ec2b82d5" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "520d4352-6ab9-4a48-a7bb-038ad637e8ad"
            Type = Input1(1, Some 0)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "1ea46cd5-9464-4c78-bee3-6edc98d05002"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "520d4352-6ab9-4a48-a7bb-038ad637e8ad" } ]
            X = 1520.5
            Y = 1720
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("1ea46cd5-9464-4c78-bee3-6edc98d05002", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "1ea46cd5-9464-4c78-bee3-6edc98d05002" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "a1888d70-596c-4285-b9e5-85de7e2c26fe"
            Type = NbitsAnd(1)
            Label = "AND1"
            InputPorts =
              [ { Id = "e5b4e22c-b01e-4443-ab10-7448ec274034"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "a1888d70-596c-4285-b9e5-85de7e2c26fe" }
                { Id = "8c474c0c-4618-42d2-a189-6191d67ad599"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "a1888d70-596c-4285-b9e5-85de7e2c26fe" } ]
            OutputPorts =
              [ { Id = "e65e3bbd-33f2-4973-8940-d3318d378f26"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "a1888d70-596c-4285-b9e5-85de7e2c26fe" } ]
            X = 1688.5
            Y = 1700
            H = 120
            W = 120
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("8c474c0c-4618-42d2-a189-6191d67ad599", Left)
                            ("e5b4e22c-b01e-4443-ab10-7448ec274034", Left)
                            ("e65e3bbd-33f2-4973-8940-d3318d378f26", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "e5b4e22c-b01e-4443-ab10-7448ec274034"
                               "8c474c0c-4618-42d2-a189-6191d67ad599" ])
                            (Right, [ "e65e3bbd-33f2-4973-8940-d3318d378f26" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "ac47e24c-08d8-4abe-9937-98f8b99cb731"
            Type = Output(1)
            Label = "C"
            InputPorts =
              [ { Id = "988c8e5c-5e42-4909-b670-46df27352b2f"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "ac47e24c-08d8-4abe-9937-98f8b99cb731" } ]
            OutputPorts = []
            X = 1919.5
            Y = 1745
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("988c8e5c-5e42-4909-b670-46df27352b2f", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "988c8e5c-5e42-4909-b670-46df27352b2f" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "21abec05-86be-4458-b1b1-4f0a650ab598"
            Source =
              { Id = "3c5dc0e1-3112-4048-b378-e200ec2b82d5"
                PortNumber = None
                PortType = PortType.Output
                HostId = "2fde2dce-f155-4a89-be05-e65d07bb943f" }
            Target =
              { Id = "8c474c0c-4618-42d2-a189-6191d67ad599"
                PortNumber = None
                PortType = PortType.Input
                HostId = "a1888d70-596c-4285-b9e5-85de7e2c26fe" }
            Vertices =
              [ 1580.5, 1785, false
                1588.5, 1785, false
                1588.5, 1785, false
                1688.5, 1785, false ] }
          { Id = "92552679-7426-4ef1-a550-a19c29896a6c"
            Source =
              { Id = "1ea46cd5-9464-4c78-bee3-6edc98d05002"
                PortNumber = None
                PortType = PortType.Output
                HostId = "520d4352-6ab9-4a48-a7bb-038ad637e8ad" }
            Target =
              { Id = "e5b4e22c-b01e-4443-ab10-7448ec274034"
                PortNumber = None
                PortType = PortType.Input
                HostId = "a1888d70-596c-4285-b9e5-85de7e2c26fe" }
            Vertices =
              [ 1580.5, 1735, false
                1588.5, 1735, false
                1588.5, 1735, false
                1688.5, 1735, false ] }
          { Id = "c5dde527-fc71-4e1f-81da-d37541726bba"
            Source =
              { Id = "e65e3bbd-33f2-4973-8940-d3318d378f26"
                PortNumber = None
                PortType = PortType.Output
                HostId = "a1888d70-596c-4285-b9e5-85de7e2c26fe" }
            Target =
              { Id = "988c8e5c-5e42-4909-b670-46df27352b2f"
                PortNumber = None
                PortType = PortType.Input
                HostId = "ac47e24c-08d8-4abe-9937-98f8b99cb731" }
            Vertices =
              [ 1808.5, 1760, false
                1816.5, 1760, false
                1816.5, 1760, false
                1919.5, 1760, false ] } ]),
       "and1")
      (([ { Id = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3"
            Type = NbitsAnd(2)
            Label = "AND1"
            InputPorts =
              [ { Id = "db3a678a-53d3-4cc2-9200-9eeb1784d552"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3" }
                { Id = "60de931f-dc75-44d3-92bb-a25abaf506b7"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3" } ]
            OutputPorts =
              [ { Id = "e5aff772-97a4-416a-90b9-76846c0ab3f4"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3" } ]
            X = 1688.5
            Y = 1700
            H = 120
            W = 120
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("60de931f-dc75-44d3-92bb-a25abaf506b7", Left)
                            ("db3a678a-53d3-4cc2-9200-9eeb1784d552", Left)
                            ("e5aff772-97a4-416a-90b9-76846c0ab3f4", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "db3a678a-53d3-4cc2-9200-9eeb1784d552"
                               "60de931f-dc75-44d3-92bb-a25abaf506b7" ])
                            (Right, [ "e5aff772-97a4-416a-90b9-76846c0ab3f4" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "6089eb8d-dbe4-4270-bfa5-995f3528744b"
            Type = Input1(2, Some 0)
            Label = "B"
            InputPorts = []
            OutputPorts =
              [ { Id = "a13aa79d-15d1-4330-b857-96d8a40c8db4"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "6089eb8d-dbe4-4270-bfa5-995f3528744b" } ]
            X = 1520.5
            Y = 1770
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree180; flipped = true }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("a13aa79d-15d1-4330-b857-96d8a40c8db4", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "a13aa79d-15d1-4330-b857-96d8a40c8db4" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "6e23ceeb-409b-47e4-a2c7-5598278eeba8"
            Type = Output(2)
            Label = "C"
            InputPorts =
              [ { Id = "86fc81e1-d00f-4526-96bd-e2c0b6207628"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "6e23ceeb-409b-47e4-a2c7-5598278eeba8" } ]
            OutputPorts = []
            X = 1919.5
            Y = 1745
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("86fc81e1-d00f-4526-96bd-e2c0b6207628", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "86fc81e1-d00f-4526-96bd-e2c0b6207628" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "f0730d32-4420-45e5-a6ff-7fda8bc9ffcd"
            Type = Input1(2, Some 0)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "e993bde5-bf82-4fcb-93c7-5803a87eebc3"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "f0730d32-4420-45e5-a6ff-7fda8bc9ffcd" } ]
            X = 1520.5
            Y = 1720
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("e993bde5-bf82-4fcb-93c7-5803a87eebc3", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "e993bde5-bf82-4fcb-93c7-5803a87eebc3" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "39244df1-0aaf-4e60-bb05-01598f005b12"
            Source =
              { Id = "e993bde5-bf82-4fcb-93c7-5803a87eebc3"
                PortNumber = None
                PortType = PortType.Output
                HostId = "f0730d32-4420-45e5-a6ff-7fda8bc9ffcd" }
            Target =
              { Id = "db3a678a-53d3-4cc2-9200-9eeb1784d552"
                PortNumber = None
                PortType = PortType.Input
                HostId = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3" }
            Vertices =
              [ 1580.5, 1735, false
                1588.5, 1735, false
                1588.5, 1735, false
                1688.5, 1735, false ] }
          { Id = "5287c227-4770-4244-bbb6-a424eee6656a"
            Source =
              { Id = "e5aff772-97a4-416a-90b9-76846c0ab3f4"
                PortNumber = None
                PortType = PortType.Output
                HostId = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3" }
            Target =
              { Id = "86fc81e1-d00f-4526-96bd-e2c0b6207628"
                PortNumber = None
                PortType = PortType.Input
                HostId = "6e23ceeb-409b-47e4-a2c7-5598278eeba8" }
            Vertices =
              [ 1808.5, 1760, false
                1816.5, 1760, false
                1816.5, 1760, false
                1919.5, 1760, false ] }
          { Id = "9d65ad85-b61c-4e8e-b076-c02581da1e51"
            Source =
              { Id = "a13aa79d-15d1-4330-b857-96d8a40c8db4"
                PortNumber = None
                PortType = PortType.Output
                HostId = "6089eb8d-dbe4-4270-bfa5-995f3528744b" }
            Target =
              { Id = "60de931f-dc75-44d3-92bb-a25abaf506b7"
                PortNumber = None
                PortType = PortType.Input
                HostId = "28bb5d09-bb2b-4492-b17b-8bc8905f23f3" }
            Vertices =
              [ 1580.5, 1785, false
                1588.5, 1785, false
                1588.5, 1785, false
                1688.5, 1785, false ] } ]),
       "and2")
      (([ { Id = "15756a58-8684-4127-abf7-359e411372e7"
            Type = Input1(32, Some 0)
            Label = "B"
            InputPorts = []
            OutputPorts =
              [ { Id = "0c10c1a3-be15-4be8-b026-475b82b656f4"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "15756a58-8684-4127-abf7-359e411372e7" } ]
            X = 1520.5
            Y = 1770
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree180; flipped = true }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("0c10c1a3-be15-4be8-b026-475b82b656f4", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "0c10c1a3-be15-4be8-b026-475b82b656f4" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "834c7284-87e0-4f6a-9e50-e923355589c6"
            Type = Input1(32, Some 0)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "1a155fc8-5a64-4d96-b084-efb80813340e"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "834c7284-87e0-4f6a-9e50-e923355589c6" } ]
            X = 1520.5
            Y = 1720
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("1a155fc8-5a64-4d96-b084-efb80813340e", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "1a155fc8-5a64-4d96-b084-efb80813340e" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "92a60c90-1db5-4a9e-a212-53393647af50"
            Type = Output(32)
            Label = "C"
            InputPorts =
              [ { Id = "d1a3074a-2386-4696-8416-3897aa83f33e"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "92a60c90-1db5-4a9e-a212-53393647af50" } ]
            OutputPorts = []
            X = 1919.5
            Y = 1745
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("d1a3074a-2386-4696-8416-3897aa83f33e", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "d1a3074a-2386-4696-8416-3897aa83f33e" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "b4de869b-52a8-403a-afed-8fede9f45174"
            Type = NbitsAnd(32)
            Label = "AND1"
            InputPorts =
              [ { Id = "257d8d0a-f5c8-452e-8959-e4290a9cb147"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "b4de869b-52a8-403a-afed-8fede9f45174" }
                { Id = "dd319f18-e9ae-4be8-91a1-6028d348c6c7"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "b4de869b-52a8-403a-afed-8fede9f45174" } ]
            OutputPorts =
              [ { Id = "1a9b33b9-c37e-4dba-b8a2-6484c6e1c29f"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "b4de869b-52a8-403a-afed-8fede9f45174" } ]
            X = 1688.5
            Y = 1700
            H = 120
            W = 120
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("1a9b33b9-c37e-4dba-b8a2-6484c6e1c29f", Right)
                            ("257d8d0a-f5c8-452e-8959-e4290a9cb147", Left)
                            ("dd319f18-e9ae-4be8-91a1-6028d348c6c7", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "257d8d0a-f5c8-452e-8959-e4290a9cb147"
                               "dd319f18-e9ae-4be8-91a1-6028d348c6c7" ])
                            (Right, [ "1a9b33b9-c37e-4dba-b8a2-6484c6e1c29f" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "43750a0a-bfb1-4a6f-af1d-dc5be558cdec"
            Source =
              { Id = "1a9b33b9-c37e-4dba-b8a2-6484c6e1c29f"
                PortNumber = None
                PortType = PortType.Output
                HostId = "b4de869b-52a8-403a-afed-8fede9f45174" }
            Target =
              { Id = "d1a3074a-2386-4696-8416-3897aa83f33e"
                PortNumber = None
                PortType = PortType.Input
                HostId = "92a60c90-1db5-4a9e-a212-53393647af50" }
            Vertices =
              [ 1808.5, 1760, false
                1816.5, 1760, false
                1816.5, 1760, false
                1919.5, 1760, false ] }
          { Id = "467ccd3c-bccd-49ce-b2f2-821987df0493"
            Source =
              { Id = "1a155fc8-5a64-4d96-b084-efb80813340e"
                PortNumber = None
                PortType = PortType.Output
                HostId = "834c7284-87e0-4f6a-9e50-e923355589c6" }
            Target =
              { Id = "257d8d0a-f5c8-452e-8959-e4290a9cb147"
                PortNumber = None
                PortType = PortType.Input
                HostId = "b4de869b-52a8-403a-afed-8fede9f45174" }
            Vertices =
              [ 1580.5, 1735, false
                1588.5, 1735, false
                1588.5, 1735, false
                1688.5, 1735, false ] }
          { Id = "ce5dc55b-3ff0-4d90-8858-47a188a6ad74"
            Source =
              { Id = "0c10c1a3-be15-4be8-b026-475b82b656f4"
                PortNumber = None
                PortType = PortType.Output
                HostId = "15756a58-8684-4127-abf7-359e411372e7" }
            Target =
              { Id = "dd319f18-e9ae-4be8-91a1-6028d348c6c7"
                PortNumber = None
                PortType = PortType.Input
                HostId = "b4de869b-52a8-403a-afed-8fede9f45174" }
            Vertices =
              [ 1580.5, 1785, false
                1588.5, 1785, false
                1588.5, 1785, false
                1688.5, 1785, false ] } ]),
       "and32")
      (([ { Id = "2f9a1c40-a96b-4afd-8447-998be63cd3cf"
            Type = Input1(64, Some 1)
            Label = "B"
            InputPorts = []
            OutputPorts =
              [ { Id = "ee95a37a-6f55-41df-848b-5c44c99c5a77"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "2f9a1c40-a96b-4afd-8447-998be63cd3cf" } ]
            X = 1487.5
            Y = 1779
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree180; flipped = true }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("ee95a37a-6f55-41df-848b-5c44c99c5a77", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "ee95a37a-6f55-41df-848b-5c44c99c5a77" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "50480dad-f04b-4200-9510-da39edc0ebe4"
            Type = IOLabel
            Label = "D"
            InputPorts =
              [ { Id = "accea5da-6ad6-43f0-afdc-69fa95fc7248"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "50480dad-f04b-4200-9510-da39edc0ebe4" } ]
            OutputPorts =
              [ { Id = "169813cd-553d-4cf3-9bc3-19638ae7088a"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "50480dad-f04b-4200-9510-da39edc0ebe4" } ]
            X = 1881.75
            Y = 1683
            H = 15
            W = 30
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("169813cd-553d-4cf3-9bc3-19638ae7088a", Right)
                            ("accea5da-6ad6-43f0-afdc-69fa95fc7248", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "accea5da-6ad6-43f0-afdc-69fa95fc7248" ])
                            (Right, [ "169813cd-553d-4cf3-9bc3-19638ae7088a" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "a58527ea-1929-4551-9b14-f6c0184d9200"
            Type = Output(64)
            Label = "C"
            InputPorts =
              [ { Id = "a0d75cb3-c9f6-4d0d-a985-77e763146475"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "a58527ea-1929-4551-9b14-f6c0184d9200" } ]
            OutputPorts = []
            X = 1952.5
            Y = 1729
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("a0d75cb3-c9f6-4d0d-a985-77e763146475", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "a0d75cb3-c9f6-4d0d-a985-77e763146475" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "af8ccbbe-c6e6-4980-aea3-c601a9c9f35c"
            Type = IOLabel
            Label = "D"
            InputPorts =
              [ { Id = "54bec1a3-a9bc-48ba-a79a-09fb9295b83e"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "af8ccbbe-c6e6-4980-aea3-c601a9c9f35c" } ]
            OutputPorts =
              [ { Id = "2064263b-8dd2-43e6-b74e-bd211ded078c"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "af8ccbbe-c6e6-4980-aea3-c601a9c9f35c" } ]
            X = 1817.8012958963282
            Y = 1711
            H = 15
            W = 30
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("2064263b-8dd2-43e6-b74e-bd211ded078c", Right)
                            ("54bec1a3-a9bc-48ba-a79a-09fb9295b83e", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "54bec1a3-a9bc-48ba-a79a-09fb9295b83e" ])
                            (Right, [ "2064263b-8dd2-43e6-b74e-bd211ded078c" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3"
            Type = NbitsAnd(64)
            Label = "AND1"
            InputPorts =
              [ { Id = "bc7d3404-4bd1-4975-b247-1e15a48cb32f"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3" }
                { Id = "4e4c6859-0b4b-4a59-8bd6-d75d25a53032"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3" } ]
            OutputPorts =
              [ { Id = "21979914-81f7-4924-8de7-baa134de4534"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3" } ]
            X = 1655.5
            Y = 1709
            H = 120
            W = 120
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("21979914-81f7-4924-8de7-baa134de4534", Right)
                            ("4e4c6859-0b4b-4a59-8bd6-d75d25a53032", Left)
                            ("bc7d3404-4bd1-4975-b247-1e15a48cb32f", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "bc7d3404-4bd1-4975-b247-1e15a48cb32f"
                               "4e4c6859-0b4b-4a59-8bd6-d75d25a53032" ])
                            (Right, [ "21979914-81f7-4924-8de7-baa134de4534" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "e0ea5256-7e2a-49f4-86a0-491df84c3d06"
            Type = Input1(64, Some 1)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "384690fe-9a63-42a9-9ac7-aa2da4c7df57"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "e0ea5256-7e2a-49f4-86a0-491df84c3d06" } ]
            X = 1487.5
            Y = 1729
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("384690fe-9a63-42a9-9ac7-aa2da4c7df57", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "384690fe-9a63-42a9-9ac7-aa2da4c7df57" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "41f3c938-522f-401d-8f03-ecd225dedcd1"
            Source =
              { Id = "384690fe-9a63-42a9-9ac7-aa2da4c7df57"
                PortNumber = None
                PortType = PortType.Output
                HostId = "e0ea5256-7e2a-49f4-86a0-491df84c3d06" }
            Target =
              { Id = "bc7d3404-4bd1-4975-b247-1e15a48cb32f"
                PortNumber = None
                PortType = PortType.Input
                HostId = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3" }
            Vertices =
              [ 1547.5, 1744, false
                1555.5, 1744, false
                1555.5, 1744, false
                1655.5, 1744, false ] }
          { Id = "4dc4a398-876c-4055-8c32-ca5095a97f51"
            Source =
              { Id = "169813cd-553d-4cf3-9bc3-19638ae7088a"
                PortNumber = None
                PortType = PortType.Output
                HostId = "50480dad-f04b-4200-9510-da39edc0ebe4" }
            Target =
              { Id = "a0d75cb3-c9f6-4d0d-a985-77e763146475"
                PortNumber = None
                PortType = PortType.Input
                HostId = "a58527ea-1929-4551-9b14-f6c0184d9200" }
            Vertices =
              [ 1911.75, 1690.5, false
                1919.75, 1690.5, false
                1919.75, 1690.5, false
                1932.125, 1690.5, false
                1932.125, 1744, false
                1944.5, 1744, false
                1944.5, 1744, false
                1952.5, 1744, false ] }
          { Id = "8c562f06-002e-4ff7-b0f5-6f4e12307051"
            Source =
              { Id = "ee95a37a-6f55-41df-848b-5c44c99c5a77"
                PortNumber = None
                PortType = PortType.Output
                HostId = "2f9a1c40-a96b-4afd-8447-998be63cd3cf" }
            Target =
              { Id = "4e4c6859-0b4b-4a59-8bd6-d75d25a53032"
                PortNumber = None
                PortType = PortType.Input
                HostId = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3" }
            Vertices =
              [ 1547.5, 1794, false
                1555.5, 1794, false
                1555.5, 1794, false
                1655.5, 1794, false ] }
          { Id = "acdf089f-fcb2-4d3a-86f0-4ca06afc2a3a"
            Source =
              { Id = "21979914-81f7-4924-8de7-baa134de4534"
                PortNumber = None
                PortType = PortType.Output
                HostId = "c5588f6f-e33a-44ba-abc8-e5ad59af38d3" }
            Target =
              { Id = "54bec1a3-a9bc-48ba-a79a-09fb9295b83e"
                PortNumber = None
                PortType = PortType.Input
                HostId = "af8ccbbe-c6e6-4980-aea3-c601a9c9f35c" }
            Vertices =
              [ 1775.5, 1769, false
                1783.5, 1769, false
                1783.5, 1769, false
                1796.650647948164, 1769, false
                1796.650647948164, 1718.5, false
                1809.8012958963282, 1718.5, false
                1809.8012958963282, 1718.5, false
                1817.8012958963282, 1718.5, false ] } ]),
       "and64")
      (([ { Id = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e"
            Type = Counter(16)
            Label = "CNT1"
            InputPorts =
              [ { Id = "c70e6fa4-bfb3-498e-800d-afc2b9378a8c"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" }
                { Id = "ef4d141d-c1c2-411e-8fc7-051262623feb"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" }
                { Id = "7cf5b085-18c7-47a1-bffc-371fd35aacd5"
                  PortNumber = Some 2
                  PortType = PortType.Input
                  HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" } ]
            OutputPorts =
              [ { Id = "d280543e-f737-4a58-809e-2379fb50ce93"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" } ]
            X = 1590.3749999999998
            Y = 1680.3970588235293
            H = 120
            W = 150
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("7cf5b085-18c7-47a1-bffc-371fd35aacd5", Left)
                            ("c70e6fa4-bfb3-498e-800d-afc2b9378a8c", Left)
                            ("d280543e-f737-4a58-809e-2379fb50ce93", Right)
                            ("ef4d141d-c1c2-411e-8fc7-051262623feb", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "c70e6fa4-bfb3-498e-800d-afc2b9378a8c"
                               "ef4d141d-c1c2-411e-8fc7-051262623feb"
                               "7cf5b085-18c7-47a1-bffc-371fd35aacd5" ])
                            (Right, [ "d280543e-f737-4a58-809e-2379fb50ce93" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "269131c2-6a64-4d4b-9ead-3c85215c4c1d"
            Type = RegisterE(16)
            Label = "REG1"
            InputPorts =
              [ { Id = "adcc149c-0bf5-4606-9a51-fc560c8bd70d"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "269131c2-6a64-4d4b-9ead-3c85215c4c1d" }
                { Id = "c4a9e3d6-5664-4049-9a1f-77bf092c4425"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "269131c2-6a64-4d4b-9ead-3c85215c4c1d" } ]
            OutputPorts =
              [ { Id = "1e5d6a81-3dfe-4b84-a309-dbb1a0b2ccc0"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "269131c2-6a64-4d4b-9ead-3c85215c4c1d" } ]
            X = 1803.375
            Y = 1710.3970588235293
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
                          [ ("1e5d6a81-3dfe-4b84-a309-dbb1a0b2ccc0", Right)
                            ("adcc149c-0bf5-4606-9a51-fc560c8bd70d", Left)
                            ("c4a9e3d6-5664-4049-9a1f-77bf092c4425", Bottom) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [ "c4a9e3d6-5664-4049-9a1f-77bf092c4425" ])
                            (Left, [ "adcc149c-0bf5-4606-9a51-fc560c8bd70d" ])
                            (Right, [ "1e5d6a81-3dfe-4b84-a309-dbb1a0b2ccc0" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "4aac1720-6e3f-45c7-8373-95eacdbf48a4"
            Type = Output(16)
            Label = "B"
            InputPorts =
              [ { Id = "01fdac56-d19d-4730-b67a-f507fa32d213"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "4aac1720-6e3f-45c7-8373-95eacdbf48a4" } ]
            OutputPorts = []
            X = 1989.375
            Y = 1725.3970588235293
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("01fdac56-d19d-4730-b67a-f507fa32d213", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "01fdac56-d19d-4730-b67a-f507fa32d213" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "789f48fc-3c15-4643-8915-29fee39e5d21"
            Type = Input1(1, Some 1)
            Label = "EN"
            InputPorts = []
            OutputPorts =
              [ { Id = "94424d0e-f604-4809-bfb6-eabf90b291a8"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "789f48fc-3c15-4643-8915-29fee39e5d21" } ]
            X = 1450.6249999999998
            Y = 1854.3970588235293
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("94424d0e-f604-4809-bfb6-eabf90b291a8", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "94424d0e-f604-4809-bfb6-eabf90b291a8" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "9d8b478b-1372-430c-98b5-df237c9586da"
            Type = Input1(16, Some 22)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "800d7b3d-0f48-4c52-a9f7-df842d9a6de1"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "9d8b478b-1372-430c-98b5-df237c9586da" } ]
            X = 1450.6249999999998
            Y = 1645.6029411764705
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("800d7b3d-0f48-4c52-a9f7-df842d9a6de1", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "800d7b3d-0f48-4c52-a9f7-df842d9a6de1" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "e4932522-6427-4c7b-8274-5d8f2411024c"
            Type = Input1(1, Some 0)
            Label = "LOAD"
            InputPorts = []
            OutputPorts =
              [ { Id = "bc9237de-8e47-4f13-a82b-bc34aa0b5b1c"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "e4932522-6427-4c7b-8274-5d8f2411024c" } ]
            X = 1450.6249999999998
            Y = 1725.3970588235293
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("bc9237de-8e47-4f13-a82b-bc34aa0b5b1c", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "bc9237de-8e47-4f13-a82b-bc34aa0b5b1c" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "448ffdca-c709-4837-9dce-73fd3513c190"
            Source =
              { Id = "1e5d6a81-3dfe-4b84-a309-dbb1a0b2ccc0"
                PortNumber = None
                PortType = PortType.Output
                HostId = "269131c2-6a64-4d4b-9ead-3c85215c4c1d" }
            Target =
              { Id = "01fdac56-d19d-4730-b67a-f507fa32d213"
                PortNumber = None
                PortType = PortType.Input
                HostId = "4aac1720-6e3f-45c7-8373-95eacdbf48a4" }
            Vertices =
              [ 1923.375, 1740.3970588235293, false
                1931.375, 1740.3970588235293, false
                1931.375, 1740.3970588235293, false
                1989.375, 1740.3970588235293, false ] }
          { Id = "634f26d2-4ef6-44aa-ba40-642f0a80998a"
            Source =
              { Id = "94424d0e-f604-4809-bfb6-eabf90b291a8"
                PortNumber = None
                PortType = PortType.Output
                HostId = "789f48fc-3c15-4643-8915-29fee39e5d21" }
            Target =
              { Id = "7cf5b085-18c7-47a1-bffc-371fd35aacd5"
                PortNumber = None
                PortType = PortType.Input
                HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" }
            Vertices =
              [ 1510.6249999999998, 1869.3970588235293, false
                1518.6249999999998, 1869.3970588235293, false
                1518.6249999999998, 1869.3970588235293, false
                1550.4999999999998, 1869.3970588235293, false
                1550.4999999999998, 1775.691176470588, false
                1582.3749999999998, 1775.691176470588, false
                1582.3749999999998, 1775.691176470588, false
                1590.3749999999998, 1775.691176470588, false ] }
          { Id = "97c87b15-f935-43ea-b672-ceb8c6f1e456"
            Source =
              { Id = "800d7b3d-0f48-4c52-a9f7-df842d9a6de1"
                PortNumber = None
                PortType = PortType.Output
                HostId = "9d8b478b-1372-430c-98b5-df237c9586da" }
            Target =
              { Id = "c70e6fa4-bfb3-498e-800d-afc2b9378a8c"
                PortNumber = None
                PortType = PortType.Input
                HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" }
            Vertices =
              [ 1510.6249999999998, 1660.6029411764705, false
                1518.6249999999998, 1660.6029411764705, false
                1518.6249999999998, 1660.6029411764705, false
                1550.4999999999998, 1660.6029411764705, false
                1550.4999999999998, 1705.1029411764705, false
                1582.3749999999998, 1705.1029411764705, false
                1582.3749999999998, 1705.1029411764705, false
                1590.3749999999998, 1705.1029411764705, false ] }
          { Id = "c59c537a-a306-4b36-aa01-e6e45cb556ce"
            Source =
              { Id = "d280543e-f737-4a58-809e-2379fb50ce93"
                PortNumber = None
                PortType = PortType.Output
                HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" }
            Target =
              { Id = "adcc149c-0bf5-4606-9a51-fc560c8bd70d"
                PortNumber = None
                PortType = PortType.Input
                HostId = "269131c2-6a64-4d4b-9ead-3c85215c4c1d" }
            Vertices =
              [ 1740.3749999999998, 1740.3970588235293, false
                1748.3749999999998, 1740.3970588235293, false
                1748.3749999999998, 1740.3970588235293, false
                1803.3749999999998, 1740.3970588235293, false ] }
          { Id = "e3ffd2b5-1d5d-46cd-86ab-822fad619b42"
            Source =
              { Id = "bc9237de-8e47-4f13-a82b-bc34aa0b5b1c"
                PortNumber = None
                PortType = PortType.Output
                HostId = "e4932522-6427-4c7b-8274-5d8f2411024c" }
            Target =
              { Id = "ef4d141d-c1c2-411e-8fc7-051262623feb"
                PortNumber = None
                PortType = PortType.Input
                HostId = "1913fbd4-e1f0-4d8d-901d-a7e367a6169e" }
            Vertices =
              [ 1510.6249999999998, 1740.3970588235293, false
                1518.6249999999998, 1740.3970588235293, false
                1518.6249999999998, 1740.3970588235293, false
                1590.3749999999998, 1740.3970588235293, false ] }
          { Id = "f699d3b3-b0d8-4bb6-9933-9fa543d588de"
            Source =
              { Id = "94424d0e-f604-4809-bfb6-eabf90b291a8"
                PortNumber = None
                PortType = PortType.Output
                HostId = "789f48fc-3c15-4643-8915-29fee39e5d21" }
            Target =
              { Id = "c4a9e3d6-5664-4049-9a1f-77bf092c4425"
                PortNumber = None
                PortType = PortType.Input
                HostId = "269131c2-6a64-4d4b-9ead-3c85215c4c1d" }
            Vertices =
              [ 1510.6249999999998, 1869.3970588235293, false
                1518.6249999999998, 1869.3970588235293, false
                1518.6249999999998, 1869.3970588235293, false
                1863.3749999999998, 1869.3970588235293, false
                1863.3749999999998, 1778.3970588235293, false
                1863.3749999999998, 1778.3970588235293, false
                1863.3749999999998, 1770.3970588235293, false ] } ]),
       "cnt16")
      (([ { Id = "371a7cbe-ba87-45e8-be0c-4c7bd0659557"
            Type = Input1(64, Some 22)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "8b0a942d-5892-4ef5-b972-ff3c8c13fa81"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "371a7cbe-ba87-45e8-be0c-4c7bd0659557" } ]
            X = 1450.625
            Y = 1645.6029411764705
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("8b0a942d-5892-4ef5-b972-ff3c8c13fa81", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "8b0a942d-5892-4ef5-b972-ff3c8c13fa81" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "4dd2a7e1-f7cc-4583-a258-25af6656ecaf"
            Type = Input1(1, Some 1)
            Label = "EN"
            InputPorts = []
            OutputPorts =
              [ { Id = "1143d423-5877-4b48-ab9a-52e6bde83040"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "4dd2a7e1-f7cc-4583-a258-25af6656ecaf" } ]
            X = 1450.625
            Y = 1854.3970588235293
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("1143d423-5877-4b48-ab9a-52e6bde83040", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "1143d423-5877-4b48-ab9a-52e6bde83040" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "4e040498-cf2f-4046-8827-c8777b4c72ed"
            Type = RegisterE(64)
            Label = "REG1"
            InputPorts =
              [ { Id = "86939cfc-719e-4236-aaf9-e449b699ef0a"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "4e040498-cf2f-4046-8827-c8777b4c72ed" }
                { Id = "f30ff833-1354-4b86-be48-afaa5db0d0a1"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "4e040498-cf2f-4046-8827-c8777b4c72ed" } ]
            OutputPorts =
              [ { Id = "a6f48217-140a-44af-9baa-123612eaaada"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "4e040498-cf2f-4046-8827-c8777b4c72ed" } ]
            X = 1803.375
            Y = 1710.3970588235293
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
                          [ ("86939cfc-719e-4236-aaf9-e449b699ef0a", Left)
                            ("a6f48217-140a-44af-9baa-123612eaaada", Right)
                            ("f30ff833-1354-4b86-be48-afaa5db0d0a1", Bottom) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [ "f30ff833-1354-4b86-be48-afaa5db0d0a1" ])
                            (Left, [ "86939cfc-719e-4236-aaf9-e449b699ef0a" ])
                            (Right, [ "a6f48217-140a-44af-9baa-123612eaaada" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "4fdd4fc8-40a2-4f21-a9e4-16edc36df06a"
            Type = Output(64)
            Label = "B"
            InputPorts =
              [ { Id = "d6dcd328-e007-4e6a-88ea-47dc21d9186c"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "4fdd4fc8-40a2-4f21-a9e4-16edc36df06a" } ]
            OutputPorts = []
            X = 1989.375
            Y = 1725.3970588235293
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("d6dcd328-e007-4e6a-88ea-47dc21d9186c", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "d6dcd328-e007-4e6a-88ea-47dc21d9186c" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "617b6760-319e-42c3-89b6-42bec999f2d2"
            Type = Input1(1, Some 0)
            Label = "LOAD"
            InputPorts = []
            OutputPorts =
              [ { Id = "c1edc084-ae38-4554-9d93-84857f53e511"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "617b6760-319e-42c3-89b6-42bec999f2d2" } ]
            X = 1450.625
            Y = 1725.3970588235293
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("c1edc084-ae38-4554-9d93-84857f53e511", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "c1edc084-ae38-4554-9d93-84857f53e511" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "6edd25d0-31a6-406d-88b4-876283c0d1bf"
            Type = Counter(64)
            Label = "CNT1"
            InputPorts =
              [ { Id = "7472cb35-e664-484d-b242-7823c774345f"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" }
                { Id = "046c8e1f-cec9-43bb-ae31-dbc28e8cb58c"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" }
                { Id = "4edf1724-e558-4fa6-89a8-62aa585b9fe4"
                  PortNumber = Some 2
                  PortType = PortType.Input
                  HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" } ]
            OutputPorts =
              [ { Id = "f11eeef8-7011-46d2-a0f7-248f5d858287"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" } ]
            X = 1590.375
            Y = 1680.3970588235293
            H = 120
            W = 150
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("046c8e1f-cec9-43bb-ae31-dbc28e8cb58c", Left)
                            ("4edf1724-e558-4fa6-89a8-62aa585b9fe4", Left)
                            ("7472cb35-e664-484d-b242-7823c774345f", Left)
                            ("f11eeef8-7011-46d2-a0f7-248f5d858287", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "7472cb35-e664-484d-b242-7823c774345f"
                               "046c8e1f-cec9-43bb-ae31-dbc28e8cb58c"
                               "4edf1724-e558-4fa6-89a8-62aa585b9fe4" ])
                            (Right, [ "f11eeef8-7011-46d2-a0f7-248f5d858287" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "1841fb1d-584a-4e4b-aa7a-3baef12cda3c"
            Source =
              { Id = "1143d423-5877-4b48-ab9a-52e6bde83040"
                PortNumber = None
                PortType = PortType.Output
                HostId = "4dd2a7e1-f7cc-4583-a258-25af6656ecaf" }
            Target =
              { Id = "f30ff833-1354-4b86-be48-afaa5db0d0a1"
                PortNumber = None
                PortType = PortType.Input
                HostId = "4e040498-cf2f-4046-8827-c8777b4c72ed" }
            Vertices =
              [ 1510.625, 1869.3970588235293, false
                1518.625, 1869.3970588235293, false
                1518.625, 1869.3970588235293, false
                1863.375, 1869.3970588235293, false
                1863.375, 1778.3970588235293, false
                1863.375, 1778.3970588235293, false
                1863.375, 1770.3970588235293, false ] }
          { Id = "1efa6678-5f36-42cb-9f72-df3b185e4068"
            Source =
              { Id = "1143d423-5877-4b48-ab9a-52e6bde83040"
                PortNumber = None
                PortType = PortType.Output
                HostId = "4dd2a7e1-f7cc-4583-a258-25af6656ecaf" }
            Target =
              { Id = "4edf1724-e558-4fa6-89a8-62aa585b9fe4"
                PortNumber = None
                PortType = PortType.Input
                HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" }
            Vertices =
              [ 1510.625, 1869.3970588235293, false
                1518.625, 1869.3970588235293, false
                1518.625, 1869.3970588235293, false
                1550.5, 1869.3970588235293, false
                1550.5, 1775.691176470588, false
                1582.375, 1775.691176470588, false
                1582.375, 1775.691176470588, false
                1590.375, 1775.691176470588, false ] }
          { Id = "20050398-758a-46d7-8839-cd77c839c408"
            Source =
              { Id = "f11eeef8-7011-46d2-a0f7-248f5d858287"
                PortNumber = None
                PortType = PortType.Output
                HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" }
            Target =
              { Id = "86939cfc-719e-4236-aaf9-e449b699ef0a"
                PortNumber = None
                PortType = PortType.Input
                HostId = "4e040498-cf2f-4046-8827-c8777b4c72ed" }
            Vertices =
              [ 1740.375, 1740.3970588235293, false
                1748.375, 1740.3970588235293, false
                1748.375, 1740.3970588235293, false
                1803.375, 1740.3970588235293, false ] }
          { Id = "2162cb0c-0def-4271-bb98-faa62eba91c5"
            Source =
              { Id = "8b0a942d-5892-4ef5-b972-ff3c8c13fa81"
                PortNumber = None
                PortType = PortType.Output
                HostId = "371a7cbe-ba87-45e8-be0c-4c7bd0659557" }
            Target =
              { Id = "7472cb35-e664-484d-b242-7823c774345f"
                PortNumber = None
                PortType = PortType.Input
                HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" }
            Vertices =
              [ 1510.625, 1660.6029411764705, false
                1518.625, 1660.6029411764705, false
                1518.625, 1660.6029411764705, false
                1550.5, 1660.6029411764705, false
                1550.5, 1705.1029411764705, false
                1582.375, 1705.1029411764705, false
                1582.375, 1705.1029411764705, false
                1590.375, 1705.1029411764705, false ] }
          { Id = "81ec9443-88d0-47fb-83a9-fe3cccb9eba0"
            Source =
              { Id = "a6f48217-140a-44af-9baa-123612eaaada"
                PortNumber = None
                PortType = PortType.Output
                HostId = "4e040498-cf2f-4046-8827-c8777b4c72ed" }
            Target =
              { Id = "d6dcd328-e007-4e6a-88ea-47dc21d9186c"
                PortNumber = None
                PortType = PortType.Input
                HostId = "4fdd4fc8-40a2-4f21-a9e4-16edc36df06a" }
            Vertices =
              [ 1923.375, 1740.3970588235293, false
                1931.375, 1740.3970588235293, false
                1931.375, 1740.3970588235293, false
                1989.375, 1740.3970588235293, false ] }
          { Id = "b453fc86-db43-4d64-bd40-f4982ee91e60"
            Source =
              { Id = "c1edc084-ae38-4554-9d93-84857f53e511"
                PortNumber = None
                PortType = PortType.Output
                HostId = "617b6760-319e-42c3-89b6-42bec999f2d2" }
            Target =
              { Id = "046c8e1f-cec9-43bb-ae31-dbc28e8cb58c"
                PortNumber = None
                PortType = PortType.Input
                HostId = "6edd25d0-31a6-406d-88b4-876283c0d1bf" }
            Vertices =
              [ 1510.625, 1740.3970588235293, false
                1518.625, 1740.3970588235293, false
                1518.625, 1740.3970588235293, false
                1590.375, 1740.3970588235293, false ] } ]),
       "cnt64") ]
