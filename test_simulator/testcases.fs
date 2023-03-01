module TestCases

open CommonTypes
open SimulatorTypes

let canvasStates: (CanvasState * string) list =
    [ (([ { Id = "1b25ccc7-9f29-43df-a60c-239924afb313"
            Type = Output(1)
            Label = "C"
            InputPorts =
              [ { Id = "3ffb039f-7980-4e5b-872e-141276983c58"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "1b25ccc7-9f29-43df-a60c-239924afb313" } ]
            OutputPorts = []
            X = 1837.75
            Y = 1729.78125
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("3ffb039f-7980-4e5b-872e-141276983c58", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "3ffb039f-7980-4e5b-872e-141276983c58" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "2c9b6415-3485-4086-8a83-ceec8fdcff32"
            Type = And
            Label = "G1"
            InputPorts =
              [ { Id = "23f4cf44-7b7e-4d58-9e29-471f72ebf141"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "2c9b6415-3485-4086-8a83-ceec8fdcff32" }
                { Id = "857923ad-a2e0-459c-9b8b-070ea95b6dc8"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "2c9b6415-3485-4086-8a83-ceec8fdcff32" } ]
            OutputPorts =
              [ { Id = "236cb495-5345-445a-aa39-8b39c2233bee"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "2c9b6415-3485-4086-8a83-ceec8fdcff32" } ]
            X = 1720.75
            Y = 1722.28125
            H = 45
            W = 45
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("236cb495-5345-445a-aa39-8b39c2233bee", Right)
                            ("23f4cf44-7b7e-4d58-9e29-471f72ebf141", Left)
                            ("857923ad-a2e0-459c-9b8b-070ea95b6dc8", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left,
                             [ "23f4cf44-7b7e-4d58-9e29-471f72ebf141"
                               "857923ad-a2e0-459c-9b8b-070ea95b6dc8" ])
                            (Right, [ "236cb495-5345-445a-aa39-8b39c2233bee" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "4bbe2223-fd26-4b02-8dd5-b92d4369a549"
            Type = Input1(1, Some 0)
            Label = "B"
            InputPorts = []
            OutputPorts =
              [ { Id = "477114ad-e47f-4ef2-b995-8b95dbc0f077"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "4bbe2223-fd26-4b02-8dd5-b92d4369a549" } ]
            X = 1602.25
            Y = 1784.28125
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("477114ad-e47f-4ef2-b995-8b95dbc0f077", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "477114ad-e47f-4ef2-b995-8b95dbc0f077" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "b4341e9a-c449-4389-9cdf-15f2c841ffc5"
            Type = Input1(1, Some 0)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "8c09fdea-45de-4d43-a3d5-4b4687ce5807"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "b4341e9a-c449-4389-9cdf-15f2c841ffc5" } ]
            X = 1602.25
            Y = 1715.71875
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("8c09fdea-45de-4d43-a3d5-4b4687ce5807", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "8c09fdea-45de-4d43-a3d5-4b4687ce5807" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "2a966f19-b988-4cbe-a956-0a31d69457bb"
            Source =
              { Id = "236cb495-5345-445a-aa39-8b39c2233bee"
                PortNumber = None
                PortType = PortType.Output
                HostId = "2c9b6415-3485-4086-8a83-ceec8fdcff32" }
            Target =
              { Id = "3ffb039f-7980-4e5b-872e-141276983c58"
                PortNumber = None
                PortType = PortType.Input
                HostId = "1b25ccc7-9f29-43df-a60c-239924afb313" }
            Vertices =
              [ 1765.75, 1744.78125, false
                1773.75, 1744.78125, false
                1773.75, 1744.78125, false
                1837.75, 1744.78125, false ] }
          { Id = "b8e732db-4f27-4bd9-8880-5f6d1014c93e"
            Source =
              { Id = "8c09fdea-45de-4d43-a3d5-4b4687ce5807"
                PortNumber = None
                PortType = PortType.Output
                HostId = "b4341e9a-c449-4389-9cdf-15f2c841ffc5" }
            Target =
              { Id = "23f4cf44-7b7e-4d58-9e29-471f72ebf141"
                PortNumber = None
                PortType = PortType.Input
                HostId = "2c9b6415-3485-4086-8a83-ceec8fdcff32" }
            Vertices =
              [ 1662.25, 1730.71875, false
                1670.25, 1730.71875, false
                1670.25, 1730.71875, false
                1720.75, 1730.71875, false ] }
          { Id = "f5cf50d1-c6db-4525-bcc0-28a6335897db"
            Source =
              { Id = "477114ad-e47f-4ef2-b995-8b95dbc0f077"
                PortNumber = None
                PortType = PortType.Output
                HostId = "4bbe2223-fd26-4b02-8dd5-b92d4369a549" }
            Target =
              { Id = "857923ad-a2e0-459c-9b8b-070ea95b6dc8"
                PortNumber = None
                PortType = PortType.Input
                HostId = "2c9b6415-3485-4086-8a83-ceec8fdcff32" }
            Vertices =
              [ 1662.25, 1799.28125, false
                1670.25, 1799.28125, false
                1670.25, 1799.28125, false
                1691.5, 1799.28125, false
                1691.5, 1758.84375, false
                1712.75, 1758.84375, false
                1712.75, 1758.84375, false
                1720.75, 1758.84375, false ] } ]),
       "and1")
      (([ { Id = "14f1e4d8-321a-492d-87f6-443b772898e5"
            Type = CounterNoEnableLoad(3)
            Label = "CNT1"
            InputPorts = []
            OutputPorts =
              [ { Id = "1b5522a5-273b-4ae5-8f30-70265aa4c893"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "14f1e4d8-321a-492d-87f6-443b772898e5" } ]
            X = 1626
            Y = 1735
            H = 60
            W = 105
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("1b5522a5-273b-4ae5-8f30-70265aa4c893", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "1b5522a5-273b-4ae5-8f30-70265aa4c893" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "34dd2e03-7657-4ea9-a592-6475db58c998"
            Type = Output(3)
            Label = "A"
            InputPorts =
              [ { Id = "eeb207b9-5086-4f9e-9e2c-d90c2e60eeb9"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "34dd2e03-7657-4ea9-a592-6475db58c998" } ]
            OutputPorts = []
            X = 1814
            Y = 1750
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("eeb207b9-5086-4f9e-9e2c-d90c2e60eeb9", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "eeb207b9-5086-4f9e-9e2c-d90c2e60eeb9" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "cacde389-7e16-46d4-bea8-aafa2cdae91a"
            Source =
              { Id = "1b5522a5-273b-4ae5-8f30-70265aa4c893"
                PortNumber = None
                PortType = PortType.Output
                HostId = "14f1e4d8-321a-492d-87f6-443b772898e5" }
            Target =
              { Id = "eeb207b9-5086-4f9e-9e2c-d90c2e60eeb9"
                PortNumber = None
                PortType = PortType.Input
                HostId = "34dd2e03-7657-4ea9-a592-6475db58c998" }
            Vertices =
              [ 1731, 1765, false; 1739, 1765, false; 1739, 1765, false; 1814, 1765, false ] } ]),
       "cnt3")
      (([ { Id = "01d2eba8-5c30-40b6-aec1-15daffb0d8a9"
            Type = Input1(8, Some 0)
            Label = "A"
            InputPorts = []
            OutputPorts =
              [ { Id = "939a510b-819d-405a-b67e-834a3e605e8c"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "01d2eba8-5c30-40b6-aec1-15daffb0d8a9" } ]
            X = 1286.7359999999999
            Y = 1719.5833333333333
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("939a510b-819d-405a-b67e-834a3e605e8c", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "939a510b-819d-405a-b67e-834a3e605e8c" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "376551d2-9716-45f0-afd0-cac026d11c7f"
            Type = Output(1)
            Label = "B1"
            InputPorts =
              [ { Id = "9e9d46e1-788a-4211-81d8-ce791c1ff23e"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "376551d2-9716-45f0-afd0-cac026d11c7f" } ]
            OutputPorts = []
            X = 1914.8639999999996
            Y = 1650.3599999999997
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("9e9d46e1-788a-4211-81d8-ce791c1ff23e", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "9e9d46e1-788a-4211-81d8-ce791c1ff23e" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "6da1bae5-15a4-4117-b499-4ccd27342fe9"
            Type =
              Custom
                  { Name = "counter8"
                    InputLabels = [ ("START", 8) ]
                    OutputLabels = [ ("COUT", 1); ("RESULT", 8) ]
                    Form = Some User
                    Description = None }
            Label = "COUNTER8.3"
            InputPorts =
              [ { Id = "c74c5278-3555-4d6e-93bf-6a2e68ed50cc"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "6da1bae5-15a4-4117-b499-4ccd27342fe9" } ]
            OutputPorts =
              [ { Id = "943ecac9-0e3d-4743-ae8c-cf3994c5546c"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "6da1bae5-15a4-4117-b499-4ccd27342fe9" }
                { Id = "74b444d8-f806-425d-8779-6f9839a1c083"
                  PortNumber = Some 1
                  PortType = PortType.Output
                  HostId = "6da1bae5-15a4-4117-b499-4ccd27342fe9" } ]
            X = 1909.8483999999999
            Y = 1725.4166666666667
            H = 110
            W = 192.84320000000002
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("74b444d8-f806-425d-8779-6f9839a1c083", Right)
                            ("943ecac9-0e3d-4743-ae8c-cf3994c5546c", Right)
                            ("c74c5278-3555-4d6e-93bf-6a2e68ed50cc", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "c74c5278-3555-4d6e-93bf-6a2e68ed50cc" ])
                            (Right,
                             [ "74b444d8-f806-425d-8779-6f9839a1c083"
                               "943ecac9-0e3d-4743-ae8c-cf3994c5546c" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "88414f2b-fe31-43b7-98ba-f58bc8f2c84c"
            Type = Output(1)
            Label = "B"
            InputPorts =
              [ { Id = "68b25865-84fd-42da-bebc-891be47df47f"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "88414f2b-fe31-43b7-98ba-f58bc8f2c84c" } ]
            OutputPorts = []
            X = 1664.3039999999996
            Y = 1616.6639999999995
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("68b25865-84fd-42da-bebc-891be47df47f", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "68b25865-84fd-42da-bebc-891be47df47f" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "d19456f1-250a-4f00-8fba-786c58e4d49a"
            Type =
              Custom
                  { Name = "counter8"
                    InputLabels = [ ("START", 8) ]
                    OutputLabels = [ ("COUT", 1); ("RESULT", 8) ]
                    Form = Some User
                    Description = None }
            Label = "COUNTER8.1"
            InputPorts =
              [ { Id = "63b1da11-0ab1-4a8f-97d8-9e5182b29d29"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "d19456f1-250a-4f00-8fba-786c58e4d49a" } ]
            OutputPorts =
              [ { Id = "a79a7982-9db7-4359-9672-9a8ef804fa6d"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "d19456f1-250a-4f00-8fba-786c58e4d49a" }
                { Id = "e907f0d6-36db-40f9-9fc8-6f2a77a7f845"
                  PortNumber = Some 1
                  PortType = PortType.Output
                  HostId = "d19456f1-250a-4f00-8fba-786c58e4d49a" } ]
            X = 1389.022
            Y = 1679.5833333333333
            H = 110
            W = 192.84320000000002
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("63b1da11-0ab1-4a8f-97d8-9e5182b29d29", Left)
                            ("a79a7982-9db7-4359-9672-9a8ef804fa6d", Right)
                            ("e907f0d6-36db-40f9-9fc8-6f2a77a7f845", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "63b1da11-0ab1-4a8f-97d8-9e5182b29d29" ])
                            (Right,
                             [ "e907f0d6-36db-40f9-9fc8-6f2a77a7f845"
                               "a79a7982-9db7-4359-9672-9a8ef804fa6d" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "ea485331-2ce7-447f-b86d-4e2a8f481b68"
            Type = Output(1)
            Label = "B2"
            InputPorts =
              [ { Id = "72264b5e-ff78-424b-ab5e-0abb90211244"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "ea485331-2ce7-447f-b86d-4e2a8f481b68" } ]
            OutputPorts = []
            X = 2155.055999999999
            Y = 1673.6879999999996
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("72264b5e-ff78-424b-ab5e-0abb90211244", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "72264b5e-ff78-424b-ab5e-0abb90211244" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "ed573306-e1bd-477c-9962-61ec73c154b0"
            Type =
              Custom
                  { Name = "counter8"
                    InputLabels = [ ("START", 8) ]
                    OutputLabels = [ ("COUT", 1); ("RESULT", 8) ]
                    Form = Some User
                    Description = None }
            Label = "COUNTER8.2"
            InputPorts =
              [ { Id = "5f0a4200-9a0b-4d8d-a588-4e2328b8ad56"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "ed573306-e1bd-477c-9962-61ec73c154b0" } ]
            OutputPorts =
              [ { Id = "324dbbd1-3fe9-46e0-83a3-a8ec4b75f4c8"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "ed573306-e1bd-477c-9962-61ec73c154b0" }
                { Id = "7509b6b7-ebf0-4c78-b873-5f99d84cc019"
                  PortNumber = Some 1
                  PortType = PortType.Output
                  HostId = "ed573306-e1bd-477c-9962-61ec73c154b0" } ]
            X = 1651.9283999999998
            Y = 1702.5
            H = 110
            W = 192.84320000000002
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("324dbbd1-3fe9-46e0-83a3-a8ec4b75f4c8", Right)
                            ("5f0a4200-9a0b-4d8d-a588-4e2328b8ad56", Left)
                            ("7509b6b7-ebf0-4c78-b873-5f99d84cc019", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "5f0a4200-9a0b-4d8d-a588-4e2328b8ad56" ])
                            (Right,
                             [ "7509b6b7-ebf0-4c78-b873-5f99d84cc019"
                               "324dbbd1-3fe9-46e0-83a3-a8ec4b75f4c8" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "f427302a-0c0f-40e9-91b8-9eea74b0c902"
            Type = Output(8)
            Label = "C"
            InputPorts =
              [ { Id = "4cded771-abb9-42de-83c7-b66c62a03572"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "f427302a-0c0f-40e9-91b8-9eea74b0c902" } ]
            OutputPorts = []
            X = 2155.055999999999
            Y = 1788.3333333333335
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("4cded771-abb9-42de-83c7-b66c62a03572", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "4cded771-abb9-42de-83c7-b66c62a03572" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "4afc1aad-24f1-4bc1-a687-c390b6326cb9"
            Source =
              { Id = "74b444d8-f806-425d-8779-6f9839a1c083"
                PortNumber = None
                PortType = PortType.Output
                HostId = "6da1bae5-15a4-4117-b499-4ccd27342fe9" }
            Target =
              { Id = "4cded771-abb9-42de-83c7-b66c62a03572"
                PortNumber = None
                PortType = PortType.Input
                HostId = "f427302a-0c0f-40e9-91b8-9eea74b0c902" }
            Vertices =
              [ 2102.6916, 1803.3333333333335, false
                2110.6916, 1803.3333333333335, false
                2110.6916, 1803.3333333333335, false
                2128.8737999999994, 1803.3333333333335, false
                2128.8737999999994, 1803.3333333333335, false
                2147.055999999999, 1803.3333333333335, false
                2147.055999999999, 1803.3333333333335, false
                2155.055999999999, 1803.3333333333335, false ] }
          { Id = "5680e865-cf48-42a1-8717-f7b12aab912d"
            Source =
              { Id = "324dbbd1-3fe9-46e0-83a3-a8ec4b75f4c8"
                PortNumber = None
                PortType = PortType.Output
                HostId = "ed573306-e1bd-477c-9962-61ec73c154b0" }
            Target =
              { Id = "9e9d46e1-788a-4211-81d8-ce791c1ff23e"
                PortNumber = None
                PortType = PortType.Input
                HostId = "376551d2-9716-45f0-afd0-cac026d11c7f" }
            Vertices =
              [ 1844.7715999999998, 1734.5833333333333, false
                1852.7715999999998, 1734.5833333333333, false
                1852.7715999999998, 1734.5833333333333, false
                1879.8177999999998, 1734.5833333333333, false
                1879.8177999999998, 1665.3599999999997, false
                1906.8639999999996, 1665.3599999999997, false
                1906.8639999999996, 1665.3599999999997, false
                1914.8639999999996, 1665.3599999999997, false ] }
          { Id = "927cdd00-5277-4788-971a-28addec5c1a5"
            Source =
              { Id = "e907f0d6-36db-40f9-9fc8-6f2a77a7f845"
                PortNumber = None
                PortType = PortType.Output
                HostId = "d19456f1-250a-4f00-8fba-786c58e4d49a" }
            Target =
              { Id = "5f0a4200-9a0b-4d8d-a588-4e2328b8ad56"
                PortNumber = None
                PortType = PortType.Input
                HostId = "ed573306-e1bd-477c-9962-61ec73c154b0" }
            Vertices =
              [ 1581.8652, 1757.5, false
                1589.8652, 1757.5, false
                1589.8652, 1757.5, false
                1616.8968, 1757.5, false
                1616.8968, 1757.5, false
                1643.9283999999998, 1757.5, false
                1643.9283999999998, 1757.5, false
                1651.9283999999998, 1757.5, false ] }
          { Id = "ccf06973-121f-4a6f-965e-34e82f3e163a"
            Source =
              { Id = "939a510b-819d-405a-b67e-834a3e605e8c"
                PortNumber = None
                PortType = PortType.Output
                HostId = "01d2eba8-5c30-40b6-aec1-15daffb0d8a9" }
            Target =
              { Id = "63b1da11-0ab1-4a8f-97d8-9e5182b29d29"
                PortNumber = None
                PortType = PortType.Input
                HostId = "d19456f1-250a-4f00-8fba-786c58e4d49a" }
            Vertices =
              [ 1346.7359999999999, 1734.5833333333333, false
                1354.7359999999999, 1734.5833333333333, false
                1354.7359999999999, 1734.5833333333333, false
                1367.879, 1734.5833333333333, false
                1367.879, 1734.5833333333333, false
                1381.022, 1734.5833333333333, false
                1381.022, 1734.5833333333333, false
                1389.022, 1734.5833333333333, false ] }
          { Id = "d83c613c-d0c5-4383-a93d-c1e829784161"
            Source =
              { Id = "7509b6b7-ebf0-4c78-b873-5f99d84cc019"
                PortNumber = None
                PortType = PortType.Output
                HostId = "ed573306-e1bd-477c-9962-61ec73c154b0" }
            Target =
              { Id = "c74c5278-3555-4d6e-93bf-6a2e68ed50cc"
                PortNumber = None
                PortType = PortType.Input
                HostId = "6da1bae5-15a4-4117-b499-4ccd27342fe9" }
            Vertices =
              [ 1844.7715999999998, 1780.4166666666667, false
                1852.7715999999998, 1780.4166666666667, false
                1852.7715999999998, 1780.4166666666667, false
                1877.31, 1780.4166666666667, false
                1877.31, 1780.4166666666667, false
                1901.8483999999999, 1780.4166666666667, false
                1901.8483999999999, 1780.4166666666667, false
                1909.8483999999999, 1780.4166666666667, false ] }
          { Id = "dda8c995-ae6f-46e5-a2b4-f2ab8d17a3dd"
            Source =
              { Id = "943ecac9-0e3d-4743-ae8c-cf3994c5546c"
                PortNumber = None
                PortType = PortType.Output
                HostId = "6da1bae5-15a4-4117-b499-4ccd27342fe9" }
            Target =
              { Id = "72264b5e-ff78-424b-ab5e-0abb90211244"
                PortNumber = None
                PortType = PortType.Input
                HostId = "ea485331-2ce7-447f-b86d-4e2a8f481b68" }
            Vertices =
              [ 2102.6916, 1757.5, false
                2110.6916, 1757.5, false
                2110.6916, 1757.5, false
                2128.8737999999994, 1757.5, false
                2128.8737999999994, 1688.6879999999996, false
                2147.055999999999, 1688.6879999999996, false
                2147.055999999999, 1688.6879999999996, false
                2155.055999999999, 1688.6879999999996, false ] }
          { Id = "fa2927d2-1be5-47e4-acb5-463da8526f17"
            Source =
              { Id = "a79a7982-9db7-4359-9672-9a8ef804fa6d"
                PortNumber = None
                PortType = PortType.Output
                HostId = "d19456f1-250a-4f00-8fba-786c58e4d49a" }
            Target =
              { Id = "68b25865-84fd-42da-bebc-891be47df47f"
                PortNumber = None
                PortType = PortType.Input
                HostId = "88414f2b-fe31-43b7-98ba-f58bc8f2c84c" }
            Vertices =
              [ 1581.8652, 1711.6666666666665, false
                1589.8652, 1711.6666666666665, false
                1589.8652, 1711.6666666666665, false
                1623.0845999999997, 1711.6666666666665, false
                1623.0845999999997, 1631.6639999999995, false
                1656.3039999999996, 1631.6639999999995, false
                1656.3039999999996, 1631.6639999999995, false
                1664.3039999999996, 1631.6639999999995, false ] } ]),
       "counters")
      (([ { Id = "1de65195-2825-4115-9de2-3ba2aac44772"
            Type = Input1(8, Some 0)
            Label = "START"
            InputPorts = []
            OutputPorts =
              [ { Id = "ef532c8d-a6af-44b4-8f31-34da5bda38ee"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "1de65195-2825-4115-9de2-3ba2aac44772" } ]
            X = 1475.7578125
            Y = 1694.875
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("ef532c8d-a6af-44b4-8f31-34da5bda38ee", Right) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [])
                            (Right, [ "ef532c8d-a6af-44b4-8f31-34da5bda38ee" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "511e3014-2569-46be-8e16-2d80f818b334"
            Type = Constant1(1, 1, "1")
            Label = "C1"
            InputPorts = []
            OutputPorts =
              [ { Id = "03bbadf3-b511-42a9-a9e8-8f1c18b60811"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "511e3014-2569-46be-8e16-2d80f818b334" } ]
            X = 1697.7578125
            Y = 1778.375
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree180; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("03bbadf3-b511-42a9-a9e8-8f1c18b60811", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "03bbadf3-b511-42a9-a9e8-8f1c18b60811" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "9a16750c-d548-44d7-bf01-544f44d9a4ec"
            Type = Output(1)
            Label = "COUT"
            InputPorts =
              [ { Id = "09568c88-1eea-4c2e-8181-19b874ae8b9c"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "9a16750c-d548-44d7-bf01-544f44d9a4ec" } ]
            OutputPorts = []
            X = 1757.2578125
            Y = 1694.875
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("09568c88-1eea-4c2e-8181-19b874ae8b9c", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "09568c88-1eea-4c2e-8181-19b874ae8b9c" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "e5829f0d-9673-4eed-9360-f8e7de836944"
            Type = NbitsAdder(8)
            Label = "ADD1"
            InputPorts =
              [ { Id = "dbb244b5-d2e9-471b-837c-c1b3339a6919"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
                { Id = "12429911-c3a8-45bc-8d5d-ac4bb2d7af1e"
                  PortNumber = Some 1
                  PortType = PortType.Input
                  HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
                { Id = "2059ae76-b279-40d0-a837-ed51dadc3c31"
                  PortNumber = Some 2
                  PortType = PortType.Input
                  HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" } ]
            OutputPorts =
              [ { Id = "c172ac43-e6d4-495b-8a43-e402b291d1b6"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
                { Id = "91719aea-09bf-433f-bc07-2449dfd014b1"
                  PortNumber = Some 1
                  PortType = PortType.Output
                  HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" } ]
            X = 1596.2578125
            Y = 1683.625
            H = 90
            W = 120
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation =
                      Map
                          [ ("12429911-c3a8-45bc-8d5d-ac4bb2d7af1e", Left)
                            ("2059ae76-b279-40d0-a837-ed51dadc3c31", Left)
                            ("91719aea-09bf-433f-bc07-2449dfd014b1", Right)
                            ("c172ac43-e6d4-495b-8a43-e402b291d1b6", Right)
                            ("dbb244b5-d2e9-471b-837c-c1b3339a6919", Bottom) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [ "dbb244b5-d2e9-471b-837c-c1b3339a6919" ])
                            (Left,
                             [ "12429911-c3a8-45bc-8d5d-ac4bb2d7af1e"
                               "2059ae76-b279-40d0-a837-ed51dadc3c31" ])
                            (Right,
                             [ "c172ac43-e6d4-495b-8a43-e402b291d1b6"
                               "91719aea-09bf-433f-bc07-2449dfd014b1" ]) ]
                    HScale = None
                    VScale = None } }
          { Id = "ea7316d1-9ef7-4252-844f-9fab1c6d4d0b"
            Type = Output(8)
            Label = "RESULT"
            InputPorts =
              [ { Id = "c9b05d9e-4e3d-4460-a8ed-90576ccd44e8"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "ea7316d1-9ef7-4252-844f-9fab1c6d4d0b" } ]
            OutputPorts = []
            X = 1958.7578125
            Y = 1694.875
            H = 30
            W = 60
            SymbolInfo =
              Some
                  { LabelBoundingBox = None
                    LabelRotation = None
                    STransform = { Rotation = Degree0; flipped = false }
                    ReversedInputPorts = Some false
                    PortOrientation = Map [ ("c9b05d9e-4e3d-4460-a8ed-90576ccd44e8", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "c9b05d9e-4e3d-4460-a8ed-90576ccd44e8" ])
                            (Right, []) ]
                    HScale = None
                    VScale = None } }
          { Id = "f5b7d2f4-de28-40a9-8a1a-db5f9e600297"
            Type = Register(8)
            Label = "REG1"
            InputPorts =
              [ { Id = "debf560a-9a1f-499a-b895-3cb448bd9e95"
                  PortNumber = Some 0
                  PortType = PortType.Input
                  HostId = "f5b7d2f4-de28-40a9-8a1a-db5f9e600297" } ]
            OutputPorts =
              [ { Id = "5fdb18e8-eea8-4c0f-a669-21eef57759f6"
                  PortNumber = Some 0
                  PortType = PortType.Output
                  HostId = "f5b7d2f4-de28-40a9-8a1a-db5f9e600297" } ]
            X = 1853.7578125
            Y = 1759.875
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
                          [ ("5fdb18e8-eea8-4c0f-a669-21eef57759f6", Right)
                            ("debf560a-9a1f-499a-b895-3cb448bd9e95", Left) ]
                    PortOrder =
                      Map
                          [ (Top, [])
                            (Bottom, [])
                            (Left, [ "debf560a-9a1f-499a-b895-3cb448bd9e95" ])
                            (Right, [ "5fdb18e8-eea8-4c0f-a669-21eef57759f6" ]) ]
                    HScale = None
                    VScale = None } } ],
        [ { Id = "60415410-dc18-4e42-9ee5-b1fb7988f874"
            Source =
              { Id = "c172ac43-e6d4-495b-8a43-e402b291d1b6"
                PortNumber = None
                PortType = PortType.Output
                HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
            Target =
              { Id = "debf560a-9a1f-499a-b895-3cb448bd9e95"
                PortNumber = None
                PortType = PortType.Input
                HostId = "f5b7d2f4-de28-40a9-8a1a-db5f9e600297" }
            Vertices =
              [ 1716.2578125, 1747.375, false
                1724.2578125, 1747.375, false
                1724.2578125, 1747.375, false
                1785.0078125, 1747.375, false
                1785.0078125, 1789.875, false
                1845.7578125, 1789.875, false
                1845.7578125, 1789.875, false
                1853.7578125, 1789.875, false ] }
          { Id = "61b1f6e8-a2de-41d5-8fb4-2b7a87be853f"
            Source =
              { Id = "03bbadf3-b511-42a9-a9e8-8f1c18b60811"
                PortNumber = None
                PortType = PortType.Output
                HostId = "511e3014-2569-46be-8e16-2d80f818b334" }
            Target =
              { Id = "dbb244b5-d2e9-471b-837c-c1b3339a6919"
                PortNumber = None
                PortType = PortType.Input
                HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
            Vertices =
              [ 1697.7578125, 1793.375, false
                1689.7578125, 1793.375, false
                1689.7578125, 1793.375, false
                1656.2578125, 1793.375, false
                1656.2578125, 1781.625, false
                1656.2578125, 1781.625, false
                1656.2578125, 1773.625, false ] }
          { Id = "70313e94-a5e9-4001-baa9-4441e3e9b4a2"
            Source =
              { Id = "c172ac43-e6d4-495b-8a43-e402b291d1b6"
                PortNumber = None
                PortType = PortType.Output
                HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
            Target =
              { Id = "c9b05d9e-4e3d-4460-a8ed-90576ccd44e8"
                PortNumber = None
                PortType = PortType.Input
                HostId = "ea7316d1-9ef7-4252-844f-9fab1c6d4d0b" }
            Vertices =
              [ 1716.2578125, 1747.375, false
                1724.2578125, 1747.375, false
                1724.2578125, 1747.375, false
                1837.5078125, 1747.375, false
                1837.5078125, 1709.875, false
                1950.7578125, 1709.875, false
                1950.7578125, 1709.875, false
                1958.7578125, 1709.875, false ] }
          { Id = "9a7ab777-9f35-42a3-9888-dc70d1ae8dfb"
            Source =
              { Id = "5fdb18e8-eea8-4c0f-a669-21eef57759f6"
                PortNumber = None
                PortType = PortType.Output
                HostId = "f5b7d2f4-de28-40a9-8a1a-db5f9e600297" }
            Target =
              { Id = "2059ae76-b279-40d0-a837-ed51dadc3c31"
                PortNumber = None
                PortType = PortType.Input
                HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
            Vertices =
              [ 1973.7578125, 1789.875, false
                1981.7578125, 1789.875, false
                1981.7578125, 1789.875, false
                1991.7578125, 1789.875, false
                1991.7578125, 1846.375, false
                1568.2578125, 1846.375, true
                1568.2578125, 1747.375, true
                1588.2578125, 1747.375, false
                1588.2578125, 1747.375, false
                1596.2578125, 1747.375, false ] }
          { Id = "9acbc780-f0c2-4771-9e76-59f24687263b"
            Source =
              { Id = "ef532c8d-a6af-44b4-8f31-34da5bda38ee"
                PortNumber = None
                PortType = PortType.Output
                HostId = "1de65195-2825-4115-9de2-3ba2aac44772" }
            Target =
              { Id = "12429911-c3a8-45bc-8d5d-ac4bb2d7af1e"
                PortNumber = None
                PortType = PortType.Input
                HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
            Vertices =
              [ 1535.7578125, 1709.875, false
                1543.7578125, 1709.875, false
                1543.7578125, 1709.875, false
                1596.2578125, 1709.875, false ] }
          { Id = "ec71fe60-5146-4d8a-b913-9f0750619862"
            Source =
              { Id = "91719aea-09bf-433f-bc07-2449dfd014b1"
                PortNumber = None
                PortType = PortType.Output
                HostId = "e5829f0d-9673-4eed-9360-f8e7de836944" }
            Target =
              { Id = "09568c88-1eea-4c2e-8181-19b874ae8b9c"
                PortNumber = None
                PortType = PortType.Input
                HostId = "9a16750c-d548-44d7-bf01-544f44d9a4ec" }
            Vertices =
              [ 1716.2578125, 1709.875, false
                1724.2578125, 1709.875, false
                1724.2578125, 1709.875, false
                1757.2578125, 1709.875, false ] } ]),
       "counter8") ]
