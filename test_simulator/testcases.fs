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
       "cnt3") ]
