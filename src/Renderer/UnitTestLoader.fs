module Renderer.UnitTestLoader

open TestDrawBlockSimpleSymbol.SimpleSymbolTesting
open TestAsserts
open CommonTypes
open DrawModelType

let testMetrics: list<(SheetT.Model -> int)> =
    [countBendsInSheet; countWiresCrossingInSheet]
let modelsToTest: TestModel list =
    [ { SimpleSymbols =
          [ { SymLabel = "G1"
              CompType = GateN(And, 2)
              Position = { X = 1638.105; Y = 1671.75 }
              STransform = { Rotation = Degree0; Flipped = false } }
            { SymLabel = "G2"
              CompType = GateN(And, 2)
              Position = { X = 1874.605; Y = 1858.25 }
              STransform = { Rotation = Degree0; Flipped = false } }
            { SymLabel = "MUX1"
              CompType = Mux2
              Position = { X = 1632.895; Y = 1780.25 }
              STransform = { Rotation = Degree0; Flipped = false } } ]
        Connections =
          [ { Source = { Label = "MUX1"; PortNumber = 0 }
              Target = { Label = "G2"; PortNumber = 0 } }
            { Source = { Label = "G1"; PortNumber = 0 }
              Target = { Label = "G2"; PortNumber = 1 } } ] }
      { SimpleSymbols =
          [ { SymLabel = "MUX1"
              CompType = Mux2
              Position = { X = 1595.5774438476562; Y = 1801.0249633789062 }
              STransform = { Rotation = Degree0; Flipped = false } }
            { SymLabel = "G2"
              CompType = GateN(And, 2)
              Position = { X = 1834.7874438476563; Y = 1886.0249633789062 }
              STransform = { Rotation = Degree0; Flipped = false } }
            { SymLabel = "ADD1"
              CompType = NbitsAdder(3)
              Position = { X = 1844.4225561523438; Y = 1666.4750366210938 }
              STransform = { Rotation = Degree0; Flipped = false } }
            { SymLabel = "G1"
              CompType = GateN(And, 2)
              Position = { X = 1600.7874438476563; Y = 1692.5249633789062 }
              STransform = { Rotation = Degree0; Flipped = false } } ]
        Connections =
          [ { Source = { Label = "ADD1"; PortNumber = 1 }
              Target = { Label = "MUX1"; PortNumber = 2 } }
            { Source = { Label = "MUX1"; PortNumber = 0 }
              Target = { Label = "ADD1"; PortNumber = 1 } }
            { Source = { Label = "MUX1"; PortNumber = 0 }
              Target = { Label = "G2"; PortNumber = 0 } }
            { Source = { Label = "G1"; PortNumber = 0 }
              Target = { Label = "G2"; PortNumber = 1 } } ] } ]

let beautifyFunction (sheetModel: SheetT.Model) : SheetT.Model =
    // Placeholder beautify logic
    sheetModel
