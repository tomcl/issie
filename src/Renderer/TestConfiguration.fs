module TestConfiguration

open TestDrawBlockHelpers.SimpleSymbol
open TestAsserts
open CommonTypes
open DrawModelType

let unitTestMetrics: list<(SheetT.Model -> int)> =
    [ countBendsInSheet; countWiresCrossingInSheet ]

let randomTestAsserts : list<(SheetT.Model -> SheetT.Model -> option<string>)> =
    [ failOnBeautifyCausesSymbolOverlap;
      failOnBeautifyIncreasesSegSymIntersect;
      failOnBeautifyDecreasesStraightWires ]

let beautifyFunction (sheetModel: SheetT.Model) : SheetT.Model =
    // Placeholder beautify logic
    sheetModel

let modelsToTest: TestModel list =
    [ 
  { SimpleSymbols = [{ SymLabel = "MUX1";
  CompType = Mux2;
  Position = { X = 1879.145;
  Y = 1787.43 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G2";
  CompType = GateN (And, 2);
  Position = { X = 1767.8549999999998;
  Y = 1855.5 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G1";
  CompType = GateN (And, 2);
  Position = { X = 1613.3549999999998;
  Y = 1674.5 };
  STransform = { Rotation = Degree0;
  Flipped = false } }];
  Connections = [{ Source = { Label = "G2";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 0 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 1 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 1 } }] }]
