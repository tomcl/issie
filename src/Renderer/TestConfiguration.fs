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
  PortNumber = 1 } }] };
  
  { SimpleSymbols = [{ SymLabel = "MUX1";
  CompType = Mux2;
  Position = { X = 1880.395;
  Y = 1729.93 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G2";
  CompType = GateN (And, 2);
  Position = { X = 1728.1049999999998;
  Y = 1855.5 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G1";
  CompType = GateN (And, 2);
  Position = { X = 1573.6049999999998;
  Y = 1674.5 };
  STransform = { Rotation = Degree0;
  Flipped = false } }];
  Connections = [{ Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 2 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 0 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 1 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 1 } }; { Source = { Label = "G2";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 0 } }] };
  { SimpleSymbols = [{ SymLabel = "G2";
  CompType = GateN (And, 2);
  Position = { X = 1732.355;
  Y = 1855.5 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G1";
  CompType = GateN (And, 2);
  Position = { X = 1577.855;
  Y = 1674.5 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "MUX2";
  CompType = Mux2;
  Position = { X = 1711.440185546875;
  Y = 1593.00244140625 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "MUX1";
  CompType = Mux2;
  Position = { X = 1871.1450000000002;
  Y = 1714.93 };
  STransform = { Rotation = Degree0;
  Flipped = false } }];
  Connections = [{ Source = { Label = "G2";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 0 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 1 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "MUX2";
  PortNumber = 1 } }; { Source = { Label = "MUX2";
  PortNumber = 0 };
  Target = { Label = "G1";
  PortNumber = 1 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 1 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 0 } }; { Source = { Label = "G2";
  PortNumber = 0 };
  Target = { Label = "MUX2";
  PortNumber = 2 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "MUX2";
  PortNumber = 0 } }; { Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "MUX1";
  PortNumber = 2 } }] };
  
  { SimpleSymbols = [{ SymLabel = "T";
  CompType = Input1 (1, Some 0);
  Position = { X = 1505.3890380859375;
  Y = 1827.813720703125 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G1";
  CompType = GateN (And, 2);
  Position = { X = 2002.1109619140625;
  Y = 1659.50244140625 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "T1";
  CompType = Input1 (1, Some 0);
  Position = { X = 1507.3890380859375;
  Y = 1915.813720703125 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "MUX2";
  CompType = Mux2;
  Position = { X = 1822.3890380859375;
  Y = 1830.813720703125 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "MUX1";
  CompType = Mux2;
  Position = { X = 1400.8890380859375;
  Y = 1548.186279296875 };
  STransform = { Rotation = Degree0;
  Flipped = false } }];
  Connections = [{ Source = { Label = "T";
  PortNumber = 0 };
  Target = { Label = "MUX2";
  PortNumber = 1 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "MUX2";
  PortNumber = 2 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "G1";
  PortNumber = 1 } }; { Source = { Label = "MUX2";
  PortNumber = 0 };
  Target = { Label = "G1";
  PortNumber = 0 } }; { Source = { Label = "T1";
  PortNumber = 0 };
  Target = { Label = "MUX2";
  PortNumber = 0 } }] }]
