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
  Position = { X = 1632.97703125;
  Y = 1780.25 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G1";
  CompType = GateN (And, 2);
  Position = { X = 1638.18703125;
  Y = 1671.75 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G2";
  CompType = GateN (And, 2);
  Position = { X = 1874.68703125;
  Y = 1858.25 };
  STransform = { Rotation = Degree0;
  Flipped = false } }];
  Connections = [{ Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 1 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 0 } }] };
  { SimpleSymbols = [{ SymLabel = "G1";
  CompType = GateN (And, 2);
  Position = { X = 1600.8694750976563;
  Y = 1692.5249633789062 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "ADD1";
  CompType = NbitsAdder 3;
  Position = { X = 1844.5045874023438;
  Y = 1666.4750366210938 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "G2";
  CompType = GateN (And, 2);
  Position = { X = 1834.8694750976563;
  Y = 1886.0249633789062 };
  STransform = { Rotation = Degree0;
  Flipped = false } }; { SymLabel = "MUX1";
  CompType = Mux2;
  Position = { X = 1595.6594750976562;
  Y = 1801.0249633789062 };
  STransform = { Rotation = Degree0;
  Flipped = false } }];
  Connections = [{ Source = { Label = "G1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 1 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "ADD1";
  PortNumber = 1 } }; { Source = { Label = "MUX1";
  PortNumber = 0 };
  Target = { Label = "G2";
  PortNumber = 0 } }; { Source = { Label = "ADD1";
  PortNumber = 1 };
  Target = { Label = "MUX1";
  PortNumber = 2 } }] } 
  { SimpleSymbols = [{ SymLabel = "COMP5";
  CompType = GateN (Nand, 3);
  Position = { X = 1243.67578125;
  Y = 2223.1694183591067 };
  STransform = { Rotation = Degree270;
  Flipped = true } }; { SymLabel = "COMP6";
  CompType = GateN (Xor, 2);
  Position = { X = 2143.67578125;
  Y = 1288.0805816408931 };
  STransform = { Rotation = Degree90;
  Flipped = true } }; { SymLabel = "COMP7";
  CompType = DFFE;
  Position = { X = 1343.67578125;
  Y = 1891.7743448362396 };
  STransform = { Rotation = Degree180;
  Flipped = false } }; { SymLabel = "COMP3";
  CompType = Mux2;
  Position = { X = 1243.67578125;
  Y = 1932.7117277958323 };
  STransform = { Rotation = Degree90;
  Flipped = false } }; { SymLabel = "COMP10";
  CompType = GateN (Nor, 4);
  Position = { X = 1943.67578125;
  Y = 1369.6780953926436 };
  STransform = { Rotation = Degree90;
  Flipped = false } }; { SymLabel = "COMP0";
  CompType = GateN (Or, 3);
  Position = { X = 1543.67578125;
  Y = 1373.639255091821 };
  STransform = { Rotation = Degree90;
  Flipped = true } }; { SymLabel = "COMP1";
  CompType = DFFE;
  Position = { X = 1943.67578125;
  Y = 2057.0938704714 };
  STransform = { Rotation = Degree0;
  Flipped = true } }; { SymLabel = "COMP11";
  CompType = GateN (Xor, 4);
  Position = { X = 1943.67578125;
  Y = 1647.6441264922098 };
  STransform = { Rotation = Degree180;
  Flipped = true } }; { SymLabel = "COMP8";
  CompType = GateN (And, 4);
  Position = { X = 2043.67578125;
  Y = 2064.0840699194814 };
  STransform = { Rotation = Degree180;
  Flipped = false } }; { SymLabel = "COMP2";
  CompType = GateN (Nor, 3);
  Position = { X = 1543.67578125;
  Y = 1967.6940229871634 };
  STransform = { Rotation = Degree180;
  Flipped = false } }; { SymLabel = "COMP4";
  CompType = GateN (Xnor, 3);
  Position = { X = 1643.67578125;
  Y = 1477.4347847828142 };
  STransform = { Rotation = Degree90;
  Flipped = false } }; { SymLabel = "COMP9";
  CompType = Mux4;
  Position = { X = 2143.67578125;
  Y = 1742.4183139736995 };
  STransform = { Rotation = Degree270;
  Flipped = true } }];
  Connections = [{ Source = { Label = "COMP0";
  PortNumber = 0 };
  Target = { Label = "COMP1";
  PortNumber = 0 } }; { Source = { Label = "COMP2";
  PortNumber = 0 };
  Target = { Label = "COMP3";
  PortNumber = 1 } }; { Source = { Label = "COMP4";
  PortNumber = 0 };
  Target = { Label = "COMP5";
  PortNumber = 1 } }; { Source = { Label = "COMP9";
  PortNumber = 0 };
  Target = { Label = "COMP10";
  PortNumber = 1 } }; { Source = { Label = "COMP5";
  PortNumber = 0 };
  Target = { Label = "COMP6";
  PortNumber = 1 } }; { Source = { Label = "COMP7";
  PortNumber = 0 };
  Target = { Label = "COMP8";
  PortNumber = 2 } }; { Source = { Label = "COMP6";
  PortNumber = 0 };
  Target = { Label = "COMP7";
  PortNumber = 0 } }; { Source = { Label = "COMP3";
  PortNumber = 0 };
  Target = { Label = "COMP4";
  PortNumber = 0 } }; { Source = { Label = "COMP8";
  PortNumber = 0 };
  Target = { Label = "COMP9";
  PortNumber = 3 } }; { Source = { Label = "COMP1";
  PortNumber = 0 };
  Target = { Label = "COMP2";
  PortNumber = 1 } }; { Source = { Label = "COMP10";
  PortNumber = 0 };
  Target = { Label = "COMP11";
  PortNumber = 2 } }] }]
