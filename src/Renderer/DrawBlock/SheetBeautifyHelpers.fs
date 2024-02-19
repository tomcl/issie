module SheetBeautifyHelpers

open CommonTypes
open DrawModelType

/// key: B1R Type: R Descrip: The dimensions of a custom component symbol
let getCustomSymbolDimensions (sym: SymbolT.Symbol) :  float * float =
    match sym.HScale with
    | Some hScale -> 
        match sym.VScale with
        | Some vScale -> (hScale, vScale)
        | None -> (hScale, 0.0)
    | None -> failwith "cannot get dimensions of a non-custom symbol"

/// key: B1W Type: W Descrip: The dimensions of a custom component symbol
let writeCustomSymbolDimensions (sym: SymbolT.Symbol)(dimensions: float * float) :  SymbolT.Symbol =
    {sym with HScale = Some (fst dimensions); VScale = Some (snd dimensions)}

/// key: B2W Type: W Descrip: The position of a symbol on the sheet
let writeSymbolPositionOnSheet (pos: XYPos) (symId: ComponentId) (sheetModel: SheetT.Model) : SheetT.Model  =
    let symModel = sheetModel.Wire.Symbol.Symbols[symId]
    let newSymModel = {symModel with Pos = pos}
    {sheetModel with Wire.Symbol.Symbols = sheetModel.Wire.Symbol.Symbols.Add(symId, newSymModel)}

/// key: B3R Type: R Descrip: Read the order of ports on a specified side of a symbol
let getPortOrder (model:SheetT.Model) :  XYPos =
    failwith "Not Implemented"

/// key: B3W Type: W Descrip: Write the order of ports on a specified side of a symbol
let writePortOrder (model:SheetT.Model) :  XYPos =
    failwith "Not Implemented"

/// key: B4R Type: R Descrip: The reverses state of the inputs of a MUX2
let getReverseStateOfInputsMux2 (model:SheetT.Model) :  XYPos=
    failwith "Not Implemented"

/// key: B4W Type: W Descrip: The reverses state of the inputs of a MUX2
let writeReverseStateOfInputsMux2 (model:SheetT.Model) :  XYPos=
    failwith "Not Implemented"

/// key: B5R type: R Descrip: The position of a port on the sheet. It cannot directly be written
let getPortPositionOnSheet (connectionId: ConnectionId) (portId:PortId) (model:SheetT.Model) :  XYPos =
    failwith "Not Implemented"

/// key: B6R Type: R Descrip: The Bounding box of a symbol outline (position is contained in this)
let getSymbolBoudingboxOutline (model:SheetT.Model) :  XYPos=
    failwith "Not Implemented"

/// key: B7R Type: R Descrip: The rotation state of a symbol
let getSymbolRotateState (sym: SymbolT.Symbol) : Rotation=
    sym.STransform.Rotation

/// key: B7W Type: W Descrip: The rotation state of a symbol
let writeSymbolRotateState (rotation: Rotation) (sym: SymbolT.Symbol) : SymbolT.Symbol=
    {sym with STransform = {sym.STransform with Rotation = rotation}}

/// key: B8R Type: R Descrip: The flip state of a symbol
let getSymbolFlipState (sym: SymbolT.Symbol) : bool =
    sym.STransform.Flipped

/// key: B8W Type: W Descrip: The flip state of a symbol
let writeSymbolFlipState (sym: SymbolT.Symbol) (filp: bool) :  SymbolT.Symbol=
    {sym with STransform = {sym.STransform with Flipped = filp}}

/// key: T1R Type: R Descrip: The number of pairs of symbols that intersect each other. See Tick3 for a related function.
/// Count over all pairs of symbols.
/// key: T2R Type: R Descrip: The number of distinct wire visible segments that intersect with one or more symbols.
/// See Tick3.HLPTick3.visibleSegments for a helper. Count over all visible wire segments.
