module Model


//------------------------------------------------------------------------//
//------------------------------Sheet Types-------------------------------//
//------------------------------------------------------------------------//

// other state needed for sheet: 
//  selected components and/or wires
//  highlighted components and/or wires
//  state needed during drag-and-connect wire addition 

type SheetMT = {
    NumberOfCircles: int
    NumberOfWires: int   
    }


//-----------------------------Model Type----------------------------------//

type Model = {
    SymbolMT: SymbolModel
    WireMT: WireModel
    SheetMT: SheetModel
    }

type Msg =
    | SymbolMT of SymbolMsg
    | WireMT of BusWireMsg
    | SheetMT of Sheet

