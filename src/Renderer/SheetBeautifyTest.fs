module SheetBeautifyTest

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open SheetBeautifyD1
open SheetBeautifyD2
open SheetBeautifyD3
open SheetBeautify
open Optics
open Optics.Operators
open GenerateData
open Elmish
open EEExtensions
open Helpers
open BlockHelpers
open ModelType
open Sheet.SheetInterface
open Symbol
open SymbolUpdate
open SymbolResizeHelpers
open BusWidthInferer
open BusWireSeparate
open RotateScale
open CanvasStateAnalyser

/// constants used by SheetBeautify
module Constants = 
    // () // dummy to make skeleton type check - remove when other content exists
    let wireLabelThreshold = 100.0 

// ------------------------------------ Team work ------------------------------------------
(* 
    This part of the code aims to test the correct usage of labels as described in D3. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more documentation. 
*)

/// dummy function to be tested (to avoid error for now)
let alignSinglyConnectedComponents (model : SheetT.Model) = 
    (model)
let sheetWireLabelSymbol (model : SheetT.Model) = 
    Ok (model) // returns the same model, no change in labels

module T123 =
    open TestDrawBlock.TestLib
    open TestDrawBlock.HLPTick3
    open TestDrawBlock.HLPTick3.Asserts
    open TestDrawBlock.HLPTick3.Builder
    open TestDrawBlock.HLPTick3.Tests

    // More helper functions
    let getWireAndPort (sym : Symbol) (model : SheetT.Model) =
        let portOption =
            mapValues model.Wire.Symbol.Ports
            |> List.tryFind (fun port -> port.HostId = sym.Component.Id) // Get ports on the wire label
        match portOption with
        | Some port ->
            let wireOption =
                mapValues model.Wire.Wires
                |> List.tryFind (fun wire -> wire.OutputPort = OutputPortId port.Id || wire.InputPort = InputPortId port.Id)
            match wireOption with
            | Some wire -> Some (sym, wire, port.Id)
            | None -> None
        | None -> None

    /// Simply count number of wire/labels
    let countPorts (model: SheetT.Model) =
        model.Wire.Wires |> Map.count

    // -------------------------- Test data generation -------------------------------------------
    
    // ------------------------------ T1 -------------------------------------
    let randomXYOFFset (maxX: float) (maxY: float) : XYPos =
        let rnd = System.Random()
        { X = rnd.NextDouble() * maxX; Y = rnd.NextDouble() * maxY }

    let makeTestCircuitBeautify (muxPos:XYPos) =
        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 150.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 280.0} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 280.0} 
        let CPos = { X = muxPos2.X + 150.0; Y = muxPos2.Y + 10.0}

        initSheetModel
        // Place a MUX on the sheet at the specified position.
        |> placeSymbol "MUX1" Mux2 muxPos
        |> Result.bind (placeSymbol "MUX2" Mux2 muxPos2) 
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos) // S1 can be any type of component
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None))BPos) // S1 can be any type of component
        |> Result.bind (placeSymbol "S1" (Input1 (1,None))s1Pos) 
        |> Result.bind (placeSymbol "S2" (Input1 (1,None)) s2Pos) 
        |> Result.bind (placeSymbol "C" (GateN(And, 1)) CPos) 
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "s2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
        |> getOkOrFail
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents
    let makeTestCircuitWithoutBeautify (muxPos:XYPos) =
        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 150.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 280.0} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 280.0} 
        let CPos = { X = muxPos2.X + 150.0; Y = muxPos2.Y + 10.0}

        initSheetModel
        // Place a MUX on the sheet at the specified position.
        |> placeSymbol "MUX1" Mux2 muxPos
        |> Result.bind (placeSymbol "MUX2" Mux2 muxPos2) 
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos) // S1 can be any type of component
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos) // S1 can be any type of component
        |> Result.bind (placeSymbol "S1" (Input1 (1,None)) s1Pos) 
        |> Result.bind (placeSymbol "S2" (Input1 (1,None))s2Pos) 
        |> Result.bind (placeSymbol "C" (GateN(And, 1)) CPos) 
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "s2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
        |> getOkOrFail

    /// A2 (Straightenning should be possible) Random off set to make parallel lines
    let makeA2RandomTestCircuit (muxPos: XYPos) =
        
        let Offset1 = randomXYOFFset 30.0 30.0 
        let Offset2 = randomXYOFFset 30.0 30.0 

        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0 + Offset1.Y}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 150.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 28.0 + Offset1.X} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 28.0 + Offset2.Y} 
        let CPos = { X = muxPos2.X + 150.0; Y = muxPos2.Y + Offset2.X}

        initSheetModel
        // Place a MUX on the sheet at the specified position.
        |> placeSymbol "MUX1" Mux2 muxPos
        |> Result.bind (placeSymbol "MUX2" Mux2 muxPos2) 
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos) // S1 can be any type of component
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos) // S1 can be any type of component
        |> Result.bind (placeSymbol "S1" (Input1 (1,None)) s1Pos) 
        |> Result.bind (placeSymbol "S2" (Input1 (1,None)) s2Pos) 
        |> Result.bind (placeSymbol "C" (GateN(And, 1)) CPos) 
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "s2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
        |> getOkOrFail
        //|> alignSinglyConnectedComponents
        //|> alignSinglyConnectedComponents
    
    let makeA2RandomBeautifyTestCircuit (muxPos: XYPos) =
        let Offset1 = randomXYOFFset 30.0 30.0 
        let Offset2 = randomXYOFFset 30.0 30.0 

        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0 + Offset1.Y}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 150.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 28.0 + Offset1.X} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 28.0 + Offset2.Y} 
        let CPos = { X = muxPos2.X + 150.0; Y = muxPos2.Y + Offset2.X}
        initSheetModel
        // Place a MUX on the sheet at the specified position.
        |> placeSymbol "MUX1" Mux2 muxPos
        |> Result.bind (placeSymbol "MUX2" Mux2 muxPos2) 
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos) // S1 can be any type of component
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos) // S1 can be any type of component
        |> Result.bind (placeSymbol "S1" (Input1 (1,None)) s1Pos) 
        |> Result.bind (placeSymbol "S2" (Input1 (1,None)) s2Pos) 
        |> Result.bind (placeSymbol "C" (GateN(And, 1))CPos) 
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "s2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
        |> getOkOrFail
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents
    let makeA3TestCircuit (muxPos: XYPos) =
        let offset = {X=30; Y=50}
        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0 + offset.X }
        let muxPos3 ={X = muxPos2.X + 150.0; Y = muxPos2.Y + 56.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 28.0 + offset.X} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 28.0 + offset.Y} 
        let CPos = { X = muxPos3.X + 150.0; Y = muxPos3.Y + 0.0}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 180.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        initSheetModel
        |> placeSymbol "MUX1" Mux2 muxPos
        |> Result.bind (placeSymbol "MUX2" Mux2 muxPos2) 
        |> Result.bind (placeSymbol "MUX3" Mux2 muxPos3)
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos) // S1 can be any type of component
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos) // S1 can be any type of component
        |> Result.bind (placeSymbol "S1" (Input1 (1,None)) s1Pos) 
        |> Result.bind (placeSymbol "S2" (Input1 (1,None)) s2Pos) 
        |> Result.bind (placeSymbol "C" (GateN(And, 1))CPos) 
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX3" 1))
        |> Result.bind (placeWire (portOf "s2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 0))
        |> Result.bind (placeWire (portOf "MUX3" 0) (portOf "C" 0))
       // |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "OutputC" 1))
        |> getOkOrFail
        //|> alignSinglyConnectedComponents
        //|> alignSinglyConnectedComponents
    let makeA3BeautifyTestCircuit (muxPos: XYPos) =
        let offset = {X=30; Y=50}
        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0 + offset.X }
        let muxPos3 ={X = muxPos2.X + 150.0; Y = muxPos2.Y + 56.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 28.0 + offset.X} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 28.0 + offset.Y} 
        let CPos = { X = muxPos3.X + 150.0; Y = muxPos3.Y + 0.0}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 180.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        initSheetModel
        |> placeSymbol "MUX1" Mux2 muxPos
        |> Result.bind (placeSymbol "MUX2" Mux2 muxPos2) 
        |> Result.bind (placeSymbol "MUX3" Mux2 muxPos3)
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos) // S1 can be any type of component
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos) // S1 can be any type of component
        |> Result.bind (placeSymbol "S1" (Input1 (1,None)) s1Pos) 
        |> Result.bind (placeSymbol "S2" (Input1 (1,None)) s2Pos) 
        |> Result.bind (placeSymbol "C" (GateN(And, 1))CPos) 
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX3" 1))
        |> Result.bind (placeWire (portOf "s2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 0))
        |> Result.bind (placeWire (portOf "MUX3" 0) (portOf "C" 0))
       // |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "OutputC" 1))
        |> getOkOrFail
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents

    let makeA5BeutifyTestCircuit (muxPos: XYPos) =
        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 28.0} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 28.0} 
        let CPos = { X = muxPos2.X + 150.0; Y = muxPos2.Y + 0.0}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 150.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        initSheetModel
        |> placeSymbol "Decode"Decode4 muxPos
        
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos)
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos)
        |> Result.bind (placeSymbol "OutputC" (GateN(And, 4)) CPos)
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "Decode" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "Decode" 1))
        |> Result.bind (placeWire (portOf "Decode" 0) (portOf "OutputC" 0))
        |> Result.bind (placeWire (portOf "Decode" 1) (portOf "OutputC" 1))
        |> Result.bind (placeWire (portOf "Decode" 2) (portOf "OutputC" 2))
        |> Result.bind (placeWire (portOf "Decode" 3) (portOf "OutputC" 3))
        |> getOkOrFail
        |> alignSinglyConnectedComponents
        |> alignSinglyConnectedComponents
    let makeA5TestCircuit (muxPos: XYPos) =
        let muxPos2 ={X = muxPos.X + 150.0; Y = muxPos.Y + 28.0}
        let APos ={X = muxPos.X - 150.0; Y = muxPos.Y - 28.0} 
        let BPos ={X = muxPos.X - 150.0; Y = muxPos.Y + 28.0} 
        let CPos = { X = muxPos2.X + 150.0; Y = muxPos2.Y + 0.0}
        let s1Pos = {X = muxPos.X - 50.0; Y = muxPos.Y + 150.0}  
        let s2Pos = {X = muxPos.X - 100.0; Y = muxPos.Y + 110.0}
        initSheetModel
        |> placeSymbol "Decode"Decode4 muxPos
        
        |> Result.bind (placeSymbol "InputA" (Input1 (1,None)) APos)
        |> Result.bind (placeSymbol "InputB" (Input1 (1,None)) BPos)
        |> Result.bind (placeSymbol "OutputC" (GateN(And, 4)) CPos)
        |> Result.bind (placeWire (portOf "InputA" 0) (portOf "Decode" 0))
        |> Result.bind (placeWire (portOf "InputB" 0) (portOf "Decode" 1))
        |> Result.bind (placeWire (portOf "Decode" 0) (portOf "OutputC" 0))
        |> Result.bind (placeWire (portOf "Decode" 1) (portOf "OutputC" 1))
        |> Result.bind (placeWire (portOf "Decode" 2) (portOf "OutputC" 2))
        |> Result.bind (placeWire (portOf "Decode" 3) (portOf "OutputC" 3))
  
        |> getOkOrFail

    let makeTestCircuit_0 (andPos:XYPos) = // ?
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    let makeTestCircuit_1 (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) (andPos+{X=1000.;Y=0.})
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    let makeTestCircuit_2 (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "I0" IOLabel (andPos+{X=60.;Y=60.}))
        |> Result.bind (placeSymbol "I1" IOLabel (middleOfSheet+{X=60.;Y=30.}))
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "I0" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "I1" 0))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    let makeTestCircuit_3 (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF (middleOfSheet-{X=0.;Y=100.}))
        |> Result.bind (placeSymbol "FF2" DFF (middleOfSheet))
        |> Result.bind (placeSymbol "FF3" DFF (middleOfSheet+{X=0.;Y=100.}))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF2" 0))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF3" 0))
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    let makeTestCircuit_alt (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF (middleOfSheet-{X=0.;Y=100.}))
        |> Result.bind (placeSymbol "FF2" DFF (middleOfSheet))
        |> Result.bind (placeSymbol "FF3" DFF (middleOfSheet+{X=0.;Y=100.}))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF2" 0))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF3" 0))
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    // ------------------------------------ Assertions -----------------------------------------
    /// Assert functions to use for the testing of D3 task
    /// The dataset used in this test must pass all the assertion in TestDrawBlocks.fs 
    /// 0. No port is connected to more than 1 label
    /// 1. Wire label placement when wire lengths > threshold.
    /// 2. Wire label removal when wire lengths < threshold.
    /// 3. Wire label correct connection between component ports. 
    /// 4. Wire label positioning adjustment to avoid overlaps.
    module Asserts = 

        /// Fails on test number: show certain test case
        let failOnSampleNumber (sampleToFail : int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: show all test cases
        let failOnAllTests (sample: int) _ =
            Some $"Sample {sample}"

        // ------------------------------- T1 --------------------------
        let failOnAllTestsD1 (sample: int) (sheet: SheetT.Model) =
            // let singlyConnectedComponents = findSinglyConnectedComponents sheet
            let singlyConnected = findAlignment sheet
            printfn "singly connected symbol, %A" singlyConnected
            Some <| $"Sample {sample}"
            // Some $"Sample {sample}"

        // ------------------------------- T3 ---------------------------
        /// 0. Each port has no more than 1 label
        ///    and if it has a label, no wire is connected to it
        ///   (for now this is abandoned, could be changed later)
        let failOnMoreThan1Label (sample: int) (model : SheetT.Model) =
            let portLabelCounts =
                model.Wire.Symbol.Ports
                |> Map.toList
                |> List.map (fun (_, port) ->
                    let labelCount =
                        model.Wire.Symbol.Symbols
                        |> Map.filter (fun _ sym -> sym.Component.Type = IOLabel)
                        |> Map.filter (fun _ sym ->
                            match getWireAndPort sym model with
                            | Some (_, _, portId) -> portId = port.Id
                            | None -> false)
                        |> Map.count
                    port.Id, labelCount)

            let failedPorts =
                portLabelCounts
                |> List.filter (fun (_, countLabel) -> countLabel > 1)

            match (List.isEmpty failedPorts) with 
                | true -> None
                | false -> Some $"Illegal circuit: Port connected to more than 1 label."

        /// 1. Check wire -> label placement
        let failOnLabelNotPlaced (sample: int) (model: SheetT.Model) =
            let wiresAboveThreshold =
                mapValues model.Wire.Wires
                |> List.filter (fun wire -> getWireLength wire >= Constants.wireLabelThreshold)

            match (List.isEmpty wiresAboveThreshold) with 
                | true -> None
                | false -> Some $"Wires are still above the threshold length and haven't been replaced by labels."

        /// 2. Check label -> wire removal
        let failOnLabelNotRemoved (sample: int) (model: SheetT.Model) =
            let labelsWithWires =
                mapValues model.Wire.Symbol.Symbols
                |> List.filter (fun sym -> sym.Component.Type = IOLabel)
                |> List.filter (fun sym ->
                    match getWireAndPort sym model with
                    | Some (_, wire, _) -> getWireLength wire < Constants.wireLabelThreshold
                    | None -> false)

            match (List.isEmpty labelsWithWires) with 
                | true -> None
                | false -> Some $"Labels still correspond to wires below the threshold length and haven't been removed."

        /// 3. Check port are connected correctly
        /// (the model should be the same before and after sheet beautify)
        let failOnWrongConnection (sample: int) (model : SheetT.Model) = 
            // assumption: this assert function would be used with "test circuit 3"
            let originalModel = makeTestCircuit_3 middleOfSheet
            let beautifiedModel = makeTestCircuit_3 middleOfSheet // TO CHANGE

            // Get all wire symbols from both models
            let originalWireSymbols =
                originalModel.Wire.Symbol.Symbols
            let beautifiedWireSymbols =
                beautifiedModel.Wire.Symbol.Symbols
                // |> Map.filter (fun _ sym -> sym.Component.Type = ComponentType.?)
            
            // Compare wire symbols between the original and beautified models
            let originalWires =
                originalWireSymbols |> Map.toSeq |> Seq.map snd |> Seq.toList
            let beautifiedWires =
                beautifiedWireSymbols |> Map.toSeq |> Seq.map snd |> Seq.toList

            // Compare wire symbols
            let areEqualWires (wire1: DrawModelType.SymbolT.Symbol) (wire2: DrawModelType.SymbolT.Symbol) =
                wire1.Id = wire2.Id 
            let wrongConnections =
                List.filter (fun origWire ->
                    not (List.exists (fun beautifiedWire -> areEqualWires origWire beautifiedWire) beautifiedWires)) originalWires

            match (List.isEmpty wrongConnections) with 
                | true -> None
                | false -> Some $"Sheetbeautifier changed circuit connection."

        /// 4. Check if wire labels are correctly positioned to avoid overlaps with symbols
        let failOnLabelOverlap (sample: int) (sheet: SheetT.Model) =
            let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            let symbols = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type <> IOLabel)
            let misplacedLabels =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    symbols |> Seq.exists (fun (_, symbol) -> overlap2DBox (getSymBoundingBox symbol) (getSymBoundingBox label))
                )

            match (Seq.isEmpty misplacedLabels) with
                | true -> None
                | false -> Some $"Wire labels are misplaced due to overlap with symbols."

    // ----------------------------------- Test driver -----------------------------------
    /// this is a similar test menu as tick 3
    module Tests = 
        // ------------------------------ T1 ---------------------------
        let test1 testNum firstSample dispatch =
            runTestOnSheets
                "D1 A2 without Beautify test"
                firstSample
                horizLinePositions
                makeTestCircuitWithoutBeautify
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch

        /// 
        let test2 testNum firstSample dispatch =
            runTestOnSheets
                "D1 A2 Beautify"
                firstSample
                horizLinePositions
                makeTestCircuitBeautify
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let test3 testNum firstSample dispatch =
            runTestOnSheets
                "D1 A3 without Beutify test "
                firstSample
                horizLinePositions
                makeA3TestCircuit
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail all tests
        let test4 testNum firstSample dispatch =
            runTestOnSheets
                "D1 A3 Beautify"
                firstSample
                horizLinePositions
                makeA3BeautifyTestCircuit
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch
        let makeA2TestCase testNum firstSample dispatch =
            runTestOnSheets
                "D1 A2 without ramdom"
                firstSample
                horizLinePositions
                makeA2RandomTestCircuit
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch

        let makeA2BeautifyTestCase testNum firstSample dispatch =
            runTestOnSheets
                "D1 A2 ramdom"
                firstSample
                horizLinePositions
                makeA2RandomBeautifyTestCircuit
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch
        let makeA5TestCase testNum firstSample dispatch =
            runTestOnSheets
                "D1 A5 without"
                firstSample
                horizLinePositions
                makeA5TestCircuit
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch
        let makeA5BeautifyTestCase testNum firstSample dispatch =
            runTestOnSheets
                "D1 A5 beautify"
                firstSample
                horizLinePositions
                makeA5BeutifyTestCircuit
                Asserts.failOnAllTestsD1
                dispatch
            |> recordPositionInTest testNum dispatch

        // -------------------------- T3 ------------------------------
        let testLabelNumber testNum firstSample dispatch = 
            runTestOnSheets
                "SheetBeautifyT3: fail when port connected to more than 1 label"
                firstSample
                horizLinePositions
                makeTestCircuit_2
                Asserts.failOnMoreThan1Label
                dispatch
            |> recordPositionInTest testNum dispatch 

        let testWireToLabel testNum firstSample dispatch = 
            runTestOnSheets
                "SheetBeautifyT3: fail when label not added"
                firstSample
                horizLinePositions
                makeTestCircuit_1
                Asserts.failOnLabelNotPlaced
                dispatch
            |> recordPositionInTest testNum dispatch 

        let testLabelToWire testNum firstSample dispatch = 
            runTestOnSheets
                    "SheetBeautifyT3: fail when label not removed"
                    firstSample
                    horizLinePositions
                    makeTestCircuit_2
                    Asserts.failOnLabelNotRemoved
                    dispatch
                |> recordPositionInTest testNum dispatch 

        let testConnection testNum firstSample dispatch = 
            runTestOnSheets
                    "SheetBeautifyT3: fail when connection info not maintained"
                    firstSample
                    horizLinePositions
                    makeTestCircuit_3
                    Asserts.failOnWrongConnection
                    dispatch
                |> recordPositionInTest testNum dispatch 
        
        let testOverlap testNum firstSample dispatch = 
            runTestOnSheets
                    "SheetBeautifyT3: fail when components overlaps"
                    firstSample
                    horizLinePositions
                    makeTestCircuit_3
                    Asserts.failOnLabelOverlap
                    dispatch
                |> recordPositionInTest testNum dispatch 
                
        let testSheetWireLabelSymbol testNum firstSample dispatch = 
            runTestOnSheets
                    "SheetBeautifyT3: fail on all tests"
                    firstSample
                    horizLinePositions
                    makeTestCircuit_alt
                    Asserts.failOnAllTests
                    dispatch
                |> recordPositionInTest testNum dispatch 

        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            [   
                // ------------------------ t1 -----------------
                "D1 A2 without", test1 
                "D1 A2 beautify", test2 
                "D1 A3 without", test3 
                "D1 A3 beautify", test4 
                "D1 A2 random ", makeA2TestCase
                "D1 A2 beautify random", makeA2BeautifyTestCase
                "D1 A5", makeA5TestCase
                "D1 A5 beautify", makeA5BeautifyTestCase
                // ------------------------ t3 -------------------
                "T3 test 0 : No port connected to more than 1 label", testLabelNumber // always PASS
                "T3 test 1 : Wire to label", testWireToLabel
                "T3 test 2 : Label to wire", testLabelToWire
                "T3 test 3 : Connection check", testConnection
                "T3 test 4 : Overlap check", testOverlap
                "(Dummy Test : FailAllTests)", testSheetWireLabelSymbol
            ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | _ ->
                func testIndex 0 dispatch

