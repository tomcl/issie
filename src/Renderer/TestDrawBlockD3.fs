module TestDrawBlockD3

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open TestDrawBlock
open GenerateData
open Elmish
open BlockHelpers
open ModelType

module constants =

    (*
    A few metrics are designed to define possible characteristics of wires that should be replaced by 
    wire labels, and characteristic of wire Labels that should be changed back to wires. The specific 
    values are temporary and should be determined later. 

    Median is used to avoid the influence of outliers. 
    
    TODO:identify suitable values through tests or create logic for the values to be user assigned during runtime
    TODO: confirm which order the metrics should be satified if they conflict, currently designed as the greater 
        the line number, the more important the metric is.
    *)
             

    /// If the length of a wire is greater than this ratio of the median length of all wires,
    /// then the wire is considered too long.
    let maxLengthRatio = 2.5

    /// If the intersection of a wire with all other wires is greater than this ratio of the median
    /// intersection of all wires, then the wire is considered too intersecting.
    let maxIntersectionRatio = 4.0

    ///If the distance between two wire label of the same wire connection is less than this ratio of the median
    /// length of all wires, then the wire labels are considered too close and unecesarry.
    let minLabelDistanceRatio = 1

    /// If the number of wire bends is greater than this ratio of the median number of bends of all wires,
    /// then the wire is considered too bendy.
    let maxBendRatio = 4.0

module TestWireLabelReplaceHelpers =

    /// get median of a List
    let median (list: float list) =
        let sortedList = List.sort list
        let length = List.length sortedList
        if length % 2 = 0 then
            (sortedList.[length / 2 - 1] + sortedList.[length / 2]) / 2.0
        else
            sortedList.[length / 2]
    let getWireLength (wire: BusWireT.Wire) =
        wire
        |> getNonZeroAbsSegments
        |> List.map (fun segment -> segment.Segment.Length)
        |> List.sum

    /// Get the number of wire labels on the sheet
    let getnumberWireLabels(sheetModel: SheetT.Model): int =
        sheetModel.Wire.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (Id, symbol) -> symbol)
        |> List.filter (fun symbol -> symbol.Component.Type = ComponentType.IOLabel)
        |> List.length

    /// Get the median length of all wires
    let getMedianLength(sheetModel: SheetT.Model): float =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map (fun (id, wire) -> getWireLength wire)
        |> median
    
    /// Get the median number of intersections of all wires
    let getMedianIntersection(sheetModel: SheetT.Model): int =
        failwith "Not implemented"

    /// Get the median number of bends across wires
    let getMedianBend(sheetModel: SheetT.Model): int =
        failwith "Not implemented"

    /// get number of wires that are too long
    let getNumberTooLong(sheetModel: SheetT.Model) (medianLength: float): int =
        sheetModel.Wire.Wires
        |> Map.toList
        |> List.map (fun (id, wire) -> getWireLength wire)
        |> List.filter (fun length -> length > constants.maxLengthRatio * medianLength)
        |> List.length
    
    /// get number of wires that are too intersecting
    let getNumberTooIntersecting(sheetModel: SheetT.Model) (medianIntersection: int): int =
        failwith "Not implemented"

    /// get number of wire labels that are too close
    let getNumberTooClose(sheetModel: SheetT.Model) (medianLength: float): int =
        failwith "Not implemented"

    /// get number of wires that are too bendy
    let getNumberTooBendy(sheetModel: SheetT.Model) (medianBend: int): int =
        failwith "Not implemented"

    /// get number of bit legends overlapping symbols or wires
    let getNumberOverlappingBitLegends(sheetModel: SheetT.Model): int =
        failwith "Not implemented"

module TestWireLabelReplace =

    /// the output type of the test
    type TestWireLabelReplaceMetrics = {
        //basic metrics
        /// the total number of wires on the sheet
        numberWires: int
        /// the total number of wire labels on the sheet
        numberWireLabels: int
        /// the total number of wire Bends on the sheet
        numberWireBends: int

        //hard metrics which should not be violated
        /// number of pairs of intersecting symbols
        numberIntersectingSymbols: int
        
        //informational metrics that can be useful to know for the result of the implementation
        /// the median length of all wires
        medianLength: float
        /// the median number of intersections of all wires
        medianIntersection: int
        /// the median number of bends of all wires
        medianBend: int
        /// the number of wires that are too long
        numberTooLong: int
        /// the number of wires that are too intersecting
        numberTooIntersecting: int
        /// the number of wire labels that are too close
        numberTooClose: int
        /// the number of wires that are too bendy
        numberTooBendy: int
        /// number of bit legends overlapping symbols or wires
        numberOverlappingBitLegends: int
    }


    /// Calculate the metrics of the sheet
    let getTestWireLableReplaceMetrics (sheetModel: SheetT.Model): TestWireLabelReplaceMetrics =
        {
            numberWires = sheetModel.Wire.Wires |> Map.count
            numberWireLabels = TestWireLabelReplaceHelpers.getnumberWireLabels sheetModel
            numberWireBends = numberWireRightAngles sheetModel
            numberIntersectingSymbols = numberWireSegmentsIntersectSymbol sheetModel
            medianLength = TestWireLabelReplaceHelpers.getMedianLength sheetModel
            medianIntersection = TestWireLabelReplaceHelpers.getMedianIntersection sheetModel
            medianBend = TestWireLabelReplaceHelpers.getMedianBend sheetModel
            numberTooLong = TestWireLabelReplaceHelpers.getNumberTooLong sheetModel (TestWireLabelReplaceHelpers.getMedianLength sheetModel)
            numberTooIntersecting = TestWireLabelReplaceHelpers.getNumberTooIntersecting sheetModel (TestWireLabelReplaceHelpers.getMedianIntersection sheetModel)
            numberTooClose = TestWireLabelReplaceHelpers.getNumberTooClose sheetModel (TestWireLabelReplaceHelpers.getMedianLength sheetModel)
            numberTooBendy = TestWireLabelReplaceHelpers.getNumberTooBendy sheetModel (TestWireLabelReplaceHelpers.getMedianBend sheetModel)
            numberOverlappingBitLegends = TestWireLabelReplaceHelpers.getNumberOverlappingBitLegends sheetModel
        }
    

    /// Module involving Creating the initial Sheet that contain a 1to4demux, a 2to1mux, and a 4to1mux, different mux used to account for
    /// different number of wires and hence affect the metrics. Used to test the wire label replace logic
     (* TODO: possibly replace this by a more general case that can build sheet with more general components*)
    module muxTestBuilder =

        /// Create the initial Sheet contain demux and mux based on positions given
        let makeTestWireLableReplaceSheet (mux2Pos: XYPos) (mux4Pos: XYPos): SheetT.Model =
            DiagramMainView.init().Sheet
            |> HLPTick3.Builder.placeSymbol "demux" ComponentType.Demux4 HLPTick3.middleOfSheet
            |> Result.bind (fun sheet -> HLPTick3.Builder.placeSymbol "mux2" ComponentType.Mux2 mux2Pos sheet)
            |> Result.bind (fun sheet -> HLPTick3.Builder.placeSymbol "mux4" ComponentType.Mux4 mux4Pos sheet)
            // TODO: do wirebindings
            |> TestLib.getOkOrFail

        /// Create random Positions for the muxes
        let makeRandomMuxPositions (repeats: int) (maxPosRange: float) (minPosRangez:float): (XYPos * XYPos) list =
            failwith "Not implemented"

        /// Create regular Positions for the muxes
        let makeRegularMuxPositions (stepSize: float) (maxPosRange: float) (minPosRange:float): (XYPos * XYPos) list =
            failwith "Not implemented"
