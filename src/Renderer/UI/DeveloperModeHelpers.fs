module DeveloperModeHelpers

open EEExtensions
open VerilogTypes
open Fulma
open Fulma.Extensions.Wikiki

open Fable.React
open Fable.React.Props
open JSHelpers

open DrawModelType
open CommonTypes
open DrawModelType.SheetT
open DrawModelType.BusWireT
open Optics
open Helpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire
open BusWireUpdateHelpers
open ModelType
open BusWireRoutingHelpers
open EEExtensions
open Symbol
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.SheetT
open BusWireRoute
open BusWireRoutingHelpers.Constants
open Sheet

// Any functions labelled "INTERIM" are temporary and will be replaced with proper implementations of helpers in another file




// --------------------------------------------------- //
//                DeveloperMode Helpers                //
// --------------------------------------------------- //


// -------- Mouse-Sensitive Data------------ //
/// function that returns the an string ID with extra formatting of a hovered wire, symbol, or ports
let findHoveredID (pos: XYPos) (model: SheetT.Model) =
    let dummySymbolId: ComponentId = ComponentId "dummy"
    // we add a 'dummy symbol' to the model to represent the mouse position (cursor)
    // solely for calculation purposes, it will not be added to the actual model
    // for convenience, we let dummy symbol be 30x30, equal to a Not gate size
    let h, w = 30.0, 30.0
    let mouseComponentDummy =
        { Id = "dummy"
          Type = Not
          Label = "dummy"
          InputPorts = List.empty
          OutputPorts = List.empty
          X = pos.X - float w / 2.0
          Y = pos.Y - float h / 2.0
          H = float h
          W = float w
          SymbolInfo = None }

    // create a mouse dummy symbol, find its bounding box, add it to a dummy model
    let mouseSymbolDummy: Symbol =
        { (createNewSymbol [] pos NotConnected "" White) with
            Component = mouseComponentDummy }

    // Lens to get and set bounding boxes in model
    let boundingBoxes_ =
        Lens.create (fun m -> m.BoundingBoxes) (fun bb m -> { m with BoundingBoxes = bb })

    // create a dummy model with the mouse dummy symbol
    let dummyModel =
        model
        |> Optic.set (SheetT.symbols_) (Map.add dummySymbolId mouseSymbolDummy model.Wire.Symbol.Symbols)
        // SheetUpdateHelpers has not implemented updateBoundingBoxes yet on master
        |> Optic.set boundingBoxes_ (Symbol.getBoundingBoxes model.Wire.Symbol)
        |> Optic.map symbols_ (Map.map (fun _ sym -> Symbol.calcLabelBoundingBox sym))
    // we calculate the bounding box of the mouse
    let mouseBoundingBox = getSymbolBoundingBox mouseSymbolDummy

    // inspired by SheetBeautifyD1's findAllBoundingBoxesOfSymIntersections
    let intersectingWiresInfo =
        dummyModel.Wire.Wires
        |> Map.values
        // findWireSymbolIntersections returns a list of bounding boxes of symbols intersected by wire.
        // we find the wires that have a boundingBox in their intersection list that contains our mouseBoundingBox
        // we might get more than one wire – so get a list

        |> Seq.map (fun wire -> (wire, (findWireSymbolIntersections dummyModel.Wire wire)))
        |> Seq.choose (fun (wire, bboxes) ->
            if
                bboxes
                |> List.exists (fun box ->

                    // findWireSymbolIntersections returns bounding boxes that have been enlarged with minWireSeparation
                    // we correct this
                    let correctedBox =
                        { W = box.W - minWireSeparation * 2.
                          H = box.H - minWireSeparation * 2.
                          TopLeft =
                            box.TopLeft
                            |> updatePos Right_ minWireSeparation
                            |> updatePos Down_ minWireSeparation }
                    mouseBoundingBox =~ correctedBox)
            then
                Some(wire.WId.ToString())

            else
                None)
        |> Seq.toList
        |> List.tryHead

    // inspired by SheetBeautifyD1's findAllBoundingBoxesOfSymIntersections
    let intersectingSymbolInfo =
        model.BoundingBoxes
        |> Map.toList
        // get all boundingBoxes in model not equal to symbolBoundingBox, see if they overlap with symbolBoundingBox, if yes, return compId
        |> List.filter (fun (compId, box) -> not (box =~ mouseBoundingBox))
        |> List.choose (fun (compId, box) ->
            match (overlap2DBoxInfo mouseBoundingBox box) with
            | Some area -> Some(compId.ToString())
            | None -> None)
        |> List.tryHead

    // inpisred by Sheet.mouseOn
    // priority: check for mouse over ports first, then symbols, then wires
    // the code for checking for mouse over ports is the same as in Sheet.mouseOn
    // otherwise symbol and wire mouseover is calculated based on intersection with mouseBoundingBox
    match intersectingWiresInfo, intersectingSymbolInfo with
    | _, Some symbolId ->
        let inputPorts, outputPorts =
            Symbol.getPortLocations model.Wire.Symbol [ ComponentId symbolId ]
            |> fun (x, y) -> Map.toList x, Map.toList y
        match mouseOnPort inputPorts pos 2.5 with
        | Some(portId, portLoc) -> "InputPort: ", portId.ToString()
        | None ->
            match mouseOnPort outputPorts pos 2.5 with
            | Some(portId, portLoc) -> "OutputPort: ", portId.ToString()
            | None -> "Symbol: ", symbolId.ToString()
    | Some wireId, _ -> "Wire: ", wireId.ToString()
    | _ -> "Component: ", "Nothing Selected"


//-----------Symbols-----------//

// A helper printing function that returns a string of the symbol's component type description
let getComponentTypeDescrFromSym (symbol : SymbolT.Symbol)  =
    match symbol.Component.Type with
    | Input1 _ -> "Input1"
    | Output _ -> "Output"
    | Viewer _ -> "Viewer"
    | IOLabel -> "IOLabel"
    | NotConnected -> "NotConnected"
    | BusCompare1 _ -> "BusCompare1"
    | BusSelection _ -> "BusSelection"
    | Constant1 _ -> "Constant1"
    | Not -> "Not"
    | Decode4 -> "Decode4"
    | GateN _ -> "GateN"
    | Mux2 -> "Mux2"
    | Mux4 -> "Mux4"
    | Mux8 -> "Mux8"
    | Demux2 -> "Demux2"
    | Demux4 -> "Demux4"
    | Demux8 -> "Demux8"
    | NbitsAdder _ -> "NbitsAdder"
    | NbitsAdderNoCin _ -> "NbitsAdderNoCin"
    | NbitsAdderNoCout _ -> "NbitsAdderNoCout"
    | NbitsAdderNoCinCout _ -> "NbitsAdderNoCinCout"
    | NbitsXor _ -> "NbitsXor"
    | NbitsAnd _ -> "NbitsAnd"
    | NbitsNot _ -> "NbitsNot"
    | NbitsOr _ -> "NbitsOr"
    | NbitSpreader _ -> "NbitSpreader"
    | Custom customDetails -> $"Custom {customDetails.Name.ToUpper()}"
    | MergeWires -> "MergeWires"
    | SplitWire _ -> "SplitWire"
    | MergeN _ -> "MergeN"
    | SplitN _ -> "SplitN"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | Register _ -> "Register"
    | RegisterE _ -> "RegisterE"
    | Counter _ -> "Counter"
    | CounterNoLoad _ -> "CounterNoLoad"
    | CounterNoEnable _ -> "CounterNoEnable"
    | CounterNoEnableLoad _ -> "CounterNoEnableLoad"
    | AsyncROM1 _ -> "AsyncROM1"
    | ROM1 _ -> "ROM1"
    | RAM1 _ -> "RAM1"
    | AsyncRAM1 _ -> "Async RAM"
    | AsyncROM _ -> "AsyncROM"
    | ROM _ -> "ROM"
    | RAM _ -> "RAM"
    | Shift _ -> "Shift"
    | BusCompare _ -> "BusCompare"
    | Input _ -> "Input"
    | Constant _ -> "Constant"

/// Function to programmatically generate a html table from PortMaps.Order
let createTableFromPortMapsOrder (map: Map<Edge, string list>) =
    Table.table
        []
        (map
         |> Map.toList
         |> List.map (fun (edge, strList) ->
             tr
                 []
                 [ td [ Style [ FontWeight "Bold" ] ] [ str (edge.ToString()) ]
                   td
                       []
                       (strList
                        |> List.collect (fun s -> [ code [] [ str ("• " + s) ]; br [] ])) ]))

/// Function to programmatically generate a html table from a Map PortMaps.Oritentation
let createTableFromPorts (portsMap: Map<string, Edge>) (symbol: Symbol) =
    let referencePortTable =
        // get a list of ports from the selected component. more efficient to search smaller list
        // than looking of ports in model.Sheet.Wire.Symbol.Symbols
        symbol.Component.InputPorts
        @ symbol.Component.OutputPorts
        |> List.map (fun port -> port.Id, port)
        |> Map.ofList
    let portDetailMap =
        portsMap
        |> Map.map (fun key _ -> Map.tryFind key referencePortTable)
        |> Map.filter (fun _ value -> value.IsSome)
        |> Map.map (fun _ value -> value.Value)
    let tableRows =
        portDetailMap
        |> Map.toList
        |> List.sortBy (fun (_,port) -> (port.PortType, port.PortNumber))
        |> List.map (fun (key, port) ->
            tr
                []
                [ td [] [ code [] [ str port.Id ] ]
                  td
                      []
                      [ str (
                            match port.PortNumber with
                            | Some num -> num.ToString()
                            | None -> "N/A"
                        ) ]
                  td
                      []
                      [ str (
                            match port.PortType with
                            | CommonTypes.PortType.Input -> "In"
                            | CommonTypes.PortType.Output -> "Out"
                        ) ]
                  td [] [ code [] [ str port.HostId ] ] ])
    Table.table
        []
        [ tr
              []
              [ th [] [ str "Port Id" ]
                th [] [ str "No." ]
                th [] [ str "I/O" ]
                th [] [ str "Host Id" ] ]
          yield! tableRows ]



//---------- Wire ---------//


/// Function to programmatically generate a html table from a list of wire segments
let createTableFromASegments (segments: ASegment list) =
    Table.table
        []
        [ tr
              []
              [ th [] [ str "Len" ]
                th [] [ str "Start" ]
                th [] [ str "End" ]
                th [] [ str "Drag?" ]
                th [] [ str "Route?" ] ]
          yield!
              segments
              |> List.map (fun seg ->
                  tr
                      []
                      [ td [] [ str (sprintf "%.1f" seg.Segment.Length) ]
                        td [] [ str (sprintf "%.1f, %.1f" seg.Start.X seg.Start.Y) ]
                        td [] [ str (sprintf "%.1f, %.1f" seg.End.X seg.End.Y) ]

                        td
                            []
                            [ str (
                                  if seg.Segment.Draggable then
                                      "T"
                                  else
                                      "F"
                              ) ]
                        td
                            []
                            [ str (
                                  match seg.Segment.Mode with
                                  | Manual -> "M"
                                  | Auto -> "A"
                              ) ] ]) ]








// --------------------------------------------------- //
//                      Constants                      //
// --------------------------------------------------- //

module Constants =
    /// Constant that decides if a wire is classified as almost-straight, if its longest segment in the minority direction is shorter than this length
    let bucketSpacing = 0.1





