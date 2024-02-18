module SheetBeautifyHelpers

    open DrawModelType
    open CommonTypes
    open Symbol
    open RotateScale
    open Optics
    open Operators
    open SymbolT
    open BlockHelpers
    open BusWireRoute
    open BusWireRoutingHelpers

    // TODO: T4 redo T4 because the implementation DOES NOT seem correct.

    /// Function that returns a lens that gets and sets a particular symbol on the sheet depending on
    /// the symbol's ID.
    ///
    /// symId - The ID of the symbol that needs it's lens to/from the sheet model extracted.
    let symbolSheet_ (symId: ComponentId) = SheetT.symbol_ >-> symbolOf_ symId

    /// Lens getting and setting Stranform (record containing rotation and flip state data) from/to a symbol.
    let STransformSymbol_ = Lens.create (fun sym -> sym.STransform) (fun sTrans sym -> {sym with STransform = sTrans})

    /// Lens getting and setting the component from/to a symbol.
    let componentSymbol_ = Lens.create (fun sym -> sym.Component) (fun comp sym -> {sym with Component = comp})



    /// B1 contains B1R and B1W, getters and setters for dimensions of custom components.
    module B1 =

        /// Lens getting and setting dimensions (height and width) from/to a component.
        let dimsComponent_ = Lens.create (fun comp -> (comp.H, comp.W)) (fun (h, w) comp -> {comp with H = h; W = w})

        /// Lens getting and setting dimensions (height and width) from/to a symbol.
        let dimsSymbol_ = componentSymbol_ >-> dimsComponent_

        /// Creates a lens getting and setting dimensions (height and width tuple) from/to the sheet.
        /// Helpful in updating the sheet after the dimensions are written to. Private since
        /// it is raw read/write, without any intermediary bounding boxes update.
        ///
        /// symId - The ID of the symbol that needs it's lens to/from the sheet model extracted.
        let private dimsHWTup_ (symId: ComponentId) = symbolSheet_ symId >-> dimsSymbol_


        /// Get dimensions in the form of an anonymous record (containing height and width information)
        /// from a symbol.
        ///
        /// model - The sheet model.
        /// 
        /// symbol - The symbol whose dimensions are to be read.
        ///
        /// returns: Record containing H and W. H is the height, W is the width.
        let readDimsCustomComp (model: SheetT.Model) (symbol: Symbol) =
            match symbol.Component.Type with
            | Custom _ ->
                let height, width = Optic.get (dimsHWTup_ symbol.Id) model
                {|H = height; W = width|}
            | _ ->
                printf "Not a custom component. Do not use readDimsCustomComp"
                {|H = 0.0; W = 0.0|}


        /// Set dimensions in the form of an anonymous record (so that arguments are not swapped while setting)
        /// to the sheet and updates all bounding boxes after the change.
        ///
        /// dims - Anonymous record with fields H: height and W: width.
        ///
        /// model - The sheet model which takes the updated symbol and updates its own symbol map through a lens.
        ///
        /// symbol - Symbol to be updated.
        let writeDimsCustomComp (dims: {|H: float; W: float|}) (model: SheetT.Model) (symbol: Symbol) =
            match symbol.Component.Type with
            | Custom _ ->
                Optic.set (dimsHWTup_ symbol.Id) (dims.H, dims.W) model
                |> SheetUpdateHelpers.updateBoundingBoxes // optimize
            | _ ->
                printf "Not a custom component. Do not use writeDimsCustomComp"
                model


        /// Creates a lens getting and setting dimensions (height and width anonymous record) from/to the sheet.
        /// The record takes the form {|H: height, W: width|}.
        ///
        /// symId - The ID of the symbol that needs it's lens to/from the sheet model extracted.
        let dimsHW_ (symbol: Symbol) = Lens.create (fun model -> readDimsCustomComp model symbol) (fun dims model -> writeDimsCustomComp dims model symbol)


        /// Extract scaled heights and widths from the symbol, returned as an anonymous record.
        ///
        /// symbol - The symbol whose scaled dimensions are to be read.
        ///
        /// returns: Anonymous record with fields scaledH: the height after scaling is applied and scaledW: the width after scaling is applied.
        let scaledDimsCustomComp (symbol: Symbol) (model: SheetT.Model) =
            let dims =
                symbol
                |> readDimsCustomComp model

            match symbol.HScale, symbol.VScale with
            | Some hScale, Some vScale -> {| scaledH = dims.H * vScale; scaledW = dims.W * hScale |}
            | None, Some vScale -> {| scaledH = dims.H * vScale; scaledW = dims.W |}
            | Some hScale, _ ->  {| scaledH = dims.H ; scaledW = dims.W * hScale |}
            | _ -> {| scaledH = dims.H; scaledW = dims.W |}



    /// B2 contains B2W, sets a new position for the symbol and updates the sheet.
    module B2 =
        open SymbolUpdate

        /// Lens getting and setting position data from/to a symbol.
        let posSymbol_: Lens<Symbol, XYPos> = Lens.create (fun sym -> sym.Pos) (fun pos sym -> {sym with Pos = pos})

        /// Creates a lens getting and setting position data of a symbol from/to the sheet directly.
        /// This is a convenience lens, and is not used in the write, because a bounds check
        /// needs to be performed to ensure the symbol is within the canvas.
        ///
        /// symId - The ID of the symbol that needs it's lens to/from the sheet model extracted.
        let pos_ (symId: ComponentId) = symbolSheet_ symId >-> posSymbol_


        /// Updates a symbol's location on the sheet.
        ///
        /// symbol - The symbol whose position needs to be changed.
        ///
        /// newPos - The new position of the symbol.
        ///
        /// model - The sheet model which takes the updated symbol and updates its own symbol map through a lens.
        let writeSymbolPosition (symbol: Symbol) (newPos: XYPos) (model: SheetT.Model) =
            let updateFunc = Optic.set posSymbol_ newPos
            let symModel = Optic.get SheetT.symbol_ model

            match newPos + symbol.getScaledDiagonal with
            | {X=x;Y=y} when x > Sheet.Constants.defaultCanvasSize || y > Sheet.Constants.defaultCanvasSize ->
                Error $"Symbol '{symbol.Component.Label}' position {newPos + symbol.getScaledDiagonal} lies outside allowed coordinates"
            | _ -> 
                let newSymbol = updateSymbol updateFunc symbol.Id symModel
                Optic.set SheetT.symbol_ newSymbol model
                |> SheetUpdateHelpers.updateBoundingBoxes // optimize
                |> Ok



    /// B3 contains B3R and B3W, getters and setters for port order on a specified side of a symbol
    module B3 =

        /// Lens getting/setting Port Order map from/to PortMaps of a symbol
        let orderPortMaps_ = Lens.create (fun pMaps -> pMaps.Order) (fun order pMaps -> {pMaps with Order = order})

        /// Lens getting/setting PortMaps from/to a symbol
        let portMapsSymbol_ = Lens.create (fun sym -> sym.PortMaps) (fun pMaps sym -> {sym with PortMaps = pMaps})

        // Lens getting/setting Port Order map from/to a symbol
        let private portOrderSymbol_ = portMapsSymbol_ >-> orderPortMaps_


        /// Based on the side, returns the port order from the port
        /// order map of a symbol. If there are no ports, returns an
        /// empty list.
        ///
        /// side - The side on which to read ports.
        ///
        /// symbol - Symbol whose ports need to be read.
        let readPortOrder (side: Edge) (symbol: Symbol) =
            Optic.get portOrderSymbol_ symbol
            |> Map.tryFind side
            |> Option.defaultValue []


        /// Given a new port order, changes the existing order of a particular side
        /// on the symbol specified.
        ///
        /// side - The side on which to update ports.
        ///
        /// newOrder - The new order of ports specified.
        ///
        /// symbol - Symbol on which this update is to be made.
        let writePortOrder (side: Edge) (newOrder: string list) (symbol: Symbol) =
            let newOrder =
                Optic.get portOrderSymbol_ symbol
                |> Map.add side newOrder

            Optic.set portOrderSymbol_ newOrder symbol


        /// Creates a lens getting/setting the Port Order map for a particular specified side based on symbol Id from/to
        /// the sheet directly. Utilizes the functions above.
        ///
        /// side - The edge at which said read/write operation needs to be performed.
        ///
        /// symId - The ID of the symbol that needs it's lens to/from the sheet model extracted.
        let sidePortOrder_ (side: Edge) (symId: ComponentId) = symbolSheet_ symId >-> Lens.create (readPortOrder side) (writePortOrder side)



    /// B4 contains B4R and B4W, getters and setters for reversed state of a mux2 component
    module B4 =
        /// Lens getting/setting the state of the reversed input ports of a symbol.
        let private reversedInputPortsSymbol_ = Lens.create (fun sym -> sym.ReversedInputPorts) (fun toggledPorts sym -> {sym with ReversedInputPorts = toggledPorts})


        /// Reads the reversed input port state only for a Mux2 component.
        /// None values are for legacy component and are treated as false to comply
        /// with the rest of the codebase.
        ///
        /// symbol - Mux2 symbol whose state is to be checked.
        let readReversedMUX2 (symbol: Symbol) =
            match symbol.Component.Type with
            | Mux2 ->
                Optic.get reversedInputPortsSymbol_ symbol
                |> Option.defaultValue false
            | _ ->
                printf $"Given symbol: {symbol.Component.Label} is not of type Mux2."
                false


        /// Toggles the reversed input port state for a Mux2 component.
        ///
        /// toggle - Toggles the reversed state if true.
        ///
        /// symbol - Symbol whose reversed state needs to be updated.
        let writeReversedMUX2 (toggle: bool) (symbol: Symbol) =
            let inputPortsState = Optic.get reversedInputPortsSymbol_ symbol

            let newInputPortsState =
                match inputPortsState with
                | Some state -> Some (state <> toggle)
                | None -> Some toggle

            match symbol.Component.Type with
            | Mux2 -> 
                Optic.set reversedInputPortsSymbol_ newInputPortsState symbol
            | _ ->
                printf $"Given symbol: {symbol.Component.Label} is not of type Mux2."
                symbol

        /// Creates a lens getting/setting the reversed input state for a Mux2 directly from the sheet
        /// based on the symbol Id.
        ///
        /// symId - The ID of the symbol that needs it's lens to/from the sheet model extracted.
        let reversedInputPortsMux2_ (symId: ComponentId) = symbolSheet_ symId >-> Lens.create readReversedMUX2 writeReversedMUX2 



    /// B5 contains B5R, which reads the position of a port on the sheet. 
    module B5 =

        /// Gets position of a port on the sheet.
        ///
        /// port - Port whose position needs to be fetched.
        ///
        /// model - The sheet model from which to fetch said information.
        let getPortPositionOnSheet (port: Port) (model: SheetT.Model) =
            getPortLocation None (Optic.get SheetT.symbol_ model) port.Id



    /// B6 contains B6R, getter for bounding boxes of a symbol.
    module B6 =

        /// Gets the bounding boxes for a given symbol from the sheet model
        ///
        /// model - The sheet model. Used to verify symbol is on the sheet.
        ///
        /// symbol - Symbol whose bounding boxes need to be fetched.
        let getSymbolBB (model: SheetT.Model) (symbol: Symbol) =
            Optic.get SheetT.symbols_ model
            |> Map.tryFind symbol.Id
            |> Option.map getSymbolBoundingBox
            |> Option.defaultWith (fun _ ->
                                        printf $"Given symbol: {symbol.Component.Label} does not exist on sheet."
                                        symbol.SymbolBoundingBox)



    /// B7 contains B7R and B7W, getters and setters for rotation state of a symbol.
    module B7 =

        /// Lens getting/setting rotation from/to STransform
        let rotationSTransform_ = Lens.create (fun sTrans -> sTrans.Rotation) (fun rot sTrans -> {sTrans with Rotation = rot})

        /// Lens getting/setting rotation directly from/to a symbol
        let private rotationSymbol_ = STransformSymbol_ >-> rotationSTransform_


        /// Given a symbol, reads its rotation state
        ///
        /// symbol - Symbol whose rotation state needs to be read.
        let readRotationState (symbol: Symbol) =
            Optic.get rotationSymbol_ symbol


        /// Given a rotation type and a symbol writes the rotation state to the symbol
        ///
        /// rotation - The rotation state to be written to the symbol
        ///
        /// symbol - The symbol whose rotation state needs to be changed.
        let writeRotationState (rotation: Rotation) (symbol: Symbol) =
            Optic.set rotationSymbol_ rotation symbol


        /// Given a symbol ID, outputs a lens directly able to get and set rotation state
        /// from the sheet.
        ///
        /// symID - ID of the symbol
        let rotation_ (symId: ComponentId) = symbolSheet_ symId >-> Lens.create readRotationState writeRotationState

        /// Rotates a symbol by the specified amount
        let rotateSymbol (rotation: Rotation) (symbol: Symbol) (sheet: SheetT.Model): SheetT.Model =
            let symCentre = getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
            let symMap = sheet.Wire.Symbol.Symbols
            let symId = ComponentId symbol.Component.Id

            let rotatedSymbol =
                symMap
                |> Map.tryFind symId
                |> Option.map (fun sym -> rotateSymbolInBlock rotation symCentre sym) // The symbol is the only one in the block

            match rotatedSymbol with
            | Some (sym) ->
                let newSymbols =
                    (symId, sym)
                    |> symMap.Add
                Optic.set SheetT.symbols_ newSymbols sheet
                |> SheetUpdateHelpers.updateBoundingBoxes // Need to recalculate bounding boxes because rotation changes them
            | None ->
                printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
                sheet



    /// B8 contains B8R and B8W, getters and setter of the flip state of a symbol.
    module B8 =

        /// Lens getting/setting flip state from/to STransform
        let flipSTransform_ = Lens.create (fun sTrans -> sTrans.Flipped) (fun flip sTrans -> {sTrans with Flipped = flip})

        /// Lens getting/setting flip state directly from/to a symbol
        let private flipSymbol_ = STransformSymbol_ >-> flipSTransform_


        /// Given a symbol, reads its flip state
        ///
        /// symbol - Symbol whose flip state needs to be identified.
        let readFlipState (symbol: Symbol) =
            Optic.get flipSymbol_ symbol

        /// Given a flip state, and a symbol writes the new flip state to the symbol
        ///
        /// flip - The new flip state
        ///
        /// symbol - The symbol whose flip state needs to be changed.
        let writeFlipState (flip: bool) (symbol: Symbol) =
            Optic.set flipSymbol_ flip symbol


        /// Function returning a lens getting/setting flip state of a given symbol ID
        /// directly from/to the sheet.
        ///
        /// symId - The symbol ID whose flip state needs to be changed.
        let flip_ (symId: ComponentId) = symbolSheet_ symId >-> Lens.create readFlipState writeFlipState


        /// Given a symbol on a sheet and its flip orientation, flips the symbol.
        ///
        /// symbol - The symbol the be flipped
        ///
        /// flip - The new flip orientation
        ///
        /// sheet - The sheet model on which symbol flip state needs to be changed.
        let flipSymbol (symbol: Symbol) (flip: FlipType) (sheet: SheetT.Model): SheetT.Model =
            let symCentre = getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
            let symMap = sheet.Wire.Symbol.Symbols
            let symId = ComponentId symbol.Component.Id

            let flippedSymbol =
                symMap
                |> Map.tryFind symId
                |> Option.map (fun sym -> flipSymbolInBlock flip symCentre sym) // The symbol is the only one in the block

            match flippedSymbol with
            | Some (sym) ->
                let newSymbols =
                    (symId, sym)
                    |> symMap.Add
                Optic.set SheetT.symbols_ newSymbols sheet
            | None ->
                printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
                sheet

//*------------------------------------------------------ TESTING HELPERS ------------------------------------------------------------ *//

    // Note that this function was given to us as a helper in HLPTick3 module of TestDrawBlock. It has been copied here for convenience.

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
    let private visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
            then
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)


    /// Helper function that gets a list of the start and end positions of all visible segments of
    /// a wire on the sheet given its connection ID.
    ///
    /// wId - The connection ID of the wire.
    ///
    /// model - The sheet on which elements lie.
    let getStartFinishVisSeg (model: SheetT.Model) (wId: ConnectionId) =

        let wire =  model.Wire.Wires[wId]
        // Since visibleSegments returns just one XYPos which corresponds to
        // the XYPos of the end of the segment, to construct a list of start and
        // end coordinates it is necessary to start from the source end and add this
        // offset

        (wire.StartPos, visibleSegments wId model)
        ||> List.scan (fun start finish -> start + finish) // Length of this list will be segments + 1 because initial state is counted too.
        |> List.pairwise // [(startPos, startPos + visSeg1); (startPos + visSeg1; startPos + visSeg2 + visSeg1);....]


    /// Checks if wire is horizontal or vertical. Also returns the coordinate that does not change
    /// along the wire (static coordinate).
    ///
    /// startPos, endPos - The starting and end points for the wire.
    let private wireOrientation ((startPos, endPos): XYPos * XYPos)=
        if abs(startPos.X - endPos.X) <= 0.05 // tolerance needed for FP calculations - This would ideally be 0.
        then startPos.X, BusWireT.Vertical
        else startPos.Y, BusWireT.Horizontal // The static coordinate can be used to confirm 1D overlap, and perpendicular intersections, as used below.


    /// Returns the start and end coordinates of all visible segments on the sheet.
    ///
    /// model - The sheet to be checked.
    let private globalWireVisSegments (model: SheetT.Model) =
        model.Wire.Wires
        |> Map.toList
        |> List.map fst
        |> List.collect (getStartFinishVisSeg model)
        |> List.distinct // to exclude the net case.



    module T1 =
        /// Returns the number of pairs of symbols on a sheet that intersect.
        ///
        /// model - The sheet model that is to be checked.
        let intersectingSymbolPairs (model: SheetT.Model) =
            let boundingBoxList = Map.toList model.BoundingBoxes
            List.allPairs boundingBoxList boundingBoxList
            |> List.collect (fun ((cid1, bbox1), (cid2, bbox2)) ->
                                if (cid1 <> cid2) && BlockHelpers.overlap2DBox bbox1 bbox2
                                then [(cid1, cid2)]
                                else [])
            |> List.length
            |> fun pairs -> pairs/2



    module T2 =

        // Adapted based on BusWireRoute.findWireSymbolIntersections. A simpler function would have involved just using
        // segmentIntersectsBoundingBox from BlockHelpers, but this would leave the mux2 bug that's pointed out in that file.
        // Not sure how relevant it is, but this function adapts the working function from Tick 3 and changes it to its use case.

        /// Given a sheet, returns the number of visible segments of wires that intersect with at least one
        /// symbol.
        let visSegsIntersectSymbol (model: SheetT.Model) =
            let allSymbolsIntersected =
                model.Wire.Symbol.Symbols
                |> Map.values
                |> Seq.toList
                |> List.filter (fun s -> s.Annotation = None)
                |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))

            /// Uses the helper defined above to extract the start and finish coordinates from
            /// a list of positions denoting the relative coordinates of the visible segments in a wire.
            ///
            /// wId - The ID corresponding to the wire needed to be extracted.
            let segVertices (wId: ConnectionId) =
                getStartFinishVisSeg model wId
                |> List.mapi (fun i (start, finish) -> (i, (start, finish)))

            // Rest of the function till the end is more or less unchanged

            let componentIsMux (comp:Component) =
                match comp.Type with
                | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> true
                | _ -> false

            let inputIsSelect (wId: ConnectionId) =
                let wire = model.Wire.Wires[wId]
                let inputCompId = model.Wire.Symbol.Ports[string wire.InputPort].HostId
                let inputSymbol = model.Wire.Symbol.Symbols[ComponentId inputCompId]
                let inputCompInPorts = inputSymbol.Component.InputPorts
        
                componentIsMux inputSymbol.Component && (inputCompInPorts[List.length inputCompInPorts - 1].Id = string wire.InputPort)

            let boxesIntersectedBySegment (lastSeg:bool) startPos endPos =
                allSymbolsIntersected
                |> List.map (fun (compType, boundingBox) ->
                    (
                        compType,
                        {
                            W = boundingBox.W + Constants.minWireSeparation * 2.
                            H = boundingBox.H + Constants.minWireSeparation * 2.
                            TopLeft =
                            boundingBox.TopLeft
                            |> BusWireRoutingHelpers.updatePos Left_ Constants.minWireSeparation
                            |> BusWireRoutingHelpers.updatePos Up_ Constants.minWireSeparation
                        }
                    ))
                |> List.filter (fun (compType, boundingBox) ->
                    // don't check if the final segments of a wire that connects to a MUX SEL port intersect with the MUX bounding box
                    match compType, lastSeg with
                    | Mux2, true | Mux4, true | Mux8, true | Demux2, true | Demux4, true | Demux8, true -> false
                    | _, _ ->
                         match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
                         | Some _ -> true // segment intersects bounding box
                         | None -> false // no intersection
                )
                |> List.map (fun (compType, boundingBox) -> boundingBox)

            /// Count the segment symbol intersections for a particular wire.
            ///
            /// Uses segVertices which, based on the wire ID is the list of start
            /// and end positions for all of the visible segments of that wire.
            let segmentSymbolIntersections (wId: ConnectionId) =
                let visibleSegments = segVertices wId // only visible segments
                visibleSegments
                |> List.collect (fun (i, (startPos, endPos)) -> boxesIntersectedBySegment (i > List.length (visibleSegments) - 2 && inputIsSelect wId) startPos endPos)
                |> List.distinct

            // For each wire, extract the number of distinct symbols it intersects.
            model.Wire.Wires
            |> Map.toList
            |> List.map fst
            |> List.fold (fun count wId -> if List.length (segmentSymbolIntersections wId) <> 0 then count + 1 else count) 0 // increments by 1 if there are ANY intersections, or else keep the count as it is.



    module T3 =
        /// For a given pair of segments, returns a boolean indicating whether they are perpendicular or not.
        let checkPerpendicularCondition (firstSegment: XYPos * XYPos) (secondSegment: XYPos * XYPos) =
            let firstStart, firstEnd = firstSegment
            let secondStart, secondEnd = secondSegment

            // For a vertical segment, it needs to have it's static x coordinate between the corresponding
            // horizontal segment's start and end x coordinates in the perpendicularity check. There is a similar case,
            // where the static y coordinate of the horizontal segment also needs to be between the corresponding
            // start and end y coordinates of the vertical segment. These conditions are checked below.

            match wireOrientation firstSegment, wireOrientation secondSegment with
            | (xCoord, BusWireT.Vertical), (yCoord, BusWireT.Horizontal) ->
                (xCoord < secondEnd.X && xCoord > secondStart.X) && (yCoord < firstEnd.Y && yCoord > firstStart.Y)
            | (yCoord, BusWireT.Horizontal), (xCoord, BusWireT.Vertical) ->
                (xCoord < firstEnd.X && xCoord > firstStart.X) && (yCoord < secondEnd.Y && yCoord > secondStart.Y)
            | _ -> false


        let perpendicularSegmentsSheet (model: SheetT.Model) =
            let allSegments = globalWireVisSegments model

            List.allPairs allSegments allSegments
            |> List.fold (fun count (firstSeg, secondSeg) -> if checkPerpendicularCondition firstSeg secondSeg then count + 1 else count) 0
            |> fun count -> count / 2



    module T4 =

        // BlockHelpers.getSourcePort

        // Can you maybe fold over all of the wires from the same source port? I think that will work. Try it

        // I think you need to fold with [] and populate the list as you go.

        // Since this implementation seems wrong, you need to check for the input and output ports
        // for same "net" wires - to ensure there is no overlap. SAME INPUT SOURCE PORTS!!!!

        /// Given a model, returns a map containing all the source ports and the visible segments
        /// connected with the source port.
        ///
        /// model - The sheet on which to perform the check.
        let sourcePortVisSegs (model: SheetT.Model) =

            /// Given a connection Id and a wire, adds the source port to all of the
            /// list elements signifying the start and end coordinates of the visible segments
            /// of a wire.
            ///
            /// wId - The wire ID
            ///
            /// wire - The wire whose visible segment vertices and source port need to be determined.
            let addSourcePortToVisSegs (wId: ConnectionId) (wire: BusWireT.Wire) =

                let sourcePort = BlockHelpers.getSourcePort model.Wire wire

                // Uses the previously defined function that given a list of ends of visible segments,
                // uses the start position of the wire to populate the start and end vertices of the wire.
                getStartFinishVisSeg model wId
                |> List.map (fun startFinishCoords -> (sourcePort, startFinishCoords))

            model.Wire.Wires
            |> Map.toList
            |> List.collect (fun (wId, wire) -> addSourcePortToVisSegs wId wire)
            |> List.groupBy fst // groups a tuple containing (port, start, end) by the port - for same net case
            |> Map.ofList
            |> Map.map (fun _port _portCoordsList -> List.map snd _portCoordsList ) // since map is of type {port: (port, start, end) list}


        /// Given two segments, checks if there is any overlap between them,
        /// and returns a list of the longer of the two if there is.
        ///
        /// firstSegment - The first of the two segments to be compared.
        ///
        /// secondSegment - The second segment.
        let segmentsOvelap (firstSegment: XYPos * XYPos) (secondSegment: XYPos * XYPos) =

            // If overlap exists,
            // returns the longer of the two segments (so that the distinct-ness of the segments can be checked)
            // and if not, returns both segments.

            let firstSegmentLength = euclideanDistance (fst firstSegment) (snd firstSegment)
            let secondSegmentLength = euclideanDistance (fst secondSegment) (snd secondSegment)

            // The static coordinates need to be the same, along with having the same orientation, for two segments to potentially overlap.
            // The below code handles an extreme edge case, where the user might forcefully move a wire by dragging it so that it exactly overlaps
            // with another wire, which would normally not happen. In such a case, for the beautify measure metric to work as intended, all segments
            // need to be checked for overlap.

            match wireOrientation firstSegment, wireOrientation secondSegment with
            | (x1Coord, BusWireT.Vertical), (x2Coord, BusWireT.Vertical) when abs(x1Coord - x2Coord) <= 0.05 ->
                // FP tolerance, would ideally be 0.
                let firstYStart, firstYEnd = (fst firstSegment).Y, (snd firstSegment).Y
                let secondYStart, secondYEnd = (fst secondSegment).Y, (snd secondSegment).Y

                if BlockHelpers.overlap1D (firstYStart, firstYEnd) (secondYStart, secondYEnd)
                then if firstSegmentLength >= secondSegmentLength then [firstSegment] else [secondSegment]
                // FP inaccuracies for equality can mess things up here, but this is an extreme edge case where two segments not meant to overlap are intentionally
                // made to do so. It also handles the net case, by returning the longer of the two segments in case of said overlap.
                else [firstSegment; secondSegment]

            | (y1Coord, BusWireT.Horizontal), (y2Coord, BusWireT.Horizontal) when abs(y1Coord - y2Coord) < 0.05 ->
                // FP tolerance, would ideally be 0.
                let firstXStart, firstXEnd = (fst firstSegment).X, (snd firstSegment).X
                let secondXStart, secondXEnd = (fst secondSegment).X, (snd secondSegment).X

                if BlockHelpers.overlap1D (firstXStart, firstXEnd) (secondXStart, secondXEnd)
                then if firstSegmentLength >= secondSegmentLength then [firstSegment] else [secondSegment]
                // FP inaccuracies for equality can mess things up here, but this is an extreme edge case where two segments not meant to overlap are intentionally
                // made to do so. It also handles the net case, by returning the longer of the two segments in case of said overlap.
                else [firstSegment; secondSegment]

            | _ -> [firstSegment; secondSegment]


        /// Returns the total visible wire length in the model.
        ///
        /// model - The sheet to be checked.
        let visibleWireLength (model: SheetT.Model) =

            let allSegments = globalWireVisSegments model

            List.allPairs allSegments allSegments
            |> List.collect (fun (firstSegment, secondSegment) -> segmentsOvelap firstSegment secondSegment)
            |> List.distinct
            |> List.fold (fun length segment -> length + euclideanDistance (fst segment) (snd segment)) 0.0



    module T5 =
        /// Given a sheet, returns the number of visible wire right angles on the sheet.
        ///
        /// model - The sheet to be checked on.
        let visibleWireRightAngles (model: SheetT.Model) =
            // Since zero length segments are coalesced, the number of right angle bends in a wire will always be
            // the number of visible segments - 1. 
            model.Wire.Wires
            |> Map.toList
            |> List.map (fst)
            |> List.fold (fun count wId -> count + (List.length (visibleSegments wId model) - 1)) 0



    module T6 =
        /// Returns from one list, the segments that retrace each other, and the ends of a wire
        /// such that the next segment would start inside a symbol (could be from the beginning or the end).
        ///
        /// wire - The wire to be checked for such an artefact.
        let wireRetrace (wire: BusWireT.Wire) =
            let retracedSegments =
                let segments = wire.Segments
                segments
                |> List.mapi (fun i seg -> i, seg) // Add index to each segment
                |> List.fold (fun lst (i, seg) ->
                    match seg.Length with
                    | length when length < 0.001 && i > 0 && i < List.length segments - 1 ->
                        let prevSeg = segments[i-1]
                        let nextSeg = segments[i+1]
                        if prevSeg.Length * nextSeg.Length < 0.0 then nextSeg :: lst else lst
                    | _ -> lst) []
                |> List.rev

            // Responsible for checking the second element and the second last element in a list of segments
            // this is the "one from the end" check, where [5; 0; -10...] or [....; -5; 0; 10] - these are the cases
            // leading to wire retrace such that the next segment is inside a symbol. This is also the reason absolute values
            // are considered.
            let endOfWireRetrace =
                let segments = wire.Segments
                segments
                |> List.mapi (fun i seg -> i, seg)
                |> List.fold (fun lst (i, seg) ->
                    match seg.Length with
                    | length when length < 0.001 && i = 1 || i = List.length segments - 2 ->
                        let prevSeg = segments[i-1]
                        let nextSeg = segments[i+1]
                        if prevSeg.Length * nextSeg.Length < 0.0 && abs(nextSeg.Length) > abs(prevSeg.Length) then nextSeg :: lst else lst
                    | _ -> lst) []
                |> List.rev

            [retracedSegments, endOfWireRetrace]



    module GroupWorkContribution =
        open EEExtensions

        type FailureType =
            | WiresNotStraightened of string
            | SymbolIntersections of string
            | WireSymbolIntersections of string


        type TestResult = | Success of string | Failure of FailureType


        type SheetAlignCircuitType =
            | MUX2Inputs of SheetT.Model
            | UnstraightenedMUX2Inputs of SheetT.Model
            | GateScaling of SheetT.Model
            | MultipleMUX of SheetT.Model
            | CustomComponentScaling of SheetT.Model

        let middleOfSheet = {X = Sheet.Constants.defaultCanvasSize/2.0; Y = Sheet.Constants.defaultCanvasSize/2.0}

        
        /// Empty function that will hold the completed sheetAlignScale function
        let sheetAlignScale (model: SheetT.Model) =
            model


        /// Given a model, applies the completed sheetAlignScale and judges if visual quality
        /// has improved or not
        ///
        /// model - The sheet to be beautified and checked.
        let evaluateBeautification (model: SheetT.Model) =
            let straightWires (model: SheetT.Model) =
                model.Wire.Wires
                |> Map.toList
                |> List.map fst
                |> List.fold (fun wires wId -> if List.length (visibleSegments wId model) = 1 then wires + 1 else wires) 0

            let beautifiedModel = sheetAlignScale model

            let initialStraightWires = straightWires model
            let finalStraightWires = straightWires beautifiedModel

            let percentWiresStraightened = int (100.0 * float (finalStraightWires - initialStraightWires) / float initialStraightWires)
            let symbolIntersectionCount = T1.intersectingSymbolPairs beautifiedModel
            let wireSymbolIntersectionCount = T2.visSegsIntersectSymbol beautifiedModel

            match (percentWiresStraightened, symbolIntersectionCount, wireSymbolIntersectionCount) with
            | (wires, _, _) when wires < 0 ->
                $">> sheetAlignScale failed - Initial Straight Wires: {initialStraightWires} | Final Straight Wires: {finalStraightWires}"
                |> WiresNotStraightened
                |> Failure
            | (_, count, _) when count > 0 ->
                $">> sheetAlignScale failed - Pairs of intersecting symbols: {count}"
                |> SymbolIntersections
                |> Failure
            | (_, _, count) when count > 0 ->
                $">> sheetAlignScale failed - Wires intersecting symbols: {count}"
                |> WireSymbolIntersections
                |> Failure
            | _ -> Success ($">> {percentWiresStraightened}" + "% wires straightened")


        /// Given a sheet, prints out the name of the symbols on it and their position to the console.
        ///
        /// model - Sheet to be replicated.
        let printSymbolProperties (model: SheetT.Model) =
            // This function is meant to be used in Playground.fs, for ease of generation of circuits from the
            // positions printed out.
            model.Wire.Symbol.Symbols
            |> Map.map (fun _compId sym ->
                            printfn $">> {sym.Component.Label}: X - {sym.Pos.X}, Y - {sym.Pos.Y}"
                            sym)
            |> ignore


        /// Place a new symbol with label symLabel onto the Sheet with given position.
        /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
        /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
        /// symLabel - the component label, will be uppercased to make a standard label name
        /// compType - the type of the component
        /// position - the top-left corner of the symbol outline.
        /// model - the Sheet model into which the new symbol is added.
        let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | {X=x;Y=y} when x > Sheet.Constants.defaultCanvasSize || y > Sheet.Constants.defaultCanvasSize ->
                Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set SheetT.symbol_ symModel
                |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok

        let simpleInputsMUX (initSheetModel: SheetT.Model) (inputPos: XYPos) =
            initSheetModel
            |> placeSymbol "I1" (Input1(1, None)) {X = inputPos.X; Y = inputPos.Y - 40.0}
            |> Result.bind (placeSymbol "I2" (Input1(1, None)) {X = inputPos.X; Y = inputPos.Y + 40.0}) // swap the order for wire crossing?
            |> Result.bind (placeSymbol "MUX" (Mux2) middleOfSheet)
            //|> Result.bind (placeWire (portOf "I1" 0) (portOf "MUX" 0))
            //|> Result.bind (placeWire (portOf "I2" 0) (portOf "MUX" 1))

