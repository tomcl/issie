module SheetBeautifyD1

open CommonTypes
open DrawModelType
open BlockHelpers
open BusWire
// this is the module for team phase work D1 

/// Extract parallel wires that can be straightened from wire list
let extractParalellWires (wires: BusWireT.Wire list) : BusWireT.ASegment list list = 
    //given three visual (non-zero) segments, determine whether combine into a parallel wire
    let isParallel (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) (seg3: BusWireT.ASegment) : bool= 
        getSegmentOrientation seg1.Start seg1.End = getSegmentOrientation seg3.Start seg3.End &&
        getSegmentOrientation seg1.Start seg1.End <> getSegmentOrientation seg2.Start seg2.End &&
        seg1.Segment.Length * seg3.Segment.Length > 0.0
    wires
    |> List.map (fun wire -> getNonZeroAbsSegments wire)
    |> List.filter (fun segs -> segs.Length = 3)
    |> List.filter (fun segs -> isParallel segs[0] segs[1] segs[2])