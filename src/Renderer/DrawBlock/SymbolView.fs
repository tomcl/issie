module SymbolView

open Fable.React
open Fable.React.Props
open Elmish


open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT
open Symbol

(*
    HLP23: This module will normally be used exclusively by team member doing the "smart rendering" part of the 
    individual coding. During group phase work how it is used is up to the
    group. Normally chnages will be to drawSymbol and the code used by this. OTHER changes to the rendering code
    are possible but you should check before doing anything. renderSymbol is not all well written, but it uses
    React cacheing (the Props of FunctionComponent.Of are used as a key so that the whole render function (which
    calls drawSymbol) is only re-executed when renderSymbolProps change. This means that normally drawSymbol is not
    called whenever the view function is evaluated - crucial to keeping view function time down!
    
    
    HLP23: There is a lot of code here. For assessment, changes to existing code, or new functions,
    MUST be documented by HLP23:AUTHOR even if from the smart rendering assigned student, so that new code
    can easily distinguished from old. (Git can also help with this, but it is not totally reliable)
    Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    HLP23: the existing drawSymbol code is imperfect. Many issues, note for example repeated pipelined
    use of append to join different elements together which is inefficient and less readable.
    HLP23: the code here does not use helpers consistently or in all suitable places.
*)


//-----------------------------------------DRAWING HELPERS ---------------------------------------------------

/// Text adding function with many parameters (such as bold, position and text)
let addText (pos: XYPos) name alignment weight size =
    let text =
            {defaultText with TextAnchor = alignment; FontWeight = weight; FontSize = size}
    [makeText pos.X pos.Y name text]

/// Add one or two lines of text, two lines are marked by a . delimiter
let addLegendText (pos: XYPos) (name:string) alignment weight size =
    let text =
            {defaultText with TextAnchor = alignment; FontWeight = weight; FontSize = size}
    match name.Split([|'.'|]) with
    | [|oneLine|] -> 
        [makeText pos.X pos.Y name text]
    | [|topLine;bottomLine|] ->
        [makeText pos.X pos.Y topLine text;
         makeText pos.X (pos.Y+Constants.legendLineSpacingInPixels) bottomLine text]
    | _ ->
        failwithf "addLegendText does not work with more than two lines demarcated by ."


let addStyledText (style:Text) (pos: XYPos) (name: string) = 
    makeText pos.X pos.Y name style

/// Generate circles on ports
let inline private portCircles (pos: XYPos) (show:ShowPorts)= 
    let circle = 
        match show with
        |ShowBothForPortMovement |ShowOneTouching _ -> {portCircle with Fill="DodgerBlue";}
        |ShowOneNotTouching _ -> {portCircle with Fill="Red"}
        |ShowTarget -> portCircleTarget
        |_ -> portCircle
    
    [makeCircle pos.X pos.Y circle]

/// Puts name on ports
let private portText (pos: XYPos) name edge =
    let pos' = 
            match edge with 
            | Left -> pos + {X = 5.; Y = -6.}
            | Top -> pos + {X = 0.; Y = 5.}
            | Right -> pos + {X = -5.; Y = -6.}
            | Bottom -> pos + {X = 0.; Y = -15.}

    let align = 
            match edge with
            | Right -> "end"
            | Left -> "start"
            | _ -> "middle"
    (addText pos' name align Constants.portTextWeight Constants.portTextSize)


/// Print the name of each port 
let drawPortsText (portList: list<Port>) (listOfNames: list<string>) (symb: Symbol) = 
    let getPortName name x = portText (getPortPosToRender symb portList[x]) name (symb.PortMaps.Orientation[portList.[x].Id])
    if listOfNames.Length < 1
    then []
    else 
        [0..(portList.Length-1)]
        |> List.map2 getPortName listOfNames 
        |> List.collect id

/// Function to draw ports using getPortPos. The ports are equidistant     
let drawPorts (portType: PortType) (portList: Port List) (showPorts:ShowPorts) (symb: Symbol)= 
    if not (portList.Length < 1) then       
        match (showPorts,portType) with
        |(ShowBoth,_) |(ShowInput,PortType.Input) |(ShowOutput,PortType.Output) | (ShowBothForPortMovement,_) -> [0..(portList.Length-1)] |> List.collect (fun x -> (portCircles (getPortPosToRender symb portList[x]) showPorts ))  
        |(ShowOneTouching p, _) | (ShowOneNotTouching p, _) -> [0..(portList.Length-1)] |> List.collect (fun x -> if portList[x] = p then (portCircles (getPortPosToRender symb portList[x]) (showPorts) ) else (portCircles (getPortPosToRender symb portList[x]) ShowBothForPortMovement ))
        |(_,_) -> []
    else []

/// Function to draw the Target of a Moving Port (if there is one)
let drawMovingPortTarget (pos: (XYPos*XYPos) option) symbol outlinePoints = 
    match pos with
    |None -> []
    |Some (targetPos,mousePos) -> 
        (portCircles targetPos ShowTarget) 
        |> List.append ([makeLine targetPos.X targetPos.Y (mousePos.X-symbol.Pos.X) (mousePos.Y-symbol.Pos.Y) {defaultLine with Stroke="DodgerBlue"; StrokeWidth="2.0px" ;StrokeDashArray="4,4"}])
        |> List.append [makePolygon outlinePoints {defaultPolygon with Fill = "No"; FillOpacity = 0.0; Stroke = "DodgerBlue"; StrokeWidth="2px"}] 

//------------------------------------------------------------------------------------------------//
//------------------------------HELPER FUNCTIONS FOR DRAWING SYMBOLS------------------------------//
//------------------------------------------------------------------------------------------------//

let private createPolygon points colour opacity = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]


let createBiColorPolygon points colour strokeColor opacity strokeWidth (comp:Component)= 
    if strokeColor <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]

let addClock (pos: XYPos) colour opacity =
    let points = sprintf $"{pos.X},{pos.Y-1.},{pos.X+8.},{pos.Y-7.},{pos.X},{pos.Y-13.}"
    createPolygon points colour opacity
    |> List.append (addText (pos + {X = 10.; Y = -13.} ) " clk" "start" "normal" "12px")

let addHorizontalLine posX1 posX2 posY opacity = // TODO: Line instead of polygon?
    let points = sprintf $"{posX1},{posY},{posX2},{posY}"
    createPolygon points "lightgray" opacity

let outlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" |"lightblue" | "#E8D0A9" | "rgba(255,255,0,0.15)"  -> "black"
    | c -> c

let addHorizontalColorLine posX1 posX2 posY opacity (color:string) = // TODO: Line instead of polygon?
    let points = sprintf $"{posX1},{posY} {posX2},{posY}"
    let outlineColor = outlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=outlineColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

/// Takes points, height and width of original shape and returns the points for it given a rotation / flipped status.
/// Degree0 rotation has TopLeft = top left coordinate of the outline, which is a box of dimensions W X H.
/// Rotation rotates the box about its centre point, keeping TopLeft fixed.

let createOldComp (comp:Component) (parameters:Path) strokeWidth= 
    match comp.Type with
    |And -> let curveAttr = makePartArcAttr 5. -20. -20. 25. 20.
            let line = {Stroke = "Black"; StrokeWidth = strokeWidth; StrokeDashArray = ""}
            let horizLineAttr = makeLineAttr 25 0
            let vertLineAttr = makeLineAttr 0 comp.H
            let length = 30
            let fillSquare = makePolygon ($"0,0 {length},0 {length},{comp.H} 0,{comp.H}") {defaultPolygon with Fill = parameters.Fill}
            [fillSquare; makeAnyPath {X = 0; Y = 0} vertLineAttr parameters;
            makeAnyPath {X = length; Y = 0} vertLineAttr {parameters with Stroke = parameters.Fill; StrokeWidth = "1.5px"};
            makeAnyPath {X = length; Y = comp.H} curveAttr parameters]

    |Or ->  let attr = makePartArcAttr 5. -20. -2. 20. 20.
            [makeAnyPath {X = 0; Y = comp.H} attr parameters]

    |Xor -> let attr = makePartArcAttr 5. -20. -20. 20. 20.
            [makeAnyPath {X = 0; Y = comp.H} attr parameters]

    |_ ->   [makeAnyPath {X = 0; Y = comp.H} (makePartArcAttr 5. -20. -2. 20. 20.) parameters;]

let createCurvedShape colour strokeWidth (comp:Component)=

    let parameters = {Stroke = "Black"; StrokeWidth = strokeWidth; StrokeDashArray = ""; StrokeLinecap = "round"; Fill = colour}
    createOldComp comp parameters strokeWidth
    
    





let rotatePoints (points) (centre:XYPos) (transform:STransform) = 
    let offset = 
            match transform.Rotation with
            | Degree0 | Degree180 -> centre
            | Degree90 | Degree270 -> {X = centre.Y; Y = centre.X}

    let relativeToCentre = Array.map (fun x -> x - centre)
    let rotateAboutCentre pointsIn = 
        match transform.Rotation with
        | Degree0   -> pointsIn
        | Degree270 -> Array.map (fun (pos:XYPos) -> {X = -pos.Y ; Y = pos.X}) pointsIn
        | Degree180 -> Array.map (fun (pos:XYPos) -> {X = -pos.X ; Y = -pos.Y}) pointsIn
        | Degree90  -> Array.map (fun (pos:XYPos) -> {X = pos.Y ; Y = -pos.X}) pointsIn

    let relativeToTopLeft = Array.map (fun x -> x + offset ) 
    /// Flips the points, needed some hacks to avoid saving transforms somewhere / saving current points
    /// Also can't guarantee it will work if there are changes to rotation / flip with funkier shapes
    let flipIfNecessary pts =
        if not transform.flipped then pts
        else
            match transform.Rotation with
            | _ -> Array.map (fun (point:XYPos) -> {X = -point.X; Y = point.Y}) pts

    points
    |> relativeToCentre
    |> rotateAboutCentre
    |> flipIfNecessary
    |> relativeToTopLeft


//--------------------------------------------------------------------------------------------//
//--------------------------------------- SYMBOL DRAWING -------------------------------------//
//--------------------------------------------------------------------------------------------//

/// Draw symbol (and its label) using theme for colors, returning a list of React components 
/// implementing all of the text and shapes needed.
let drawSymbol (symbol:Symbol) (theme:ThemeType) =
    let appear = symbol.Appearance
    let colour = appear.Colour
    let showPorts = appear.ShowPorts
    // let showOutputPorts = appear.ShowOutputPorts
    let opacity = appear.Opacity
    let comp = symbol.Component
    let h,w = getRotatedHAndW symbol
    let H = float comp.H*(Option.defaultValue 1.0 symbol.VScale)
    let W = float comp.W*(Option.defaultValue 1.0 symbol.HScale)
    let transform = symbol.STransform

    let mergeSplitLine pos msb lsb  =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> sprintf $"({msb})"
            | false, _ -> sprintf $"({msb}:{lsb})"
        addText pos text "middle" "bold" Constants.mergeSplitTextSize


    let busSelectLine msb lsb  =
        let text = 
            match msb = lsb  with
            | true -> sprintf $"({lsb})"
            | false -> sprintf $"({msb}:{lsb})"
        let pos, align = 
            let rotate' = 
                if not transform.flipped then 
                    transform.Rotation
                else
                    match transform.Rotation with 
                    | Degree90 -> Degree270 | Degree270 -> Degree90 | r -> r
            match rotate' with
            | Degree0 -> {X=w/2.; Y= h/2. + 7.}, "middle"
            | Degree180 -> {X=w/2.; Y= -8.}, "middle"
            | Degree270 -> {X= 4.; Y=h/2. - 7.}, "end"
            | Degree90 -> {X= 5.+ w/2.; Y=h/2. }, "start"
        addText pos text align "bold" Constants.busSelectTextSize

    let clockTxtPos = 
        match transform.Rotation, transform.flipped with
        | Degree0, false -> {X = 17.; Y = H - 13.}
        | Degree180, true -> {X = 17.; Y = 2.}
        | Degree90, false -> {X = float w - 8.; Y = float h - 20.}
        | Degree270, true ->  {X = float w - 10.; Y = 11.}
        | Degree180, false -> {X = W - 19.; Y = 2.}
        | Degree0, true -> {X = W - 17.; Y = H - 13.}
        | Degree270, false -> {X = 10.; Y = 11.}
        | Degree90, true -> {X = 8.; Y = float h - 20.}

    /// Points that define the edges of the symbol
    let points =
        let toString = Array.fold (fun x (pos:XYPos) -> x + (sprintf $" {pos.X},{pos.Y}")) "" 
        let originalPoints =
            match comp.Type with
            // legacy component: to be deleted
            | Input _
            | Input1 _ |Output _ -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W*4./5.;Y=H};{X=W;Y=H/2.};{X=W*0.8;Y=0}|] 
            //| Output _ -> 
            //    [|{X=W/5.;Y=0};{X=0;Y=H/2.};{X=W/5.;Y=H};{X=W;Y=H};{X=W;Y=0}|]
            | Constant1 _ -> 
                [|{X=W;Y=H/2.};{X=W/2.;Y=H/2.};{X=0;Y=H};{X=0;Y=0};{X=W/2.;Y=H/2.}|]
            | IOLabel ->
                [|{X=0.;Y=H/2.};{X=W;Y=H/2.}|]
            | Viewer _ ->
                [|{X=W/5.;Y=0};{X=0;Y=H/2.};{X=W/5.;Y=H};{X=W;Y=H};{X=W;Y=0}|]
            | MergeWires -> 
                [|{X=0;Y=H/6.};{X=W/2.;Y=H/6.};{X=W/2.;Y=H/2.};{X=W;Y=H/2.};{X=W/2.;Y=H/2.};{X=W/2.;Y=5.*H/6.};{X=0;Y=5.*H/6.};{X=W/2.;Y=5.*H/6.};{X=W/2.;Y=H/6.}|]
            | SplitWire _ -> 
                [|{X=W;Y=H/6.};{X=W/2.;Y=H/6.};{X=W/2.;Y=H/2.};{X=0;Y=H/2.};{X=W/2.;Y=H/2.};{X=W/2.;Y=5.*H/6.};{X=W;Y=5.*H/6.};{X=W/2.;Y=5.*H/6.};{X=W/2.;Y=H/6.}|]
            // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
            // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
            | Demux2 | Demux4 | Demux8 ->
                [|{X=0;Y=0.3*W};{X=0;Y=H-0.3*W};{X=W;Y=H};{X=W;Y=0}|]
            | Mux2 | Mux4 | Mux8 -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H-0.3*W};{X=W;Y=0.3*W}|]
            | BusSelection _ -> 
                [|{X=0;Y=H/2.}; {X=W;Y=H/2.}|]
            | BusCompare _ |BusCompare1 _-> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W*0.6;Y=H};{X=W*0.8;Y=H*0.7};{X=W;Y=H*0.7};{X=W;Y =H*0.3};{X=W*0.8;Y=H*0.3};{X=W*0.6;Y=0}|]
            | Not | Nand | Nor | Xnor ->
                  [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H};{X=W;Y=H/2.};{X=W+9.;Y=H/2.};{X=W;Y=H/2.-8.};{X=W;Y=H/2.};{X=W;Y=0}|]
                //[|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H};{X=W;Y=H/2.};{X=W+9.;Y=H/2.};{X=W;Y=H/2.-8.};{X=W;Y=H/2.};{X=W;Y=0}|]
            | DFF | DFFE | Register _ | RegisterE _ | ROM1 _ |RAM1 _ | AsyncRAM1 _ 
            | Counter _ | CounterNoEnable _ 
            | CounterNoLoad _ | CounterNoEnableLoad _ -> 
                [|{X=0;Y=H-13.};{X=8.;Y=H-7.};{X=0;Y=H-1.};{X=0;Y=0};{X=W;Y=0};{X=W;Y=H};{X=0;Y=H}|]
            | Custom x when symbol.IsClocked = true -> 
                [|{X=0;Y=H-13.};{X=8.;Y=H-7.};{X=0;Y=H-1.};{X=0;Y=0};{X=W;Y=0};{X=W;Y=H};{X=0;Y=H}|]
            | NbitSpreader _ ->
                [|{X=0;Y=H/2.};{X=W*0.4;Y=H/2.};{X=W*0.4;Y=H};{X=W*0.4;Y=0.};{X=W*0.4;Y=H/2.};{X=W;Y=H/2.}|]
            | _ -> 
                [|{X=0;Y=0};{X=0;Y=H};{X=W;Y=H};{X=W;Y=0}|]
        rotatePoints originalPoints {X=W/2.;Y=H/2.} transform
        |> toString 



    let additions =       // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
        let mergeWiresTextPos =
            let textPoints = rotatePoints [|{X=W/5.;Y=H/6.+2.};{X=W/5.;Y=H*5./6.+2.};{X=W*0.75;Y=H/2.+2.}|] {X=W/2.;Y=H/2.} transform
            match transform.Rotation with
            | Degree90 | Degree270 -> Array.map (fun pos -> pos + {X=12.;Y=0}) textPoints
            | Degree180 -> Array.map (fun pos -> pos + {X=0;Y= +5.}) textPoints
            | _ -> textPoints
        let splitWiresTextPos =
            let textPoints = rotatePoints [|{X=W*0.75;Y=H/6.+2.};{X=W*0.75;Y=H*5./6.+2.};{X=W/4.;Y=H/2.+2.}|] {X=W/2.;Y=H/2.} transform
            match transform.Rotation with
            | Degree90 | Degree270 -> Array.map (fun pos -> pos + {X=12.;Y=0}) textPoints
            | Degree180 -> Array.map (fun pos -> pos + {X=0;Y= +5.}) textPoints
            | _ -> textPoints
        let NbitSpreaderTextPos =
            let textPoints = rotatePoints [|{X=W/4.;Y=H/2.+2.};{X=W*0.7;Y=H/2.+4.}|] {X=W/2.;Y=H/2.} transform
            match transform.Rotation with
            | Degree90 -> Array.map (fun pos -> pos + {X=13.;Y=(-5.0)}) textPoints
            | Degree180 -> Array.map (fun pos -> pos + {X=0;Y= +8.}) textPoints
            | Degree270 -> Array.map (fun pos -> pos + {X=18.;Y=(-5.0)}) textPoints
            | _ -> textPoints
        let rotate1 pos = 
            match rotatePoints [|pos|] {X=W/2.;Y=H/2.} transform with 
            | [|pos'|]-> pos' 
            | _ -> failwithf "What? Can't happen"

        match comp.Type with
        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            let values = [(midt,0);(msb,midb);(msb,0)]
            List.fold (fun og i ->
                og @ mergeSplitLine 
                        mergeWiresTextPos[i] 
                        (fst values[i]) 
                        (snd values[i])) [] [0..2]
        | NbitSpreader n -> 
            //let lo = 1
            //let msb = hi + lo - 1
            //let midb = lo
            //let midt = lo - 1
            let values = [(-1,0);((n-1),0)]
            List.fold (fun og i ->
                og @ mergeSplitLine 
                        NbitSpreaderTextPos[i] 
                        (fst values[i]) 
                        (snd values[i])) [] [0..1]
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            let values = [(midt,0);(msb,midb);(msb,0)]
            List.fold (fun og i -> 
                og @ mergeSplitLine 
                        splitWiresTextPos[i] 
                        (fst values[i]) 
                        (snd values[i])) [] [0..2]
        | DFF | DFFE | Register _ |RegisterE _ | ROM1 _ |RAM1 _ | AsyncRAM1 _ | Counter _ | CounterNoEnable _ | CounterNoLoad _ | CounterNoEnableLoad _  -> 
            (addText clockTxtPos " clk" "middle" "normal" "12px")
        | BusSelection(nBits,lsb) ->           
            busSelectLine (lsb + nBits - 1) lsb
        | Constant1 (_, _, dialogVal) -> 
            let align, yOffset, xOffset= 
                match transform.flipped, transform.Rotation with
                | false, Degree180
                | true, Degree0 -> "end",0.,5.
                | _, Degree90 -> "end",-15.,-5.
                | _, Degree270 -> "end",0.,-5.
                | _ -> "start",0.,-5.
            let fontSize = if dialogVal.Length < 2 then "14px" else "12px"
            addText {X = w/2. + xOffset; Y = h/1.5 + yOffset}  dialogVal align "normal" fontSize
        | BusCompare (_,y) ->
            (addText {X = w/2.-2.; Y = h/2.7-1.} ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
        |BusCompare1 (_,_,t) -> 
            (addText {X = w/2.-2.; Y = h/2.7-1.} ("= " + t) "middle" "bold" "10px")
        // legacy component type: to be deleted
        | Input x
        | Input1 (x, _) | Output x-> 
            (addText {X = w/2.; Y = h/2.7} (busTitleAndBits "" x) "middle" "normal" "12px")
        | Viewer (x) -> 
            (addText {X = w/2.; Y = h/2.7 - 1.25} (busTitleAndBits "" x) "middle" "normal" "9px")
        | _ when symbol.IsClocked -> 
            (addText (Array.head (rotatePoints [|{X = 15.; Y = float H - 11.}|] {X=W/2.;Y=H/2.} transform )) " clk" "middle" "normal" "12px")
        | _ -> []

    let outlineColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColor colour, "4.0"
        |NbitSpreader _ -> outlineColor colour, "4.0"
        | IOLabel -> outlineColor colour, "4.0"
        | BusSelection _ -> outlineColor colour, "4.0"
        | _ -> "black", "1.0"
    


    /// to deal with the label
    let addComponentLabel (comp: Component) transform colour = 
        let weight = Constants.componentLabelStyle.FontWeight // bold or normal
        let style = {Constants.componentLabelStyle with FontWeight = weight}
        let box = symbol.LabelBoundingBox
        let margin = 
            match comp.Type with
            | BusSelection _ | IOLabel -> Constants.thinComponentLabelOffsetDistance
            | _ -> Constants.componentLabelOffsetDistance


        let pos = box.TopLeft - symbol.Pos + {X=margin;Y=margin} + Constants.labelCorrection
        let text = addStyledText {style with DominantBaseline="hanging"} pos comp.Label
        match Constants.testShowLabelBoundingBoxes, colour with
        | false, "lightgreen" ->
            let x,y = pos.X - margin*0.8, pos.Y - margin*0.8
            let w,h = box.W - margin*0.4, box.H - margin * 0.4
            let polyStyle = {defaultPolygon with Fill = "lightgreen"; StrokeWidth = "0"}
            let poly = makePolygon $"{x},{y} {x+w},{y} {x+w},{y+h} {x},{y+h}" polyStyle 
            [ poly ; text ]
        | false, _ ->           
            [text]
        | true, _ ->
            // Display label bounding box corners for testing new fonts etc.
            let dimW = {X=box.W;Y=0.}
            let dimH = {X=0.;Y=box.H}
            let corners = 
                [box.TopLeft; box.TopLeft+dimW; box.TopLeft+dimH; box.TopLeft+dimW+dimH]
                |> List.map (fun c -> 
                    let c' = c - symbol.Pos
                    makeCircle (c'.X) (c'.Y) {defaultCircle with R=3.})
            text :: corners



 
            
    let labelcolour = outlineColor symbol.Appearance.Colour
    let legendOffset (compWidth: float) (compHeight:float) (symbol: Symbol) : XYPos=
        let pMap = symbol.PortMaps.Order
        let vertFlip = symbol.STransform.Rotation = Degree180
        let getNum  (edge: Edge) = 
            Map.tryFind edge pMap
            |> Option.map (fun lst -> lst.Length)
            |> Option.defaultValue 0
        let lhsPortNum = getNum Edge.Left
        let rhsPortNum = getNum Edge.Right
        let offset:XYPos = 
            match lhsPortNum % 2, rhsPortNum % 2, symbol.Component.Type with
            | _, _, Custom _ -> {X=0;Y=0}
            | _, _, Not -> {X=0;Y=0}
            | _, _, IsBinaryGate -> {X=0;Y=0}
            | 1, 1, _ -> {X = 0.; Y = Constants.legendVertOffset * (if vertFlip then 0.5 else -3.)}
            | 0, 0, _ -> {X = 0.; Y = 0.}
            | 1, 0, _ -> {X = 10.; Y = 0.}
            | 0, 1, _ -> {X = -10.; Y = 0.}
            | _ -> failwithf "What? Can't happen"

        {X=compWidth / 2.; Y=compHeight / 2. - 7.} + offset
    let legendFontSize (ct:ComponentType) =
        match ct with
        | Custom _ -> "16px"
        | _ -> "14px"

    // Put everything together 
    (drawPorts PortType.Output comp.OutputPorts showPorts symbol)
    |> List.append (drawPorts PortType.Input comp.InputPorts showPorts symbol)
    |> List.append (drawPortsText (comp.InputPorts @ comp.OutputPorts) (portNames comp.Type) symbol)
    |> List.append (addLegendText 
                        (legendOffset w h symbol) 
                        (getComponentLegend comp.Type transform.Rotation) 
                        "middle" 
                        "bold" 
                        (legendFontSize comp.Type))
    |> List.append (addComponentLabel comp transform labelcolour)
    |> List.append (additions)
    |> List.append (drawMovingPortTarget symbol.MovingPortTarget symbol points)
    //|> List.append (createBiColorPolygon points colour outlineColour opacity strokeWidth comp)
    |> List.append (createCurvedShape colour strokeWidth comp)


//----------------------------------------------------------------------------------------//
//---------------------------------View Function for Symbols------------------------------//
//----------------------------------------------------------------------------------------//

type private RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string
        Theme: ThemeType
    }

/// View for one symbol. Using FunctionComponent.Of to improve efficiency 
/// (not printing all symbols but only those that are changing).
let private renderSymbol =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y=fY}:XYPos) = symbol.Pos
            let appear = symbol.Appearance
            g ([ Style [ Transform(sprintf $"translate({fX}px, {fY}px)") ] ]) 
                (drawSymbol props.Symbol props.Theme)
            
        , "Symbol"
        , equalsButFunctions
        )
    
/// View function for symbol layer of SVG
let MapsIntoLists map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |>Map.toList
        |>List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |>Map.toList
        |>List.map snd
    listMoving @ listNotMoving


let view (model : Model) (dispatch : Msg -> unit) =    
    /// View function for symbol layer of SVG
    let toListOfMovingAndNot map =
        let listMoving = 
            Map.filter (fun _ sym -> not sym.Moving) map
            |> Map.toList
            |> List.map snd
        let listNotMoving =
            Map.filter (fun _ sym -> sym.Moving) map
            |> Map.toList
            |> List.map snd
        listMoving @ listNotMoving

    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> toListOfMovingAndNot
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
        renderSymbol
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
                Theme = model.Theme
            }
    )
    |> ofList
    |> TimeHelpers.instrumentInterval "SymbolView" start

/// init function for initial Symbol Model
let init () = 
    { 
        Symbols = Map.empty; CopiedSymbols = Map.empty
        Ports = Map.empty ; InputPortsConnected= Set.empty
        OutputPortsConnected = Map.empty; Theme = Colourful
    }, Cmd.none
