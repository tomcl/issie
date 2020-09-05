(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open DiagramStyle
open DiagramModelType
open DiagramMessageType
open CommonTypes
open PopupView
open Extractor
open System

type Bbox = {
    LTop: (int*int)
    RBot: (int*int)
    }

/// Current Draw2D schematic sheet scroll and zoom position.
/// Component positions are always set or returned as unscaled, and with no offset.
/// Sheet values, offsets, and scaling change what portion of draw2d canvas is seen.

type ScrollPos = {
    /// X view size in unzoomed pixels
    SheetX: int
    /// Y view size in unzoomed pixels
    SheetY: int
    /// X view leftmost offset in zoomed pixels
    SheetLeft: int
    /// Y view topmost offset in zoomed pixels
    SheetTop: int
    /// Draw2D canvas element width in unzoomed pixels
    CanvasX: int
    /// Draw2D canvas element hight in unzoomed pixels
    CanvasY: int
    /// Zoom factor for scaling. > 1 => shrink objects, < 1 => magnify objects
    Zoom: float
}


let getViewableXY (sPos: ScrollPos) =
    let lTop = sPos.SheetLeft, sPos.SheetTop
    {
        LTop = lTop
        RBot =  
            lTop |> 
            fun (x,y) -> x + sPos.SheetX, y + sPos.SheetY
    }

let checkOnCanvas sPos box =
    let {LTop=(x1,y1); RBot=(x2,y2)} = box
    let {LTop=(x1',y1'); RBot=(x2',y2')} = getViewableXY sPos
    x1 < x1' || y1 < y1' || x2 > x2' || y2 > y2'
    |> not

/// Obtain scroll and zoom values to put box into viewable area
/// If zoom is adjusted the highest magnification that allows this is used
/// zoom is limited by max and min values
/// too small zoomMax might mean the box is not viewable
/// Return Error on box not viewable or box not on canvas
let GetNewPos (zoomMin: float) (zoomMax: float) (model: Model) ( box: Bbox) (sPos: ScrollPos) =
    let x1,y1 = box.LTop
    let x2,y2 = box.RBot
    let zoomIdeal = max ((float x2-float x1)/(float sPos.SheetX)) ((float y2-float y1)/(float sPos.SheetY))
    let zoom = 
        if zoomIdeal < zoomMin then 
            zoomMin else 
        if zoomIdeal > zoomMax then
            zoomMax else
            zoomIdeal

    let scale pos = int (float pos / float sPos.Zoom)
    let newScrollPos =
        { sPos with
            Zoom = zoom
            SheetLeft = (scale x1) 
            SheetTop = (scale y1) 
        }
    if checkOnCanvas newScrollPos box then  
        Ok newScrollPos
    else
        Error <| sprintf "Can't view %A inside the allowed box of %A" box (getViewableXY newScrollPos)



type Direction = TOP | BOTTOM | LEFT | RIGHT | MID

let rTop bb = match bb with {LTop=(x,y); RBot=(x',y')} -> (x',y)
let lBot bb = match bb with {LTop=(x,y); RBot=(x',y')} -> (x,y')

let sheetDefault = {
    SheetX =1000
    SheetY = 1000
    SheetLeft = 0
    SheetTop = 0
    CanvasX = CommonTypes.draw2dCanvasWidth
    CanvasY = CommonTypes.draw2dCanvasHeight
    Zoom = 1.0
    }


/// get current Draw2D schematic sheet scroll and zoom position
/// component positions are always set or returned as unscaled, and with no offset
/// sheet values, offsets, and scaling change what portion of draw2d canvas is seen
let scrollData model =
    let scrollArea model  = model.Diagram.GetScrollArea()
    let zoomOpt model = model.Diagram.GetZoom()
    match scrollArea model, zoomOpt model with 
        | Some a, Some z -> 
            //printfn "Area = (%d,%d,%d,%d, %.2f)" a.Width a.Height a.Left a.Top z
            let mag n = (float n * z)
            let mag' n = min (float n) (float n * z)
            
            {
                SheetX = mag' a.Width |> int
                SheetY = mag' a.Height |> int
                SheetLeft = mag a.Left |> int
                SheetTop = mag a.Top |> int
                Zoom = z
                CanvasX = CommonTypes.draw2dCanvasWidth
                CanvasY = CommonTypes.draw2dCanvasHeight
            }
        | _ -> sheetDefault

/// alter the zoom settings according to mag and zoomcentre.
/// mag > 1 => zoom in, mag < 1 => zoom out
/// zoomCentre specified coordinates on sheet that stay unchanged by zoom.
/// If zoomCentre is None magnify from centre of visible sheet.
let changeZoom (model:Model)  (zoomCentre: (int * int) option) (mag:float) (sd:ScrollPos)=
    let zoom' =
        let maxZoom = max (float sd.CanvasX / float sd.SheetX) (float sd.CanvasY / float sd.SheetY)
        let minZoom = 0.2 // TODO - work this out from component bounding boxes
        assertThat (mag > 0.01 && mag < 100.) (sprintf "mag %A  (< 0.01 or > 100) is not allowed: zoom is normally in range approx 0.2 - 5" mag)
        match float sd.Zoom / mag with
        | z when z > maxZoom -> 
            printfn "Zoom maximum of %.1f reached" maxZoom
            maxZoom
        | z when z < minZoom ->
            printfn "Zoom minimum of %.1f reached" minZoom
            minZoom
        | z -> z
    let x0,y0 =
        match zoomCentre with
        | None -> sd.SheetLeft + sd.SheetX/2, sd.SheetTop + sd.SheetY/2
        | Some(x,y) -> x,y
    let zr = zoom' / sd.Zoom
    let left' = max 0. ((float sd.SheetLeft) + float (x0-sd.SheetLeft)*(1. - zr))
    let top' = max 0. ((float sd.SheetTop) + float (y0-sd.SheetTop)*(1. - zr))
    let sa  = model.Diagram.GetScrollArea()

    //printfn "BEFORE:\nsd=%A\nsa=%A\nleft'=%f\ntop'=%f\nzoom'=%f\nzoom=%f" sd sa left' top' zoom' sd.Zoom
    model.Diagram.SetScrollZoom (int (left'/zr)) (int (top'/zr)) zoom'
    //printfn "AFTER:\nsd=%A\nsa=%A\n\n"  (scrollData model) (model.Diagram.GetScrollArea())
 
let zoomDiagram (mag: float) (model:Model) = 
    scrollData model
    |> changeZoom model None mag

let computeBoundingBox (boxes: Bbox list) =
    let bbMin = fst (List.minBy (fun xyPos -> fst xyPos.LTop) boxes).LTop , snd (List.minBy (fun xyPos -> snd xyPos.LTop) boxes).LTop
    let bbMax = fst (List.maxBy (fun xyPos -> fst xyPos.RBot) boxes).RBot , snd (List.maxBy (fun xyPos -> snd xyPos.RBot) boxes).RBot
    {LTop=bbMin; RBot=bbMax}

let computeVertexBBox (conn:Connection) =
    let verts = conn.Vertices
    if verts = [] then  failwithf "computeVertexBBox called with empty list of vertices!"
    let intFst = fst >> int
    let intSnd = snd >> int
    let bbMin = (List.maxBy (fun (x,y) -> x) verts |> intFst), (List.minBy (fun (x,y) -> y) verts |> intSnd)
    let bbMax = (List.maxBy (fun (x,y) -> x) verts |> intFst), (List.minBy (fun (x,y) -> y) verts |> intSnd)
    {LTop=bbMin; RBot=bbMax}


/// Choose a good position to place the next component on the sheet based on where existing
/// components are placed. One of 3 heuristics is chosen.
//  Return (x,y) coordinates as accepted by draw2d.
let getNewComponentPosition (model:Model) =

    let maxX = 60
    let maxY = 60
    let offsetY = 30
   
    let meshSize1 = 21
    let meshSize2 = 3

    let sDat = scrollData model

    let bbTopLeft = {LTop=(sDat.SheetLeft,sDat.SheetTop); RBot=(sDat.SheetLeft,sDat.SheetTop)}

    let isFullyVisible (x,y) = x >= sDat.SheetLeft + maxX/2 && y >= sDat.SheetTop + maxY/2  && x < sDat.SheetLeft + sDat.SheetX - maxX && y < sDat.SheetTop + sDat.SheetY - maxY
    let isPartlyVisible (x,y) = x >= sDat.SheetLeft - maxX && y >= sDat.SheetTop - maxY  && x < sDat.SheetLeft + sDat.SheetX  && y < sDat.SheetTop + sDat.SheetY

    let componentPositions , boundingBox, comps  =
        match model.Diagram.GetCanvasState () with
        | None -> 
            printfn "No canvas detected!"
            [bbTopLeft],bbTopLeft, []
        | Some jsState ->
            let comps,conns = Extractor.extractState jsState
            let xyPosL =
                comps
                |> List.map (fun co -> {LTop=(co.X,co.Y); RBot=(co.X+co.W,co.Y+co.H)})
                |> List.filter (fun co -> isPartlyVisible co.LTop)
            if xyPosL = [] then 
                [bbTopLeft],bbTopLeft, [] // add default top left component to keep code from breaking
            else
                xyPosL, computeBoundingBox xyPosL, comps
    /// x value to choose for y offset heuristic
    let xDefault =
        componentPositions
        |> List.filter (fun bb -> isPartlyVisible bb.LTop)
        |> List.map (fun bb -> bb.LTop)
        |> List.minBy snd
        |> fst
        |> (fun x -> min x (sDat.SheetX - maxX))
        

    /// y value to choose for x offset heuristic
    let yDefault =
        componentPositions
        |> List.filter (fun bb -> isPartlyVisible bb.LTop)
        |> List.map (fun bb -> bb.LTop)
        |> List.minBy fst
        |> snd
        |> (fun y -> min y (sDat.SheetY - maxY))

    /// work out the minimum Euclidean distance between (x,y) and any existing component
    let checkDistance (compBb) =
        let {LTop=(xRef,yRef)} = compBb
        let dir (x,y) bb = 
            let d1 =
                match x < fst bb.LTop, x <= fst bb.RBot with
                | true, _ -> LEFT
                | _, false -> RIGHT
                | _ -> MID
         
            let d2 =
                match y < snd bb.LTop, y <= snd bb.RBot with
                | true, _ -> TOP
                | _, false -> BOTTOM
                | _ -> MID
            (d2,d1)
        
        let avg x x' = (float x + float x' ) / 2.

        let euc (x,y) (x',y') (x'',y'') = 
            let (xx,yy) = avg x' x'', avg y' y''
            sqrt( (float x - xx)**2. + (float y - yy)**2.)

        let euclidean (pt:int*int) (bb:Bbox) = 
            let {LTop=(x1,y1); RBot=(x2,y2)} = bb
            match dir pt bb with
            | TOP, LEFT -> euc pt bb.LTop bb.LTop
            | TOP, MID -> euc pt bb.LTop (rTop bb)
            | TOP, RIGHT -> euc pt (rTop bb) (rTop bb)
            | BOTTOM, LEFT -> euc pt (lBot bb) (lBot bb)
            | BOTTOM, MID -> euc pt (lBot bb) bb.RBot
            | BOTTOM, RIGHT -> euc pt bb.RBot bb.RBot
            | MID, LEFT -> euc pt bb.LTop (lBot bb)
            | MID, MID -> - 0.
            | MID, RIGHT -> euc pt (rTop bb) bb.RBot
            | x -> failwithf "What? '%A' Can't happen based on definition of dir!" x
        
        let euclideanBox (bb:Bbox) (bb1:Bbox) =
            let d = List.min [ euclidean bb.RBot bb1 ; euclidean bb.LTop bb1; euclidean (rTop bb) bb1; euclidean (lBot bb) bb1 ]
            match d with
            | 0. ->
                let {LTop=(x1,y1); RBot=(x2,y2)} = bb
                euc (int(avg x1 x2), int(avg y1 y2))  bb1.RBot bb1.LTop - float (maxX + maxY)
            | x -> x

        componentPositions
        |> List.filter (fun {LTop=(x,y)} -> abs(x-xRef) < 3*maxX && abs(y-yRef) < 3*maxY)
        |> List.map (euclideanBox compBb)
        |> (function |[] -> float (sDat.SheetX + sDat.SheetY) | lst -> List.min lst)

            

    let xyToBb (x,y) = {LTop=(x,y); RBot=(x+maxX,y+maxY)}

    /// get from model the correct draw2d coords of the last component added.
    let lastCompPos =
        match model.CreateComponent with
        | None -> None
        | Some cComp -> 
            match List.tryFind (fun (comp:Component) -> comp.Id = cComp.Id) comps with
            | Some comp -> Some (comp.X, comp.Y, comp.H, comp.W)
            | None -> None

    let mesh (num:int) (low:int) (high:int) =
        [0..num-1] |> List.map (fun i ->  low + (i*(high-low)) / num)

    match boundingBox.RBot, lastCompPos with
    | _ when boundingBox = bbTopLeft -> 
        // Place first component on empty sheet top middle
        //int (float (sDat.SheetLeft  + sDat.SheetX / 2 + offsetY) * sDat.Zoom), int (float sDat.SheetTop * sDat.Zoom)
        sDat.SheetLeft + sDat.SheetX / 2 - maxX / 2, sDat.SheetTop + maxY // TODO - make this scroll aware
    | _, Some (x,y,h,w) when checkDistance {LTop=(x,y+h+offsetY); RBot=(x+w,y+2*h+offsetY)} > float 0 && y + h + offsetY < sDat.SheetTop + sDat.SheetY - maxY -> 
        // if possible, place new component just below the last component placed, even if this has ben moved.
        x, y + h + offsetY
    | (_,y),_ when y < sDat.SheetY + sDat.SheetTop - 2*maxY && y > sDat.SheetTop -> 
        // if possible, align horizontally with vertical offset from lowest component
        // this case will ensure components are stacked vertically (which is usually wanted)
        xDefault, y + maxY
    | (x,_),_ when x < sDat.SheetX + sDat.SheetLeft - 2*maxX && x > sDat.SheetTop -> 
        // if possible, next choice is align vertically with horizontal offset from rightmost component
        // this case will stack component horizontally
        x + maxX, yDefault
    | _ ->
        // try to find some free space anywhere on the sheet
        // do a coarse search for largest Euclidean distance to any component's worst case bounding box
        List.allPairs (mesh meshSize1 (sDat.SheetLeft+maxX) (sDat.SheetLeft+sDat.SheetX-maxX)) (mesh meshSize1 (sDat.SheetTop+maxY) (sDat.SheetTop+sDat.SheetY-maxY))
        |> List.map xyToBb
        |> List.sortByDescending (checkDistance)
        |> List.truncate 10
        //|> (fun lst -> printfn "Search1:%A" (List.zip lst (lst |> List.map checkDistance)); lst)
        |> List.collect (fun {LTop=(xEst,yEst)} ->
                //now do the same thing locally with a narrower search pitch
                let mX = sDat.SheetX / (3*meshSize1) + 1
                let mY = sDat.SheetY/ (3*meshSize1) + 1
                //printfn "xest=%d yEst=%d mX = %d mY = %d" xEst yEst mX mY
                List.allPairs (mesh meshSize2 (xEst - mX) (xEst + mX)) (mesh meshSize2 (yEst - mY) (yEst + mY))
                |> List.distinct
                |> List.filter (isFullyVisible) // delete anything too near edge
                |> List.map xyToBb
                //|> (fun lst -> printfn "Narrow: \n%A\n\n" ((List.zip lst (lst |> List.map checkDistance)) |> List.map (sprintf "%A") |> String.concat "\n"); lst)
                |> (function | [] -> [] | lst -> List.maxBy checkDistance lst |> fun bb -> [bb]))
        |> (function | []  -> let pt = sDat.SheetLeft+sDat.SheetX/2,sDat.SheetTop+sDat.SheetY/2
                              {LTop= pt; RBot = pt}
                     | lst -> List.maxBy checkDistance lst)
        |> (fun bb -> bb.LTop)
    |> (fun (x,y) -> printf "Pos=(%d,%d)" x y; (x,y))
        

 
let getPortNames (cType: ComponentType) =
    match cType with
    | DFF |Register _ -> ["D"], ["Q"]
    | DFFE | RegisterE _ -> ["D";"En"], ["Q"]
    | Constant _ -> [], ["Out"]
    | And | Or | Nand | Nor | Xor | Xnor -> ["IN1";"IN2"], ["OUT"]
    | NbitsAdder _ -> ["Cin"; "A"; "B"], ["Sum";"Cout"]
    | Decode4 -> ["Sel";"Data"], ["0";"1";"2";"3"]
    | Not | BusSelection _ -> ["In"],["Out"]
    | Mux2 -> ["0";"1";"Sel"],["Out"]
    | Demux2 ->["In";"Sel"],["0";"1"]
    | ROM _ | AsyncROM _ -> ["Addr"],["Data"]
    | RAM _ -> ["Addr";"Data-in";"Write";],["Data-out"]
    | MergeWires -> ["MSWire"; "LSWire"], ["Out"]
    | SplitWire n -> ["In"],[ sprintf "MS-%d-bits" n; "LS-bits"]
    | IOLabel | Input _ | Output  _-> failwithf "What? Waveforms for %A should not have names looked up since symbol name is used for the (only) waveform" cType
    | Custom _ -> failwithf "Custom component port names not yet implemented!"

/// p must be the component port (with number) not the connection port (no number). PortId can be used to look up one from the other.
/// Returns the port name.
let lookupPortName (comp: Component) (p:Port) =
    let nameList = 
        match p.PortType with
        | PortType.Input -> fst (getPortNames comp.Type)
        | PortType.Output -> snd (getPortNames comp.Type)
    match p.PortNumber with
    | None -> failwithf "can't lookup port on connection with no number"
    | Some n -> 
        match List.tryItem n nameList with
        | None -> failwithf "What? %A has lists %A so can't lookup up %A %d port" comp.Type (getPortNames comp.Type) p.PortType n
        | Some name -> name

let lookupComponentAndPortName (conn: Connection) (isTarget: bool) =
    failwithf "Not implemented yet"



    

        

let private menuItem label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick ] ]
        [ str label ]

let private createComponent comp label model dispatch =
    let x,y = getNewComponentPosition model
    match model.Diagram.CreateComponent comp label x y with
    | Some jsComp -> 
        Extractor.extractComponent jsComp
        |> SetCreateComponent 
        |> dispatch
        |> ignore
    | None -> ()
    ReloadSelectedComponent model.LastUsedDialogWidth |> dispatch

let stdLabel (compType: ComponentType) (model:Model) =
    (*
    Custom component I/O names are the names of the corresp in and out connections.
    Naming wires. If driven by a splitwire / mergewire etc go to the driving components. Include bit numbers. Concatenate via commas multiple outputs when they are merged.
    *)
    let prefix = 
        match compType with
        | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
        | Mux2 -> "MUX"
        | Demux2 -> "DM"
        | NbitsAdder _ -> "A"
        | DFF | DFFE -> "FF"
        | Register _ | RegisterE _ -> "REG"
        | AsyncROM _ -> "AROM"
        | ROM _ -> "ROM"
        | RAM _ -> "RAM"
        | Custom c -> c.Name
        | _ -> ""

    let samePrefPlusNum (word: string)  = 
        match word.[0..String.length prefix - 1] = prefix with
        | true -> 
            word.[String.length prefix..String.length word - 1]
            |> Seq.forall Char.IsDigit
            |> function
               | true -> [ int word.[String.length prefix..String.length word - 1] ]
               | false -> []
        | false -> []

    let sortedLst2ind lst =
        match List.tryFind (fun (a,b) -> b > a + 1) (List.pairwise lst) with
        | None when List.length lst = 0 -> -1
        | None when List.length lst = 1 -> lst.[0]
        | None -> lst.[List.length lst - 1]
        | Some (a,_) -> a
        |> (+) 1

    match model.Diagram.GetCanvasState () with
    | None when prefix <> "" -> prefix + "0"
    | Some jsState when prefix <> "" -> 
        extractState jsState
        |> fst
        |> List.map (fun c -> c.Label) 
        |> List.collect samePrefPlusNum
        |> List.sort
        |> sortedLst2ind
        |> string
        |> (+) prefix
    | _ -> ""

let createCompStdLabel comp model dispatch =
    createComponent comp (stdLabel comp model) model dispatch

let private makeCustom model (loadedComponent: SimulatorTypes.LoadedComponent) =
    menuItem loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = loadedComponent.InputLabels
            OutputLabels = loadedComponent.OutputLabels
        }
        model.Diagram.CreateComponent custom (stdLabel custom model) 100 100
        |> ignore
    )

let private makeCustomList model =
    match model.CurrProject with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.map (makeCustom model)

let private createIOPopup hasInt typeStr compType (model:Model) dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = model.LastUsedDialogWidth
    let body = 
        match hasInt with
        | true -> dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
        | false -> dialogPopupBodyOnlyText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            createComponent (compType inputInt) (formatLabelFromType (compType inputInt) inputText) model dispatch
            if hasInt then dispatch (ReloadSelectedComponent inputInt)
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createNbitsAdderPopup (model:Model) dispatch =
    let title = sprintf "Add N bits adder"
    let beforeInt =
        fun _ -> str "How many bits should each operand have?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            printfn "creating adder %d" inputInt
            createCompStdLabel (NbitsAdder inputInt) {model with LastUsedDialogWidth = inputInt} dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createSplitWirePopup model dispatch =
    let title = sprintf "Add SplitWire node" 
    let beforeInt =
        fun _ -> str "How many bits should go to the top wire? The remaining bits will go to the bottom wire."
    let intDefault = 1
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            createCompStdLabel (SplitWire inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createConstantPopup model dispatch =
    let title = sprintf "Add Constant" 
    let beforeInt2 =
        fun _ -> str "What is the decimal value of the constant?"
    let beforeInt =
        fun _ -> str "How many bits has wire carrying the constant?"
    let intDefault = 1
    let intDefault2 = 0
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, intDefault2) dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let width = getInt dialogData
            let constant = getInt2 dialogData
            createCompStdLabel (Constant(width,constant)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1 || getInt dialogData > 32
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createBusSelectPopup model dispatch =
    let title = sprintf "Add Bus Selection node" 
    let beforeInt2 =
        fun _ -> str "Which input bit is the least significant output bit?"
    let beforeInt =
        fun _ -> str "How many bits width is the output bus?"
    let intDefault = 1
    let intDefault2 = 0
    let body = dialogPopupBodyTwoInts (beforeInt,beforeInt2) (intDefault, intDefault2) dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let width = getInt dialogData
            let lsb = getInt2 dialogData
            createCompStdLabel (BusSelection(width,lsb)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1 || getInt2 dialogData < 0
    dialogPopup title body buttonText buttonAction isDisabled dispatch


let private createRegisterPopup regType (model:Model) dispatch =
    let title = sprintf "Add Register" 
    let beforeInt =
        fun _ -> str "How wide should the register be (in bits)?"
    let intDefault = model.LastUsedDialogWidth
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            createCompStdLabel (regType inputInt) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createMemoryPopup memType model dispatch =
    let title = "Create memory"
    let body = dialogPopupBodyMemorySetup model.LastUsedDialogWidth dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let addressWidth, wordWidth = getMemorySetup dialogData
            let memory = {
                AddressWidth = addressWidth
                WordWidth = wordWidth
                Data = List.replicate (pow2 addressWidth) (int64 0) // Initialise with zeros.
            }
            createCompStdLabel (memType memory) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            let addressWidth, wordWidth = getMemorySetup dialogData
            addressWidth < 1 || wordWidth < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private makeMenuGroup title menuList =
    details [Open true] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let viewCatalogue model dispatch =
        let catTip name func (tip:string) = JSHelpers.tipRef "Cat_" "left" name (menuItem name func) tip
        let catTipInstall el = 
                if not (isNull el) then
                    printfn "Installing cat"
                    JSHelpers.tippy (JSHelpers.tippyOpts1 "left" ) "[data-tippy-content]" |> ignore

 
        let catTip1 name func (tip:string) = 
            div [Props.Ref (fun element -> if not (isNull element) then element.setAttribute("data-tippy-content",tip))] [menuItem name func]
        Menu.menu [Props [Class "py-1"; Ref catTipInstall]]  [
                makeMenuGroup
                    "Input / Output"
                    [ catTip1 "Input"  (fun _ -> createIOPopup true "input" Input model dispatch) "Input connection to current sheet: one or more bits"
                      catTip1 "Output" (fun _ -> createIOPopup true "output" Output model dispatch) "Output connection from current sheet: one or more bits"
                      catTip1 "Constant" (fun _ -> createConstantPopup model dispatch) "Define a one or more bit constant value, \
                                                                                        e.g. 0 or 1 to drive an unused input"
                      catTip1 "Wire Label" (fun _ -> createIOPopup false "label" (fun _ -> IOLabel) model dispatch) "Labels with the same name connect \
                                                                                                                     together wires or busses"]
                makeMenuGroup
                    "Buses"
                    [ catTip1 "MergeWires"  (fun _ -> createComponent MergeWires "" model dispatch) "Use Mergewire when you want to \
                                                                                   join the bits of a two busses to make a wider bus"
                      catTip1 "SplitWire" (fun _ -> createSplitWirePopup model dispatch) "Use Splitwire when you want to split the \
                                                                                         bits of a bus into two sets"
                      catTip1 "Bus Select" (fun _ -> createBusSelectPopup model dispatch) "Bus Select output connects to one or 
                                                                                            more selected bits of its input" ]
                makeMenuGroup
                    "Gates"
                    [ menuItem "Not"  (fun _ -> createCompStdLabel Not model dispatch)
                      menuItem "And"  (fun _ -> createCompStdLabel And model dispatch)
                      menuItem "Or"   (fun _ -> createCompStdLabel Or model dispatch)
                      menuItem "Xor"  (fun _ -> createCompStdLabel Xor model dispatch)
                      menuItem "Nand" (fun _ -> createCompStdLabel Nand model dispatch)
                      menuItem "Nor"  (fun _ -> createCompStdLabel Nor model dispatch)
                      menuItem "Xnor" (fun _ -> createCompStdLabel Xnor model dispatch) ]
                makeMenuGroup
                    "Mux / Demux"
                    [ catTip1 "Mux2" (fun _ -> createCompStdLabel Mux2 model dispatch) "Selects one of its two input busses 
                                                                            to be the output. Adjusts bus width to match."
                      menuItem "Demux2" (fun _ -> createCompStdLabel Demux2 model dispatch) 
                      catTip1 "Decode4" (fun _ -> createCompStdLabel Decode4 model dispatch) "The output numbered by the binary value 
                                                                                            of the 2 bit sel input is equal to Data, the others are 0"]
                makeMenuGroup
                    "Arithmetic"
                    [ menuItem "N bits adder" (fun _ -> createNbitsAdderPopup model dispatch) ]
                makeMenuGroup
                    "Flip Flops and Registers"
                    [ menuItem "D-flip-flop" (fun _ -> createCompStdLabel DFF model dispatch)
                      menuItem "D-flip-flop with enable" (fun _ -> createCompStdLabel DFFE model dispatch)
                      menuItem "Register" (fun _ -> createRegisterPopup Register model dispatch)
                      menuItem "Register with enable" (fun _ -> createRegisterPopup RegisterE model dispatch) ]
                makeMenuGroup
                    "Memories"
                    [ catTip1 "ROM (asynchronous)" (fun _ -> createMemoryPopup AsyncROM model dispatch) "This is combinational: \
                                                the output is available in the same clock cycle that the address is presented"
                      catTip1 "ROM (synchronous)" (fun _ -> createMemoryPopup ROM model dispatch) "A ROM whose output contains \
                                                the addressed data in the clock cycle after the address is presented"
                      catTip1 "RAM" (fun _ -> createMemoryPopup RAM model dispatch)  "A RAM whose output contains the addressed \
                                               data in the clock cycle after the address is presented"]
                makeMenuGroup
                    "This project"
                    (makeCustomList model)
            ]
