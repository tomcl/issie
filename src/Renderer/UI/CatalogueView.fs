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

type Bbox = {
    LTop: (int*int)
    RBot: (int*int)
    }

type Direction = TOP | BOTTOM | LEFT | RIGHT | MID

let rTop bb = match bb with {LTop=(x,y); RBot=(x',y')} -> (x',y)
let lBot bb = match bb with {LTop=(x,y); RBot=(x',y')} -> (x,y')

/// Choose a good position to place the next component on the sheet based on where existing
/// components are placed. One of 3 heuristics is chosen.
//  Return (x,y) coordinates as accepted by draw2d.
let getNewComponentPosition (model:Model) =

    let maxX = 60
    let maxY = 60
    let offsetY = 100
    let sheetX = 1000
    let sheetY = 1000
    let meshPitch1 = 45
    let meshPitch2 = 5
    let bbTopLeft = {LTop=(0,0); RBot=(0,0)}

    let componentPositions , boundingBox  =
        match model.Diagram.GetCanvasState () with
        | None -> 
            printfn "No canvas detected!"
            [bbTopLeft],bbTopLeft
        | Some jsState ->
            let comps,conns = Extractor.extractState jsState
            let xyPosL =
                comps
                |> List.map (fun co -> {LTop=(co.X,co.Y); RBot=(co.X+co.W,co.Y+co.H)})
            if xyPosL = [] then 
                [bbTopLeft],bbTopLeft // add default top left component to keep code from breaking
            else
                let bbMin = fst (List.minBy (fun xyPos -> fst xyPos.LTop) xyPosL).LTop , snd (List.minBy (fun xyPos -> snd xyPos.LTop) xyPosL).LTop
                let bbMax = fst (List.maxBy (fun xyPos -> fst xyPos.RBot) xyPosL).RBot , snd (List.maxBy (fun xyPos -> snd xyPos.RBot) xyPosL).RBot
                xyPosL, {LTop=bbMin; RBot=bbMax}
    /// x value to choose for y offset heuristic
    let xDefault =
        componentPositions
        |> List.map (fun bb -> bb.LTop)
        |> List.minBy snd
        |> fst
        |> (fun x -> min x (sheetX - maxX))
        

    /// y value to choose for x offset heuristic
    let yDefault =
        componentPositions
        |> List.map (fun bb -> bb.LTop)
        |> List.minBy fst
        |> snd
        |> (fun y -> min y (sheetY - maxY))

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

        let euclidean (pt:int*int) (bb:Bbox) = 
            let euc (x,y) (x',y') (x'',y'') = 
                let (xx,yy) = avg x' x'', avg y' y''
                sqrt(((float x - xx)**2. + (float y - yy)**2.)/2.)
            match dir pt bb with
            | TOP, LEFT -> euc pt bb.LTop bb.LTop
            | TOP, MID -> euc pt bb.LTop (rTop bb)
            | TOP, RIGHT -> euc pt (rTop bb) (rTop bb)
            | BOTTOM, LEFT -> euc pt (lBot bb) (lBot bb)
            | BOTTOM, MID -> euc pt (lBot bb) bb.RBot
            | BOTTOM, RIGHT -> euc pt bb.RBot bb.RBot
            | MID, LEFT -> euc pt bb.LTop (lBot bb)
            | MID, MID -> 0.
            | MID, RIGHT -> euc pt (rTop bb) bb.RBot
            | x -> failwithf "What? '%A' Can't happen based on definition of dir!" x
        
        let euclideanBox (bb:Bbox) (bb1:Bbox) =
            ((List.min [ euclidean bb.RBot bb1 ; euclidean bb.LTop bb1; euclidean (rTop bb) bb1; euclidean (lBot bb) bb1 ]), bb1)
            |> (fun (x, _) ->
                if x <> 0. then x else
                    let bbAv {LTop=(x,y);RBot=(x',y')} = avg x x', avg y y'
                    let (x,y) = bbAv bb
                    let (x',y')=bbAv bb1
                    -(float maxX) - (float maxY) + sqrt((x - x')**2. + (y - y')**2.))
        componentPositions
        |> List.filter (fun {LTop=(x,y)} -> abs(x-xRef) < 3*maxX && abs(y-yRef) < 3*maxY)
        |> List.map (euclideanBox compBb)
        |> (function |[] -> float (sheetX + sheetY) | lst -> List.min lst)

            

    let xyToBb (x,y) = {LTop=(x,y); RBot=(x+maxX,y+maxY)}

    /// get from model the correct draw2d coords of the last component added.
    let lastCompPos =
        match model.CreateComponent with
        | None -> None
        | Some comp -> 
            match model.Diagram.GetComponentById comp.Id with
            | Ok jsComp -> 
                Extractor.extractComponent jsComp
                |> (fun comp -> Some (comp.X,comp.Y))
            | _  -> None



    match boundingBox.RBot, lastCompPos with
    | _ when boundingBox = bbTopLeft -> 
        // Place first component on empty sheet top middle
        sheetX / 2, maxY
    | _, Some (x,y) when checkDistance {LTop=(x,y+offsetY); RBot=(x+maxX,y+maxY+offsetY)} > float 0 && y + offsetY < sheetY - maxY -> 
        // if possible, place new component just below the last component placed, even if this has ben moved.
        x, y + offsetY
    | (_,y),_ when y < sheetY - 2*maxY  -> 
        // if possible, align horizontally with vertical offset from lowest component
        // this case will ensure components are stacked vertically (which is usually wanted)
        xDefault, y + maxY
    | (x,_),_ when x < sheetX - 2*maxX -> 
        // if possible, next choice is align vertically with horizontal offset from rightmost component
        // this case will stack component horizontally
        x + maxX, yDefault
    | _ ->
        // try to find some free space anywhere on the sheet
        // do a coarse search for largest Euclidean distance to any component's worst case bounding box
        List.allPairs [maxX..meshPitch1..sheetX-maxX] [maxY..meshPitch1..sheetY-maxY]
        |> List.map xyToBb
        |> List.sortByDescending (checkDistance)
        |> List.take 10
        |> List.collect (fun {LTop=(xEst,yEst)} ->
                //now do the same thing locally with a narrower search pitch
                List.allPairs [xEst - meshPitch1/3..meshPitch2..xEst + meshPitch1/3] [yEst - meshPitch1/3..meshPitch2..yEst + meshPitch1/3]
                |> List.filter (fun (x,y) -> x < sheetX-maxX && y < sheetY-maxY) // delete anything too near edge
                |> List.map xyToBb
                |> List.maxBy checkDistance
                |> (fun  bb -> [bb]))
        |> List.maxBy checkDistance
        |> (fun bb -> bb.LTop)
        

 


    

        

let private menuItem label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick ] ]
        [ str label ]

let private makeCustom model loadedComponent =
    menuItem loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = loadedComponent.InputLabels
            OutputLabels = loadedComponent.OutputLabels
        }
        model.Diagram.CreateComponent custom loadedComponent.Name 100 100
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
            createComponent (NbitsAdder inputInt) "" {model with LastUsedDialogWidth = inputInt} dispatch
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
            createComponent (SplitWire inputInt) "" model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
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
            printfn "Reg inutInt=%d" inputInt
            createComponent (regType inputInt) "" model dispatch
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
            createComponent (memType memory) "" model dispatch
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
    Menu.menu [] [
            makeMenuGroup
                "Input / Output"
                [ menuItem "Input"  (fun _ -> createIOPopup true "input" Input model dispatch)
                  menuItem "Output" (fun _ -> createIOPopup true "output" Output model dispatch)
                  menuItem "Wire Label" (fun _ -> createIOPopup false "label" (fun _ -> IOLabel) model dispatch)]
            makeMenuGroup
                "Buses"
                [ menuItem "MergeWires"  (fun _ -> createComponent MergeWires "" model dispatch)
                  menuItem "SplitWire" (fun _ -> createSplitWirePopup model dispatch) ]
            makeMenuGroup
                "Gates"
                [ menuItem "Not"  (fun _ -> createComponent Not "" model dispatch)
                  menuItem "And"  (fun _ -> createComponent And "" model dispatch)
                  menuItem "Or"   (fun _ -> createComponent Or "" model dispatch)
                  menuItem "Xor"  (fun _ -> createComponent Xor "" model dispatch)
                  menuItem "Nand" (fun _ -> createComponent Nand "" model dispatch)
                  menuItem "Nor"  (fun _ -> createComponent Nor "" model dispatch)
                  menuItem "Xnor" (fun _ -> createComponent Xnor "" model dispatch) ]
            makeMenuGroup
                "Mux / Demux"
                [ menuItem "Mux2" (fun _ -> createComponent Mux2 "" model dispatch)
                  menuItem "Demux2" (fun _ -> createComponent Demux2 "" model dispatch) ]
            makeMenuGroup
                "Arithmetic"
                [ menuItem "N bits adder" (fun _ -> createNbitsAdderPopup model dispatch) ]
            makeMenuGroup
                "Flip Flops and Registers"
                [ menuItem "D-flip-flop" (fun _ -> createComponent DFF "" model dispatch)
                  menuItem "D-flip-flop with enable" (fun _ -> createComponent DFFE "" model dispatch)
                  menuItem "Register" (fun _ -> createRegisterPopup Register model dispatch)
                  menuItem "Register with enable" (fun _ -> createRegisterPopup RegisterE model dispatch) ]
            makeMenuGroup
                "Memories"
                [ menuItem "ROM (asynchronous)" (fun _ -> createMemoryPopup AsyncROM model dispatch)
                  menuItem "ROM (synchronous)" (fun _ -> createMemoryPopup ROM model dispatch)
                  menuItem "RAM" (fun _ -> createMemoryPopup RAM model dispatch) ]
            makeMenuGroup
                "This project"
                (makeCustomList model)
        ]
