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

/// Choose a good position to place the next component on the sheet based on where existing
/// components are placed. One of 3 heuristics is chosen.
//  Return (x,y) coordinates as accepted by draw2d.
let getNewComponentPosition (model:Model) =

    let maxX = 120
    let maxY = 120
    let sheetX = 1200
    let sheetY = 1200
    let meshPitch1 = 50
    let meshPitch2 = 5

    let componentPositions , boundingBox  =
        let bbTopLeft = (0,0), (0,0)
        match model.Diagram.GetCanvasState () with
        | None -> 
            printfn "No canvas detected!"
            [0,0],bbTopLeft
        | Some jsState ->
            let comps,conns = Extractor.extractState jsState
            let xyPos =
                comps
                |> List.map (fun co -> co.X,co.Y)
            if xyPos = [] then 
                [0,0],bbTopLeft // add default top left component to keep coe from breaking
            else
                let bbMin = List.minBy fst xyPos |> fst, List.minBy snd xyPos |> snd
                let bbMax = List.maxBy fst xyPos |> fst , List.maxBy snd xyPos |> snd
                xyPos, (bbMin, bbMax)
    /// x value to choose for y offset heuristic
    let xDefault =
        componentPositions
        |> List.minBy snd
        |> fst
        |> (fun x -> min x (sheetX - maxX))

    /// y value to choose for x offset heuristic
    let yDefault =
        componentPositions
        |> List.minBy fst
        |> snd
        |> (fun y -> min y (sheetY - maxY))

    /// work out the minimum Euclidean distance between (x,y) and any existing component
    /// not quite accurate since bounding boxes are not known, but good enough
    let checkDistance (x,y) =
        let euclidean (a, b) = 
            (a-x)*(a-x) + (b-y)*(b-y)
        componentPositions
        |> List.minBy euclidean
        |> euclidean
 
    match boundingBox with
    | (0,0),(0,0) -> 
        // Place first component on empty sheet top middle
        sheetX / 2, maxY
    | _, (_,y) when y < sheetY - 2*maxY -> 
        // if possible, align horizontally with vertical offset from lowest component
        // this case will ensure components are stacked vertically (which is usually wanted)
        xDefault, y + maxY
    | _, (x,_) when x < sheetX - 2*maxX -> 
        // if possible, next choice is align vertically with horizontal offset from rightmost component
        // this case will stack component horizontally
        x + maxX, yDefault
    | _ ->
        // try to find some free space anywhere on the sheet
        // do a coarse search for largest Euclidean distance to any component's worst case bounding box
        // TODO - extract better bounding boxes and use them
        List.allPairs [maxX..meshPitch1..sheetX-maxX] [maxY..meshPitch1..sheetY-maxY]
        |> List.maxBy checkDistance
        |> (fun (xEst,yEst) ->
                //now do the same thing locally with a narrower search pitch
                List.allPairs [xEst - meshPitch1/2..meshPitch2..xEst + meshPitch1/2] [yEst - meshPitch1/2..meshPitch2..yEst + meshPitch1/2]
                |> List.filter (fun (x,y) -> x > maxX && x < sheetX-maxX && y > maxY && y < sheetY-maxY) // delete anything too near edge
                |> List.maxBy checkDistance)
 


    

        

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
    let offset = model.CreateComponentOffset
    model.Diagram.CreateComponent comp label x y |> ignore
    (offset + 50) % 200 |> SetCreateComponentOffset |> dispatch
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
