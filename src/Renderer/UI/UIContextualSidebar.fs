module UIContextualSideBar
open EEExtensions
open Fulma

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props


open ElectronAPI
open JSHelpers
open Helpers
open ModelType
open CommonTypes
open CodeEditorHelpers
open DrawModelType
open Browser





/// A button on a contextual sidebar
/// Buttons take in a model and return a message
type SidebarButton = {
    ButtonClassNames: string; // for colours with fulma
    ButtonText: string; // for the text on the button
    ButtonAction: ModelType.Model -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit

}


type SidebarOptions = {
    ExtraStyle: CSSProp list;
    TitleText: string;
    SideBarButtons: SidebarButton list;
    Cancellable: bool;
}

/// Constructs a button component based on SidebarButton information
let createButton buttonInfo (dispatch: Msg -> Unit) (model: ModelType.Model)  : ReactElement =
    Button.button [
        Button.Option.Props[ClassName ("button " + buttonInfo.ButtonClassNames);];
        Button.Props[Style [ Margin "3px" ]];
        Button.OnClick (buttonInfo.ButtonAction model dispatch)
    ] [ str buttonInfo.ButtonText ]

/// CSS for the sidebar.
// It has absolute position so it can be placed on top of the rightbar. CSSProp.Left set to 2px so the dividerbar is visible
// PaddingTop set to 20px to look like it covers the tabbing bar
let sidebarDivCSS = [Position PositionOptions.Relative; ZIndex 100; Background "white"; Width "100%"; PaddingTop "20px"; OverflowY OverflowOptions.Auto]

// let mutable sidebarDiv:Types.Element option = None

/// A simple sidebar with a title and a list of buttons, with a static body
/// Creates the sidebar with a dynamic body
let buildSimpleSidebar (options: SidebarOptions) (body: (Msg -> Unit) -> Model ->  ReactElement) =
    fun (dispatch: Msg -> Unit) (model: Model) ->
        let buttons = options.SideBarButtons |> List.map (fun buttonInfo -> createButton buttonInfo dispatch model )
        let maybeCancel =
            if options.Cancellable then
                Some (Button.button [
                    Button.Option.Props[ClassName "button is-light"];
                    Button.Props[Style [ Margin "0px 3px" ]];
                    Button.Props[OnClick (fun _ -> dispatch (CloseContextualSidebar)) ]
                ] [ str "Close" ])
            else None


        div [  Style (options.ExtraStyle @ sidebarDivCSS);] [
        div [ Style [Margin "30px 20px"]] [

            Heading.h4 [] [ str options.TitleText ]
            div [ Style [ Margin "15px 0" ]] [ body dispatch model ]
            div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; FlexDirection FlexDirection.Row ] ] [
                div [ Style [  ] ] buttons
                div [ Style [  Display DisplayOptions.Flex; Flex "0"; AlignItems AlignItemsOptions.FlexEnd] ] (maybeCancel |> Option.toList)
            ]
        ]
        ]

/// To be called in MainView
let viewSidebar (model: ModelType.Model) dispatch : ReactElement option =
    match model.ContextualSidebarViewFunction with
    | Some contextualSidebar -> Some (contextualSidebar dispatch model)
    | None -> None





module ColourGenerator =
    module Constants =
        // A colour is considered too close to another if the distance is less than minimumColourDistance (thus making it invalid)
        let minimumColourDistance = 23.
        // A colour is considered too close to a blacklisted colour if the distance is less than blackListedColourDistance (thus making it invalid)
        let blackListedColourDistance = 25.
        // how many times to resample the HSL space if an invalid colour is generated (too close to the blacklisted colours or existing colours)
        let trialLimit = 2500
        let satRange = (30.,80.)
        let lumRange = (75.,92.)
        // an increment in HSL space
        let hIncrement = 41.
        let sIncrement = 11.
        let lIncrement = 5. //minimumColourDistance/3.**0.5

    open System
    open Constants

    // Define a color type for RGB colors
    type ColourRGB = { R: float; G: float; B: float }
    type ColourHSL = { H: float; S: float; L: float }


    /// Convert RGB to HSL (helper function)
    let rgbToHSL (color: ColourRGB) : ColourHSL =
        let (r: float), (g: float), (b: float) = color.R, color.G, color.B
        let max = Math.Max(r, Math.Max(g, b))
        let min = Math.Min(r, Math.Min(g, b))
        let l = (max + min) / 2.0
        let (h, s) =
            if max = min then
                0.0, 0.0  // achromatic, hence no saturation and hue is 0
            else
                let d = max - min
                let s = if l > 0.5 then d / (2.0 - max - min) else d / (max + min)
                let h =
                    if max = r then
                        (g - b) / d + (if g < b then 6.0 else 0.0)
                    elif max = g then
                        (b - r) / d + 2.0
                    else
                        (r - g) / d + 4.0
                h * 60.0, s
        { H = h; S = s*100.; L = l*100. }


    /// Convert HSL to RGB (helper function)
    /// Note that sat and lum are in 0-100
    let hslToRGB (color: ColourHSL) : ColourRGB =
        let (h: float), (s: float), (l: float) = color.H, color.S/100.0, color.L/100.0
        let c = (1.0 - Math.Abs(2.0 * l - 1.0)) * s
        let x = c * (1.0 - Math.Abs((h / 60.0 % 2.0) - 1.0))
        let m = l - c / 2.0
        let (r, g, b) =
            match h with
            | h when h < 60.0 -> (c, x, 0.0)
            | h when h < 120.0 -> (x, c, 0.0)
            | h when h < 180.0 -> (0.0, c, x)
            | h when h < 240.0 -> (0.0, x, c)
            | h when h < 300.0 -> (x, 0.0, c)
            | _ -> (c, 0.0, x)
        { R = r + m; G = g + m; B = b + m }

    /// Convert RGB to Hex
    let rgbToHex (color: ColourRGB) : string =
            let r = int (color.R * 255.0)
            let g = int (color.G * 255.0)
            let b = int (color.B * 255.0)
            String.Format("#{0:X2}{1:X2}{2:X2}", r, g, b)

    /// Convert Hex to HSL
    let hexToRGB (color: string) =
        // drop the # in front (if it exists)
        let hex = if color.StartsWith("#") then color.Substring(1) else color
        let r = float (Convert.ToInt32(hex.Substring(0, 2), 16)) / 255.0
        let g = float (Convert.ToInt32(hex.Substring(2, 2), 16)) / 255.0
        let b = float (Convert.ToInt32(hex.Substring(4, 2), 16)) / 255.0
        { R = r; G = g; B = b }


    /// Calculate perceived distance between two HSL colors.
    /// Note that dh (difference in hue) is given more weight, so generated colours are less likely to clash with UI colours
    let colourPerceivedDistance (c1: ColourHSL) (c2: ColourHSL) =
        let dh: float = c1.H - c2.H
        let ds: float = c1.S - c2.S
        let dl: float = c1.L - c2.L

        ( (dh * dh) + (ds * ds) + (dl * dl) )**0.5

    /// List of blacklisted colors in RGB
    let blacklistedColorsRGB = [
        { R = 1.0; G = 1.0; B = 0.85 } // #FFFFD9
        { R = 0.678; G = 0.847; B = 0.902 } // # CSS Light Blue or #ADD8E6
        { R = 0.91; G = 0.816; B = 0.663 } // #E8D0A9
        { R = 0.564; G = 0.933; B = 0.564 } // #90EE90 aka CSS Light Green
    ]

    let blacklistedColoursHSL = blacklistedColorsRGB |> List.map rgbToHSL

    /// Checks if a generated colour is valid
    let isColourValid (newSample: ColourHSL) (existingColours : ColourHSL list) : bool =
        // for every existingColour, make sure it is not too close to the newSample

        let farAwayFromExistingColours = not (List.exists (fun existingColour ->
            let distance = colourPerceivedDistance newSample existingColour
            distance < minimumColourDistance
        ) existingColours)

        let farAwayFromBlacklistedColours = not (List.exists (fun blacklistedColour ->
            let distance = colourPerceivedDistance newSample blacklistedColour
            distance < blackListedColourDistance
        ) blacklistedColoursHSL)

        farAwayFromExistingColours && farAwayFromBlacklistedColours

    let generateBaseHSLColour (random: Random) : ColourHSL =
        let h: float = random.NextDouble() * 360.0
        let s: float = float (random.NextDouble()*(snd satRange - fst satRange) + fst satRange)
        let l: float = float (random.NextDouble()*(snd lumRange - fst lumRange) + fst lumRange)
        { H = h; S = s; L = l }

    let generateIncrementedHSLColour (sample:ColourHSL) =
        let newH: float = (sample.H + hIncrement) % 360.0
        let newS: float = ((sample.S - (fst satRange) + sIncrement) % (snd satRange - fst satRange)) + fst satRange
        let newL: float = ((sample.L - (fst satRange)  + lIncrement) % (snd lumRange - fst lumRange)) + fst lumRange

        { H = newH; S = newS; L = newL }

    let getNewColour (existingColours : ColourHSL list) =
        // generate a new colour using poisson-disk sampling.
        // make this deterministic.
        let random = Random(existingColours.Length)
        // 1: generate initial sample
        let newSample =
            match existingColours.Length with
            | 0 -> printf "No existing colours, drawing..."; generateBaseHSLColour random
            | _ -> generateIncrementedHSLColour (List.head existingColours)


        // 2: repeat for number of trials
        let rec generateNewColour (sample: ColourHSL) (existingColours : ColourHSL list) (trial : int) =
            match trial with
            | trial when trial > trialLimit + 5 ->
                printf "Exceeded trial limits again. Generating random colour"
                generateBaseHSLColour (Random existingColours.Length)
            | trial when trial > trialLimit ->
                printf "Exceeded trial limits, trying with less constraints (avoid blacklisted colours only)"
                let newSample = generateIncrementedHSLColour sample
                if isColourValid newSample blacklistedColoursHSL then newSample
                else generateNewColour newSample existingColours (trial + 1)
            | _ ->
                let newSample = generateIncrementedHSLColour sample
                // printf "Trial: %d with colour: %A\n" trial newSample
                if isColourValid newSample existingColours then newSample
                else generateNewColour newSample existingColours (trial + 1)


        generateNewColour newSample existingColours 0

    let generateColourFromModel (model: SheetT.Model) =
        let existingModelColours =
            model.Wire.Symbol.GroupInfoMap
            |> Map.values
            |> Array.toList
            // convert from RGB hex (with # in front) to HSL
            |> List.map (fun (groupInfo: GroupInfo) -> groupInfo.Colour |> hexToRGB |> rgbToHSL)


        // let sheetDispatch = (fun sMsg -> dispatch (Sheet sMsg))

        let newHSLColour = getNewColour existingModelColours
        let newGroupInfo = {
            Id = GroupId (uuid());
            CreationDate = DateTime.Now;
            Colour = (newHSLColour |> hslToRGB |> rgbToHex) ;
        }

        let closestDistanceToBlacklisted =
            blacklistedColoursHSL
            |> List.map (fun blacklistedColour -> colourPerceivedDistance newHSLColour blacklistedColour)
            |> List.min

        printf "Generated colour: %s Closest Distance To Blacklist: %A" (newHSLColour |> hslToRGB |> rgbToHex) (closestDistanceToBlacklisted.ToString("F2"))
        printf "Existing Colours: %A" (existingModelColours |> List.map (fun c -> c |> hslToRGB |> rgbToHex))

        newGroupInfo.Colour





// module ColourGenerator =
//     open System

//     // Define a color type for RGB colors
//     type Color = { R: float; G: float; B: float }

//     /// Convert HSL to RGB (helper function)
//     let hslToRgb (h: float) (s: float) (l: float): Color =
//         let c = (1.0 - Math.Abs(2.0 * l - 1.0)) * s
//         let x = c * (1.0 - Math.Abs((h / 60.0 % 2.0) - 1.0))
//         let m = l - c / 2.0
//         let (r, g, b) =
//             match h with
//             | h when h < 60.0 -> (c, x, 0.0)
//             | h when h < 120.0 -> (x, c, 0.0)
//             | h when h < 180.0 -> (0.0, c, x)
//             | h when h < 240.0 -> (0.0, x, c)
//             | h when h < 300.0 -> (x, 0.0, c)
//             | _ -> (c, 0.0, x)
//         { R = r + m; G = g + m; B = b + m }

//     /// Calculate Euclidean distance between two colors
//     let colorDistance (c1: Color) (c2: Color) =
//         let dr = c1.R - c2.R
//         let dg = c1.G - c2.G
//         let db = c1.B - c2.B
//         Math.Sqrt(dr * dr + dg * dg + db * db)

//     /// List of blacklisted colors in RGB
//     let blacklistedColors = [
//         { R = 1.0; G = 1.0; B = 0.85 } // #FFFFD9
//         { R = 0.678; G = 0.847; B = 0.902 } // # CSS Light Blue
//         { R = 0.91; G = 0.816; B = 0.663 } // #E8D0A9
//         { R = 0.564; G = 0.933; B = 0.564 } // #90EE90 aka CSS Light Green
//         { R = 1.0; G = 1.0; B = 1.0} //White

//     ]

//     /// Check if a color is too close to any blacklisted colors
//     let isColorTooClose (color: Color) =
//         blacklistedColors |> List.exists (fun blacklisted -> colorDistance color blacklisted < 0.1) // Adjust the threshold as needed

//     /// Generates a color ensuring it's not too close to blacklisted colors
//     let rec generateValidColor random =
//         let hue = (random : Random).NextDouble() * 360.0 // 0 to 360
//         let saturation = 0.30 +  (random : Random).NextDouble() * 0.60 // 0.30 to 0.90
//         let luminance = 0.66 +  (random : Random).NextDouble() * 0.34 // 0.66 to 1.0
//         let color = hslToRgb hue saturation luminance
//         if isColorTooClose color then generateValidColor random
//         else color

//     /// Generates a uniformly distributed HSL color based on the model's draw count and seed
//     let generateColor (model: SheetT.Model) =
//         let random = Random(model.ColourRandomSeed + model.ColourDrawnCount)
//         generateValidColor random

//     /// Top Level Call: given the model, generate a colour and update with the new model
//     let generateColourFromModel (model: SheetT.Model) (dispatch) =
//         let colour = generateColor model
//         let hexColour = "#" + (colour.R * 255.0).ToString("X2") + (colour.G * 255.0).ToString("X2") + (colour.B * 255.0).ToString("X2")
//         let sheetDispatch sMsg = dispatch (Sheet sMsg)
//         sheetDispatch (SheetT.IncrementColourDrawnCount), hexColour
