module UIContextualSideBar
open EEExtensions
open Fulma
open VerilogTypes
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


type ButtonDispatchPayload = | ButtonDispatchPayload of string

/// A button on a contextual sidebar
/// Buttons take in a model and return a message
type SidebarButton = {
    ButtonClassNames: string; // for colours with fulma
    ButtonText: string; // for the text on the button
    ButtonAction: ButtonDispatchPayload -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit
    ButtonPayload: ButtonDispatchPayload

}


type SidebarOptions = {
    ExtraStyle: CSSProp list;
    TitleText: string;
    SideBarButtons: SidebarButton list;
    Cancellable: bool;
}

/// Constructs a button component based on SidebarButton information
let createButton buttonInfo (dispatch: Msg -> Unit) (model: Model)  : ReactElement =
    Button.button [
        Button.Option.Props[ClassName buttonInfo.ButtonClassNames;];
        Button.Props[Style [ Margin "0px 3px" ]];
        Button.OnClick (buttonInfo.ButtonAction buttonInfo.ButtonPayload dispatch)
    ] [ str buttonInfo.ButtonText ]

/// CSS for the sidebar.
// It has absolute position so it can be placed on top of the rightbar. CSSProp.Left set to 2px so the dividerbar is visible
// PaddingTop set to 20px to look like it covers the tabbing bar
let sidebarDivCSS = [Position PositionOptions.Absolute; ZIndex 100; Background "white"; CSSProp.Left "2px"; CSSProp.Right 0; CSSProp.Top 0; CSSProp.Bottom 0; PaddingTop "20px"]

/// A simple sidebar with a title and a list of buttons, with a static body
/// Creates the sidebar with a dynamic body
let buildSimpleSidebar (options: SidebarOptions) (body: Model -> ReactElement) =
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


        div [  Style (options.ExtraStyle @ sidebarDivCSS) ] [
        div [ Style [Margin "30px 20px"]] [

            Heading.h4 [] [ str options.TitleText ]
            div [ Style [ Margin "15px 0" ]] [ body model ]
            div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; FlexDirection FlexDirection.Row ] ] [
                div [ Style [ Flex "1" ] ] buttons
                div [ Style [ Flex "0" ] ] (maybeCancel |> Option.toList)
            ]
        ]
        ]

let viewSidebar (model: ModelType.Model) dispatch : ReactElement option =
    match model.ContextualViewFunction with
    | Some contextualSidebar -> Some (contextualSidebar dispatch model)
    | None -> None



// let buildSimpleSidebar (options: SidebarOptions) (body: Model -> ReactElement) =
//     fun (dispatch: Msg -> Unit)(model:Model) ->
//         let buttons =
//             options.SideBarButtons
//             |> List.map (fun buttonInfo ->
//                 Button.button [ (*ClassName button.ButtonClassNames; OnClick (fun _ -> button.ButtonAction model dispatch)*) ] [ str buttonInfo.ButtonText ]
//             )

//         div [] [
//         Heading.h3 [  ] [ str options.TitleText ]
//         div [] [
//             body model
//         ]
//         div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; Padding "0.5rem"; ] ] [
//         ]
//     ]


module ColourGenerator =

    open System
    // the minimum distance between a valid colour and a blacklisted colour.
    // blacklisted colours are the existing UI colours, so that users do not get confused
    let colourDistanceThreshold = 0.2

    // Define a color type for RGB colors
    type ColorRGB = { R: float; G: float; B: float }
    type ColorHSL = { H: float; S: float; L: float }

    /// Convert HSL to RGB (helper function)
    let hslToRgb (color: ColorHSL) : ColorRGB =
        let (h: float), (s: float), (l: float) = color.H, color.S, color.L
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


    /// Convert RGB to HSL (helper function)
    let rgbToHsl (color: ColorRGB) : ColorHSL =
        let r = color.R
        let g = color.G
        let b = color.B

        let max = Math.Max(r, Math.Max(g, b))
        let min = Math.Min(r, Math.Min(g, b))
        let delta = max - min

        let l = (max + min) / 2.0

        let s =
            if delta = 0.0 then 0.0
            else delta / (if l > 0.5 then 2.0 - max - min else max + min)

        let h =
            if delta = 0.0 then
                0.0
            elif max = r then
                (g - b) / delta + (if g < b then 6.0 else 0.0)
            elif max = g then
                (b - r) / delta + 2.0
            else
                (r - g) / delta + 4.0

        let hue = h * 60.0
        { H = hue; S = s; L = l }

    /// Calculate Euclidean distance between two colors
    let colorDistance (c1: ColorRGB) (c2: ColorRGB) =
        let dr = c1.R - c2.R
        let dg = c1.G - c2.G
        let db = c1.B - c2.B
        Math.Sqrt(dr * dr + dg * dg + db * db)

    /// List of blacklisted colors in RGB
    let blacklistedColors = [
        { R = 1.0; G = 1.0; B = 0.85 } // #FFFFD9
        { R = 0.678; G = 0.847; B = 0.902 } // # CSS Light Blue
        { R = 0.91; G = 0.816; B = 0.663 } // #E8D0A9
        { R = 0.564; G = 0.933; B = 0.564 } // #90EE90 aka CSS Light Green


    ]

    /// Check if a color is too close to any blacklisted colors
    let isColorTooClose (color: ColorRGB) =
        blacklistedColors |> List.exists (fun blacklisted -> colorDistance color blacklisted < colourDistanceThreshold)

    /// Generates a color ensuring it's not too close to blacklisted colors
    let rec findValidColorVariant (color: ColorHSL) (random: Random) =
        let hue = (color.H + (random : Random).NextDouble() * 30.0) % 360.0
        let saturation = ((color.S - 0.4 + (random : Random).NextDouble() * 0.11) % 0.6) + 0.4
        let luminance = ((color.L - 0.7 + (random : Random).NextDouble() * 0.11) % 0.3) + 0.7
        let color = hslToRgb {H=hue; S=saturation; L=luminance}
        if isColorTooClose color then findValidColorVariant (rgbToHsl color) random else color


    /// Generate a color based on a draw count, cycling through HSL systematically
    let generateColor (drawCount: int) (randomSeed : int)  =
        // Hue cycles every 12 steps before incrementing saturation
        let hue = (float ((drawCount * 31) % 360))
        let saturation = 0.4 + (float ((drawCount  / 12 * 17) % 60))/100.     // Wrap around after exceeding 1.0
        let luminance = 0.7 +  (float ((drawCount  / 12 / 5 * 17) % 30))/100. // Wrap around after exceeding 1.0
        let color = hslToRgb {H=hue; S=saturation; L=luminance}
        if isColorTooClose color then
            let random = Random(randomSeed + drawCount)
            findValidColorVariant (rgbToHsl color) random
        else color

    /// Top Level Call: given the model, generate a colour and update with the new model
    let generateColourFromModel (model: SheetT.Model) (dispatch) =
        let colour = generateColor model.ColourDrawnCount model.ColourRandomSeed
        let hexColour = "#" + (colour.R * 255.0).ToString("X2") + (colour.G * 255.0).ToString("X2") + (colour.B * 255.0).ToString("X2")
        printf "Generated colour: %s\n%A" hexColour colour
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
        sheetDispatch (SheetT.IncrementColourDrawnCount), hexColour


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
