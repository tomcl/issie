module Groups

open EEExtensions


open JSHelpers
open System


open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT

open Symbol
open Optics


open DrawModelType.SheetT

/// Generate colours for groups
// generateColourFromModel is a top level function in this module that generates a new colour for a group
module ColourGenerator =
    module Constants =
        // A colour is considered too close to another if the distance is less than minimumColourDistance (thus making it invalid)
        let minimumColourDistance = 26.
        // A colour is considered too close to a blacklisted colour if the distance is less than blackListedColourDistance (thus making it invalid)
        let blackListedColourDistance = 25.
        // how many times to resample the HSL space if an invalid colour is generated (too close to the blacklisted colours or existing colours)
        let trialLimit = 2500
        let satRange = (30.,80.)
        let lumRange = (75.,92.)
        // an increment in HSL space
        let hIncrement = 111.
        let sIncrement = 12.
        let lIncrement = 5. //minimumColourDistance/3.**0.5
        let seed = 36

    open Constants

    // Define a color type for RGB colors
    type ColourRGB = { R: float; G: float; B: float }
    type ColourHSL = { H: float; S: float; L: float }


    /// Convert RGB to HSL (helper function)
    /// R,G,B are in 0-1 range
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
    /// Note that sat and lum are in range 0-100 while hue is in range 0-360
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


    /// Calculate distance between two HSL colors.
    let colourDistance (c1: ColourHSL) (c2: ColourHSL) =
        let dh: float = c1.H - c2.H
        let ds: float = c1.S - c2.S
        let dl: float = c1.L - c2.L

        ( (dh * dh) + (ds * ds) + (dl * dl) )**0.5

    /// List of blacklisted colors in RGB
    /// These are colours already used by the UI. We want colours that are far away from the blacklisted colours so that
    /// groups are easily distinguishable.
    let blacklistedColorsRGB = [
        { R = 1.0; G = 1.0; B = 0.85 } // #FFFFD9 for non-clocked components
        { R = 0.678; G = 0.847; B = 0.902 } // # CSS Light Blue or #ADD8E6 for clocked components
        { R = 0.91; G = 0.816; B = 0.663 } // #E8D0A9 for inputs/outptus
        { R = 0.564; G = 0.933; B = 0.564 } // #90EE90 aka CSS Light Green for selected components
    ]

    let blacklistedColoursHSL = blacklistedColorsRGB |> List.map rgbToHSL

    /// Checks if a generated colour is valid, i.e. not too close to other colours or blacklisted colours
    let isColourValid (newSample: ColourHSL) (existingColours : ColourHSL list) : bool =
        // for every existingColour, make sure it is not too close to the newSample

        let farAwayFromExistingColours = not (List.exists (fun existingColour ->
            let distance = colourDistance newSample existingColour
            distance < minimumColourDistance
        ) existingColours)

        let farAwayFromBlacklistedColours = not (List.exists (fun blacklistedColour ->
            let distance = colourDistance newSample blacklistedColour
            distance < blackListedColourDistance
        ) blacklistedColoursHSL)

        farAwayFromExistingColours && farAwayFromBlacklistedColours

    /// Generate a base HSL colour randomly
    let generateBaseHSLColour (random: Random) : ColourHSL =
        let h: float = random.NextDouble() * 360.0
        let s: float = float (random.NextDouble()*(snd satRange - fst satRange) + fst satRange)
        let l: float = float (random.NextDouble()*(snd lumRange - fst lumRange) + fst lumRange)
        { H = h; S = s; L = l }

    /// Generate a new HSL colour by incrementing the existing colour
    let generateIncrementedHSLColour (sample:ColourHSL) =
        let newH: float = (sample.H + hIncrement) % 360.0
        let newS: float = ((sample.S - (fst satRange) + sIncrement) % (snd satRange - fst satRange)) + fst satRange
        let newL: float = ((sample.L - (fst satRange)  + lIncrement) % (snd lumRange - fst lumRange)) + fst lumRange

        { H = newH; S = newS; L = newL }

    /// Generates a new colour given a list of existing colours and a list of blacklisted colours
    let generateNewColour (existingColours : ColourHSL list) (blacklistedColoursHSL: ColourHSL list) =
        // generate a new colour using poisson-disk sampling.
        // make this deterministic based on seed and length of existingColours
        let random = Random(existingColours.Length * seed)

        // 1: generate initial sample
        let newSample =
            match existingColours.Length with
            | 0 -> printf "No existing colours, drawing..."; generateBaseHSLColour random
            | _ -> generateIncrementedHSLColour (List.head existingColours)


        // 2: repeat for number of trials
        let rec generateNewColourRecursive (sample: ColourHSL) (existingColours : ColourHSL list) (trial : int) =
            match trial with
            | trial when trial > trialLimit + 5 ->
                printf "Exceeded trial limits again. Generating random colour"
                generateBaseHSLColour (Random (existingColours.Length * seed))
            | trial when trial > trialLimit ->
                printf "Exceeded trial limits, trying with less constraints (avoid blacklisted colours only)"
                let newSample = generateIncrementedHSLColour sample
                if isColourValid newSample blacklistedColoursHSL then newSample
                else generateNewColourRecursive newSample existingColours (trial + 1)
            | _ ->
                let newSample = generateIncrementedHSLColour sample
                // printf "Trial: %d with colour: %A\n" trial newSample
                if isColourValid newSample existingColours then newSample
                else generateNewColourRecursive newSample existingColours (trial + 1)

        generateNewColourRecursive newSample existingColours 0


    /// Top level function that generates a new colour for a group
    let generateColourFromModel (model: SymbolT.Model) =
        // First, convert existing colours in the model (stored as rgb) to HSL.
        let existingModelColours =
            model.GroupInfoMap
            |> Map.values
            |> Array.toList
            // convert from RGB hex (with # in front) to HSL
            |> List.map (fun (groupInfo: GroupInfo) -> groupInfo.Colour |> hexToRGB |> rgbToHSL)

        // generate a new colour that is not too close to existing colours or blacklisted colours
        let newHSLColour = generateNewColour existingModelColours blacklistedColoursHSL

        // some stats for printing
        let closestDistanceToBlacklisted =
            blacklistedColoursHSL
            |> List.map (fun blacklistedColour -> colourDistance newHSLColour blacklistedColour)
            |> List.min

        printf "Generated colour: %s Closest Distance To Blacklist: %A" (newHSLColour |> hslToRGB |> rgbToHex) (closestDistanceToBlacklisted.ToString("F2"))
        printf "Existing Colours: %A" (existingModelColours |> List.map (fun c -> c |> hslToRGB |> rgbToHex))

        // convert back to RGB and then to hex
        newHSLColour |> hslToRGB |> rgbToHex


open ColourGenerator

// --------------- Helper Functions --------------- //
/// A helper that returns a list of componentIds that are part of any group
let getGroupedComponentIds (symModel: SymbolT.Model)  =
    symModel.GroupMap
    |> Map.values
    |> Array.toList
    |> List.concat


/// A helper that returns a list of symbols in the sheet model that are ungrouped, i.e. not in any group
/// The symbols returned are sorted in alphabetical order of their labels.
let getUngroupedSymbols (symModel: SymbolT.Model) =
    let groupedComponentIds = getGroupedComponentIds symModel
    symModel.Symbols
    |> Map.toList
    |> List.map snd
    // filter out annotations (symbols with no label)
    |> List.filter (fun symbol -> symbol.Component.Label.ToString() <> "" )
    // filter out wire-centric components
    |> List.filter (fun symbol ->
        match symbol.Component.Type with
        | MergeWires | SplitWire _ | BusSelection _  | NbitSpreader _ -> false
        | _ -> true)
    // make sure symbol is not part of an existing group
    |> List.filter (fun symbol -> not (List.exists ((=) (ComponentId symbol.Component.Id)) groupedComponentIds))
    |> List.sortBy (fun symbol -> symbol.Component.Label.ToString())

/// A helper that returns a list of symbols that are selected on sheet, and are ungrouped, i.e. not in any group.
/// The symbols returned are sorted in alphabetical order of their labels.
let getUngroupedSelectedSymbols (model: SheetT.Model) =
    getUngroupedSymbols model.Wire.Symbol
        |> List.filter (fun symbol ->  (List.exists ((=) (ComponentId symbol.Component.Id)) model.SelectedComponents))

/// A helper that returns alist of symbols that are in a group
let getGroupedSymbols (model : SheetT.Model) =
    let groupedComponentIds = getGroupedComponentIds model.Wire.Symbol
    model.Wire.Symbol.Symbols
    |> Map.toList
    |> List.map snd
    |> List.filter (fun symbol -> groupedComponentIds |> List.exists ((=) (ComponentId symbol.Component.Id)))

/// A helper that returns alist of symbols that are in a group. Takes in a
let getGroupedSymbolsFromSymModel (model : SymbolT.Model) =
    let groupedComponentIds = getGroupedComponentIds model
    model.Symbols
    |> Map.toList
    |> List.map snd
    |> List.filter (fun symbol -> groupedComponentIds |> List.exists ((=) (ComponentId symbol.Component.Id)))



// --------------- Core Functions --------------- //
// READ: most core functions will return a new groupMap and groupInfoMap.
// this is so they can be easily placed into DrawModelType.SymbolT.SetGroupMapAndInfo
// some core functions only return a new groupMap, in which case they can be placed into DrawModelType.SymbolT.SetGroupMap

/// Optic to read and update groupMap in a SymbolT.Model
let groupMap_ = Lens.create (fun (model: SymbolT.Model ) -> model.GroupMap) (fun groupMap model -> { model with GroupMap = groupMap } )
/// Optic to read and update groupInfoMap in a SymbolT.Model
let groupInfoMap_ = Lens.create (fun (model: SymbolT.Model ) -> model.GroupInfoMap) (fun groupInfo model -> { model with GroupInfoMap = groupInfo } )



/// Create a new group with an automatically generated colour. Returns the new groupMap and groupInfoMap.
/// Will not add duplicate componentIds if they are already part of a group.
/// If the list of componentIds is empty, nothing is changed (cannot create a group with no components).
let createNewGroup (symModel : SymbolT.Model) (componentIds: ComponentId list) =
    let allCompIdsInAGroup = getGroupedComponentIds symModel
    // filter out componentIds that are already part of a group
    let componentIdsFiltered =
        componentIds
        |> List.filter (fun componentId -> not (List.exists ((=) componentId) allCompIdsInAGroup))

    // if empty list, cannot create a new group, just return the existing groupMap and groupInfoMap.
    match componentIdsFiltered with
    | [] -> symModel.GroupMap, symModel.GroupInfoMap
    | _ ->
        let groupId = (GroupId(uuid()))
        let groupInfo = {
            Id = groupId;
            CreationDate = DateTime.Now;
            Colour = (generateColourFromModel symModel)
            }

        let newGroupInfoMap = Map.add groupId groupInfo symModel.GroupInfoMap

        let newGroupMap = Map.add groupId componentIdsFiltered symModel.GroupMap

        newGroupMap, newGroupInfoMap

/// Add a list of componentIds to an existing group. Returns the new groupMap. If groupId does not exist, nothing is changed.
/// Will not add duplicate componentIds if they are already part of a group.
let addToGroup (sheetModel : SheetT.Model) (groupId: GroupId) (componentIds: ComponentId list) =
    let groupMap = sheetModel.Wire.Symbol.GroupMap
    let allCompIdsInAGroup = getGroupedComponentIds sheetModel.Wire.Symbol

    // filter out componentIds that are already part of a group
    let componentIdsFiltered =
        componentIds
        |> List.filter (fun componentId -> not (List.exists ((=) componentId) allCompIdsInAGroup))

    match Map.tryFind groupId groupMap with
    | Some existingComponentIds ->
        let newComponentIds = existingComponentIds @ componentIdsFiltered
        Map.add groupId newComponentIds groupMap
    | None -> printf "no group exists for given groupId, returning unchanged groupMap"; groupMap

/// Delete a component from a group. If the group has no components left, delete it. Returns the new groupMap and groupInfoMap. If groupId does not exist, nothing is changed.
let deleteComponentFromGroup  (symModel : SymbolT.Model) (groupId: GroupId) (componentId: ComponentId) =
    let groupMap = symModel.GroupMap
    let newGroupMap =
        match Map.tryFind groupId groupMap with
        | Some componentIds ->
            let newComponentIds = List.filter ((<>) componentId) componentIds
            Map.add groupId newComponentIds groupMap
        | None -> printf "nothing found for given groupId"; groupMap

    // if the last component has been removed, delete the group from the groupMap and groupInfoMap
    // else just leave unchanged
    match newGroupMap |> Map.tryFind groupId with
    | Some [] ->
        printf "All components removed from group, deleting empty group..."
        let newGroupMap = Map.remove groupId newGroupMap
        let newGroupInfoMap = Map.remove groupId symModel.GroupInfoMap
        (newGroupMap, newGroupInfoMap)
    | _ -> (newGroupMap, symModel.GroupInfoMap)




/// Delete a component if it exists in any group. Returns the new groupMap and groupInfoMap.
let deleteGroupedComponents (symModel : SymbolT.Model) (componentIds: ComponentId list) =
    let groupedSymbols = getGroupedSymbolsFromSymModel symModel

    // Helper function to delete a single component
    let deleteGroupedComponent (symModel : SymbolT.Model) (componentId: ComponentId) =
        match List.tryFind (fun symbol -> symbol.Component.Id = componentId.ToString()) groupedSymbols with
        | Some _ ->
            let groupId = symModel.GroupMap |> Map.findKey (fun _ componentIds -> List.exists ((=) componentId) componentIds)
            deleteComponentFromGroup symModel groupId componentId
        | None -> symModel.GroupMap, symModel.GroupInfoMap

    // Fold over the list of component IDs to accumulate the changes
    let initialGroupMap = symModel.GroupMap
    let initialGroupInfoMap = symModel.GroupInfoMap

    let finalGroupMap, finalGroupInfoMap =
        List.fold (fun (currentGroupMap, currentGroupInfoMap) componentId ->
            let sheetModelWithCurrentMaps =
                symModel
                |> Optic.set groupMap_ currentGroupMap
                |> Optic.set groupInfoMap_ currentGroupInfoMap
            let newGroupMap, newGroupInfoMap = deleteGroupedComponent sheetModelWithCurrentMaps componentId
            newGroupMap, newGroupInfoMap
        ) (initialGroupMap, initialGroupInfoMap) componentIds

    finalGroupMap, finalGroupInfoMap


/// Delete a whole group. Returns the new groupMap and groupInfoMap
let deleteWholeGroup  (symModel : SymbolT.Model)  (groupId: GroupId) =
    let newGroupMap =
        match Map.tryFind groupId symModel.GroupMap with
        | Some _ ->
            Map.remove groupId symModel.GroupMap
        | None -> printf "No group exists for given groupId in GroupMap" ;symModel.GroupMap

    let newGroupInfoMap =
        match Map.tryFind groupId symModel.GroupInfoMap with
        | Some _ ->
            Map.remove groupId symModel.GroupInfoMap
        | None -> printf "No group exists for given groupId in GroupInfoMap" ; symModel.GroupInfoMap

    (newGroupMap, newGroupInfoMap)


