module ContextMenus
open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI

//
// **** DO NOT open or use renderer module code ****
//

//---------------------------------------------------------------------------------------//
//-------Menus for context-dependent right-click actions - mainly used in draw block-----//
//---------------------------------------------------------------------------------------//

(*
    NB - this file is linked into Main project as well as Renderer - so it cannot reference
    Renderer modules which are not compiled with Main.fs (that is nearly all of the renderer).
*)


/// Item offered on a schematic component while a wave simulation is running, which adds waveforms
/// from that component to the viewer. Which menu the renderer asks for decides whether it appears -
/// this file cannot see the model. See UpdateHelpers.getContextMenu.
let addWavesItem = "Add waveforms to viewer"

/// Item offered on a schematic wire while a wave simulation is running, which adds the signal on
/// that wire to the viewer's selection. As with addWavesItem, the renderer decides whether it
/// appears - a wire only carries ONE signal of the simulation when the sheet it is drawn on has
/// exactly one instance in the simulated design, and only the renderer can ask that.
let addWireWaveItem = "Add wave to viewer"

/// Items offered on an instance of a library component, which take the place of "Go to sheet".
/// A library component is meant to be one thing rather than a sheet with innards, so its sheet is
/// normally unreachable; these open it read-only, and put it away again. Which of the two is
/// offered depends on whether that sheet is already being viewed, so as with the waveform item
/// above the renderer decides by asking for a different menu.
let viewLibraryItem = "View library component"
let hideLibraryItem = "Hide library component"

let private componentItems =
    ["Rotate Clockwise (Ctrl+Right)"; "Rotate AntiClockwise (Ctrl+Left)" ; "Flip Vertical (Ctrl+Up)"; "Flip Horizontal (Ctrl+Down)" ; "Delete (DEL)"; "Copy (Ctrl+C)"; "Properties"]

let private customComponentItems =
    ["Go to sheet" ; "Properties" ; "Move ports" ; "Resize symbol"]

/// As customComponentItems, for an instance of a library component: the instance itself sits on an
/// ordinary sheet and can be moved and resized like any other, so only the first item differs.
let private libraryInstanceItems (openItem: string) =
    [openItem ; "Properties" ; "Move ports" ; "Resize symbol"]

/// The context menu info is a map of menu name -> list of menu items
/// menu and item names can be arbitrary strings
/// add menus as here
let contextMenus = [
        "SheetMenuBreadcrumbDev", ["Rename"; "Duplicate"; "Delete"; "Set as top"; "Save as library component"; "Write design as Verilog"; "Lock"; "Unlock"; "Lock Subtree"; "Unlock Subtree"]
        "SheetMenuBreadcrumb", ["Rename"; "Duplicate"; "Delete"; "Set as top"; "Save as library component"; "Write design as Verilog"]
        "ProjectPath", ["Copy path"; "Open directory"]
        "CustomComponent", customComponentItems
        "CustomComponentWaveSim", customComponentItems @ [addWavesItem]
        "LibraryInstance", libraryInstanceItems viewLibraryItem
        "LibraryInstanceWaveSim", libraryInstanceItems viewLibraryItem @ [addWavesItem]
        "LibraryInstanceOpen", libraryInstanceItems hideLibraryItem
        "LibraryInstanceOpenWaveSim", libraryInstanceItems hideLibraryItem @ [addWavesItem]
        "ScalingBox", ["Rotate Clockwise (Ctrl+Right)"; "Rotate AntiClockwise (Ctrl+Left)" ; "Flip Vertical (Ctrl+Up)"; "Flip Horizontal (Ctrl+Down)"; "Delete Box (DEL)"; "Copy Box (Ctrl+C)"; "Move Box (Drag any component)"]
        "Component", componentItems
        "ComponentWaveSim", componentItems @ [addWavesItem]
        // These labels spell their own keys, so they have to be kept in step with KeyTypes by
        // hand - this file cannot see it, being compiled into the main process as well.
        "Canvas", ["Zoom-in (Ctrl+plus) and centre" ; "Zoom-out (Ctrl+minus)" ; "Fit to window (Ctrl+0)" ; "Paste (Ctrl+V)"; "Reroute all wires"; "Properties"]
        "Wire", ["Unfix Wire"]
        "WireWaveSim", ["Unfix Wire"; addWireWaveItem]
        // Menus offered while a library component's sheet is being viewed. Every item that would
        // change the sheet is absent rather than disabled, because the sheet is held at what it
        // loaded with and the change would be silently undone. A wire and the scaling box have
        // nothing left at all, so they are given no menu. Fit to window stays: it moves the whole
        // circuit rather than editing it, and is allowed for that reason.
        "ComponentReadOnly", ["Properties"]
        "LibraryInstanceReadOnly", [viewLibraryItem; "Properties"]
        "LibraryInstanceOpenReadOnly", [hideLibraryItem; "Properties"]
        "CanvasReadOnly", ["Zoom-in (Ctrl+plus) and centre" ; "Zoom-out (Ctrl+minus)" ; "Fit to window (Ctrl+0)" ; "Properties"]
        "SheetMenuBreadcrumbLibrary", [hideLibraryItem]
        // Each of these is the name of a case in UIPopups.viewWaveInfoPopup, which is given the
        // item clicked on verbatim. A name here that has no case there reaches its catch-all, so
        // the two lists must be changed together. They cannot be one list: this file is compiled
        // into the main process, which has none of the renderer, and UIPopups is compiled long
        // before this file in the renderer itself.
        "WaveSimHelp", ["Waveform and RAM selection"; "Viewing Waveforms"; "Miscellaneous"]
        "", [] // Empty string for no context menu.
    ]

let private menuMap = Map.ofList contextMenus

//---------------------------------------------------------------------------------------//
//----------- Reading the menus from the main process, across two Fable libraries --------//
//---------------------------------------------------------------------------------------//
//
// This file is compiled into BOTH projects, but Fable emits one .fs.js beside the .fs source, so
// there is a single ContextMenus.fs.js and it lives under src/Renderer. It therefore builds its
// values with the RENDERER's copy of fable-library, while the main process reads them with its
// own copy in src/Main/fable_modules - a different set of module instances of the same library.
//
// That matters for anything with an internal structure. Map is a balanced tree whose traversal
// asks `node instanceof MapTreeNode`, and Main's MapTreeNode is a different class object from the
// Renderer's, so every node of a Renderer-built map reads to Main as a leaf: `Map.tryFind` and
// `Map.toList` see the ROOT and nothing else. A right-click reported 'Canvas' is not a menu name:
// it must be one of LibraryInstance - one name, the root of a 21-entry map - and every menu except
// that one failed to open.
//
// So the map is private, and what crosses the boundary is a string array: a plain JS array, with
// no class identity to disagree about. These two functions run here, with the library that built
// the map, and are the only way in.

/// The items of the named menu; empty where the name is not a menu.
let menuItemsFor (name: string) : string array =
    Map.tryFind name menuMap
    |> Option.map List.toArray
    |> Option.defaultValue [||]

/// Whether the name is one of the menus. Distinct from `menuItemsFor` returning empty, because
/// the no-menu entry "" is a real name whose menu is deliberately empty.
let isMenuName (name: string) : bool = Map.containsKey name menuMap

/// Every menu name, for saying what was expected when a lookup fails.
let menuNames : string array =
    menuMap |> Map.toList |> List.map fst |> List.toArray
