module HLP25CodeB

open Hlp25Types
open Fable.React
open Fable.React.Props

open JSHelpers
open NumberHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupHelpers
open UIPopups
open Notifications
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open CatalogueView
open TopMenuView
open MenuHelpers

//------------------------------------- Part B ---------------------------------------------------//
//----------------------------- Sample Code for HLP25 --------------------------------------------//
//----------------------------- use these to get started -----------------------------------------//
//-------------------- Modify the signatures as you see fit for your implementation---------------//
//------------------------------------------------------------------------------------------------//

/// 1. An input box for a string that can be used to seach wave names in the Waveform Selector.
/// Any substring of a wave name of form 'sheet.CompName.PortName' should be matched.
/// 2. A search box for parts of sheet names
/// 3. A search box for component names.
/// 4. A search box for port names
/// 5. (optional) a search box for component type
/// Overall search is AND of all five searches.
/// The search boxes should be able to filter a list of wave names in the Waveform Selector.
/// In addition the search boxes must change a breadcrumb display so that the user can see coloured the
/// sheets in which matches are found.
/// The Sheet search box 2.  has additional functionality to allow the user to navigate to the design visually.
/// When a breadcrumb is clicked the corresponding sheet name is displayed in the sheet box and displayed
/// ports and components are restricted to those in the sheet.
/// Box 1 has additional functionality: when it is changed the component and port boxes are emptied. (good?)
let searchBoxes (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    // See MiscMenuView for Breadcrumb generation functions
    // see WaveSelectView for the existing Waveform Selector search box
    // Use the existing Waveform Selector search box as a template for the new search boxes.
    failwithf "Not implemented yet"

/// Function to configure how filtered components, ports, etc are displayed in the Waveform Selector.
/// This abstracts out decisions about how to display the Waveform Selector from its actual implementation.
let makeWaveDisplayTree (wsModel: WaveSimModel) : WaveDisplayTree =
    // Not required for MVP; but useful for a full implementation.
    // This function could be written as individual HLP25 code.
    //
    // The wave selector display is arranged as a tree of sheets, components and ports.
    // Nodes in the tree correspond to sets of items that are hidden and optionally displayed
    // The actual display is currently implemented by the recursive function makeSheetTree
    // this both doe sthe implementation and determines the recursive tree structure.
    // 
    // This function abstracts out the tree structure from the implementation.
    // It could be used to implement an optimal tree structure for a given set of
    // filtered waves.
    failwithf "Not implemented yet"

/// Converts a tree of wave display nodes into a react element that can be displayed in the Waveform Selector.
/// The output display uses check boxes and clickables to display or hide nodes, and select/deselect waves. 
let implementWaveSelector (wsModel: WaveSimModel) (dispatch: Msg -> unit) (wTree: WaveDisplayTree): ReactElement =
    // This function implements the display of the waveform selection table
    // as a set of rows that can be hidden or displayed.
    // The structure and order of the rows is determined by the tree structure.
    // Details to display in each row are determined by the node content.
    // Additional details can be looked up from wsModel if necessary.
    // This is not required for MVP, but useful for a full implementation.
    // It could be implemented as HLP25 individual code.
    failwithf "Not implemented yet"

/// Displays a breadcrumb display of the simulation design sheet hierarchy with
/// coloured sheets indicating where the search string is found. Possibly the number of
/// matches in each sheet is displayed.
let waveSelectBreadcrumbs (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    // See MiscMenuView for Breadcrumb generation functions
    // see WaveSelectView for the existing Waveform Selector search box
    // Use the existing Waveform Selector search box as a template for the new search boxes.
    failwithf "Not implemented yet"

/// Displays a react element that allows the user to select waves for display in the waveform viewer.
let selectWavesHlp25 (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    // see WaveSimSelect.selectWaves for the existing display of waveforms to select
    // For MVP this could be a cut-down version of that function that displays a flat list of components
    // and hidable ports, with a checkbox to select each one.
    // For a full implementation: this function could be abstracted as the two functions above:
    //    makeWaveDisplayTree: that determines the tree structure of the display
    //    implementWaveSelector: that displays the given tree structure
    failwithf "Not implemented yet"


let selectWavesModalHlp25 (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    // See WaveSimSelect.selectWavesModal for the existing Waveform Selector top level view
    // This contains a search box to filter waves, and a wave selection box to select/deselect
    // the (filtered) waves for display.
    // Both will be changed in part B
    //
    // The wave selection box will be replaced by a breadcrumb display,
    // and a new wave selection box, side-by-side
    //
    // The current wave selection rows are displayed in a table by the recursive function makeSheetRow
    // This makes, hierarchically, rows for sheets and its components and ports
    // Subsheets are displayed as a single row with a button to open the subsheet
    //
    // The new wave selection rows should not use this sheet hierarchy (the breadcrums deal with that).
    // For an MVP they could be displayed as a flat list of components names, which open when clicked to show ports.
    // This is a simplification of the current display, and implemented in the function makeComponentRow.
    //
    // For a full implementation:
    // The display should depend on the number of (filtered) waves, fewer waves should show more detail.
    // Each row should contain the component sheet name.
    // Whether ports are hidden or not should depend on the number of waves.
    // Each row should (maybe) include component group, with (maybe) the list ordered by class and then component name
    //        See makeComponentGroup for how components are grouped in current display.
    // The display should be adjusted so that the user can quickly select any number of waves.
    // It should also be possible to select all waves in a sheet.
    // Note that each signal can be selected in multiple places, from its driving port, and its receiving port(s).
    // Although these are separate waves only one wave from each signal will be allowed in the waveform viewer.
    // Duplicates are filtered out: 
    failwithf "Not implemented yet"



