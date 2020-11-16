import { Record, Union } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { unit_type, lambda_type, list_type, float64_type, array_type, uint32_type, tuple_type, int32_type, string_type, record_type, option_type, class_type, bool_type, union_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { SavedWaveInfo, ComponentId, NumberBase, ConnectionId$reflection, ComponentId$reflection, JSDiagramMsg$2$reflection, LoadedComponent$reflection, Connection$reflection, Component$reflection, NetGroup$reflection, NumberBase$reflection } from "../../Common/CommonTypes.fs.js";
import { SimulationComponent$reflection, SimulationError$reflection, JSComponent$reflection, SimulationData$reflection } from "../../Simulator/SimulatorTypes.fs.js";
import { JSCanvas$reflection } from "../Interface/JSTypes.fs.js";
import { FSharpResult$2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { compareSafe, equals, structuralHash } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { Draw2dWrapper__GetCanvasState, Draw2dWrapper$reflection } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { add, tryFind, FSharpMap__Add, empty } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { value as value_1, bind, defaultArg, map } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { extractComponent, extractReducedState, extractState } from "../Interface/Extractor.fs.js";
import { cons, tryFind as tryFind_1, map as map_1, empty as empty_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { ofList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { getNetList } from "../../Common/Helpers.fs.js";
import { toFail, toConsole, join, printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";

export class RightTab extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Properties", "Catalogue", "Simulation", "WaveSim"];
    }
}

export function RightTab$reflection() {
    return union_type("ModelType.RightTab", [], RightTab, () => [[], [], [], []]);
}

export class MemoryEditorData extends Record {
    constructor(OnlyDiff, Address, Start, NumberBase) {
        super();
        this.OnlyDiff = OnlyDiff;
        this.Address = Address;
        this.Start = Start;
        this.NumberBase = NumberBase;
    }
}

export function MemoryEditorData$reflection() {
    return record_type("ModelType.MemoryEditorData", [], MemoryEditorData, () => [["OnlyDiff", bool_type], ["Address", option_type(class_type("System.Int64"))], ["Start", class_type("System.Int64")], ["NumberBase", NumberBase$reflection()]]);
}

export class PopupDialogData extends Record {
    constructor(Text$, Int, Int2, MemorySetup, MemoryEditorData) {
        super();
        this.Text = Text$;
        this.Int = Int;
        this.Int2 = Int2;
        this.MemorySetup = MemorySetup;
        this.MemoryEditorData = MemoryEditorData;
    }
}

export function PopupDialogData$reflection() {
    return record_type("ModelType.PopupDialogData", [], PopupDialogData, () => [["Text", option_type(string_type)], ["Int", option_type(int32_type)], ["Int2", option_type(int32_type)], ["MemorySetup", option_type(tuple_type(int32_type, int32_type))], ["MemoryEditorData", option_type(MemoryEditorData$reflection())]]);
}

export class TopMenu extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Closed", "Project", "Files"];
    }
}

export function TopMenu$reflection() {
    return union_type("ModelType.TopMenu", [], TopMenu, () => [[], [], []]);
}

export class KeyboardShortcutMsg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["CtrlS", "AltC", "AltV", "AltZ", "AltShiftZ", "DEL"];
    }
}

export function KeyboardShortcutMsg$reflection() {
    return union_type("ModelType.KeyboardShortcutMsg", [], KeyboardShortcutMsg, () => [[], [], [], [], [], []]);
}

export class Wire extends Record {
    constructor(NBits, BitData) {
        super();
        this.NBits = NBits;
        this.BitData = BitData;
    }
}

export function Wire$reflection() {
    return record_type("ModelType.Wire", [], Wire, () => [["NBits", uint32_type], ["BitData", class_type("System.Numerics.BigInteger")]]);
}

export class Sample extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Wire", "StateSample"];
    }
}

export function Sample$reflection() {
    return union_type("ModelType.Sample", [], Sample, () => [[["Item", Wire$reflection()]], [["Item", array_type(string_type)]]]);
}

export class SVGCacheT extends Record {
    constructor(Top, Waves, Bottom) {
        super();
        this.Top = Top;
        this.Waves = Waves;
        this.Bottom = Bottom;
    }
}

export function SVGCacheT$reflection() {
    return record_type("ModelType.SVGCacheT", [], SVGCacheT, () => [["Top", array_type(class_type("Fable.React.ReactElement"))], ["Waves", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, class_type("Fable.React.ReactElement")])], ["Bottom", array_type(class_type("Fable.React.ReactElement"))]]);
}

export class SimParamsT extends Record {
    constructor(WaveViewerRadix, LastClkTime, CursorTime, ClkSvgWidth, DispNames, LastScrollPos) {
        super();
        this.WaveViewerRadix = WaveViewerRadix;
        this.LastClkTime = LastClkTime;
        this.CursorTime = CursorTime;
        this.ClkSvgWidth = ClkSvgWidth;
        this.DispNames = DispNames;
        this.LastScrollPos = LastScrollPos;
    }
}

export function SimParamsT$reflection() {
    return record_type("ModelType.SimParamsT", [], SimParamsT, () => [["WaveViewerRadix", NumberBase$reflection()], ["LastClkTime", uint32_type], ["CursorTime", uint32_type], ["ClkSvgWidth", float64_type], ["DispNames", array_type(string_type)], ["LastScrollPos", option_type(float64_type)]]);
}

export class WSViewT extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["WSClosed", "WSInitEditorOpen", "WSEditorOpen", "WSViewerOpen"];
    }
}

export function WSViewT$reflection() {
    return union_type("ModelType.WSViewT", [], WSViewT, () => [[], [], [], []]);
}

export class SimActionT extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["MakeSVGs", "ChangeParameters"];
    }
}

export function SimActionT$reflection() {
    return union_type("ModelType.SimActionT", [], SimActionT, () => [[["Item", array_type(NetGroup$reflection())]], [["Item", SimParamsT$reflection()]]]);
}

export class WaveSimModel extends Record {
    constructor(InitWaveSimGraph, SimParams, AllWaveNames, AllNets, DispWaveSVGCache, SimDataCache, CursorBoxIsEmpty, WSViewState, WSTransition, LastCanvasState) {
        super();
        this.InitWaveSimGraph = InitWaveSimGraph;
        this.SimParams = SimParams;
        this.AllWaveNames = AllWaveNames;
        this.AllNets = AllNets;
        this.DispWaveSVGCache = DispWaveSVGCache;
        this.SimDataCache = SimDataCache;
        this.CursorBoxIsEmpty = CursorBoxIsEmpty;
        this.WSViewState = WSViewState;
        this.WSTransition = WSTransition;
        this.LastCanvasState = LastCanvasState;
    }
}

export function WaveSimModel$reflection() {
    return record_type("ModelType.WaveSimModel", [], WaveSimModel, () => [["InitWaveSimGraph", option_type(SimulationData$reflection())], ["SimParams", SimParamsT$reflection()], ["AllWaveNames", array_type(string_type)], ["AllNets", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, NetGroup$reflection()])], ["DispWaveSVGCache", SVGCacheT$reflection()], ["SimDataCache", array_type(SimulationData$reflection())], ["CursorBoxIsEmpty", bool_type], ["WSViewState", WSViewT$reflection()], ["WSTransition", option_type(tuple_type(SimParamsT$reflection(), WSViewT$reflection()))], ["LastCanvasState", option_type(tuple_type(list_type(Component$reflection()), list_type(Connection$reflection())))]]);
}

export class DiagEl extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Comp", "Conn"];
    }
}

export function DiagEl$reflection() {
    return union_type("ModelType.DiagEl", [], DiagEl, () => [[["Item", Component$reflection()]], [["Item", Connection$reflection()]]]);
}

export class DragMode extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["DragModeOn", "DragModeOff"];
    }
}

export function DragMode$reflection() {
    return union_type("ModelType.DragMode", [], DragMode, () => [[["Item", int32_type]], []]);
}

export class IntMode extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["FirstInt", "SecondInt"];
    }
}

export function IntMode$reflection() {
    return union_type("ModelType.IntMode", [], IntMode, () => [[], []]);
}

export class MenuCommand extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["MenuPrint", "MenuSaveFile", "MenuNewFile", "MenuZoom"];
    }
}

export function MenuCommand$reflection() {
    return union_type("ModelType.MenuCommand", [], MenuCommand, () => [[], [], [], [["Item", float64_type]]]);
}

export class Project extends Record {
    constructor(ProjectPath, OpenFileName, LoadedComponents) {
        super();
        this.ProjectPath = ProjectPath;
        this.OpenFileName = OpenFileName;
        this.LoadedComponents = LoadedComponents;
    }
}

export function Project$reflection() {
    return record_type("ModelType.Project", [], Project, () => [["ProjectPath", string_type], ["OpenFileName", string_type], ["LoadedComponents", list_type(LoadedComponent$reflection())]]);
}

export class Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSDiagramMsg", "KeyboardShortcutMsg", "StartSimulation", "SetLastSavedCanvas", "SetCurrFileWSMod", "SetWSError", "AddWaveSimFile", "SetSimulationGraph", "SetSimulationBase", "IncrementSimulationClockTick", "EndSimulation", "EndWaveSim", "ChangeRightTab", "SetHighlighted", "SetSelWavesHighlighted", "SetClipboard", "SetCreateComponent", "SetProject", "CloseProject", "ShowPopup", "ClosePopup", "SetPopupDialogText", "SetPopupDialogInt", "SetPopupDialogTwoInts", "SetPopupDialogMemorySetup", "SetPopupMemoryEditorData", "SetSelectedComponentMemoryLocation", "CloseDiagramNotification", "SetSimulationNotification", "CloseSimulationNotification", "CloseWaveSimNotification", "SetFilesNotification", "CloseFilesNotification", "SetMemoryEditorNotification", "CloseMemoryEditorNotification", "SetPropertiesNotification", "ClosePropertiesNotification", "SetTopMenu", "ReloadSelectedComponent", "SetDragMode", "SetViewerWidth", "MenuAction", "DiagramMouseEvent", "SelectionHasChanged", "SetWaveSimIsOutOfDate", "SetIsLoading", "SetWaveSimModel", "WaveSimulateNow", "InitiateWaveSimulation", "SetLastSimulatedCanvasState", "UpdateScrollPos", "SetLastScrollPos"];
    }
}

export function Msg$reflection() {
    return union_type("ModelType.Msg", [], Msg, () => [[["Item", JSDiagramMsg$2$reflection(JSCanvas$reflection(), JSComponent$reflection())]], [["Item", KeyboardShortcutMsg$reflection()]], [["Item", union_type("Microsoft.FSharp.Core.FSharpResult`2", [SimulationData$reflection(), SimulationError$reflection()], FSharpResult$2, () => [[["ResultValue", SimulationData$reflection()]], [["ErrorValue", SimulationError$reflection()]]])]], [["Item1", string_type], ["Item2", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))]], [["Item", WaveSimModel$reflection()]], [["Item", option_type(SimulationError$reflection())]], [["Item1", string_type], ["Item2", WaveSimModel$reflection()]], [["Item", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), SimulationComponent$reflection()])]], [["Item", NumberBase$reflection()]], [], [], [], [["Item", RightTab$reflection()]], [["Item1", list_type(ComponentId$reflection())], ["Item2", list_type(ConnectionId$reflection())]], [["Item", array_type(ConnectionId$reflection())]], [["Item", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))]], [["Item", Component$reflection()]], [["Item", Project$reflection()]], [], [["Item", lambda_type(PopupDialogData$reflection(), class_type("Fable.React.ReactElement"))]], [], [["Item", option_type(string_type)]], [["Item", option_type(int32_type)]], [["Item", tuple_type(option_type(int32_type), IntMode$reflection())]], [["Item", option_type(tuple_type(int32_type, int32_type))]], [["Item", option_type(MemoryEditorData$reflection())]], [["Item1", class_type("System.Int64")], ["Item2", class_type("System.Int64")]], [], [["Item", lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement"))]], [], [], [["Item", lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement"))]], [], [["Item", lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement"))]], [], [["Item", lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement"))]], [], [["Item", TopMenu$reflection()]], [["Item", int32_type]], [["Item", DragMode$reflection()]], [["Item", int32_type]], [["Item1", MenuCommand$reflection()], ["Item2", lambda_type(Msg$reflection(), unit_type)]], [], [], [["Item", bool_type]], [["Item", bool_type]], [["Sheet", string_type], ["WSModel", WaveSimModel$reflection()]], [], [["Item", tuple_type(WSViewT$reflection(), SimParamsT$reflection())]], [["Item", option_type(tuple_type(list_type(Component$reflection()), list_type(Connection$reflection())))]], [["Item", bool_type]], [["Item", option_type(float64_type)]]]);
}

export class Notifications extends Record {
    constructor(FromDiagram, FromSimulation, FromWaveSim, FromFiles, FromMemoryEditor, FromProperties) {
        super();
        this.FromDiagram = FromDiagram;
        this.FromSimulation = FromSimulation;
        this.FromWaveSim = FromWaveSim;
        this.FromFiles = FromFiles;
        this.FromMemoryEditor = FromMemoryEditor;
        this.FromProperties = FromProperties;
    }
}

export function Notifications$reflection() {
    return record_type("ModelType.Notifications", [], Notifications, () => [["FromDiagram", option_type(lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement")))], ["FromSimulation", option_type(lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement")))], ["FromWaveSim", option_type(lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement")))], ["FromFiles", option_type(lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement")))], ["FromMemoryEditor", option_type(lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement")))], ["FromProperties", option_type(lambda_type(lambda_type(Msg$reflection(), unit_type), class_type("Fable.React.ReactElement")))]]);
}

export class AutoSaveT extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Saving", "Deleting", "Inactive"];
    }
}

export function AutoSaveT$reflection() {
    return union_type("ModelType.AutoSaveT", [], AutoSaveT, () => [[], [], []]);
}

export class AsyncTasksT extends Record {
    constructor(AutoSave, LastAutoSave, LastAutoSaveCheck, LastSavedCanvasState, RunningSimulation) {
        super();
        this.AutoSave = AutoSave;
        this.LastAutoSave = LastAutoSave;
        this.LastAutoSaveCheck = LastAutoSaveCheck;
        this.LastSavedCanvasState = LastSavedCanvasState;
        this.RunningSimulation = RunningSimulation;
    }
}

export function AsyncTasksT$reflection() {
    return record_type("ModelType.AsyncTasksT", [], AsyncTasksT, () => [["AutoSave", AutoSaveT$reflection()], ["LastAutoSave", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, class_type("System.DateTime")])], ["LastAutoSaveCheck", class_type("System.DateTime")], ["LastSavedCanvasState", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))])], ["RunningSimulation", bool_type]]);
}

export class Model extends Record {
    constructor(AsyncActivity, WaveSim, Diagram, IsLoading, WaveSimulationIsOutOfDate, LastSimulatedCanvasState, LastDetailedSavedState, CurrentSelected, LastSelectedIds, LastUsedDialogWidth, SelectedComponent, CurrentStepSimulationStep, RightPaneTabVisible, Hilighted, Clipboard, LastCreatedComponent, SavedSheetIsOutOfDate, CurrentProj, PopupViewFunc, PopupDialogData, Notifications, TopMenuOpenState, DividerDragMode, WaveSimViewerWidth, SimulationInProgress, ConnsOfSelectedWavesAreHighlighted, CheckWaveformScrollPosition) {
        super();
        this.AsyncActivity = AsyncActivity;
        this.WaveSim = WaveSim;
        this.Diagram = Diagram;
        this.IsLoading = IsLoading;
        this.WaveSimulationIsOutOfDate = WaveSimulationIsOutOfDate;
        this.LastSimulatedCanvasState = LastSimulatedCanvasState;
        this.LastDetailedSavedState = LastDetailedSavedState;
        this.CurrentSelected = CurrentSelected;
        this.LastSelectedIds = LastSelectedIds;
        this.LastUsedDialogWidth = (LastUsedDialogWidth | 0);
        this.SelectedComponent = SelectedComponent;
        this.CurrentStepSimulationStep = CurrentStepSimulationStep;
        this.RightPaneTabVisible = RightPaneTabVisible;
        this.Hilighted = Hilighted;
        this.Clipboard = Clipboard;
        this.LastCreatedComponent = LastCreatedComponent;
        this.SavedSheetIsOutOfDate = SavedSheetIsOutOfDate;
        this.CurrentProj = CurrentProj;
        this.PopupViewFunc = PopupViewFunc;
        this.PopupDialogData = PopupDialogData;
        this.Notifications = Notifications;
        this.TopMenuOpenState = TopMenuOpenState;
        this.DividerDragMode = DividerDragMode;
        this.WaveSimViewerWidth = (WaveSimViewerWidth | 0);
        this.SimulationInProgress = SimulationInProgress;
        this.ConnsOfSelectedWavesAreHighlighted = ConnsOfSelectedWavesAreHighlighted;
        this.CheckWaveformScrollPosition = CheckWaveformScrollPosition;
    }
    GetHashCode() {
        const this$ = this;
        return structuralHash(reduce(this$)) | 0;
    }
    Equals(x) {
        let x$0027;
        const this$ = this;
        return (x instanceof Model) ? (x$0027 = x, equals(reduce(this$), reduce(x$0027))) : false;
    }
}

export function Model$reflection() {
    return record_type("ModelType.Model", [], Model, () => [["AsyncActivity", AsyncTasksT$reflection()], ["WaveSim", tuple_type(class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, WaveSimModel$reflection()]), option_type(SimulationError$reflection()))], ["Diagram", Draw2dWrapper$reflection()], ["IsLoading", bool_type], ["WaveSimulationIsOutOfDate", bool_type], ["LastSimulatedCanvasState", option_type(tuple_type(list_type(Component$reflection()), list_type(Connection$reflection())))], ["LastDetailedSavedState", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["CurrentSelected", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["LastSelectedIds", tuple_type(list_type(string_type), list_type(string_type))], ["LastUsedDialogWidth", int32_type], ["SelectedComponent", option_type(Component$reflection())], ["CurrentStepSimulationStep", option_type(union_type("Microsoft.FSharp.Core.FSharpResult`2", [SimulationData$reflection(), SimulationError$reflection()], FSharpResult$2, () => [[["ResultValue", SimulationData$reflection()]], [["ErrorValue", SimulationError$reflection()]]]))], ["RightPaneTabVisible", RightTab$reflection()], ["Hilighted", tuple_type(tuple_type(list_type(ComponentId$reflection()), list_type(ConnectionId$reflection())), list_type(ConnectionId$reflection()))], ["Clipboard", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["LastCreatedComponent", option_type(Component$reflection())], ["SavedSheetIsOutOfDate", bool_type], ["CurrentProj", option_type(Project$reflection())], ["PopupViewFunc", option_type(lambda_type(PopupDialogData$reflection(), class_type("Fable.React.ReactElement")))], ["PopupDialogData", PopupDialogData$reflection()], ["Notifications", Notifications$reflection()], ["TopMenuOpenState", TopMenu$reflection()], ["DividerDragMode", DragMode$reflection()], ["WaveSimViewerWidth", int32_type], ["SimulationInProgress", option_type(SimActionT$reflection())], ["ConnsOfSelectedWavesAreHighlighted", bool_type], ["CheckWaveformScrollPosition", bool_type]]);
}

export function setSimParams(setFn, wsm) {
    const SimParams = setFn(wsm.SimParams);
    return new WaveSimModel(wsm.InitWaveSimGraph, SimParams, wsm.AllWaveNames, wsm.AllNets, wsm.DispWaveSVGCache, wsm.SimDataCache, wsm.CursorBoxIsEmpty, wsm.WSViewState, wsm.WSTransition, wsm.LastCanvasState);
}

export function setDispNames(names, wsMod) {
    return setSimParams((sp) => (new SimParamsT(sp.WaveViewerRadix, sp.LastClkTime, sp.CursorTime, sp.ClkSvgWidth, names, sp.LastScrollPos)), wsMod);
}

export function setEditorView(view, wsModel) {
    return new WaveSimModel(wsModel.InitWaveSimGraph, wsModel.SimParams, wsModel.AllWaveNames, wsModel.AllNets, wsModel.DispWaveSVGCache, wsModel.SimDataCache, wsModel.CursorBoxIsEmpty, view, void 0, wsModel.LastCanvasState);
}

export function setEditorNextView(nView, simParas, wsModel) {
    return new WaveSimModel(wsModel.InitWaveSimGraph, wsModel.SimParams, wsModel.AllWaveNames, wsModel.AllNets, wsModel.DispWaveSVGCache, wsModel.SimDataCache, wsModel.CursorBoxIsEmpty, wsModel.WSViewState, [simParas, nView], wsModel.LastCanvasState);
}

export function initWS(allNames, allPorts) {
    const SimDataCache = [];
    const DispWaveSVGCache = new SVGCacheT([], empty(), []);
    let SimParams;
    const DispNames = [];
    SimParams = (new SimParamsT(new NumberBase(2), 9, 0, 1, DispNames, void 0));
    return new WaveSimModel(void 0, SimParams, allNames, allPorts, DispWaveSVGCache, SimDataCache, false, new WSViewT(0), void 0, void 0);
}

export function reduce(this$) {
    const CurrProject = (this$.PopupViewFunc == null) ? false : true;
    return {
        AsyncActivity: this$.AsyncActivity,
        Clipboard: this$.Clipboard,
        ConnsToBeHighlighted: this$.ConnsOfSelectedWavesAreHighlighted,
        CreateComponent: this$.LastCreatedComponent,
        CurrProject: CurrProject,
        CurrentSelected: this$.CurrentSelected,
        DragMode: this$.DividerDragMode,
        HasUnsavedChanges: false,
        Hilighted: this$.Hilighted,
        LastSelectedIds: this$.LastSelectedIds,
        LastSimulatedCanvasState: this$.LastSimulatedCanvasState,
        LastUsedDialogWidth: this$.LastUsedDialogWidth,
        PopupDialogData: this$.PopupDialogData,
        RightTab: this$.RightPaneTabVisible,
        SelectedComponent: this$.SelectedComponent,
        SimulationInProgress: this$.SimulationInProgress,
        SimulationIsStale: this$.WaveSimulationIsOutOfDate,
        TopMenu: this$.TopMenuOpenState,
        ViewerWidth: this$.WaveSimViewerWidth,
    };
}

export function reduceApprox(this$) {
    const CurrProject = (this$.PopupViewFunc == null) ? false : true;
    const CurrProject_1 = (this$.PopupViewFunc == null) ? false : true;
    return {
        Clipboard: this$.Clipboard,
        CreateComponent: this$.LastCreatedComponent,
        CurrProject: CurrProject,
        CurrProject: CurrProject_1,
        DragMode: this$.DividerDragMode,
        HasUnsavedChanges: false,
        LastUsedDialogWidth: this$.LastUsedDialogWidth,
        PopupDialogData: this$.PopupDialogData,
        RightTab: this$.RightPaneTabVisible,
        SimulationInProgress: this$.SimulationInProgress,
        SimulationIsStale: this$.WaveSimulationIsOutOfDate,
        ViewerWidth: this$.WaveSimViewerWidth,
    };
}

export function setActivity(f, model) {
    return new Model(f(model.AsyncActivity), model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
}

export function getDetailedState(model) {
    let option_1;
    const option = Draw2dWrapper__GetCanvasState(model.Diagram);
    option_1 = map((tupledArg) => extractState(tupledArg[0], tupledArg[1]), option);
    return defaultArg(option_1, [empty_1(), empty_1()]);
}

export function getReducedState(model) {
    const option = Draw2dWrapper__GetCanvasState(model.Diagram);
    return map((tupledArg) => extractReducedState(tupledArg[0], tupledArg[1]), option);
}

export function addReducedState(a, name, model) {
    const lastState = a.LastSavedCanvasState;
    const matchValue = getReducedState(model);
    if (matchValue != null) {
        const state = matchValue;
        return FSharpMap__Add(lastState, name, state);
    }
    else {
        return lastState;
    }
}

export function changeSimulationIsStale(b, m) {
    return new Model(m.AsyncActivity, m.WaveSim, m.Diagram, m.IsLoading, b, m.LastSimulatedCanvasState, m.LastDetailedSavedState, m.CurrentSelected, m.LastSelectedIds, m.LastUsedDialogWidth, m.SelectedComponent, m.CurrentStepSimulationStep, m.RightPaneTabVisible, m.Hilighted, m.Clipboard, m.LastCreatedComponent, m.SavedSheetIsOutOfDate, m.CurrentProj, m.PopupViewFunc, m.PopupDialogData, m.Notifications, m.TopMenuOpenState, m.DividerDragMode, m.WaveSimViewerWidth, m.SimulationInProgress, m.ConnsOfSelectedWavesAreHighlighted, m.CheckWaveformScrollPosition);
}

export function getComponentIds(model) {
    const extractIds = (tupledArg) => {
        const jsComps = tupledArg[0];
        const jsConns = tupledArg[1];
        let list_1;
        list_1 = map_1(extractComponent, jsComps);
        return map_1((comp) => (new ComponentId(0, comp.Id)), list_1);
    };
    let elements;
    let option_1;
    const option = Draw2dWrapper__GetCanvasState(model.Diagram);
    option_1 = map(extractIds, option);
    elements = defaultArg(option_1, empty_1());
    return ofList(elements, {
        Compare: compareSafe,
    });
}

export function waveSimModel2SavedWaveInfo(wsMod) {
    const pars = wsMod.SimParams;
    return new SavedWaveInfo(pars.ClkSvgWidth, pars.CursorTime, pars.WaveViewerRadix, pars.LastClkTime, wsMod.SimParams.DispNames);
}

export function savedWaveInfo2WaveSimModel(sWInfo) {
    const AllNets = empty();
    const SimDataCache = [];
    const DispWaveSVGCache = new SVGCacheT([], empty(), []);
    const AllWaveNames = [];
    const SimParams = new SimParamsT(sWInfo.Radix, 9, 0, 1, sWInfo.DisplayedPortIds, void 0);
    return new WaveSimModel(void 0, SimParams, AllWaveNames, AllNets, DispWaveSVGCache, SimDataCache, false, new WSViewT(0), void 0, void 0);
}

export function getSheetWaveSimOpt(model) {
    return bind((p) => tryFind(p.OpenFileName, model.WaveSim[0]), model.CurrentProj);
}

export function getSheetWaveSimErr(model) {
    let option_1;
    option_1 = map((p) => model.WaveSim[1], model.CurrentProj);
    return defaultArg(option_1, void 0);
}

export function getSheetWaveCanvasState(model) {
    let option_1;
    const option = getSheetWaveSimOpt(model);
    option_1 = map((ws) => ws.LastCanvasState, option);
    return defaultArg(option_1, void 0);
}

export function getSheetWaveNetList(model) {
    const option = getSheetWaveCanvasState(model);
    return map((tupledArg) => getNetList(tupledArg[0], tupledArg[1]), option);
}

export function spComp(comp) {
    const matchValue = comp.Type;
    if (matchValue.tag === 16) {
        const ol = matchValue.fields[0].OutputLabels;
        const name = matchValue.fields[0].Name;
        const il = matchValue.fields[0].InputLabels;
        const clo1 = toText(printf("Custom:%s(ins=%A:outs=%A)"));
        const clo2 = clo1(name);
        const clo3 = clo2(il);
        return clo3(il);
    }
    else {
        const x = matchValue;
        const clo1_1 = toText(printf("%A"));
        return clo1_1(x);
    }
}

export function spConn(conn) {
    const clo1 = toText(printf("Conn:%A"));
    return clo1(conn.Vertices);
}

export function spState(_arg1_0, _arg1_1) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    const arg10 = map_1(spComp, comps);
    const arg20 = map_1(spConn, conns);
    const clo1 = toText(printf("Canvas\u003c%A,%A\u003e"));
    const clo2 = clo1(arg10);
    return clo2(arg20);
}

export function spCanvas(model) {
    let option_2;
    let option_1;
    const option = Draw2dWrapper__GetCanvasState(model.Diagram);
    option_1 = map((tupledArg) => extractState(tupledArg[0], tupledArg[1]), option);
    option_2 = map((tupledArg_1) => spState(tupledArg_1[0], tupledArg_1[1]), option_1);
    return defaultArg(option_2, "None");
}

export function spComps(comps) {
    const arg10 = map_1(spComp, comps);
    const clo1 = toText(printf("Comps%A"));
    return clo1(arg10);
}

export function spOpt(f, thingOpt) {
    if (thingOpt != null) {
        const x = value_1(thingOpt);
        const arg10 = f(x);
        const clo1 = toText(printf("Some %s"));
        return clo1(arg10);
    }
    else {
        return "None";
    }
}

export function spLdComp(ldc) {
    let arg30;
    let comps;
    comps = ldc.CanvasState[0];
    arg30 = spComps(comps);
    const clo1 = toText(printf("LDC\u003c%s:%A:%s\u003e"));
    const clo2 = clo1(ldc.Name);
    const clo3 = clo2(ldc.TimeStamp);
    return clo3(arg30);
}

export function spProj(p) {
    const arg20 = join("\n", map_1(spLdComp, p.LoadedComponents));
    const clo1 = toText(printf("PROJ||Sheet=%s\n%s||ENDP\n"));
    const clo2 = clo1(p.OpenFileName);
    return clo2(arg20);
}

export function pp(model) {
    const arg10 = spCanvas(model);
    const arg20 = spOpt(spProj, model.CurrentProj);
    const clo1 = toConsole(printf("\n%s\n%s"));
    const clo2 = clo1(arg10);
    clo2(arg20);
}

export function spMess(msg) {
    const x = msg;
    const clo1 = toText(printf("MSG\u003c\u003c%20A\u003e\u003eENDM"));
    return clo1(x);
}

export function updateLdComps(name, changeFun, ldComps) {
    return map_1((ldc) => {
        if (ldc.Name === name) {
            return changeFun(ldc);
        }
        else {
            return ldc;
        }
    }, ldComps);
}

export function updateLdCompsWithCompOpt(newCompOpt, ldComps) {
    if (newCompOpt != null) {
        const newComp = newCompOpt;
        const matchValue = tryFind_1((ldc) => (ldc.Name === newComp.Name), ldComps);
        if (matchValue != null) {
            return updateLdComps(newComp.Name, (_arg1) => newComp, ldComps);
        }
        else {
            return cons(newComp, ldComps);
        }
    }
    else {
        return ldComps;
    }
}

export function getCurrFileWSMod(model) {
    const wsMap = model.WaveSim;
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const proj = matchValue;
        return tryFind(proj.OpenFileName, wsMap[0]);
    }
    else {
        return void 0;
    }
}

export function getWSModelOrFail(model, errMsg) {
    const matchValue = getCurrFileWSMod(model);
    if (matchValue == null) {
        const clo1 = toFail(printf("%s"));
        return clo1(errMsg);
    }
    else {
        const ws = matchValue;
        return ws;
    }
}

export function getCurrFileWSModNextView(model) {
    const option = getCurrFileWSMod(model);
    return bind((ws) => ws.WSTransition, option);
}

export function getCurrFile(model) {
    const matchValue = model.CurrentProj;
    if (matchValue == null) {
        return void 0;
    }
    else {
        const proj = matchValue;
        return proj.OpenFileName;
    }
}

export function setCurrFileWSMod(ws, model) {
    const matchValue = getCurrFile(model);
    if (matchValue == null) {
        return model;
    }
    else {
        const fileName = matchValue;
        const WaveSim = [add(fileName, ws, model.WaveSim[0]), model.WaveSim[1]];
        return new Model(model.AsyncActivity, WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
    }
}

export function updateCurrFileWSMod(updateFun, model) {
    const wsMap = model.WaveSim;
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const proj = matchValue;
        let option_1;
        const option = tryFind(proj.OpenFileName, wsMap[0]);
        option_1 = map((wsModel) => {
            const ws$0027 = updateFun(wsModel);
            return setCurrFileWSMod(ws$0027, model);
        }, option);
        return defaultArg(option_1, model);
    }
    else {
        return model;
    }
}

