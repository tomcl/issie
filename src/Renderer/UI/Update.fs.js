import { toList, add, tryFind as tryFind_1, FSharpMap__TryFind, FSharpMap__Add, empty, map as map_2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { Draw2dWrapper__GetComponentById_Z721C83C5, Draw2dWrapper__HighlightComponent, Draw2dWrapper__UnHighlightComponent_Z721C83C5, Draw2dWrapper__ResetSelected, Draw2dWrapper__UnHighlightConnection_Z721C83C5, Draw2dWrapper__printCanvas_7911749C, Draw2dWrapper__DeleteSelected, Draw2dWrapper__Redo, Draw2dWrapper__Undo, Draw2dWrapper__GetSelected, Draw2dWrapper__InitCanvas_Z25B2DB98, Draw2dWrapper__HighlightConnection, Draw2dWrapper__GetCanvasState, Draw2dWrapper__UpdateSplitWireLabels, Draw2dWrapper__UpdateMergeWiresLabels, Draw2dWrapper__PaintConnection } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { length, cons, contains as contains_1, empty as empty_1, ofArray, singleton, tryFind, iterate, item, map as map_3, fold } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { inferConnectionsWidth, getConnectionWidth } from "../../Common/WidthInferer.fs.js";
import { HighLightColor, Component, Memory, ComponentType, LoadedComponent, JSDiagramMsg$2, ConnectionId } from "../../Common/CommonTypes.fs.js";
import { bind, defaultArg, map as map_4, value as value_5 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { extractComponent, extractState } from "../Interface/Extractor.fs.js";
import { setSimParams, SimParamsT, setEditorView, WaveSimModel, getCurrFileWSModNextView, setEditorNextView, updateCurrFileWSMod, PopupDialogData as PopupDialogData_6, RightTab, setCurrFileWSMod, getCurrFileWSMod, changeSimulationIsStale, addReducedState, getReducedState, setActivity, AsyncTasksT, Project, updateLdCompsWithCompOpt, getDetailedState, Msg, Model, Notifications as Notifications_10 } from "./ModelType.fs.js";
import { errorNotification } from "./PopupView.fs.js";
import { join, toText, toConsole, printf, toFail } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { contains, ofSeq } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { hashSafe, equalsSafe, comparePrimitives, equals, compareSafe, equalArrays, compare } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { pasteAction } from "./MainView.fs.js";
import { currWaveSimModel, addFileToProject, saveOpenFileActionWithModelUpdate, saveOpenFileAction } from "./FileMenuView.fs.js";
import { firstTip, zoomDiagram } from "./CatalogueView.fs.js";
import { savePngFile } from "../Interface/FilesIO.fs.js";
import { toString, addSeconds, compare as compare_1, now, minValue } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Date.js";
import { debugTrace, getFailIfNull } from "../Interface/JSHelpers.fs.js";
import { Cmd_none, Cmd_OfFunc_result, Cmd_batch } from "../.fable/Fable.Elmish.3.1.0/cmd.fs.js";
import { map as map_5, truncate } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { SimulationData } from "../../Simulator/SimulatorTypes.fs.js";
import { FSharpResult$2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { simulateAndMakeWaves, adjustPars } from "./WaveSimHelpers.fs.js";

function repaintConnections(model, connsWidth) {
    let value;
    value = map_2((_arg1, width) => {
        const connId = _arg1.fields[0];
        if (width != null) {
            const w = width | 0;
            Draw2dWrapper__PaintConnection(model.Diagram, connId, w, void 0);
        }
    }, connsWidth);
    void value;
}

function mapPortIdsToConnWidth(conns, connsWidth) {
    const state = empty();
    return fold((map, conn) => {
        let width;
        width = getConnectionWidth(connsWidth, new ConnectionId(0, conn.Id));
        const map_1 = FSharpMap__Add(map, conn.Source.Id, width);
        return FSharpMap__Add(map_1, conn.Target.Id, width);
    }, state, conns);
}

function repaintBusComponents(model, connsWidth, state_0, state_1) {
    const state = [state_0, state_1];
    const conns = state[1];
    const comps = state[0];
    const portIdsToConnWidth = mapPortIdsToConnWidth(conns, connsWidth);
    const lookupWidthOnPort = (portId) => {
        const matchValue = FSharpMap__TryFind(portIdsToConnWidth, portId);
        if (matchValue != null) {
            const w = value_5(matchValue);
            return w;
        }
        else {
            return void 0;
        }
    };
    let value;
    value = map_3((comp) => {
        const matchValue_1 = comp.Type;
        switch (matchValue_1.tag) {
            case 17: {
                const arg10 = lookupWidthOnPort(item(0, comp.InputPorts).Id);
                const arg20 = lookupWidthOnPort(item(1, comp.InputPorts).Id);
                const arg30 = lookupWidthOnPort(item(0, comp.OutputPorts).Id);
                Draw2dWrapper__UpdateMergeWiresLabels(model.Diagram, comp.Id, arg10, arg20, arg30);
                break;
            }
            case 18: {
                const topOutputWidth = matchValue_1.fields[0] | 0;
                const arg10_1 = lookupWidthOnPort(item(0, comp.InputPorts).Id);
                const arg30_1 = lookupWidthOnPort(item(1, comp.OutputPorts).Id);
                Draw2dWrapper__UpdateSplitWireLabels(model.Diagram, comp.Id, arg10_1, topOutputWidth, arg30_1);
                break;
            }
            default: {
            }
        }
    }, comps);
    void value;
}

function runBusWidthInference(model) {
    const matchValue = Draw2dWrapper__GetCanvasState(model.Diagram);
    if (matchValue != null) {
        const jsState = matchValue;
        const state = extractState(jsState[0], jsState[1]);
        let _arg1;
        const tupledArg = state;
        _arg1 = inferConnectionsWidth(tupledArg[0], tupledArg[1]);
        if (_arg1.tag === 0) {
            const connsWidth = _arg1.fields[0];
            repaintConnections(model, connsWidth);
            repaintBusComponents(model, connsWidth, state[0], state[1]);
            let Notifications_1;
            const inputRecord_1 = model.Notifications;
            Notifications_1 = (new Notifications_10(void 0, inputRecord_1.FromSimulation, inputRecord_1.FromWaveSim, inputRecord_1.FromFiles, inputRecord_1.FromMemoryEditor, inputRecord_1.FromProperties));
            return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_1, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
        }
        else {
            const e = _arg1.fields[0];
            iterate((_arg1_1) => {
                const c = _arg1_1.fields[0];
                Draw2dWrapper__HighlightConnection(model.Diagram, c, "red");
            }, e.ConnectionsAffected);
            let Notifications;
            const inputRecord = model.Notifications;
            Notifications = (new Notifications_10(((dispatch) => errorNotification(e.Msg, new Msg(27), dispatch)), inputRecord.FromSimulation, inputRecord.FromWaveSim, inputRecord.FromFiles, inputRecord.FromMemoryEditor, inputRecord.FromProperties));
            return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
        }
    }
    else {
        return model;
    }
}

function getSimulationDataOrFail(model, msg) {
    const matchValue = model.CurrentStepSimulationStep;
    if (matchValue != null) {
        const sim = matchValue;
        if (sim.tag === 0) {
            const simData = sim.fields[0];
            return simData;
        }
        else {
            const clo1_1 = toFail(printf("what? Getting simulation data when could not start because of error: %s"));
            return clo1_1(msg);
        }
    }
    else {
        const clo1 = toFail(printf("what? Getting simulation data when no simulation is running: %s"));
        return clo1(msg);
    }
}

function handleJSDiagramMsg(msg, model) {
    let tupledArg_2, tupledArg_3, clo1;
    switch (msg.tag) {
        case 1: {
            const jsComponent = msg.fields[0];
            let SelectedComponent;
            const arg0 = extractComponent(jsComponent);
            SelectedComponent = arg0;
            return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
        }
        case 2: {
            return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, void 0, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
        }
        case 3: {
            return runBusWidthInference(model);
        }
        case 4: {
            const s = msg.fields[0];
            const isNotEquiv = (tupledArg) => {
                const comps1 = tupledArg[0];
                const conns1 = tupledArg[1];
                return (tupledArg_1) => {
                    const comps2 = tupledArg_1[0];
                    const conns2 = tupledArg_1[1];
                    return (!ofSeq(comps1, {
                        Compare: compare,
                    }).Equals(ofSeq(comps2, {
                        Compare: compare,
                    }))) ? true : (!ofSeq(conns1, {
                        Compare: compare,
                    }).Equals(ofSeq(conns2, {
                        Compare: compare,
                    })));
                };
            };
            const nState = getDetailedState(model);
            if ((s ? (tupledArg_2 = model.LastDetailedSavedState, tupledArg_3 = nState, (clo1 = isNotEquiv([tupledArg_2[0], tupledArg_2[1]]), clo1([tupledArg_3[0], tupledArg_3[1]]))) : false) ? (!model.IsLoading) : false) {
                return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, s, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
            }
            else if (!s) {
                return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, nState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, false, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
            }
            else {
                return model;
            }
        }
        default: {
            const canvas = msg.fields[0];
            Draw2dWrapper__InitCanvas_Z25B2DB98(model.Diagram, canvas);
            return model;
        }
    }
}

function handleKeyboardShortcutMsg(msg, model) {
    let inputRecord, LastSavedCanvasState, redState_1, ldc;
    switch (msg.tag) {
        case 1: {
            const matchValue_1 = Draw2dWrapper__GetSelected(model.Diagram);
            if (matchValue_1 != null) {
                const jsState = matchValue_1;
                const Clipboard = extractState(jsState[0], jsState[1]);
                return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
            }
            else {
                return model;
            }
        }
        case 2: {
            pasteAction(model);
            return model;
        }
        case 3: {
            Draw2dWrapper__Undo(model.Diagram);
            return model;
        }
        case 4: {
            Draw2dWrapper__Redo(model.Diagram);
            return model;
        }
        case 5: {
            Draw2dWrapper__DeleteSelected(model.Diagram);
            return model;
        }
        default: {
            const opt = saveOpenFileAction(false, model);
            const ldcOpt = map_4((tuple) => tuple[0], opt);
            const redState = map_4((tuple_1) => tuple_1[1], opt);
            let proj$0027;
            const matchValue = model.CurrentProj;
            if (matchValue == null) {
                proj$0027 = (void 0);
            }
            else {
                const p = matchValue;
                const lcs = updateLdCompsWithCompOpt(ldcOpt, p.LoadedComponents);
                proj$0027 = (new Project(p.ProjectPath, p.OpenFileName, lcs));
            }
            return new Model((inputRecord = model.AsyncActivity, (LastSavedCanvasState = ((opt != null) ? (redState_1 = opt[1], (ldc = opt[0], (aState) => FSharpMap__Add(aState, ldc.Name, redState_1))) : ((x) => x))(model.AsyncActivity.LastSavedCanvasState), new AsyncTasksT(inputRecord.AutoSave, inputRecord.LastAutoSave, inputRecord.LastAutoSaveCheck, LastSavedCanvasState, inputRecord.RunningSimulation))), model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, false, proj$0027, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
        }
    }
}

export function getMenuView(act, model, dispatch) {
    switch (act.tag) {
        case 1: {
            const value = saveOpenFileActionWithModelUpdate(model, dispatch);
            void value;
            dispatch((new Msg(0, new JSDiagramMsg$2(4, false))));
            break;
        }
        case 2: {
            addFileToProject(model, dispatch);
            break;
        }
        case 3: {
            const z = act.fields[0];
            zoomDiagram(z, model);
            break;
        }
        default: {
            const matchValue = model.CurrentProj;
            if (matchValue != null) {
                const p = matchValue;
                Draw2dWrapper__printCanvas_7911749C(model.Diagram, (fn, png) => {
                    savePngFile(p.ProjectPath, p.OpenFileName, png);
                });
            }
        }
    }
    return model;
}

export function getCurrentTimeStamp(model) {
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const p = matchValue;
        let _arg1;
        _arg1 = tryFind((lc) => (lc.Name === p.OpenFileName), p.LoadedComponents);
        if (_arg1 == null) {
            let arg20;
            arg20 = map_3((lc_2) => lc_2.Name, p.LoadedComponents);
            const clo1 = toFail(printf("Project inconsistency: can\u0027t find component %s in %A"));
            const clo2 = clo1(p.OpenFileName);
            return clo2(arg20);
        }
        else {
            const lc_1 = _arg1;
            return lc_1.TimeStamp;
        }
    }
    else {
        return minValue();
    }
}

export function updateTimeStamp(model) {
    const setTimeStamp = (lc) => {
        const TimeStamp = now();
        return new LoadedComponent(lc.Name, TimeStamp, lc.FilePath, lc.WaveInfo, lc.CanvasState, lc.InputLabels, lc.OutputLabels);
    };
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const p = matchValue;
        let lcs;
        lcs = map_3((lc_1) => {
            if (lc_1.Name === p.OpenFileName) {
                return setTimeStamp(lc_1);
            }
            else {
                return lc_1;
            }
        }, p.LoadedComponents);
        const CurrentProj = new Project(p.ProjectPath, p.OpenFileName, lcs);
        return new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
    }
    else {
        return model;
    }
}

export function checkSelection(model, cmd) {
    const extractIds = (tupledArg) => {
        const jsComps = tupledArg[0];
        const jsConns = tupledArg[1];
        let compIds;
        compIds = map_3((comp) => getFailIfNull(comp, singleton("id")), jsComps);
        let connIds;
        connIds = map_3((conn) => getFailIfNull(conn, singleton("id")), jsConns);
        return [compIds, connIds];
    };
    const matchValue = Draw2dWrapper__GetSelected(model.Diagram);
    if (matchValue != null) {
        const newSelection = matchValue;
        const newSelectedIds = extractIds(newSelection);
        if (equalArrays(newSelectedIds, model.LastSelectedIds)) {
            return [model, cmd];
        }
        else {
            let model_1;
            const CurrentSelected = extractState(newSelection[0], newSelection[1]);
            model_1 = (new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, CurrentSelected, newSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition));
            return [model_1, Cmd_batch(ofArray([cmd, Cmd_OfFunc_result(new Msg(43))]))];
        }
    }
    else {
        return [model, cmd];
    }
}

export function checkForAutoSaveOrSelectionChanged(msg, model, cmd) {
    const simIsStale = (newState, simState) => {
        const matchValue = [newState, simState];
        if (matchValue[0] != null) {
            if (matchValue[1] != null) {
                const comps = matchValue[1][0];
                const conns = matchValue[1][1];
                const ncomps = matchValue[0][0];
                const nconns = matchValue[0][1];
                if (!ofSeq(ncomps, {
                    Compare: compareSafe,
                }).Equals(ofSeq(comps, {
                    Compare: compareSafe,
                }))) {
                    return true;
                }
                else {
                    return !ofSeq(nconns, {
                        Compare: compareSafe,
                    }).Equals(ofSeq(conns, {
                        Compare: compareSafe,
                    }));
                }
            }
            else {
                return true;
            }
        }
        else {
            return false;
        }
    };
    const needsAutoSave = (proj, newState_1, state) => {
        const matchValue_1 = [newState_1, tryFind_1(proj.OpenFileName, state)];
        let pattern_matching_result, comps_1, conns_1, ncomps_1, nconns_1;
        if (matchValue_1[0] != null) {
            if (matchValue_1[1] != null) {
                pattern_matching_result = 1;
                comps_1 = matchValue_1[1][0];
                conns_1 = matchValue_1[1][1];
                ncomps_1 = matchValue_1[0][0];
                nconns_1 = matchValue_1[0][1];
            }
            else {
                pattern_matching_result = 0;
            }
        }
        else {
            pattern_matching_result = 0;
        }
        switch (pattern_matching_result) {
            case 0: {
                return false;
            }
            case 1: {
                if (!ofSeq(ncomps_1, {
                    Compare: compareSafe,
                }).Equals(ofSeq(comps_1, {
                    Compare: compareSafe,
                }))) {
                    return true;
                }
                else {
                    return !ofSeq(nconns_1, {
                        Compare: compareSafe,
                    }).Equals(ofSeq(conns_1, {
                        Compare: compareSafe,
                    }));
                }
            }
        }
    };
    if (msg.tag === 42) {
        return checkSelection(model, cmd);
    }
    else if (compare_1(now(), addSeconds(model.AsyncActivity.LastAutoSaveCheck, 0.1)) < 0) {
        return [model, cmd];
    }
    else {
        const patternInput = checkSelection(model, cmd);
        const model_1 = patternInput[0];
        const cmd_1 = patternInput[1];
        const model_2 = setActivity((a) => {
            const LastAutoSaveCheck = now();
            return new AsyncTasksT(a.AutoSave, a.LastAutoSave, LastAutoSaveCheck, a.LastSavedCanvasState, a.RunningSimulation);
        }, model_1);
        const matchValue_2 = model_2.CurrentProj;
        if (matchValue_2 != null) {
            const proj_1 = matchValue_2;
            const newReducedState = getReducedState(model_2);
            const update_1 = (!model_2.IsLoading) ? needsAutoSave(proj_1, newReducedState, model_2.AsyncActivity.LastSavedCanvasState) : false;
            const simUpdate = simIsStale(newReducedState, model_2.LastSimulatedCanvasState);
            let model_6;
            let model_5;
            if (update_1) {
                let arg10;
                let copyOfStruct = now();
                arg10 = toString(copyOfStruct, "mm:ss");
                const clo1 = toConsole(printf("AutoSaving at \u0027%s\u0027"));
                clo1(arg10);
                let model$0027;
                let model_4;
                const model_3 = new Model(model_2.AsyncActivity, model_2.WaveSim, model_2.Diagram, model_2.IsLoading, model_2.WaveSimulationIsOutOfDate, model_2.LastSimulatedCanvasState, model_2.LastDetailedSavedState, model_2.CurrentSelected, model_2.LastSelectedIds, model_2.LastUsedDialogWidth, model_2.SelectedComponent, model_2.CurrentStepSimulationStep, model_2.RightPaneTabVisible, model_2.Hilighted, model_2.Clipboard, model_2.LastCreatedComponent, true, model_2.CurrentProj, model_2.PopupViewFunc, model_2.PopupDialogData, model_2.Notifications, model_2.TopMenuOpenState, model_2.DividerDragMode, model_2.WaveSimViewerWidth, model_2.SimulationInProgress, model_2.ConnsOfSelectedWavesAreHighlighted, model_2.CheckWaveformScrollPosition);
                model_4 = updateTimeStamp(model_3);
                model$0027 = setActivity((a_1) => {
                    const LastSavedCanvasState = addReducedState(a_1, proj_1.OpenFileName, model_2);
                    return new AsyncTasksT(a_1.AutoSave, a_1.LastAutoSave, a_1.LastAutoSaveCheck, LastSavedCanvasState, a_1.RunningSimulation);
                }, model_4);
                const value = saveOpenFileAction(true, model$0027);
                void value;
                model_5 = setActivity((a_2) => {
                    const LastAutoSave = FSharpMap__Add(a_2.LastAutoSave, proj_1.OpenFileName, now());
                    return new AsyncTasksT(a_2.AutoSave, LastAutoSave, a_2.LastAutoSaveCheck, a_2.LastSavedCanvasState, a_2.RunningSimulation);
                }, model$0027);
            }
            else {
                model_5 = model_2;
            }
            model_6 = setActivity((a_3) => {
                const matchValue_3 = update_1 ? true : equals(tryFind_1(proj_1.OpenFileName, a_3.LastSavedCanvasState), void 0);
                if (matchValue_3) {
                    const LastSavedCanvasState_1 = addReducedState(a_3, proj_1.OpenFileName, model_2);
                    return new AsyncTasksT(a_3.AutoSave, a_3.LastAutoSave, a_3.LastAutoSaveCheck, LastSavedCanvasState_1, a_3.RunningSimulation);
                }
                else {
                    return a_3;
                }
            }, model_5);
            return [changeSimulationIsStale(simUpdate, model_6), cmd_1];
        }
        else {
            return [model_2, cmd_1];
        }
    }
}

function setSelWavesHighlighted(model, connIds) {
    const patternInput = model.Hilighted;
    const oldConnIds = patternInput[1];
    const errConnIds = patternInput[0][1];
    let currentConnIds;
    let arg00;
    let option_2;
    let option_1;
    const option = Draw2dWrapper__GetCanvasState(model.Diagram);
    option_1 = map_4((tupledArg) => extractState(tupledArg[0], tupledArg[1]), option);
    option_2 = map_4((arg) => {
        let list;
        list = arg[1];
        return map_3((conn) => conn.Id, list);
    }, option_1);
    arg00 = defaultArg(option_2, empty_1());
    currentConnIds = ofSeq(arg00, {
        Compare: comparePrimitives,
    });
    const isCurrent = (cId) => contains(cId, currentConnIds);
    let value_1;
    value_1 = map_3((_arg1) => {
        const c = _arg1.fields[0];
        const matchValue = contains_1(new ConnectionId(0, c), errConnIds, {
            Equals: equalsSafe,
            GetHashCode: hashSafe,
        });
        let pattern_matching_result;
        if (matchValue) {
            pattern_matching_result = 1;
        }
        else if (isCurrent(c)) {
            pattern_matching_result = 0;
        }
        else {
            pattern_matching_result = 1;
        }
        switch (pattern_matching_result) {
            case 0: {
                Draw2dWrapper__UnHighlightConnection_Z721C83C5(model.Diagram, c);
                break;
            }
            case 1: {
                break;
            }
        }
    }, oldConnIds);
    void value_1;
    let value_2;
    value_2 = map_3((_arg2) => {
        const c_1 = _arg2.fields[0];
        const matchValue_1 = contains_1(new ConnectionId(0, c_1), errConnIds, {
            Equals: equalsSafe,
            GetHashCode: hashSafe,
        });
        let pattern_matching_result_1;
        if (matchValue_1) {
            pattern_matching_result_1 = 1;
        }
        else if (isCurrent(c_1)) {
            pattern_matching_result_1 = 0;
        }
        else {
            pattern_matching_result_1 = 1;
        }
        switch (pattern_matching_result_1) {
            case 0: {
                Draw2dWrapper__HighlightConnection(model.Diagram, c_1, "green");
                break;
            }
            case 1: {
                break;
            }
        }
    }, connIds);
    void value_2;
    return fold((st, cId_1) => (contains_1(cId_1, errConnIds, {
        Equals: equalsSafe,
        GetHashCode: hashSafe,
    }) ? st : cons(cId_1, st)), empty_1(), connIds);
}

export function updateComponentMemory(addr, data, compOpt) {
    let Type;
    let pattern_matching_result, comp, ct, mem;
    if (compOpt != null) {
        if (compOpt.Type.tag === 23) {
            pattern_matching_result = 1;
            comp = compOpt;
            ct = compOpt.Type;
            mem = compOpt.Type.fields[0];
        }
        else if (compOpt.Type.tag === 24) {
            pattern_matching_result = 1;
            comp = compOpt;
            ct = compOpt.Type;
            mem = compOpt.Type.fields[0];
        }
        else if (compOpt.Type.tag === 25) {
            pattern_matching_result = 1;
            comp = compOpt;
            ct = compOpt.Type;
            mem = compOpt.Type.fields[0];
        }
        else {
            pattern_matching_result = 2;
        }
    }
    else {
        pattern_matching_result = 0;
    }
    switch (pattern_matching_result) {
        case 0: {
            return void 0;
        }
        case 1: {
            const update_1 = (mem_1, ct_1) => {
                switch (ct_1.tag) {
                    case 23: {
                        return new ComponentType(23, mem_1);
                    }
                    case 24: {
                        return new ComponentType(24, mem_1);
                    }
                    case 25: {
                        return new ComponentType(25, mem_1);
                    }
                    default: {
                        return ct_1;
                    }
                }
            };
            let mem$0027;
            let Data;
            Data = add(addr, data, mem.Data);
            mem$0027 = (new Memory(mem.AddressWidth, mem.WordWidth, Data));
            return Type = update_1(mem$0027, ct), new Component(comp.Id, Type, comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W);
        }
        case 2: {
            return compOpt;
        }
    }
}

export function update(msg, model) {
    let option_2, WaveSim_1, CurrentStepSimulationStep_1, arg0_1, arg0, CurrentStepSimulationStep_2, arg0_3, arg0_2, CurrentStepSimulationStep_3, arg0_5, arg0_4, ClockTickNumber, WaveSim_2, model_1, PopupDialogData, PopupDialogData_1, inputRecord, PopupDialogData_2, inputRecord_1, PopupDialogData_3, n_1, inputRecord_3, n, inputRecord_2, PopupDialogData_4, inputRecord_4, PopupDialogData_5, inputRecord_5, SelectedComponent, Notifications, inputRecord_6, Notifications_1, inputRecord_7, Notifications_2, inputRecord_8, Notifications_3, inputRecord_9, Notifications_4, inputRecord_10, Notifications_5, inputRecord_11, Notifications_6, inputRecord_12, Notifications_7, inputRecord_13, Notifications_8, inputRecord_14, Notifications_9, inputRecord_15, WaveSim_3;
    const getGraphSize = (g) => {
        let option_1;
        option_1 = map_4((sd) => {
            let list;
            list = toList(sd.Graph);
            return length(list) | 0;
        }, g);
        return defaultArg(option_1, -1) | 0;
    };
    const sdlen = getGraphSize((option_2 = getCurrFileWSMod(model), (bind((ws) => ws.InitWaveSimGraph, option_2)))) | 0;
    if (contains("update", debugTrace())) {
        let msgS;
        let strings;
        let source_1;
        let source;
        const clo1 = toText(printf("%A..."));
        source = clo1(msg);
        source_1 = truncate(60, source.split(""));
        strings = map_5((c) => c, source_1);
        msgS = join("", strings);
        const clo1_1 = toConsole(printf("%d %s"));
        const clo2 = clo1_1(sdlen);
        clo2(msgS);
    }
    let tupledArg;
    switch (msg.tag) {
        case 40: {
            const w = msg.fields[0] | 0;
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, w, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 0: {
            const msg$0027 = msg.fields[0];
            tupledArg = [handleJSDiagramMsg(msg$0027, model), Cmd_none()];
            break;
        }
        case 1: {
            const msg$0027_1 = msg.fields[0];
            tupledArg = [handleKeyboardShortcutMsg(msg$0027_1, model), Cmd_none()];
            break;
        }
        case 3: {
            const state = msg.fields[1];
            const name = msg.fields[0];
            tupledArg = [setActivity((a) => {
                const LastSavedCanvasState = add(name, state, a.LastSavedCanvasState);
                return new AsyncTasksT(a.AutoSave, a.LastAutoSave, a.LastAutoSaveCheck, LastSavedCanvasState, a.RunningSimulation);
            }, model), Cmd_none()];
            break;
        }
        case 2: {
            const simData = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, simData, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 4: {
            const wSMod = msg.fields[0];
            tupledArg = [setCurrFileWSMod(wSMod, model), Cmd_none()];
            break;
        }
        case 5: {
            const err = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, [model.WaveSim[0], err], model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 6: {
            const wSMod$0027 = msg.fields[1];
            const fileName = msg.fields[0];
            tupledArg = [(WaveSim_1 = [add(fileName, wSMod$0027, model.WaveSim[0]), model.WaveSim[1]], new Model(model.AsyncActivity, WaveSim_1, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 7: {
            const graph = msg.fields[0];
            const simData_1 = getSimulationDataOrFail(model, "SetSimulationGraph");
            tupledArg = [(CurrentStepSimulationStep_1 = (arg0_1 = (arg0 = (new SimulationData(graph, simData_1.Inputs, simData_1.Outputs, simData_1.IsSynchronous, simData_1.NumberBase, simData_1.ClockTickNumber)), (new FSharpResult$2(0, arg0))), (arg0_1)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, CurrentStepSimulationStep_1, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 8: {
            const numBase = msg.fields[0];
            const simData_2 = getSimulationDataOrFail(model, "SetSimulationBase");
            tupledArg = [(CurrentStepSimulationStep_2 = (arg0_3 = (arg0_2 = (new SimulationData(simData_2.Graph, simData_2.Inputs, simData_2.Outputs, simData_2.IsSynchronous, numBase, simData_2.ClockTickNumber)), (new FSharpResult$2(0, arg0_2))), (arg0_3)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, CurrentStepSimulationStep_2, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 9: {
            const simData_3 = getSimulationDataOrFail(model, "IncrementSimulationClockTick");
            tupledArg = [(CurrentStepSimulationStep_3 = (arg0_5 = (arg0_4 = (ClockTickNumber = ((simData_3.ClockTickNumber + 1) | 0), new SimulationData(simData_3.Graph, simData_3.Inputs, simData_3.Outputs, simData_3.IsSynchronous, simData_3.NumberBase, ClockTickNumber)), (new FSharpResult$2(0, arg0_4))), (arg0_5)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, CurrentStepSimulationStep_3, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 10: {
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, void 0, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 11: {
            tupledArg = [(WaveSim_2 = [empty(), void 0], new Model(model.AsyncActivity, WaveSim_2, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 12: {
            const newTab = msg.fields[0];
            let inferMsg;
            inferMsg = (new Msg(0, new JSDiagramMsg$2(3, void 0)));
            let editCmds;
            editCmds = map_3(Cmd_OfFunc_result, singleton(inferMsg));
            firstTip(true, true);
            tupledArg = [(model_1 = ((!equalsSafe(newTab, new RightTab(3))) ? (Draw2dWrapper__ResetSelected(model.Diagram), runBusWidthInference(model)) : model), (new Model(model_1.AsyncActivity, model_1.WaveSim, model_1.Diagram, model_1.IsLoading, model_1.WaveSimulationIsOutOfDate, model_1.LastSimulatedCanvasState, model_1.LastDetailedSavedState, model_1.CurrentSelected, model_1.LastSelectedIds, model_1.LastUsedDialogWidth, model_1.SelectedComponent, model_1.CurrentStepSimulationStep, newTab, model_1.Hilighted, model_1.Clipboard, model_1.LastCreatedComponent, model_1.SavedSheetIsOutOfDate, model_1.CurrentProj, model_1.PopupViewFunc, model_1.PopupDialogData, model_1.Notifications, model_1.TopMenuOpenState, model_1.DividerDragMode, model_1.WaveSimViewerWidth, model_1.SimulationInProgress, model_1.ConnsOfSelectedWavesAreHighlighted, model_1.CheckWaveformScrollPosition))), (newTab.tag === 1) ? (Cmd_batch(editCmds)) : ((newTab.tag === 2) ? (Cmd_batch(editCmds)) : ((newTab.tag === 3) ? Cmd_none() : (Cmd_batch(editCmds))))];
            break;
        }
        case 13: {
            const connectionIds = msg.fields[1];
            const componentIds = msg.fields[0];
            const patternInput = model.Hilighted[0];
            const oldConnectionIds = patternInput[1];
            const oldComponentIds = patternInput[0];
            let value_1;
            value_1 = map_3((_arg1) => {
                const c_1 = _arg1.fields[0];
                Draw2dWrapper__UnHighlightComponent_Z721C83C5(model.Diagram, c_1);
            }, oldComponentIds);
            void value_1;
            let value_2;
            value_2 = map_3((_arg2) => {
                const c_2 = _arg2.fields[0];
                Draw2dWrapper__HighlightComponent(model.Diagram, new HighLightColor(0), c_2);
            }, componentIds);
            void value_2;
            let value_3;
            value_3 = map_3((_arg3) => {
                const c_3 = _arg3.fields[0];
                Draw2dWrapper__UnHighlightConnection_Z721C83C5(model.Diagram, c_3);
            }, oldConnectionIds);
            void value_3;
            let value_4;
            value_4 = map_3((_arg4) => {
                const c_4 = _arg4.fields[0];
                Draw2dWrapper__HighlightConnection(model.Diagram, c_4, "red");
            }, connectionIds);
            void value_4;
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, [[componentIds, connectionIds], model.Hilighted[1]], model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 14: {
            const connIds = msg.fields[0];
            const lst = setSelWavesHighlighted(model, ofArray(connIds));
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, [model.Hilighted[0], lst], model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, false, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 15: {
            const components = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, components, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 16: {
            const pos = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, pos, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 17: {
            const project = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, project, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 18: {
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, void 0, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 19: {
            const popup = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, popup, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 20: {
            tupledArg = [(PopupDialogData = (new PopupDialogData_6(void 0, void 0, void 0, void 0, void 0)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, void 0, PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 21: {
            const text = msg.fields[0];
            tupledArg = [(PopupDialogData_1 = (inputRecord = model.PopupDialogData, new PopupDialogData_6(text, inputRecord.Int, inputRecord.Int2, inputRecord.MemorySetup, inputRecord.MemoryEditorData)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, PopupDialogData_1, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 22: {
            const int = msg.fields[0];
            tupledArg = [(PopupDialogData_2 = (inputRecord_1 = model.PopupDialogData, new PopupDialogData_6(inputRecord_1.Text, int, inputRecord_1.Int2, inputRecord_1.MemorySetup, inputRecord_1.MemoryEditorData)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, PopupDialogData_2, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 23: {
            const data = msg.fields[0];
            tupledArg = [(PopupDialogData_3 = ((data[1].tag === 1) ? (n_1 = data[0], (inputRecord_3 = model.PopupDialogData, new PopupDialogData_6(inputRecord_3.Text, inputRecord_3.Int, n_1, inputRecord_3.MemorySetup, inputRecord_3.MemoryEditorData))) : (n = data[0], (inputRecord_2 = model.PopupDialogData, new PopupDialogData_6(inputRecord_2.Text, n, inputRecord_2.Int2, inputRecord_2.MemorySetup, inputRecord_2.MemoryEditorData)))), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, PopupDialogData_3, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 24: {
            const m = msg.fields[0];
            tupledArg = [(PopupDialogData_4 = (inputRecord_4 = model.PopupDialogData, new PopupDialogData_6(inputRecord_4.Text, inputRecord_4.Int, inputRecord_4.Int2, m, inputRecord_4.MemoryEditorData)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, PopupDialogData_4, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 25: {
            const m_1 = msg.fields[0];
            tupledArg = [(PopupDialogData_5 = (inputRecord_5 = model.PopupDialogData, new PopupDialogData_6(inputRecord_5.Text, inputRecord_5.Int, inputRecord_5.Int2, inputRecord_5.MemorySetup, m_1)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, PopupDialogData_5, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 26: {
            const data_1 = msg.fields[1];
            const addr = msg.fields[0];
            tupledArg = [(SelectedComponent = updateComponentMemory(addr, data_1, model.SelectedComponent), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 27: {
            tupledArg = [(Notifications = (inputRecord_6 = model.Notifications, new Notifications_10(void 0, inputRecord_6.FromSimulation, inputRecord_6.FromWaveSim, inputRecord_6.FromFiles, inputRecord_6.FromMemoryEditor, inputRecord_6.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 28: {
            const n_2 = msg.fields[0];
            tupledArg = [(Notifications_1 = (inputRecord_7 = model.Notifications, new Notifications_10(inputRecord_7.FromDiagram, n_2, inputRecord_7.FromWaveSim, inputRecord_7.FromFiles, inputRecord_7.FromMemoryEditor, inputRecord_7.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_1, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 29: {
            tupledArg = [(Notifications_2 = (inputRecord_8 = model.Notifications, new Notifications_10(inputRecord_8.FromDiagram, void 0, inputRecord_8.FromWaveSim, inputRecord_8.FromFiles, inputRecord_8.FromMemoryEditor, inputRecord_8.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_2, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 30: {
            tupledArg = [(Notifications_3 = (inputRecord_9 = model.Notifications, new Notifications_10(inputRecord_9.FromDiagram, inputRecord_9.FromSimulation, void 0, inputRecord_9.FromFiles, inputRecord_9.FromMemoryEditor, inputRecord_9.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_3, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 31: {
            const n_3 = msg.fields[0];
            tupledArg = [(Notifications_4 = (inputRecord_10 = model.Notifications, new Notifications_10(inputRecord_10.FromDiagram, inputRecord_10.FromSimulation, inputRecord_10.FromWaveSim, n_3, inputRecord_10.FromMemoryEditor, inputRecord_10.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_4, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 32: {
            tupledArg = [(Notifications_5 = (inputRecord_11 = model.Notifications, new Notifications_10(inputRecord_11.FromDiagram, inputRecord_11.FromSimulation, inputRecord_11.FromWaveSim, void 0, inputRecord_11.FromMemoryEditor, inputRecord_11.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_5, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 33: {
            const n_4 = msg.fields[0];
            tupledArg = [(Notifications_6 = (inputRecord_12 = model.Notifications, new Notifications_10(inputRecord_12.FromDiagram, inputRecord_12.FromSimulation, inputRecord_12.FromWaveSim, inputRecord_12.FromFiles, n_4, inputRecord_12.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_6, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 34: {
            tupledArg = [(Notifications_7 = (inputRecord_13 = model.Notifications, new Notifications_10(inputRecord_13.FromDiagram, inputRecord_13.FromSimulation, inputRecord_13.FromWaveSim, inputRecord_13.FromFiles, void 0, inputRecord_13.FromProperties)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_7, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 35: {
            const n_5 = msg.fields[0];
            tupledArg = [(Notifications_8 = (inputRecord_14 = model.Notifications, new Notifications_10(inputRecord_14.FromDiagram, inputRecord_14.FromSimulation, inputRecord_14.FromWaveSim, inputRecord_14.FromFiles, inputRecord_14.FromMemoryEditor, n_5)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_8, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 36: {
            tupledArg = [(Notifications_9 = (inputRecord_15 = model.Notifications, new Notifications_10(inputRecord_15.FromDiagram, inputRecord_15.FromSimulation, inputRecord_15.FromWaveSim, inputRecord_15.FromFiles, inputRecord_15.FromMemoryEditor, void 0)), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, Notifications_9, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        case 37: {
            const t = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, t, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 38: {
            const width = msg.fields[0] | 0;
            let x;
            const matchValue = model.SelectedComponent;
            if (matchValue != null) {
                const comp = matchValue;
                const matchValue_1 = Draw2dWrapper__GetComponentById_Z721C83C5(model.Diagram, comp.Id);
                if (matchValue_1.tag === 0) {
                    const jsComp = matchValue_1.fields[0];
                    let SelectedComponent_1;
                    const arg0_7 = extractComponent(jsComp);
                    SelectedComponent_1 = arg0_7;
                    x = (new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, width, SelectedComponent_1, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition));
                }
                else {
                    const err_1 = matchValue_1.fields[0];
                    x = (new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, width, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition));
                }
            }
            else {
                x = (new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, width, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition));
            }
            tupledArg = [x, Cmd_none()];
            break;
        }
        case 41: {
            const dispatch = msg.fields[1];
            const act = msg.fields[0];
            tupledArg = [getMenuView(act, model, dispatch), Cmd_none()];
            break;
        }
        case 42: {
            tupledArg = [model, Cmd_none()];
            break;
        }
        case 43: {
            let m_2;
            const matchValue_2 = currWaveSimModel(model);
            let pattern_matching_result;
            if (matchValue_2 != null) {
                if (matchValue_2.WSViewState.tag === 0) {
                    pattern_matching_result = 0;
                }
                else {
                    pattern_matching_result = 1;
                }
            }
            else {
                pattern_matching_result = 0;
            }
            switch (pattern_matching_result) {
                case 0: {
                    m_2 = model;
                    break;
                }
                case 1: {
                    m_2 = (new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, true, model.CheckWaveformScrollPosition));
                    break;
                }
            }
            tupledArg = [m_2, Cmd_none()];
            break;
        }
        case 44: {
            const b = msg.fields[0];
            tupledArg = [changeSimulationIsStale(b, model), Cmd_none()];
            break;
        }
        case 45: {
            const b_1 = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, b_1, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 48: {
            const view = msg.fields[0][0];
            const paras = msg.fields[0][1];
            tupledArg = [updateCurrFileWSMod((ws_1) => setEditorNextView(view, paras, ws_1), model), Cmd_none()];
            break;
        }
        case 47: {
            const matchValue_3 = [getCurrFileWSMod(model), getCurrFileWSModNextView(model)];
            if (matchValue_3[0] != null) {
                if (matchValue_3[1] == null) {
                    tupledArg = [model, Cmd_none()];
                }
                else {
                    const nView = matchValue_3[1][1];
                    const pars = matchValue_3[1][0];
                    const wsMod = matchValue_3[0];
                    const checkCursor = wsMod.SimParams.CursorTime !== pars.CursorTime;
                    const pars$0027 = adjustPars(wsMod, pars, wsMod.SimParams.LastScrollPos);
                    Draw2dWrapper__ResetSelected(model.Diagram);
                    let wsMod$0027;
                    let wsModel;
                    const ws_2 = simulateAndMakeWaves(model, wsMod, pars$0027);
                    wsModel = (new WaveSimModel(ws_2.InitWaveSimGraph, ws_2.SimParams, ws_2.AllWaveNames, ws_2.AllNets, ws_2.DispWaveSVGCache, ws_2.SimDataCache, ws_2.CursorBoxIsEmpty, nView, void 0, ws_2.LastCanvasState));
                    wsMod$0027 = setEditorView(nView, wsModel);
                    let model_3;
                    let model_2;
                    const Hilighted_2 = [model.Hilighted[0], setSelWavesHighlighted(model, empty_1())];
                    model_2 = (new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, Hilighted_2, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition));
                    model_3 = setCurrFileWSMod(wsMod$0027, model_2);
                    tupledArg = [new Model(model_3.AsyncActivity, model_3.WaveSim, model_3.Diagram, model_3.IsLoading, model_3.WaveSimulationIsOutOfDate, model_3.LastSimulatedCanvasState, model_3.LastDetailedSavedState, model_3.CurrentSelected, model_3.LastSelectedIds, model_3.LastUsedDialogWidth, model_3.SelectedComponent, model_3.CurrentStepSimulationStep, model_3.RightPaneTabVisible, model_3.Hilighted, model_3.Clipboard, model_3.LastCreatedComponent, model_3.SavedSheetIsOutOfDate, model_3.CurrentProj, model_3.PopupViewFunc, model_3.PopupDialogData, model_3.Notifications, model_3.TopMenuOpenState, model_3.DividerDragMode, model_3.WaveSimViewerWidth, model_3.SimulationInProgress, model_3.ConnsOfSelectedWavesAreHighlighted, checkCursor), Cmd_none()];
                }
            }
            else {
                throw (new Error("SetSimInProgress dispatched when getCurrFileWSMod is None"));
            }
            break;
        }
        case 49: {
            const cS = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, cS, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
            break;
        }
        case 50: {
            const b_2 = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, b_2), Cmd_none()];
            break;
        }
        case 51: {
            const posOpt = msg.fields[0];
            const updateParas = (sp) => (new SimParamsT(sp.WaveViewerRadix, sp.LastClkTime, sp.CursorTime, sp.ClkSvgWidth, sp.DispNames, posOpt));
            tupledArg = [updateCurrFileWSMod((ws_3) => setSimParams(updateParas, ws_3), model), Cmd_none()];
            break;
        }
        case 46: {
            const wSModel = msg.fields[1];
            const sheetName = msg.fields[0];
            const updateWaveSim = (sheetName_1, wSModel_1, model_4) => {
                const patternInput_1 = model_4.WaveSim;
                const sims = patternInput_1[0];
                const err_2 = patternInput_1[1];
                return [FSharpMap__Add(sims, sheetName_1, wSModel_1), err_2];
            };
            tupledArg = [(WaveSim_3 = updateWaveSim(sheetName, wSModel, model), new Model(model.AsyncActivity, WaveSim_3, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition)), Cmd_none()];
            break;
        }
        default: {
            const mode = msg.fields[0];
            tupledArg = [new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, mode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), Cmd_none()];
        }
    }
    return checkForAutoSaveOrSelectionChanged(msg, tupledArg[0], tupledArg[1]);
}

