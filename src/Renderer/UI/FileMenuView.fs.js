import { TopMenu, getComponentIds, initWS, updateLdCompsWithCompOpt, Model, savedWaveInfo2WaveSimModel, waveSimModel2SavedWaveInfo, Project, Msg, getCurrFile } from "./ModelType.fs.js";
import { empty as empty_1, tryFind, add, containsKey, FSharpMap__get_Item } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { viewInfoPopup, confirmationPopup, unclosablePopup, choicePopup, dialogPopup, dialogPopupBodyOnlyText, getText, errorFilesNotification } from "./PopupView.fs.js";
import { append, maxBy, cons, item, singleton, filter, ofArray, iterate, tryFind as tryFind_1, find, mapIndexed, tryFindIndex, length, map, empty } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { Draw2dWrapper__GetCanvasState, Draw2dWrapper__FlushCommandStack, Draw2dWrapper__GetAndClearLoadComponentErrors, Draw2dWrapper__GetAndClearLoadConnectionErrors, Draw2dWrapper__LoadConnection, Draw2dWrapper__LoadComponent_596CF542, Draw2dWrapper__ClearCanvas } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { equalsSafe, equals } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { toFail, join, printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { Component, CustomComponentType, ComponentType, LoadedComponent, JSDiagramMsg$2 } from "../../Common/CommonTypes.fs.js";
import { loadAllComponentFiles, askForExistingProjectPath, JsonHelpers_stateToJsonString, writeFile, baseName as baseName_2, tryCreateFolder, askForNewProjectPath, removeFile, renameFile, pathJoin, createEmptyDgmFile, makeLoadedComponentFromCanvasData, removeFileWithExtn, saveStateToFile, saveAutoStateToFile, parseDiagramSignature, dirName } from "../Interface/FilesIO.fs.js";
import { extractReducedState, extractState } from "../Interface/Extractor.fs.js";
import { defaultArg, map as map_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { compare, now } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Date.js";
import * as react from "react";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { tryItem, forAll, exists } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { isDigit, isLetterOrDigit } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Char.js";
import { cropToLength, assertThat } from "../../Common/Helpers.fs.js";
import { list as list_4, menu, Item_Option, Item_li } from "../.fable/Fulma.2.9.0/Components/Menu.fs.js";
import { End_div, Brand_div, Option as Option_1, navbar, divider, Item_a, Dropdown_Option, Dropdown_div, Link_a, Item_Option as Item_Option_1, Item_div } from "../.fable/Fulma.2.9.0/Components/Navbar.fs.js";
import { right, item as item_1, left, Level_Option, level } from "../.fable/Fulma.2.9.0/Layouts/Level.fs.js";
import { Color_IColor, Size_ISize, Common_GenericOption } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { Option, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { CSSProp, DOMAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { leftSectionWidth } from "./Style.fs.js";
import { item as item_2, Option as Option_2, breadcrumb } from "../.fable/Fulma.2.9.0/Components/Breadcrumb.fs.js";

export function currWaveSimModel(model) {
    let fileName;
    const matchValue = getCurrFile(model);
    let pattern_matching_result, fileName_1;
    if (matchValue != null) {
        if (fileName = matchValue, containsKey(fileName, model.WaveSim[0])) {
            pattern_matching_result = 0;
            fileName_1 = matchValue;
        }
        else {
            pattern_matching_result = 1;
        }
    }
    else {
        pattern_matching_result = 1;
    }
    switch (pattern_matching_result) {
        case 0: {
            return FSharpMap__get_Item(model.WaveSim[0], fileName_1);
        }
        case 1: {
            return void 0;
        }
    }
}

function displayFileErrorNotification(err, dispatch) {
    const note = errorFilesNotification(err);
    return dispatch(new Msg(31, note));
}

function loadStateIntoModel(compToSetup, waveSim, ldComps, model, dispatch) {
    let arg0_1;
    const JSdispatch = (mess) => {
        dispatch((new Msg(0, mess)));
    };
    const name = compToSetup.Name;
    dispatch(new Msg(13, empty(), empty()));
    Draw2dWrapper__ClearCanvas(model.Diagram);
    dispatch(new Msg(45, true));
    const patternInput = compToSetup.CanvasState;
    const connections = patternInput[1];
    const components = patternInput[0];
    const value = map((arg00) => Draw2dWrapper__LoadComponent_596CF542(model.Diagram, arg00), components);
    void value;
    const value_1 = map((arg10) => {
        Draw2dWrapper__LoadConnection(model.Diagram, true, arg10);
    }, connections);
    void value_1;
    const errs = Draw2dWrapper__GetAndClearLoadConnectionErrors(model.Diagram);
    if (!equals(errs, empty())) {
        let errMsg;
        const arg10_1 = length(errs) | 0;
        const clo1 = toText(printf("Issie failed to load %d connections: which were incorrectly \r\n                saved due to a bug in the draw library.\n \r\n                Please recreate these connections, altering slightly component positions,\n \r\n                or drawing connections in the opposite direction,\r\n                to work around this bug. \n \r\n                If the error repeats please make a bug report"));
        errMsg = clo1(arg10_1);
        const error = errorFilesNotification(errMsg);
        dispatch(new Msg(31, error));
    }
    const errs_1 = Draw2dWrapper__GetAndClearLoadComponentErrors(model.Diagram);
    if (!equals(errs_1, empty())) {
        let errMsg_1;
        const arg10_2 = length(errs_1) | 0;
        const arg20 = join("\n\n", errs_1) + "\n\n";
        const clo1_1 = toText(printf("Issie failed to load %d component: which were incorrectly \r\n                saved due to a bug in the draw library.\n \r\n                Please recreate these connections, altering slightly component positions,\n \r\n                or drawing connections in the opposite direction,\r\n                to work around this bug. \n \r\n                If the error repeats please make a bug report. Details: %s\n\n"));
        const clo2 = clo1_1(arg10_2);
        errMsg_1 = clo2(arg20);
        const error_1 = errorFilesNotification(errMsg_1);
        dispatch(new Msg(31, error_1));
    }
    Draw2dWrapper__FlushCommandStack(model.Diagram);
    JSdispatch(new JSDiagramMsg$2(3, void 0));
    JSdispatch(new JSDiagramMsg$2(4, false));
    dispatch(new Msg(46, name, waveSim));
    dispatch((arg0_1 = (new Project(dirName(compToSetup.FilePath), compToSetup.Name, ldComps)), (new Msg(17, arg0_1))));
    dispatch(new Msg(44, true));
    dispatch(new Msg(45, false));
}

export function updateLoadedComponents(name, setFun, lcLst) {
    const n = tryFindIndex((lc) => (lc.Name === name), lcLst);
    if (n != null) {
        const n_1 = n | 0;
        return mapIndexed((i, x) => ((i === n_1) ? setFun(x) : x), lcLst);
    }
    else {
        const clo1 = toFail(printf("Can\u0027t find name=\u0027%s\u0027 in components:%A"));
        const clo2 = clo1(name);
        return clo2(lcLst);
    }
}

export function updateProjectFromCanvas(model) {
    const matchValue = Draw2dWrapper__GetCanvasState(model.Diagram);
    if (matchValue != null) {
        const state = matchValue;
        const canvas = extractState(state[0], state[1]);
        const patternInput = parseDiagramSignature(canvas[0], canvas[1]);
        const outputs = patternInput[1];
        const inputs = patternInput[0];
        const setLc = (lc) => (new LoadedComponent(lc.Name, lc.TimeStamp, lc.FilePath, lc.WaveInfo, canvas, inputs, outputs));
        return map_1((p) => {
            const LoadedComponents = updateLoadedComponents(p.OpenFileName, setLc, p.LoadedComponents);
            return new Project(p.ProjectPath, p.OpenFileName, LoadedComponents);
        }, model.CurrentProj);
    }
    else {
        return model.CurrentProj;
    }
}

export function getSavedWave(model) {
    const matchValue = currWaveSimModel(model);
    if (matchValue == null) {
        return void 0;
    }
    else {
        const wSModel = matchValue;
        const arg0 = waveSimModel2SavedWaveInfo(wSModel);
        return arg0;
    }
}

export function setSavedWave(compIds, wave, model) {
    const matchValue = [wave, getCurrFile(model)];
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            const fileName = matchValue[1];
            const waveInfo = matchValue[0];
            const WaveSim = [add(fileName, savedWaveInfo2WaveSimModel(waveInfo), model.WaveSim[0]), model.WaveSim[1]];
            return new Model(model.AsyncActivity, WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, model.LastUsedDialogWidth, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition);
        }
        else {
            const waveInfo_1 = matchValue[0];
            return model;
        }
    }
    else {
        return model;
    }
}

export function saveOpenFileAction(isAuto, model) {
    let timeStamp;
    const matchValue = [Draw2dWrapper__GetCanvasState(model.Diagram), model.CurrentProj];
    let pattern_matching_result, jsState, project;
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            pattern_matching_result = 1;
            jsState = matchValue[0];
            project = matchValue[1];
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
            return void 0;
        }
        case 1: {
            const reducedState = extractReducedState(jsState[0], jsState[1]);
            const state = extractState(jsState[0], jsState[1]);
            const savedState = [state, getSavedWave(model)];
            if (isAuto) {
                saveAutoStateToFile(project.ProjectPath, project.OpenFileName, savedState[0], savedState[1]);
                return void 0;
            }
            else {
                saveStateToFile(project.ProjectPath, project.OpenFileName, savedState[0], savedState[1]);
                removeFileWithExtn(".dgmauto", project.ProjectPath, project.OpenFileName);
                let origLdComp;
                origLdComp = find((lc) => (lc.Name === project.OpenFileName), project.LoadedComponents);
                let savedWaveSim;
                const option = tryFind(project.OpenFileName, model.WaveSim[0]);
                savedWaveSim = map_1(waveSimModel2SavedWaveInfo, option);
                return [(timeStamp = now(), makeLoadedComponentFromCanvasData(state[0], state[1], origLdComp.FilePath, timeStamp, savedWaveSim)), reducedState];
            }
        }
    }
}

export function saveOpenFileActionWithModelUpdate(model, dispatch) {
    let arg0, lc;
    const opt = saveOpenFileAction(false, model);
    const ldcOpt = map_1((tuple) => tuple[0], opt);
    let reducedState;
    const option = map_1((tuple_1) => tuple_1[1], opt);
    reducedState = defaultArg(option, [empty(), empty()]);
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const p = matchValue;
        dispatch((arg0 = (lc = updateLdCompsWithCompOpt(ldcOpt, p.LoadedComponents), (new Project(p.ProjectPath, p.OpenFileName, lc))), (new Msg(17, arg0))));
        dispatch(new Msg(3, p.OpenFileName, reducedState));
    }
    else {
        toFail(printf("What? Should never be able to save sheet when project=None"));
    }
    dispatch((new Msg(0, new JSDiagramMsg$2(4, false))));
    return opt;
}

function getFileInProject(name, project) {
    return tryFind_1((comp) => (comp.Name === name), project.LoadedComponents);
}

function isFileInProject(name, project) {
    const _arg1 = getFileInProject(name, project);
    if (_arg1 != null) {
        return true;
    }
    else {
        return false;
    }
}

function createEmptyDiagramFile(projectPath, name) {
    createEmptyDgmFile(projectPath, name);
    const TimeStamp = now();
    return new LoadedComponent(name, TimeStamp, pathJoin([projectPath, name + ".dgm"]), void 0, [empty(), empty()], empty(), empty());
}

export function createEmptyComponentAndFile(pPath, sheetName) {
    let clo1;
    createEmptyDgmFile(pPath, sheetName);
    return new LoadedComponent(sheetName, now(), pathJoin([pPath, (clo1 = toText(printf("%s.dgm")), clo1(sheetName))]), void 0, [empty(), empty()], empty(), empty());
}

export function setupProjectFromComponents(sheetName, ldComps, model, dispatch) {
    let arg0;
    let compToSetup;
    if (ldComps.tail == null) {
        compToSetup = toFail(printf("setupProjectComponents must be called with at least one LoadedComponent"));
    }
    else {
        const comps = ldComps;
        let matchValue;
        matchValue = tryFind_1((comp) => (comp.Name === sheetName), comps);
        if (matchValue != null) {
            const comp_1 = matchValue;
            compToSetup = comp_1;
        }
        else {
            let arg20;
            arg20 = map((c) => c.Name, comps);
            const clo1 = toFail(printf("What? can\u0027t find sheet %s in loaded sheets %A"));
            const clo2 = clo1(sheetName);
            compToSetup = clo2(arg20);
        }
    }
    const matchValue_1 = model.CurrentProj;
    if (matchValue_1 != null) {
        const p = matchValue_1;
        dispatch(new Msg(10));
    }
    let waveSim;
    let option_1;
    option_1 = map_1(savedWaveInfo2WaveSimModel, compToSetup.WaveInfo);
    const value = initWS([], empty_1());
    waveSim = defaultArg(option_1, value);
    loadStateIntoModel(compToSetup, waveSim, ldComps, model, dispatch);
    dispatch((arg0 = (new Project(dirName(compToSetup.FilePath), compToSetup.Name, ldComps)), (new Msg(17, arg0))));
}

function openFileInProject(name, project, model, dispatch) {
    const matchValue = getFileInProject(name, project);
    if (matchValue != null) {
        const lc = matchValue;
        const matchValue_1 = updateProjectFromCanvas(model);
        if (matchValue_1 != null) {
            const p = matchValue_1;
            const opt = saveOpenFileAction(false, model);
            const ldcOpt = map_1((tuple) => tuple[0], opt);
            const ldComps = updateLdCompsWithCompOpt(ldcOpt, project.LoadedComponents);
            let reducedState;
            const option = map_1((tuple_1) => tuple_1[1], opt);
            reducedState = defaultArg(option, [empty(), empty()]);
            const matchValue_2 = model.CurrentProj;
            if (matchValue_2 != null) {
                const p_1 = matchValue_2;
                dispatch(new Msg(3, p_1.OpenFileName, reducedState));
            }
            else {
                toFail(printf("What? Should never be able to save sheet when project=None"));
            }
            dispatch((new Msg(0, new JSDiagramMsg$2(4, false))));
            setupProjectFromComponents(name, ldComps, model, dispatch);
        }
        else {
            toFail(printf("What? current project cannot be None at this point in openFileInProject"));
        }
    }
    else {
        let msg;
        const clo1 = toText(printf("Warning: openFileInProject could not find the component %s in the project"));
        msg = clo1(name);
        console.log(msg);
    }
}

export function renameSheet(oldName, newName, model, dispatch) {
    const saveAllFilesFromProject = (proj) => {
        iterate((ldc) => {
            const name = ldc.Name;
            const state = ldc.CanvasState;
            const waveInfo = ldc.WaveInfo;
            saveStateToFile(proj.ProjectPath, name, state, waveInfo);
            removeFileWithExtn(".dgmauto", proj.ProjectPath, name);
        }, proj.LoadedComponents);
    };
    const renameComps = (oldName_1, newName_1, comps) => map((comp) => {
        let customType, compName;
        let pattern_matching_result, compName_1, customType_1;
        if (comp.Type.tag === 16) {
            if (customType = comp.Type.fields[0], (compName = comp.Type.fields[0].Name, compName === oldName_1)) {
                pattern_matching_result = 0;
                compName_1 = comp.Type.fields[0].Name;
                customType_1 = comp.Type.fields[0];
            }
            else {
                pattern_matching_result = 1;
            }
        }
        else {
            pattern_matching_result = 1;
        }
        switch (pattern_matching_result) {
            case 0: {
                const Type = new ComponentType(16, new CustomComponentType(newName_1, customType_1.InputLabels, customType_1.OutputLabels));
                return new Component(comp.Id, Type, comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y, comp.H, comp.W);
            }
            case 1: {
                const c = comp;
                return c;
            }
        }
    }, comps);
    const renameCustomComponents = (newName_2, ldComp) => {
        const state_1 = ldComp.CanvasState;
        const CanvasState = [renameComps(oldName, newName_2, state_1[0]), state_1[1]];
        return new LoadedComponent(ldComp.Name, ldComp.TimeStamp, ldComp.FilePath, ldComp.WaveInfo, CanvasState, ldComp.InputLabels, ldComp.OutputLabels);
    };
    const renameSheetsInProject = (oldName_2, newName_3, proj_1) => {
        const OpenFileName = (proj_1.OpenFileName === oldName_2) ? newName_3 : proj_1.OpenFileName;
        let LoadedComponents;
        LoadedComponents = map((ldComp_1) => {
            let lcName;
            if (lcName = ldComp_1.Name, lcName === oldName_2) {
                const lcName_1 = ldComp_1.Name;
                return new LoadedComponent(newName_3, ldComp_1.TimeStamp, ldComp_1.FilePath, ldComp_1.WaveInfo, ldComp_1.CanvasState, ldComp_1.InputLabels, ldComp_1.OutputLabels);
            }
            else {
                return renameCustomComponents(newName_3, ldComp_1);
            }
        }, proj_1.LoadedComponents);
        return new Project(proj_1.ProjectPath, OpenFileName, LoadedComponents);
    };
    const matchValue = updateProjectFromCanvas(model);
    if (matchValue != null) {
        const p = matchValue;
        const opt = saveOpenFileAction(false, model);
        const ldcOpt = map_1((tuple) => tuple[0], opt);
        const ldComps = updateLdCompsWithCompOpt(ldcOpt, p.LoadedComponents);
        let reducedState;
        const option = map_1((tuple_1) => tuple_1[1], opt);
        reducedState = defaultArg(option, [empty(), empty()]);
        dispatch(new Msg(3, p.OpenFileName, reducedState));
        dispatch((new Msg(0, new JSDiagramMsg$2(4, false))));
        const proj$0027 = renameSheetsInProject(oldName, newName, p);
        setupProjectFromComponents(proj$0027.OpenFileName, proj$0027.LoadedComponents, model, dispatch);
        iterate((extn) => {
            renameFile(extn, proj$0027.ProjectPath, oldName, newName);
        }, ofArray([".dgm", ".dgmauto"]));
        saveAllFilesFromProject(proj$0027);
    }
    else {
        toFail(printf("What? current project cannot be None at this point in renamesheet"));
    }
}

export function renameFileInProject(name, project, model, dispatch) {
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const project_1 = matchValue;
        const title = "Rename sheet in project";
        const before = (dialogData) => {
            let value, option, s_10, clo1, clo2;
            const dialogText = getText(dialogData);
            let maybeWarning;
            if (isFileInProject(dialogText, project_1)) {
                const props = [["style", {
                    color: "red",
                }]];
                maybeWarning = react.createElement("div", keyValueList(props, 1), "This sheet already exists.");
            }
            else if (exists((y) => ("." === y), dialogText.split(""))) {
                const props_2 = [["style", {
                    color: "red",
                }]];
                maybeWarning = react.createElement("div", keyValueList(props_2, 1), "The new name cannot contain a file suffix");
            }
            else if (value = forAll(isLetterOrDigit, dialogText.split("")), (!value)) {
                const props_4 = [["style", {
                    color: "red",
                }]];
                maybeWarning = react.createElement("div", keyValueList(props_4, 1), "The new name must be alphanumeric");
            }
            else if (equals((option = (tryItem(0, dialogText.split(""))), (map_1(isDigit, option))), true)) {
                const props_6 = [["style", {
                    color: "red",
                }]];
                maybeWarning = react.createElement("div", keyValueList(props_6, 1), "The new name must not start with a digit");
            }
            else {
                maybeWarning = react.createElement("div", {});
            }
            const children_10 = [("Warning: the current sheet will be saved during this operation."), react.createElement("br", {}), ("Names of existing components in other sheets that use the renamed sheet will still reflect the old sheet name."), (" You may change names manually if you wish, operation does not depend on the name."), react.createElement("br", {}), react.createElement("br", {}), (s_10 = (clo1 = toText(printf("Sheet %s will be renamed as %s:")), clo2 = clo1(name), clo2(dialogText)), (s_10)), react.createElement("br", {}), react.createElement("br", {}), maybeWarning];
            return react.createElement("div", {}, ...children_10);
        };
        const placeholder = "New name for design sheet";
        const body = (dialogData_1) => dialogPopupBodyOnlyText(before, placeholder, dispatch, dialogData_1);
        const buttonText = "Rename";
        const buttonAction = (dialogData_2) => {
            const newName = getText(dialogData_2).toLocaleLowerCase();
            renameSheet(name, newName, model, dispatch);
            dispatch(new Msg(20));
        };
        const isDisabled = (dialogData_3) => {
            const dialogText_1 = getText(dialogData_3);
            if (isFileInProject(dialogText_1, project_1)) {
                return true;
            }
            else {
                return dialogText_1 === "";
            }
        };
        dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
    }
    else {
        console.log("Warning: renameFileInProject called when no project is currently open");
    }
}

function removeFileInProject(name, project, model, dispatch) {
    let value;
    removeFile(project.ProjectPath, name);
    removeFile(project.ProjectPath, name + "auto");
    const newComponents = filter((lc) => (lc.Name !== name), project.LoadedComponents);
    let newComponents_1;
    const matchValue = newComponents.tail == null;
    newComponents_1 = (matchValue ? singleton(createEmptyDiagramFile(project.ProjectPath, "main")) : newComponents);
    const project_1 = new Project(project.ProjectPath, project.OpenFileName, newComponents_1);
    dispatch((new Msg(17, project_1)));
    assertThat((value = (project_1.LoadedComponents.tail == null), (!value)), "removeFileInProject");
    const matchValue_1 = name === project_1.OpenFileName;
    if (matchValue_1) {
        openFileInProject(item(0, project_1.LoadedComponents).Name, project_1, model, dispatch);
    }
}

export function addFileToProject(model, dispatch) {
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const project = matchValue;
        const title = "Add sheet to project";
        const before = (dialogData) => {
            let s_2;
            const dialogText = getText(dialogData);
            let maybeWarning;
            if (isFileInProject(dialogText, project)) {
                const props = [["style", {
                    color: "red",
                }]];
                maybeWarning = react.createElement("div", keyValueList(props, 1), "This sheet already exists.");
            }
            else {
                maybeWarning = react.createElement("div", {});
            }
            const children_4 = ["A new sheet will be created at:", react.createElement("br", {}), (s_2 = pathJoin([project.ProjectPath, dialogText + ".dgm"]), (s_2)), maybeWarning];
            return react.createElement("div", {}, ...children_4);
        };
        const placeholder = "Insert design sheet name";
        const body = (dialogData_1) => dialogPopupBodyOnlyText(before, placeholder, dispatch, dialogData_1);
        const buttonText = "Add";
        const buttonAction = (dialogData_2) => {
            const name = getText(dialogData_2).toLocaleLowerCase();
            createEmptyDgmFile(project.ProjectPath, name);
            let newComponent;
            const TimeStamp = now();
            newComponent = (new LoadedComponent(name, TimeStamp, pathJoin([project.ProjectPath, name + ".dgm"]), void 0, [empty(), empty()], empty(), empty()));
            const updatedProject = new Project(project.ProjectPath, name, cons(newComponent, project.LoadedComponents));
            openFileInProject(name, updatedProject, model, dispatch);
            dispatch(new Msg(20));
        };
        const isDisabled = (dialogData_3) => {
            const dialogText_1 = getText(dialogData_3);
            if (isFileInProject(dialogText_1, project)) {
                return true;
            }
            else {
                return dialogText_1 === "";
            }
        };
        dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
    }
    else {
        console.log("Warning: addFileToProject called when no project is currently open");
    }
}

function closeProject(model, dispatch, _arg1) {
    dispatch(new Msg(10));
    dispatch(new Msg(18));
    Draw2dWrapper__ClearCanvas(model.Diagram);
}

function newProject(model, dispatch, _arg1) {
    const matchValue = askForNewProjectPath();
    if (matchValue != null) {
        const path = matchValue;
        const matchValue_1 = tryCreateFolder(path);
        if (matchValue_1.tag === 0) {
            dispatch(new Msg(10));
            const projectFile = baseName_2(path) + ".dprj";
            writeFile(pathJoin([path, projectFile]), "");
            const initialComponent = createEmptyComponentAndFile(path, "main");
            setupProjectFromComponents("main", singleton(initialComponent), model, dispatch);
        }
        else {
            const err = matchValue_1.fields[0];
            console.log(err);
            const errMsg = "Could not create a folder for the project.";
            displayFileErrorNotification(errMsg, dispatch);
        }
    }
}

export function resolveComponentOpenPopup(pPath_mut, components_mut, resolves_mut, model_mut, dispatch_mut) {
    resolveComponentOpenPopup:
    while (true) {
        const pPath = pPath_mut, components = components_mut, resolves = resolves_mut, model = model_mut, dispatch = dispatch_mut;
        const chooseWhichToOpen = (comps) => maxBy((comp) => comp.TimeStamp, comps, {
            Compare: compare,
        }).Name;
        dispatch(new Msg(20));
        if (resolves.tail != null) {
            if (resolves.head.tag === 2) {
                const autoComp_1 = resolves.head.fields[0];
                const rLst_1 = resolves.tail;
                const errMsg = "Could not load saved project file \u0027%s\u0027 - using autosave file instead";
                displayFileErrorNotification(errMsg, dispatch);
                pPath_mut = pPath;
                components_mut = cons(autoComp_1, components);
                resolves_mut = rLst_1;
                model_mut = model;
                dispatch_mut = dispatch;
                continue resolveComponentOpenPopup;
            }
            else if (resolves.head.tag === 1) {
                const comp_2 = resolves.head.fields[0];
                const rLst_2 = resolves.tail;
                pPath_mut = pPath;
                components_mut = cons(comp_2, components);
                resolves_mut = rLst_2;
                model_mut = model;
                dispatch_mut = dispatch;
                continue resolveComponentOpenPopup;
            }
            else {
                const autoComp = resolves.head.fields[1];
                const ldComp = resolves.head.fields[0];
                const rLst = resolves.tail;
                const buttonAction = (autoSave, _arg1) => {
                    let comp_1;
                    const inputRecord = autoSave ? autoComp : ldComp;
                    const TimeStamp = now();
                    comp_1 = (new LoadedComponent(inputRecord.Name, TimeStamp, inputRecord.FilePath, inputRecord.WaveInfo, inputRecord.CanvasState, inputRecord.InputLabels, inputRecord.OutputLabels));
                    const data = JsonHelpers_stateToJsonString(comp_1.CanvasState, comp_1.WaveInfo);
                    writeFile(comp_1.FilePath, data);
                    resolveComponentOpenPopup(pPath, cons(comp_1, components), rLst, model, dispatch);
                };
                const title = "Warning!";
                let body;
                let s;
                const clo1 = toText(printf("Warning: changes were made to sheet \u0027%s\u0027 after your last Save. There is an automatically saved version which is more uptodate. Do you want to keep the newer AutoSaved version or the older saved version?"));
                s = clo1(ldComp.Name);
                body = s;
                choicePopup(title, body, "Newer AutoSaved file", "Older Saved file", buttonAction, dispatch);
            }
        }
        else {
            setupProjectFromComponents(chooseWhichToOpen(components), components, model, dispatch);
        }
        break;
    }
}

function openProject(model, dispatch, _arg1) {
    const matchValue = askForExistingProjectPath();
    if (matchValue != null) {
        const path = matchValue;
        const matchValue_1 = loadAllComponentFiles(path);
        if (matchValue_1.tag === 0) {
            const componentsToResolve = matchValue_1.fields[0];
            resolveComponentOpenPopup(path, empty(), componentsToResolve, model, dispatch);
        }
        else {
            const err = matchValue_1.fields[0];
            console.log(err);
            const errMsg = "Could not load diagrams files in the project. The files may be malformed.";
            displayFileErrorNotification(errMsg, dispatch);
        }
    }
}

export function viewNoProjectMenu(model, dispatch) {
    const menuItem = (label, action) => Item_li(ofArray([new Item_Option(0, false), new Item_Option(3, action)]), singleton(label));
    const initialMenu = menu(empty(), singleton(list_4(empty(), ofArray([menuItem("New project", (arg20$0040) => {
        newProject(model, dispatch, arg20$0040);
    }), menuItem("Open project", (arg20$0040_1) => {
        openProject(model, dispatch, arg20$0040_1);
    })]))));
    if (model.CurrentProj == null) {
        return unclosablePopup(void 0, initialMenu, void 0, empty());
    }
    else {
        return react.createElement("div", {});
    }
}

export function viewTopMenu(model, messagesFunc, simulateButtonFunc, dispatch) {
    let css_2, b, css_5, s_15, props_6;
    const compIds = getComponentIds(model);
    messagesFunc(model, dispatch);
    const style = ["style", {
        width: "100%",
    }];
    let patternInput;
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const project = matchValue;
        patternInput = [project.ProjectPath, project.OpenFileName];
    }
    else {
        patternInput = ["no open project", "no open sheet"];
    }
    const projectPath = patternInput[0];
    const fileName = patternInput[1];
    const makeFileLine = (name, project_1) => Item_div(singleton(new Item_Option_1(5, singleton(style))), singleton(level(singleton(new Level_Option(0, singleton(style))), ofArray([left(empty(), singleton(item_1(empty(), singleton(name)))), right(singleton(new Common_GenericOption(1, singleton(["style", {
        marginLeft: "20px",
    }]))), ofArray([item_1(empty(), singleton(button(ofArray([new Option(1, new Size_ISize(0)), new Option(4), new Option(0, new Color_IColor(4)), new Option(15, name === project_1.OpenFileName), new Option(17, (_arg1) => {
        openFileInProject(name, project_1, model, dispatch);
    })]), singleton("open")))), item_1(empty(), singleton(button(ofArray([new Option(1, new Size_ISize(0)), new Option(4), new Option(0, new Color_IColor(5)), new Option(17, (_arg2) => {
        renameFileInProject(name, project_1, model, dispatch);
    })]), singleton("rename")))), item_1(empty(), singleton(button(ofArray([new Option(1, new Size_ISize(0)), new Option(4), new Option(0, new Color_IColor(8)), new Option(17, (_arg4) => {
        let s_4;
        const title = "Delete sheet";
        let body;
        const children = ["Are you sure you want to delete the following design sheet?", react.createElement("br", {}), (s_4 = pathJoin([project_1.ProjectPath, name + ".dgm"]), (s_4)), react.createElement("br", {}), ("This action is irreversible.")];
        body = react.createElement("div", {}, ...children);
        const buttonText = "Delete";
        const buttonAction = (_arg3) => {
            removeFileInProject(name, project_1, model, dispatch);
            dispatch(new Msg(20));
        };
        confirmationPopup(title, body, buttonText, buttonAction, dispatch);
    })]), singleton("delete"))))]))]))));
    let fileTab;
    const matchValue_1 = model.CurrentProj;
    if (matchValue_1 != null) {
        const project_2 = matchValue_1;
        let projectFiles;
        projectFiles = map((comp) => makeFileLine(comp.Name, project_2), project_2.LoadedComponents);
        fileTab = Item_div(ofArray([new Item_Option_1(3), new Item_Option_1(5, singleton(new DOMAttr(40, (_arg5) => {
            let arg0;
            dispatch((arg0 = (equalsSafe(model.TopMenuOpenState, new TopMenu(2)) ? (new TopMenu(0)) : (new TopMenu(2))), (new Msg(37, arg0))));
        })))]), ofArray([Link_a(empty(), singleton("Sheets")), Dropdown_div(singleton(new Dropdown_Option(3, singleton((css_2 = singleton(new CSSProp(125, (b = equalsSafe(model.TopMenuOpenState, new TopMenu(2)), b) ? "block" : "none")), ["style", keyValueList(css_2, 1)])))), append(ofArray([Item_a(singleton(new Item_Option_1(5, singleton(new DOMAttr(40, (_arg6) => {
            addFileToProject(model, dispatch);
        })))), singleton("New Sheet")), divider(empty(), empty())]), projectFiles))]));
    }
    else {
        fileTab = Item_div(empty(), empty());
    }
    const props_8 = [leftSectionWidth(model)];
    const children_4 = [navbar(singleton(new Option_1(6, singleton(["style", {
        height: "100%",
        width: "100%",
    }]))), singleton(Brand_div(singleton(new Common_GenericOption(1, singleton(["style", {
        height: "100%",
        width: "100%",
    }]))), ofArray([Item_div(ofArray([new Item_Option_1(3), new Item_Option_1(5, singleton(new DOMAttr(40, (_arg7) => {
        let arg0_1;
        dispatch((arg0_1 = (equalsSafe(model.TopMenuOpenState, new TopMenu(1)) ? (new TopMenu(0)) : (new TopMenu(1))), (new Msg(37, arg0_1))));
    })))]), ofArray([Link_a(empty(), singleton("Project")), Dropdown_div(singleton(new Dropdown_Option(3, singleton((css_5 = singleton(new CSSProp(125, equalsSafe(model.TopMenuOpenState, new TopMenu(1)) ? "block" : "none")), ["style", keyValueList(css_5, 1)])))), ofArray([Item_a(singleton(new Item_Option_1(5, singleton((new DOMAttr(40, (arg20$0040) => {
        newProject(model, dispatch, arg20$0040);
    }))))), singleton("New project")), Item_a(singleton(new Item_Option_1(5, singleton((new DOMAttr(40, (arg20$0040_1) => {
        openProject(model, dispatch, arg20$0040_1);
    }))))), singleton("Open project")), Item_a(singleton(new Item_Option_1(5, singleton((new DOMAttr(40, (arg20$0040_2) => {
        closeProject(model, dispatch, arg20$0040_2);
    }))))), singleton("Close project"))]))])), fileTab, Item_div(empty(), singleton(Item_div(empty(), singleton(breadcrumb(singleton(new Option_2(2)), [item_2(empty(), singleton((s_15 = cropToLength(30, false, projectPath), (s_15)))), item_2(empty(), singleton((props_6 = [["style", {
        fontWeight: "bold",
    }]], react.createElement("span", keyValueList(props_6, 1), fileName))))]))))), Item_div(empty(), singleton(Item_div(empty(), singleton(button(ofArray([new Option(0, model.SavedSheetIsOutOfDate ? (new Color_IColor(6)) : (new Color_IColor(3))), new Option(17, (_arg8) => {
        const value = saveOpenFileActionWithModelUpdate(model, dispatch);
        void value;
    })]), singleton("Save")))))), End_div(empty(), singleton(Item_div(empty(), singleton(simulateButtonFunc(compIds, model, dispatch))))), End_div(empty(), singleton(Item_div(empty(), singleton(button(singleton(new Option(17, (_arg9) => {
        viewInfoPopup(dispatch);
    })), singleton("Info"))))))]))))];
    return react.createElement("div", keyValueList(props_8, 1), ...children_4);
}

