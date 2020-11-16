import { Union } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { LoadedComponent$reflection, LoadedComponent, SavedWaveInfo$reflection, Connection$reflection, Component$reflection } from "../../Common/CommonTypes.fs.js";
import { union_type, class_type, option_type, tuple_type, list_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { compare, now, minValue } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Date.js";
import { SimpleJson_tryParse, SimpleJson_stringify } from "../.fable/Fable.SimpleJson.3.11.0/SimpleJson.fs.js";
import { FSharpResult$2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { createTypeInfo } from "../.fable/Fable.SimpleJson.3.11.0/TypeInfo.Converter.fs.js";
import { Convert_fromJson } from "../.fable/Fable.SimpleJson.3.11.0/Json.Converter.fs.js";
import * as path_1 from "path";
import * as fs from "fs";
import { map, filter, singleton as singleton_1, ofArray, item, fold, length, ofSeq, reverse, empty, cons } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { toConsole, toText, endsWith, printf, toFail, split } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { exists, rangeNumber } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { createObj } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { singleton } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import * as electron from "electron";
import { some, bind } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { isLetterOrDigit } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Char.js";
import { tryFindError } from "../../Common/Helpers.fs.js";

export class JsonHelpers_SavedInfo extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["CanvasOnly", "CanvasWithFileWaveInfo"];
    }
}

export function JsonHelpers_SavedInfo$reflection() {
    return union_type("FilesIO.JsonHelpers.SavedInfo", [], JsonHelpers_SavedInfo, () => [[["Item", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))]], [["Item1", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["Item2", option_type(SavedWaveInfo$reflection())], ["Item3", class_type("System.DateTime")]]]);
}

export function JsonHelpers_SavedInfo__get_getCanvas(self) {
    if (self.tag === 1) {
        const c_1 = self.fields[0];
        return c_1;
    }
    else {
        const c = self.fields[0];
        return c;
    }
}

export function JsonHelpers_SavedInfo__get_getTimeStamp(self) {
    if (self.tag === 1) {
        const ts = self.fields[2];
        return ts;
    }
    else {
        return minValue();
    }
}

export function JsonHelpers_SavedInfo__get_getWaveInfo(self) {
    if (self.tag === 1) {
        const waveInfo = self.fields[1];
        return waveInfo;
    }
    else {
        return void 0;
    }
}

export function JsonHelpers_stateToJsonString(cState, waveInfo) {
    const time = now();
    return SimpleJson_stringify(new JsonHelpers_SavedInfo(1, cState, waveInfo, time));
}

export function JsonHelpers_jsonStringToState(jsonString) {
    let matchValue, inputJson, typeInfo, matchValue_1, inputJson_1, typeInfo_1;
    let _arg1;
    try {
        _arg1 = (new FSharpResult$2(0, (matchValue = SimpleJson_tryParse(jsonString), (matchValue != null) ? (inputJson = matchValue, (typeInfo = createTypeInfo(tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))), Convert_fromJson(inputJson, typeInfo))) : (() => {
            throw (new Error("Couldn\u0027t parse the input JSON string because it seems to be invalid"));
        })())));
    }
    catch (ex) {
        _arg1 = (new FSharpResult$2(1, ex.message));
    }
    if (_arg1.tag === 1) {
        let matchValue_2;
        try {
            matchValue_2 = (new FSharpResult$2(0, (matchValue_1 = SimpleJson_tryParse(jsonString), (matchValue_1 != null) ? (inputJson_1 = matchValue_1, (typeInfo_1 = createTypeInfo(JsonHelpers_SavedInfo$reflection()), Convert_fromJson(inputJson_1, typeInfo_1))) : (() => {
                throw (new Error("Couldn\u0027t parse the input JSON string because it seems to be invalid"));
            })())));
        }
        catch (ex_1) {
            matchValue_2 = (new FSharpResult$2(1, ex_1.message));
        }
        if (matchValue_2.tag === 1) {
            return void 0;
        }
        else {
            const state_1 = matchValue_2.fields[0];
            return state_1;
        }
    }
    else {
        const state = _arg1.fields[0];
        return new JsonHelpers_SavedInfo(0, state);
    }
}

function fileExistsWithExtn(extn, folderPath, baseName_1) {
    const path = path_1.join(folderPath, baseName_1 + extn);
    return fs.existsSync(path);
}

function tryLoadStateFromPath(filePath) {
    if (!fs.existsSync(filePath)) {
        return void 0;
    }
    else {
        const jsonString = fs.readFileSync(filePath, "utf8");
        return JsonHelpers_jsonStringToState(jsonString);
    }
}

export function pathJoin(args) {
    return path_1.join(...args);
}

export function baseName(filePath) {
    return path_1.basename(filePath);
}

export function dirName(filePath) {
    return path_1.dirname(filePath);
}

export function parseDiagramSignature(canvasState_0, canvasState_1) {
    const canvasState = [canvasState_0, canvasState_1];
    const extractIO = (components_mut, inputs_mut, outputs_mut) => {
        extractIO:
        while (true) {
            const components = components_mut, inputs = inputs_mut, outputs = outputs_mut;
            if (components.tail != null) {
                const components$0027 = components.tail;
                const comp = components.head;
                const matchValue = comp.Type;
                switch (matchValue.tag) {
                    case 0: {
                        const width = matchValue.fields[0] | 0;
                        components_mut = components$0027;
                        inputs_mut = cons([comp.Label, width], inputs);
                        outputs_mut = outputs;
                        continue extractIO;
                    }
                    case 1: {
                        const width_1 = matchValue.fields[0] | 0;
                        components_mut = components$0027;
                        inputs_mut = inputs;
                        outputs_mut = cons([comp.Label, width_1], outputs);
                        continue extractIO;
                    }
                    default: {
                        components_mut = components$0027;
                        inputs_mut = inputs;
                        outputs_mut = outputs;
                        continue extractIO;
                    }
                }
            }
            else {
                return [inputs, outputs];
            }
            break;
        }
    };
    const components_1 = canvasState[0];
    const patternInput = extractIO(components_1, empty(), empty());
    const outputs_1 = patternInput[1];
    const inputs_1 = patternInput[0];
    return [reverse(inputs_1), reverse(outputs_1)];
}

export function getBaseNameNoExtension(filePath) {
    const name = baseName(filePath);
    let matchValue;
    const source = split(name, ["."], null, 0);
    matchValue = ofSeq(source);
    if (matchValue.tail != null) {
        if (matchValue.tail.tail == null) {
            const name_1 = matchValue.head;
            return name_1;
        }
        else {
            const firstSplit = matchValue.head;
            const splits = matchValue.tail;
            let rest;
            const list = ofSeq(rangeNumber(0, 1, length(splits) - 2));
            rest = fold((baseName_1, i) => ((name + ".") + item(i, splits)), "", list);
            return firstSplit + rest;
        }
    }
    else {
        return toFail(printf("what? split at . in a filename should never return empty list"));
    }
}

const projectFileFilters = (() => {
    let value_1;
    const value = createObj(ofArray([["name", "ISSIE project file"], ["extensions", ["dprj"]]]));
    value_1 = value;
    return singleton(value_1);
})();

const projectFilters = (() => {
    let value_1;
    const value = createObj(ofArray([["name", "ISSIE project"], ["extensions", [""]]]));
    value_1 = value;
    return singleton(value_1);
})();

export function askForExistingProjectPath() {
    const options = {};
    options.filters = projectFileFilters;
    const w = electron.remote.getCurrentWindow();
    const option = electron.remote.dialog.showOpenDialogSync(w, options);
    return bind((arg) => {
        let _arg1;
        _arg1 = ofSeq(arg);
        if (_arg1.tail != null) {
            const p = _arg1.head;
            const arg0 = path_1.dirname(p);
            return arg0;
        }
        else {
            return void 0;
        }
    }, option);
}

export function askForNewProjectPath() {
    const options = {};
    options.filters = projectFilters;
    options.title = "Enter new ISSIE project directory and name";
    options.nameFieldLabel = "New project name";
    options.buttonLabel = "Create Project";
    options.properties = ["createDirectory", "showOverwriteConfirmation"];
    const w = electron.remote.getCurrentWindow();
    const option = electron.remote.dialog.showSaveDialogSync(options);
    return bind((dPath) => {
        const dir = dirName(dPath);
        let files;
        files = fs.readdirSync(dir);
        if (exists((fn) => endsWith(fn, ".dprj"), files)) {
            electron.remote.dialog.showErrorBox("Invalid project directory", "You are trying to craete a new Issie project inside an existing project directory. This is not allowed, please choose a different directory");
            return askForNewProjectPath();
        }
        else {
            return dPath;
        }
    }, option);
}

export function tryCreateFolder(path) {
    if (exists((ch) => (!isLetterOrDigit(ch)), baseName(path).split(""))) {
        return new FSharpResult$2(1, "\u0027%s\u0027 file or project names nust contain only letters or digits");
    }
    else {
        try {
            const arg0_1 = fs.mkdirSync(path);
            return new FSharpResult$2(0, void 0);
        }
        catch (ex) {
            let arg0_2;
            const clo1 = toText(printf("%A"));
            arg0_2 = clo1(ex);
            return new FSharpResult$2(1, arg0_2);
        }
    }
}

export function removeFileWithExtn(extn, folderPath, baseName_1) {
    const path = path_1.join(folderPath, baseName_1 + extn);
    fs.unlink(path, (value) => {
        void value;
    });
}

export function renameFile(extn, folderPath, baseName_1, newBaseName) {
    const oldPath = path_1.join(folderPath, baseName_1 + extn);
    const newPath = path_1.join(folderPath, newBaseName + extn);
    if (fs.existsSync(oldPath)) {
        fs.renameSync(oldPath, newPath);
    }
}

export function removeFile(folderPath, baseName_1) {
    removeFileWithExtn(".dgm", folderPath, baseName_1);
}

export function removeAutoFile(folderPath, baseName_1) {
    const path = path_1.join(folderPath, baseName_1 + ".dgmauto");
    fs.unlink(path, (value) => {
        void value;
    });
}

export function writeFileBase64(path, data) {
    let options;
    const arg0 = createObj(singleton_1(["encoding", "base64"]));
    options = some(arg0);
    fs.writeFileSync(path, data, some(options));
}

export function writeFile(path, data) {
    let options;
    const arg0 = createObj(singleton_1(["encoding", "utf8"]));
    options = some(arg0);
    fs.writeFileSync(path, data, some(options));
}

export function savePngFile(folderPath, baseName_1, png) {
    const path = pathJoin([folderPath, baseName_1 + ".png"]);
    writeFileBase64(path, png);
}

export function formatSavedState(canvas, wave) {
    return new JsonHelpers_SavedInfo(1, canvas, wave, now());
}

export function saveAutoStateToFile(folderPath, baseName_1, state_0, state_1) {
    const state = [state_0, state_1];
    const path = pathJoin([folderPath, baseName_1 + ".dgmauto"]);
    const data = JsonHelpers_stateToJsonString(state[0], state[1]);
    writeFile(path, data);
}

export function saveStateToFile(folderPath, baseName_1, state_0, state_1) {
    const state = [state_0, state_1];
    const path = pathJoin([folderPath, baseName_1 + ".dgm"]);
    const data = JsonHelpers_stateToJsonString(state[0], state[1]);
    writeFile(path, data);
}

export function createEmptyDgmFile(folderPath, baseName_1) {
    saveStateToFile(folderPath, baseName_1, [empty(), empty()], void 0);
}

export function makeLoadedComponentFromCanvasData(canvas_0, canvas_1, filePath, timeStamp, waveInfo) {
    const canvas = [canvas_0, canvas_1];
    const patternInput = parseDiagramSignature(canvas[0], canvas[1]);
    const outputs = patternInput[1];
    const inputs = patternInput[0];
    const Name = getBaseNameNoExtension(filePath);
    return new LoadedComponent(Name, timeStamp, filePath, waveInfo, canvas, inputs, outputs);
}

function tryLoadComponentFromPath(filePath) {
    const matchValue = tryLoadStateFromPath(filePath);
    if (matchValue != null) {
        const state = matchValue;
        let arg0_1;
        const tupledArg = JsonHelpers_SavedInfo__get_getCanvas(state);
        const timeStamp = JsonHelpers_SavedInfo__get_getTimeStamp(state);
        const waveInfo = JsonHelpers_SavedInfo__get_getWaveInfo(state);
        arg0_1 = makeLoadedComponentFromCanvasData(tupledArg[0], tupledArg[1], filePath, timeStamp, waveInfo);
        return new FSharpResult$2(0, arg0_1);
    }
    else {
        let arg0;
        const clo1 = toText(printf("Can\u0027t load component from \u0027%s\u0027"));
        arg0 = clo1(filePath);
        return new FSharpResult$2(1, arg0);
    }
}

export class LoadStatus extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Resolve", "OkComp", "OkAuto"];
    }
}

export function LoadStatus$reflection() {
    return union_type("FilesIO.LoadStatus", [], LoadStatus, () => [[["Item1", LoadedComponent$reflection()], ["Item2", LoadedComponent$reflection()]], [["Item", LoadedComponent$reflection()]], [["Item", LoadedComponent$reflection()]]]);
}

export function loadAllComponentFiles(folderPath) {
    const x = fs.readdirSync(folderPath);
    let arg20;
    arg20 = ofSeq(x);
    const clo1 = toConsole(printf("loadallComponentFiles %s %A"));
    const clo2 = clo1(folderPath);
    clo2(arg20);
    let lst;
    let list_1;
    let list;
    list = ofSeq(x);
    list_1 = filter((arg) => {
        let y;
        y = path_1.extname(arg);
        return ".dgm" === y;
    }, list);
    lst = map((fileName) => {
        let ldComp_1, autoComp_1;
        const filePath = path_1.join(folderPath, fileName);
        let ldComp;
        ldComp = tryLoadComponentFromPath(filePath);
        let autoComp;
        const filePath_2 = filePath + "auto";
        autoComp = tryLoadComponentFromPath(filePath_2);
        const matchValue = [ldComp, autoComp];
        let pattern_matching_result, autoComp_2, ldComp_2;
        const copyOfStruct = matchValue[0];
        if (copyOfStruct.tag === 0) {
            const copyOfStruct_1 = matchValue[1];
            if (copyOfStruct_1.tag === 0) {
                if (ldComp_1 = copyOfStruct.fields[0], (autoComp_1 = copyOfStruct_1.fields[0], compare(ldComp_1.TimeStamp, autoComp_1.TimeStamp) < 0)) {
                    pattern_matching_result = 0;
                    autoComp_2 = copyOfStruct_1.fields[0];
                    ldComp_2 = copyOfStruct.fields[0];
                }
                else {
                    pattern_matching_result = 1;
                }
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
                return new FSharpResult$2(0, new LoadStatus(0, ldComp_2, autoComp_2));
            }
            case 1: {
                const copyOfStruct_2 = matchValue[0];
                if (copyOfStruct_2.tag === 1) {
                    const copyOfStruct_3 = matchValue[1];
                    if (copyOfStruct_3.tag === 0) {
                        const autoComp_3 = copyOfStruct_3.fields[0];
                        return new FSharpResult$2(0, new LoadStatus(2, autoComp_3));
                    }
                    else {
                        const msg = copyOfStruct_2.fields[0];
                        return new FSharpResult$2(1, msg);
                    }
                }
                else {
                    const ldComp_3 = copyOfStruct_2.fields[0];
                    return new FSharpResult$2(0, new LoadStatus(1, ldComp_3));
                }
            }
        }
    }, list_1);
    return tryFindError(lst);
}

