import * as electron from "electron";
import { uncurry, comparePrimitives, jsOptions } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { checkPerformance } from "../Common/Helpers.fs.js";
import { KeyboardShortcutMsg, MenuCommand, Msg } from "./UI/ModelType.fs.js";
import { VersionString } from "./Interface/Version.fs.js";
import { viewInfoPopup } from "./UI/PopupView.fs.js";
import { setDebugLevel, debugTrace, debugLevel } from "./Interface/JSHelpers.fs.js";
import { ofList } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { singleton, empty, ofArray } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { map as map_1 } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import { Cmd_batch, Cmd_map, Cmd_none, Cmd_ofSub } from "./.fable/Fable.Elmish.3.1.0/cmd.fs.js";
import { displayView, init as init_2 } from "./UI/MainView.fs.js";
import { update as update_2 } from "./UI/Update.fs.js";
import { printf, toConsole } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { ProgramModule_runWith, ProgramModule_map, ProgramModule_withSubscription, ProgramModule_mkProgram } from "./.fable/Fable.Elmish.3.1.0/program.fs.js";
import { Program_Internal_withReactBatchedUsing } from "./.fable/Fable.Elmish.React.3.0.1/react.fs.js";
import { lazyView2With } from "./.fable/Fable.Elmish.HMR.4.0.1/common.fs.js";
import { Internal_saveState, Model$1, Msg$1, Internal_tryRestoreState } from "./.fable/Fable.Elmish.HMR.4.0.1/hmr.fs.js";
import { value as value_1 } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";

export const isMac = process.platform === "darwin";

export function exitApp() {
    electron.ipcRenderer.send("exit-the-app");
}

export const menuSeparator = (() => {
    const sep = {};
    sep.type = "separator";
    return sep;
})();

export function makeItem(label, accelerator, iAction) {
    const handlerCaster = (f) => ((delegateArg0, delegateArg1, delegateArg2) => {
        f(delegateArg0, delegateArg1, delegateArg2);
    });
    const item = {};
    item.label = label;
    if (accelerator != null) {
        const a = accelerator;
        item.accelerator = a;
    }
    item.click = handlerCaster((_arg2, _arg1, keyEvent) => {
        iAction(keyEvent);
    });
    return item;
}

export function makeRoleItem(label, accelerator, role) {
    const item = makeItem(label, accelerator, (_arg1) => {
    });
    item.role = role;
    return item;
}

export function makeCondRoleItem(cond, label, accelerator, role) {
    const item = makeItem(label, accelerator, (_arg1) => {
    });
    item.role = role;
    item.visible = cond;
    return item;
}

export function makeCondItem(cond, label, accelerator, action) {
    const item = makeItem(label, accelerator, action);
    item.visible = cond;
    return item;
}

export function makeElmItem(label, accelerator, action) {
    return jsOptions((item) => {
        item.label = label;
        item.accelerator = accelerator;
        item.click = ((_arg3, _arg2, _arg1) => {
            action();
        });
    });
}

export function makeMenu(topLevel, name, table) {
    const subMenu = {};
    subMenu.type = (topLevel ? "normal" : "submenu");
    subMenu.label = name;
    subMenu.submenu = Array.from(table);
    return subMenu;
}

export function displayPerformance(n, m) {
    checkPerformance(n, m, (label) => {
        console.time(label);
    }, (label_1) => {
        console.timeEnd(label_1);
    });
}

export function fileMenu(dispatch) {
    return makeMenu(false, "Sheet", ofArray([makeItem("New Sheet", "CmdOrCtrl+N", (ev) => {
        dispatch(new Msg(41, new MenuCommand(2), dispatch));
    }), makeItem("Save Sheet", "CmdOrCtrl+S", (ev_1) => {
        dispatch(new Msg(41, new MenuCommand(1), dispatch));
    }), makeItem("Print Sheet", "CmdOrCtrl+P", (ev_2) => {
        dispatch(new Msg(41, new MenuCommand(0), dispatch));
    }), makeItem("Exit Issie", void 0, (ev_3) => {
        exitApp();
    }), makeItem("About Issie " + VersionString, void 0, (ev_4) => {
        viewInfoPopup(dispatch);
    }), makeCondItem((debugLevel() !== 0) ? (!isMac) : false, "Restart app", void 0, (_arg1) => {
        const webContents = electron.remote.getCurrentWebContents();
        webContents.reload();
    }), makeCondItem((debugLevel() !== 0) ? (!isMac) : false, "Trace all", void 0, (_arg2) => {
        debugTrace(ofList(ofArray(["update", "view"]), {
            Compare: comparePrimitives,
        }), true);
    }), makeCondItem((debugLevel() !== 0) ? (!isMac) : false, "Trace off", void 0, (_arg3) => {
        debugTrace(ofList(empty(), {
            Compare: comparePrimitives,
        }), true);
    }), makeCondItem((debugLevel() !== 0) ? (!isMac) : false, "Run performance check", void 0, (_arg4) => {
        displayPerformance(3, 1000000);
    })]));
}

export function viewMenu(dispatch) {
    const devToolsKey = isMac ? "Alt+Command+I" : "Ctrl+Shift+I";
    return makeMenu(false, "View", ofArray([makeRoleItem("Toggle Fullscreen", "F11", "toggleFullScreen"), menuSeparator, makeRoleItem("Zoom  In", "CmdOrCtrl+Plus", "zoomIn"), makeRoleItem("Zoom  Out", "CmdOrCtrl+-", "zoomOut"), makeRoleItem("Reset Zoom", "CmdOrCtrl+0", "resetZoom"), menuSeparator, makeItem("Diagram Zoom In", "CmdOrCtrl+z", (ev) => {
        dispatch(new Msg(41, new MenuCommand(3, 1.25), dispatch));
    }), makeItem("Diagram Zoom Out", "CmdOrCtrl+y", (ev_1) => {
        dispatch(new Msg(41, new MenuCommand(3, 1 / 1.25), dispatch));
    }), menuSeparator, makeCondItem(debugLevel() !== 0, "Toggle Dev Tools", devToolsKey, (_arg1) => {
        const webContents = electron.remote.getCurrentWebContents();
        webContents.toggleDevTools();
    })]));
}

export function editMenu(dispatch) {
    const dispatch_1 = (arg) => {
        dispatch((new Msg(1, arg)));
    };
    return jsOptions((invisibleMenu) => {
        invisibleMenu.type = "submenu";
        invisibleMenu.label = "Edit";
        invisibleMenu.visible = true;
        const arg0_1 = [makeElmItem("Save Sheet", "CmdOrCtrl+S", () => {
            dispatch_1(new KeyboardShortcutMsg(0));
        }), makeElmItem("Copy", "Alt+C", () => {
            dispatch_1(new KeyboardShortcutMsg(1));
        }), makeElmItem("Paste", "Alt+V", () => {
            dispatch_1(new KeyboardShortcutMsg(2));
        }), makeElmItem("Delete", isMac ? "Backspace" : "delete", () => {
            dispatch_1(new KeyboardShortcutMsg(5));
        }), makeElmItem("Undo", "Alt+Z", () => {
            dispatch_1(new KeyboardShortcutMsg(3));
        }), makeElmItem("Redo", "Alt+Shift+Z", () => {
            dispatch_1(new KeyboardShortcutMsg(4));
        })];
        invisibleMenu.submenu = arg0_1;
    });
}

export function attachMenusAndKeyShortcuts(dispatch) {
    const sub = (dispatch_1) => {
        let menu;
        let arg00;
        const array = [fileMenu(dispatch_1), editMenu(dispatch_1), viewMenu(dispatch_1)];
        arg00 = map_1((arg0) => arg0, array);
        const objectArg = electron.remote.Menu;
        menu = objectArg.buildFromTemplate(arg00);
        menu.items[0].visible = true;
        electron.remote.app.applicationMenu = menu;
    };
    return Cmd_ofSub(sub);
}

export function init() {
    setDebugLevel();
    return [init_2(), Cmd_none()];
}

export function view(model, dispatch) {
    return displayView(model, dispatch);
}

export function update(msg, model) {
    return update_2(msg, model);
}

toConsole(printf("Starting renderer..."));

(function () {
    let program_3;
    let program_2;
    const program = ProgramModule_mkProgram(init, update, view);
    program_2 = Program_Internal_withReactBatchedUsing(lazyView2With, "app", program);
    program_3 = ProgramModule_withSubscription(attachMenusAndKeyShortcuts, program_2);
    let hmrState = null;
    const hot = module.hot;
    if (!(hot == null)) {
        window.Elmish_HMR_Count = ((window.Elmish_HMR_Count == null) ? 0 : (window.Elmish_HMR_Count + 1));
        const value = hot.accept();
        void undefined;
        const matchValue = Internal_tryRestoreState(hot);
        if (matchValue == null) {
        }
        else {
            const previousState = value_1(matchValue);
            hmrState = previousState;
        }
    }
    const map = (tupledArg) => {
        const model_2 = tupledArg[0];
        const cmd = tupledArg[1];
        return [model_2, (Cmd_map((arg0) => (new Msg$1(0, arg0)), cmd))];
    };
    const mapUpdate = (update_1, msg_1, model_3) => {
        let msg_2, userModel, patternInput, newModel, cmd_2;
        const patternInput_1 = map((msg_1.tag === 1) ? [new Model$1(0), Cmd_none()] : (msg_2 = msg_1.fields[0], (model_3.tag === 1) ? (userModel = model_3.fields[0], (patternInput = update_1(msg_2, userModel), (newModel = patternInput[0], (cmd_2 = patternInput[1], [new Model$1(1, newModel), cmd_2])))) : [model_3, Cmd_none()]));
        const newModel_1 = patternInput_1[0];
        const cmd_3 = patternInput_1[1];
        hmrState = newModel_1;
        return [newModel_1, cmd_3];
    };
    const createModel = (tupledArg_1) => {
        const model_4 = tupledArg_1[0];
        const cmd_4 = tupledArg_1[1];
        return [new Model$1(1, model_4), cmd_4];
    };
    const mapInit = (init_1) => {
        if (hmrState == null) {
            return (arg_2) => createModel((map(init_1(arg_2))));
        }
        else {
            return (_arg1) => [hmrState, Cmd_none()];
        }
    };
    const mapSetState = (setState, model_5, dispatch_3) => {
        if (model_5.tag === 1) {
            const userModel_1 = model_5.fields[0];
            setState(userModel_1, (arg_3) => dispatch_3((new Msg$1(0, arg_3))));
        }
    };
    let hmrSubscription;
    const handler = (dispatch_4) => {
        if (!(hot == null)) {
            hot.dispose((data) => {
                Internal_saveState(data, hmrState);
                return dispatch_4(new Msg$1(1));
            });
        }
    };
    hmrSubscription = singleton(handler);
    const mapSubscribe = (subscribe_1, model_6) => {
        let cmd_5;
        if (model_6.tag === 1) {
            const userModel_2 = model_6.fields[0];
            return Cmd_batch(ofArray([(cmd_5 = subscribe_1(userModel_2), (Cmd_map((arg0_2) => (new Msg$1(0, arg0_2)), cmd_5))), hmrSubscription]));
        }
        else {
            return Cmd_none();
        }
    };
    const mapView = (view_2, model_7, dispatch_5) => {
        if (model_7.tag === 1) {
            const userModel_3 = model_7.fields[0];
            return view_2(userModel_3, (arg_4) => dispatch_5((new Msg$1(0, arg_4))));
        }
        else {
            throw (new Error("\nYour are using HMR and this Elmish application has been marked as inactive.\n\nYou should not see this message\n                    "));
        }
    };
    let program_7;
    program_7 = ProgramModule_map(uncurry(2, mapInit), mapUpdate, mapView, mapSetState, mapSubscribe, program_3);
    ProgramModule_runWith(void 0, program_7);
})();

