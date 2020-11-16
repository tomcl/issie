import * as electron from "electron";
import { ofArray, contains, exists, map, ofSeq } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { jsOptions, createAtom, structuralHash } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { REDUX_DEVTOOLS, REACT_DEVELOPER_TOOLS } from "electron-devtools-installer";
import electron$002Ddevtools$002Dinstaller from "electron-devtools-installer";
import { promise } from "./.fable/Fable.Promise.2.1.0/PromiseImpl.fs.js";
import { PromiseBuilder__Delay_62FBFDE1, PromiseBuilder__Run_212F1D4B } from "./.fable/Fable.Promise.2.1.0/Promise.fs.js";
import { some } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { connectViaExtension } from "remotedev";
import * as path from "path";
import { printf, toText } from "./.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";

(() => {
    const objectArg = electron.systemPreferences;
    return (tupledArg) => {
        objectArg.setUserDefault(tupledArg[0], tupledArg[1], tupledArg[2]);
    };
})()[["NSDisabledDictationMenuItem", "boolean", "true"]];

(() => {
    const objectArg = electron.systemPreferences;
    return (tupledArg) => {
        objectArg.setUserDefault(tupledArg[0], tupledArg[1], tupledArg[2]);
    };
})()[["NSDisabledCharacterPaletteMenu", "boolean", "true"]];

export const args = (() => {
    let list;
    const source = process.argv;
    list = ofSeq(source);
    return map((s) => s.toLocaleLowerCase(), list);
})();

export function argFlagIsOn(flags) {
    const fl = map((s) => s.toLocaleLowerCase(), flags);
    return exists((flag) => contains(flag, args, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    }), fl);
}

export function hasDebugArgs() {
    return argFlagIsOn(ofArray(["--debug", "-d"]));
}

export const debug = false;

const DevTools_installDevTools = electron$002Ddevtools$002Dinstaller;

const DevTools_REACT_DEVELOPER_TOOLS = REACT_DEVELOPER_TOOLS;

const DevTools_REDUX_DEVTOOLS = REDUX_DEVTOOLS;

function DevTools_installDevTool(extensionRef) {
    let pr;
    const builder$0040 = promise;
    pr = PromiseBuilder__Run_212F1D4B(builder$0040, PromiseBuilder__Delay_62FBFDE1(builder$0040, () => (PromiseBuilder__Delay_62FBFDE1(builder$0040, () => (DevTools_installDevTools(extensionRef, false).then(((_arg1) => {
        console.log(some("Added extension"), _arg1);
        return Promise.resolve();
    })))).catch(((_arg2) => {
        console.log(some("An error occurred adding extension:"), _arg2);
        return Promise.resolve();
    })))));
    pr.then();
}

export function DevTools_installAllDevTools(win) {
    DevTools_installDevTool(DevTools_REACT_DEVELOPER_TOOLS);
    DevTools_installDevTool(DevTools_REDUX_DEVTOOLS);
    const value = win.webContents.executeJavaScript("require(\u0027devtron\u0027).install()");
    void value;
}

export function DevTools_uninstallAllDevTools(win) {
    electron.Session.defaultSession.removeExtension("React Developer Tools");
    electron.Session.defaultSession.removeExtension("Redux DevTools");
    return win.webContents.executeJavaScript("require(\u0027devtron\u0027).uninstall()");
}

export const DevTools_connectRemoteDevViaExtension = connectViaExtension;

electron.app.name = "Issie";

export const mainWindow = createAtom(void 0);

export function createMainWindow() {
    let options_1;
    options_1 = jsOptions((options) => {
        options.width = 1200;
        options.height = 800;
        options.show = false;
        options.autoHideMenuBar = false;
        options.frame = true;
        options.hasShadow = true;
        options.backgroundColor = "#505050";
        if (process.platform === "win32") {
            options.icon = path.join(__static, "icon.ico");
        }
        options.title = "ISSIE";
        options.webPreferences = jsOptions((o) => {
            o.nodeIntegration = true;
            o.enableRemoteModule = true;
        });
    });
    const window$ = new electron.BrowserWindow(options_1);
    let value;
    value = (window$.once('ready-to-show',((_arg1) => {
        if (window$.isMinimized()) {
            window$.show();
        }
        options_1.backgroundColor = "#505050";
        window$.focus();
    })));
    void value;
    DevTools_installAllDevTools(window$);
    if (debug) {
        window$.webContents.openDevTools();
    }
    let value_1;
    let arg00_1;
    const arg10 = process.env.ELECTRON_WEBPACK_WDS_PORT;
    const clo1 = toText(printf("http://localhost:%s"));
    arg00_1 = clo1(arg10);
    value_1 = window$.loadURL(arg00_1);
    void value_1;
    const value_2 = process.on("uncaughtException", (err) => {
        console.error(some(err));
    });
    void value_2;
    let value_3;
    value_3 = (window$.on('closed',((_arg2) => {
        mainWindow(void 0, true);
    })));
    void value_3;
    window$.maximize();
    mainWindow(window$, true);
}

(function () {
    const value = electron.app.on('ready',((_arg2, _arg1) => {
        createMainWindow();
    }));
    void value;
})();

(function () {
    let value;
    const objectArg = electron.app;
    value = (objectArg.on('window-all-closed',((_arg3) => {
        if (process.platform !== "darwin") {
            electron.app.quit();
        }
    })));
    void value;
})();

(function () {
    let value;
    const objectArg = electron.app;
    value = (objectArg.on('activate',((_arg5, _arg4) => {
        if (mainWindow() == null) {
            createMainWindow();
        }
    })));
    void value;
})();

(function () {
    const value = electron.ipcMain.on("exit-the-app", (_arg6) => {
        electron.app.quit();
    });
    void value;
})();

