import { printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { HTMLAttr, SVGAttr, CSSProp } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { ofSeq, append, ofArray, singleton } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { singleton as singleton_1, append as append_1, delay } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";

const headerHeight = "72px";

const rightSectionWidthS = "400px";

const rightSectionWidthL = "650px";

export const minViewerWidth = 400;

export const minEditorWidth = 400;

export const rightSectionWidthViewerDefault = 650;

export function rightSectionWidth(model) {
    const matchValue = model.RightPaneTabVisible;
    switch (matchValue.tag) {
        case 1: {
            return rightSectionWidthS;
        }
        case 2: {
            return rightSectionWidthL;
        }
        case 3: {
            const clo1 = toText(printf("%dpx"));
            return clo1(model.WaveSimViewerWidth);
        }
        default: {
            return rightSectionWidthS;
        }
    }
}

export function leftSectionWidth(model) {
    let arg20, clo1, clo2;
    const css = singleton(new CSSProp(395, (arg20 = rightSectionWidth(model), (clo1 = toText(printf("calc(100%s - %s - 10px)")), clo2 = clo1("%"), clo2(arg20)))));
    return ["style", keyValueList(css, 1)];
}

export function navbarStyle(model) {
    return ["style", {
        width: "100%",
        height: headerHeight,
    }];
}

export function rightSectionStyle(model) {
    const widthRightSec = rightSectionWidth(model);
    return ["style", {
        position: "fixed",
        right: "0px",
        top: "0px",
        height: "100%",
        width: widthRightSec,
        overflowX: "hidden",
        overflowY: "scroll",
        borderTop: "2px solid lightgray",
        userSelect: "none",
        zIndex: 31,
        backgroundColor: "white",
    }];
}

export function canvasVisibleStyle(model) {
    const widthRightSec = rightSectionWidth(model);
    return ["style", {
        display: "block",
        position: "absolute",
        overflowX: "scroll",
        overflowY: "scroll",
        top: headerHeight,
        left: "0px",
        bottom: "0px",
        right: widthRightSec,
        borderTop: "2px solid lightgray",
    }];
}

export const canvasSmallMenuStyle = (() => {
    let clo1;
    const css = ofArray([new CSSProp(125, "block"), new CSSProp(291, "absolute"), new CSSProp(271, "hidden"), new CSSProp(272, "hidden"), new CSSProp(208, "10px"), new CSSProp(83, "25px"), new CSSProp(298, (clo1 = toText(printf("calc(100%s - 300px)")), clo1("%"))), new CSSProp(392, "nowrap")]);
    return ["style", keyValueList(css, 1)];
})();

export const canvasSmallButtonStyle = ["style", {
    marginRight: "5px",
    backgroundColor: "white",
    borderRadius: "4px",
    borderStyle: "solid",
    outline: "none",
    padding: "4px",
    opacity: 0.7,
}];

export const notificationStyle = ["style", {
    zIndex: 100,
    position: "absolute",
    right: "20px",
    bottom: "20px",
}];

export const simulationNumberStyle = ["style", {
    width: "320px",
    height: "30px",
}];

export const simulationBitStyle = ["style", {
    width: "100px",
    height: "30px",
    paddingTop: "3px",
}];

export const menuLabelStyle = ["style", {
    outline: "none",
    marginTop: "10px",
    marginBottom: "10px",
    color: "#7a7a7a",
    fontSize: "0.80em",
    letterSpacing: "0.1em",
    textTransform: "uppercase",
}];

export const clkLineWidth = 0.0125;

export const transLen = 0.1;

export const vPos = 0;

export const zoomFactor = 1.3;

export const maxZoom = 3;

export const minZoom = 0.2;

export const maxBusValGap = 3;

export const busLabelTextSize = 0.6;

export const sigLineThick = 0.025;

export const spacing = 0.4;

export const sigHeight = 0.3;

export function vbWidth(m) {
    return m.SimParams.ClkSvgWidth * (m.SimParams.LastClkTime + 1);
}

export function maxWavesColWidthFloat(m) {
    return (vbWidth(m) * 40) + 4;
}

export function maxWavesColWidth(m) {
    return maxWavesColWidthFloat(m).toString() + "px";
}

export function waveCellWidth(m) {
    return new CSSProp(395, maxWavesColWidth(m));
}

export function widthAndVBwave(m) {
    let css;
    return ofArray([(css = singleton(waveCellWidth(m)), ["style", keyValueList(css, 1)]), new SVGAttr(40, ("0 0 " + vbWidth(m).toString()) + " 0.7")]);
}

export function clkRulerStyle(m) {
    return append(widthAndVBwave(m), ofArray([new HTMLAttr(65, "clkRulerSvg"), new SVGAttr(23, "none")]));
}

export function cursorLeftPx(m, cursor) {
    return cursor * ((m.ClkSvgWidth * 40) + (4 / (m.LastClkTime + 1)));
}

export function cursRectStyle(m) {
    let w, value;
    const css = ofArray([new CSSProp(208, (w = (value = (cursorLeftPx(m, m.CursorTime)), (value.toString())), (w + "px"))), new CSSProp(395, 40 * (m.ClkSvgWidth - clkLineWidth))]);
    return ["style", keyValueList(css, 1)];
}

export function cursRectText(m, i) {
    return ofArray([new HTMLAttr(65, "clkNumStyle"), new SVGAttr(44, m.SimParams.ClkSvgWidth * (i + 0.5)), new SVGAttr(57, 0.5)]);
}

export function inWaveLabel(nLabels, xInd, m) {
    return ofArray([new HTMLAttr(65, "busValueStyle"), new SVGAttr(44, xInd * m.ClkSvgWidth), new SVGAttr(57, (spacing + (sigHeight * 0.7)) - (((0.3 * sigHeight) * (nLabels - 1)) / 2)), new SVGAttr(9, (busLabelTextSize * sigHeight) / nLabels)]);
}

export function waveCellSvg(m, last) {
    return append(widthAndVBwave(m), ofSeq(delay(() => append_1(last ? singleton_1(new HTMLAttr(65, "lastWaveCellSvg")) : singleton_1(new HTMLAttr(65, "waveCellSvg")), delay(() => singleton_1(new SVGAttr(23, "none")))))));
}

export function waveCell(m) {
    let css;
    return ofArray([new HTMLAttr(65, "rowHeight"), (css = singleton(waveCellWidth(m)), ["style", keyValueList(css, 1)])]);
}

export function lwaveCell(m) {
    let css;
    return ofArray([new HTMLAttr(65, "fullHeight"), (css = singleton(waveCellWidth(m)), ["style", keyValueList(css, 1)])]);
}

export const waveDiv = ["style", {
    width: "100%",
    height: "100%",
    position: "relative",
    overflowX: "scroll",
}];

export function wavesTable(m) {
    let css;
    return ofArray([new HTMLAttr(65, "wavesColTableStyle"), (css = singleton(waveCellWidth(m)), ["style", keyValueList(css, 1)])]);
}

