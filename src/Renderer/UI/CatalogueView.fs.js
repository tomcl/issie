import { Union, Record } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { union_type, float64_type, record_type, tuple_type, int32_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { createAtom, int32ToString, structuralHash, equalArrays, equalsSafe, partialApply, equals, min, comparePrimitives, max } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { FSharpResult$2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { toFail, toConsole, printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { Memory, CustomComponentType, ComponentType, draw2dCanvasHeight, draw2dCanvasWidth } from "../../Common/CommonTypes.fs.js";
import { Draw2dWrapper__CreateComponent, Draw2dWrapper__GetCanvasState, Draw2dWrapper__SetScrollZoom, Draw2dWrapper__GetZoom, Draw2dWrapper__GetScrollArea } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { assertThat } from "../../Common/Helpers.fs.js";
import { head, contains, cons, max as max_1, tryItem, distinct, collect, truncate, sortByDescending, ofSeq, tryFind, ofArray, min as min_1, singleton, filter, map, empty, maxBy, minBy } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { extractComponent, extractState } from "../Interface/Extractor.fs.js";
import { forAll, allPairs, rangeNumber } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { menu, list as list_18, Item_Option, Item_li } from "../.fable/Fulma.2.9.0/Components/Menu.fs.js";
import { Prop, HTMLAttr, DOMAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { reduceApprox, AsyncTasksT, AutoSaveT, Model, Msg } from "./ModelType.fs.js";
import { isDigit } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Char.js";
import { parse } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Int32.js";
import { getMemorySetup, dialogPopupBodyMemorySetup, getInt2, dialogPopupBodyTwoInts, dialogPopupBodyOnlyInt, dialogPopup, formatLabelFromType, getInt, getText, dialogPopupBodyOnlyText, dialogPopupBodyTextAndInt } from "./PopupView.fs.js";
import { empty as empty_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import * as react from "react";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { menuLabelStyle } from "./Style.fs.js";
import { minValue } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Date.js";
import { createSingleton, tippyDom, tippyOpts } from "../Interface/JSHelpers.fs.js";
import { Common_GenericOption } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { Common_lazyView } from "../.fable/Fable.Elmish.React.3.0.1/common.fs.js";

export class Bbox extends Record {
    constructor(LTop, RBot) {
        super();
        this.LTop = LTop;
        this.RBot = RBot;
    }
}

export function Bbox$reflection() {
    return record_type("CatalogueView.Bbox", [], Bbox, () => [["LTop", tuple_type(int32_type, int32_type)], ["RBot", tuple_type(int32_type, int32_type)]]);
}

export class ScrollPos extends Record {
    constructor(SheetX, SheetY, SheetLeft, SheetTop, CanvasX, CanvasY, Zoom) {
        super();
        this.SheetX = (SheetX | 0);
        this.SheetY = (SheetY | 0);
        this.SheetLeft = (SheetLeft | 0);
        this.SheetTop = (SheetTop | 0);
        this.CanvasX = (CanvasX | 0);
        this.CanvasY = (CanvasY | 0);
        this.Zoom = Zoom;
    }
}

export function ScrollPos$reflection() {
    return record_type("CatalogueView.ScrollPos", [], ScrollPos, () => [["SheetX", int32_type], ["SheetY", int32_type], ["SheetLeft", int32_type], ["SheetTop", int32_type], ["CanvasX", int32_type], ["CanvasY", int32_type], ["Zoom", float64_type]]);
}

export function getViewableXY(sPos) {
    let tupledArg, x, y;
    const lTop = [sPos.SheetLeft, sPos.SheetTop];
    return new Bbox(lTop, (tupledArg = lTop, (x = (tupledArg[0] | 0), y = (tupledArg[1] | 0), [x + sPos.SheetX, y + sPos.SheetY])));
}

export function checkOnCanvas(sPos, box) {
    const y2 = box.RBot[1] | 0;
    const y1 = box.LTop[1] | 0;
    const x2 = box.RBot[0] | 0;
    const x1 = box.LTop[0] | 0;
    const patternInput = getViewableXY(sPos);
    const y2$0027 = patternInput.RBot[1] | 0;
    const y1$0027 = patternInput.LTop[1] | 0;
    const x2$0027 = patternInput.RBot[0] | 0;
    const x1$0027 = patternInput.LTop[0] | 0;
    if (((x1 < x1$0027) ? true : (y1 < y1$0027)) ? true : (x2 > x2$0027)) {
        return true;
    }
    else {
        const value = y2 > y2$0027;
        return !value;
    }
}

export function GetNewPos(zoomMin, zoomMax, model, box, sPos) {
    const patternInput = box.LTop;
    const y1 = patternInput[1] | 0;
    const x1 = patternInput[0] | 0;
    const patternInput_1 = box.RBot;
    const y2 = patternInput_1[1] | 0;
    const x2 = patternInput_1[0] | 0;
    const zoomIdeal = max(comparePrimitives, (x2 - x1) / sPos.SheetX, (y2 - y1) / sPos.SheetY);
    const zoom = (zoomIdeal < zoomMin) ? zoomMin : ((zoomIdeal > zoomMax) ? zoomMax : zoomIdeal);
    const scale = (pos) => (~(~(pos / sPos.Zoom)));
    let newScrollPos;
    const SheetLeft = scale(x1) | 0;
    const SheetTop = scale(y1) | 0;
    newScrollPos = (new ScrollPos(sPos.SheetX, sPos.SheetY, SheetLeft, SheetTop, sPos.CanvasX, sPos.CanvasY, zoom));
    if (checkOnCanvas(newScrollPos, box)) {
        return new FSharpResult$2(0, newScrollPos);
    }
    else {
        let arg0;
        const arg20 = getViewableXY(newScrollPos);
        const clo1 = toText(printf("Can\u0027t view %A inside the allowed box of %A"));
        const clo2 = clo1(box);
        arg0 = clo2(arg20);
        return new FSharpResult$2(1, arg0);
    }
}

export class Direction extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["TOP", "BOTTOM", "LEFT", "RIGHT", "MID"];
    }
}

export function Direction$reflection() {
    return union_type("CatalogueView.Direction", [], Direction, () => [[], [], [], [], []]);
}

export function rTop(bb) {
    const y$0027 = bb.RBot[1] | 0;
    const y = bb.LTop[1] | 0;
    const x$0027 = bb.RBot[0] | 0;
    const x = bb.LTop[0] | 0;
    return [x$0027, y];
}

export function lBot(bb) {
    const y$0027 = bb.RBot[1] | 0;
    const y = bb.LTop[1] | 0;
    const x$0027 = bb.RBot[0] | 0;
    const x = bb.LTop[0] | 0;
    return [x, y$0027];
}

export const sheetDefault = new ScrollPos(1000, 1000, 0, 0, draw2dCanvasWidth, draw2dCanvasHeight, 1);

export function scrollData(model) {
    const scrollArea = (model_1) => Draw2dWrapper__GetScrollArea(model_1.Diagram);
    const zoomOpt = (model_2) => Draw2dWrapper__GetZoom(model_2.Diagram);
    const matchValue = [scrollArea(model), zoomOpt(model)];
    let pattern_matching_result, a, z;
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            pattern_matching_result = 0;
            a = matchValue[0];
            z = matchValue[1];
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
            const mag = (n) => (n * z);
            const mag$0027 = (n_1) => min(comparePrimitives, n_1, n_1 * z);
            let SheetX;
            const value = mag$0027(a.Width);
            SheetX = (~(~value));
            let SheetY;
            const value_1 = mag$0027(a.Height);
            SheetY = (~(~value_1));
            let SheetLeft;
            const value_2 = mag(a.Left);
            SheetLeft = (~(~value_2));
            let SheetTop;
            const value_3 = mag(a.Top);
            SheetTop = (~(~value_3));
            return new ScrollPos(SheetX, SheetY, SheetLeft, SheetTop, draw2dCanvasWidth, draw2dCanvasHeight, z);
        }
        case 1: {
            return sheetDefault;
        }
    }
}

export function changeZoom(model, zoomCentre, mag, sd) {
    let clo1, z, z_2;
    let zoom$0027;
    const maxZoom = max(comparePrimitives, sd.CanvasX / sd.SheetX, sd.CanvasY / sd.SheetY);
    const minZoom = 0.2;
    assertThat((mag > 0.01) ? (mag < 100) : false, (clo1 = toText(printf("mag %A  (\u003c 0.01 or \u003e 100) is not allowed: zoom is normally in range approx 0.2 - 5")), clo1(mag)));
    const matchValue = sd.Zoom / mag;
    if (z = matchValue, z > maxZoom) {
        const z_1 = matchValue;
        const clo1_1 = toConsole(printf("Zoom maximum of %.1f reached"));
        clo1_1(maxZoom);
        zoom$0027 = maxZoom;
    }
    else if (z_2 = matchValue, z_2 < minZoom) {
        const z_3 = matchValue;
        const clo1_2 = toConsole(printf("Zoom minimum of %.1f reached"));
        clo1_2(minZoom);
        zoom$0027 = minZoom;
    }
    else {
        const z_4 = matchValue;
        zoom$0027 = z_4;
    }
    let patternInput;
    if (zoomCentre != null) {
        const y_1 = zoomCentre[1] | 0;
        const x_1 = zoomCentre[0] | 0;
        patternInput = [x_1, y_1];
    }
    else {
        patternInput = [sd.SheetLeft + (~(~(sd.SheetX / 2))), sd.SheetTop + (~(~(sd.SheetY / 2)))];
    }
    const y0 = patternInput[1] | 0;
    const x0 = patternInput[0] | 0;
    const zr = zoom$0027 / sd.Zoom;
    const left$0027 = max(comparePrimitives, 0, sd.SheetLeft + ((x0 - sd.SheetLeft) * (1 - zr)));
    const top$0027 = max(comparePrimitives, 0, sd.SheetTop + ((y0 - sd.SheetTop) * (1 - zr)));
    const sa = Draw2dWrapper__GetScrollArea(model.Diagram);
    const arg00 = (~(~(left$0027 / zr))) | 0;
    const arg10_3 = (~(~(top$0027 / zr))) | 0;
    Draw2dWrapper__SetScrollZoom(model.Diagram, arg00, arg10_3, zoom$0027);
}

export function zoomDiagram(mag, model) {
    const sd = scrollData(model);
    changeZoom(model, void 0, mag, sd);
}

export function computeBoundingBox(boxes) {
    const bbMin = [minBy((xyPos) => xyPos.LTop[0], boxes, {
        Compare: comparePrimitives,
    }).LTop[0], minBy((xyPos_1) => xyPos_1.LTop[1], boxes, {
        Compare: comparePrimitives,
    }).LTop[1]];
    const bbMax = [maxBy((xyPos_2) => xyPos_2.RBot[0], boxes, {
        Compare: comparePrimitives,
    }).RBot[0], maxBy((xyPos_3) => xyPos_3.RBot[1], boxes, {
        Compare: comparePrimitives,
    }).RBot[1]];
    return new Bbox(bbMin, bbMax);
}

export function computeVertexBBox(conn) {
    const verts = conn.Vertices;
    if (equals(verts, empty())) {
        toFail(printf("computeVertexBBox called with empty list of vertices!"));
    }
    const intFst = (arg) => {
        let value;
        value = arg[0];
        return (~(~value)) | 0;
    };
    const intSnd = (arg_1) => {
        let value_1;
        value_1 = arg_1[1];
        return (~(~value_1)) | 0;
    };
    const bbMin = [intFst(maxBy((tupledArg) => {
        const x = tupledArg[0];
        const y = tupledArg[1];
        return x;
    }, verts, {
        Compare: comparePrimitives,
    })), intSnd(minBy((tupledArg_1) => {
        const x_2 = tupledArg_1[0];
        const y_2 = tupledArg_1[1];
        return y_2;
    }, verts, {
        Compare: comparePrimitives,
    }))];
    const bbMax = [intFst(maxBy((tupledArg_2) => {
        const x_4 = tupledArg_2[0];
        const y_4 = tupledArg_2[1];
        return x_4;
    }, verts, {
        Compare: comparePrimitives,
    })), intSnd(minBy((tupledArg_3) => {
        const x_6 = tupledArg_3[0];
        const y_6 = tupledArg_3[1];
        return y_6;
    }, verts, {
        Compare: comparePrimitives,
    }))];
    return new Bbox(bbMin, bbMax);
}

export function getNewComponentPosition(model) {
    let y_15, x_18, y_13, x_16, w, h;
    const maxX = 60;
    const maxY = 60;
    const offsetY = 30;
    const meshSize1 = 21;
    const meshSize2 = 3;
    const sDat = scrollData(model);
    const bbTopLeft = new Bbox([sDat.SheetLeft, sDat.SheetTop], [sDat.SheetLeft, sDat.SheetTop]);
    const isFullyVisible = (tupledArg) => {
        const x = tupledArg[0] | 0;
        const y = tupledArg[1] | 0;
        if (((x >= (sDat.SheetLeft + (~(~(maxX / 2))))) ? (y >= (sDat.SheetTop + (~(~(maxY / 2))))) : false) ? (x < ((sDat.SheetLeft + sDat.SheetX) - maxX)) : false) {
            return y < ((sDat.SheetTop + sDat.SheetY) - maxY);
        }
        else {
            return false;
        }
    };
    const isPartlyVisible = (tupledArg_1) => {
        const x_1 = tupledArg_1[0] | 0;
        const y_1 = tupledArg_1[1] | 0;
        if (((x_1 >= (sDat.SheetLeft - maxX)) ? (y_1 >= (sDat.SheetTop - maxY)) : false) ? (x_1 < (sDat.SheetLeft + sDat.SheetX)) : false) {
            return y_1 < (sDat.SheetTop + sDat.SheetY);
        }
        else {
            return false;
        }
    };
    let patternInput_1;
    const matchValue = Draw2dWrapper__GetCanvasState(model.Diagram);
    if (matchValue != null) {
        const jsState = matchValue;
        const patternInput = extractState(jsState[0], jsState[1]);
        const conns = patternInput[1];
        const comps = patternInput[0];
        let xyPosL;
        let list_1;
        list_1 = map((co) => (new Bbox([co.X, co.Y], [co.X + co.W, co.Y + co.H])), comps);
        xyPosL = filter((co_1) => isPartlyVisible(co_1.LTop), list_1);
        patternInput_1 = (equals(xyPosL, empty()) ? [singleton(bbTopLeft), bbTopLeft, empty()] : [xyPosL, computeBoundingBox(xyPosL), comps]);
    }
    else {
        toConsole(printf("No canvas detected!"));
        patternInput_1 = [singleton(bbTopLeft), bbTopLeft, empty()];
    }
    const comps_1 = patternInput_1[2];
    const componentPositions = patternInput_1[0];
    const boundingBox = patternInput_1[1];
    let xDefault;
    let x_3;
    let tuple_1;
    let list_4;
    let list_3;
    list_3 = filter((bb) => isPartlyVisible(bb.LTop), componentPositions);
    list_4 = map((bb_1) => bb_1.LTop, list_3);
    tuple_1 = minBy((tuple) => tuple[1], list_4, {
        Compare: comparePrimitives,
    });
    x_3 = tuple_1[0];
    xDefault = min(comparePrimitives, x_3, sDat.SheetX - maxX);
    let yDefault;
    let y_5;
    let tuple_3;
    let list_7;
    let list_6;
    list_6 = filter((bb_2) => isPartlyVisible(bb_2.LTop), componentPositions);
    list_7 = map((bb_3) => bb_3.LTop, list_6);
    tuple_3 = minBy((tuple_2) => tuple_2[0], list_7, {
        Compare: comparePrimitives,
    });
    y_5 = tuple_3[1];
    yDefault = min(comparePrimitives, y_5, sDat.SheetY - maxY);
    const checkDistance = (compBb) => {
        const yRef = compBb.LTop[1] | 0;
        const xRef = compBb.LTop[0] | 0;
        const dir = (tupledArg_2) => {
            const x_7 = tupledArg_2[0] | 0;
            const y_7 = tupledArg_2[1] | 0;
            return (bb_4) => {
                let d1;
                const matchValue_1 = [x_7 < bb_4.LTop[0], x_7 <= bb_4.RBot[0]];
                d1 = (matchValue_1[0] ? (new Direction(2)) : (matchValue_1[1] ? (new Direction(4)) : (new Direction(3))));
                let d2;
                const matchValue_2 = [y_7 < bb_4.LTop[1], y_7 <= bb_4.RBot[1]];
                d2 = (matchValue_2[0] ? (new Direction(0)) : (matchValue_2[1] ? (new Direction(4)) : (new Direction(1))));
                return [d2, d1];
            };
        };
        const avg = (x_8, x$0027) => ((x_8 + x$0027) / 2);
        const euc = (tupledArg_3) => {
            const x_9 = tupledArg_3[0] | 0;
            const y_8 = tupledArg_3[1] | 0;
            return (tupledArg_4) => {
                const x$0027_1 = tupledArg_4[0] | 0;
                const y$0027 = tupledArg_4[1] | 0;
                return (tupledArg_5) => {
                    const x$0027$0027 = tupledArg_5[0] | 0;
                    const y$0027$0027 = tupledArg_5[1] | 0;
                    const patternInput_2 = [avg(x$0027_1, x$0027$0027), avg(y$0027, y$0027$0027)];
                    const yy = patternInput_2[1];
                    const xx = patternInput_2[0];
                    return Math.sqrt(Math.pow(x_9 - xx, 2) + Math.pow(y_8 - yy, 2));
                };
            };
        };
        const euclidean = (pt, bb_5) => {
            const y2 = bb_5.RBot[1] | 0;
            const y1 = bb_5.LTop[1] | 0;
            const x2 = bb_5.RBot[0] | 0;
            const x1 = bb_5.LTop[0] | 0;
            const matchValue_3 = dir(pt)(bb_5);
            let pattern_matching_result, x_10;
            if (matchValue_3[0].tag === 0) {
                if (matchValue_3[1].tag === 2) {
                    pattern_matching_result = 0;
                }
                else if (matchValue_3[1].tag === 4) {
                    pattern_matching_result = 1;
                }
                else if (matchValue_3[1].tag === 3) {
                    pattern_matching_result = 2;
                }
                else {
                    pattern_matching_result = 9;
                    x_10 = matchValue_3;
                }
            }
            else if (matchValue_3[0].tag === 1) {
                if (matchValue_3[1].tag === 2) {
                    pattern_matching_result = 3;
                }
                else if (matchValue_3[1].tag === 4) {
                    pattern_matching_result = 4;
                }
                else if (matchValue_3[1].tag === 3) {
                    pattern_matching_result = 5;
                }
                else {
                    pattern_matching_result = 9;
                    x_10 = matchValue_3;
                }
            }
            else if (matchValue_3[0].tag === 4) {
                if (matchValue_3[1].tag === 2) {
                    pattern_matching_result = 6;
                }
                else if (matchValue_3[1].tag === 4) {
                    pattern_matching_result = 7;
                }
                else if (matchValue_3[1].tag === 3) {
                    pattern_matching_result = 8;
                }
                else {
                    pattern_matching_result = 9;
                    x_10 = matchValue_3;
                }
            }
            else {
                pattern_matching_result = 9;
                x_10 = matchValue_3;
            }
            switch (pattern_matching_result) {
                case 0: {
                    return euc(pt)(bb_5.LTop)(bb_5.LTop);
                }
                case 1: {
                    return euc(pt)(bb_5.LTop)(rTop(bb_5));
                }
                case 2: {
                    return euc(pt)(rTop(bb_5))(rTop(bb_5));
                }
                case 3: {
                    return euc(pt)(lBot(bb_5))(lBot(bb_5));
                }
                case 4: {
                    return euc(pt)(lBot(bb_5))(bb_5.RBot);
                }
                case 5: {
                    return euc(pt)(bb_5.RBot)(bb_5.RBot);
                }
                case 6: {
                    return euc(pt)(bb_5.LTop)(lBot(bb_5));
                }
                case 7: {
                    return -0;
                }
                case 8: {
                    return euc(pt)(rTop(bb_5))(bb_5.RBot);
                }
                case 9: {
                    const tupledArg_6 = x_10;
                    const clo1 = toFail(printf("What? \u0027%A\u0027 Can\u0027t happen based on definition of dir!"));
                    return clo1([tupledArg_6[0], tupledArg_6[1]]);
                }
            }
        };
        const euclideanBox = (bb_6, bb1) => {
            const d = min_1(ofArray([euclidean(bb_6.RBot, bb1), euclidean(bb_6.LTop, bb1), euclidean(rTop(bb_6), bb1), euclidean(lBot(bb_6), bb1)]), {
                Compare: comparePrimitives,
            });
            if (d === 0) {
                const y2_1 = bb_6.RBot[1] | 0;
                const y1_1 = bb_6.LTop[1] | 0;
                const x2_1 = bb_6.RBot[0] | 0;
                const x1_1 = bb_6.LTop[0] | 0;
                return euc([~(~avg(x1_1, x2_1)), ~(~avg(y1_1, y2_1))])(bb1.RBot)(bb1.LTop) - (maxX + maxY);
            }
            else {
                const x_12 = d;
                return x_12;
            }
        };
        let _arg1_1;
        let list_9;
        list_9 = filter((_arg1) => {
            const y_10 = _arg1.LTop[1] | 0;
            const x_13 = _arg1.LTop[0] | 0;
            if (Math.abs(x_13 - xRef) < (3 * maxX)) {
                return Math.abs(y_10 - yRef) < (3 * maxY);
            }
            else {
                return false;
            }
        }, componentPositions);
        const mapping_3 = partialApply(1, euclideanBox, [compBb]);
        _arg1_1 = map(mapping_3, list_9);
        if (_arg1_1.tail == null) {
            return sDat.SheetX + sDat.SheetY;
        }
        else {
            const lst = _arg1_1;
            return min_1(lst, {
                Compare: comparePrimitives,
            });
        }
    };
    const xyToBb = (tupledArg_7) => {
        const x_15 = tupledArg_7[0] | 0;
        const y_12 = tupledArg_7[1] | 0;
        return new Bbox([x_15, y_12], [x_15 + maxX, y_12 + maxY]);
    };
    let lastCompPos;
    const matchValue_4 = model.LastCreatedComponent;
    if (matchValue_4 != null) {
        const cComp = matchValue_4;
        const matchValue_5 = tryFind((comp) => (comp.Id === cComp.Id), comps_1);
        if (matchValue_5 == null) {
            lastCompPos = (void 0);
        }
        else {
            const comp_1 = matchValue_5;
            lastCompPos = [comp_1.X, comp_1.Y, comp_1.H, comp_1.W];
        }
    }
    else {
        lastCompPos = (void 0);
    }
    const mesh = (num, low, high) => {
        const list_10 = ofSeq(rangeNumber(0, 1, num - 1));
        return map((i) => (low + (~(~((i * (high - low)) / num)))), list_10);
    };
    const matchValue_6 = [boundingBox.RBot, lastCompPos];
    if (equalsSafe(boundingBox, bbTopLeft)) {
        return [(sDat.SheetLeft + (~(~(sDat.SheetX / 2)))) - (~(~(maxX / 2))), sDat.SheetTop + maxY];
    }
    else {
        let pattern_matching_result_1, h_1, w_1, x_17, y_14;
        if (matchValue_6[1] != null) {
            if (y_13 = (matchValue_6[1][1] | 0), (x_16 = (matchValue_6[1][0] | 0), (w = (matchValue_6[1][3] | 0), (h = (matchValue_6[1][2] | 0), (checkDistance(new Bbox([x_16, (y_13 + h) + offsetY], [x_16 + w, (y_13 + (2 * h)) + offsetY])) > 0) ? (((y_13 + h) + offsetY) < ((sDat.SheetTop + sDat.SheetY) - maxY)) : false)))) {
                pattern_matching_result_1 = 0;
                h_1 = matchValue_6[1][2];
                w_1 = matchValue_6[1][3];
                x_17 = matchValue_6[1][0];
                y_14 = matchValue_6[1][1];
            }
            else {
                pattern_matching_result_1 = 1;
            }
        }
        else {
            pattern_matching_result_1 = 1;
        }
        switch (pattern_matching_result_1) {
            case 0: {
                return [x_17, (y_14 + h_1) + offsetY];
            }
            case 1: {
                if (y_15 = (matchValue_6[0][1] | 0), (y_15 < ((sDat.SheetY + sDat.SheetTop) - (2 * maxY))) ? (y_15 > sDat.SheetTop) : false) {
                    const y_16 = matchValue_6[0][1] | 0;
                    return [xDefault, y_16 + maxY];
                }
                else if (x_18 = (matchValue_6[0][0] | 0), (x_18 < ((sDat.SheetX + sDat.SheetLeft) - (2 * maxX))) ? (x_18 > sDat.SheetTop) : false) {
                    const x_19 = matchValue_6[0][0] | 0;
                    return [x_19 + maxX, yDefault];
                }
                else {
                    let bb_8;
                    let _arg3;
                    let list_17;
                    let list_13;
                    let list_12;
                    const list_11 = ofSeq(allPairs(mesh(meshSize1, sDat.SheetLeft + maxX, (sDat.SheetLeft + sDat.SheetX) - maxX), mesh(meshSize1, sDat.SheetTop + maxY, (sDat.SheetTop + sDat.SheetY) - maxY)));
                    list_12 = map(xyToBb, list_11);
                    list_13 = sortByDescending(checkDistance, list_12, {
                        Compare: comparePrimitives,
                    });
                    list_17 = truncate(10, list_13);
                    _arg3 = collect((_arg2) => {
                        const yEst = _arg2.LTop[1] | 0;
                        const xEst = _arg2.LTop[0] | 0;
                        const mX = ((~(~(sDat.SheetX / (3 * meshSize1)))) + 1) | 0;
                        const mY = ((~(~(sDat.SheetY / (3 * meshSize1)))) + 1) | 0;
                        let _arg2_1;
                        let list_16;
                        let list_15;
                        const list_14 = ofSeq(allPairs(mesh(meshSize2, xEst - mX, xEst + mX), mesh(meshSize2, yEst - mY, yEst + mY)));
                        list_15 = distinct(list_14, {
                            Equals: equalArrays,
                            GetHashCode: structuralHash,
                        });
                        list_16 = filter(isFullyVisible, list_15);
                        _arg2_1 = map(xyToBb, list_16);
                        if (_arg2_1.tail == null) {
                            return empty();
                        }
                        else {
                            const lst_1 = _arg2_1;
                            const bb_7 = maxBy(checkDistance, lst_1, {
                                Compare: comparePrimitives,
                            });
                            return singleton(bb_7);
                        }
                    }, list_17);
                    if (_arg3.tail == null) {
                        const pt_1 = [sDat.SheetLeft + (~(~(sDat.SheetX / 2))), sDat.SheetTop + (~(~(sDat.SheetY / 2)))];
                        bb_8 = (new Bbox(pt_1, pt_1));
                    }
                    else {
                        const lst_2 = _arg3;
                        bb_8 = maxBy(checkDistance, lst_2, {
                            Compare: comparePrimitives,
                        });
                    }
                    return bb_8.LTop;
                }
            }
        }
    }
}

export function getPortNames(cType) {
    let clo1;
    switch (cType.tag) {
        case 21: {
            return [singleton("D"), singleton("Q")];
        }
        case 20:
        case 22: {
            return [ofArray(["D", "En"]), singleton("Q")];
        }
        case 4: {
            return [empty(), singleton("Out")];
        }
        case 6:
        case 7:
        case 9:
        case 10:
        case 8:
        case 11: {
            return [ofArray(["In1", "In2"]), singleton("Out")];
        }
        case 15: {
            return [ofArray(["Cin", "A", "B"]), ofArray(["Sum", "Cout"])];
        }
        case 12: {
            return [ofArray(["Sel", "Data"]), ofArray(["0", "1", "2", "3"])];
        }
        case 5:
        case 3: {
            return [singleton("In"), singleton("Out")];
        }
        case 13: {
            return [ofArray(["0", "1", "Sel"]), singleton("Out")];
        }
        case 14: {
            return [ofArray(["In", "Sel"]), ofArray(["0", "1"])];
        }
        case 24:
        case 23: {
            return [singleton("Addr"), singleton("Dout")];
        }
        case 25: {
            return [ofArray(["Addr", "Din", "Write"]), singleton("Dout")];
        }
        case 17: {
            return [ofArray(["MSWire", "LSWire"]), singleton("Out")];
        }
        case 18: {
            const n = cType.fields[0] | 0;
            return [singleton("In"), ofArray([(clo1 = toText(printf("MS-%d-bits")), clo1(n)), "LS-bits"])];
        }
        case 2:
        case 0:
        case 1: {
            const clo1_1 = toFail(printf("What? Waveforms for %A should not have names looked up since symbol name is used for the (only) waveform"));
            return clo1_1(cType);
        }
        case 16: {
            return toFail(printf("Custom component port names not yet implemented!"));
        }
        default: {
            return [singleton("D"), singleton("Q")];
        }
    }
}

export function lookupPortName(comp, p) {
    const nameList = (p.PortType.tag === 1) ? getPortNames(comp.Type)[1] : getPortNames(comp.Type)[0];
    const matchValue_1 = p.PortNumber;
    if (matchValue_1 != null) {
        const n = matchValue_1 | 0;
        const matchValue_2 = tryItem(n, nameList);
        if (matchValue_2 != null) {
            const name = matchValue_2;
            return name;
        }
        else {
            const tupledArg = getPortNames(comp.Type);
            const clo1 = toFail(printf("What? %A has lists %A so can\u0027t lookup up %A %d port"));
            const clo2 = clo1(comp.Type);
            const clo3 = clo2([tupledArg[0], tupledArg[1]]);
            const clo4 = clo3(p.PortType);
            return clo4(n);
        }
    }
    else {
        return toFail(printf("can\u0027t lookup port on connection with no number"));
    }
}

export function lookupComponentAndPortName(conn, isTarget) {
    return toFail(printf("Not implemented yet"));
}

function menuItem(label, onClick) {
    return Item_li(ofArray([new Item_Option(0, false), new Item_Option(1, singleton(new DOMAttr(40, onClick)))]), singleton(label));
}

function createComponent(comp, label, model, dispatch) {
    let arg0;
    const patternInput = getNewComponentPosition(model);
    const y = patternInput[1] | 0;
    const x = patternInput[0] | 0;
    let matchValue;
    matchValue = Draw2dWrapper__CreateComponent(model.Diagram, comp, label, x, y);
    if (matchValue == null) {
    }
    else {
        const jsComp = matchValue;
        const value = dispatch((arg0 = extractComponent(jsComp), (new Msg(16, arg0))));
        void value;
    }
    return dispatch(new Msg(38, model.LastUsedDialogWidth));
}

export function stdLabel(compType, model) {
    let list_4, list_3, tuple, jsState;
    let prefix;
    switch (compType.tag) {
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 10:
        case 11: {
            prefix = "G";
            break;
        }
        case 13: {
            prefix = "MUX";
            break;
        }
        case 14: {
            prefix = "DM";
            break;
        }
        case 15: {
            prefix = "A";
            break;
        }
        case 19:
        case 20: {
            prefix = "FF";
            break;
        }
        case 21:
        case 22: {
            prefix = "REG";
            break;
        }
        case 23: {
            prefix = "AROM";
            break;
        }
        case 24: {
            prefix = "ROM";
            break;
        }
        case 25: {
            prefix = "RAM";
            break;
        }
        case 16: {
            const c = compType.fields[0];
            prefix = (c.Name + ".I");
            break;
        }
        case 4: {
            prefix = "C";
            break;
        }
        default: {
            prefix = "";
        }
    }
    const samePrefixPlusNum = (word) => {
        const matchValue = word.slice(0, (prefix.length - 1) + 1) === prefix;
        if (matchValue) {
            const w = word.slice(prefix.length, (word.length - 1) + 1);
            let _arg1;
            const w_1 = w;
            _arg1 = [forAll(isDigit, w_1.split("")), w_1];
            let pattern_matching_result;
            if (_arg1[1] === "") {
                pattern_matching_result = 0;
            }
            else if (_arg1[0]) {
                pattern_matching_result = 1;
            }
            else {
                pattern_matching_result = 0;
            }
            switch (pattern_matching_result) {
                case 0: {
                    return empty();
                }
                case 1: {
                    return singleton(parse(w, 511, false, 32));
                }
            }
        }
        else {
            return empty();
        }
    };
    const pickSuffixNumberNotUsed = (lst) => {
        let list_2;
        let list_1;
        let n;
        n = max_1(cons(0, lst), {
            Compare: comparePrimitives,
        });
        list_1 = ofSeq(rangeNumber(1, 1, n + 1));
        list_2 = filter((i) => {
            const value = contains(i, lst, {
                Equals: (x_1, y_1) => (x_1 === y_1),
                GetHashCode: structuralHash,
            });
            return !value;
        }, list_1);
        return head(list_2) | 0;
    };
    const matchValue_1 = Draw2dWrapper__GetCanvasState(model.Diagram);
    let pattern_matching_result_1;
    if (matchValue_1 == null) {
        if (prefix !== "") {
            pattern_matching_result_1 = 0;
        }
        else {
            pattern_matching_result_1 = 1;
        }
    }
    else {
        pattern_matching_result_1 = 1;
    }
    switch (pattern_matching_result_1) {
        case 0: {
            return prefix + "1";
        }
        case 1: {
            let pattern_matching_result_2, jsState_1;
            if (matchValue_1 != null) {
                if (jsState = matchValue_1, prefix !== "") {
                    pattern_matching_result_2 = 0;
                    jsState_1 = matchValue_1;
                }
                else {
                    pattern_matching_result_2 = 1;
                }
            }
            else {
                pattern_matching_result_2 = 1;
            }
            switch (pattern_matching_result_2) {
                case 0: {
                    let y_2;
                    const value_1 = pickSuffixNumberNotUsed((list_4 = (list_3 = (tuple = extractState(jsState_1[0], jsState_1[1]), (tuple[0])), (map((c_1) => c_1.Label, list_3))), (collect(samePrefixPlusNum, list_4)))) | 0;
                    y_2 = int32ToString(value_1);
                    return prefix + y_2;
                }
                case 1: {
                    return "";
                }
            }
        }
    }
}

export function createCompStdLabel(comp, model, dispatch) {
    return createComponent(comp, stdLabel(comp, model), model, dispatch);
}

function makeCustom(model, loadedComponent) {
    return menuItem(loadedComponent.Name, (_arg1) => {
        const custom = new ComponentType(16, new CustomComponentType(loadedComponent.Name, loadedComponent.InputLabels, loadedComponent.OutputLabels));
        const patternInput = getNewComponentPosition(model);
        const y = patternInput[1] | 0;
        const x = patternInput[0] | 0;
        let value;
        const arg10 = stdLabel(custom, model);
        value = Draw2dWrapper__CreateComponent(model.Diagram, custom, arg10, x, y);
        void value;
    });
}

function makeCustomList(model) {
    const matchValue = model.CurrentProj;
    if (matchValue != null) {
        const project = matchValue;
        let list_1;
        list_1 = filter((comp) => (comp.Name !== project.OpenFileName), project.LoadedComponents);
        return map((loadedComponent) => makeCustom(model, loadedComponent), list_1);
    }
    else {
        return empty();
    }
}

function createIOPopup(hasInt, typeStr, compType, model, dispatch) {
    let title;
    const clo1 = toText(printf("Add %s node"));
    title = clo1(typeStr);
    const beforeText = (_arg1) => {
        let s;
        const clo1_1 = toText(printf("How do you want to name your %s?"));
        s = clo1_1(typeStr);
        return s;
    };
    const placeholder = "Component name";
    const beforeInt = (_arg2) => {
        let s_2;
        const clo1_2 = toText(printf("How many bits should the %s node have?"));
        s_2 = clo1_2(typeStr);
        return s_2;
    };
    const intDefault = model.LastUsedDialogWidth | 0;
    const body = hasInt ? dialogPopupBodyTextAndInt(beforeText, placeholder, beforeInt, intDefault, dispatch) : ((dialogData) => dialogPopupBodyOnlyText(beforeText, placeholder, dispatch, dialogData));
    const buttonText = "Add";
    const buttonAction = (dialogData_1) => {
        const inputText = getText(dialogData_1);
        const inputInt = getInt(dialogData_1) | 0;
        createComponent(compType(inputInt), formatLabelFromType(compType(inputInt), inputText), model, dispatch);
        if (hasInt) {
            dispatch(new Msg(38, inputInt));
        }
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_2) => {
        if (getInt(dialogData_2) < 1) {
            return true;
        }
        else {
            return getText(dialogData_2) === "";
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function createNbitsAdderPopup(model, dispatch) {
    const title = toText(printf("Add N bits adder"));
    const beforeInt = (_arg1) => "How many bits should each operand have?";
    const intDefault = model.LastUsedDialogWidth | 0;
    const body = dialogPopupBodyOnlyInt(beforeInt, intDefault, dispatch);
    const buttonText = "Add";
    const buttonAction = (dialogData) => {
        const inputInt = getInt(dialogData) | 0;
        const clo1 = toConsole(printf("creating adder %d"));
        clo1(inputInt);
        createCompStdLabel(new ComponentType(15, inputInt), new Model(model.AsyncActivity, model.WaveSim, model.Diagram, model.IsLoading, model.WaveSimulationIsOutOfDate, model.LastSimulatedCanvasState, model.LastDetailedSavedState, model.CurrentSelected, model.LastSelectedIds, inputInt, model.SelectedComponent, model.CurrentStepSimulationStep, model.RightPaneTabVisible, model.Hilighted, model.Clipboard, model.LastCreatedComponent, model.SavedSheetIsOutOfDate, model.CurrentProj, model.PopupViewFunc, model.PopupDialogData, model.Notifications, model.TopMenuOpenState, model.DividerDragMode, model.WaveSimViewerWidth, model.SimulationInProgress, model.ConnsOfSelectedWavesAreHighlighted, model.CheckWaveformScrollPosition), dispatch);
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_1) => (getInt(dialogData_1) < 1);
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function createSplitWirePopup(model, dispatch) {
    const title = toText(printf("Add SplitWire node"));
    const beforeInt = (_arg1) => "How many bits should go to the top wire? The remaining bits will go to the bottom wire.";
    const intDefault = 1;
    const body = dialogPopupBodyOnlyInt(beforeInt, intDefault, dispatch);
    const buttonText = "Add";
    const buttonAction = (dialogData) => {
        const inputInt = getInt(dialogData) | 0;
        createCompStdLabel(new ComponentType(18, inputInt), model, dispatch);
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_1) => (getInt(dialogData_1) < 1);
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function createConstantPopup(model, dispatch) {
    const title = toText(printf("Add Constant"));
    const beforeInt2 = (_arg1) => "What is the decimal value of the constant?";
    const beforeInt = (_arg2) => "How many bits has wire carrying the constant?";
    const intDefault = 1;
    const intDefault2 = 0;
    const body = dialogPopupBodyTwoInts(beforeInt, beforeInt2, intDefault, intDefault2, dispatch);
    const buttonText = "Add";
    const buttonAction = (dialogData) => {
        const width = getInt(dialogData) | 0;
        const constant = getInt2(dialogData) | 0;
        createCompStdLabel(new ComponentType(4, width, constant), model, dispatch);
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_1) => {
        if (getInt(dialogData_1) < 1) {
            return true;
        }
        else {
            return getInt(dialogData_1) > 32;
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function createBusSelectPopup(model, dispatch) {
    const title = toText(printf("Add Bus Selection node"));
    const beforeInt2 = (_arg1) => "Which input bit is the least significant output bit?";
    const beforeInt = (_arg2) => "How many bits width is the output bus?";
    const intDefault = 1;
    const intDefault2 = 0;
    const body = dialogPopupBodyTwoInts(beforeInt, beforeInt2, intDefault, intDefault2, dispatch);
    const buttonText = "Add";
    const buttonAction = (dialogData) => {
        const width = getInt(dialogData) | 0;
        const lsb = getInt2(dialogData) | 0;
        createCompStdLabel(new ComponentType(3, width, lsb), model, dispatch);
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_1) => {
        if (getInt(dialogData_1) < 1) {
            return true;
        }
        else {
            return getInt2(dialogData_1) < 0;
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function createRegisterPopup(regType, model, dispatch) {
    const title = toText(printf("Add Register"));
    const beforeInt = (_arg1) => "How wide should the register be (in bits)?";
    const intDefault = model.LastUsedDialogWidth | 0;
    const body = dialogPopupBodyOnlyInt(beforeInt, intDefault, dispatch);
    const buttonText = "Add";
    const buttonAction = (dialogData) => {
        const inputInt = getInt(dialogData) | 0;
        createCompStdLabel(regType(inputInt), model, dispatch);
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_1) => (getInt(dialogData_1) < 1);
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function createMemoryPopup(memType, model, dispatch) {
    const title = "Create memory";
    const body = dialogPopupBodyMemorySetup(model.LastUsedDialogWidth, dispatch);
    const buttonText = "Add";
    const buttonAction = (dialogData) => {
        const patternInput = getMemorySetup(dialogData);
        const wordWidth = patternInput[1] | 0;
        const addressWidth = patternInput[0] | 0;
        const memory = new Memory(addressWidth, wordWidth, empty_1());
        createCompStdLabel(memType(memory), model, dispatch);
        dispatch(new Msg(20));
    };
    const isDisabled = (dialogData_1) => {
        const patternInput_1 = getMemorySetup(dialogData_1);
        const wordWidth_1 = patternInput_1[1] | 0;
        const addressWidth_1 = patternInput_1[0] | 0;
        if (addressWidth_1 < 1) {
            return true;
        }
        else {
            return wordWidth_1 < 1;
        }
    };
    dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch);
}

function makeMenuGroup(title, menuList) {
    const children_2 = [react.createElement("summary", keyValueList([menuLabelStyle], 1), title), list_18(empty(), menuList)];
    return react.createElement("details", {
        open: false,
    }, ...children_2);
}

export const firstTip = createAtom(true);

export const tippyNodes = createAtom(empty());

function makeMenuGroupWithTip(title, tip, menuList) {
    const addTip = (el) => {
        if ((!(el == null)) ? firstTip() : false) {
            el.setAttribute("data-tippy-content", tip);
            tippyNodes(cons(el, tippyNodes()), true);
        }
    };
    const children_2 = [react.createElement("summary", keyValueList([menuLabelStyle], 1), title), list_18(empty(), menuList)];
    return react.createElement("details", {
        open: false,
        ref: addTip,
    }, ...children_2);
}

export function compareModelsApprox(m1, m2) {
    let initActivity;
    const LastSavedCanvasState = empty_1();
    const LastAutoSaveCheck = minValue();
    initActivity = (new AsyncTasksT(new AutoSaveT(2), empty_1(), LastAutoSaveCheck, LastSavedCanvasState, false));
    const m1r = reduceApprox(m1);
    const m2r = reduceApprox(m2);
    const b = equals(m1r, m2r);
    const clo1 = toConsole(printf("Model equality:%A"));
    clo1(b);
    if (b === false) {
        const clo1_1 = toConsole(printf("\n\n%A\n\n%A\n\n"));
        const clo2 = clo1_1(m1r);
        clo2(m2r);
    }
    return b;
}

export const tippys = createAtom(void 0);

export function viewCatalogue(model, dispatch) {
    const viewCatOfModel = (model_1) => {
        const catTipInstall = (el) => {
            let list;
            if ((!(el == null)) ? firstTip() : false) {
                const props = tippyOpts("left");
                if (tippys() == null) {
                }
                else {
                    const tips = tippys()[1];
                    const single = tippys()[0];
                    single.destroy();
                    tips.forEach((tip) => {
                        tip.destroy();
                    });
                }
                const tip_1 = tippyDom((list = tippyNodes(), (Array.from(list))), props);
                const single_1 = createSingleton(tip_1, props);
                tippys([single_1, tip_1], true);
                tippyNodes(empty(), true);
                firstTip(false, true);
            }
        };
        const catTip1 = (name, func, tip_2) => {
            const children = [menuItem(name, func)];
            return react.createElement("div", {
                ref: (element) => {
                    if ((!(element == null)) ? firstTip() : false) {
                        element.setAttribute("data-tippy-content", tip_2);
                        tippyNodes(cons(element, tippyNodes()), true);
                    }
                },
            }, ...children);
        };
        return menu(singleton(new Common_GenericOption(1, ofArray([new HTMLAttr(65, "py-1"), new Prop(1, catTipInstall)]))), ofArray([makeMenuGroup("Input / Output", ofArray([catTip1("Input", (_arg1) => {
            createIOPopup(true, "input", (arg0) => (new ComponentType(0, arg0)), model_1, dispatch);
        }, "Input connection to current sheet: one or more bits"), catTip1("Output", (_arg2) => {
            createIOPopup(true, "output", (arg0_1) => (new ComponentType(1, arg0_1)), model_1, dispatch);
        }, "Output connection from current sheet: one or more bits"), catTip1("Constant", (_arg3) => {
            createConstantPopup(model_1, dispatch);
        }, "Define a one or more bit constant value, e.g. 0 or 1 to drive an unused input"), catTip1("Wire Label", (_arg5) => {
            createIOPopup(false, "label", (_arg4) => (new ComponentType(2)), model_1, dispatch);
        }, "Labels with the same name connect together wires or busses")])), makeMenuGroup("Buses", ofArray([catTip1("MergeWires", (_arg6) => {
            createComponent(new ComponentType(17), "", model_1, dispatch);
        }, "Use Mergewire when you want to join the bits of a two busses to make a wider bus"), catTip1("SplitWire", (_arg7) => {
            createSplitWirePopup(model_1, dispatch);
        }, "Use Splitwire when you want to split the bits of a bus into two sets"), catTip1("Bus Select", (_arg8) => {
            createBusSelectPopup(model_1, dispatch);
        }, "Bus Select output connects to one or \r\n                                                                                                more selected bits of its input")])), makeMenuGroup("Gates", ofArray([catTip1("Not", (_arg9) => {
            createCompStdLabel(new ComponentType(5), model_1, dispatch);
        }, "Invertor: output is negation of input"), catTip1("And", (_arg10) => {
            createCompStdLabel(new ComponentType(6), model_1, dispatch);
        }, "Output is 1 if both the two inputs are 1"), catTip1("Or", (_arg11) => {
            createCompStdLabel(new ComponentType(7), model_1, dispatch);
        }, "Output is 1 if either of the two inputs are 1"), catTip1("Xor", (_arg12) => {
            createCompStdLabel(new ComponentType(8), model_1, dispatch);
        }, "Output is 1 if the two inputs have different values"), catTip1("Nand", (_arg13) => {
            createCompStdLabel(new ComponentType(9), model_1, dispatch);
        }, "Output is 0 if both the two inputs are 1"), catTip1("Nor", (_arg14) => {
            createCompStdLabel(new ComponentType(10), model_1, dispatch);
        }, "Output is 0 if either of the two inputs are 1"), catTip1("Xnor", (_arg15) => {
            createCompStdLabel(new ComponentType(11), model_1, dispatch);
        }, "Output is 1 if the two inputs have the same values")])), makeMenuGroup("Mux / Demux", ofArray([catTip1("Mux2", (_arg16) => {
            createCompStdLabel(new ComponentType(13), model_1, dispatch);
        }, "Selects the one of its two input busses numbered by the value of the select input\r\n                                                                                to be the output. Adjusts bus width to match."), catTip1("Demux2", (_arg17) => {
            createCompStdLabel(new ComponentType(14), model_1, dispatch);
        }, "The output is equal to the input, the other is 0"), catTip1("Decode4", (_arg18) => {
            createCompStdLabel(new ComponentType(12), model_1, dispatch);
        }, "The output numbered by the binary value \r\n                                                                                                of the 2 bit sel input is equal to Data, the others are 0")])), makeMenuGroup("Arithmetic", singleton(catTip1("N bits adder", (_arg19) => {
            createNbitsAdderPopup(model_1, dispatch);
        }, "N bit Binary adder with carry in to bit 0 and carry out from bit N-1"))), makeMenuGroup("Flip Flops and Registers", ofArray([catTip1("D-flip-flop", (_arg20) => {
            createCompStdLabel(new ComponentType(19), model_1, dispatch);
        }, "D flip-flop - note that clock is assumed always connected to a global clock, so ripple counters cannot be implemented in Issie"), catTip1("D-flip-flop with enable", (_arg21) => {
            createCompStdLabel(new ComponentType(20), model_1, dispatch);
        }, "D flip-flop: output will remain unchanged when En is 0"), catTip1("Register", (_arg22) => {
            createRegisterPopup((arg0_2) => (new ComponentType(21, arg0_2)), model_1, dispatch);
        }, "N D flip-flops with inputs and outputs combined into single N bit busses"), catTip1("Register with enable", (_arg23) => {
            createRegisterPopup((arg0_3) => (new ComponentType(22, arg0_3)), model_1, dispatch);
        }, "As register but outputs stay the same if En is 0")])), makeMenuGroup("Memories", ofArray([catTip1("ROM (asynchronous)", (_arg24) => {
            createMemoryPopup((arg0_4) => (new ComponentType(23, arg0_4)), model_1, dispatch);
        }, "This is combinational: the output is available in the same clock cycle that the address is presented"), catTip1("ROM (synchronous)", (_arg25) => {
            createMemoryPopup((arg0_5) => (new ComponentType(24, arg0_5)), model_1, dispatch);
        }, "A ROM whose output contains the addressed data in the clock cycle after the address is presented"), catTip1("RAM", (_arg26) => {
            createMemoryPopup((arg0_6) => (new ComponentType(25, arg0_6)), model_1, dispatch);
        }, "A RAM whose output contains the addressed data in the clock cycle after the address is presented")])), makeMenuGroupWithTip("This project", "Every design sheet is available for use in other sheets as a custom component: it can be added any number of times, each instance replicating the sheet logic", makeCustomList(model_1))]));
    };
    return Common_lazyView(viewCatOfModel)(model);
}

