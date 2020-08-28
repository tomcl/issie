/**
 * Draw2d functions that are used in the Draw2dWrapper.fs file.
 */

import { setDispatchMessages } from "./MVU_messages.js"
import { createDigitalConnection } from "./draw2d_digital_connections.js"
import "./draw2d_digital_components.js"
import "./drag_connection_create_policy_fixed.js"

function printCanvas(canvas,funcPrint) {
    let writer = new draw2d.io.png.Writer();
    writer.marshal(canvas, funcPrint);
}

function createCanvas(id, width, height) {
    let canvas = new draw2d.Canvas(id, width, height);
    canvas.setScrollArea('#'+id);
    // Make sure the canvas does not overflow the parent div.
    let style = document.getElementById(id).style;
    style.height = 'auto'; style.width = 'auto';
    return canvas;
}

function initialiseCanvas (canvas) {
    // Show canvas grid.
    canvas.installEditPolicy(new draw2d.policy.canvas.ShowGridEditPolicy());
    // Fadeout all decorations like ports, resize handles if the user didn't move
    // the mouse.
    canvas.installEditPolicy(new draw2d.policy.canvas.FadeoutDecorationPolicy());
    // Install a Connection create policy which matches to a 'circuit like'
    // connections.
    canvas.installEditPolicy( new draw2d.policy.connection.ComposedConnectionCreatePolicy(
        [
            // Create a connection via Drag&Drop of ports.
            new draw2d.policy.connection.DragConnectionCreatePolicy({
                createConnection: createDigitalConnection
            }),
            // Or via click and point.
            // TODO: if you want to readd this, it is necessary to override the
            // class similarly to DragConnectionPolicy.
            //new draw2d.policy.connection.OrthogonalConnectionCreatePolicy({
            //    createConnection: createDigitalConnection
            //})
        ])
    );
    // Install policy to allow to zoom with: SHIFT + mouse wheel.
    canvas.installEditPolicy(new draw2d.policy.canvas.WheelZoomPolicy());
    // Make ports visible only when mouse is near them.
    canvas.installEditPolicy(new draw2d.policy.canvas.CoronaDecorationPolicy({diameterToBeVisible: 70}));
    // Show guides to align components better.
    canvas.installEditPolicy(new draw2d.policy.canvas.SnapToGeometryEditPolicy());
    canvas.installEditPolicy(new draw2d.policy.canvas.SnapToCenterEditPolicy());
    canvas.installEditPolicy(new draw2d.policy.canvas.SnapToInBetweenEditPolicy());
}

function clearCanvas (canvas) {
    canvas.setScrollLeft(0);
    canvas.setScrollTop(0);
    canvas.setZoom(1.0);
    canvas.clear();
}

function getScrollArea(canvas) {
    return [ canvas.getWidth(), canvas.getHeight(), canvas.getScrollLeft(), canvas.getScrollTop()];
}

function getZoom(canvas) {
    return canvas.getZoom();
}

function setScrollZoom(canvas, scrollLeft, scrollTop, zoom) {
    canvas.setZoom(zoom);
    canvas.setScrollLeft(scrollLeft);
    canvas.setScrollTop(scrollTop);
}
    

function addComponentToCanvas(canvas, comp) {
    // Keep track of the action so it can be undone.
    let command = new draw2d.command.CommandAdd(canvas, comp, comp.getPosition());
    canvas.getCommandStack().execute(command);
}

function addConnectionToCanvas(canvas, conn) {
    // Keep track of the action so it can be undone.
    let command = new draw2d.command.CommandAdd(canvas, conn, conn.getPosition());
    canvas.getCommandStack().execute(command);
}

function addComponentLabel(comp, label) {
    comp.add(
        new draw2d.shape.basic.Label({text: label, stroke: 0}),
        new draw2d.layout.locator.TopLocator()
    );
}

function setComponentLabel(comp, newLabel) {
    comp.children.data[0].figure.setText(newLabel);
    comp.setSVG(comp.getSVG()); // Refresh svg.
}

function setConnectionLabel(conn, newLabel) {
    conn.children.data[0].figure.setText(newLabel);
}

function setComponentId(comp, id) {
    comp.setId(id);
}

function setConnectionId(conn, id) {
    conn.setId(id);
}

function setPortId(port, id) {
    port.setId(id);
}

function setComponentBackground(comp, color) {
    comp.setBackgroundColor(color);
}

function setConnectionColor(conn, color) {
    conn.setColor(color);
}

function setConnectionStroke(conn, w) {
    conn.setStroke(w);
}

function getConnectionVertices(conn) {
    return conn.getVertices();
}

function setConnectionVertices(conn, vertices) {
    conn.setVertices(vertices);
}

function getInputPorts(comp) {
    return comp.getInputPorts().data;
}

function getOutputPorts(comp) {
    return comp.getOutputPorts().data;
}

function installSelectionPolicy(comp) {
    comp.installEditPolicy(new draw2d.policy.figure.AntSelectionFeedbackPolicy({
        onSelect: function(canvas, comp, isPrimarySelection) {
            comp.setBackgroundColor('#00D1B2');
            if (dispatchOnSelectComponentMessage !== "undefined") {
                dispatchOnSelectComponentMessage(comp);
            } else {
                console.log("Warning: trying to dispatch a JS SelectComponent message but dispatcher is not defined.")
            }
        },
        onUnselect: function(canvas, comp) {
            comp.setBackgroundColor('lightgray');
            if (dispatchOnUnselectComponentMessage !== "undefined") {
                dispatchOnUnselectComponentMessage(comp);
            } else {
                console.log("Warning: trying to dispatch a JS UnselectComponent message but dispatcher is not defined.")
            }
        }
    }));
}

function createDigitalInput(x, y, numberOfBits) {
    return new draw2d.shape.digital.Input({ x: x, y: y, numberOfBits: numberOfBits, resizeable: false });
}

function createDigitalConstant(x, y, numberOfBits, constValue) {
    return new draw2d.shape.digital.Constant({ x: x, y: y, numberOfBits: numberOfBits, constValue: constValue, resizeable: false });
}

function createDigitalOutput(x, y, numberOfBits) {
    return new draw2d.shape.digital.Output({x:x,y:y,numberOfBits:numberOfBits,resizeable:false});
}

function createDigitalLabel(x, y) {
    return new draw2d.shape.digital.Label({ x: x, y: y,  resizeable: false });
}

function createDigitalBusSelection(x, y, numberOfBits, lsbBitNumber) {
    return new draw2d.shape.digital.BusSelection({ x: x, y: y, numberOfBits: numberOfBits, lsbBitNumber: lsbBitNumber, resizeable: false });
}


function createDigitalNot(x, y) {
    return new draw2d.shape.digital.Not({x:x,y:y,resizeable:false});
}

function createDigitalAnd(x, y) {
    return new draw2d.shape.digital.And({x:x,y:y,resizeable:false});
}

function createDigitalOr(x, y) {
    return new draw2d.shape.digital.Or({x:x,y:y,resizeable:false});
}

function createDigitalXor(x, y) {
    return new draw2d.shape.digital.Xor({x:x,y:y,resizeable:false});
}

function createDigitalNand(x, y) {
    return new draw2d.shape.digital.Nand({x:x,y:y,resizeable:false});
}

function createDigitalNor(x, y) {
    return new draw2d.shape.digital.Nor({x:x,y:y,resizeable:false});
}

function createDigitalXnor(x, y) {
    return new draw2d.shape.digital.Xnor({x:x,y:y,resizeable:false});
}

function createDigitalMux2(x, y) {
    return new draw2d.shape.digital.Mux2({x:x,y:y,resizeable:false});
}

function createDigitalDemux2(x, y) {
    return new draw2d.shape.digital.Demux2({x:x,y:y,resizeable:false});
}

function createDigitalNbitsAdder(x, y, numberOfBits) {
    return new draw2d.shape.digital.NbitsAdder({
        x:x,
        y:y,
        resizeable:false,
        numberOfBits:numberOfBits
    });
}

function createDigitalDecode4(x, y) {
    return new draw2d.shape.digital.Decode4({
        x: x,
        y: y,
        resizeable: false,
    });
}

function createDigitalCustom(x, y, name, inputs, outputs) {
    return new draw2d.shape.digital.Custom({
        x: x,
        y: y,
        resizeable: false,
        name: name,
        inputs: inputs,
        outputs: outputs,
    });
}

function createDigitalMergeWires(x, y) {
    return new draw2d.shape.digital.MergeWires({x:x,y:y,resizeable:false});
}

function createDigitalSplitWire(x, y, topOutputWidth) {
    return new draw2d.shape.digital.SplitWire({
        x:x,y:y,topOutputWidth:topOutputWidth,resizeable:false});
}

function createDigitalDFF(x, y) {
    return new draw2d.shape.digital.DFF({x:x,y:y,resizeable:false});
}

function createDigitalDFFE(x, y) {
    return new draw2d.shape.digital.DFFE({x:x,y:y,resizeable:false});
}

function createDigitalRegister(x, y, regWidth) {
    return new draw2d.shape.digital.Register({
        x: x,
        y: y,
        regWidth: regWidth,
        resizeable: false
    });
}

function createDigitalRegisterE(x, y, regWidth) {
    return new draw2d.shape.digital.RegisterE({
        x: x,
        y: y,
        regWidth: regWidth,
        resizeable: false
    });
}

function createDigitalAsyncROM(x, y, addressWidth, wordWidth, memData) {
    return new draw2d.shape.digital.AsyncROM({
        x: x,
        y: y,
        resizeable: false,
        addressWidth: addressWidth,
        wordWidth: wordWidth,
        memData: memData
    });
}

function createDigitalROM(x, y, addressWidth, wordWidth, memData) {
    return new draw2d.shape.digital.ROM({
        x: x,
        y: y,
        resizeable: false,
        addressWidth: addressWidth,
        wordWidth: wordWidth,
        memData: memData
    });
}

function createDigitalRAM(x, y, addressWidth, wordWidth, memData) {
    return new draw2d.shape.digital.RAM({
        x: x,
        y: y,
        resizeable: false,
        addressWidth: addressWidth,
        wordWidth: wordWidth,
        memData: memData
    });
}

function writeMemoryLine(comp, addr, value) {
    if (comp.memData === null || comp.memData === "undefined") {
        throw `Cannot write memory line of component that does not have a memory: ${comp.componentType}`;
    }
    if (addr >= comp.memData.length) {
        throw `Address out of bound while writing memory line for ${comp.componentType}: ${addr} >= ${comp.memData.length}`
    }
    comp.memData[addr] = value;
}

/// Should only be used for Input, Output and NbitsAdder components.
function setNumberOfBits(comp, numberOfBits) {
    if (comp.numberOfBits === null || comp.numberOfBits === "undefined") {
        throw `Cannot set number of bits of component: ${comp.componentType}`;
    }
    comp.numberOfBits = numberOfBits;
    dispatchInferWidthsMessage();
    comp.setSVG(comp.getSVG()); // Refresh svg.
}

/// Should only be used for Constant components.
function setConstantNumber(comp, cVal) {
    if (comp.constValue === null || comp.constValue === "undefined") {
        throw `Cannot set constValue of component: ${comp.componentType}`;
    }
    comp.constValue = cVal;
    //comp.setSvgWidth();
    dispatchInferWidthsMessage();
    comp.setSVG(comp.getSVG()); // Refresh svg.
}


/// Should only be used for Input, Output and NbitsAdder components.
function setLsbBitNumber(comp, lsbBitNumber) {
    if (comp.lsbBitNumber === null || comp.lsbBitNumber === "undefined") {
        throw `Cannot set number of bits of component: ${comp.componentType}`;
    }
    comp.lsbBitNumber = lsbBitNumber;
    dispatchInferWidthsMessage();
    comp.setSVG(comp.getSVG()); // Refresh svg.
}

/// Should only be used for SplitWire nodes.
function setTopOutputWidth(comp, topOutputWidth) {
    if (comp.topOutputWidth === null || comp.topOutputWidth === "undefined") {
        throw `Cannot set topOutputWidth of non-SplitWire component: ${comp.componentType}`;
    }
    comp.topOutputWidth = topOutputWidth;
    dispatchInferWidthsMessage();
}

/// Should only be used for Register nodes.
function setRegisterWidth(comp, regWidth) {
    if (comp.regWidth === null || comp.regWidth === "undefined") {
        throw `Cannot set regWidth of non-Register component: ${comp.componentType}`;
    }
    comp.regWidth = regWidth;
    dispatchInferWidthsMessage();
    comp.setSVG(comp.getSVG()); // Refresh svg.
}

function updateMergeWiresLabels(comp, topInputWidth, bottomInputWidth, outputWidth) {
    comp.topInputWidth = topInputWidth;
    comp.bottomInputWidth = bottomInputWidth;
    comp.outputWidth = outputWidth;
    comp.setSVG(comp.getSVG()); // Refresh svg.
}

function updateSplitWireLabels(comp, inputWidth, topOutputWidth, bottomOutputWidth) {
    comp.inputWidth = inputWidth;
    comp.topOutputWidth = topOutputWidth;
    comp.bottomOutputWidth = bottomOutputWidth;
    comp.setSVG(comp.getSVG()); // Refresh svg.
}

function getComponentById(canvas, id) {
    return canvas.getFigures().find(function(comp) {
        return comp.id === id;
    });
}

function getConnectionById(canvas, id) {
    return canvas.getLines().find(function(conn) {
        return conn.id === id;
    });
}

function refreshComponentById(canvas, id) {
    let comp = getComponentById(canvas, id);
    comp.setSVG(comp.getSVG()); // refresh SVG
}

function getPortById(comp, id) {
    return comp.getPorts().find(function(port) {
        return port.id === id;
    });
}

function getAllJsComponents(canvas) {
    return canvas.getFigures().data;
}

function getAllJsConnections(canvas) {
    return canvas.getLines().data;
}

function getSelectedJsComponents(canvas) {
    let components = [];
    canvas.getSelection().each( (i, figure) => {
        if (figure instanceof draw2d.shape.digital) {
            components.push(figure);
        }
    });
    return components;
}

function getSelectedJsConnections(canvas) {
    let connections = [];
    canvas.getSelection().each( (i, figure) => {
        if (figure instanceof draw2d.Connection) {
            connections.push(figure);
        }
    });
    return connections;
}

function undoLastAction(canvas) {
    canvas.getCommandStack().undo();
}

function redoLastAction(canvas) {
    canvas.getCommandStack().redo();
}

function flushCommandStack (canvas) {
    canvas.commandStack = new draw2d.command.CommandStack();
}

function resetSelection(canvas) {
    let lst = new draw2d.util.ArrayList;
    canvas.setCurrentSelection(lst);
}

function addCompSelection(canvas, comp) {
    canvas.addSelection(comp);
}

function addConnSelection(canvas, conn) {
    canvas.addSelection(conn);
}

export {
    setDispatchMessages,
    printCanvas,
    createCanvas,
    initialiseCanvas,
    clearCanvas,
    getScrollArea,
    getZoom,
    setScrollZoom,
    addComponentToCanvas,
    addConnectionToCanvas,
    addComponentLabel,
    setComponentLabel,
    setConnectionLabel,
    setComponentId,
    setConnectionId,
    setPortId,
    setComponentBackground,
    setConnectionColor,
    setConnectionStroke,
    getConnectionVertices,
    setConnectionVertices,
    getInputPorts,
    getOutputPorts,
    installSelectionPolicy,
    createDigitalInput,
    createDigitalOutput,
    createDigitalLabel,
    createDigitalBusSelection,
    createDigitalConstant,
    createDigitalNot,
    createDigitalAnd,
    createDigitalOr,
    createDigitalXor,
    createDigitalNand,
    createDigitalNor,
    createDigitalXnor,
    createDigitalMux2,
    createDigitalDemux2,
    createDigitalDecode4,
    createDigitalNbitsAdder,
    createDigitalCustom,
    createDigitalMergeWires,
    createDigitalSplitWire,
    createDigitalDFF,
    createDigitalDFFE,
    createDigitalRegister,
    createDigitalRegisterE,
    createDigitalAsyncROM,
    createDigitalROM,
    createDigitalRAM,
    createDigitalConnection,
    writeMemoryLine,
    setNumberOfBits,
    setConstantNumber,
    setLsbBitNumber,
    setTopOutputWidth,
    setRegisterWidth,
    updateMergeWiresLabels,
    updateSplitWireLabels,
    getComponentById,
    getConnectionById,
    getPortById,
    getAllJsComponents,
    getAllJsConnections,
    getSelectedJsComponents,
    getSelectedJsConnections,
    undoLastAction,
    redoLastAction,
    flushCommandStack,
    resetSelection,
    addCompSelection,
    addConnSelection,
};
