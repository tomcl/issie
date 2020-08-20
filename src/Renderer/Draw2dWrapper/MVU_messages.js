/**
 * Collection of functions that can be used from the JS code to trigger
 * events in the MVU loop.
 * These functions get initialised by the draw2dWrapper.
 */

global.dispatchInferWidthsMessage         = "undefined";
global.dispatchOnSelectComponentMessage   = "undefined";
global.dispatchOnSelectConnectionMessage  = "undefined";
global.dispatchOnUnselectComponentMessage = "undefined";
global.dispatchHasUnsavedChangesMessage   = "undefined";

function setDispatchMessages(
        dispatchInferWidthsMessage_,
        dispatchOnSelectComponentMessage_,
        dispatchOnSelectConnectionMessage_,
        dispatchOnUnselectComponentMessage_,
        dispatchHasUnsavedChangesMessage_,
    ) {
    dispatchInferWidthsMessage         = dispatchInferWidthsMessage_;
    dispatchOnSelectComponentMessage   = dispatchOnSelectComponentMessage_;
    dispatchOnSelectConnectionMessage  = dispatchOnSelectConnectionMessage_;
    dispatchOnUnselectComponentMessage = dispatchOnUnselectComponentMessage_;
    dispatchHasUnsavedChangesMessage   = dispatchHasUnsavedChangesMessage_;
}

module.exports = { setDispatchMessages };
