/**
 * Collection of functions that can be used from the JS code to trigger
 * events in the MVU loop.
 * These functions get initialised by the draw2dWrapper.
 */

let dispatchInferWidthsMessage         = "undefined";
let dispatchOnSelectComponentMessage   = "undefined";
let dispatchOnUnselectComponentMessage = "undefined";

function setDispatchMessages(
        dispatchInferWidthsMessage_,
        dispatchOnSelectComponentMessage_,
        dispatchOnUnselectComponentMessage_,
    ) {
    dispatchInferWidthsMessage         = dispatchInferWidthsMessage_;
    dispatchOnSelectComponentMessage   = dispatchOnSelectComponentMessage_;
    dispatchOnUnselectComponentMessage = dispatchOnUnselectComponentMessage_;
}
