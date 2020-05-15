/**
 * Collection of functions that can be used from the JS code to trigger
 * events in the MVU loop.
 * These functions get initialised by the draw2dWrapper.
 */

let dispatchInferWidthsMessage = "undefined";

function setDispatchMessages(dispatchInferWidthsMessage_) {
    dispatchInferWidthsMessage = dispatchInferWidthsMessage_;
}
