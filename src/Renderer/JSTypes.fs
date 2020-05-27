module JSTypes

//=========================================//
// Types for interface with draw2d library //
//=========================================//

type JSCanvas      = | JSCanvas of obj
type JSComponent   = | JSComponent of obj
type JSComponents  = | JSComponents of obj // JS list of JSComponent.
type JSConnection  = | JSConnection of obj
type JSConnections = | JSConnections of obj // JS list of JSConnection.
type JSPort        = | JSPort of obj
type JSPorts       = | JSPorts of obj // JS list of JSPort.
type JSVertices    = | JSVertices of obj // Js list of x,y objects.

type JSCanvasState = JSComponent list * JSConnection list
