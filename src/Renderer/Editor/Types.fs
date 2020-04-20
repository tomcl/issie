module EditorTypes

//=============================//
// Types for library interface //
//=============================//

type JSEditor = | JSEditor of obj

//==========//
// Messages //
//==========//

// Messages that will be sent from JS code.
type JSEditorMsg =
    | InitEditor of JSEditor // Has to be dispatched only once.

type Msg =
    | JSEditorMsg of JSEditorMsg
    | GetCode
