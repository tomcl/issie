module BusTypes

// TODO: share with simulatorTypes?
type ComponentId      = | ComponentId of string
type ConnectionId     = | ConnectionId of string
type InputPortId      = | InputPortId of string
type OutputPortId     = | OutputPortId of string
type InputPortNumber  = | InputPortNumber of int
type OutputPortNumber = | OutputPortNumber of int

// Value set to None if the connection width could not be inferred.
type ConnectionsWidth = Map<ConnectionId, int option>

type WidthInferError = {
    Msg : string
    ConnectionsAffected : ConnectionId list // A list of connection Ids.
}
