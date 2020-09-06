module BusTypes
open CommonTypes

// Value set to None if the connection width could not be inferred.
type ConnectionsWidth = Map<ConnectionId, int option>

type WidthInferError = {
    Msg : string
    ConnectionsAffected : ConnectionId list // A list of connection Ids.
}
