namespace Fable.SimpleJson

/// A type representing Javascript Object Notation
type Json = 
    | JNumber of float
    | JString of string
    | JBool of bool
    | JNull
    | JArray of Json list
    | JObject of Map<string, Json>