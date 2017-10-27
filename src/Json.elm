module Json exposing (Value(..))


type Value
    = JsonString String
    | JsonInt Int
    | JsonFloat Float
    | JsonNull
    | JsonArray (List Value)
    | JsonObject (List ( String, Value ))
