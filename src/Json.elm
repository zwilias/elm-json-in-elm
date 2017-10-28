module Json exposing (Value(..), fromCore, toCore)

import Json.Decode as CoreDecode
import Json.Encode as Core


type Value
    = JsonString String
    | JsonInt Int
    | JsonFloat Float
    | JsonNull
    | JsonArray (List Value)
    | JsonObject (List ( String, Value ))


toCore : Value -> Core.Value
toCore value =
    case value of
        JsonString val ->
            Core.string val

        JsonInt val ->
            Core.int val

        JsonFloat val ->
            Core.float val

        JsonNull ->
            Core.null

        JsonArray values ->
            List.map toCore values |> Core.list

        JsonObject kvPairs ->
            List.map (Tuple.mapSecond toCore) kvPairs |> Core.object


fromCore : Core.Value -> Value
fromCore value =
    CoreDecode.decodeValue decoder value
        |> Result.withDefault JsonNull


decoder : CoreDecode.Decoder Value
decoder =
    CoreDecode.oneOf
        [ CoreDecode.map JsonString CoreDecode.string
        , CoreDecode.map JsonInt CoreDecode.int
        , CoreDecode.map JsonFloat CoreDecode.float
        , CoreDecode.null JsonNull
        , CoreDecode.map JsonArray (CoreDecode.list <| CoreDecode.lazy <| \_ -> decoder)
        , CoreDecode.map JsonObject (CoreDecode.keyValuePairs <| CoreDecode.lazy <| \_ -> decoder)
        ]
