module Json exposing (Value(..), fromCore, toCore)

import Json.Decode as CoreDecode
import Json.Encode as Core


type Value
    = String String
    | Int Int
    | Float Float
    | Null
    | Array (List Value)
    | Object (List ( String, Value ))


toCore : Value -> Core.Value
toCore value =
    case value of
        String val ->
            Core.string val

        Int val ->
            Core.int val

        Float val ->
            Core.float val

        Null ->
            Core.null

        Array values ->
            List.map toCore values |> Core.list

        Object kvPairs ->
            List.map (Tuple.mapSecond toCore) kvPairs |> Core.object


fromCore : Core.Value -> Value
fromCore value =
    CoreDecode.decodeValue decoder value
        |> Result.withDefault Null


decoder : CoreDecode.Decoder Value
decoder =
    CoreDecode.oneOf
        [ CoreDecode.map String CoreDecode.string
        , CoreDecode.map Int CoreDecode.int
        , CoreDecode.map Float CoreDecode.float
        , CoreDecode.null Null
        , CoreDecode.map Array (CoreDecode.list <| CoreDecode.lazy <| \_ -> decoder)
        , CoreDecode.map (List.reverse >> Object) (CoreDecode.keyValuePairs <| CoreDecode.lazy <| \_ -> decoder)
        ]
