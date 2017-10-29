module Json.Encoder exposing (encode, float, int, list, null, object, string)

import Dict
import Json exposing (Value)
import Set exposing (Set)


string : String -> Value
string =
    Json.String


int : Int -> Value
int =
    Json.Int


float : Float -> Value
float v =
    if (round >> toFloat) v == v then
        Json.Int (round v)
    else
        Json.Float v


list : List Value -> Value
list =
    Json.Array


null : Value
null =
    Json.Null


type Either a b
    = Left a
    | Right b


classifyKey : String -> Either String Int
classifyKey key =
    String.toInt key
        |> Result.andThen
            (\intKey ->
                if intKey >= 0 && toString intKey == key then
                    Ok <| Right intKey
                else
                    Err "not a valid intKey"
            )
        |> Result.withDefault (Left key)


object : List ( String, Value ) -> Value
object =
    List.foldr
        (\( key, value ) ( keys, stringKeyAcc, intKeyDict ) ->
            if Set.member key keys then
                ( keys, stringKeyAcc, intKeyDict )
            else
                case classifyKey key of
                    Left stringKey ->
                        ( Set.insert stringKey keys, ( stringKey, value ) :: stringKeyAcc, intKeyDict )

                    Right intKey ->
                        ( keys, stringKeyAcc, Dict.update key (Maybe.withDefault value >> Just) intKeyDict )
        )
        ( Set.empty, [], Dict.empty )
        >> (\( _, vals, intKeyDict ) -> Dict.toList intKeyDict ++ vals)
        >> Json.Object


encode : Value -> String
encode value =
    case value of
        Json.String string ->
            "\"" ++ escape string ++ "\""

        Json.Int int ->
            toString int

        Json.Float float ->
            toString float

        Json.Null ->
            "null"

        Json.Array valueList ->
            "[" ++ (List.map encode valueList |> String.join ",") ++ "]"

        Json.Object valueStringList ->
            "{" ++ (List.map encodePair valueStringList |> String.join ",") ++ "}"


encodePair : ( String, Value ) -> String
encodePair ( string, value ) =
    "\"" ++ escape string ++ "\":" ++ encode value


escape : String -> String
escape =
    String.toList >> List.map escapeChar >> String.join ""


escapeChar : Char -> String
escapeChar char =
    case char of
        '\\' ->
            "\\\\"

        '\x08' ->
            "\\b"

        '\x0C' ->
            "\\f"

        '\n' ->
            "\\n"

        '\x0D' ->
            "\\r"

        '\t' ->
            "\\t"

        '"' ->
            "\\\""

        _ ->
            String.fromChar char
