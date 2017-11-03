module Json.Encoder exposing (encode, float, int, list, null, object, string)

{-| TODO

@docs encode

@docs string, float, int, null, list, object

-}

import Dict
import Json exposing (Value)
import Set exposing (Set)


{-| TODO
-}
string : String -> Value
string val =
    Json.String val Nothing


{-| TODO
-}
int : Int -> Value
int val =
    Json.Int val Nothing


{-| TODO
-}
float : Float -> Value
float v =
    if (round >> toFloat) v == v then
        Json.Int (round v) Nothing
    else
        Json.Float v Nothing


{-| TODO
-}
list : List Value -> Value
list vals =
    Json.Array vals Nothing


{-| TODO
-}
null : Value
null =
    Json.Null Nothing


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


{-| TODO
-}
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
        >> flip Json.Object Nothing


{-| TODO
-}
encode : Int -> Value -> String
encode indentDepth value =
    let
        separators : Separators
        separators =
            if indentDepth == 0 then
                zeroIndent
            else
                indent indentDepth
    in
    tokenize value []
        |> stringify separators 0 ""


zeroIndent : Separators
zeroIndent =
    { increase = 0, line = "", pair = "" }


indent : Int -> Separators
indent increment =
    { increase = increment, line = "\n", pair = " " }


type alias Separators =
    { increase : Int, line : String, pair : String }


stringify : Separators -> Int -> String -> List Token -> String
stringify separator currentDepth acc tokens =
    let
        indent : Int -> String
        indent depth =
            String.repeat depth " "
    in
    case tokens of
        [] ->
            acc

        [ Token token content ] ->
            case token of
                Close ->
                    acc ++ content

                Literal ->
                    acc ++ content

                _ ->
                    acc

        (Token current val) :: (((Token next _) :: _) as rest) ->
            let
                nextDepth =
                    nextIndent currentDepth separator.increase current next

                suffix =
                    case ( current, next ) of
                        ( Literal, Close ) ->
                            separator.line ++ indent nextDepth

                        ( Literal, _ ) ->
                            "," ++ separator.line ++ indent nextDepth

                        ( PairKey, _ ) ->
                            ":" ++ separator.pair

                        ( Open, Close ) ->
                            ""

                        ( Open, _ ) ->
                            separator.line ++ indent nextDepth

                        ( _, Close ) ->
                            separator.line ++ indent nextDepth

                        ( Close, _ ) ->
                            "," ++ separator.line ++ indent nextDepth
            in
            stringify separator nextDepth (acc ++ val ++ suffix) rest


nextIndent : Int -> Int -> TokenType -> TokenType -> Int
nextIndent depth increment current next =
    case ( current, next ) of
        ( Open, Close ) ->
            depth

        ( Open, _ ) ->
            depth + increment

        ( _, Close ) ->
            depth - increment

        _ ->
            depth


type Token
    = Token TokenType String


type TokenType
    = Open
    | Close
    | PairKey
    | Literal


open : String -> Token
open =
    Token Open


close : String -> Token
close =
    Token Close


pairKey : String -> Token
pairKey =
    Token PairKey


literal : String -> Token
literal =
    Token Literal


tokenize : Value -> List Token -> List Token
tokenize value tokens =
    case value of
        Json.String string _ ->
            (literal <| "\"" ++ escape string ++ "\"") :: tokens

        Json.Int int _ ->
            (literal <| toString int) :: tokens

        Json.Float float _ ->
            (literal <| toString float) :: tokens

        Json.Null _ ->
            literal "null" :: tokens

        Json.Array valueList _ ->
            open "[" :: List.foldr tokenize (close "]" :: tokens) valueList

        Json.Object valueStringList _ ->
            open "{"
                :: List.foldr
                    (\( k, v ) acc ->
                        pairKey ("\"" ++ escape k ++ "\"") :: tokenize v acc
                    )
                    (close "}" :: tokens)
                    valueStringList


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
