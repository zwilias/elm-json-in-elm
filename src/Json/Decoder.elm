module Json.Decoder exposing (..)

import Json exposing (Value(..))
import Json.Parser exposing (parse)


type Error
    = Field String Error
    | Index Int Error
    | OneOf (List Error)
    | Failure String Value
    | BadJson Json.Parser.Error


type Decoder a
    = Decoder (Value -> Result Error a)


succeed : a -> Decoder a
succeed a =
    Decoder (\_ -> Ok a)


fail : String -> Decoder a
fail msg =
    value |> andThen (Decoder << always << Err << Failure msg)


value : Decoder Value
value =
    Decoder <| \val -> Ok val


decodeString : Decoder a -> String -> Result Error a
decodeString decoder string =
    parse string
        |> Result.mapError BadJson
        |> Result.andThen (decodeValue decoder)


decodeValue : Decoder a -> (Value -> Result Error a)
decodeValue (Decoder decoderF) =
    decoderF


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toB (Decoder decoderF) =
    Decoder <|
        \val ->
            case decoderF val of
                Ok decoded ->
                    decodeValue (toB decoded) val

                Err err ->
                    Err err


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoderF) =
    Decoder (decoderF >> Result.map f)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder decoderFA) (Decoder decoderFB) =
    Decoder <|
        \val ->
            Result.map2 f (decoderFA val) (decoderFB val)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


map3 :
    (a -> b -> c -> d)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
map3 f decA decB decC =
    map f decA
        |> andMap decB
        |> andMap decC


map4 :
    (a -> b -> c -> d -> e)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
map4 f decA decB decC decD =
    map f decA
        |> andMap decB
        |> andMap decC
        |> andMap decD


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
map5 f decA decB decC decD decE =
    map f decA
        |> andMap decB
        |> andMap decC
        |> andMap decD
        |> andMap decE


map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
map6 f decA decB decC decD decE decF =
    map f decA
        |> andMap decB
        |> andMap decC
        |> andMap decD
        |> andMap decE
        |> andMap decF


map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
map7 f decA decB decC decD decE decF decG =
    map f decA
        |> andMap decB
        |> andMap decC
        |> andMap decD
        |> andMap decE
        |> andMap decF
        |> andMap decG


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
map8 f decA decB decC decD decE decF decG decH =
    map f decA
        |> andMap decB
        |> andMap decC
        |> andMap decD
        |> andMap decE
        |> andMap decF
        |> andMap decG
        |> andMap decH


string : Decoder String
string =
    Decoder <|
        \json ->
            case json of
                JsonString val ->
                    Ok val

                _ ->
                    Err (Failure "Expected a string" json)


int : Decoder Int
int =
    Decoder <|
        \json ->
            case json of
                JsonInt val ->
                    Ok val

                _ ->
                    Err (Failure "Expected an integer" json)


float : Decoder Float
float =
    Decoder <|
        \json ->
            case json of
                JsonFloat val ->
                    Ok val

                _ ->
                    Err (Failure "Expected a float" json)


null : a -> Decoder a
null onNull =
    Decoder <|
        \json ->
            case json of
                JsonNull ->
                    Ok onNull

                _ ->
                    Err (Failure "Expected null" json)


field : String -> Decoder a -> Decoder a
field name (Decoder decoderF) =
    Decoder <|
        \json ->
            case json of
                JsonObject keyValuePairs ->
                    let
                        entry : Maybe Value
                        entry =
                            keyValuePairs
                                |> List.filter (\( key, _ ) -> key == name)
                                |> List.head
                                |> Maybe.map Tuple.second
                    in
                    case entry of
                        Nothing ->
                            Err <| Failure ("Expected an object with key '" ++ name ++ "'") json

                        Just fieldValue ->
                            decoderF fieldValue
                                |> Result.mapError (Field name)

                _ ->
                    Err <| Failure ("Expected an object with key '" ++ name ++ "'") json


at : List String -> Decoder a -> Decoder a
at path decoder =
    List.foldr field decoder path


keyValuePairs : Decoder a -> Decoder (List ( String, a ))
keyValuePairs (Decoder decoderF) =
    Decoder <|
        \json ->
            case json of
                JsonObject rawKeyValuePairs ->
                    List.foldl
                        (\( key, value ) accResult ->
                            Result.map2 ((,) key >> (::))
                                (decoderF value |> Result.mapError (Field key))
                                accResult
                        )
                        (Ok [])
                        rawKeyValuePairs
                        |> Result.map List.reverse

                _ ->
                    Err <| Failure "Expected an object" json


list : Decoder a -> Decoder (List a)
list (Decoder decoderF) =
    Decoder <|
        \json ->
            case json of
                JsonArray rawValues ->
                    List.foldl
                        (\value ( accResult, idx ) ->
                            ( Result.map2 (::)
                                (decoderF value |> Result.mapError (Index idx))
                                accResult
                            , idx + 1
                            )
                        )
                        ( Ok [], 0 )
                        rawValues
                        |> Tuple.first
                        |> Result.map List.reverse

                _ ->
                    Err <| Failure "Expected a list" json


index : Int -> Decoder a -> Decoder a
index idx (Decoder decoderF) =
    Decoder <|
        \json ->
            case json of
                JsonArray rawValues ->
                    List.foldl
                        (\value ( accResult, currentIdx ) ->
                            if idx == currentIdx then
                                ( decoderF value |> Result.mapError (Index idx), currentIdx + 1 )
                            else
                                ( accResult, currentIdx + 1 )
                        )
                        ( Err <| Failure ("Expected an array with index " ++ toString idx) json, 0 )
                        rawValues
                        |> Tuple.first

                _ ->
                    Err <| Failure "Expected a list" json


oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder <|
        \json ->
            List.foldl
                (\(Decoder decoderF) ( errors, result ) ->
                    case result of
                        Just val ->
                            ( errors, result )

                        Nothing ->
                            case decoderF json of
                                Ok val ->
                                    ( errors, Just val )

                                Err e ->
                                    ( e :: errors, Nothing )
                )
                ( [], Nothing )
                decoders
                |> (\( errors, result ) ->
                        case result of
                            Just val ->
                                Ok val

                            _ ->
                                List.reverse errors
                                    |> OneOf
                                    |> Err
                   )
