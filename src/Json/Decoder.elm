module Json.Decoder exposing (..)

import Json exposing (Value(..))
import Json.Parser exposing (parse)


{-|

    import Json

-}
type Error
    = Field String Error
    | Index Int Error
    | OneOf (List Error)
    | Failure String Value
    | BadJson Json.Parser.Error


type Decoder a
    = Decoder (Value -> Result Error a)



-- Running decoders


{-| Given a `String` representing JSON, run the provided decoder on that input.
-}
decodeString : Decoder a -> String -> Result Error a
decodeString decoder string =
    parse string
        |> Result.mapError BadJson
        |> Result.andThen (decodeValue decoder)


{-| Given a `Json.Value`, run the provided the decoder. This can never result in
a `BadJson` error.

    Json.JsonString "foo"
        |> decodeValue string
        |> Ok "foo"

-}
decodeValue : Decoder a -> (Value -> Result Error a)
decodeValue (Decoder decoderF) =
    decoderF



-- Special decoders


{-| Create a decoder that will return the same value for every _structurally
valid_ JSON.

    """ "hello world" """
        |> decodeString (succeed "foobar")
    --> Ok "foobar"

For input that fails to parse, you will still receive an error.

    """ foo """
        |> decodeString (succeed "bar")
        |> Result.mapError (always "parse error")
    --> Err "parse error"

-}
succeed : a -> Decoder a
succeed a =
    Decoder (\_ -> Ok a)


{-| Create a decoder that will always fail with the same message for every
structurally valid JSON.

    """ "hello world" """
        |> decodeString (fail "oops")
    --> Err (Failure "oops" (Json.JsonString "hello world"))

For input that fails with a parse error, you will still receive the parse error.

    parseErrorToString : Error -> String
    parseErrorToString error =
        case error of
            BadJson _ ->
                "parse error"
            _ ->
                toString error


    """ foo """
        |> decodeString (fail "oops")
        |> Result.mapError parseErrorToString
    -->  Err "parse error"

-}
fail : String -> Decoder a
fail msg =
    value |> andThen (Decoder << always << Err << Failure msg)


{-| Decode the raw `Json.Value` from the input.

    """ null """
        |> decodeString value
    --> Ok Json.JsonNull


    """ { "key": "value" } """
        |> decodeString (field "key" value)
    --> Ok (Json.JsonString "value")

-}
value : Decoder Value
value =
    Decoder <| \val -> Ok val



-- Primitives


{-| Decode a JSON string into an Elm `String`. JSON strings are weird.

    """ "hello world" """
        |> decodeString string
    --> Ok "hello world"


    """ 12 """
        |> decodeString string
    --> Err (Failure "Expected a string" (Json.JsonInt 12))


    """ "foo\\nbar\\nbar" """
        |> decodeString string
    --> Ok "foo\nbar\nbar"


    """ "foo\\u{0020}bar" """
        |> decodeString string
    --> Ok "foo bar"


    """ "foo\\u0020bar" """
        |> decodeString string
    --> Ok "foo bar"

-}
string : Decoder String
string =
    Decoder <|
        \json ->
            case json of
                JsonString val ->
                    Ok val

                _ ->
                    Err (Failure "Expected a string" json)


{-| Decode a JSON number that is a valid Elm `Int` into an Elm `Int`. JSON
numbers are really annoying.

    """ 12 """
        |> decodeString int
    --> Ok 12


    """ 3e3 """
        |> decodeString int
    --> Ok 3000


    """ 3E-2 """
        |> decodeString int
        |> Result.mapError (always "that's a float")
    --> Err "that's a float"

-}
int : Decoder Int
int =
    Decoder <|
        \json ->
            case json of
                JsonInt val ->
                    Ok val

                _ ->
                    Err (Failure "Expected an integer" json)


{-| Decodes any JSON number into a `Float`. (But like, seriously, JSON numbers).

    """ 0.5 """
        |> decodeString float
    --> Ok 0.5


    """ 12 """
        |> decodeString float
    --> Ok 12


    """ 3e-2 """
        |> decodeString float
    --> Ok 0.03


    """ 3.25e+1 """
        |> decodeString float
    --> Ok 32.5

-}
float : Decoder Float
float =
    Decoder <|
        \json ->
            case json of
                JsonFloat val ->
                    Ok val

                JsonInt val ->
                    Ok (toFloat val)

                _ ->
                    Err (Failure "Expected a float" json)


{-| Matches exactly `null`, returning a predefined value when it encounters that.

    """ null """
        |> decodeString (null 99)
    --> Ok 99

-}
null : a -> Decoder a
null onNull =
    Decoder <|
        \json ->
            case json of
                JsonNull ->
                    Ok onNull

                _ ->
                    Err (Failure "Expected null" json)



-- Structural primitives


{-| Decode the field of a JSON object with some decoder.

    """ { "foo": "bar" } """
        |> decodeString (field "foo" string)
    --> Ok "bar"


    """ "oops" """
        |> decodeString (field "foo" string)
    --> Err (Failure "Expected an object with a field 'foo'" (Json.JsonString "oops"))

-}
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
                            Err <| Failure ("Expected an object with a field '" ++ name ++ "'") json

                        Just fieldValue ->
                            decoderF fieldValue
                                |> Result.mapError (Field name)

                _ ->
                    Err <| Failure ("Expected an object with a field '" ++ name ++ "'") json


{-| Decode a certain path in nested objects.

    """ { "first": { "second": 12 } } """
        |> decodeString (at [ "first", "second" ] int)
    --> Ok 12


    """ "or with an empty list" """
        |> decodeString (at [] string)
    --> Ok "or with an empty list"

-}
at : List String -> Decoder a -> Decoder a
at path decoder =
    List.foldr field decoder path


{-| Generic decoder for turning a JSON object into a list of key-value pairs.

    """ { "utensil": "spoon", "quality": "high", "size": "large" } """
        |> decodeString (keyValuePairs string)
    --> Ok [ ( "utensil", "spoon" ), ( "quality", "high" ), ( "size", "large" ) ]


    """ { "name": "Alex", "age": 34 } """
        |> decodeString (keyValuePairs string)
    --> Err (Field "age" (Failure "Expected a string" (Json.JsonInt 34)))

-}
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


{-| Decode a JSON array into a list of values.

    """ [ "hello", "world" ] """
        |> decodeString (list string)
    --> Ok [ "hello", "world" ]


    """ [ null, 12 ] """
        |> decodeString (list int)
    --> Err (Index 0 (Failure "Expected an integer" Json.JsonNull))

-}
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


{-| Decode the value at a certain offset in a JSON array.

    """ [ null, 12 ] """
        |> decodeString (index 1 int)
    --> Ok 12

-}
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



-- Complicated structures


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


maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    oneOf [ map Just decoder, succeed Nothing ]



-- Combining decoders


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
