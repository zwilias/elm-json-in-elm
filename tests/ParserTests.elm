module ParserTests exposing (..)

import Expect exposing (Expectation)
import Json
import Json.Parser as Parser
import JsonFuzzer exposing (json)
import Test exposing (..)


fuzzTest : Test
fuzzTest =
    fuzz (json 3 0) "Random JSON strings" <|
        \( json, string ) ->
            string
                |> Parser.parse
                |> expectOk json


fuzzWithIndentationTest : Test
fuzzWithIndentationTest =
    fuzz (json 3 2) "Random JSON strings with indentation" <|
        \( json, string ) ->
            string
                |> Parser.parse
                |> expectOk json


strings : Test
strings =
    [ """ "" """ => Json.String "" Nothing
    , """ "foo" """ => Json.String "foo" Nothing
    , """ "a\\"" """ => Json.String "a\"" Nothing
    , """ "\\\\b" """ => Json.String "\\b" Nothing
    , """ "This \\" is a \\n complicated \\t string" """ => Json.String "This \" is a \n complicated \t string" Nothing
    , """ "unicode\\u0020spaces" """ => Json.String "unicode spaces" Nothing
    , """ "💩" """ => Json.String "💩" Nothing
    ]
        |> successTests "strings"


failStrings : Test
failStrings =
    [ "\"\n\""
    , "foo"
    , "\"\x0D\""
    , "\"\t\""
    ]
        |> failTests "failure string"


ints : Test
ints =
    [ """ 5 """ => Json.Int 5 Nothing
    , """ -0 """ => Json.Int 0 Nothing
    , """ 0 """ => Json.Int 0 Nothing
    , """ -9099 """ => Json.Int -9099 Nothing
    , """ 1e9 """ => Json.Int (1 * 10 ^ 9) Nothing
    , """ -4e2 """ => Json.Int (-4 * 10 ^ 2) Nothing
    , """ 1E2 """ => Json.Int (1 * 10 ^ 2) Nothing
    , """ 1e+2 """ => Json.Int (1 * 10 ^ 2) Nothing
    , """ -0.0 """ => Json.Int 0 Nothing
    , """ 0.6e5 """ => Json.Int (6 * 10 ^ 4) Nothing
    , """ 0.0e0 """ => Json.Int 0 Nothing
    , """ 0.001e3 """ => Json.Int 1 Nothing
    , """ -9.5e6 """ => Json.Int -9500000 Nothing
    ]
        |> successTests "ints"


badInts : Test
badInts =
    [ "1a0"
    , "e0"
    , "0x05"
    , "1e5.2"
    ]
        |> failTests "bad integers"


floats : Test
floats =
    [ """ 0.5 """ => Json.Float 0.5 Nothing
    , """ 0.1 """ => Json.Float 0.1 Nothing
    , """ 0.0001 """ => Json.Float 0.0001 Nothing
    , """ 1e-1 """ => Json.Float 0.1 Nothing
    ]
        |> successTests "floats"


arrays : Test
arrays =
    [ """ [] """ => Json.Array [] Nothing
    , """ [ "foo" ] """ => Json.Array [ Json.String "foo" Nothing ] Nothing
    , """ [null] """ => Json.Array [ Json.Null Nothing ] Nothing
    , """ [
null
\t,         "foo"] """
        => Json.Array
            [ Json.Null Nothing
            , Json.String "foo" Nothing
            ]
            Nothing
    , """ [ 5, 6.0, 1.0e9, -12 ] """
        => Json.Array
            [ Json.Int 5 Nothing
            , Json.Int 6 Nothing
            , Json.Int 1000000000 Nothing
            , Json.Int -12 Nothing
            ]
            Nothing
    , "[[]]" => Json.Array [ Json.Array [] Nothing ] Nothing
    ]
        |> successTests "arrays"


badArrays : Test
badArrays =
    [ "["
    , "]"
    , "[ foo ]"
    , "[ null, ]"
    , "[,]"
    ]
        |> failTests "bad arrays"


objects : Test
objects =
    [ """ {} """ => Json.Object [] Nothing
    , """ { "hello": "world" } """ => Json.Object [ "hello" => Json.String "world" Nothing ] Nothing
    ]
        |> successTests "objects"


badObjects : Test
badObjects =
    [ "{", "}", "{,}", "{:}", "{null: null}" ]
        |> failTests "bad objects"


null : Test
null =
    successTest "null" (Json.Null Nothing)


arbitrary : Test
arbitrary =
    let
        input : String
        input =
            """
[
    null,
    "foo",
    1.23,
    99,
    {
        "type": "foo",
        "bar": "baz",
        "age": 27,
        "stuff": [ 1, 2, 3 ]
    }
]
"""

        expected : Json.Value
        expected =
            Json.Array
                [ Json.Null Nothing
                , Json.String "foo" Nothing
                , Json.Float 1.23 Nothing
                , Json.Int 99 Nothing
                , Json.Object
                    [ "type" => Json.String "foo" Nothing
                    , "bar" => Json.String "baz" Nothing
                    , "age" => Json.Int 27 Nothing
                    , "stuff"
                        => Json.Array
                            [ Json.Int 1 Nothing
                            , Json.Int 2 Nothing
                            , Json.Int 3 Nothing
                            ]
                            Nothing
                    ]
                    Nothing
                ]
                Nothing
    in
    test "arbitrary" <|
        \_ ->
            input
                |> Parser.parse
                |> expectOk expected



-- helpers


successTests : String -> List ( String, Json.Value ) -> Test
successTests description cases =
    List.map (uncurry successTest) cases |> describe description


successTest : String -> Json.Value -> Test
successTest input output =
    test input <|
        \_ ->
            input
                |> Parser.parse
                |> expectOk output


failTests : String -> List String -> Test
failTests description cases =
    List.map failTest cases
        |> describe description


failTest : String -> Test
failTest input =
    test input <|
        \_ ->
            input
                |> Parser.parse
                |> Result.mapError (always ())
                |> Expect.equal (Err ())


infixr 0 =>
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


expectOk : Json.Value -> Result e Json.Value -> Expect.Expectation
expectOk okVal =
    result (toString >> Expect.fail) (JsonFuzzer.equal okVal)


result : (e -> b) -> (a -> b) -> Result e a -> b
result onError onOk value =
    case value of
        Ok a ->
            onOk a

        Err e ->
            onError e
