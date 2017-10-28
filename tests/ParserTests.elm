module ParserTests exposing (..)

import Expect exposing (Expectation)
import Json exposing (Value)
import Json.Parser as Parser
import Test exposing (..)


strings : Test
strings =
    [ """ "" """ => Json.String ""
    , """ "foo" """ => Json.String "foo"
    , """ "a\\"" """ => Json.String "a\""
    , """ "\\\\b" """ => Json.String "\\b"
    , """ "This \\" is a \\n complicated \\t string" """ => Json.String "This \" is a \n complicated \t string"
    , """ "unicode\\u{0020}spaces" """ => Json.String "unicode spaces"
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
    [ """ 5 """ => Json.Int 5
    , """ -0 """ => Json.Int 0
    , """ 0 """ => Json.Int 0
    , """ -9099 """ => Json.Int -9099
    , """ 1e9 """ => Json.Int <| 1 * 10 ^ 9
    , """ -4e2 """ => Json.Int <| -4 * 10 ^ 2
    , """ 1E2 """ => Json.Int <| 1 * 10 ^ 2
    , """ 1e+2 """ => Json.Int <| 1 * 10 ^ 2
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
    [ """ 0.5 """ => Json.Float 0.5
    , """ -0.0 """ => Json.Float 0
    , """ 0.6e5 """ => Json.Float 6.0e4
    , """ 0.0e0 """ => Json.Float 0
    , """ -9.5e6 """ => Json.Float -9.5e6
    , """ 1e-1 """ => Json.Float 0.1
    ]
        |> successTests "floats"


arrays : Test
arrays =
    [ """ [] """ => Json.Array []
    , """ [ "foo" ] """ => Json.Array [ Json.String "foo" ]
    , """ [null] """ => Json.Array [ Json.Null ]
    , """ [
null
\t,         "foo"] """ => Json.Array [ Json.Null, Json.String "foo" ]
    , """ [ 5, 6.0, 1.0e9, -12 ] """ => Json.Array [ Json.Int 5, Json.Float 6, Json.Float 1.0e9, Json.Int -12 ]
    , "[[]]" => Json.Array [ Json.Array [] ]
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
    [ """ {} """ => Json.Object []
    , """ { "hello": "world" } """ => Json.Object [ "hello" => Json.String "world" ]
    ]
        |> successTests "objects"


badObjects : Test
badObjects =
    [ "{", "}", "{,}", "{:}", "{null: null}" ]
        |> failTests "bad objects"


null : Test
null =
    successTest "null" Json.Null


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

        expected : Value
        expected =
            Json.Array
                [ Json.Null
                , Json.String "foo"
                , Json.Float 1.23
                , Json.Int 99
                , Json.Object
                    [ "type" => Json.String "foo"
                    , "bar" => Json.String "baz"
                    , "age" => Json.Int 27
                    , "stuff" => Json.Array [ Json.Int 1, Json.Int 2, Json.Int 3 ]
                    ]
                ]
    in
    test "arbitrary" <|
        \_ ->
            input
                |> Parser.parse
                |> Expect.equal (Ok expected)



-- helpers


successTests : String -> List ( String, Value ) -> Test
successTests description cases =
    List.map (uncurry successTest) cases |> describe description


successTest : String -> Value -> Test
successTest input output =
    test input <|
        \_ ->
            input
                |> Parser.parse
                |> Expect.equal (Ok output)


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
