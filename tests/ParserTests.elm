module ParserTests exposing (..)

import Expect exposing (Expectation)
import Json exposing (Value(..))
import Json.Parser as Parser
import Test exposing (..)


strings : Test
strings =
    [ """ "" """ => JsonString ""
    , """ "foo" """ => JsonString "foo"
    , """ "a\\"" """ => JsonString "a\""
    , """ "\\\\b" """ => JsonString "\\b"
    , """ "This \\" is a \\n complicated \\t string" """ => JsonString "This \" is a \n complicated \t string"
    , """ "unicode\\u{0020}spaces" """ => JsonString "unicode spaces"
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
    [ """ 5 """ => JsonInt 5
    , """ -0 """ => JsonInt 0
    , """ 0 """ => JsonInt 0
    , """ -9099 """ => JsonInt -9099
    , """ 1e9 """ => JsonInt <| 1 * 10 ^ 9
    , """ -4e2 """ => JsonInt <| -4 * 10 ^ 2
    , """ 1E2 """ => JsonInt <| 1 * 10 ^ 2
    , """ 1e+2 """ => JsonInt <| 1 * 10 ^ 2
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
    [ """ 0.5 """ => JsonFloat 0.5
    , """ -0.0 """ => JsonFloat 0
    , """ 0.6e5 """ => JsonFloat 6.0e4
    , """ 0.0e0 """ => JsonFloat 0
    , """ -9.5e6 """ => JsonFloat -9.5e6
    , """ 1e-1 """ => JsonFloat 0.1
    ]
        |> successTests "floats"


arrays : Test
arrays =
    [ """ [] """ => JsonArray []
    , """ [ "foo" ] """ => JsonArray [ JsonString "foo" ]
    , """ [null] """ => JsonArray [ JsonNull ]
    , """ [
null
\t,         "foo"] """ => JsonArray [ JsonNull, JsonString "foo" ]
    , """ [ 5, 6.0, 1.0e9, -12 ] """ => JsonArray [ JsonInt 5, JsonFloat 6, JsonFloat 1.0e9, JsonInt -12 ]
    , "[[]]" => JsonArray [ JsonArray [] ]
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
    [ """ {} """ => JsonObject []
    , """ { "hello": "world" } """ => JsonObject [ "hello" => JsonString "world" ]
    ]
        |> successTests "objects"


badObjects : Test
badObjects =
    [ "{", "}", "{,}", "{:}", "{null: null}" ]
        |> failTests "bad objects"


null : Test
null =
    successTest "null" JsonNull


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
            JsonArray
                [ JsonNull
                , JsonString "foo"
                , JsonFloat 1.23
                , JsonInt 99
                , JsonObject
                    [ "type" => JsonString "foo"
                    , "bar" => JsonString "baz"
                    , "age" => JsonInt 27
                    , "stuff" => JsonArray [ JsonInt 1, JsonInt 2, JsonInt 3 ]
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
