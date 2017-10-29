module EncoderTests exposing (..)

import Expect
import Json.Encoder as Encoder
import JsonFuzzer exposing (json)
import Test exposing (Test, fuzz, test)


simpleFuzz : Test
simpleFuzz =
    fuzz (json 0) "simple fuzz test" <|
        \( json, string ) ->
            Encoder.encode json
                |> Expect.equal string


fullFuzz : Test
fullFuzz =
    fuzz (json 3) "full on fuzzer" <|
        \( json, string ) ->
            Encoder.encode json
                |> Expect.equal string


nonUniqueKeyPrecedenceForStringKeys : Test
nonUniqueKeyPrecedenceForStringKeys =
    test "When a key is used multiple time, the last one wins" <|
        \_ ->
            Encoder.object
                [ "a" => Encoder.string "a"
                , "b" => Encoder.null
                , "a" => Encoder.string "b"
                ]
                |> Encoder.encode
                |> Expect.equal """{"b":null,"a":"b"}"""


intKeysBeforeStringKeys : Test
intKeysBeforeStringKeys =
    test "Integer keys come before string keys" <|
        \_ ->
            Encoder.object
                [ "a" => Encoder.string "a"
                , "0" => Encoder.string "ignore me"
                , "-1" => Encoder.string "third"
                , "a" => Encoder.string "fourth"
                , "0" => Encoder.string "first"
                , "99999" => Encoder.string "second"
                ]
                |> Encoder.encode
                |> Expect.equal """{"0":"first","99999":"second","-1":"third","a":"fourth"}"""


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
