module DecoderTests exposing (..)

import Expect exposing (Expectation)
import Json.Decoder as Decoder
import Test exposing (..)


simpleString : Test
simpleString =
    test "simple string" <|
        \_ ->
            """ "hello" """
                |> Decoder.decodeString Decoder.string
                |> Expect.equal (Ok "hello")


simpleInt : Test
simpleInt =
    test "simple int" <|
        \_ ->
            """ 5 """
                |> Decoder.decodeString Decoder.int
                |> Expect.equal (Ok 5)


simpleFloat : Test
simpleFloat =
    test "simple float" <|
        \_ ->
            """ 5.0 """
                |> Decoder.decodeString Decoder.float
                |> Expect.equal (Ok 5.0)


simpleNull : Test
simpleNull =
    test "simple null" <|
        \_ ->
            """ null """
                |> Decoder.decodeString (Decoder.null ())
                |> Expect.equal (Ok ())


simpleList : Test
simpleList =
    test "simple list" <|
        \_ ->
            """ [] """
                |> Decoder.decodeString (Decoder.list Decoder.string)
                |> Expect.equal (Ok [])


simpleKeyValuePairs : Test
simpleKeyValuePairs =
    test "simple key value pairs" <|
        \_ ->
            """ {} """
                |> Decoder.decodeString (Decoder.keyValuePairs Decoder.string)
                |> Expect.equal (Ok [])
