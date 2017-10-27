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
