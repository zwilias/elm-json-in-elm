module JsonFuzzer exposing (equal, json, jsonString)

import Bitwise
import Char
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json
import Json.Decode as Decode
import Json.Encode as Encode exposing (encode)


jsonString : Int -> Fuzzer String
jsonString =
    json >> Fuzz.map Tuple.second


json : Int -> Fuzzer ( Json.Value, String )
json =
    rawJson
        >> Fuzz.map
            (\value ->
                let
                    asString =
                        value |> Json.toCore |> Encode.encode 0

                    fromString =
                        asString
                            |> Decode.decodeString Decode.value
                            |> Result.map Json.fromCore
                            |> Result.withDefault Json.Null
                in
                ( fromString, asString )
            )


rawJson : Int -> Fuzzer Json.Value
rawJson maxDepth =
    if maxDepth == 0 then
        Fuzz.oneOf <| List.map Tuple.second leaves
    else
        Fuzz.frequency <| leaves ++ (branches <| maxDepth - 1)


leaves : List ( Float, Fuzzer Json.Value )
leaves =
    [ 1 => Fuzz.constant Json.Null
    , 3 => Fuzz.map Json.String hardcoreString
    , 3 => Fuzz.map Json.Int Fuzz.int
    , 3 => Fuzz.map Json.Float Fuzz.float
    ]


branches : Int -> List ( Float, Fuzzer Json.Value )
branches maxDepth =
    [ 1 => Fuzz.map Json.Array (Fuzz.list (rawJson maxDepth))
    , 1 => Fuzz.map Json.Object (Fuzz.list (objectEntry maxDepth))
    ]


objectEntry : Int -> Fuzzer ( String, Json.Value )
objectEntry maxDepth =
    Fuzz.map2 (,)
        hardcoreString
        (rawJson maxDepth)


{-| Regular `Fuzz.string` is "just" ASCII. We can do better.
-}
hardcoreString : Fuzzer String
hardcoreString =
    Fuzz.list
        (ranges
            [ 0x20 => 0x7D

            -- Skipping `DELETE`
            , 0x80 => 0xD7FF

            -- Skipping surrogate pairs
            , 0xE000 => 0xFFFF

            -- Emoji!
            , 0x0001F600 => 0x0001F64F
            ]
            |> Fuzz.map byteToString
        )
        |> Fuzz.map (String.join "")


byteToString : Int -> String
byteToString int =
    if int <= 0x00010000 then
        Char.fromCode int |> String.fromChar
    else
        let
            c =
                int - 0x00010000
        in
        [ Char.fromCode (Bitwise.shiftRightZfBy 10 c |> Bitwise.or 0xD800)
        , Char.fromCode (Bitwise.and 0x03FF c |> Bitwise.or 0xDC00)
        ]
            |> String.fromList


ranges : List ( Int, Int ) -> Fuzzer Int
ranges =
    let
        makeRange : ( Int, Int ) -> ( Float, Fuzzer Int )
        makeRange ( begin, end ) =
            ( toFloat (end - begin), Fuzz.intRange begin end )
    in
    List.map makeRange >> Fuzz.frequency


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



-- JSON equality


equal : Json.Value -> Json.Value -> Expectation
equal expected actual =
    case ( expected, actual ) of
        ( Json.Float left, Json.Float right ) ->
            Expect.within (Expect.Absolute 0.0000000001) left right

        ( Json.Array left, Json.Array right ) ->
            List.map2 equal left right
                |> (::) (Expect.equal (List.length left) (List.length right))
                |> expectAll

        ( Json.Object left, Json.Object right ) ->
            let
                ( leftKeys, leftValues ) =
                    List.unzip left

                ( rightKeys, rightValues ) =
                    List.unzip right

                keysExpectation : Expectation
                keysExpectation =
                    Expect.equalLists leftKeys rightKeys

                valuesExpectation : Expectation
                valuesExpectation =
                    List.map2 equal leftValues rightValues
                        |> (::) (Expect.equal (List.length left) (List.length right))
                        |> expectAll
            in
            expectAll [ keysExpectation, valuesExpectation ]

        _ ->
            Expect.equal expected actual


expectAll : List Expectation -> Expectation
expectAll =
    List.map always >> flip Expect.all ()
