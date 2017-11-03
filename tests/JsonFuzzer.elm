module JsonFuzzer exposing (equal, json)

import Bitwise
import Char
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json exposing (Value)
import Json.Encode as Core
import Json.Encoder as Encoder


json : Int -> Int -> Fuzzer ( Value, String )
json maxDepth indent =
    rawJson maxDepth
        |> Fuzz.map
            (\value -> ( value, value |> Json.toCore |> Core.encode indent ))


rawJson : Int -> Fuzzer Value
rawJson maxDepth =
    if maxDepth == 0 then
        Fuzz.oneOf <| List.map Tuple.second leaves
    else
        Fuzz.frequency <| leaves ++ (branches <| maxDepth - 1)


leaves : List ( Float, Fuzzer Value )
leaves =
    [ 1 => Fuzz.constant Encoder.null
    , 3 => Fuzz.map Encoder.string hardcoreString
    , 3 => Fuzz.map Encoder.int Fuzz.int
    , 3 => Fuzz.map Encoder.float Fuzz.float
    ]


branches : Int -> List ( Float, Fuzzer Value )
branches maxDepth =
    [ 1 => Fuzz.map Encoder.list (Fuzz.list (rawJson maxDepth))
    , 1 => Fuzz.map Encoder.object (Fuzz.list (objectEntry maxDepth))
    ]


objectEntry : Int -> Fuzzer ( String, Value )
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


equal : Value -> Value -> Expectation
equal expected actual =
    case ( expected, actual ) of
        ( Json.Int left _, Json.Int right _ ) ->
            left |> Expect.equal right

        ( Json.Float left _, Json.Float right _ ) ->
            Expect.within (Expect.Absolute 0.0000000001) left right

        ( Json.Array left _, Json.Array right _ ) ->
            List.map2 equal left right
                |> (::) (Expect.equal (List.length left) (List.length right))
                |> expectAll

        ( Json.String left _, Json.String right _ ) ->
            left |> Expect.equal right

        ( Json.Null _, Json.Null _ ) ->
            Expect.pass

        ( Json.Object left _, Json.Object right _ ) ->
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
            Expect.fail "I need a better error here 😞"


expectAll : List Expectation -> Expectation
expectAll =
    List.map always >> flip Expect.all ()
