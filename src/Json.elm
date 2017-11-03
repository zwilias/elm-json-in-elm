module Json exposing (Bounds, Location, Value(..), equals, toCore)

{-| An exposed recursively defined union type that describes a JSON value, and
functions to go back and forth between `elm-lang/core`'s `Value` representation.

Note that manipulating this structure is much nicer when done through `Encoder`
and `Decoder`.

@docs Value, Location, Bounds, toCore, equals

-}

import Json.Encode as Core


{-| A JSON value (actual JSON, not random JS) can be described using a structure
like this one.

Since the tags are also regular types, they are expected to be used qualified
, so `Json.String` rather than `String`.

-}
type Value
    = String String (Maybe Bounds)
    | Int Int (Maybe Bounds)
    | Float Float (Maybe Bounds)
    | Null (Maybe Bounds)
    | Array (List Value) (Maybe Bounds)
    | Object (List ( String, Value )) (Maybe Bounds)


{-| TOD
-}
type alias Bounds =
    { start : Location, end : Location }


{-| TODO
-}
type alias Location =
    ( Int, Int )


{-| Convert a `Json.Value` to `Json.Encode.Value` (which is an alias for
`Json.Decode.Value`). Useful when integrating with `elm-lang/core`, though I
don't expect this library to actually be used.
-}
toCore : Value -> Core.Value
toCore value =
    case value of
        String val _ ->
            Core.string val

        Int val _ ->
            Core.int val

        Float val _ ->
            Core.float val

        Null _ ->
            Core.null

        Array values _ ->
            List.map toCore values |> Core.list

        Object kvPairs _ ->
            List.map (Tuple.mapSecond toCore) kvPairs
                |> Core.object


equalLists : (a -> a -> Bool) -> List a -> List a -> Bool
equalLists check lefts rights =
    case ( lefts, rights ) of
        ( [], [] ) ->
            True

        ( x :: xs, y :: ys ) ->
            if check x y then
                equalLists check xs ys
            else
                False

        _ ->
            False


{-| TODO
-}
equals : Value -> Value -> Bool
equals leftVal rightVal =
    let
        maxFloatDeviation : Float
        maxFloatDeviation =
            0.00000001
    in
    case ( leftVal, rightVal ) of
        ( Null _, Null _ ) ->
            True

        ( String left _, String right _ ) ->
            left == right

        ( Int left _, Int right _ ) ->
            left == right

        ( Float left _, Float right _ ) ->
            (left - maxFloatDeviation) <= right && (left + maxFloatDeviation) >= right

        ( Array leftVals _, Array rightVals _ ) ->
            equalLists equals leftVals rightVals

        ( Object lefts _, Object rights _ ) ->
            equalLists
                (\( lKey, lVal ) ( rKey, rVal ) ->
                    lKey == rKey && equals lVal rVal
                )
                (List.sortBy Tuple.first lefts)
                (List.sortBy Tuple.first rights)

        _ ->
            False
