module Json.Parser exposing (Error, parse)

import Char
import Json exposing (Value(..))
import Parser exposing (..)
import Parser.LanguageKit exposing (..)


type alias Error =
    Parser.Error


parse : String -> Result Error Value
parse =
    run (json |. end)


spaces : Parser ()
spaces =
    ignore zeroOrMore (flip List.member [ ' ', '\t', '\n' ])


json : Parser Value
json =
    succeed identity
        |. spaces
        |= oneOf
            [ map JsonString jsonString
            , map (either JsonInt JsonFloat) jsonNumber
            , map (\_ -> JsonNull) (keyword "null")
            , map JsonArray (lazy <| \_ -> jsonArray)
            , map JsonObject (lazy <| \_ -> jsonObject)
            ]
        |. spaces



-- Dealing with strings


jsonString : Parser String
jsonString =
    inContext "string" <|
        succeed identity
            |. symbol "\""
            |= stringContent ""
            |. symbol "\""


stringContent : String -> Parser String
stringContent acc =
    oneOf
        [ escapedControlCharacter |> andThen (\s -> stringContent <| acc ++ s)
        , escapedUnicode |> andThen (\s -> stringContent <| acc ++ s)
        , nonControlCharacters |> andThen (\s -> stringContent <| acc ++ s)
        , succeed acc
        ]


nonControlCharacters : Parser String
nonControlCharacters =
    keep oneOrMore
        (not << anyMatch [ (==) '"', (==) '\\', Char.toCode >> isControlChar ])


anyMatch : List (a -> Bool) -> a -> Bool
anyMatch predicates value =
    List.map ((|>) value) predicates |> List.any identity


isControlChar : Char.KeyCode -> Bool
isControlChar keyCode =
    (keyCode < 0x20) || (keyCode == 0x7F)


escapedControlCharacter : Parser String
escapedControlCharacter =
    [ ( "\\\"", "\"" )
    , ( "\\\\", "\\" )
    , ( "\\b", "\x08" )
    , ( "\\r", "\x0D" )
    , ( "\\n", "\n" )
    , ( "\\f", "\x0C" )
    , ( "\\t", "\t" )
    ]
        |> List.map (uncurry symbolicString)
        |> oneOf


symbolicString : String -> String -> Parser String
symbolicString expected replacement =
    succeed replacement |. symbol expected


escapedUnicode : Parser String
escapedUnicode =
    succeed identity
        |. symbol "\\u"
        |= possiblyWrapped
            { begin = "{"
            , end = "}"
            , content = hexQuad |> map (Char.fromCode >> String.fromChar)
            }


possiblyWrapped : { begin : String, end : String, content : Parser a } -> Parser a
possiblyWrapped { begin, end, content } =
    oneOf
        [ succeed identity |. symbol begin |= content |. symbol end
        , content
        ]


hexQuad : Parser Int
hexQuad =
    keep (Exactly 4) Char.isHexDigit |> andThen hexQuadToInt


hexQuadToInt : String -> Parser Int
hexQuadToInt quad =
    ("0x" ++ quad)
        |> String.toInt
        |> result fail succeed



-- Dealing with numbers


jsonNumber : Parser (Either Int Float)
jsonNumber =
    inContext "a json number" <|
        succeed applySignToIntOrFloat
            |= maybeNegative
            |= (digits |> andThen exponentiateOrFloat)


exponentiateOrFloat : Int -> Parser (Either Int Float)
exponentiateOrFloat int =
    oneOf
        [ parseFloat int |> map Right |> andThen maybeExponentiate
        , succeed int |> map Left |> andThen maybeExponentiate
        ]


parseFloat : Int -> Parser Float
parseFloat intPart =
    inContext "float" <|
        succeed (makeFloat intPart)
            |. symbol "."
            |= digits


makeFloat : Int -> Int -> Float
makeFloat integerPart fracPart =
    toFloat integerPart + toFractional (toFloat fracPart)


toFractional : Float -> Float
toFractional float =
    if float > 1 then
        toFractional (float / 10)
    else
        float


exponent : Parser Int
exponent =
    inContext "exponent" <|
        succeed applySign
            |. oneOf [ symbol "e", symbol "E" ]
            |= sign
            |= digits


maybeExponentiate : Either Int Float -> Parser (Either Int Float)
maybeExponentiate number =
    oneOf
        [ map (applyExponent number) exponent
        , succeed number
        ]


applyExponent : Either Int Float -> Int -> Either Int Float
applyExponent coeff exponent =
    case coeff of
        Left int ->
            if exponent < 0 then
                Right <| applyExponentToFloat (toFloat int) exponent
            else
                Left <| applyExponentToInt int exponent

        Right float ->
            Right <| applyExponentToFloat float exponent


applyExponentToInt : Int -> Int -> Int
applyExponentToInt coeff exponent =
    coeff * (10 ^ exponent)


applyExponentToFloat : Float -> Int -> Float
applyExponentToFloat coeff exponent =
    coeff * (10 ^ toFloat exponent)


digits : Parser Int
digits =
    keep oneOrMore Char.isDigit
        |> andThen (String.toInt >> result fail succeed)


maybeNegative : Parser Sign
maybeNegative =
    oneOf
        [ symbol "-" |> map (always Negative)
        , succeed Positive
        ]


sign : Parser Sign
sign =
    oneOf
        [ symbol "-" |> map (always Negative)
        , symbol "+" |> map (always Positive)
        , succeed Positive
        ]


type Sign
    = Positive
    | Negative


applySign : Sign -> number -> number
applySign sign number =
    case sign of
        Positive ->
            number

        Negative ->
            -number


applySignToIntOrFloat : Sign -> Either number1 number2 -> Either number1 number2
applySignToIntOrFloat sign =
    mapBoth (applySign sign)



-- Dealing with arrays


jsonArray : Parser (List Value)
jsonArray =
    list spaces (lazy <| \_ -> json)


jsonObject : Parser (List ( String, Value ))
jsonObject =
    sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = spaces
        , item = lazy <| \_ -> jsonField
        , trailing = Forbidden
        }


jsonField : Parser ( String, Value )
jsonField =
    succeed (,)
        |= jsonString
        |. spaces
        |. symbol ":"
        |. spaces
        |= (lazy <| \_ -> json)



-- generic helpers


type Either a b
    = Left a
    | Right b


either : (a -> c) -> (b -> c) -> Either a b -> c
either left right leftOrRight =
    case leftOrRight of
        Left val ->
            left val

        Right val ->
            right val


mapBoth : (a -> b) -> Either a a -> Either b b
mapBoth f leftOrRight =
    case leftOrRight of
        Left val ->
            Left (f val)

        Right val ->
            Right (f val)


result : (e -> b) -> (a -> b) -> Result e a -> b
result onError onSuccess res =
    case res of
        Err e ->
            onError e

        Ok a ->
            onSuccess a
