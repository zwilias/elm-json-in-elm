module Json.Parser exposing (Error, parse)

import Char
import Json exposing (Value)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Count(Exactly)
        , Parser
        , andThen
        , end
        , fail
        , ignore
        , inContext
        , keep
        , keyword
        , lazy
        , map
        , oneOf
        , oneOrMore
        , succeed
        , symbol
        , zeroOrMore
        )
import Parser.LanguageKit exposing (Trailing(Forbidden), list, sequence)


type alias Error =
    Parser.Error


parse : String -> Result Error Value
parse =
    Parser.run (json |. end)


spaces : Parser ()
spaces =
    ignore zeroOrMore (flip List.member [ ' ', '\t', '\n' ])


json : Parser Value
json =
    succeed identity
        |. spaces
        |= oneOf
            [ map Json.String jsonString
            , map (either Json.Int Json.Float) jsonNumber
            , succeed Json.Null |. keyword "null"
            , map Json.Array (lazy <| \_ -> jsonArray)
            , map Json.Object (lazy <| \_ -> jsonObject)
            ]
        |. spaces



-- Dealing with strings
{- A JSON string is a sequence of characters enclosed by double quotes. Within
   those quotes, a specific bunch of characters is allowed and has special
   meaning.

   Importantly, control characters are not allowed unless escaped.
-}


jsonString : Parser String
jsonString =
    inContext "string" <|
        succeed identity
            |. symbol "\""
            |= stringContent ""
            |. symbol "\""



{- We build up the contents of the string piece by piece. Once we run out of
   acceptable characters, the next character must be a double quote. If it is
   not, the JSON isn't valid.
-}


stringContent : String -> Parser String
stringContent acc =
    let
        {- This little trick allows us to piece the parts together.
           Strictly speaking, it requires a few extra allocations that aren't
           absolutely necessary, but clarity, simplicity and correctness are
           chosen over pure performance in this implementation.
        -}
        continue : String -> Parser String
        continue string =
            stringContent <| acc ++ string
    in
    oneOf
        [ {- First things first, escaped control characters.
             This means that characters like `\n` must be escaped as `\\n`
             So a literal backslash followed by a literal `n`.
          -}
          escapedControlCharacter |> andThen continue

        {- Arbitrary unicode can be embedded using `\uXXXX` where the `X`s form
           a valid hexadecimal sequence.
        -}
        , escapedUnicode |> andThen continue

        {- Finally, we have the rest of unicode, specifically disallowing certain
           things: control characters, literal `\` and literal `"`.
        -}
        , nonControlCharacters |> andThen continue

        {- If none of the above produce anything, we succeed with what we've
           accumulated so far.
        -}
        , succeed acc
        ]


escapedControlCharacter : Parser String
escapedControlCharacter =
    {- According to [RFC](), these character must be escaped using a literal
       backslash, except `/` which _may_ be escaped.

       [RFC]: https://tools.ietf.org/html/rfc7159#section-7
    -}
    [ ( "\\\"", "\"" )
    , ( "\\\\", "\\" )
    , ( "\\/", "/" )
    , ( "\\b", "\x08" )
    , ( "\\f", "\x0C" )
    , ( "\\n", "\n" )
    , ( "\\r", "\x0D" )
    , ( "\\t", "\t" )
    ]
        |> List.map symbolicString
        |> oneOf


symbolicString : ( String, String ) -> Parser String
symbolicString ( expected, replacement ) =
    succeed replacement |. symbol expected


escapedUnicode : Parser String
escapedUnicode =
    {- JavaScript (and soon, Elm) allow arbitrary UTF-16 codepoints to be
       written using `\uBEEF` syntax. These may also appear in escaped version
       in JSON, so a literal `\u` followed by 4 hexadecimal characters.

       This means something like a space may also be written as `\\u0020`
    -}
    succeed (Char.fromCode >> String.fromChar)
        |. symbol "\\u"
        |= hexQuad


hexQuad : Parser Int
hexQuad =
    keep (Exactly 4) Char.isHexDigit |> andThen hexQuadToInt


hexQuadToInt : String -> Parser Int
hexQuadToInt quad =
    ("0x" ++ quad)
        -- Kind of cheating here.
        |> String.toInt
        |> result fail succeed


nonControlCharacters : Parser String
nonControlCharacters =
    {- So basically, anything other than control characters, literal backslashes
       (which are already handled, unless it's invalid json), and the closing `"`
    -}
    keep oneOrMore
        (noneMatch [ (==) '"', (==) '\\', Char.toCode >> isControlChar ])


isControlChar : Char.KeyCode -> Bool
isControlChar keyCode =
    (keyCode < 0x20) || (keyCode == 0x7F)



-- Dealing with numbers


jsonNumber : Parser (Either Int Float)
jsonNumber =
    {- This involves quite a few brittle things:

        - decode the sign (so possibly a unary minus)
        - decodes digits, then checks if its scientific notation or a float
        - and finally, it applies the sign.

       The sign has to be applied at the end, or we risk messing up float-stuff.
    -}
    inContext "a json number" <|
        succeed applySignToIntOrFloat
            |= maybeNegative
            |= (digits |> andThen exponentiateOrFloat)


digitString : Parser String
digitString =
    keep oneOrMore Char.isDigit


digits : Parser Int
digits =
    digitString
        |> andThen (String.toInt >> result fail succeed)


applySignToIntOrFloat : Sign -> Either number1 number2 -> Either number1 number2
applySignToIntOrFloat sign =
    mapBoth (applySign sign)


maybeNegative : Parser Sign
maybeNegative =
    oneOf
        [ symbol "-" |> map (always Negative)
        , succeed Positive
        ]


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
            |= digitString


makeFloat : Int -> String -> Float
makeFloat integerPart fracPart =
    toFloat integerPart + toFractional fracPart


toFractional : String -> Float
toFractional floatString =
    {- We parse "0.1" in 2 parts - an integer `0` and a _String_ `1`. The reason
       for parsing the fractional as a String is because we need to handle cases
       like `1.01`. So, to turn the string `"01"` into `0.01`, we turn it into
       an integer and move if right by dividing by 10 a couple of times.
    -}
    floatString
        |> String.toInt
        |> Result.withDefault 0
        |> dividedBy (10 ^ String.length floatString)


dividedBy : Int -> Int -> Float
dividedBy divisor dividend =
    toFloat dividend / toFloat divisor


maybeExponentiate : Either Int Float -> Parser (Either Int Float)
maybeExponentiate number =
    {- At this point, we're dealing with either an Int or a Float, and may
       encounter an exponent (in the scientific notation sense).

       For example, `0.01e2` means we'll have a `Right 0.01` value here, and
       want to multiply it by `10 ^ 2`.

       Writing this down makes me realize that `0.01e2` could conceivably be
       considered an Int; and I may need to revise my strategy. Especially since
       we already make sure to parse `4e-1` as a Float (since having a `0.4` as
       an Int seems _really_ wrong).
    -}
    oneOf
        [ exponent |> map (applyExponent number)
        , succeed number
        ]


exponent : Parser Int
exponent =
    inContext "exponent" <|
        succeed applySign
            |. oneOf [ symbol "e", symbol "E" ]
            |= sign
            |= digits


applyExponent : Either Int Float -> Int -> Either Int Float
applyExponent coeff exponent =
    {- I'm not particularly proud of this bit.

       The core issue is that exponentiation in Elm isn't very type-safe.

           (^) : number -> number -> number

       In other words, according to the type signature, `n ^ m`  is an integer
       when both `n` and `m` are integers. However, negative exponents sort of
       mess this up: `10 ^ -1 = 0.1`. However, if both parameters are integers,
       Elm will actually say `0.1` is an integer.

       Hence, when the coefficient is an integer and the exponent is negative,
       we return a `Float` here.

       So; new idea: consider everything a Float and "cast" it to an Int if
       possible as late as possible.
    -}
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


type Sign
    = Positive
    | Negative


sign : Parser Sign
sign =
    oneOf
        [ symbol "-" |> map (always Negative)
        , symbol "+" |> map (always Positive)
        , succeed Positive
        ]


applySign : Sign -> number -> number
applySign sign number =
    case sign of
        Positive ->
            number

        Negative ->
            -number



-- Dealing with arrays


jsonArray : Parser (List Value)
jsonArray =
    list spaces (lazy <| \_ -> json)



-- Dealing with objects


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


noneMatch : List (a -> Bool) -> a -> Bool
noneMatch predicates value =
    not <| anyMatch predicates value


anyMatch : List (a -> Bool) -> a -> Bool
anyMatch predicates value =
    List.map ((|>) value) predicates |> List.any identity
