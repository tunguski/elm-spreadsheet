module Spreadsheet.Value exposing
    ( Value(..)
    , Error(..)
    , errorText
    , isError
    , toNumber
    , toNumberLenient
    , toText
    , toBool
    , typeName
    , compare
    , equalValue
    , fromString
    )

{-| The dynamically-typed value that lives in (or is computed by) a cell.

A spreadsheet cell is, ultimately, one of a small set of scalar values — a number,
a string, a boolean, "empty", or one of the standard `#…!` error sentinels. Formula
evaluation is a function from the sheet to one of these. Coercions between them follow
the conventions used by Excel / Google Sheets (e.g. an empty cell coerces to `0` in a
numeric context and to `""` in a text context; `TRUE` is `1`).

@docs Value, Error, errorText, isError
@docs toNumber, toNumberLenient, toText, toBool, typeName, compare, equalValue, fromString

-}


{-| A scalar cell value. Ranges/arrays are handled one level up in the evaluator;
a single cell only ever holds one of these.
-}
type Value
    = VNumber Float
    | VText String
    | VBool Bool
    | VEmpty
    | VError Error


{-| The standard spreadsheet error values. `Circular` and `Parse` are our own,
surfaced as `#CIRC!` and `#ERROR!`.
-}
type Error
    = DivZero
    | RefErr
    | NameErr
    | ValueErr
    | NumErr
    | NA
    | Circular
    | Parse


{-| The visible text of an error, e.g. `#DIV/0!`. -}
errorText : Error -> String
errorText err =
    case err of
        DivZero ->
            "#DIV/0!"

        RefErr ->
            "#REF!"

        NameErr ->
            "#NAME?"

        ValueErr ->
            "#VALUE!"

        NumErr ->
            "#NUM!"

        NA ->
            "#N/A"

        Circular ->
            "#CIRC!"

        Parse ->
            "#ERROR!"


{-| True when the value is any error. -}
isError : Value -> Bool
isError value =
    case value of
        VError _ ->
            True

        _ ->
            False


{-| Coerce to a number the way an arithmetic operator would. Empty is `0`, booleans
are `1`/`0`, a numeric-looking string parses, anything else (or an error) yields a
`#VALUE!` error wrapped as `Err`.
-}
toNumber : Value -> Result Error Float
toNumber value =
    case value of
        VNumber n ->
            Ok n

        VBool b ->
            Ok (boolToNum b)

        VEmpty ->
            Ok 0

        VError e ->
            Err e

        VText s ->
            case parseNumber (String.trim s) of
                Just n ->
                    Ok n

                Nothing ->
                    Err ValueErr


{-| Like `toNumber` but non-numeric text counts as `0` rather than an error — used by
aggregate functions (SUM, AVERAGE) which ignore text in ranges. Errors still propagate.
-}
toNumberLenient : Value -> Result Error Float
toNumberLenient value =
    case value of
        VText _ ->
            Ok 0

        VBool _ ->
            Ok 0

        _ ->
            toNumber value


{-| Coerce to display/operand text. Numbers use a compact, round-trippy format. -}
toText : Value -> String
toText value =
    case value of
        VText s ->
            s

        VNumber n ->
            numberToString n

        VBool b ->
            if b then
                "TRUE"

            else
                "FALSE"

        VEmpty ->
            ""

        VError e ->
            errorText e


{-| Coerce to boolean. Numbers are truthy when non-zero; "TRUE"/"FALSE" text parses;
empty is false; errors propagate.
-}
toBool : Value -> Result Error Bool
toBool value =
    case value of
        VBool b ->
            Ok b

        VNumber n ->
            Ok (n /= 0)

        VEmpty ->
            Ok False

        VError e ->
            Err e

        VText s ->
            case String.toUpper (String.trim s) of
                "TRUE" ->
                    Ok True

                "FALSE" ->
                    Ok False

                _ ->
                    Err ValueErr


{-| A human label for the value's type, used in tests and tooltips. -}
typeName : Value -> String
typeName value =
    case value of
        VNumber _ ->
            "number"

        VText _ ->
            "text"

        VBool _ ->
            "boolean"

        VEmpty ->
            "empty"

        VError _ ->
            "error"


boolToNum : Bool -> Float
boolToNum b =
    if b then
        1

    else
        0


{-| Ordering used by sorting, MIN/MAX over mixed types and the comparison operators.
Follows the Excel type order: numbers < text < booleans, with empty treated as `0`/`""`
contextually. Here we give a total order: number < text < bool < error.
-}
compare : Value -> Value -> Order
compare a b =
    case ( a, b ) of
        ( VNumber x, VNumber y ) ->
            Basics.compare x y

        ( VText x, VText y ) ->
            Basics.compare (String.toUpper x) (String.toUpper y)

        ( VBool x, VBool y ) ->
            Basics.compare (boolToNum x) (boolToNum y)

        _ ->
            Basics.compare (rank a) (rank b)


rank : Value -> Int
rank value =
    case value of
        VEmpty ->
            0

        VNumber _ ->
            1

        VText _ ->
            2

        VBool _ ->
            3

        VError _ ->
            4


{-| Value equality with spreadsheet coercion: `1 = TRUE`, `"x" = "X"` (text compares
case-insensitively), empty equals `0` and `""`.
-}
equalValue : Value -> Value -> Bool
equalValue a b =
    case ( a, b ) of
        ( VError _, _ ) ->
            False

        ( _, VError _ ) ->
            False

        ( VText x, VText y ) ->
            String.toUpper x == String.toUpper y

        ( VText _, _ ) ->
            False

        ( _, VText _ ) ->
            False

        _ ->
            case ( toNumber a, toNumber b ) of
                ( Ok x, Ok y ) ->
                    x == y

                _ ->
                    False


{-| Parse a raw user-entered string (not a formula) into a literal value: numbers,
booleans and percentages are recognised, everything else stays text. Empty input is
`VEmpty`.
-}
fromString : String -> Value
fromString raw =
    let
        s =
            String.trim raw
    in
    if s == "" then
        VEmpty

    else
        case String.toUpper s of
            "TRUE" ->
                VBool True

            "FALSE" ->
                VBool False

            _ ->
                if String.endsWith "%" s then
                    case parseNumber (String.dropRight 1 s) of
                        Just n ->
                            VNumber (n / 100)

                        Nothing ->
                            VText raw

                else
                    case parseNumber s of
                        Just n ->
                            VNumber n

                        Nothing ->
                            VText raw


parseNumber : String -> Maybe Float
parseNumber s =
    let
        trimmed =
            String.trim s
    in
    if trimmed == "" then
        Nothing

    else
        String.toFloat trimmed


{-| Render a Float without a trailing `.0` for integers, and without exponent noise for
the values a spreadsheet typically holds. This is the *default* display; `Format` can
override it.
-}
numberToString : Float -> String
numberToString n =
    if isNaN n then
        "#NUM!"

    else if isInfinite n then
        "#NUM!"

    else if n == toFloat (round n) && abs n < 1.0e15 then
        String.fromInt (round n)

    else
        String.fromFloat n
