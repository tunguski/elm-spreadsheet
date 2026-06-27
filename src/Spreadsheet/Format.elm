module Spreadsheet.Format exposing
    ( Format(..)
    , default
    , format
    , applyTextFormat
    , describe
    , alignmentClass
    )

{-| Turn a computed `Value` into the string the user sees, honouring a cell's number
format.

Two layers:

  - `Format` is the structured, UI-friendly set of formats a cell can carry (General,
    Number, Currency, Percent, Scientific, DateTime, Text, or a raw `Custom` code).
  - `applyTextFormat` interprets an Excel/Sheets-style format **code string**
    (`"#,##0.00"`, `"0.0%"`, `"$#,##0"`, `"yyyy-mm-dd"`). It backs both the `Custom`
    format and the `TEXT()` worksheet function.

Colour and alignment are intentionally *not* baked into the string — they are surfaced
as CSS classes (`alignmentClass`) so the stylesheet stays in charge of appearance.

@docs Format, default, format, applyTextFormat, describe, alignmentClass

-}

import Spreadsheet.Value as Value exposing (Value(..))


{-| A cell's number format. -}
type Format
    = General
    | Number Int Bool
      -- decimals, thousands-separator
    | Currency String Int
      -- symbol, decimals
    | Percent Int
      -- decimals
    | Scientific Int
    | DateTime String
      -- a date/time code, e.g. "yyyy-mm-dd"
    | Text
    | Custom String


{-| The default ("General") format. -}
default : Format
default =
    General


{-| A short human label, for a format picker. -}
describe : Format -> String
describe fmt =
    case fmt of
        General ->
            "General"

        Number d _ ->
            "Number (" ++ String.fromInt d ++ "dp)"

        Currency sym _ ->
            "Currency " ++ sym

        Percent _ ->
            "Percent"

        Scientific _ ->
            "Scientific"

        DateTime code ->
            "Date " ++ code

        Text ->
            "Text"

        Custom code ->
            code


{-| The CSS class describing default horizontal alignment for a value: numbers/dates
align right, text left, booleans/errors centre — exactly as a spreadsheet does. -}
alignmentClass : Value -> String
alignmentClass value =
    case value of
        VNumber _ ->
            "ss-align-right"

        VText _ ->
            "ss-align-left"

        VBool _ ->
            "ss-align-center"

        VError _ ->
            "ss-align-center"

        VEmpty ->
            "ss-align-left"


{-| Render a value for display under a format. -}
format : Format -> Value -> String
format fmt value =
    case value of
        VError e ->
            Value.errorText e

        _ ->
            case fmt of
                General ->
                    Value.toText value

                Text ->
                    Value.toText value

                Number decimals thousands ->
                    numericOr value (formatFixedNumber decimals thousands False "" "" value)

                Currency symbol decimals ->
                    numericOr value (formatFixedNumber decimals True False symbol "" value)

                Percent decimals ->
                    numericOr value (formatFixedNumber decimals False True "" "" value)

                Scientific decimals ->
                    numericOr value (formatScientific decimals value)

                DateTime code ->
                    numericOr value (applyTextFormat code value)

                Custom code ->
                    applyTextFormat code value


{-| Fall back to the plain text rendering when the value isn't numeric (e.g. a text cell
that happens to carry a Number format). -}
numericOr : Value -> String -> String
numericOr value formatted =
    case Value.toNumber value of
        Ok _ ->
            formatted

        Err _ ->
            Value.toText value


formatScientific : Int -> Value -> String
formatScientific decimals value =
    case Value.toNumber value of
        Ok 0 ->
            padFixed decimals "0" ++ "E+00"

        Ok n ->
            let
                expo =
                    floor (logBase 10 (abs n))

                mantissa =
                    n / (10 ^ toFloat expo)

                sign =
                    if expo < 0 then
                        "-"

                    else
                        "+"

                expStr =
                    String.padLeft 2 '0' (String.fromInt (abs expo))
            in
            formatFixedNumber decimals False False "" "" (VNumber mantissa) ++ "E" ++ sign ++ expStr

        Err _ ->
            Value.toText value


{-| Format a number with fixed decimals, optional grouping, percent scaling, prefix and
suffix. The integer rounding is done in fixed-point so it doesn't drift on `.5`. -}
formatFixedNumber : Int -> Bool -> Bool -> String -> String -> Value -> String
formatFixedNumber decimals thousands percent prefix suffix value =
    case Value.toNumber value of
        Err _ ->
            Value.toText value

        Ok raw ->
            let
                scaled =
                    if percent then
                        raw * 100

                    else
                        raw

                negative =
                    scaled < 0

                ( intPart, fracPart ) =
                    fixedParts decimals (abs scaled)

                grouped =
                    if thousands then
                        groupThousands intPart

                    else
                        intPart

                body =
                    if decimals > 0 then
                        grouped ++ "." ++ fracPart

                    else
                        grouped

                signStr =
                    if negative then
                        "-"

                    else
                        ""
            in
            signStr ++ prefix ++ body ++ suffix ++ percentSuffix percent


percentSuffix : Bool -> String
percentSuffix percent =
    if percent then
        "%"

    else
        ""


{-| Split a non-negative float into (integer-digits, fractional-digits) rounded to
`decimals` places. -}
fixedParts : Int -> Float -> ( String, String )
fixedParts decimals x =
    let
        factor =
            10 ^ decimals

        scaledRounded =
            roundFloat (x * toFloat factor)

        intValue =
            scaledRounded // factor

        fracValue =
            scaledRounded - intValue * factor
    in
    ( String.fromInt intValue
    , if decimals > 0 then
        String.padLeft decimals '0' (String.fromInt fracValue)

      else
        ""
    )


roundFloat : Float -> Int
roundFloat x =
    floor (x + 0.5)


padFixed : Int -> String -> String
padFixed decimals intStr =
    if decimals > 0 then
        intStr ++ "." ++ String.repeat decimals "0"

    else
        intStr


groupThousands : String -> String
groupThousands digits =
    -- Reverse, chunk into 3s from the left, comma-join, reverse back:
    -- "1234567" → "7654321" → ["765","432","1"] → "765,432,1" → "1,234,567".
    digits
        |> String.reverse
        |> chunk3
        |> String.join ","
        |> String.reverse


chunk3 : String -> List String
chunk3 s =
    if String.length s <= 3 then
        [ s ]

    else
        String.left 3 s :: chunk3 (String.dropLeft 3 s)



-- FORMAT-CODE INTERPRETER ----------------------------------------------------


{-| Interpret an Excel/Sheets-style format code for a value. Recognises date/time codes
(containing `y`/`d`/`h`/`s` or `mmm`) and numeric codes (`#`, `0`, `,`, `.`, `%`, a
leading currency symbol). Backs both `Custom` formats and `TEXT()`. -}
applyTextFormat : String -> Value -> String
applyTextFormat code value =
    case value of
        VError e ->
            Value.errorText e

        _ ->
            if looksLikeDate code then
                case Value.toNumber value of
                    Ok serial ->
                        formatDateCode code (roundFloat serial)

                    Err _ ->
                        Value.toText value

            else
                applyNumericCode code value


looksLikeDate : String -> Bool
looksLikeDate code =
    let
        lower =
            String.toLower code
    in
    String.contains "y" lower
        || String.contains "d" lower
        || String.contains "h" lower
        || String.contains "s" lower
        || String.contains "mmm" lower


applyNumericCode : String -> Value -> String
applyNumericCode code value =
    let
        percent =
            String.contains "%" code

        thousands =
            String.contains ",#" code || String.contains ",0" code || String.contains "#," code

        decimals =
            decimalsOf code

        prefix =
            leadingSymbol code
    in
    formatFixedNumber decimals thousands percent prefix "" value


decimalsOf : String -> Int
decimalsOf code =
    case String.split "." code of
        _ :: frac :: _ ->
            String.length (String.filter (\c -> c == '0' || c == '#') frac)

        _ ->
            0


leadingSymbol : String -> String
leadingSymbol code =
    String.toList code
        |> takeWhileSymbol
        |> String.fromList


takeWhileSymbol : List Char -> List Char
takeWhileSymbol chars =
    case chars of
        c :: rest ->
            if c == '$' || c == '€' || c == '£' || c == '¥' then
                c :: takeWhileSymbol rest

            else
                []

        [] ->
            []



-- DATE FORMATTING ------------------------------------------------------------


formatDateCode : String -> Int -> String
formatDateCode code serial =
    let
        ( y, m, d ) =
            serialToDate serial

        weekday =
            modBy 7 (serial - 1 + 1)
    in
    replaceDateTokens (String.toList (String.toLower code)) y m d weekday ""


replaceDateTokens : List Char -> Int -> Int -> Int -> Int -> String -> String
replaceDateTokens chars y m d wd acc =
    case chars of
        [] ->
            acc

        'y' :: 'y' :: 'y' :: 'y' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.padLeft 4 '0' (String.fromInt y))

        'y' :: 'y' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.padLeft 2 '0' (String.fromInt (modBy 100 y)))

        'm' :: 'm' :: 'm' :: 'm' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ monthName m)

        'm' :: 'm' :: 'm' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.left 3 (monthName m))

        'm' :: 'm' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.padLeft 2 '0' (String.fromInt m))

        'm' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.fromInt m)

        'd' :: 'd' :: 'd' :: 'd' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ dayName wd)

        'd' :: 'd' :: 'd' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.left 3 (dayName wd))

        'd' :: 'd' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.padLeft 2 '0' (String.fromInt d))

        'd' :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.fromInt d)

        c :: rest ->
            replaceDateTokens rest y m d wd (acc ++ String.fromChar c)


monthName : Int -> String
monthName m =
    case m of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        12 ->
            "December"

        _ ->
            "?"


dayName : Int -> String
dayName wd =
    case wd of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "?"


{-| Inverse of the date serial model in `Functions` (kept in sync: DATE(1900,1,1)=1). -}
serialToDate : Int -> ( Int, Int, Int )
serialToDate serial =
    civilFromDays (serial + epoch)


epoch : Int
epoch =
    daysFromCivil 1899 12 31


daysFromCivil : Int -> Int -> Int -> Int
daysFromCivil y0 m d =
    let
        y =
            if m <= 2 then
                y0 - 1

            else
                y0

        era =
            (if y >= 0 then
                y

             else
                y - 399
            )
                // 400

        yoe =
            y - era * 400

        doy =
            (153 * (if m > 2 then m - 3 else m + 9) + 2) // 5 + d - 1

        doe =
            yoe * 365 + yoe // 4 - yoe // 100 + doy
    in
    era * 146097 + doe - 719468


civilFromDays : Int -> ( Int, Int, Int )
civilFromDays z0 =
    let
        z =
            z0 + 719468

        era =
            (if z >= 0 then
                z

             else
                z - 146096
            )
                // 146097

        doe =
            z - era * 146097

        yoe =
            (doe - doe // 1460 + doe // 36524 - doe // 146096) // 365

        y =
            yoe + era * 400

        doy =
            doe - (365 * yoe + yoe // 4 - yoe // 100)

        mp =
            (5 * doy + 2) // 153

        d =
            doy - (153 * mp + 2) // 5 + 1

        m =
            if mp < 10 then
                mp + 3

            else
                mp - 9
    in
    ( if m <= 2 then
        y + 1

      else
        y
    , m
    , d
    )
