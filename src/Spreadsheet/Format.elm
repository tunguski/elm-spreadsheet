module Spreadsheet.Format exposing
    ( Format(..)
    , default
    , format
    , applyTextFormat
    , colorOf
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

        VText s ->
            case sectionAt 3 (String.split ";" code) of
                Just textSec ->
                    renderTextSection (stripColors textSec) s

                Nothing ->
                    s

        VBool _ ->
            Value.toText value

        VEmpty ->
            ""

        VNumber n ->
            let
                ( section, useAbs ) =
                    pickNumberSection code n

                clean =
                    stripColors section

                vNum =
                    if useAbs then
                        VNumber (abs n)

                    else
                        VNumber n
            in
            if looksLikeDate clean then
                formatDateCode clean (roundFloat n)

            else if not (String.any isDigitPlaceholder clean) then
                cleanLiteral clean

            else if String.contains "/" clean then
                applyFraction clean vNum

            else
                applyNumericCodeScaled clean vNum


{-| The colour (`#rrggbb` / name) a format code asks for, given a value — e.g. `[Red]`
in the section that applies to a negative number. The sheet uses this to tint a cell. -}
colorOf : String -> Value -> Maybe String
colorOf code value =
    let
        section =
            case value of
                VNumber n ->
                    Tuple.first (pickNumberSection code n)

                VText _ ->
                    Maybe.withDefault "" (sectionAt 3 (String.split ";" code))

                _ ->
                    code
    in
    bracketColor section


{-| Choose the section of a multi-part code (`pos;neg;zero;text`) for a number, and whether
that section's value should be rendered as an absolute value (true for an explicit negative
section, which supplies its own sign). -}
pickNumberSection : String -> Float -> ( String, Bool )
pickNumberSection code n =
    let
        sections =
            String.split ";" code

        count =
            List.length sections
    in
    if count <= 1 then
        ( code, False )

    else if n < 0 && count >= 2 then
        ( sectionOr 1 sections, True )

    else if n == 0 && count >= 3 then
        ( sectionOr 2 sections, False )

    else
        ( sectionOr 0 sections, False )


sectionAt : Int -> List String -> Maybe String
sectionAt i sections =
    List.head (List.drop i sections)


sectionOr : Int -> List String -> String
sectionOr i sections =
    Maybe.withDefault (Maybe.withDefault "" (List.head sections)) (sectionAt i sections)


renderTextSection : String -> String -> String
renderTextSection section s =
    String.replace "@" s (String.replace "\"" "" section)


{-| Drop bracketed directives like `[Red]` / `[$-409]` from a section. -}
stripColors : String -> String
stripColors code =
    case String.indexes "[" code of
        i :: _ ->
            case String.indexes "]" (String.dropLeft i code) of
                j :: _ ->
                    stripColors (String.left i code ++ String.dropLeft (i + j + 1) code)

                [] ->
                    code

        [] ->
            code


bracketColor : String -> Maybe String
bracketColor code =
    namedColor (String.toLower code)


namedColor : String -> Maybe String
namedColor lower =
    if String.contains "[red]" lower then
        Just "#d93025"

    else if String.contains "[blue]" lower then
        Just "#1a73e8"

    else if String.contains "[green]" lower then
        Just "#188038"

    else if String.contains "[magenta]" lower then
        Just "#a142f4"

    else
        Nothing


{-| Apply trailing-comma scaling (each trailing comma divides by 1000) then the numeric code. -}
applyNumericCodeScaled : String -> Value -> String
applyNumericCodeScaled code value =
    let
        commas =
            trailingCommas code

        scaled =
            case value of
                VNumber n ->
                    VNumber (n / (1000 ^ toFloat commas))

                _ ->
                    value
    in
    applyNumericCode (dropTrailingCommas code) scaled


trailingCommas : String -> Int
trailingCommas code =
    leadingCommas (String.reverse code)


leadingCommas : String -> Int
leadingCommas s =
    case String.uncons s of
        Just ( ',', rest ) ->
            1 + leadingCommas rest

        _ ->
            0


dropTrailingCommas : String -> String
dropTrailingCommas code =
    String.reverse (String.dropLeft (trailingCommas code) (String.reverse code))


{-| Render a number as a (mixed) fraction, with the maximum denominator implied by the
count of `?` after the `/` (`# ?/?` → up to 9, `?/??` → up to 99). -}
applyFraction : String -> Value -> String
applyFraction code value =
    case value of
        VNumber n ->
            let
                maxDen =
                    fractionMaxDen code

                whole =
                    truncate (abs n)

                ( num, den ) =
                    bestFraction (abs n - toFloat whole) maxDen

                sign =
                    if n < 0 then
                        "-"

                    else
                        ""
            in
            if num == 0 then
                sign ++ String.fromInt whole

            else if whole == 0 then
                sign ++ String.fromInt num ++ "/" ++ String.fromInt den

            else
                sign ++ String.fromInt whole ++ " " ++ String.fromInt num ++ "/" ++ String.fromInt den

        _ ->
            Value.toText value


fractionMaxDen : String -> Int
fractionMaxDen code =
    case String.split "/" code of
        _ :: after :: _ ->
            let
                digits =
                    String.length (String.filter (\c -> c == '?' || c == '0' || c == '#') after)
            in
            if digits <= 0 then
                9

            else
                10 ^ digits - 1

        _ ->
            9


bestFraction : Float -> Int -> ( Int, Int )
bestFraction frac maxDen =
    let
        ( bn, bd, _ ) =
            List.foldl
                (\d ( accN, accD, accErr ) ->
                    let
                        nn =
                            round (frac * toFloat d)

                        err =
                            abs (frac - toFloat nn / toFloat d)
                    in
                    if err < accErr then
                        ( nn, d, err )

                    else
                        ( accN, accD, accErr )
                )
                ( 0, 1, 2 )
                (List.range 1 (max 1 maxDen))
    in
    reduceFraction bn bd


reduceFraction : Int -> Int -> ( Int, Int )
reduceFraction n d =
    let
        g =
            gcdInt n d
    in
    if g == 0 then
        ( n, d )

    else
        ( n // g, d // g )


gcdInt : Int -> Int -> Int
gcdInt a b =
    if b == 0 then
        abs a

    else
        gcdInt b (modBy b a)


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
    in
    formatFixedNumber decimals thousands percent (literalPrefix code) (literalSuffix code) value


{-| Literal text before the first digit placeholder (a currency symbol, an opening paren,
a word…), with quotes/backslashes removed. -}
literalPrefix : String -> String
literalPrefix code =
    cleanLiteral (String.fromList (takeWhileNonDigit (String.toList code)))


{-| Literal text after the last digit placeholder. -}
literalSuffix : String -> String
literalSuffix code =
    cleanLiteral (String.reverse (String.fromList (takeWhileNonDigit (List.reverse (String.toList code)))))


takeWhileNonDigit : List Char -> List Char
takeWhileNonDigit chars =
    case chars of
        c :: rest ->
            if isNumberToken c then
                []

            else
                c :: takeWhileNonDigit rest

        [] ->
            []


isNumberToken : Char -> Bool
isNumberToken c =
    c == '#' || c == '0' || c == '?' || c == '.' || c == ',' || c == '%'


isDigitPlaceholder : Char -> Bool
isDigitPlaceholder c =
    c == '#' || c == '0' || c == '?'


cleanLiteral : String -> String
cleanLiteral s =
    s |> String.replace "\"" "" |> String.replace "\\" ""


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
