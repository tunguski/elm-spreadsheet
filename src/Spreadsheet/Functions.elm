module Spreadsheet.Functions exposing
    ( Arg(..)
    , call
    , isKnown
    , flatten
    , matrixOf
    , firstValue
    , matchCriteria
    )

{-| The built-in function library — the part of a spreadsheet engine that users think
of as "the functions". Each entry maps a (already-evaluated) argument list to a result
`Value`. Laziness-requiring forms (IF, IFERROR, CHOOSE, …) and reference-aware forms
(ROW, COLUMN) are handled one level up in `Spreadsheet.Eval`; everything strict lives
here so the table is a pure `String -> List Arg -> Value`.

Arguments arrive as `Arg`, which preserves whether each came from a single value or a
2-D range — `INDEX`, `VLOOKUP`, `MATCH` need the rectangle, while `SUM` just flattens.

@docs Arg, call, isKnown, flatten, matrixOf, firstValue, matchCriteria

-}

import Bitwise
import Spreadsheet.Regex as Regex
import Spreadsheet.Value as Value exposing (Error(..), Value(..))


{-| An evaluated argument: either a scalar value or a 2-D block of values (from a range
reference or an array). -}
type Arg
    = Scalar Value
    | Matrix (List (List Value))


{-| Flatten every argument to a flat list of values (row-major), the form most
aggregate functions want. -}
flatten : List Arg -> List Value
flatten args =
    List.concatMap argValues args


argValues : Arg -> List Value
argValues arg =
    case arg of
        Scalar v ->
            [ v ]

        Matrix rows ->
            List.concat rows


{-| View any argument as a 2-D matrix (a scalar becomes 1×1). -}
matrixOf : Arg -> List (List Value)
matrixOf arg =
    case arg of
        Scalar v ->
            [ [ v ] ]

        Matrix rows ->
            rows


{-| The first value of an argument — a range used in scalar position collapses to its
top-left cell (a deliberate simplification of Excel's implicit intersection). -}
firstValue : Arg -> Value
firstValue arg =
    case arg of
        Scalar v ->
            v

        Matrix rows ->
            case rows of
                (v :: _) :: _ ->
                    v

                _ ->
                    VEmpty


{-| Does the library know this (upper-cased) function name? Used to surface `#NAME?`
distinctly from a runtime argument error. Includes the lazy/reference forms handled in
`Spreadsheet.Eval`. -}
isKnown : String -> Bool
isKnown name =
    List.member name knownNames


knownNames : List String
knownNames =
    [ -- math / aggregate
      "SUM", "SUMSQ", "PRODUCT", "AVERAGE", "AVERAGEA", "COUNT", "COUNTA"
    , "COUNTBLANK", "MAX", "MIN", "MEDIAN", "MODE", "STDEV", "STDEVP", "VAR"
    , "VARP", "LARGE", "SMALL", "ABS", "SIGN", "SQRT", "POWER", "EXP", "LN"
    , "LOG10", "LOG", "MOD", "QUOTIENT", "INT", "TRUNC", "ROUND", "ROUNDUP"
    , "ROUNDDOWN", "MROUND", "CEILING", "FLOOR", "PI", "FACT", "COMBIN", "GCD"
    , "LCM"

    -- trig
    , "SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN", "ATAN2", "SINH", "COSH"
    , "TANH", "DEGREES", "RADIANS"

    -- conditional aggregates
    , "COUNTIF", "SUMIF", "AVERAGEIF"

    -- logical
    , "AND", "OR", "XOR", "NOT", "TRUE", "FALSE", "IF", "IFS", "IFERROR"
    , "IFNA", "SWITCH"

    -- information
    , "ISNUMBER", "ISTEXT", "ISBLANK", "ISLOGICAL", "ISERROR", "ISERR"
    , "ISNA", "ISEVEN", "ISODD", "N", "NA", "TYPE"

    -- text
    , "CONCAT", "CONCATENATE", "TEXTJOIN", "LEN", "LEFT", "RIGHT", "MID"
    , "UPPER", "LOWER", "TRIM", "CLEAN", "PROPER", "REPT", "REPLACE"
    , "SUBSTITUTE", "FIND", "SEARCH", "EXACT", "VALUE", "CHAR", "CODE", "T"
    , "TEXT", "TEXTBEFORE", "TEXTAFTER", "ARRAYTOTEXT", "VALUETOTEXT", "HYPERLINK"

    -- functional / structured / audit
    , "XMATCH", "ERROR.TYPE"

    -- regex
    , "REGEXTEST", "REGEXEXTRACT", "REGEXREPLACE"

    -- database (criteria-range queries)
    , "DSUM", "DCOUNT", "DCOUNTA", "DAVERAGE", "DMAX", "DMIN", "DGET", "DPRODUCT"

    -- statistics & aggregation
    , "AGGREGATE", "PERCENTRANK", "TRIMMEAN", "COVARIANCE.P", "STANDARDIZE"

    -- lookup / reference
    , "VLOOKUP", "HLOOKUP", "INDEX", "MATCH", "CHOOSE", "ROWS", "COLUMNS"
    , "ROW", "COLUMN"

    -- date
    , "DATE", "YEAR", "MONTH", "DAY", "WEEKDAY", "DAYS", "DATEVALUE"

    -- finance & analysis
    , "SUMPRODUCT", "SUMIFS", "COUNTIFS", "AVERAGEIFS", "MINIFS", "MAXIFS"
    , "SUBTOTAL", "PERCENTILE", "QUARTILE", "RANK", "XLOOKUP"
    , "PMT", "FV", "PV", "NPV", "IRR", "NPER"

    -- statistics & forecasting
    , "CORREL", "SLOPE", "INTERCEPT", "RSQ", "FORECAST", "GEOMEAN", "HARMEAN"
    , "DEVSQ"

    -- financial depth
    , "RATE", "IPMT", "PPMT", "MIRR", "XNPV", "XIRR", "SLN", "DDB", "CUMIPMT"

    -- statistical distributions
    , "NORM.DIST", "NORM.S.DIST", "NORM.INV", "NORM.S.INV", "BINOM.DIST"
    , "POISSON.DIST", "EXPON.DIST"

    -- engineering, base & unit conversion
    , "MDETERM", "DEC2BIN", "BIN2DEC", "DEC2HEX", "HEX2DEC", "DEC2OCT", "OCT2DEC"
    , "BITAND", "BITOR", "BITXOR", "BITLSHIFT", "BITRSHIFT", "CONVERT"

    -- date & time
    , "TIME", "HOUR", "MINUTE", "SECOND", "EDATE", "EOMONTH", "WORKDAY"
    , "NETWORKDAYS", "YEARFRAC"

    -- dynamic references
    , "OFFSET", "INDIRECT", "ADDRESS"
    ]



-- DISPATCH -------------------------------------------------------------------


{-| Evaluate a strict built-in. Unknown names yield `#NAME?`. -}
call : String -> List Arg -> Value
call name args =
    let
        vals =
            flatten args
    in
    case name of
        -- Math / aggregate ----------------------------------------------------
        "SUM" ->
            numAgg (List.foldl (+) 0) args

        "SUMSQ" ->
            numAgg (List.foldl (\x acc -> acc + x * x) 0) args

        "PRODUCT" ->
            numAgg (List.foldl (*) 1) args

        "AVERAGE" ->
            numAggNonEmpty average args

        "AVERAGEA" ->
            withNumbersA vals average

        "COUNT" ->
            VNumber (toFloat (List.length (numbersOnly vals)))

        "COUNTA" ->
            VNumber (toFloat (List.length (List.filter (\v -> v /= VEmpty) vals)))

        "COUNTBLANK" ->
            VNumber (toFloat (List.length (List.filter isBlank vals)))

        "MAX" ->
            numAggNonEmptyOr 0 (List.foldl Basics.max -inf) args

        "MIN" ->
            numAggNonEmptyOr 0 (List.foldl Basics.min inf) args

        "MEDIAN" ->
            statAgg median args

        "MODE" ->
            statAgg mode args

        "STDEV" ->
            statAgg (stdev True) args

        "STDEVP" ->
            statAgg (stdev False) args

        "VAR" ->
            statAgg (variance True) args

        "VARP" ->
            statAgg (variance False) args

        "LARGE" ->
            nthOrdered True args

        "SMALL" ->
            nthOrdered False args

        "ABS" ->
            unaryNum abs args

        "SIGN" ->
            unaryNum (\x -> toFloat (sign x)) args

        "SQRT" ->
            unaryNumChecked (\x -> if x < 0 then Err NumErr else Ok (sqrt x)) args

        "POWER" ->
            binNum (\a b -> a ^ b) args

        "EXP" ->
            unaryNum (\x -> e ^ x) args

        "LN" ->
            unaryNumChecked (\x -> if x <= 0 then Err NumErr else Ok (logBase e x)) args

        "LOG10" ->
            unaryNumChecked (\x -> if x <= 0 then Err NumErr else Ok (logBase 10 x)) args

        "LOG" ->
            logFn args

        "MOD" ->
            binNumChecked (\a b -> if b == 0 then Err DivZero else Ok (a - b * toFloat (floor (a / b)))) args

        "QUOTIENT" ->
            binNumChecked (\a b -> if b == 0 then Err DivZero else Ok (toFloat (truncate (a / b)))) args

        "INT" ->
            unaryNum (\x -> toFloat (floor x)) args

        "TRUNC" ->
            truncFn args

        "ROUND" ->
            roundFn roundHalfAway args

        "ROUNDUP" ->
            roundFn (\x -> ceiling (x - 1.0e-9)) args

        "ROUNDDOWN" ->
            roundFn (\x -> floor (x + 1.0e-9)) args

        "MROUND" ->
            binNumChecked (\x m -> if m == 0 then Ok 0 else Ok (toFloat (roundHalfAway (x / m)) * m)) args

        "CEILING" ->
            ceilingFloor (\x m -> toFloat (ceiling (x / m)) * m) args

        "FLOOR" ->
            ceilingFloor (\x m -> toFloat (floor (x / m)) * m) args

        "PI" ->
            VNumber pi

        "FACT" ->
            unaryNumChecked factorial args

        "COMBIN" ->
            combin args

        "GCD" ->
            intReduce gcdI 0 vals

        "LCM" ->
            intReduce lcmI 1 vals

        -- Trig ----------------------------------------------------------------
        "SIN" ->
            unaryNum sin args

        "COS" ->
            unaryNum cos args

        "TAN" ->
            unaryNum tan args

        "ASIN" ->
            unaryNum asin args

        "ACOS" ->
            unaryNum acos args

        "ATAN" ->
            unaryNum atan args

        "ATAN2" ->
            binNum (\x y -> atan2 y x) args

        "SINH" ->
            unaryNum (\x -> (e ^ x - e ^ -x) / 2) args

        "COSH" ->
            unaryNum (\x -> (e ^ x + e ^ -x) / 2) args

        "TANH" ->
            unaryNum (\x -> (e ^ x - e ^ -x) / (e ^ x + e ^ -x)) args

        "DEGREES" ->
            unaryNum (\x -> x * 180 / pi) args

        "RADIANS" ->
            unaryNum (\x -> x * pi / 180) args

        -- Conditional aggregates ---------------------------------------------
        "COUNTIF" ->
            countIf args

        "SUMIF" ->
            sumIf args

        "AVERAGEIF" ->
            averageIf args

        -- Logical (strict) ----------------------------------------------------
        "AND" ->
            boolAgg (List.all identity) args

        "OR" ->
            boolAgg (List.any identity) args

        "XOR" ->
            boolAgg (\bs -> modBy 2 (List.length (List.filter identity bs)) == 1) args

        "NOT" ->
            case vals of
                [ v ] ->
                    case Value.toBool v of
                        Ok b ->
                            VBool (not b)

                        Err er ->
                            VError er

                _ ->
                    VError ValueErr

        "TRUE" ->
            VBool True

        "FALSE" ->
            VBool False

        -- Information ---------------------------------------------------------
        "ISNUMBER" ->
            predicate isNumber vals

        "ISTEXT" ->
            predicate isText vals

        "ISBLANK" ->
            predicate isBlank vals

        "ISLOGICAL" ->
            predicate isLogical vals

        "ISERROR" ->
            predicate Value.isError vals

        "ISERR" ->
            predicate isErrNotNA vals

        "ISNA" ->
            predicate isNA vals

        "ISEVEN" ->
            intPredicate (\n -> modBy 2 n == 0) args

        "ISODD" ->
            intPredicate (\n -> modBy 2 n == 1) args

        "N" ->
            case vals of
                [ v ] ->
                    case Value.toNumber v of
                        Ok n ->
                            VNumber n

                        Err _ ->
                            VNumber 0

                _ ->
                    VError ValueErr

        "NA" ->
            VError NA

        "TYPE" ->
            case vals of
                [ v ] ->
                    VNumber (toFloat (typeCode v))

                _ ->
                    VError ValueErr

        -- Text ----------------------------------------------------------------
        "CONCAT" ->
            VText (String.concat (List.map Value.toText vals))

        "CONCATENATE" ->
            VText (String.concat (List.map Value.toText vals))

        "TEXTJOIN" ->
            textJoin args

        "TEXTBEFORE" ->
            textPart True vals

        "TEXTAFTER" ->
            textPart False vals

        "ARRAYTOTEXT" ->
            case args of
                a :: _ ->
                    VText (String.join ", " (List.map Value.toText (flatten [ a ])))

                [] ->
                    VError ValueErr

        "VALUETOTEXT" ->
            case vals of
                v :: rest ->
                    let
                        strict =
                            case rest of
                                f :: _ ->
                                    case Value.toNumber f of
                                        Ok n ->
                                            n >= 0.5

                                        Err _ ->
                                            False

                                [] ->
                                    False
                    in
                    case v of
                        VText s ->
                            if strict then
                                VText ("\"" ++ s ++ "\"")

                            else
                                VText s

                        _ ->
                            VText (Value.toText v)

                [] ->
                    VError ValueErr

        "HYPERLINK" ->
            case vals of
                _ :: label :: _ ->
                    VText (Value.toText label)

                url :: [] ->
                    VText (Value.toText url)

                [] ->
                    VError ValueErr

        "XMATCH" ->
            xmatchFn args

        "ERROR.TYPE" ->
            case vals of
                [ VError er ] ->
                    VNumber (toFloat (errorCode er))

                [ _ ] ->
                    VError NA

                _ ->
                    VError ValueErr

        -- Regex --------------------------------------------------------------
        "REGEXTEST" ->
            case vals of
                text :: pat :: rest ->
                    VBool (Regex.test (icFlag rest 0) (Value.toText pat) (Value.toText text))

                _ ->
                    VError ValueErr

        "REGEXEXTRACT" ->
            case vals of
                text :: pat :: rest ->
                    case Regex.extract (icFlag rest 0) (Value.toText pat) (Value.toText text) of
                        Just s ->
                            VText s

                        Nothing ->
                            VError NA

                _ ->
                    VError ValueErr

        "REGEXREPLACE" ->
            case vals of
                text :: pat :: repl :: rest ->
                    VText (Regex.replace (icFlag rest 0) (Value.toText pat) (Value.toText repl) (Value.toText text))

                _ ->
                    VError ValueErr

        -- Database functions -------------------------------------------------
        "DSUM" ->
            dQuery (\xs -> VNumber (List.sum (numbersOnly xs))) args

        "DPRODUCT" ->
            dQuery (\xs -> VNumber (List.foldl (*) 1 (numbersOnly xs))) args

        "DCOUNT" ->
            dQuery (\xs -> VNumber (toFloat (List.length (numbersOnly xs)))) args

        "DCOUNTA" ->
            dQuery (\xs -> VNumber (toFloat (List.length (List.filter (\v -> v /= VEmpty) xs)))) args

        "DAVERAGE" ->
            dQuery dAverage args

        "DMAX" ->
            dQuery (dExtreme List.maximum) args

        "DMIN" ->
            dQuery (dExtreme List.minimum) args

        "DGET" ->
            dQuery dGet args

        -- Aggregation & statistics -------------------------------------------
        "AGGREGATE" ->
            aggregateFn args

        "PERCENTRANK" ->
            percentRankFn args

        "TRIMMEAN" ->
            trimMeanFn args

        "COVARIANCE.P" ->
            covarianceFn args

        "STANDARDIZE" ->
            case vals of
                [ x, m, sd ] ->
                    case ( Value.toNumber x, Value.toNumber m, Value.toNumber sd ) of
                        ( Ok xv, Ok mv, Ok sdv ) ->
                            if sdv == 0 then
                                VError DivZero

                            else
                                VNumber ((xv - mv) / sdv)

                        _ ->
                            VError ValueErr

                _ ->
                    VError ValueErr

        "LEN" ->
            case vals of
                [ v ] ->
                    VNumber (toFloat (String.length (Value.toText v)))

                _ ->
                    VError ValueErr

        "LEFT" ->
            textSlice (\s n -> String.left n s) args

        "RIGHT" ->
            textSlice (\s n -> String.right n s) args

        "MID" ->
            midFn args

        "UPPER" ->
            unaryText String.toUpper vals

        "LOWER" ->
            unaryText String.toLower vals

        "TRIM" ->
            unaryText (collapseSpaces << String.trim) vals

        "CLEAN" ->
            unaryText (String.filter (\c -> Char.toCode c >= 32)) vals

        "PROPER" ->
            unaryText properCase vals

        "REPT" ->
            reptFn args

        "REPLACE" ->
            replaceFn args

        "SUBSTITUTE" ->
            substituteFn args

        "FIND" ->
            findFn True args

        "SEARCH" ->
            findFn False args

        "EXACT" ->
            case vals of
                [ a, b ] ->
                    VBool (Value.toText a == Value.toText b)

                _ ->
                    VError ValueErr

        "VALUE" ->
            case vals of
                [ v ] ->
                    case Value.toNumber v of
                        Ok n ->
                            VNumber n

                        Err er ->
                            VError er

                _ ->
                    VError ValueErr

        "CHAR" ->
            unaryNumChecked (\x -> Ok x) args
                |> mapNumberToChar

        "CODE" ->
            case vals of
                [ v ] ->
                    case String.toList (Value.toText v) of
                        c :: _ ->
                            VNumber (toFloat (Char.toCode c))

                        [] ->
                            VError ValueErr

                _ ->
                    VError ValueErr

        "T" ->
            case vals of
                [ VText s ] ->
                    VText s

                [ _ ] ->
                    VText ""

                _ ->
                    VError ValueErr

        -- Lookup --------------------------------------------------------------
        "VLOOKUP" ->
            vlookup args

        "HLOOKUP" ->
            hlookup args

        "INDEX" ->
            indexFn args

        "MATCH" ->
            matchFn args

        "ROWS" ->
            case args of
                a :: _ ->
                    VNumber (toFloat (List.length (matrixOf a)))

                _ ->
                    VError ValueErr

        "COLUMNS" ->
            case args of
                a :: _ ->
                    case matrixOf a of
                        row :: _ ->
                            VNumber (toFloat (List.length row))

                        [] ->
                            VNumber 0

                _ ->
                    VError ValueErr

        -- Finance & analysis --------------------------------------------------
        "SUMPRODUCT" ->
            sumProduct args

        "SUMIFS" ->
            sumIfs args

        "COUNTIFS" ->
            countIfs args

        "AVERAGEIFS" ->
            averageIfs args

        "MINIFS" ->
            minMaxIfs True args

        "MAXIFS" ->
            minMaxIfs False args

        "SUBTOTAL" ->
            subtotal args

        "PERCENTILE" ->
            percentileFn args

        "QUARTILE" ->
            quartileFn args

        "RANK" ->
            rankFn args

        "XLOOKUP" ->
            xlookup args

        "PMT" ->
            financeFn pmt args

        "FV" ->
            financeFn fv args

        "PV" ->
            financeFn pv args

        "NPER" ->
            financeFn nper args

        "NPV" ->
            npvFn args

        "IRR" ->
            irrFn args

        -- Financial depth -----------------------------------------------------
        "RATE" ->
            rateFn args

        "IPMT" ->
            ipmtFn args

        "PPMT" ->
            ppmtFn args

        "MIRR" ->
            mirrFn args

        "XNPV" ->
            xnpvFn args

        "XIRR" ->
            xirrFn args

        "SLN" ->
            case ( sc 0 args, sc 1 args, sc 2 args ) of
                ( Just cost, Just salvage, Just life ) ->
                    if life == 0 then
                        VError DivZero

                    else
                        VNumber ((cost - salvage) / life)

                _ ->
                    VError ValueErr

        "DDB" ->
            ddbFn args

        "CUMIPMT" ->
            cumipmtFn args

        -- Statistical distributions -------------------------------------------
        "NORM.S.DIST" ->
            case ( sc 0 args, boolArg 1 args ) of
                ( Just z, cumulative ) ->
                    VNumber (normSDist z cumulative)

                _ ->
                    VError ValueErr

        "NORM.DIST" ->
            case ( sc 0 args, sc 1 args, sc 2 args ) of
                ( Just x, Just mean, Just sd ) ->
                    if sd <= 0 then
                        VError NumErr

                    else
                        VNumber (normSDist ((x - mean) / sd) (boolArg 3 args) / scaleFor (boolArg 3 args) sd)

                _ ->
                    VError ValueErr

        "NORM.S.INV" ->
            case sc 0 args of
                Just p ->
                    probResult (normSInv p)

                _ ->
                    VError ValueErr

        "NORM.INV" ->
            case ( sc 0 args, sc 1 args, sc 2 args ) of
                ( Just p, Just mean, Just sd ) ->
                    probResult (mean + sd * normSInv p)

                _ ->
                    VError ValueErr

        "BINOM.DIST" ->
            case ( sc 0 args, sc 1 args, sc 2 args ) of
                ( Just k, Just n, Just p ) ->
                    VNumber (binomDist (round k) (round n) p (boolArg 3 args))

                _ ->
                    VError ValueErr

        "POISSON.DIST" ->
            case ( sc 0 args, sc 1 args ) of
                ( Just k, Just mean ) ->
                    VNumber (poissonDist (round k) mean (boolArg 2 args))

                _ ->
                    VError ValueErr

        "EXPON.DIST" ->
            case ( sc 0 args, sc 1 args ) of
                ( Just x, Just lambda ) ->
                    if lambda <= 0 || x < 0 then
                        VError NumErr

                    else if boolArg 2 args then
                        VNumber (1 - e ^ (-lambda * x))

                    else
                        VNumber (lambda * e ^ (-lambda * x))

                _ ->
                    VError ValueErr

        -- Engineering, base & unit conversion ---------------------------------
        "MDETERM" ->
            case args of
                a :: _ ->
                    determinant (numbersMatrix (matrixOf a))

                [] ->
                    VError ValueErr

        "DEC2BIN" ->
            baseFromDec 2 args

        "DEC2OCT" ->
            baseFromDec 8 args

        "DEC2HEX" ->
            baseFromDec 16 args

        "BIN2DEC" ->
            baseToDec 2 args

        "OCT2DEC" ->
            baseToDec 8 args

        "HEX2DEC" ->
            baseToDec 16 args

        "BITAND" ->
            bitOp Bitwise.and args

        "BITOR" ->
            bitOp Bitwise.or args

        "BITXOR" ->
            bitOp Bitwise.xor args

        "BITLSHIFT" ->
            bitOp (\a n -> Bitwise.shiftLeftBy n a) args

        "BITRSHIFT" ->
            bitOp (\a n -> Bitwise.shiftRightZfBy n a) args

        "CONVERT" ->
            convertFn args

        -- Statistics & forecasting --------------------------------------------
        "CORREL" ->
            twoArray correl args

        "RSQ" ->
            twoArray (\xs ys -> correl xs ys |> Result.map (\r -> r * r)) args

        "SLOPE" ->
            twoArray slope args

        "INTERCEPT" ->
            twoArray intercept args

        "FORECAST" ->
            forecastFn args

        "GEOMEAN" ->
            statAgg geomean args

        "HARMEAN" ->
            statAgg harmean args

        "DEVSQ" ->
            statAgg devsq args

        -- Date & time ---------------------------------------------------------
        "TIME" ->
            timeFn args

        "HOUR" ->
            timePart 24 args

        "MINUTE" ->
            timePart 1440 args

        "SECOND" ->
            timePart 86400 args

        "EDATE" ->
            edateFn 0 args

        "EOMONTH" ->
            eomonthFn args

        "WORKDAY" ->
            workdayFn args

        "NETWORKDAYS" ->
            networkdaysFn args

        "YEARFRAC" ->
            yearfracFn args

        -- Dynamic references --------------------------------------------------
        "ADDRESS" ->
            addressFn args

        -- Date ----------------------------------------------------------------
        "DATE" ->
            dateFn args

        "YEAR" ->
            datePart (\( y, _, _ ) -> y) args

        "MONTH" ->
            datePart (\( _, m, _ ) -> m) args

        "DAY" ->
            datePart (\( _, _, d ) -> d) args

        "WEEKDAY" ->
            weekdayFn args

        "DAYS" ->
            binNum (\endd startd -> endd - startd) args

        "DATEVALUE" ->
            case vals of
                [ v ] ->
                    case parseDateText (Value.toText v) of
                        Just serial ->
                            VNumber (toFloat serial)

                        Nothing ->
                            VError ValueErr

                _ ->
                    VError ValueErr

        "_" ->
            VError NameErr

        _ ->
            VError NameErr



-- NUMERIC HELPERS ------------------------------------------------------------


inf : Float
inf =
    1 / 0


{-| Collect numbers per spreadsheet rules: scalar args coerce (text→#VALUE!, bool→1/0,
empty→0); range cells ignore text/bool/empty; any error short-circuits. -}
collectNumbers : List Arg -> Result Error (List Float)
collectNumbers args =
    List.foldr
        (\arg acc ->
            case acc of
                Err er ->
                    Err er

                Ok nums ->
                    case arg of
                        Scalar v ->
                            case Value.toNumber v of
                                Ok n ->
                                    Ok (n :: nums)

                                Err er ->
                                    Err er

                        Matrix rows ->
                            collectFromCells (List.concat rows) nums
        )
        (Ok [])
        args


collectFromCells : List Value -> List Float -> Result Error (List Float)
collectFromCells cells acc =
    case cells of
        [] ->
            Ok acc

        v :: rest ->
            case v of
                VError er ->
                    Err er

                VNumber n ->
                    collectFromCells rest (n :: acc)

                _ ->
                    collectFromCells rest acc


numAgg : (List Float -> Float) -> List Arg -> Value
numAgg f args =
    case collectNumbers args of
        Ok nums ->
            VNumber (f nums)

        Err er ->
            VError er


numAggNonEmpty : (List Float -> Maybe Float) -> List Arg -> Value
numAggNonEmpty f args =
    case collectNumbers args of
        Ok nums ->
            case f nums of
                Just r ->
                    VNumber r

                Nothing ->
                    VError DivZero

        Err er ->
            VError er


numAggNonEmptyOr : Float -> (List Float -> Float) -> List Arg -> Value
numAggNonEmptyOr fallback f args =
    case collectNumbers args of
        Ok nums ->
            if List.isEmpty nums then
                VNumber fallback

            else
                VNumber (f nums)

        Err er ->
            VError er


{-| Aggregate over the flat numeric list (already filtered), erroring on an empty set —
for MEDIAN/STDEV/VAR which need at least one (or two) numbers. -}
statAgg : (List Float -> Result Error Float) -> List Arg -> Value
statAgg f args =
    case collectNumbers args of
        Ok nums ->
            case f nums of
                Ok r ->
                    VNumber r

                Err er ->
                    VError er

        Err er ->
            VError er


numbersOnly : List Value -> List Float
numbersOnly vals =
    List.filterMap
        (\v ->
            case v of
                VNumber n ->
                    Just n

                _ ->
                    Nothing
        )
        vals


average : List Float -> Maybe Float
average nums =
    case nums of
        [] ->
            Nothing

        _ ->
            Just (List.sum nums / toFloat (List.length nums))


withNumbersA : List Value -> (List Float -> Maybe Float) -> Value
withNumbersA vals f =
    -- AVERAGEA: text counts as 0, booleans as 1/0.
    let
        nums =
            List.filterMap
                (\v ->
                    case v of
                        VEmpty ->
                            Nothing

                        VError _ ->
                            Just 0

                        _ ->
                            Result.toMaybe (Value.toNumberLenient v)
                                |> orElse (Just 0)
                )
                vals
    in
    case f nums of
        Just r ->
            VNumber r

        Nothing ->
            VError DivZero


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback m =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback


median : List Float -> Result Error Float
median nums =
    case List.sort nums of
        [] ->
            Err NumErr

        sorted ->
            let
                n =
                    List.length sorted

                mid =
                    n // 2
            in
            if modBy 2 n == 1 then
                Ok (nth mid sorted)

            else
                Ok ((nth (mid - 1) sorted + nth mid sorted) / 2)


mode : List Float -> Result Error Float
mode nums =
    case nums of
        [] ->
            Err NA

        _ ->
            let
                counts =
                    List.map (\x -> ( x, List.length (List.filter (\y -> y == x) nums) )) nums

                best =
                    List.foldl
                        (\( x, c ) acc ->
                            case acc of
                                Nothing ->
                                    Just ( x, c )

                                Just ( _, bc ) ->
                                    if c > bc then
                                        Just ( x, c )

                                    else
                                        acc
                        )
                        Nothing
                        counts
            in
            case best of
                Just ( x, c ) ->
                    if c <= 1 then
                        Err NA

                    else
                        Ok x

                Nothing ->
                    Err NA


variance : Bool -> List Float -> Result Error Float
variance sample nums =
    let
        n =
            List.length nums
    in
    if n < 2 && sample then
        Err DivZero

    else if n < 1 then
        Err DivZero

    else
        let
            mean =
                List.sum nums / toFloat n

            ss =
                List.sum (List.map (\x -> (x - mean) ^ 2) nums)

            denom =
                if sample then
                    toFloat (n - 1)

                else
                    toFloat n
        in
        Ok (ss / denom)


stdev : Bool -> List Float -> Result Error Float
stdev sample nums =
    Result.map sqrt (variance sample nums)


nthOrdered : Bool -> List Arg -> Value
nthOrdered largest args =
    case args of
        rangeArg :: kArg :: _ ->
            case Value.toNumber (firstValue kArg) of
                Ok kf ->
                    let
                        k =
                            round kf

                        sorted =
                            List.sort (numbersOnly (flatten [ rangeArg ]))

                        ordered =
                            if largest then
                                List.reverse sorted

                            else
                                sorted
                    in
                    if k >= 1 && k <= List.length ordered then
                        VNumber (nth (k - 1) ordered)

                    else
                        VError NumErr

                Err er ->
                    VError er

        _ ->
            VError ValueErr


unaryNum : (Float -> Float) -> List Arg -> Value
unaryNum f args =
    case flatten args of
        [ v ] ->
            mapNum f v

        _ ->
            VError ValueErr


unaryNumChecked : (Float -> Result Error Float) -> List Arg -> Value
unaryNumChecked f args =
    case flatten args of
        [ v ] ->
            case Value.toNumber v of
                Ok n ->
                    case f n of
                        Ok r ->
                            VNumber r

                        Err er ->
                            VError er

                Err er ->
                    VError er

        _ ->
            VError ValueErr


binNum : (Float -> Float -> Float) -> List Arg -> Value
binNum f args =
    binNumChecked (\a b -> Ok (f a b)) args


binNumChecked : (Float -> Float -> Result Error Float) -> List Arg -> Value
binNumChecked f args =
    case flatten args of
        [ a, b ] ->
            case ( Value.toNumber a, Value.toNumber b ) of
                ( Ok x, Ok y ) ->
                    case f x y of
                        Ok r ->
                            VNumber r

                        Err er ->
                            VError er

                ( Err er, _ ) ->
                    VError er

                ( _, Err er ) ->
                    VError er

        _ ->
            VError ValueErr


mapNum : (Float -> Float) -> Value -> Value
mapNum f v =
    case Value.toNumber v of
        Ok n ->
            VNumber (f n)

        Err er ->
            VError er


roundFn : (Float -> Int) -> List Arg -> Value
roundFn rounder args =
    let
        ( value, digits ) =
            twoNums args
    in
    case value of
        Ok x ->
            case digits of
                Ok d ->
                    let
                        factor =
                            10 ^ toFloat (round d)
                    in
                    VNumber (toFloat (rounder (x * factor)) / factor)

                Err er ->
                    VError er

        Err er ->
            VError er


truncFn : List Arg -> Value
truncFn args =
    let
        ( value, digits ) =
            twoNumsOptional args 0
    in
    case ( value, digits ) of
        ( Ok x, Ok d ) ->
            let
                factor =
                    10 ^ toFloat (round d)
            in
            VNumber (toFloat (truncate (x * factor)) / factor)

        ( Err er, _ ) ->
            VError er

        ( _, Err er ) ->
            VError er


ceilingFloor : (Float -> Float -> Float) -> List Arg -> Value
ceilingFloor f args =
    let
        ( value, sig ) =
            twoNumsOptional args 1
    in
    case ( value, sig ) of
        ( Ok x, Ok m ) ->
            if m == 0 then
                VNumber 0

            else
                VNumber (f x m)

        ( Err er, _ ) ->
            VError er

        ( _, Err er ) ->
            VError er


twoNums : List Arg -> ( Result Error Float, Result Error Float )
twoNums args =
    case flatten args of
        [ a, b ] ->
            ( Value.toNumber a, Value.toNumber b )

        [ a ] ->
            ( Value.toNumber a, Ok 0 )

        _ ->
            ( Err ValueErr, Err ValueErr )


twoNumsOptional : List Arg -> Float -> ( Result Error Float, Result Error Float )
twoNumsOptional args dflt =
    case flatten args of
        [ a, b ] ->
            ( Value.toNumber a, Value.toNumber b )

        [ a ] ->
            ( Value.toNumber a, Ok dflt )

        _ ->
            ( Err ValueErr, Err ValueErr )


logFn : List Arg -> Value
logFn args =
    case flatten args of
        [ v ] ->
            case Value.toNumber v of
                Ok x ->
                    if x <= 0 then
                        VError NumErr

                    else
                        VNumber (logBase 10 x)

                Err er ->
                    VError er

        [ v, b ] ->
            case ( Value.toNumber v, Value.toNumber b ) of
                ( Ok x, Ok base ) ->
                    if x <= 0 || base <= 0 || base == 1 then
                        VError NumErr

                    else
                        VNumber (logBase base x)

                ( Err er, _ ) ->
                    VError er

                ( _, Err er ) ->
                    VError er

        _ ->
            VError ValueErr


roundHalfAway : Float -> Int
roundHalfAway x =
    if x >= 0 then
        floor (x + 0.5)

    else
        ceiling (x - 0.5)


sign : Float -> Int
sign x =
    if x > 0 then
        1

    else if x < 0 then
        -1

    else
        0


factorial : Float -> Result Error Float
factorial x =
    let
        n =
            round x
    in
    if x < 0 || toFloat n /= x then
        Err NumErr

    else
        Ok (List.product (List.map toFloat (List.range 1 n)))


combin : List Arg -> Value
combin args =
    case flatten args of
        [ a, b ] ->
            case ( Value.toNumber a, Value.toNumber b ) of
                ( Ok nf, Ok kf ) ->
                    let
                        n =
                            round nf

                        k =
                            round kf
                    in
                    if n < 0 || k < 0 || k > n then
                        VError NumErr

                    else
                        VNumber (toFloat (binomial n k))

                ( Err er, _ ) ->
                    VError er

                ( _, Err er ) ->
                    VError er

        _ ->
            VError ValueErr


binomial : Int -> Int -> Int
binomial n k =
    let
        kk =
            min k (n - k)
    in
    List.foldl (\i acc -> acc * (n - i) // (i + 1)) 1 (List.range 0 (kk - 1))


intReduce : (Int -> Int -> Int) -> Int -> List Value -> Value
intReduce f start vals =
    let
        ints =
            List.map (\v -> Value.toNumber v |> Result.map (\x -> abs (round x))) vals
    in
    case firstError ints of
        Just er ->
            VError er

        Nothing ->
            VNumber (toFloat (List.foldl (\v acc -> f acc v) start (List.filterMap Result.toMaybe ints)))


gcdI : Int -> Int -> Int
gcdI a b =
    if b == 0 then
        a

    else
        gcdI b (modBy b a)


lcmI : Int -> Int -> Int
lcmI a b =
    if a == 0 || b == 0 then
        0

    else
        abs (a * b) // gcdI a b


firstError : List (Result Error a) -> Maybe Error
firstError results =
    List.foldl
        (\r acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case r of
                        Err er ->
                            Just er

                        Ok _ ->
                            Nothing
        )
        Nothing
        results



-- BOOLEAN / INFO HELPERS -----------------------------------------------------


boolAgg : (List Bool -> Bool) -> List Arg -> Value
boolAgg f args =
    let
        bs =
            List.filterMap
                (\v ->
                    case v of
                        VEmpty ->
                            Nothing

                        _ ->
                            Result.toMaybe (Value.toBool v)
                )
                (flatten args)

        anyErr =
            firstError (List.map Value.toBool (List.filter (\v -> v /= VEmpty) (flatten args)))
    in
    case anyErr of
        Just er ->
            VError er

        Nothing ->
            if List.isEmpty bs then
                VError ValueErr

            else
                VBool (f bs)


predicate : (Value -> Bool) -> List Value -> Value
predicate f vals =
    case vals of
        [ v ] ->
            VBool (f v)

        _ ->
            VError ValueErr


intPredicate : (Int -> Bool) -> List Arg -> Value
intPredicate f args =
    case flatten args of
        [ v ] ->
            case Value.toNumber v of
                Ok n ->
                    VBool (f (truncate n))

                Err er ->
                    VError er

        _ ->
            VError ValueErr


isNumber : Value -> Bool
isNumber v =
    case v of
        VNumber _ ->
            True

        _ ->
            False


isText : Value -> Bool
isText v =
    case v of
        VText _ ->
            True

        _ ->
            False


isBlank : Value -> Bool
isBlank v =
    v == VEmpty


isLogical : Value -> Bool
isLogical v =
    case v of
        VBool _ ->
            True

        _ ->
            False


isNA : Value -> Bool
isNA v =
    v == VError NA


isErrNotNA : Value -> Bool
isErrNotNA v =
    Value.isError v && v /= VError NA


typeCode : Value -> Int
typeCode v =
    case v of
        VNumber _ ->
            1

        VText _ ->
            2

        VBool _ ->
            4

        VError _ ->
            16

        VEmpty ->
            1



-- TEXT HELPERS ---------------------------------------------------------------


unaryText : (String -> String) -> List Value -> Value
unaryText f vals =
    case vals of
        [ v ] ->
            VText (f (Value.toText v))

        _ ->
            VError ValueErr


textSlice : (String -> Int -> String) -> List Arg -> Value
textSlice f args =
    case flatten args of
        [ s, n ] ->
            case Value.toNumber n of
                Ok nf ->
                    if nf < 0 then
                        VError ValueErr

                    else
                        VText (f (Value.toText s) (round nf))

                Err er ->
                    VError er

        [ s ] ->
            VText (f (Value.toText s) 1)

        _ ->
            VError ValueErr


midFn : List Arg -> Value
midFn args =
    case flatten args of
        [ s, startV, lenV ] ->
            case ( Value.toNumber startV, Value.toNumber lenV ) of
                ( Ok startf, Ok lenf ) ->
                    let
                        start =
                            round startf

                        len =
                            round lenf
                    in
                    if start < 1 || len < 0 then
                        VError ValueErr

                    else
                        VText (String.slice (start - 1) (start - 1 + len) (Value.toText s))

                ( Err er, _ ) ->
                    VError er

                ( _, Err er ) ->
                    VError er

        _ ->
            VError ValueErr


reptFn : List Arg -> Value
reptFn args =
    case flatten args of
        [ s, n ] ->
            case Value.toNumber n of
                Ok nf ->
                    VText (String.repeat (max 0 (round nf)) (Value.toText s))

                Err er ->
                    VError er

        _ ->
            VError ValueErr


replaceFn : List Arg -> Value
replaceFn args =
    case flatten args of
        [ old, startV, lenV, new ] ->
            case ( Value.toNumber startV, Value.toNumber lenV ) of
                ( Ok startf, Ok lenf ) ->
                    let
                        s =
                            Value.toText old

                        start =
                            round startf - 1

                        len =
                            round lenf
                    in
                    VText (String.left start s ++ Value.toText new ++ String.dropLeft (start + len) s)

                ( Err er, _ ) ->
                    VError er

                ( _, Err er ) ->
                    VError er

        _ ->
            VError ValueErr


substituteFn : List Arg -> Value
substituteFn args =
    case flatten args of
        [ s, old, new ] ->
            VText (String.replace (Value.toText old) (Value.toText new) (Value.toText s))

        [ s, old, new, _ ] ->
            -- instance number not honoured; replace all (documented simplification)
            VText (String.replace (Value.toText old) (Value.toText new) (Value.toText s))

        _ ->
            VError ValueErr


findFn : Bool -> List Arg -> Value
findFn caseSensitive args =
    case flatten args of
        needleV :: hayV :: rest ->
            let
                needle =
                    if caseSensitive then
                        Value.toText needleV

                    else
                        String.toLower (Value.toText needleV)

                hay =
                    if caseSensitive then
                        Value.toText hayV

                    else
                        String.toLower (Value.toText hayV)

                startAt =
                    case rest of
                        n :: _ ->
                            case Value.toNumber n of
                                Ok nf ->
                                    round nf - 1

                                Err _ ->
                                    0

                        [] ->
                            0
            in
            case indexOfFrom needle hay startAt of
                Just i ->
                    VNumber (toFloat (i + 1))

                Nothing ->
                    VError ValueErr

        _ ->
            VError ValueErr


textJoin : List Arg -> Value
textJoin args =
    case args of
        delimArg :: skipArg :: rest ->
            let
                delim =
                    Value.toText (firstValue delimArg)

                skipEmpty =
                    Result.withDefault True (Value.toBool (firstValue skipArg))

                pieces =
                    flatten rest
                        |> List.filterMap
                            (\v ->
                                if skipEmpty && v == VEmpty then
                                    Nothing

                                else
                                    Just (Value.toText v)
                            )
            in
            VText (String.join delim pieces)

        _ ->
            VError ValueErr


mapNumberToChar : Value -> Value
mapNumberToChar v =
    case v of
        VNumber n ->
            VText (String.fromChar (Char.fromCode (round n)))

        _ ->
            v


{-| `TEXTBEFORE`/`TEXTAFTER`: the part of the text before (or after) the nth occurrence of a
delimiter. -}
textPart : Bool -> List Value -> Value
textPart before vals =
    case vals of
        text :: delim :: rest ->
            let
                instance =
                    case rest of
                        i :: _ ->
                            Maybe.withDefault 1 (Maybe.map round (Result.toMaybe (Value.toNumber i)))

                        [] ->
                            1
            in
            case splitAtNth (Value.toText delim) instance (Value.toText text) of
                Just ( pre, post ) ->
                    VText
                        (if before then
                            pre

                         else
                            post
                        )

                Nothing ->
                    VError NA

        _ ->
            VError ValueErr


splitAtNth : String -> Int -> String -> Maybe ( String, String )
splitAtNth delim n s =
    if delim == "" || n < 1 then
        Nothing

    else
        case List.head (List.drop (n - 1) (String.indexes delim s)) of
            Just idx ->
                Just ( String.left idx s, String.dropLeft (idx + String.length delim) s )

            Nothing ->
                Nothing


{-| `XMATCH`: the 1-based position of the first exact match of `lookup` in the array. -}
xmatchFn : List Arg -> Value
xmatchFn args =
    case args of
        lookupArg :: arrayArg :: _ ->
            case indexOfValue (firstValue lookupArg) (flatten [ arrayArg ]) of
                Just i ->
                    VNumber (toFloat (i + 1))

                Nothing ->
                    VError NA

        _ ->
            VError ValueErr


indexOfValue : Value -> List Value -> Maybe Int
indexOfValue target xs =
    indexOfHelp 0 target xs


indexOfHelp : Int -> Value -> List Value -> Maybe Int
indexOfHelp i target xs =
    case xs of
        [] ->
            Nothing

        y :: rest ->
            if Value.equalValue y target then
                Just i

            else
                indexOfHelp (i + 1) target rest


{-| The numeric code `ERROR.TYPE` reports for each error value (Excel's table; `#SPILL!`
is 9). -}
errorCode : Error -> Int
errorCode err =
    case err of
        DivZero ->
            2

        ValueErr ->
            3

        RefErr ->
            4

        NameErr ->
            5

        NumErr ->
            6

        NA ->
            7

        Spill ->
            9

        Circular ->
            8

        Parse ->
            8


collapseSpaces : String -> String
collapseSpaces s =
    String.words s |> String.join " "


properCase : String -> String
properCase s =
    String.words s
        |> List.map
            (\w ->
                String.left 1 (String.toUpper w) ++ String.dropLeft 1 (String.toLower w)
            )
        |> String.join " "



-- LOOKUP HELPERS -------------------------------------------------------------


vlookup : List Arg -> Value
vlookup args =
    case args of
        keyArg :: tableArg :: idxArg :: rest ->
            let
                key =
                    firstValue keyArg

                table =
                    matrixOf tableArg

                colIndex =
                    Value.toNumber (firstValue idxArg) |> Result.map round

                approx =
                    case rest of
                        a :: _ ->
                            Result.withDefault True (Value.toBool (firstValue a))

                        [] ->
                            True
            in
            case colIndex of
                Ok ci ->
                    lookupRows key table ci approx

                Err er ->
                    VError er

        _ ->
            VError ValueErr


lookupRows : Value -> List (List Value) -> Int -> Bool -> Value
lookupRows key rows colIndex approx =
    let
        matchRow row =
            case row of
                first :: _ ->
                    if approx then
                        Value.compare first key /= GT

                    else
                        Value.equalValue first key

                [] ->
                    False

        chosen =
            if approx then
                lastMatching matchRow rows

            else
                firstMatching matchRow rows
    in
    case chosen of
        Just row ->
            nthValue (colIndex - 1) row

        Nothing ->
            VError NA


hlookup : List Arg -> Value
hlookup args =
    case args of
        keyArg :: tableArg :: idxArg :: rest ->
            let
                transposed =
                    transpose (matrixOf tableArg)

                approx =
                    case rest of
                        a :: _ ->
                            Result.withDefault True (Value.toBool (firstValue a))

                        [] ->
                            True
            in
            case Value.toNumber (firstValue idxArg) |> Result.map round of
                Ok ri ->
                    lookupRows (firstValue keyArg) transposed ri approx

                Err er ->
                    VError er

        _ ->
            VError ValueErr


indexFn : List Arg -> Value
indexFn args =
    case args of
        matArg :: rest ->
            let
                rows =
                    matrixOf matArg

                ( rowN, colN ) =
                    case rest of
                        r :: c :: _ ->
                            ( numArg r, numArg c )

                        [ r ] ->
                            ( numArg r, 1 )

                        [] ->
                            ( 1, 1 )
            in
            indexInto rows rowN colN

        _ ->
            VError ValueErr


indexInto : List (List Value) -> Int -> Int -> Value
indexInto rows rowN colN =
    let
        nrows =
            List.length rows

        ncols =
            case rows of
                r :: _ ->
                    List.length r

                [] ->
                    0
    in
    if colN == 0 && nrows == 1 then
        -- single row, INDEX(row, k) → kth column
        case List.head rows of
            Just row ->
                nthValue (rowN - 1) row

            Nothing ->
                VError RefErr

    else if colN == 0 && ncols == 1 then
        -- single column, INDEX(col, k) → kth row
        nthValue (rowN - 1) (List.filterMap List.head rows)

    else
        let
            effCol =
                if colN == 0 then
                    1

                else
                    colN
        in
        case nthRow (rowN - 1) rows of
            Just row ->
                nthValue (effCol - 1) row

            Nothing ->
                VError RefErr


matchFn : List Arg -> Value
matchFn args =
    case args of
        keyArg :: rangeArg :: rest ->
            let
                key =
                    firstValue keyArg

                cells =
                    flatten [ rangeArg ]

                matchType =
                    case rest of
                        a :: _ ->
                            Result.withDefault 1 (Value.toNumber (firstValue a) |> Result.map round)

                        [] ->
                            1
            in
            matchIn key cells matchType

        _ ->
            VError ValueErr


matchIn : Value -> List Value -> Int -> Value
matchIn key cells matchType =
    let
        indexed =
            List.indexedMap (\i v -> ( i, v )) cells
    in
    if matchType == 0 then
        case firstMatching (\( _, v ) -> Value.equalValue v key) indexed of
            Just ( i, _ ) ->
                VNumber (toFloat (i + 1))

            Nothing ->
                VError NA

    else if matchType == 1 then
        -- largest value ≤ key, assuming ascending
        case lastMatching (\( _, v ) -> Value.compare v key /= GT) indexed of
            Just ( i, _ ) ->
                VNumber (toFloat (i + 1))

            Nothing ->
                VError NA

    else
        -- smallest value ≥ key, assuming descending
        case firstMatching (\( _, v ) -> Value.compare v key /= LT) indexed of
            Just ( i, _ ) ->
                VNumber (toFloat (i + 1))

            Nothing ->
                VError NA


numArg : Arg -> Int
numArg arg =
    case Value.toNumber (firstValue arg) of
        Ok n ->
            round n

        Err _ ->
            0



-- CONDITIONAL AGGREGATES -----------------------------------------------------


{-| Test a value against a criterion à la COUNTIF: a bare value means equality, a
leading comparison operator (`>5`, `<>x`, `>=10`) compares, and `*`/`?` wildcards work
for text equality. Exposed so conditional formatting can reuse it. -}
matchCriteria : String -> Value -> Bool
matchCriteria criterion value =
    let
        ( op, rhs ) =
            splitCriterion (String.trim criterion)
    in
    case op of
        "=" ->
            criteriaEq rhs value

        "<>" ->
            not (criteriaEq rhs value)

        ">" ->
            criteriaCompare value rhs GT

        "<" ->
            criteriaCompare value rhs LT

        ">=" ->
            criteriaCompareEq value rhs GT

        "<=" ->
            criteriaCompareEq value rhs LT

        _ ->
            criteriaEq criterion value


splitCriterion : String -> ( String, String )
splitCriterion s =
    if String.startsWith ">=" s then
        ( ">=", String.dropLeft 2 s )

    else if String.startsWith "<=" s then
        ( "<=", String.dropLeft 2 s )

    else if String.startsWith "<>" s then
        ( "<>", String.dropLeft 2 s )

    else if String.startsWith ">" s then
        ( ">", String.dropLeft 1 s )

    else if String.startsWith "<" s then
        ( "<", String.dropLeft 1 s )

    else if String.startsWith "=" s then
        ( "=", String.dropLeft 1 s )

    else
        ( "", s )


criteriaEq : String -> Value -> Bool
criteriaEq rhs value =
    case String.toFloat (String.trim rhs) of
        Just n ->
            case Value.toNumber value of
                Ok v ->
                    v == n

                Err _ ->
                    False

        Nothing ->
            if String.contains "*" rhs || String.contains "?" rhs then
                wildcardMatch (String.toLower rhs) (String.toLower (Value.toText value))

            else
                String.toLower (Value.toText value) == String.toLower rhs


criteriaCompare : Value -> String -> Order -> Bool
criteriaCompare value rhs want =
    case ( Value.toNumber value, String.toFloat (String.trim rhs) ) of
        ( Ok v, Just n ) ->
            Basics.compare v n == want

        _ ->
            Basics.compare (Value.toText value) rhs == want


criteriaCompareEq : Value -> String -> Order -> Bool
criteriaCompareEq value rhs want =
    criteriaCompare value rhs want
        || criteriaCompare value rhs EQ
        || (case ( Value.toNumber value, String.toFloat (String.trim rhs) ) of
                ( Ok v, Just n ) ->
                    v == n

                _ ->
                    Value.toText value == rhs
           )


countIf : List Arg -> Value
countIf args =
    case args of
        rangeArg :: critArg :: _ ->
            let
                crit =
                    Value.toText (firstValue critArg)
            in
            VNumber (toFloat (List.length (List.filter (matchCriteria crit) (flatten [ rangeArg ]))))

        _ ->
            VError ValueErr


sumIf : List Arg -> Value
sumIf args =
    case args of
        rangeArg :: critArg :: rest ->
            let
                crit =
                    Value.toText (firstValue critArg)

                testCells =
                    flatten [ rangeArg ]

                sumCells =
                    case rest of
                        sa :: _ ->
                            flatten [ sa ]

                        [] ->
                            testCells

                pairs =
                    zip testCells sumCells
            in
            VNumber
                (List.sum
                    (List.filterMap
                        (\( t, s ) ->
                            if matchCriteria crit t then
                                case s of
                                    VNumber n ->
                                        Just n

                                    _ ->
                                        Nothing

                            else
                                Nothing
                        )
                        pairs
                    )
                )

        _ ->
            VError ValueErr


averageIf : List Arg -> Value
averageIf args =
    case args of
        rangeArg :: critArg :: rest ->
            let
                crit =
                    Value.toText (firstValue critArg)

                testCells =
                    flatten [ rangeArg ]

                avgCells =
                    case rest of
                        sa :: _ ->
                            flatten [ sa ]

                        [] ->
                            testCells

                matched =
                    List.filterMap
                        (\( t, s ) ->
                            if matchCriteria crit t then
                                case s of
                                    VNumber n ->
                                        Just n

                                    _ ->
                                        Nothing

                            else
                                Nothing
                        )
                        (zip testCells avgCells)
            in
            case average matched of
                Just r ->
                    VNumber r

                Nothing ->
                    VError DivZero

        _ ->
            VError ValueErr


wildcardMatch : String -> String -> Bool
wildcardMatch pattern str =
    wmatch (String.toList pattern) (String.toList str)


wmatch : List Char -> List Char -> Bool
wmatch pattern str =
    case pattern of
        [] ->
            List.isEmpty str

        '*' :: ps ->
            wmatch ps str
                || (case str of
                        _ :: ss ->
                            wmatch pattern ss

                        [] ->
                            False
                   )

        '?' :: ps ->
            case str of
                _ :: ss ->
                    wmatch ps ss

                [] ->
                    False

        p :: ps ->
            case str of
                s :: ss ->
                    p == s && wmatch ps ss

                [] ->
                    False



-- DATE HELPERS ---------------------------------------------------------------
-- A clean proleptic-Gregorian serial model: DATE(1900,1,1) = 1. (We omit Excel's
-- historical 1900-leap-year bug, so serials differ from Excel by 1 after Feb 1900.)


dateEpoch : Int
dateEpoch =
    daysFromCivil 1899 12 31


dateFn : List Arg -> Value
dateFn args =
    case flatten args of
        [ y, m, d ] ->
            case ( Value.toNumber y, Value.toNumber m, Value.toNumber d ) of
                ( Ok yf, Ok mf, Ok df ) ->
                    VNumber (toFloat (dateToSerial (round yf) (round mf) (round df)))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


dateToSerial : Int -> Int -> Int -> Int
dateToSerial y m d =
    -- normalise out-of-range months the way Excel does (month 13 → next January)
    let
        y2 =
            y + (m - 1) // 12

        m2 =
            modBy 12 (m - 1) + 1
    in
    daysFromCivil y2 m2 d - dateEpoch


serialToDate : Int -> ( Int, Int, Int )
serialToDate serial =
    civilFromDays (serial + dateEpoch)


datePart : (( Int, Int, Int ) -> Int) -> List Arg -> Value
datePart extract args =
    case flatten args of
        [ v ] ->
            case Value.toNumber v of
                Ok serial ->
                    VNumber (toFloat (extract (serialToDate (round serial))))

                Err er ->
                    VError er

        _ ->
            VError ValueErr


weekdayFn : List Arg -> Value
weekdayFn args =
    case flatten args of
        v :: _ ->
            case Value.toNumber v of
                Ok serial ->
                    -- 1 = Sunday … 7 = Saturday (default type). Day 1 (1900-01-01) was a Monday.
                    VNumber (toFloat (modBy 7 (round serial - 1 + 1) + 1))

                Err er ->
                    VError er

        _ ->
            VError ValueErr


parseDateText : String -> Maybe Int
parseDateText s =
    case List.filterMap String.toInt (String.split "-" (String.trim s)) of
        [ y, m, d ] ->
            Just (dateToSerial y m d)

        _ ->
            case List.filterMap String.toInt (String.split "/" (String.trim s)) of
                [ m, d, y ] ->
                    Just (dateToSerial y m d)

                _ ->
                    Nothing


{-| Days from 1970-01-01 (Howard Hinnant's civil-from-days, inverted). -}
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



-- SMALL LIST UTILITIES -------------------------------------------------------


nth : Int -> List Float -> Float
nth i xs =
    case List.drop i xs of
        x :: _ ->
            x

        [] ->
            0


nthValue : Int -> List Value -> Value
nthValue i xs =
    case List.drop i xs of
        x :: _ ->
            x

        [] ->
            VError RefErr


nthRow : Int -> List (List Value) -> Maybe (List Value)
nthRow i xs =
    List.head (List.drop i xs)


firstMatching : (a -> Bool) -> List a -> Maybe a
firstMatching f xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if f x then
                Just x

            else
                firstMatching f rest


lastMatching : (a -> Bool) -> List a -> Maybe a
lastMatching f xs =
    List.foldl
        (\x acc ->
            if f x then
                Just x

            else
                acc
        )
        Nothing
        xs


transpose : List (List a) -> List (List a)
transpose rows =
    case rows of
        [] ->
            []

        [] :: _ ->
            []

        _ ->
            let
                heads =
                    List.filterMap List.head rows

                tails =
                    List.map (List.drop 1) rows
            in
            heads :: transpose tails


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xrest, y :: yrest ) ->
            ( x, y ) :: zip xrest yrest

        _ ->
            []


indexOfFrom : String -> String -> Int -> Maybe Int
indexOfFrom needle hay startAt =
    let
        sliced =
            String.dropLeft (max 0 startAt) hay
    in
    case String.indexes needle sliced of
        i :: _ ->
            Just (i + max 0 startAt)

        [] ->
            Nothing



-- FINANCE & ANALYSIS ---------------------------------------------------------
-- A pack of financial (PMT/FV/PV/NPER/NPV/IRR), multi-criteria (SUMIFS/COUNTIFS/...),
-- statistical (PERCENTILE/QUARTILE/RANK), SUMPRODUCT, SUBTOTAL and XLOOKUP functions.


resToMaybe : Result e a -> Maybe a
resToMaybe r =
    case r of
        Ok x ->
            Just x

        Err _ ->
            Nothing


{-| The numeric value of the scalar argument at position `i`. -}
sc : Int -> List Arg -> Maybe Float
sc i args =
    case List.drop i args of
        a :: _ ->
            resToMaybe (Value.toNumber (firstValue a))

        [] ->
            Nothing


{-| An optional numeric argument: position `i`, or `d` if absent/non-numeric. -}
opt : Int -> Float -> List Arg -> Float
opt i d args =
    Maybe.withDefault d (sc i args)


num0 : Value -> Float
num0 v =
    case Value.toNumber v of
        Ok n ->
            n

        Err _ ->
            0


firstErr : List Value -> Maybe Error
firstErr vals =
    List.head
        (List.filterMap
            (\v ->
                case v of
                    VError e ->
                        Just e

                    _ ->
                        Nothing
            )
            vals
        )


nthF : Int -> List Float -> Float
nthF i list =
    Maybe.withDefault 0 (List.head (List.drop i list))


firstIndexWhere : (a -> Bool) -> List a -> Maybe Int
firstIndexWhere pred xs =
    firstIndexHelp pred xs 0


firstIndexHelp : (a -> Bool) -> List a -> Int -> Maybe Int
firstIndexHelp pred xs i =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if pred x then
                Just i

            else
                firstIndexHelp pred rest (i + 1)


sumProduct : List Arg -> Value
sumProduct args =
    let
        flats =
            List.map (\a -> flatten [ a ]) args
    in
    case firstErr (List.concat flats) of
        Just e ->
            VError e

        Nothing ->
            case flats of
                [] ->
                    VNumber 0

                first :: _ ->
                    let
                        len =
                            List.length first

                        nums =
                            List.map (List.map num0) flats
                    in
                    if List.all (\l -> List.length l == len) flats then
                        VNumber (List.sum (List.foldl (List.map2 (*)) (List.repeat len 1) nums))

                    else
                        VError ValueErr


toPairs : List Arg -> List ( Arg, String )
toPairs args =
    case args of
        r :: c :: rest ->
            ( r, Value.toText (firstValue c) ) :: toPairs rest

        _ ->
            []


masksOf : List ( Arg, String ) -> List (List Bool)
masksOf pairs =
    List.map (\( rangeArg, crit ) -> List.map (matchCriteria crit) (flatten [ rangeArg ])) pairs


boolAt : Int -> List Bool -> Bool
boolAt i bs =
    Maybe.withDefault False (List.head (List.drop i bs))


matchIdx : Int -> List (List Bool) -> List Int
matchIdx n masks =
    List.filter (\i -> List.all (boolAt i) masks) (List.range 0 (n - 1))


countIfs : List Arg -> Value
countIfs args =
    let
        pairs =
            toPairs args

        n =
            case pairs of
                ( r, _ ) :: _ ->
                    List.length (flatten [ r ])

                [] ->
                    0
    in
    VNumber (toFloat (List.length (matchIdx n (masksOf pairs))))


numAt : Int -> List Value -> Maybe Float
numAt i vals =
    case List.head (List.drop i vals) of
        Just (VNumber x) ->
            Just x

        _ ->
            Nothing


selectedNums : List Arg -> Maybe (List Float)
selectedNums args =
    case args of
        sumArg :: rest ->
            let
                pairs =
                    toPairs rest

                sumVals =
                    flatten [ sumArg ]

                idx =
                    matchIdx (List.length sumVals) (masksOf pairs)
            in
            Just (List.filterMap (\i -> numAt i sumVals) idx)

        _ ->
            Nothing


sumIfs : List Arg -> Value
sumIfs args =
    case selectedNums args of
        Just ns ->
            VNumber (List.sum ns)

        Nothing ->
            VError ValueErr


averageIfs : List Arg -> Value
averageIfs args =
    case selectedNums args of
        Just [] ->
            VError DivZero

        Just ns ->
            VNumber (List.sum ns / toFloat (List.length ns))

        Nothing ->
            VError ValueErr


minMaxIfs : Bool -> List Arg -> Value
minMaxIfs isMin args =
    case selectedNums args of
        Just [] ->
            VNumber 0

        Just (n :: rest) ->
            VNumber
                (List.foldl
                    (if isMin then
                        Basics.min

                     else
                        Basics.max
                    )
                    n
                    rest
                )

        Nothing ->
            VError ValueErr


subtotal : List Arg -> Value
subtotal args =
    case args of
        fArg :: rest ->
            case Value.toNumber (firstValue fArg) of
                Ok f ->
                    case modBy 100 (round f) of
                        1 ->
                            call "AVERAGE" rest

                        2 ->
                            call "COUNT" rest

                        3 ->
                            call "COUNTA" rest

                        4 ->
                            call "MAX" rest

                        5 ->
                            call "MIN" rest

                        6 ->
                            call "PRODUCT" rest

                        7 ->
                            call "STDEV" rest

                        8 ->
                            call "STDEVP" rest

                        9 ->
                            call "SUM" rest

                        10 ->
                            call "VAR" rest

                        11 ->
                            call "VARP" rest

                        _ ->
                            VError ValueErr

                Err e ->
                    VError e

        _ ->
            VError ValueErr


percentileFn : List Arg -> Value
percentileFn args =
    case args of
        arrArg :: kArg :: _ ->
            case ( collectNumbers [ arrArg ], Value.toNumber (firstValue kArg) ) of
                ( Ok nums, Ok k ) ->
                    percentileInc (List.sort nums) k

                ( Err e, _ ) ->
                    VError e

                ( _, Err e ) ->
                    VError e

        _ ->
            VError ValueErr


percentileInc : List Float -> Float -> Value
percentileInc sorted k =
    if k < 0 || k > 1 then
        VError NumErr

    else
        case sorted of
            [] ->
                VError NumErr

            _ ->
                let
                    rank =
                        k * toFloat (List.length sorted - 1)

                    lo =
                        floor rank

                    frac =
                        rank - toFloat lo

                    a =
                        nthF lo sorted

                    b =
                        nthF (lo + 1) sorted
                in
                VNumber (a + frac * (b - a))


quartileFn : List Arg -> Value
quartileFn args =
    case args of
        arrArg :: qArg :: _ ->
            case Value.toNumber (firstValue qArg) of
                Ok q ->
                    percentileFn [ arrArg, Scalar (VNumber (q / 4)) ]

                Err e ->
                    VError e

        _ ->
            VError ValueErr


rankFn : List Arg -> Value
rankFn args =
    case args of
        vArg :: arrArg :: rest ->
            case ( Value.toNumber (firstValue vArg), collectNumbers [ arrArg ] ) of
                ( Ok v, Ok nums ) ->
                    let
                        ascending =
                            case rest of
                                o :: _ ->
                                    opt 0 0 [ o ] /= 0

                                [] ->
                                    False

                        ordered =
                            if ascending then
                                List.sort nums

                            else
                                List.reverse (List.sort nums)
                    in
                    case firstIndexWhere (\x -> x == v) ordered of
                        Just i ->
                            VNumber (toFloat (i + 1))

                        Nothing ->
                            VError NA

                ( Err e, _ ) ->
                    VError e

                ( _, Err e ) ->
                    VError e

        _ ->
            VError ValueErr


xlookup : List Arg -> Value
xlookup args =
    case args of
        keyArg :: lookArg :: retArg :: rest ->
            let
                key =
                    firstValue keyArg

                looks =
                    flatten [ lookArg ]

                rets =
                    flatten [ retArg ]
            in
            case firstIndexWhere (\v -> Value.equalValue v key) looks of
                Just i ->
                    Maybe.withDefault (VError NA) (List.head (List.drop i rets))

                Nothing ->
                    case rest of
                        nf :: _ ->
                            firstValue nf

                        [] ->
                            VError NA

        _ ->
            VError ValueErr


financeFn : (Float -> Float -> Float -> Float -> Float -> Float) -> List Arg -> Value
financeFn f args =
    case ( sc 0 args, sc 1 args, sc 2 args ) of
        ( Just a, Just b, Just c ) ->
            let
                r =
                    f a b c (opt 3 0 args) (opt 4 0 args)
            in
            if isNaN r || isInfinite r then
                VError NumErr

            else
                VNumber r

        _ ->
            VError ValueErr


pmt : Float -> Float -> Float -> Float -> Float -> Float
pmt rate nperiods presentValue futureValue typ =
    if rate == 0 then
        if nperiods == 0 then
            0 / 0

        else
            -(presentValue + futureValue) / nperiods

    else
        let
            p =
                (1 + rate) ^ nperiods
        in
        -(rate * (presentValue * p + futureValue)) / ((1 + rate * typ) * (p - 1))


fv : Float -> Float -> Float -> Float -> Float -> Float
fv rate nperiods payment presentValue typ =
    if rate == 0 then
        -(presentValue + payment * nperiods)

    else
        let
            p =
                (1 + rate) ^ nperiods
        in
        -(presentValue * p + payment * (1 + rate * typ) * (p - 1) / rate)


pv : Float -> Float -> Float -> Float -> Float -> Float
pv rate nperiods payment futureValue typ =
    if rate == 0 then
        -(futureValue + payment * nperiods)

    else
        let
            p =
                (1 + rate) ^ nperiods
        in
        -(futureValue + payment * (1 + rate * typ) * (p - 1) / rate) / p


nper : Float -> Float -> Float -> Float -> Float -> Float
nper rate payment presentValue futureValue typ =
    if rate == 0 then
        if payment == 0 then
            0 / 0

        else
            -(presentValue + futureValue) / payment

    else
        let
            adj =
                payment * (1 + rate * typ)
        in
        logBase (1 + rate) ((adj - futureValue * rate) / (adj + presentValue * rate))


{-| Numbers in their natural row-major order (unlike `collectNumbers`, which reverses
within a matrix — fine for commutative aggregates but wrong for ordered cash flows). -}
orderedNumbers : List Arg -> Result Error (List Float)
orderedNumbers args =
    let
        vals =
            flatten args
    in
    case firstErr vals of
        Just e ->
            Err e

        Nothing ->
            Ok (numbersOnly vals)


npvFn : List Arg -> Value
npvFn args =
    case args of
        rateArg :: rest ->
            case Value.toNumber (firstValue rateArg) of
                Ok rate ->
                    case orderedNumbers rest of
                        Ok nums ->
                            VNumber (npvAt rate 1 nums)

                        Err e ->
                            VError e

                Err e ->
                    VError e

        _ ->
            VError ValueErr


npvAt : Float -> Int -> List Float -> Float
npvAt rate startExp nums =
    List.sum (List.indexedMap (\i v -> v / (1 + rate) ^ toFloat (i + startExp)) nums)


irrFn : List Arg -> Value
irrFn args =
    case args of
        valsArg :: rest ->
            case orderedNumbers [ valsArg ] of
                Ok nums ->
                    let
                        guess =
                            case rest of
                                g :: _ ->
                                    opt 0 0.1 [ g ]

                                [] ->
                                    0.1
                    in
                    case newton (\r -> npvAt r 0 nums) guess 0 of
                        Just r ->
                            VNumber r

                        Nothing ->
                            VError NumErr

                Err e ->
                    VError e

        _ ->
            VError ValueErr


newton : (Float -> Float) -> Float -> Int -> Maybe Float
newton f x iter =
    if iter > 100 then
        Nothing

    else
        let
            fx =
                f x
        in
        if isNaN fx || isInfinite fx then
            Nothing

        else if abs fx < 1.0e-7 then
            Just x

        else
            let
                d =
                    (f (x + 1.0e-6) - fx) / 1.0e-6
            in
            if d == 0 then
                Nothing

            else
                let
                    nx =
                        x - fx / d
                in
                newton f
                    (if nx <= -0.9999 then
                        -0.9999

                     else
                        nx
                    )
                    (iter + 1)



-- STATISTICS & FORECASTING ---------------------------------------------------


{-| Run a two-array statistic over the numeric pairs of two arguments. -}
twoArray : (List Float -> List Float -> Result Error Float) -> List Arg -> Value
twoArray f args =
    case args of
        a :: b :: _ ->
            case firstErr (flatten [ a ] ++ flatten [ b ]) of
                Just e ->
                    VError e

                Nothing ->
                    let
                        ( xs, ys ) =
                            pairFloats a b
                    in
                    if List.length xs < 2 then
                        VError DivZero

                    else
                        case f xs ys of
                            Ok r ->
                                VNumber r

                            Err e ->
                                VError e

        _ ->
            VError ValueErr


{-| The aligned numeric pairs of two arguments (positions where both are numbers). -}
pairFloats : Arg -> Arg -> ( List Float, List Float )
pairFloats a b =
    let
        kept =
            List.filterMap
                (\( x, y ) ->
                    case ( x, y ) of
                        ( VNumber xn, VNumber yn ) ->
                            Just ( xn, yn )

                        _ ->
                            Nothing
                )
                (zip (flatten [ a ]) (flatten [ b ]))
    in
    ( List.map Tuple.first kept, List.map Tuple.second kept )


meanOf : List Float -> Float
meanOf xs =
    List.sum xs / toFloat (List.length xs)


correl : List Float -> List Float -> Result Error Float
correl xs ys =
    let
        mx =
            meanOf xs

        my =
            meanOf ys

        sxy =
            List.sum (List.map2 (\x y -> (x - mx) * (y - my)) xs ys)

        sxx =
            List.sum (List.map (\x -> (x - mx) ^ 2) xs)

        syy =
            List.sum (List.map (\y -> (y - my) ^ 2) ys)
    in
    if sxx == 0 || syy == 0 then
        Err DivZero

    else
        Ok (sxy / sqrt (sxx * syy))


slope : List Float -> List Float -> Result Error Float
slope ys xs =
    let
        mx =
            meanOf xs

        my =
            meanOf ys

        sxy =
            List.sum (List.map2 (\x y -> (x - mx) * (y - my)) xs ys)

        sxx =
            List.sum (List.map (\x -> (x - mx) ^ 2) xs)
    in
    if sxx == 0 then
        Err DivZero

    else
        Ok (sxy / sxx)


intercept : List Float -> List Float -> Result Error Float
intercept ys xs =
    case slope ys xs of
        Ok m ->
            Ok (meanOf ys - m * meanOf xs)

        Err e ->
            Err e


forecastFn : List Arg -> Value
forecastFn args =
    case args of
        xArg :: yArg :: xsArg :: _ ->
            case Value.toNumber (firstValue xArg) of
                Ok x ->
                    case firstErr (flatten [ yArg ] ++ flatten [ xsArg ]) of
                        Just e ->
                            VError e

                        Nothing ->
                            let
                                ( ys, xs ) =
                                    pairFloats yArg xsArg
                            in
                            if List.length xs < 2 then
                                VError DivZero

                            else
                                case ( slope ys xs, intercept ys xs ) of
                                    ( Ok m, Ok b ) ->
                                        VNumber (m * x + b)

                                    ( Err e, _ ) ->
                                        VError e

                                    ( _, Err e ) ->
                                        VError e

                Err e ->
                    VError e

        _ ->
            VError ValueErr


geomean : List Float -> Result Error Float
geomean nums =
    case nums of
        [] ->
            Err NumErr

        _ ->
            if List.any (\x -> x <= 0) nums then
                Err NumErr

            else
                Ok (List.product nums ^ (1 / toFloat (List.length nums)))


harmean : List Float -> Result Error Float
harmean nums =
    case nums of
        [] ->
            Err DivZero

        _ ->
            if List.any (\x -> x <= 0) nums then
                Err NumErr

            else
                Ok (toFloat (List.length nums) / List.sum (List.map (\x -> 1 / x) nums))


devsq : List Float -> Result Error Float
devsq nums =
    case nums of
        [] ->
            Err NumErr

        _ ->
            let
                m =
                    meanOf nums
            in
            Ok (List.sum (List.map (\x -> (x - m) ^ 2) nums))



-- DATE & TIME ----------------------------------------------------------------


timeFn : List Arg -> Value
timeFn args =
    case flatten args of
        [ h, m, s ] ->
            case ( Value.toNumber h, Value.toNumber m, Value.toNumber s ) of
                ( Ok hf, Ok mf, Ok sf ) ->
                    let
                        frac =
                            (hf * 3600 + mf * 60 + sf) / 86400
                    in
                    VNumber (frac - toFloat (floor frac))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


timePart : Int -> List Arg -> Value
timePart unit args =
    case flatten args of
        [ v ] ->
            case Value.toNumber v of
                Ok serial ->
                    let
                        frac =
                            serial - toFloat (floor serial)

                        total =
                            floor (frac * toFloat unit + 1.0e-9)

                        wrapped =
                            if unit == 24 then
                                modBy 24 total

                            else
                                modBy 60 total
                    in
                    VNumber (toFloat wrapped)

                Err e ->
                    VError e

        _ ->
            VError ValueErr


isLeap : Int -> Bool
isLeap y =
    (modBy 4 y == 0 && modBy 100 y /= 0) || modBy 400 y == 0


lastDay : Int -> Int -> Int
lastDay y m =
    if m == 2 then
        if isLeap y then
            29

        else
            28

    else if m == 4 || m == 6 || m == 9 || m == 11 then
        30

    else
        31


addMonths : Int -> Int -> Int -> ( Int, Int )
addMonths y m offset =
    let
        t =
            y * 12 + (m - 1) + offset
    in
    ( t // 12, modBy 12 t + 1 )


edateFn : Int -> List Arg -> Value
edateFn _ args =
    case flatten args of
        start :: months :: _ ->
            case ( Value.toNumber start, Value.toNumber months ) of
                ( Ok s, Ok mo ) ->
                    let
                        ( y, m, d ) =
                            serialToDate (round s)

                        ( y2, m2 ) =
                            addMonths y m (round mo)
                    in
                    VNumber (toFloat (dateToSerial y2 m2 (min d (lastDay y2 m2))))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


eomonthFn : List Arg -> Value
eomonthFn args =
    case flatten args of
        start :: months :: _ ->
            case ( Value.toNumber start, Value.toNumber months ) of
                ( Ok s, Ok mo ) ->
                    let
                        ( y, m, _ ) =
                            serialToDate (round s)

                        ( y2, m2 ) =
                            addMonths y m (round mo)
                    in
                    VNumber (toFloat (dateToSerial y2 m2 (lastDay y2 m2)))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


{-| Day of week, 1 = Sunday … 7 = Saturday (serial 1 = 1900-01-01, a Monday → 2). -}
dowSun1 : Int -> Int
dowSun1 serial =
    modBy 7 serial + 1


isWorkday : List Int -> Int -> Bool
isWorkday holidays serial =
    let
        d =
            dowSun1 serial
    in
    d /= 1 && d /= 7 && not (List.member serial holidays)


holidaysOf : List Arg -> List Int
holidaysOf rest =
    case rest of
        h :: _ ->
            List.map round (numbersOnly (flatten [ h ]))

        [] ->
            []


workdayFn : List Arg -> Value
workdayFn args =
    case args of
        startArg :: daysArg :: rest ->
            case ( Value.toNumber (firstValue startArg), Value.toNumber (firstValue daysArg) ) of
                ( Ok s, Ok days ) ->
                    VNumber (toFloat (stepWork (holidaysOf rest) (round s) (signOf (round days)) (abs (round days))))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


signOf : Int -> Int
signOf n =
    if n < 0 then
        -1

    else
        1


stepWork : List Int -> Int -> Int -> Int -> Int
stepWork holidays cur dir remaining =
    if remaining <= 0 then
        cur

    else
        let
            next =
                cur + dir
        in
        if isWorkday holidays next then
            stepWork holidays next dir (remaining - 1)

        else
            stepWork holidays next dir remaining


networkdaysFn : List Arg -> Value
networkdaysFn args =
    case args of
        startArg :: endArg :: rest ->
            case ( Value.toNumber (firstValue startArg), Value.toNumber (firstValue endArg) ) of
                ( Ok a, Ok b ) ->
                    let
                        lo =
                            min (round a) (round b)

                        hi =
                            max (round a) (round b)

                        count =
                            List.length (List.filter (isWorkday (holidaysOf rest)) (List.range lo hi))
                    in
                    VNumber (toFloat (count * signOf (round b - round a + 1)))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


yearfracFn : List Arg -> Value
yearfracFn args =
    case args of
        startArg :: endArg :: rest ->
            case ( Value.toNumber (firstValue startArg), Value.toNumber (firstValue endArg) ) of
                ( Ok a, Ok b ) ->
                    let
                        basis =
                            case rest of
                                x :: _ ->
                                    round (Result.withDefault 0 (Value.toNumber (firstValue x)))

                                [] ->
                                    0

                        days =
                            abs (b - a)

                        denom =
                            if basis == 1 then
                                365.25

                            else if basis == 3 then
                                365

                            else
                                360
                    in
                    VNumber (days / denom)

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr



-- ADDRESS --------------------------------------------------------------------


addressFn : List Arg -> Value
addressFn args =
    case flatten args of
        rowV :: colV :: rest ->
            case ( Value.toNumber rowV, Value.toNumber colV ) of
                ( Ok r, Ok c ) ->
                    let
                        absnum =
                            case rest of
                                a :: _ ->
                                    round (Result.withDefault 1 (Value.toNumber a))

                                [] ->
                                    1

                        ( cd, rd ) =
                            case absnum of
                                2 ->
                                    ( "", "$" )

                                3 ->
                                    ( "$", "" )

                                4 ->
                                    ( "", "" )

                                _ ->
                                    ( "$", "$" )
                    in
                    VText (cd ++ colLetters (round c - 1) ++ rd ++ String.fromInt (round r))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


{-| 0-based column index to letters (`0 → "A"`, `26 → "AA"`). -}
colLetters : Int -> String
colLetters col =
    if col < 0 then
        ""

    else
        colLettersHelp col ""


colLettersHelp : Int -> String -> String
colLettersHelp n acc =
    let
        letter =
            String.fromChar (Char.fromCode (Char.toCode 'A' + modBy 26 n))

        next =
            n // 26 - 1
    in
    if next < 0 then
        letter ++ acc

    else
        colLettersHelp next (letter ++ acc)


-- REGEX / DATABASE / AGGREGATION HELPERS --------------------------------------


{-| Read a case-insensitivity flag (1 = ignore case) from the optional trailing args. -}
icFlag : List Value -> Int -> Bool
icFlag vals i =
    case List.head (List.drop i vals) of
        Just v ->
            case Value.toNumber v of
                Ok n ->
                    round n == 1

                Err _ ->
                    False

        Nothing ->
            False


sameTextF : String -> String -> Bool
sameTextF a b =
    String.toUpper (String.trim a) == String.toUpper (String.trim b)


findIndexBy : (a -> Bool) -> List a -> Maybe Int
findIndexBy pred xs =
    findIndexHelp 0 pred xs


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp i pred xs =
    case xs of
        [] ->
            Nothing

        y :: rest ->
            if pred y then
                Just i

            else
                findIndexHelp (i + 1) pred rest


{-| Run a database query: `DSUM(database, field, criteria)` and friends. `database` is a
range whose first row is the field headers; `field` selects the column to aggregate (by
header text or 1-based index); `criteria` is a range whose first row is field names and
whose remaining rows are criteria (cells in a row AND together, rows OR together). -}
dQuery : (List Value -> Value) -> List Arg -> Value
dQuery combine args =
    case args of
        dbArg :: fieldArg :: critArg :: _ ->
            let
                db =
                    matrixOf dbArg

                crit =
                    matrixOf critArg
            in
            case dbColumn db (firstValue fieldArg) of
                Just colIdx ->
                    combine (List.map (nthValue colIdx) (dbMatchingRows db crit))

                Nothing ->
                    VError ValueErr

        _ ->
            VError ValueErr


dbColumn : List (List Value) -> Value -> Maybe Int
dbColumn db field =
    case db of
        headers :: _ ->
            case field of
                VNumber n ->
                    let
                        i =
                            round n - 1
                    in
                    if i >= 0 && i < List.length headers then
                        Just i

                    else
                        Nothing

                _ ->
                    findIndexBy (\h -> sameTextF (Value.toText h) (Value.toText field)) headers

        [] ->
            Nothing


dbMatchingRows : List (List Value) -> List (List Value) -> List (List Value)
dbMatchingRows db criteria =
    case ( db, criteria ) of
        ( dbHeaders :: dbRows, critHeaders :: critRows ) ->
            List.filter (\row -> dbRowMatches dbHeaders row critHeaders critRows) dbRows

        _ ->
            []


dbRowMatches : List Value -> List Value -> List Value -> List (List Value) -> Bool
dbRowMatches dbHeaders row critHeaders critRows =
    if List.isEmpty critRows then
        True

    else
        List.any (\cr -> dbCritRowMatches dbHeaders row critHeaders cr) critRows


dbCritRowMatches : List Value -> List Value -> List Value -> List Value -> Bool
dbCritRowMatches dbHeaders row critHeaders cr =
    List.all identity
        (List.map2 (\ch cv -> dbCellMatches dbHeaders row (Value.toText ch) cv) critHeaders cr)


dbCellMatches : List Value -> List Value -> String -> Value -> Bool
dbCellMatches dbHeaders row fieldName cv =
    if Value.toText cv == "" then
        True

    else
        case findIndexBy (\h -> sameTextF (Value.toText h) fieldName) dbHeaders of
            Just i ->
                matchCriteria (Value.toText cv) (nthValue i row)

            Nothing ->
                True


dAverage : List Value -> Value
dAverage xs =
    case numbersOnly xs of
        [] ->
            VError DivZero

        ns ->
            VNumber (List.sum ns / toFloat (List.length ns))


dExtreme : (List Float -> Maybe Float) -> List Value -> Value
dExtreme f xs =
    case f (numbersOnly xs) of
        Just x ->
            VNumber x

        Nothing ->
            VError NumErr


dGet : List Value -> Value
dGet xs =
    case xs of
        [ v ] ->
            v

        [] ->
            VError ValueErr

        _ ->
            VError NumErr


{-| `AGGREGATE(funcNum, options, …)`: dispatch to the aggregate named by `funcNum`,
optionally ignoring error values (options 2, 3, 6, 7). -}
aggregateFn : List Arg -> Value
aggregateFn args =
    case args of
        fnArg :: optArg :: dataArgs ->
            let
                fnum =
                    round (Result.withDefault 0 (Value.toNumber (firstValue fnArg)))

                opt =
                    round (Result.withDefault 0 (Value.toNumber (firstValue optArg)))

                name =
                    aggregateName fnum
            in
            if name == "" then
                VError ValueErr

            else if List.member opt [ 2, 3, 6, 7 ] then
                call name (List.map dropErrors dataArgs)

            else
                case firstErr (flatten dataArgs) of
                    Just er ->
                        VError er

                    Nothing ->
                        call name dataArgs

        _ ->
            VError ValueErr


aggregateName : Int -> String
aggregateName n =
    case n of
        1 ->
            "AVERAGE"

        2 ->
            "COUNT"

        3 ->
            "COUNTA"

        4 ->
            "MAX"

        5 ->
            "MIN"

        6 ->
            "PRODUCT"

        7 ->
            "STDEV"

        8 ->
            "STDEVP"

        9 ->
            "SUM"

        10 ->
            "VAR"

        11 ->
            "VARP"

        12 ->
            "MEDIAN"

        13 ->
            "MODE"

        14 ->
            "LARGE"

        15 ->
            "SMALL"

        16 ->
            "PERCENTILE"

        17 ->
            "QUARTILE"

        _ ->
            ""


dropErrors : Arg -> Arg
dropErrors arg =
    case arg of
        Scalar v ->
            if Value.isError v then
                Matrix []

            else
                Scalar v

        Matrix rows ->
            Matrix (List.map (List.filter (\v -> not (Value.isError v))) rows)


percentRankFn : List Arg -> Value
percentRankFn args =
    case args of
        arrArg :: xArg :: _ ->
            percentRank (List.sort (numbersOnly (flatten [ arrArg ]))) (Result.withDefault 0 (Value.toNumber (firstValue xArg)))

        _ ->
            VError ValueErr


percentRank : List Float -> Float -> Value
percentRank sorted x =
    case sorted of
        [] ->
            VError NumErr

        _ ->
            let
                n =
                    List.length sorted

                lo =
                    Maybe.withDefault 0 (List.minimum sorted)

                hi =
                    Maybe.withDefault 0 (List.maximum sorted)
            in
            if n == 1 || x <= lo then
                VNumber 0

            else if x >= hi then
                VNumber 1

            else
                VNumber (interpRank sorted x / toFloat (n - 1))


interpRank : List Float -> Float -> Float
interpRank sorted x =
    case sorted of
        a :: b :: rest ->
            if x >= a && x < b then
                (x - a) / (b - a)

            else
                1 + interpRank (b :: rest) x

        _ ->
            0


trimMeanFn : List Arg -> Value
trimMeanFn args =
    case args of
        arrArg :: pctArg :: _ ->
            let
                ns =
                    List.sort (numbersOnly (flatten [ arrArg ]))

                p =
                    Result.withDefault 0 (Value.toNumber (firstValue pctArg))

                n =
                    List.length ns

                k =
                    floor (toFloat n * p / 2)

                trimmed =
                    List.drop k (List.take (n - k) ns)
            in
            case trimmed of
                [] ->
                    VError NumErr

                _ ->
                    VNumber (List.sum trimmed / toFloat (List.length trimmed))

        _ ->
            VError ValueErr


covarianceFn : List Arg -> Value
covarianceFn args =
    case args of
        xArg :: yArg :: _ ->
            let
                pairs =
                    zip (numbersOnly (flatten [ xArg ])) (numbersOnly (flatten [ yArg ]))

                n =
                    List.length pairs
            in
            if n == 0 then
                VError DivZero

            else
                let
                    mx =
                        List.sum (List.map Tuple.first pairs) / toFloat n

                    my =
                        List.sum (List.map Tuple.second pairs) / toFloat n
                in
                VNumber (List.sum (List.map (\( a, b ) -> (a - mx) * (b - my)) pairs) / toFloat n)

        _ ->
            VError ValueErr


-- FINANCIAL DEPTH ------------------------------------------------------------


{-| Read an argument as a truthy flag (the cumulative flag of a distribution); default True. -}
boolArg : Int -> List Arg -> Bool
boolArg i args =
    case List.head (List.drop i args) of
        Just a ->
            Result.withDefault True (Value.toBool (firstValue a))

        Nothing ->
            True


probResult : Float -> Value
probResult x =
    if isNaN x || isInfinite x then
        VError NumErr

    else
        VNumber x


{-| Newton's method for a 1-D root of `f` from `x0`; `Nothing` if it doesn't converge. -}
solveNewton : (Float -> Float) -> Float -> Int -> Maybe Float
solveNewton f x0 iters =
    if iters <= 0 then
        Nothing

    else
        let
            fx =
                f x0
        in
        if abs fx < 1.0e-9 then
            Just x0

        else
            let
                h =
                    1.0e-6

                deriv =
                    (f (x0 + h) - fx) / h
            in
            if deriv == 0 then
                Nothing

            else
                solveNewton f (x0 - fx / deriv) (iters - 1)


rateFn : List Arg -> Value
rateFn args =
    case ( sc 0 args, sc 1 args, sc 2 args ) of
        ( Just nperiods, Just payment, Just presentValue ) ->
            let
                fvv =
                    opt 3 0 args

                typ =
                    opt 4 0 args

                guess =
                    opt 5 0.1 args

                f r =
                    if abs r < 1.0e-10 then
                        presentValue + payment * nperiods + fvv

                    else
                        presentValue * (1 + r) ^ nperiods + payment * (1 + r * typ) * ((1 + r) ^ nperiods - 1) / r + fvv
            in
            case solveNewton f guess 100 of
                Just r ->
                    VNumber r

                Nothing ->
                    VError NumErr

        _ ->
            VError ValueErr


{-| The interest portion of payment `per` of a loan (Excel `IPMT`). -}
ipmtVal : Float -> Int -> Float -> Float -> Float -> Float -> Float
ipmtVal rate per nperiods presentValue fvv typ =
    if rate == 0 then
        0

    else
        let
            pay =
                pmt rate nperiods presentValue fvv typ

            balance k =
                presentValue * (1 + rate) ^ toFloat k + pay * ((1 + rate) ^ toFloat k - 1) / rate
        in
        if per == 1 then
            if typ == 1 then
                0

            else
                -(presentValue * rate)

        else
            let
                raw =
                    -(balance (per - 1) * rate)
            in
            if typ == 1 then
                raw / (1 + rate)

            else
                raw


ipmtFn : List Arg -> Value
ipmtFn args =
    case ( sc 0 args, sc 1 args, sc 2 args, sc 3 args ) of
        ( Just rate, Just perF, Just nperiods, Just presentValue ) ->
            let
                per =
                    round perF
            in
            if per < 1 || toFloat per > nperiods then
                VError NumErr

            else
                VNumber (ipmtVal rate per nperiods presentValue (opt 4 0 args) (opt 5 0 args))

        _ ->
            VError ValueErr


ppmtFn : List Arg -> Value
ppmtFn args =
    case ( sc 0 args, sc 1 args, sc 2 args, sc 3 args ) of
        ( Just rate, Just perF, Just nperiods, Just presentValue ) ->
            let
                per =
                    round perF

                fvv =
                    opt 4 0 args

                typ =
                    opt 5 0 args
            in
            if per < 1 || toFloat per > nperiods then
                VError NumErr

            else
                VNumber (pmt rate nperiods presentValue fvv typ - ipmtVal rate per nperiods presentValue fvv typ)

        _ ->
            VError ValueErr


cumipmtFn : List Arg -> Value
cumipmtFn args =
    case ( sc 0 args, sc 1 args, sc 2 args ) of
        ( Just rate, Just nperiods, Just presentValue ) ->
            case ( sc 3 args, sc 4 args ) of
                ( Just startF, Just endF ) ->
                    let
                        s =
                            round startF

                        en =
                            round endF

                        typ =
                            opt 5 0 args
                    in
                    if s < 1 || en < s then
                        VError NumErr

                    else
                        VNumber (List.sum (List.map (\p -> ipmtVal rate p nperiods presentValue 0 typ) (List.range s en)))

                _ ->
                    VError ValueErr

        _ ->
            VError ValueErr


ddbFn : List Arg -> Value
ddbFn args =
    case ( sc 0 args, sc 1 args, sc 2 args ) of
        ( Just cost, Just salvage, Just life ) ->
            case sc 3 args of
                Just periodF ->
                    let
                        period =
                            round periodF
                    in
                    if life <= 0 || period < 1 then
                        VError NumErr

                    else
                        VNumber (ddbStep cost salvage life (opt 4 2 args) period 1 0)

                Nothing ->
                    VError ValueErr

        _ ->
            VError ValueErr


ddbStep : Float -> Float -> Float -> Float -> Int -> Int -> Float -> Float
ddbStep cost salvage life factor period i accum =
    let
        dep =
            min ((cost - accum) * factor / life) (max 0 (cost - salvage - accum))
    in
    if i >= period then
        dep

    else
        ddbStep cost salvage life factor period (i + 1) (accum + dep)


mirrFn : List Arg -> Value
mirrFn args =
    case args of
        valsArg :: _ ->
            case orderedNumbers [ valsArg ] of
                Ok nums ->
                    let
                        finRate =
                            Maybe.withDefault 0 (sc 1 args)

                        reinvRate =
                            Maybe.withDefault 0 (sc 2 args)

                        n =
                            List.length nums

                        pvNeg =
                            List.sum (List.indexedMap (\i v -> ifLess0 v / (1 + finRate) ^ toFloat i) nums)

                        fvPos =
                            List.sum (List.indexedMap (\i v -> ifMore0 v * (1 + reinvRate) ^ toFloat (n - 1 - i)) nums)
                    in
                    if pvNeg == 0 || n < 2 then
                        VError DivZero

                    else
                        VNumber ((-fvPos / pvNeg) ^ (1 / toFloat (n - 1)) - 1)

                Err e ->
                    VError e

        _ ->
            VError ValueErr


ifLess0 : Float -> Float
ifLess0 v =
    if v < 0 then
        v

    else
        0


ifMore0 : Float -> Float
ifMore0 v =
    if v > 0 then
        v

    else
        0


argNums : Int -> List Arg -> Maybe (List Float)
argNums i args =
    Maybe.map (\a -> numbersOnly (flatten [ a ])) (List.head (List.drop i args))


xnpvFn : List Arg -> Value
xnpvFn args =
    case ( sc 0 args, argNums 1 args, argNums 2 args ) of
        ( Just rate, Just values, Just dates ) ->
            case List.head dates of
                Just d0 ->
                    VNumber (List.sum (List.map2 (\v d -> v / (1 + rate) ^ ((d - d0) / 365)) values dates))

                Nothing ->
                    VError NumErr

        _ ->
            VError ValueErr


xirrFn : List Arg -> Value
xirrFn args =
    case ( argNums 0 args, argNums 1 args ) of
        ( Just values, Just dates ) ->
            case List.head dates of
                Just d0 ->
                    let
                        f r =
                            List.sum (List.map2 (\v d -> v / (1 + r) ^ ((d - d0) / 365)) values dates)
                    in
                    case solveNewton f (opt 2 0.1 args) 100 of
                        Just r ->
                            VNumber r

                        Nothing ->
                            VError NumErr

                Nothing ->
                    VError NumErr

        _ ->
            VError ValueErr



-- STATISTICAL DISTRIBUTIONS --------------------------------------------------


scaleFor : Bool -> Float -> Float
scaleFor cumulative sd =
    if cumulative then
        1

    else
        sd


{-| The error function (Abramowitz & Stegun 7.1.26, |error| < 1.5e-7). -}
erf : Float -> Float
erf x =
    let
        t =
            1 / (1 + 0.3275911 * abs x)

        y =
            1 - (((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t) * e ^ (-(x * x))
    in
    if x < 0 then
        -y

    else
        y


{-| Standard-normal density (`cumulative = False`) or CDF (`True`). -}
normSDist : Float -> Bool -> Float
normSDist z cumulative =
    if cumulative then
        0.5 * (1 + erf (z / sqrt 2))

    else
        e ^ (-(z * z) / 2) / sqrt (2 * pi)


{-| Inverse standard-normal CDF (Acklam's rational approximation). -}
normSInv : Float -> Float
normSInv p =
    if p <= 0 then
        -1 / 0

    else if p >= 1 then
        1 / 0

    else if p < 0.02425 then
        let
            q =
                sqrt (-2 * logBase e p)
        in
        (((((invC 0 * q + invC 1) * q + invC 2) * q + invC 3) * q + invC 4) * q + invC 5)
            / ((((invD 0 * q + invD 1) * q + invD 2) * q + invD 3) * q + 1)

    else if p <= 0.97575 then
        let
            q =
                p - 0.5

            r =
                q * q
        in
        (((((invA 0 * r + invA 1) * r + invA 2) * r + invA 3) * r + invA 4) * r + invA 5)
            * q
            / (((((invB 0 * r + invB 1) * r + invB 2) * r + invB 3) * r + invB 4) * r + 1)

    else
        let
            q =
                sqrt (-2 * logBase e (1 - p))
        in
        -(((((invC 0 * q + invC 1) * q + invC 2) * q + invC 3) * q + invC 4) * q + invC 5)
            / ((((invD 0 * q + invD 1) * q + invD 2) * q + invD 3) * q + 1)


invA : Int -> Float
invA i =
    nthCoef i [ -3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2, 1.38357751867269e2, -3.066479806614716e1, 2.506628277459239 ]


invB : Int -> Float
invB i =
    nthCoef i [ -5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2, 6.680131188771972e1, -1.328068155288572e1 ]


invC : Int -> Float
invC i =
    nthCoef i [ -7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783 ]


invD : Int -> Float
invD i =
    nthCoef i [ 7.784695709041462e-3, 3.224671290700398e-1, 2.445134137142996, 3.754408661907416 ]


nthCoef : Int -> List Float -> Float
nthCoef i xs =
    Maybe.withDefault 0 (List.head (List.drop i xs))


binomDist : Int -> Int -> Float -> Bool -> Float
binomDist k n p cumulative =
    if cumulative then
        List.sum (List.map (\i -> binomPmf i n p) (List.range 0 k))

    else
        binomPmf k n p


binomPmf : Int -> Int -> Float -> Float
binomPmf k n p =
    combinF n k * p ^ toFloat k * (1 - p) ^ toFloat (n - k)


combinF : Int -> Int -> Float
combinF n k =
    List.foldl (\i acc -> acc * toFloat (n - i) / toFloat (i + 1)) 1 (List.range 0 (k - 1))


poissonDist : Int -> Float -> Bool -> Float
poissonDist k mean cumulative =
    if cumulative then
        List.sum (List.map (\i -> poissonPmf i mean) (List.range 0 k))

    else
        poissonPmf k mean


poissonPmf : Int -> Float -> Float
poissonPmf k mean =
    e ^ (-mean) * mean ^ toFloat k / factF k


factF : Int -> Float
factF n =
    List.foldl (\i acc -> acc * toFloat i) 1 (List.range 1 n)



-- ENGINEERING, BASE & UNIT CONVERSION ----------------------------------------


numbersMatrix : List (List Value) -> List (List Float)
numbersMatrix m =
    List.map (List.map (\v -> Result.withDefault 0 (Value.toNumber v))) m


determinant : List (List Float) -> Value
determinant m =
    let
        n =
            List.length m
    in
    if n == 0 || List.any (\row -> List.length row /= n) m then
        VError ValueErr

    else
        VNumber (detCofactor m)


detCofactor : List (List Float) -> Float
detCofactor rows =
    case rows of
        [] ->
            1

        firstRow :: _ ->
            if List.length firstRow == 1 then
                Maybe.withDefault 0 (List.head firstRow)

            else
                List.sum
                    (List.indexedMap
                        (\j x ->
                            (if modBy 2 j == 0 then
                                1

                             else
                                -1
                            )
                                * x
                                * detCofactor (List.map (removeAt j) (List.drop 1 rows))
                        )
                        firstRow
                    )


removeAt : Int -> List a -> List a
removeAt j row =
    List.take j row ++ List.drop (j + 1) row


baseFromDec : Int -> List Arg -> Value
baseFromDec radix args =
    case sc 0 args of
        Just nF ->
            let
                n =
                    round nF
            in
            if n < 0 then
                VError NumErr

            else
                let
                    str =
                        toBaseString radix n
                in
                case sc 1 args of
                    Just placesF ->
                        VText (String.padLeft (round placesF) '0' str)

                    Nothing ->
                        VText str

        _ ->
            VError ValueErr


toBaseString : Int -> Int -> String
toBaseString radix n =
    if n == 0 then
        "0"

    else
        String.fromList (toBaseDigits radix n [])


toBaseDigits : Int -> Int -> List Char -> List Char
toBaseDigits radix n acc =
    if n <= 0 then
        acc

    else
        toBaseDigits radix (n // radix) (digitChar (modBy radix n) :: acc)


digitChar : Int -> Char
digitChar d =
    if d < 10 then
        Char.fromCode (Char.toCode '0' + d)

    else
        Char.fromCode (Char.toCode 'A' + d - 10)


baseToDec : Int -> List Arg -> Value
baseToDec radix args =
    case args of
        a :: _ ->
            case parseBase radix (String.toList (String.toUpper (Value.toText (firstValue a)))) 0 of
                Just n ->
                    VNumber (toFloat n)

                Nothing ->
                    VError NumErr

        [] ->
            VError ValueErr


parseBase : Int -> List Char -> Int -> Maybe Int
parseBase radix chars acc =
    case chars of
        [] ->
            Just acc

        c :: rest ->
            case digitValue c of
                Just d ->
                    if d < radix then
                        parseBase radix rest (acc * radix + d)

                    else
                        Nothing

                Nothing ->
                    Nothing


digitValue : Char -> Maybe Int
digitValue c =
    if Char.isDigit c then
        Just (Char.toCode c - Char.toCode '0')

    else if c >= 'A' && c <= 'F' then
        Just (Char.toCode c - Char.toCode 'A' + 10)

    else
        Nothing


bitOp : (Int -> Int -> Int) -> List Arg -> Value
bitOp f args =
    case ( sc 0 args, sc 1 args ) of
        ( Just a, Just b ) ->
            VNumber (toFloat (f (round a) (round b)))

        _ ->
            VError ValueErr


convertFn : List Arg -> Value
convertFn args =
    case ( sc 0 args, List.drop 1 args ) of
        ( Just value, fromArg :: toArg :: _ ) ->
            case convertUnits value (Value.toText (firstValue fromArg)) (Value.toText (firstValue toArg)) of
                Just r ->
                    VNumber r

                Nothing ->
                    VError NA

        _ ->
            VError ValueErr


convertUnits : Float -> String -> String -> Maybe Float
convertUnits value from to =
    if isTemp from && isTemp to then
        Just (toTemp to (fromTemp from value))

    else
        case ( unitFactor from, unitFactor to ) of
            ( Just ( qa, fa ), Just ( qb, fb ) ) ->
                if qa == qb then
                    Just (value * fa / fb)

                else
                    Nothing

            _ ->
                Nothing


isTemp : String -> Bool
isTemp u =
    List.member u [ "C", "F", "K", "cel", "fah", "kel" ]


{-| A temperature to kelvin. -}
fromTemp : String -> Float -> Float
fromTemp u v =
    case u of
        "F" ->
            (v - 32) * 5 / 9 + 273.15

        "fah" ->
            (v - 32) * 5 / 9 + 273.15

        "C" ->
            v + 273.15

        "cel" ->
            v + 273.15

        _ ->
            v


{-| Kelvin to a temperature unit. -}
toTemp : String -> Float -> Float
toTemp u k =
    case u of
        "F" ->
            (k - 273.15) * 9 / 5 + 32

        "fah" ->
            (k - 273.15) * 9 / 5 + 32

        "C" ->
            k - 273.15

        "cel" ->
            k - 273.15

        _ ->
            k


{-| A unit's `(quantity, factor-to-base-unit)`, for linear conversions. -}
unitFactor : String -> Maybe ( String, Float )
unitFactor u =
    case u of
        "m" ->
            Just ( "len", 1 )

        "km" ->
            Just ( "len", 1000 )

        "cm" ->
            Just ( "len", 0.01 )

        "mm" ->
            Just ( "len", 0.001 )

        "in" ->
            Just ( "len", 0.0254 )

        "ft" ->
            Just ( "len", 0.3048 )

        "yd" ->
            Just ( "len", 0.9144 )

        "mi" ->
            Just ( "len", 1609.344 )

        "g" ->
            Just ( "mass", 1 )

        "kg" ->
            Just ( "mass", 1000 )

        "lbm" ->
            Just ( "mass", 453.59237 )

        "ozm" ->
            Just ( "mass", 28.349523125 )

        "sec" ->
            Just ( "time", 1 )

        "s" ->
            Just ( "time", 1 )

        "min" ->
            Just ( "time", 60 )

        "hr" ->
            Just ( "time", 3600 )

        "day" ->
            Just ( "time", 86400 )

        _ ->
            Nothing
