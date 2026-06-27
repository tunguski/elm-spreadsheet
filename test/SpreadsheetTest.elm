module SpreadsheetTest exposing (suite)

{-| The elm-spreadsheet test suite.

The engine is pure — formulas evaluate to values with no side effects — so every
behaviour is checked headlessly: parse a formula, evaluate it against an in-memory sheet,
assert the value/format/style. The suite spans the whole stack:

  - value coercions and A1 addressing
  - the formula parser (operators, precedence, ranges, functions)
  - the function library across every category (math, stats, text, logical, lookup, date)
  - number/date formatting and the `TEXT` code interpreter
  - static + conditional styling
  - the dependency graph, sync recalculation, circular detection
  - the async/visible-first recalculator agreeing with the sync path

-}

import Dict exposing (Dict)
import Expect
import Fuzz
import Set
import Spreadsheet.Ast exposing (Expr)
import Spreadsheet.Csv as Csv
import Spreadsheet.Deps as Deps
import Spreadsheet.Export as Export
import Spreadsheet.Find as Find
import Spreadsheet.Eval as Eval
import Spreadsheet.Format as Format
import Spreadsheet.Parser as Parser
import Spreadsheet.Recalc as Recalc
import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Refactor as Refactor
import Spreadsheet.Render as Render
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Style as Style
import Spreadsheet.Validation as Validation
import Spreadsheet.Value as Value exposing (Error(..), Value(..))
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "elm-spreadsheet"
        [ valueTests
        , refTests
        , parserTests
        , arithmeticTests
        , comparisonTests
        , mathFnTests
        , statsFnTests
        , textFnTests
        , logicalFnTests
        , lookupFnTests
        , infoFnTests
        , dateFnTests
        , errorTests
        , formatTests
        , styleTests
        , depsTests
        , recalcTests
        , asyncTests
        , absRefTests
        , renderTests
        , refactorTests
        , structuralTests
        , clipboardTests
        , fillTests
        , sortFilterTests
        , nameTests
        , csvTests
        , financeTests
        , analysisFnTests
        , exportTests
        , notesTests
        , mergeTests
        , validationTests
        , findTests
        ]



-- HELPERS --------------------------------------------------------------------


{-| A1 string to a ref (defaults to A1 on parse failure, so tests fail loudly elsewhere). -}
at : String -> Ref
at a1 =
    Maybe.withDefault { col = 0, row = 0 } (Ref.fromA1 a1)


{-| Build an evaluation context from explicit A1→value bindings. -}
ctxFrom : List ( String, Value ) -> Eval.Context
ctxFrom pairs =
    let
        d =
            Dict.fromList (List.map (\( a, v ) -> ( ( (at a).col, (at a).row ), v )) pairs)
    in
    { lookup = \ref -> Maybe.withDefault VEmpty (Dict.get ( ref.col, ref.row ) d)
    , self = { col = 0, row = 0 }
    , names = \_ -> Nothing
    }


{-| Evaluate a formula against the given cell bindings. -}
ev : List ( String, Value ) -> String -> Value
ev cells formula =
    Eval.evalString (ctxFrom cells) formula


{-| Evaluate a formula with no cell references. -}
ev0 : String -> Value
ev0 formula =
    ev [] formula


{-| A recalculated sheet built from A1→raw-input bindings. -}
sheetWith : List ( String, String ) -> Sheet
sheetWith pairs =
    Sheet.empty 200 30
        |> Sheet.setRawMany (List.map (\( a, r ) -> ( at a, r )) pairs)
        |> Sheet.recalcAll


valOf : String -> Sheet -> Value
valOf a1 s =
    Sheet.valueAt (at a1) s


{-| Compare two values, normalising numbers first.

This backend distinguishes a whole-number `Float` *literal* (e.g. `VNumber 5` written in
source) from the same value produced by computation (`String.toFloat "5"`), so naive
`Expect.equal` reports spurious "expected 5 but got 5" failures. Round-tripping every
number through `String.fromFloat`/`toFloat` puts both sides in the computed
representation, so equality is meaningful. -}
expectVal : Value -> Value -> Expect.Expectation
expectVal expected actual =
    Expect.equal (normVal expected) (normVal actual)


expectVal2 : ( Value, Value ) -> ( Value, Value ) -> Expect.Expectation
expectVal2 ( a, b ) ( c, d ) =
    Expect.equal ( normVal a, normVal b ) ( normVal c, normVal d )


normVal : Value -> Value
normVal v =
    case v of
        VNumber n ->
            VNumber (Maybe.withDefault n (String.toFloat (String.fromFloat n)))

        _ ->
            v


{-| Wrap a `toNumber`-style result as a value for normalised comparison. -}
numResult : Result Error Float -> Value
numResult r =
    case r of
        Ok n ->
            VNumber n

        Err e ->
            VError e



-- VALUE ----------------------------------------------------------------------


valueTests : Test
valueTests =
    describe "Value"
        [ test "fromString parses integers" <|
            \_ -> expectVal (VNumber 42) (Value.fromString "42")
        , test "fromString parses decimals" <|
            \_ -> expectVal (VNumber 3.14) (Value.fromString "3.14")
        , test "fromString parses booleans case-insensitively" <|
            \_ -> expectVal (VBool True) (Value.fromString "true")
        , test "fromString parses a trailing-percent literal" <|
            \_ -> expectVal (VNumber 0.25) (Value.fromString "25%")
        , test "fromString keeps non-numeric text" <|
            \_ -> expectVal (VText "hello") (Value.fromString "hello")
        , test "empty string is VEmpty" <|
            \_ -> expectVal VEmpty (Value.fromString "   ")
        , test "toNumber coerces a numeric string" <|
            \_ -> expectVal (VNumber 5) (numResult (Value.toNumber (VText "5")))
        , test "toNumber of empty is 0" <|
            \_ -> Expect.equal (Ok 0) (Value.toNumber VEmpty)
        , test "toNumber of text errors" <|
            \_ -> Expect.equal (Err ValueErr) (Value.toNumber (VText "abc"))
        , test "toText of a whole number drops the .0" <|
            \_ -> Expect.equal "5" (Value.toText (VNumber 5))
        , test "equalValue compares text case-insensitively" <|
            \_ -> Expect.equal True (Value.equalValue (VText "Abc") (VText "aBC"))
        , test "equalValue coerces number and boolean" <|
            \_ -> Expect.equal True (Value.equalValue (VNumber 1) (VBool True))
        , test "errorText renders the sentinel" <|
            \_ -> Expect.equal "#DIV/0!" (Value.errorText DivZero)
        ]



-- REF ------------------------------------------------------------------------


refTests : Test
refTests =
    describe "Ref"
        [ test "colToString A/Z/AA" <|
            \_ ->
                Expect.equal [ "A", "Z", "AA", "AB" ]
                    [ Ref.colToString 0, Ref.colToString 25, Ref.colToString 26, Ref.colToString 27 ]
        , test "fromA1 parses a simple ref" <|
            \_ -> Expect.equal (Just { col = 0, row = 0 }) (Ref.fromA1 "A1")
        , test "fromA1 parses a multi-letter, multi-digit ref" <|
            \_ -> Expect.equal (Just { col = 27, row = 99 }) (Ref.fromA1 "AB100")
        , test "fromA1 tolerates absolute markers" <|
            \_ -> Expect.equal (Just { col = 2, row = 4 }) (Ref.fromA1 "$C$5")
        , test "toA1 round-trips" <|
            \_ -> Expect.equal "C5" (Ref.toA1 { col = 2, row = 4 })
        , test "cellsOf enumerates a range row-major" <|
            \_ ->
                Expect.equal
                    [ { col = 0, row = 0 }, { col = 1, row = 0 }, { col = 0, row = 1 }, { col = 1, row = 1 } ]
                    (Ref.cellsOf { start = { col = 0, row = 0 }, end = { col = 1, row = 1 } })
        , test "width and height" <|
            \_ ->
                Expect.equal ( 3, 2 )
                    ( Ref.width { start = { col = 0, row = 0 }, end = { col = 2, row = 1 } }
                    , Ref.height { start = { col = 0, row = 0 }, end = { col = 2, row = 1 } }
                    )
        , test "contains" <|
            \_ ->
                Expect.equal True
                    (Ref.contains { start = { col = 0, row = 0 }, end = { col = 5, row = 5 } } { col = 3, row = 2 })
        , fuzz (Fuzz.intRange 0 1000) "colToString/colFromString round-trip" <|
            \n -> Expect.equal (Just n) (Ref.colFromString (Ref.colToString n))
        , fuzz2 (Fuzz.intRange 0 50) (Fuzz.intRange 0 50) "A1 round-trips" <|
            \c r -> Expect.equal (Just { col = c, row = r }) (Ref.fromA1 (Ref.toA1 { col = c, row = r }))
        ]



-- PARSER ---------------------------------------------------------------------


parserTests : Test
parserTests =
    describe "Parser"
        [ test "parses a number" <|
            \_ -> Expect.equal True (isOk (Parser.parse "1"))
        , test "parses a string literal" <|
            \_ -> Expect.equal True (isOk (Parser.parse "\"hi\""))
        , test "parses a function call" <|
            \_ -> Expect.equal True (isOk (Parser.parse "SUM(A1:A3, 5)"))
        , test "parses nested calls and operators" <|
            \_ -> Expect.equal True (isOk (Parser.parse "IF(A1>0, MAX(B1:B5), -1)"))
        , test "rejects an unbalanced paren" <|
            \_ -> Expect.equal True (isErr (Parser.parse "SUM(A1"))
        , test "parseFormula strips a leading =" <|
            \_ -> Expect.equal True (isOk (Parser.parseFormula "=1+2"))
        , fuzz (Fuzz.intRange -1000 1000) "any integer literal parses and evaluates to itself" <|
            \n -> expectVal (VNumber (toFloat n)) (ev0 (String.fromInt n))
        ]


isOk : Result e a -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result e a -> Bool
isErr r =
    not (isOk r)



-- ARITHMETIC -----------------------------------------------------------------


arithmeticTests : Test
arithmeticTests =
    describe "arithmetic & operators"
        [ test "addition" <| \_ -> expectVal (VNumber 3) (ev0 "=1+2")
        , test "precedence: * before +" <| \_ -> expectVal (VNumber 14) (ev0 "=2+3*4")
        , test "parentheses override precedence" <| \_ -> expectVal (VNumber 20) (ev0 "=(2+3)*4")
        , test "division" <| \_ -> expectVal (VNumber 2.5) (ev0 "=10/4")
        , test "division by zero" <| \_ -> expectVal (VError DivZero) (ev0 "=1/0")
        , test "power is right-associative" <| \_ -> expectVal (VNumber 512) (ev0 "=2^3^2")
        , test "unary minus binds tighter than power (-2^2 = 4)" <| \_ -> expectVal (VNumber 4) (ev0 "=-2^2")
        , test "percent postfix" <| \_ -> expectVal (VNumber 0.5) (ev0 "=50%")
        , test "percent in an expression" <| \_ -> expectVal (VNumber 1) (ev0 "=2*50%")
        , test "concatenation with &" <| \_ -> expectVal (VText "ab") (ev0 "=\"a\"&\"b\"")
        , test "concatenation coerces numbers" <| \_ -> expectVal (VText "12") (ev0 "=1&2")
        , test "reads a cell reference" <| \_ -> expectVal (VNumber 7) (ev [ ( "A1", VNumber 7 ) ] "=A1")
        , test "combines references" <| \_ -> expectVal (VNumber 10) (ev [ ( "A1", VNumber 4 ), ( "A2", VNumber 6 ) ] "=A1+A2")
        ]



-- COMPARISON -----------------------------------------------------------------


comparisonTests : Test
comparisonTests =
    describe "comparison operators"
        [ test "less-than" <| \_ -> expectVal (VBool True) (ev0 "=1<2")
        , test "greater-or-equal" <| \_ -> expectVal (VBool True) (ev0 "=2>=2")
        , test "not-equal with <>" <| \_ -> expectVal (VBool True) (ev0 "=1<>2")
        , test "equality is case-insensitive on text" <| \_ -> expectVal (VBool True) (ev0 "=\"a\"=\"A\"")
        , test "number sorts before text" <| \_ -> expectVal (VBool True) (ev0 "=5<\"apple\"")
        ]



-- MATH FUNCTIONS -------------------------------------------------------------


mathFnTests : Test
mathFnTests =
    describe "math functions"
        [ test "ABS" <| \_ -> expectVal (VNumber 5) (ev0 "=ABS(-5)")
        , test "SQRT" <| \_ -> expectVal (VNumber 3) (ev0 "=SQRT(9)")
        , test "SQRT of negative is #NUM!" <| \_ -> expectVal (VError NumErr) (ev0 "=SQRT(-1)")
        , test "POWER" <| \_ -> expectVal (VNumber 8) (ev0 "=POWER(2,3)")
        , test "MOD" <| \_ -> expectVal (VNumber 1) (ev0 "=MOD(7,3)")
        , test "MOD follows divisor sign" <| \_ -> expectVal (VNumber 2) (ev0 "=MOD(-1,3)")
        , test "INT floors" <| \_ -> expectVal (VNumber 3) (ev0 "=INT(3.9)")
        , test "ROUND half away from zero" <| \_ -> expectVal (VNumber 3) (ev0 "=ROUND(2.5,0)")
        , test "ROUND to decimals" <| \_ -> expectVal (VNumber 2.35) (ev0 "=ROUND(2.345,2)")
        , test "ROUNDUP" <| \_ -> expectVal (VNumber 2.1) (ev0 "=ROUNDUP(2.01,1)")
        , test "ROUNDDOWN" <| \_ -> expectVal (VNumber 2) (ev0 "=ROUNDDOWN(2.99,0)")
        , test "GCD" <| \_ -> expectVal (VNumber 6) (ev0 "=GCD(12,18)")
        , test "LCM" <| \_ -> expectVal (VNumber 36) (ev0 "=LCM(12,18)")
        , test "FACT" <| \_ -> expectVal (VNumber 120) (ev0 "=FACT(5)")
        , test "COMBIN" <| \_ -> expectVal (VNumber 10) (ev0 "=COMBIN(5,2)")
        , test "PI is available" <| \_ -> expectVal (VBool True) (ev0 "=PI()>3.14")
        , test "SUM of literal args" <| \_ -> expectVal (VNumber 6) (ev0 "=SUM(1,2,3)")
        , test "PRODUCT" <| \_ -> expectVal (VNumber 24) (ev0 "=PRODUCT(2,3,4)")
        , test "nested functions" <| \_ -> expectVal (VNumber 5) (ev0 "=SUM(ABS(-2),MAX(1,3))")
        ]



-- STATS FUNCTIONS (range-based, via Sheet) -----------------------------------


statsFnTests : Test
statsFnTests =
    describe "statistical functions over ranges"
        [ test "SUM over a range" <|
            \_ -> expectVal (VNumber 6) (valOf "B1" (numColumn [ "1", "2", "3" ] "=SUM(A1:A3)"))
        , test "AVERAGE" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "1", "2", "3" ] "=AVERAGE(A1:A3)"))
        , test "AVERAGE ignores text in the range" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "1", "x", "3" ] "=AVERAGE(A1:A3)"))
        , test "COUNT counts only numbers" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "1", "x", "3" ] "=COUNT(A1:A3)"))
        , test "COUNTA counts non-empty" <|
            \_ -> expectVal (VNumber 3) (valOf "B1" (numColumn [ "1", "x", "3" ] "=COUNTA(A1:A3)"))
        , test "MAX" <|
            \_ -> expectVal (VNumber 9) (valOf "B1" (numColumn [ "4", "9", "1" ] "=MAX(A1:A3)"))
        , test "MIN" <|
            \_ -> expectVal (VNumber 1) (valOf "B1" (numColumn [ "4", "9", "1" ] "=MIN(A1:A3)"))
        , test "MEDIAN odd count" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "3", "1", "2" ] "=MEDIAN(A1:A3)"))
        , test "MEDIAN even count" <|
            \_ -> expectVal (VNumber 2.5) (valOf "B1" (numColumn [ "1", "2", "3", "4" ] "=MEDIAN(A1:A4)"))
        , test "VAR (sample)" <|
            \_ -> expectVal (VNumber 4) (valOf "B1" (numColumn [ "2", "4", "6" ] "=VAR(A1:A4)"))
        , test "STDEV (sample)" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "2", "4", "6" ] "=STDEV(A1:A4)"))
        , test "LARGE picks the k-th biggest" <|
            \_ -> expectVal (VNumber 4) (valOf "B1" (numColumn [ "5", "1", "4", "2" ] "=LARGE(A1:A4,2)"))
        , test "SMALL picks the k-th smallest" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "5", "1", "4", "2" ] "=SMALL(A1:A4,2)"))
        , test "COUNTIF with a comparison criterion" <|
            \_ -> expectVal (VNumber 2) (valOf "B1" (numColumn [ "1", "2", "3" ] "=COUNTIF(A1:A3,\">1\")"))
        , test "SUMIF with a comparison criterion" <|
            \_ -> expectVal (VNumber 5) (valOf "B1" (numColumn [ "1", "2", "3" ] "=SUMIF(A1:A3,\">1\")"))
        , test "AVERAGEIF" <|
            \_ -> expectVal (VNumber 2.5) (valOf "B1" (numColumn [ "1", "2", "3" ] "=AVERAGEIF(A1:A3,\">1\")"))
        ]


{-| A column A1.. filled with the given raw cells, plus a formula in B1. -}
numColumn : List String -> String -> Sheet
numColumn cells formula =
    sheetWith (List.indexedMap (\i v -> ( "A" ++ String.fromInt (i + 1), v )) cells ++ [ ( "B1", formula ) ])



-- TEXT FUNCTIONS -------------------------------------------------------------


textFnTests : Test
textFnTests =
    describe "text functions"
        [ test "LEN" <| \_ -> expectVal (VNumber 5) (ev0 "=LEN(\"hello\")")
        , test "LEFT" <| \_ -> expectVal (VText "he") (ev0 "=LEFT(\"hello\",2)")
        , test "RIGHT" <| \_ -> expectVal (VText "lo") (ev0 "=RIGHT(\"hello\",2)")
        , test "MID" <| \_ -> expectVal (VText "ell") (ev0 "=MID(\"hello\",2,3)")
        , test "UPPER" <| \_ -> expectVal (VText "HELLO") (ev0 "=UPPER(\"hello\")")
        , test "LOWER" <| \_ -> expectVal (VText "hello") (ev0 "=LOWER(\"HELLO\")")
        , test "PROPER" <| \_ -> expectVal (VText "Hello World") (ev0 "=PROPER(\"hello world\")")
        , test "TRIM collapses inner spaces" <| \_ -> expectVal (VText "a b") (ev0 "=TRIM(\"  a   b  \")")
        , test "CONCAT" <| \_ -> expectVal (VText "abc") (ev0 "=CONCAT(\"a\",\"b\",\"c\")")
        , test "CONCATENATE" <| \_ -> expectVal (VText "a1") (ev0 "=CONCATENATE(\"a\",1)")
        , test "TEXTJOIN with delimiter" <| \_ -> expectVal (VText "a-b-c") (ev0 "=TEXTJOIN(\"-\",TRUE,\"a\",\"b\",\"c\")")
        , test "REPT" <| \_ -> expectVal (VText "xxx") (ev0 "=REPT(\"x\",3)")
        , test "SUBSTITUTE" <| \_ -> expectVal (VText "a-b-c") (ev0 "=SUBSTITUTE(\"a b c\",\" \",\"-\")")
        , test "REPLACE" <| \_ -> expectVal (VText "abXYe") (ev0 "=REPLACE(\"abcde\",3,2,\"XY\")")
        , test "FIND is 1-based" <| \_ -> expectVal (VNumber 3) (ev0 "=FIND(\"c\",\"abcde\")")
        , test "SEARCH is case-insensitive" <| \_ -> expectVal (VNumber 1) (ev0 "=SEARCH(\"A\",\"abc\")")
        , test "EXACT" <| \_ -> expectVal (VBool False) (ev0 "=EXACT(\"a\",\"A\")")
        , test "VALUE parses a numeric string" <| \_ -> expectVal (VNumber 12.5) (ev0 "=VALUE(\"12.5\")")
        , test "CHAR/CODE round-trip" <| \_ -> expectVal (VNumber 65) (ev0 "=CODE(CHAR(65))")
        ]



-- LOGICAL FUNCTIONS ----------------------------------------------------------


logicalFnTests : Test
logicalFnTests =
    describe "logical functions"
        [ test "IF true branch" <| \_ -> expectVal (VText "yes") (ev0 "=IF(1>0,\"yes\",\"no\")")
        , test "IF false branch" <| \_ -> expectVal (VText "no") (ev0 "=IF(1<0,\"yes\",\"no\")")
        , test "IF does not evaluate the untaken branch (no DIV/0)" <|
            \_ -> expectVal (VNumber 1) (ev0 "=IF(TRUE,1,1/0)")
        , test "AND" <| \_ -> expectVal (VBool True) (ev0 "=AND(1>0,2>1)")
        , test "OR" <| \_ -> expectVal (VBool True) (ev0 "=OR(1<0,2>1)")
        , test "NOT" <| \_ -> expectVal (VBool False) (ev0 "=NOT(TRUE)")
        , test "XOR" <| \_ -> expectVal (VBool True) (ev0 "=XOR(TRUE,FALSE)")
        , test "IFERROR catches an error" <| \_ -> expectVal (VText "oops") (ev0 "=IFERROR(1/0,\"oops\")")
        , test "IFERROR passes a good value" <| \_ -> expectVal (VNumber 2) (ev0 "=IFERROR(1+1,\"oops\")")
        , test "IFNA only catches #N/A" <| \_ -> expectVal (VError DivZero) (ev0 "=IFNA(1/0,\"x\")")
        , test "IFS picks the first true" <| \_ -> expectVal (VText "b") (ev0 "=IFS(1>2,\"a\",2>1,\"b\")")
        , test "SWITCH matches a case" <| \_ -> expectVal (VText "two") (ev0 "=SWITCH(2,1,\"one\",2,\"two\",\"other\")")
        , test "SWITCH falls through to default" <| \_ -> expectVal (VText "other") (ev0 "=SWITCH(9,1,\"one\",2,\"two\",\"other\")")
        , test "CHOOSE selects by index" <| \_ -> expectVal (VText "b") (ev0 "=CHOOSE(2,\"a\",\"b\",\"c\")")
        ]



-- LOOKUP FUNCTIONS -----------------------------------------------------------


lookupFnTests : Test
lookupFnTests =
    describe "lookup functions"
        [ test "VLOOKUP exact match" <|
            \_ -> expectVal (VText "b") (valOf "D1" lookupSheet)
        , test "INDEX into a 2-D range" <|
            \_ -> expectVal (VText "c") (valOf "D2" lookupSheet)
        , test "MATCH exact" <|
            \_ -> expectVal (VNumber 3) (valOf "D3" lookupSheet)
        , test "HLOOKUP returns the value in the requested row of the matching column" <|
            \_ -> expectVal (VNumber 200) (valOf "D4" lookupSheet)
        , test "ROWS / COLUMNS of a range" <|
            \_ -> expectVal (VNumber 32) (valOf "D5" lookupSheet)
        ]


lookupSheet : Sheet
lookupSheet =
    sheetWith
        [ ( "A1", "1" ), ( "B1", "a" )
        , ( "A2", "2" ), ( "B2", "b" )
        , ( "A3", "3" ), ( "B3", "c" )

        -- a horizontal row for HLOOKUP: G1=10 G2=20 across columns
        , ( "G1", "10" ), ( "H1", "20" ), ( "I1", "30" )
        , ( "G2", "100" ), ( "H2", "200" ), ( "I2", "300" )

        -- formulas
        , ( "D1", "=VLOOKUP(2,A1:B3,2,FALSE)" )
        , ( "D2", "=INDEX(A1:B3,3,2)" )
        , ( "D3", "=MATCH(3,A1:A3,0)" )
        , ( "D4", "=HLOOKUP(20,G1:I2,2,FALSE)" )
        , ( "D5", "=ROWS(A1:A3)*10 + COLUMNS(G1:I1)-1" )
        ]



-- INFORMATION FUNCTIONS ------------------------------------------------------


infoFnTests : Test
infoFnTests =
    describe "information functions"
        [ test "ISNUMBER on a number" <| \_ -> expectVal (VBool True) (ev0 "=ISNUMBER(5)")
        , test "ISNUMBER on text" <| \_ -> expectVal (VBool False) (ev0 "=ISNUMBER(\"x\")")
        , test "ISTEXT" <| \_ -> expectVal (VBool True) (ev0 "=ISTEXT(\"x\")")
        , test "ISERROR" <| \_ -> expectVal (VBool True) (ev0 "=ISERROR(1/0)")
        , test "ISBLANK on an empty cell" <| \_ -> expectVal (VBool True) (ev [ ( "A1", VEmpty ) ] "=ISBLANK(A1)")
        , test "ISEVEN" <| \_ -> expectVal (VBool True) (ev0 "=ISEVEN(4)")
        , test "ISODD" <| \_ -> expectVal (VBool True) (ev0 "=ISODD(3)")
        , test "N coerces" <| \_ -> expectVal (VNumber 5) (ev0 "=N(5)")
        , test "TYPE of a number is 1" <| \_ -> expectVal (VNumber 1) (ev0 "=TYPE(5)")
        ]



-- DATE FUNCTIONS -------------------------------------------------------------


dateFnTests : Test
dateFnTests =
    describe "date functions"
        [ test "YEAR round-trips through DATE" <| \_ -> expectVal (VNumber 2020) (ev0 "=YEAR(DATE(2020,5,15))")
        , test "MONTH round-trips" <| \_ -> expectVal (VNumber 5) (ev0 "=MONTH(DATE(2020,5,15))")
        , test "DAY round-trips" <| \_ -> expectVal (VNumber 15) (ev0 "=DAY(DATE(2020,5,15))")
        , test "DAYS between two dates" <| \_ -> expectVal (VNumber 30) (ev0 "=DAYS(DATE(2020,1,31),DATE(2020,1,1))")
        , test "DATE normalises an overflow month" <| \_ -> expectVal (VNumber 2021) (ev0 "=YEAR(DATE(2020,13,1))")
        , test "WEEKDAY is in 1..7" <| \_ -> expectVal (VBool True) (ev0 "=AND(WEEKDAY(DATE(2020,5,15))>=1,WEEKDAY(DATE(2020,5,15))<=7)")
        ]



-- ERROR PROPAGATION ----------------------------------------------------------


errorTests : Test
errorTests =
    describe "error handling"
        [ test "errors propagate through arithmetic" <| \_ -> expectVal (VError DivZero) (ev0 "=1+(1/0)")
        , test "NA propagates" <| \_ -> expectVal (VError NA) (ev0 "=NA()+1")
        , test "unknown name is #NAME?" <| \_ -> expectVal (VError NameErr) (ev0 "=NOPE(1)")
        , test "bad reference text is #NAME? via bare name" <| \_ -> expectVal (VError NameErr) (ev0 "=zzz")
        , test "a malformed formula is #ERROR!" <| \_ -> expectVal (VError Value.Parse) (ev0 "=1+")
        , test "SUM propagates an error cell" <|
            \_ -> expectVal (VError DivZero) (valOf "B1" (numColumn [ "1", "=1/0", "3" ] "=SUM(A1:A3)"))
        ]



-- FORMAT ---------------------------------------------------------------------


formatTests : Test
formatTests =
    describe "Format"
        [ test "General renders a plain number" <|
            \_ -> Expect.equal "1234" (Format.format Format.General (VNumber 1234))
        , test "Number with 2 decimals" <|
            \_ -> Expect.equal "3.50" (Format.format (Format.Number 2 False) (VNumber 3.5))
        , test "Number with thousands separator" <|
            \_ -> Expect.equal "1,234,567" (Format.format (Format.Number 0 True) (VNumber 1234567))
        , test "Currency" <|
            \_ -> Expect.equal "$1,200.00" (Format.format (Format.Currency "$" 2) (VNumber 1200))
        , test "Percent" <|
            \_ -> Expect.equal "25.0%" (Format.format (Format.Percent 1) (VNumber 0.25))
        , test "negative number keeps its sign" <|
            \_ -> Expect.equal "-1.50" (Format.format (Format.Number 2 False) (VNumber -1.5))
        , test "rounding in the formatter" <|
            \_ -> Expect.equal "2.35" (Format.format (Format.Number 2 False) (VNumber 2.345))
        , test "an error value shows its sentinel regardless of format" <|
            \_ -> Expect.equal "#DIV/0!" (Format.format (Format.Number 2 False) (VError DivZero))
        , test "text passes through a number format unharmed" <|
            \_ -> Expect.equal "hello" (Format.format (Format.Number 2 False) (VText "hello"))
        , test "TEXT code: padded integer" <|
            \_ -> Expect.equal "7.00" (Format.applyTextFormat "0.00" (VNumber 7))
        , test "TEXT code: grouped" <|
            \_ -> Expect.equal "12,345" (Format.applyTextFormat "#,##0" (VNumber 12345))
        , test "TEXT code: percent" <|
            \_ -> Expect.equal "50%" (Format.applyTextFormat "0%" (VNumber 0.5))
        , test "DateTime format code" <|
            \_ -> Expect.equal "2020-05-15" (Format.format (Format.DateTime "yyyy-mm-dd") (numericSerial 2020 5 15))
        , test "alignmentClass: numbers right, text left" <|
            \_ ->
                Expect.equal ( "ss-align-right", "ss-align-left" )
                    ( Format.alignmentClass (VNumber 1), Format.alignmentClass (VText "x") )
        ]


{-| The serial number DATE(y,m,d) would produce, for testing date formats. -}
numericSerial : Int -> Int -> Int -> Value
numericSerial y m d =
    ev0 ("=DATE(" ++ String.fromInt y ++ "," ++ String.fromInt m ++ "," ++ String.fromInt d ++ ")")



-- STYLE ----------------------------------------------------------------------


styleTests : Test
styleTests =
    describe "Style"
        [ test "matches GreaterThan" <|
            \_ -> Expect.equal True (Style.matches (Style.GreaterThan 5) (VNumber 7))
        , test "matches Between" <|
            \_ -> Expect.equal True (Style.matches (Style.Between 1 10) (VNumber 5))
        , test "matches TextContains case-insensitively" <|
            \_ -> Expect.equal True (Style.matches (Style.TextContainsC "ell") (VText "Hello"))
        , test "matches a COUNTIF-style criterion" <|
            \_ -> Expect.equal True (Style.matches (Style.CriteriaC ">=10") (VNumber 10))
        , test "IsEmpty" <|
            \_ -> Expect.equal True (Style.matches Style.IsEmptyC VEmpty)
        , test "mergeStyle ORs booleans and prefers the top colour" <|
            \_ ->
                let
                    base =
                        { emptyStyle | bold = True, color = Just "#000000" }

                    top =
                        { emptyStyle | italic = True, color = Just "#ff0000" }

                    merged =
                        Style.mergeStyle base top
                in
                Expect.equal ( True, True, Just "#ff0000" ) ( merged.bold, merged.italic, merged.color )
        , test "render assigns a bold class and right-align for numbers" <|
            \_ ->
                let
                    r =
                        Style.render { emptyStyle | bold = True } (VNumber 5)
                in
                Expect.equal True (List.member "ss-bold" r.classes && List.member "ss-align-right" r.classes)
        , test "lerpColor at t=0 returns the low colour channels" <|
            \_ -> Expect.equal "rgb(0,0,0)" (Style.lerpColor "#000000" "#ffffff" 0)
        , test "lerpColor at t=1 returns the high colour" <|
            \_ -> Expect.equal "rgb(255,255,255)" (Style.lerpColor "#000000" "#ffffff" 1)
        , test "dataBarPercent scales within the range" <|
            \_ -> Expect.equal 50 (round (Style.dataBarPercent 0 10 5))
        , test "conditional formatting applies through the sheet" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "100" ) ]
                            |> Sheet.addConditional
                                { range = { start = at "A1", end = at "A1" }
                                , condition = Style.GreaterThan 50
                                , style = { emptyStyle | bold = True }
                                }

                    style =
                        Sheet.effectiveStyle (at "A1") s
                in
                Expect.equal True style.bold
        ]


emptyStyle : Style.CellStyle
emptyStyle =
    Style.emptyStyle



-- DEPENDENCIES ---------------------------------------------------------------


depsTests : Test
depsTests =
    describe "Deps"
        [ test "precedents of a single ref" <|
            \_ ->
                case Parser.parseFormula "=A1+B2" of
                    Ok expr ->
                        Expect.equal [ ( 0, 0 ), ( 1, 1 ) ] (List.sort (Deps.precedents expr))

                    Err _ ->
                        Expect.fail "parse failed"
        , test "precedents expand a range" <|
            \_ ->
                case Parser.parseFormula "=SUM(A1:A3)" of
                    Ok expr ->
                        Expect.equal [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ] (List.sort (Deps.precedents expr))

                    Err _ ->
                        Expect.fail "parse failed"
        , test "topoSort orders dependencies before dependents" <|
            \_ ->
                let
                    -- B depends on A, C depends on B
                    depsOf k =
                        if k == ( 0, 1 ) then
                            [ ( 0, 0 ) ]

                        else if k == ( 0, 2 ) then
                            [ ( 0, 1 ) ]

                        else
                            []

                    ( ordered, cyclic ) =
                        Deps.topoSort depsOf [ ( 0, 2 ), ( 0, 0 ), ( 0, 1 ) ]
                in
                Expect.equal ( [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ], True ) ( ordered, Set.isEmpty cyclic )
        , test "topoSort reports a cycle" <|
            \_ ->
                let
                    depsOf k =
                        if k == ( 0, 0 ) then
                            [ ( 0, 1 ) ]

                        else
                            [ ( 0, 0 ) ]

                    ( _, cyclic ) =
                        Deps.topoSort depsOf [ ( 0, 0 ), ( 0, 1 ) ]
                in
                Expect.equal 2 (Set.size cyclic)
        ]



-- RECALCULATION --------------------------------------------------------------


recalcTests : Test
recalcTests =
    describe "synchronous recalculation"
        [ test "a formula computes from its precedents" <|
            \_ -> expectVal (VNumber 3) (valOf "B1" (sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "B1", "=A1+A2" ) ]))
        , test "a dependency chain settles in one pass" <|
            \_ ->
                expectVal (VNumber 4)
                    (valOf "C1" (sheetWith [ ( "A1", "1" ), ( "B1", "=A1+1" ), ( "C1", "=B1*2" ) ]))
        , test "out-of-order definitions still resolve" <|
            \_ ->
                expectVal (VNumber 4)
                    (valOf "C1" (sheetWith [ ( "C1", "=B1*2" ), ( "B1", "=A1+1" ), ( "A1", "1" ) ]))
        , test "editing a precedent and recalcFrom updates dependents" <|
            \_ ->
                let
                    s0 =
                        sheetWith [ ( "A1", "1" ), ( "B1", "=A1+1" ), ( "C1", "=B1*2" ) ]

                    s1 =
                        s0
                            |> Sheet.setRaw (at "A1") "5"
                            |> Sheet.recalcFrom [ at "A1" ]
                in
                expectVal2 ( VNumber 6, VNumber 12 ) ( valOf "B1" s1, valOf "C1" s1 )
        , test "a self-reference is marked circular" <|
            \_ -> expectVal (VError Circular) (valOf "A1" (sheetWith [ ( "A1", "=A1+1" ) ]))
        , test "a two-cell cycle is marked circular" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "=B1" ), ( "B1", "=A1" ) ]
                in
                Expect.equal ( VError Circular, VError Circular ) ( valOf "A1" s, valOf "B1" s )
        , test "displayString applies the cell's format" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "0.25" ) ]
                            |> Sheet.setFormat (at "A1") (Format.Percent 0)
                in
                Expect.equal "25%" (Sheet.displayString (at "A1") s)
        ]



-- ASYNC RECALC ---------------------------------------------------------------


asyncTests : Test
asyncTests =
    describe "asynchronous (visible-first) recalculation"
        [ test "stepping to completion equals the synchronous result" <|
            \_ ->
                let
                    base =
                        Sheet.setRawMany (List.map (\( a, r ) -> ( at a, r )) bigSheetInputs) (Sheet.empty 200 30)

                    syncResult =
                        Sheet.recalcAll base

                    asyncResult =
                        runAsyncFull fullViewport base
                in
                Expect.equal True (sameValues asyncResult syncResult (List.map Tuple.first bigSheetInputs))
        , test "visible cells are computed in the first batch" <|
            \_ ->
                let
                    base =
                        Sheet.setRawMany
                            [ ( at "A1", "5" )
                            , ( at "B1", "=A1*2" )

                            -- off-screen formula
                            , ( at "C10", "5" )
                            , ( at "D10", "=C10+1" )
                            ]
                            (Sheet.empty 200 30)

                    viewport =
                        { minCol = 0, minRow = 0, maxCol = 1, maxRow = 1 }

                    ( started, state ) =
                        Recalc.begin viewport [ at "A1", at "B1", at "C10", at "D10" ] base

                    ( afterOne, _ ) =
                        Recalc.step 1 state started
                in
                -- the visible B1 settles immediately; the off-screen D10 is still pending
                expectVal2 ( VNumber 10, VEmpty )
                    ( Sheet.valueAt (at "B1") afterOne, Sheet.valueAt (at "D10") afterOne )
        , test "progress reports completed/total" <|
            \_ ->
                let
                    base =
                        Sheet.setRawMany (List.map (\( a, r ) -> ( at a, r )) bigSheetInputs) (Sheet.empty 200 30)

                    ( started, state ) =
                        Recalc.beginAll fullViewport base

                    ( _, total ) =
                        Recalc.progress state
                in
                Expect.equal True (total > 0)
        ]


fullViewport : Recalc.Viewport
fullViewport =
    { minCol = 0, minRow = 0, maxCol = 29, maxRow = 199 }


{-| A sheet with a fan of formulas: a column of numbers and several derived columns. -}
bigSheetInputs : List ( String, String )
bigSheetInputs =
    List.concatMap
        (\i ->
            let
                r =
                    String.fromInt i
            in
            [ ( "A" ++ r, String.fromInt i )
            , ( "B" ++ r, "=A" ++ r ++ "*2" )
            , ( "C" ++ r, "=B" ++ r ++ "+A" ++ r )
            , ( "D" ++ r, "=SUM(A1:A" ++ r ++ ")" )
            ]
        )
        (List.range 1 20)


runAsyncFull : Recalc.Viewport -> Sheet -> Sheet
runAsyncFull viewport sheet =
    let
        ( started, state ) =
            Recalc.beginAll viewport sheet
    in
    asyncLoop started state


asyncLoop : Sheet -> Recalc.State -> Sheet
asyncLoop sheet state =
    if Recalc.isDone state then
        sheet

    else
        let
            ( sheet2, state2 ) =
                Recalc.step 4 state sheet
        in
        asyncLoop sheet2 state2


sameValues : Sheet -> Sheet -> List String -> Bool
sameValues a b addresses =
    List.all
        (\addr ->
            Sheet.valueAt (at addr) a == Sheet.valueAt (at addr) b
        )
        addresses



-- NEW-FEATURE HELPERS --------------------------------------------------------


{-| A range from an `A1:B2`-style string (defaults to A1 on parse failure). -}
rng : String -> Range
rng a =
    Maybe.withDefault { start = { col = 0, row = 0 }, end = { col = 0, row = 0 } } (Ref.rangeFromA1 a)


round2 : Value -> Value
round2 v =
    case v of
        VNumber n ->
            VNumber (toFloat (round (n * 100)) / 100)

        _ ->
            v


round4 : Value -> Value
round4 v =
    case v of
        VNumber n ->
            VNumber (toFloat (round (n * 10000)) / 10000)

        _ ->
            v


rawOf : String -> Sheet -> String
rawOf a1 s =
    Sheet.rawAt (at a1) s


{-| Parse a formula body and render it back — exercises the parser ⇄ serializer pair. -}
reformat : String -> String
reformat src =
    case Parser.parse src of
        Ok e ->
            Render.expr e

        Err msg ->
            "ERR:" ++ msg


{-| Parse a formula body, apply an `Expr` transform, render the result. -}
rewrite : (Expr -> Expr) -> String -> String
rewrite f src =
    case Parser.parse src of
        Ok e ->
            Render.expr (f e)

        Err msg ->
            "ERR:" ++ msg



-- ABSOLUTE / RELATIVE REFERENCES ---------------------------------------------


absRefTests : Test
absRefTests =
    describe "absolute references"
        [ test "fromA1Abs reads $ markers on both axes" <|
            \_ -> Expect.equal (Just ( { col = 2, row = 4 }, { col = True, row = True } )) (Ref.fromA1Abs "$C$5")
        , test "fromA1Abs reads a row-only anchor" <|
            \_ -> Expect.equal (Just ( { col = 2, row = 4 }, { col = False, row = True } )) (Ref.fromA1Abs "C$5")
        , test "fromA1Abs reads a relative ref" <|
            \_ -> Expect.equal (Just ( { col = 2, row = 4 }, { col = False, row = False } )) (Ref.fromA1Abs "C5")
        , test "toA1Abs renders the markers" <|
            \_ -> Expect.equal "$C5" (Ref.toA1Abs { col = True, row = False } { col = 2, row = 4 })
        , test "a fully-absolute ref evaluates like a relative one" <|
            \_ -> expectVal (VNumber 5) (ev [ ( "A1", VNumber 5 ) ] "=$A$1")
        , test "absolute markers survive a parse/serialize round-trip" <|
            \_ -> Expect.equal "$A$1+B$2-$C3" (reformat "$A$1+B$2-$C3")
        ]



-- FORMULA SERIALIZATION ------------------------------------------------------


renderTests : Test
renderTests =
    describe "formula serialization"
        [ test "round-trips without spurious parens" <|
            \_ -> Expect.equal "A1+B1*2" (reformat "A1+B1*2")
        , test "keeps necessary parens" <|
            \_ -> Expect.equal "(A1+B1)*2" (reformat "(A1+B1)*2")
        , test "unary minus binds tighter than power" <|
            \_ -> Expect.equal "-2^2" (reformat "-2^2")
        , test "power is right associative" <|
            \_ -> Expect.equal "2^2^3" (reformat "2^2^3")
        , test "subtraction keeps left-grouping" <|
            \_ -> Expect.equal "A1-(B1-C1)" (reformat "A1-(B1-C1)")
        , test "function calls and ranges" <|
            \_ -> Expect.equal "SUM(A1:A3,B1)" (reformat "SUM(A1:A3, B1)")
        , test "string literals re-quote" <|
            \_ -> Expect.equal "CONCAT(\"a\",\"b\")" (reformat "CONCAT(\"a\",\"b\")")
        , test "percent postfix" <|
            \_ -> Expect.equal "A1*50%" (reformat "A1*50%")
        ]



-- REFERENCE REWRITING --------------------------------------------------------


refactorTests : Test
refactorTests =
    describe "reference rewriting"
        [ test "translate shifts relative refs but pins absolute ones" <|
            \_ -> Expect.equal "B2+$B$1" (rewrite (Refactor.translate 1 1) "A1+$B$1")
        , test "translate off the top-left edge yields #REF!" <|
            \_ -> Expect.equal "#REF!" (rewrite (Refactor.translate -1 0) "A1")
        , test "insertCols shifts refs at/after the insert point" <|
            \_ -> Expect.equal "C1" (rewrite (Refactor.insertCols 0 1) "B1")
        , test "insertCols shifts even absolute refs" <|
            \_ -> Expect.equal "$C$1" (rewrite (Refactor.insertCols 0 1) "$B$1")
        , test "deleteCols turns a deleted ref into #REF!" <|
            \_ -> Expect.equal "#REF!" (rewrite (Refactor.deleteCols 1 1) "B1")
        , test "deleteCols shifts refs past the deleted band" <|
            \_ -> Expect.equal "B1" (rewrite (Refactor.deleteCols 1 1) "C1")
        , test "insertRows expands a range spanning the insert" <|
            \_ -> Expect.equal "SUM(A1:A4)" (rewrite (Refactor.insertRows 1 1) "SUM(A1:A3)")
        , test "deleteRows shrinks a range overlapping the deletion" <|
            \_ -> Expect.equal "SUM(A1:A2)" (rewrite (Refactor.deleteRows 2 1) "SUM(A1:A3)")
        ]



-- STRUCTURAL EDITS -----------------------------------------------------------


structuralTests : Test
structuralTests =
    describe "insert/delete rows & columns"
        [ test "insertRows shifts a formula and its precedents" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "=A1+A2" ) ]
                            |> Sheet.insertRows 0 1
                            |> Sheet.recalcAll
                in
                Expect.equal ( "=A2+A3", normVal (VNumber 3) ) ( rawOf "A4" s, normVal (valOf "A4" s) )
        , test "deleteRows makes a reference to a deleted cell #REF!" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "=A1+1" ) ]
                            |> Sheet.deleteRows 0 1
                            |> Sheet.recalcAll
                in
                expectVal (VError RefErr) (valOf "A1" s)
        , test "insertCols shifts columns and rewrites formulas" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "10" ), ( "B1", "=A1*2" ) ]
                            |> Sheet.insertCols 0 1
                            |> Sheet.recalcAll
                in
                Expect.equal ( "=B1*2", normVal (VNumber 20) ) ( rawOf "C1" s, normVal (valOf "C1" s) )
        , test "deleteCols shifts a surviving reference" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "C1", "5" ), ( "D1", "=C1+1" ) ]
                            |> Sheet.deleteCols 1 1
                            |> Sheet.recalcAll
                in
                Expect.equal ( "=B1+1", normVal (VNumber 6) ) ( rawOf "C1" s, normVal (valOf "C1" s) )
        ]



-- CLIPBOARD ------------------------------------------------------------------


clipboardTests : Test
clipboardTests =
    describe "copy / cut / paste"
        [ test "copyPaste translates relative references" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "=B1" ), ( "B1", "10" ), ( "B2", "20" ) ]
                            |> Sheet.copyPaste (rng "A1") (at "A2")
                            |> Sheet.recalcAll
                in
                Expect.equal ( "=B2", normVal (VNumber 20) ) ( rawOf "A2" s, normVal (valOf "A2" s) )
        , test "copyPaste keeps absolute references pinned" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "=$B$1" ), ( "B1", "10" ) ]
                            |> Sheet.copyPaste (rng "A1") (at "A5")
                            |> Sheet.recalcAll
                in
                Expect.equal ( "=$B$1", normVal (VNumber 10) ) ( rawOf "A5" s, normVal (valOf "A5" s) )
        , test "cutPaste moves a cell verbatim and clears the source" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "5" ) ]
                            |> Sheet.cutPaste (rng "A1") (at "C3")
                in
                expectVal2 ( VEmpty, VNumber 5 ) ( valOf "A1" s, valOf "C3" s )
        ]



-- AUTOFILL -------------------------------------------------------------------


fillTests : Test
fillTests =
    describe "autofill"
        [ test "fillDown copies a formula with shifting refs" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "B1", "=A1*10" ) ]
                            |> Sheet.fillDown (rng "B1:B3")
                            |> Sheet.recalcAll
                in
                expectVal2 ( VNumber 20, VNumber 30 ) ( valOf "B2" s, valOf "B3" s )
        , test "fillRight copies across columns" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "2" ), ( "B1", "3" ), ( "C1", "4" ), ( "A2", "=A1*10" ) ]
                            |> Sheet.fillRight (rng "A2:C2")
                            |> Sheet.recalcAll
                in
                expectVal2 ( VNumber 30, VNumber 40 ) ( valOf "B2" s, valOf "C2" s )
        , test "fillSeries extrapolates a linear series from two seeds" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ) ]
                            |> Sheet.fillSeries (rng "A1:A5")
                in
                expectVal2 ( VNumber 4, VNumber 5 ) ( valOf "A4" s, valOf "A5" s )
        , test "fillSeries steps by 1 from a single seed" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "5" ) ]
                            |> Sheet.fillSeries (rng "A1:A3")
                in
                expectVal2 ( VNumber 6, VNumber 7 ) ( valOf "A2" s, valOf "A3" s )
        ]



-- SORT & FILTER --------------------------------------------------------------


sortFilterTests : Test
sortFilterTests =
    describe "sort & filter"
        [ test "sortRange orders rows ascending by a key column" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "3" ), ( "A2", "1" ), ( "A3", "2" ) ]
                            |> Sheet.sortRange (rng "A1:A3") 0 True
                in
                Expect.equal (List.map normVal [ VNumber 1, VNumber 2, VNumber 3 ])
                    (List.map (\a -> normVal (valOf a s)) [ "A1", "A2", "A3" ])
        , test "sortRange carries the whole row" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "3" ), ( "B1", "c" ), ( "A2", "1" ), ( "B2", "a" ), ( "A3", "2" ), ( "B3", "b" ) ]
                            |> Sheet.sortRange (rng "A1:B3") 0 True
                in
                Expect.equal [ VText "a", VText "b", VText "c" ]
                    (List.map (\a -> valOf a s) [ "B1", "B2", "B3" ])
        , test "sortRange descending" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "3" ), ( "A3", "2" ) ]
                            |> Sheet.sortRange (rng "A1:A3") 0 False
                in
                Expect.equal (List.map normVal [ VNumber 3, VNumber 2, VNumber 1 ])
                    (List.map (\a -> normVal (valOf a s)) [ "A1", "A2", "A3" ])
        , test "filterRows returns the matching rows" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "10" ), ( "A2", "5" ), ( "A3", "20" ), ( "A4", "5" ) ]

                    atLeast10 v =
                        Value.compare v (VNumber 10) /= LT
                in
                Expect.equal [ 0, 2 ] (Sheet.filterRows (rng "A1:A4") 0 atLeast10 s)
        ]



-- NAMED RANGES ---------------------------------------------------------------


nameTests : Test
nameTests =
    describe "named ranges"
        [ test "a name resolves to a single cell in a formula" <|
            \_ ->
                let
                    s =
                        Sheet.empty 50 10
                            |> Sheet.setRawMany [ ( at "B1", "0.2" ), ( at "A1", "=100*TAX" ) ]
                            |> Sheet.defineName "TAX" (rng "B1")
                            |> Sheet.recalcAll
                in
                expectVal (VNumber 20) (valOf "A1" s)
        , test "a name resolves to a range inside an aggregate" <|
            \_ ->
                let
                    s =
                        Sheet.empty 50 10
                            |> Sheet.setRawMany [ ( at "A1", "1" ), ( at "A2", "2" ), ( at "A3", "3" ), ( at "B1", "=SUM(DATA)" ) ]
                            |> Sheet.defineName "DATA" (rng "A1:A3")
                            |> Sheet.recalcAll
                in
                expectVal (VNumber 6) (valOf "B1" s)
        , test "an undefined name is #NAME?" <|
            \_ -> expectVal (VError NameErr) (valOf "A1" (sheetWith [ ( "A1", "=NOPE" ) ]))
        , test "editing a named cell updates dependents (name tracked as a precedent)" <|
            \_ ->
                let
                    s =
                        Sheet.empty 50 10
                            |> Sheet.setRawMany [ ( at "B1", "10" ), ( at "A1", "=RATE*2" ) ]
                            |> Sheet.defineName "RATE" (rng "B1")
                            |> Sheet.recalcAll
                            |> Sheet.setRaw (at "B1") "20"
                            |> Sheet.recalcFrom [ at "B1" ]
                in
                expectVal (VNumber 40) (valOf "A1" s)
        ]



-- CSV ------------------------------------------------------------------------


csvTests : Test
csvTests =
    describe "CSV import/export"
        [ test "encode quotes fields containing a comma" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "x,y" ), ( "B2", "3" ) ]
                in
                Expect.equal "1,2\n\"x,y\",3" (Csv.encode (rng "A1:B2") s)
        , test "parse splits quoted fields and rows" <|
            \_ -> Expect.equal [ [ "a,b", "c" ], [ "d" ] ] (Csv.parse "\"a,b\",c\nd")
        , test "parse unescapes doubled quotes" <|
            \_ -> Expect.equal [ [ "a\"b" ] ] (Csv.parse "\"a\"\"b\"")
        , test "decode lands values at the anchor and types them" <|
            \_ ->
                let
                    s =
                        Csv.decode (at "A1") "10,20\n30,40" (Sheet.empty 10 10)
                            |> Sheet.recalcAll
                in
                Expect.equal (List.map normVal [ VNumber 10, VNumber 20, VNumber 30, VNumber 40 ])
                    (List.map (\a -> normVal (valOf a s)) [ "A1", "B1", "A2", "B2" ])
        , test "round-trips a numeric block" <|
            \_ ->
                let
                    original =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" ) ]

                    restored =
                        Csv.decode (at "A1") (Csv.encode (rng "A1:B2") original) (Sheet.empty 10 10)
                            |> Sheet.recalcAll
                in
                Expect.equal True (sameValues original restored [ "A1", "B1", "A2", "B2" ])
        ]



-- FINANCE --------------------------------------------------------------------


financeTests : Test
financeTests =
    describe "finance functions"
        [ test "PMT amortises a loan" <|
            \_ -> Expect.equal (round2 (VNumber -402.11)) (round2 (ev0 "=PMT(0.1,3,1000)"))
        , test "PMT with zero rate is straight division" <|
            \_ -> expectVal (VNumber -100) (ev0 "=PMT(0,10,1000)")
        , test "FV of a payment stream" <|
            \_ -> Expect.equal (round2 (VNumber 1593.74)) (round2 (ev0 "=FV(0.1,10,-100)"))
        , test "PV of a payment stream" <|
            \_ -> Expect.equal (round2 (VNumber 614.46)) (round2 (ev0 "=PV(0.1,10,-100)"))
        , test "NPV discounts future cash flows" <|
            \_ -> Expect.equal (round2 (VNumber 248.69)) (round2 (ev0 "=NPV(0.1,100,100,100)"))
        , test "IRR solves for the zero-NPV rate" <|
            \_ -> Expect.equal (round4 (VNumber 0.1307)) (round4 (irrOf [ -100, 60, 60 ]))
        ]


{-| IRR over an explicit cash-flow column. -}
irrOf : List Float -> Value
irrOf flows =
    let
        cells =
            List.indexedMap (\i v -> ( "A" ++ String.fromInt (i + 1), VNumber v )) flows
    in
    ev cells "=IRR(A1:A3)"



-- ANALYSIS FUNCTIONS ---------------------------------------------------------


analysisFnTests : Test
analysisFnTests =
    describe "analysis functions"
        [ test "SUMPRODUCT multiplies element-wise then sums" <|
            \_ -> expectVal (VNumber 32) (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ), ( "B1", VNumber 4 ), ( "B2", VNumber 5 ), ( "B3", VNumber 6 ) ] "=SUMPRODUCT(A1:A3,B1:B3)")
        , test "SUMIFS sums where the criterion matches" <|
            \_ -> expectVal (VNumber 40) (ev regionData "=SUMIFS(A1:A3,B1:B3,\"x\")")
        , test "COUNTIFS counts matches" <|
            \_ -> expectVal (VNumber 2) (ev regionData "=COUNTIFS(B1:B3,\"x\")")
        , test "AVERAGEIFS averages matches" <|
            \_ -> expectVal (VNumber 20) (ev regionData "=AVERAGEIFS(A1:A3,B1:B3,\"x\")")
        , test "MAXIFS / MINIFS over matches" <|
            \_ -> expectVal2 ( VNumber 30, VNumber 10 ) ( ev regionData "=MAXIFS(A1:A3,B1:B3,\"x\")", ev regionData "=MINIFS(A1:A3,B1:B3,\"x\")" )
        , test "SUBTOTAL dispatches by function number" <|
            \_ -> expectVal2 ( VNumber 15, VNumber 3 ) ( ev nums15 "=SUBTOTAL(9,A1:A5)", ev nums15 "=SUBTOTAL(1,A1:A5)" )
        , test "PERCENTILE interpolates" <|
            \_ -> expectVal (VNumber 3) (ev nums15 "=PERCENTILE(A1:A5,0.5)")
        , test "QUARTILE Q1 and Q2" <|
            \_ -> expectVal2 ( VNumber 2, VNumber 3 ) ( ev nums15 "=QUARTILE(A1:A5,1)", ev nums15 "=QUARTILE(A1:A5,2)" )
        , test "RANK descending and ascending" <|
            \_ -> expectVal2 ( VNumber 2, VNumber 4 ) ( ev nums15 "=RANK(4,A1:A5)", ev nums15 "=RANK(4,A1:A5,1)" )
        , test "XLOOKUP returns the aligned value" <|
            \_ -> expectVal (VNumber 20) (ev regionData "=XLOOKUP(\"y\",B1:B3,A1:A3)")
        , test "XLOOKUP not-found fallback" <|
            \_ -> expectVal (VText "none") (ev regionData "=XLOOKUP(\"z\",B1:B3,A1:A3,\"none\")")
        ]


regionData : List ( String, Value )
regionData =
    [ ( "A1", VNumber 10 ), ( "A2", VNumber 20 ), ( "A3", VNumber 30 ), ( "B1", VText "x" ), ( "B2", VText "y" ), ( "B3", VText "x" ) ]


nums15 : List ( String, Value )
nums15 =
    [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ), ( "A4", VNumber 4 ), ( "A5", VNumber 5 ) ]



-- EXPORT ---------------------------------------------------------------------


exportTests : Test
exportTests =
    describe "export"
        [ test "tsv joins with tabs and newlines" <|
            \_ -> Expect.equal "Name\tQty\nPen\t3" (Export.tsv (rng "A1:B2") exportSheet)
        , test "markdown table with header separator" <|
            \_ -> Expect.equal "| Name | Qty |\n| --- | --- |\n| Pen | 3 |" (Export.markdown (rng "A1:B2") exportSheet)
        , test "json keeps numbers numeric and text quoted" <|
            \_ -> Expect.equal "[[\"Name\",\"Qty\"],[\"Pen\",3]]" (Export.json (rng "A1:B2") exportSheet)
        , test "html emits a table with header cells" <|
            \_ -> Expect.equal True (String.contains "<th>Name</th>" (Export.html (rng "A1:B2") exportSheet))
        ]


exportSheet : Sheet
exportSheet =
    sheetWith [ ( "A1", "Name" ), ( "B1", "Qty" ), ( "A2", "Pen" ), ( "B2", "3" ) ]



-- NOTES ----------------------------------------------------------------------


notesTests : Test
notesTests =
    describe "cell notes"
        [ test "set and read a note" <|
            \_ ->
                let
                    s =
                        Sheet.setNote (at "B2") "check this" (Sheet.empty 10 10)
                in
                Expect.equal (Just "check this") (Sheet.noteAt (at "B2") s)
        , test "an empty note clears it" <|
            \_ ->
                let
                    s =
                        Sheet.empty 10 10 |> Sheet.setNote (at "B2") "x" |> Sheet.setNote (at "B2") ""
                in
                Expect.equal Nothing (Sheet.noteAt (at "B2") s)
        , test "a note follows its cell through an inserted row" <|
            \_ ->
                let
                    s =
                        Sheet.empty 10 10 |> Sheet.setNote (at "B2") "n" |> Sheet.insertRows 0 1
                in
                Expect.equal ( Nothing, Just "n" ) ( Sheet.noteAt (at "B2") s, Sheet.noteAt (at "B3") s )
        ]



-- MERGED CELLS ---------------------------------------------------------------


mergeTests : Test
mergeTests =
    describe "merged cells"
        [ test "the anchor keeps its value, covered cells are cleared" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "Title" ), ( "B1", "x" ), ( "A2", "y" ), ( "B2", "z" ) ]
                            |> Sheet.mergeCells (rng "A1:B2")
                in
                expectVal2 ( VText "Title", VEmpty ) ( valOf "A1" s, valOf "B1" s )
        , test "covered cells report as covered, the anchor does not" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "Title" ) ] |> Sheet.mergeCells (rng "A1:B2")
                in
                Expect.equal ( False, True, True ) ( Sheet.isCovered (at "A1") s, Sheet.isCovered (at "B1") s, Sheet.isCovered (at "B2") s )
        , test "mergeAnchorAt returns the span at the anchor only" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "Title" ) ] |> Sheet.mergeCells (rng "A1:B2")
                in
                Expect.equal ( Just (rng "A1:B2"), Nothing ) ( Sheet.mergeAnchorAt (at "A1") s, Sheet.mergeAnchorAt (at "B1") s )
        , test "unmerge releases the block" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "Title" ) ] |> Sheet.mergeCells (rng "A1:B2") |> Sheet.unmerge (at "B1")
                in
                Expect.equal False (Sheet.isCovered (at "B1") s)
        ]



-- DATA VALIDATION ------------------------------------------------------------


validationTests : Test
validationTests =
    describe "data validation"
        [ test "number-between accepts in range, rejects out" <|
            \_ ->
                let
                    s =
                        Sheet.empty 10 10 |> Sheet.addValidation (rng "A1:A5") (Validation.NumberBetween 1 10)
                in
                Expect.equal ( True, False ) ( Sheet.validate (at "A2") "5" s, Sheet.validate (at "A2") "20" s )
        , test "cells outside the rule range accept anything" <|
            \_ ->
                let
                    s =
                        Sheet.empty 10 10 |> Sheet.addValidation (rng "A1:A5") (Validation.NumberBetween 1 10)
                in
                Expect.equal True (Sheet.validate (at "C1") "999" s)
        , test "list rule exposes a dropdown and validates case-insensitively" <|
            \_ ->
                let
                    s =
                        Sheet.empty 10 10 |> Sheet.addValidation (rng "A1:A5") (Validation.OneOf [ "Yes", "No" ])
                in
                Expect.equal ( Just [ "Yes", "No" ], True, False ) ( Sheet.dropdownAt (at "A1") s, Sheet.validate (at "A1") "yes" s, Sheet.validate (at "A1") "maybe" s )
        , test "isInvalid flags a current out-of-range value" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "50" ) ] |> Sheet.addValidation (rng "A1:A5") (Validation.NumberBetween 1 10)
                in
                Expect.equal True (Sheet.isInvalid (at "A1") s)
        , test "a blank cell passes everything but NotBlank" <|
            \_ -> Expect.equal ( True, False ) ( Validation.check (Validation.NumberBetween 1 10) VEmpty, Validation.check Validation.NotBlank VEmpty )
        ]



-- FIND & REPLACE -------------------------------------------------------------


findTests : Test
findTests =
    describe "find & replace"
        [ test "substring find is case-insensitive, row-major" <|
            \_ -> Expect.equal [ at "A1", at "B1" ] (Find.findAll { defs | text = "apple" } findSheet)
        , test "whole-cell find" <|
            \_ -> Expect.equal [ at "A1" ] (Find.findAll { defs | text = "apple", wholeCell = True } findSheet)
        , test "match-case excludes differing case" <|
            \_ -> Expect.equal [] (Find.findAll { defs | text = "APPLE", matchCase = True } findSheet)
        , test "replaceAll rewrites raw input and recalculates" <|
            \_ ->
                let
                    s =
                        Find.replaceAll { defs | text = "apple" } "orange" findSheet
                in
                Expect.equal ( VText "orange", "orange pie" ) ( valOf "A1" s, rawOf "B1" s )
        , test "case-insensitive replace covers all occurrences" <|
            \_ ->
                let
                    s =
                        Find.replaceAll { defs | text = "a" } "b" (sheetWith [ ( "A1", "aAa" ) ])
                in
                Expect.equal "bbb" (rawOf "A1" s)
        ]


defs : Find.Query
defs =
    Find.defaults


findSheet : Sheet
findSheet =
    sheetWith [ ( "A1", "apple" ), ( "B1", "apple pie" ), ( "C1", "grape" ), ( "A2", "Banana" ) ]
