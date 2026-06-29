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
import Json.Decode as D
import Json.Encode as E
import Set
import SheetDoc
import Spreadsheet.Analysis as Analysis
import Spreadsheet.Ast exposing (Expr)
import Spreadsheet.Chart as Chart
import Spreadsheet.Csv as Csv
import Spreadsheet.Deps as Deps
import Spreadsheet.Export as Export
import Spreadsheet.Find as Find
import Spreadsheet.Eval as Eval
import Spreadsheet.Format as Format
import Spreadsheet.Json as Json
import Spreadsheet.Parser as Parser
import Spreadsheet.Scenarios as Scenarios
import Spreadsheet.Suggest as Suggest
import Spreadsheet.Pivot as Pivot
import Spreadsheet.Recalc as Recalc
import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Refactor as Refactor
import Spreadsheet.Render as Render
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Spill as Spill
import Spreadsheet.Style as Style
import Spreadsheet.Validation as Validation
import Spreadsheet.Value as Value exposing (Error(..), Value(..))
import Spreadsheet.Workbook as Workbook
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
        , workbookTests
        , pivotTests
        , condFmtTests
        , spillTests
        , statsFn2Tests
        , dateFn2Tests
        , dynRefTests
        , customFormatTests
        , analysisTests
        , chartTests
        , sheetDocTests
        , dynArrayTests
        , arrayShapeTests
        , regressionTests
        , letTests
        , spillRefTests
        , iconSetTests
        , lambdaTests
        , namedLambdaTests
        , broadcastTests
        , tableTests
        , textFn2Tests
        , reshapeTests
        , auditTests
        , groupByTests
        , dbFnTests
        , regexTests
        , statFn3Tests
        , aggregateTests
        , jsonTests
        , depEdgeTests
        , borderTests
        , formulaCondTests
        , suggestTests
        , protectionTests
        , filterTests
        , nameBoxTests
        , scenarioTests
        , matrixTests
        , financeFn2Tests
        , distributionTests
        , engineeringTests
        , pivotTableTests
        , chart2Tests
        , dataToolTests
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
    , external = \_ _ -> VEmpty
    , locals = Eval.noLocals
    , spill = Eval.noSpill
    , lambda = Eval.noLambda
    , tableRange = Eval.noTableRange
    , tableTotals = Eval.noTableTotals
    , formulaText = Eval.noFormulaText
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


{-| Compare two values.

These used to round-trip every number through `String.fromFloat`/`toFloat` to work around a
backend quirk where a whole-number `Float` *literal* (`VNumber 5` in source) did not `==` the
same value produced by computation. That is now fixed in the compiler (structural `==` coerces
numbers, including ones nested in tuples/ctors/lists/records), so these are plain pass-throughs
kept only so the call sites read in terms of values. -}
expectVal : Value -> Value -> Expect.Expectation
expectVal =
    Expect.equal


expectVal2 : ( Value, Value ) -> ( Value, Value ) -> Expect.Expectation
expectVal2 =
    Expect.equal


normVal : Value -> Value
normVal v =
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



-- WORKBOOK / CROSS-SHEET -----------------------------------------------------


workbookTests : Test
workbookTests =
    describe "workbook / cross-sheet references"
        [ test "a cross-sheet reference reads another sheet" <|
            \_ ->
                let
                    data =
                        Sheet.setRawMany [ ( at "A1", "10" ) ] (Sheet.empty 10 5)

                    main =
                        Sheet.setRawMany [ ( at "A1", "=Data!A1+5" ) ] (Sheet.empty 10 5)

                    wb =
                        Workbook.recalc (Workbook.init [ ( "Data", data ), ( "Main", main ) ])
                in
                expectVal (VNumber 15) (Workbook.valueAt "Main" (at "A1") wb)
        , test "a cross-sheet range works in an aggregate" <|
            \_ ->
                let
                    data =
                        Sheet.setRawMany [ ( at "A1", "1" ), ( at "A2", "2" ), ( at "A3", "3" ) ] (Sheet.empty 10 5)

                    main =
                        Sheet.setRawMany [ ( at "B1", "=SUM(Data!A1:A3)" ) ] (Sheet.empty 10 5)

                    wb =
                        Workbook.recalc (Workbook.init [ ( "Data", data ), ( "Main", main ) ])
                in
                expectVal (VNumber 6) (Workbook.valueAt "Main" (at "B1") wb)
        , test "cross-sheet chains settle to a fixed point" <|
            \_ ->
                let
                    s3 =
                        Sheet.setRawMany [ ( at "A1", "10" ) ] (Sheet.empty 10 5)

                    s2 =
                        Sheet.setRawMany [ ( at "A1", "=Sheet3!A1*2" ) ] (Sheet.empty 10 5)

                    s1 =
                        Sheet.setRawMany [ ( at "A1", "=Sheet2!A1+1" ) ] (Sheet.empty 10 5)

                    wb =
                        Workbook.recalc (Workbook.init [ ( "Sheet1", s1 ), ( "Sheet2", s2 ), ( "Sheet3", s3 ) ])
                in
                expectVal2 ( VNumber 20, VNumber 21 )
                    ( Workbook.valueAt "Sheet2" (at "A1") wb, Workbook.valueAt "Sheet1" (at "A1") wb )
        , test "sheet names are case-insensitive" <|
            \_ ->
                let
                    data =
                        Sheet.setRawMany [ ( at "A1", "7" ) ] (Sheet.empty 10 5)

                    main =
                        Sheet.setRawMany [ ( at "A1", "=DATA!A1" ) ] (Sheet.empty 10 5)

                    wb =
                        Workbook.recalc (Workbook.init [ ( "Data", data ), ( "Main", main ) ])
                in
                expectVal (VNumber 7) (Workbook.valueAt "Main" (at "A1") wb)
        , test "a reference to an unknown sheet is #REF!" <|
            \_ ->
                let
                    main =
                        Sheet.setRawMany [ ( at "A1", "=Nope!A1" ) ] (Sheet.empty 10 5)

                    wb =
                        Workbook.recalc (Workbook.init [ ( "Main", main ) ])
                in
                expectVal (VError RefErr) (Workbook.valueAt "Main" (at "A1") wb)
        , test "a single sheet recalculated alone fails cross-sheet refs" <|
            \_ ->
                let
                    s =
                        Sheet.recalcAll (Sheet.setRawMany [ ( at "A1", "=Data!A1" ) ] (Sheet.empty 10 5))
                in
                expectVal (VError RefErr) (Sheet.valueAt (at "A1") s)
        , test "cross-sheet references serialize round-trip" <|
            \_ -> Expect.equal "Data!A1+Sheet2!B2:C3" (reformat "Data!A1+Sheet2!B2:C3")
        , test "tab order, active sheet and setActive" <|
            \_ ->
                let
                    wb =
                        Workbook.init [ ( "One", Sheet.empty 5 5 ), ( "Two", Sheet.empty 5 5 ) ]
                in
                Expect.equal ( [ "One", "Two" ], "One", "Two" )
                    ( Workbook.sheetNames wb, Workbook.activeName wb, Workbook.activeName (Workbook.setActive "Two" wb) )
        , test "addSheet appends and removeSheet reassigns active" <|
            \_ ->
                let
                    wb =
                        Workbook.init [ ( "One", Sheet.empty 5 5 ) ]
                            |> Workbook.addSheet "Two" (Sheet.empty 5 5)
                            |> Workbook.removeSheet "One"
                in
                Expect.equal ( [ "Two" ], "Two" ) ( Workbook.sheetNames wb, Workbook.activeName wb )
        ]



-- PIVOT TABLES ---------------------------------------------------------------


pivotTests : Test
pivotTests =
    describe "pivot tables"
        [ test "sum by group" <|
            \_ ->
                Expect.equal (List.map (\( k, v ) -> ( k, normVal v )) [ ( "North", VNumber 150 ), ( "South", VNumber 230 ) ])
                    (List.map (\( k, v ) -> ( k, normVal v )) (Pivot.pivot { keyCol = 0, valueCol = 1, agg = Pivot.Sum } (rng "A1:B4") pivotSheet))
        , test "count by group" <|
            \_ ->
                Expect.equal (List.map (\( k, v ) -> ( k, normVal v )) [ ( "North", VNumber 2 ), ( "South", VNumber 2 ) ])
                    (List.map (\( k, v ) -> ( k, normVal v )) (Pivot.pivot { keyCol = 0, valueCol = 1, agg = Pivot.Count } (rng "A1:B4") pivotSheet))
        , test "average by group" <|
            \_ ->
                Expect.equal (List.map (\( k, v ) -> ( k, normVal v )) [ ( "North", VNumber 75 ), ( "South", VNumber 115 ) ])
                    (List.map (\( k, v ) -> ( k, normVal v )) (Pivot.pivot { keyCol = 0, valueCol = 1, agg = Pivot.Average } (rng "A1:B4") pivotSheet))
        , test "max by group" <|
            \_ ->
                Expect.equal (List.map (\( k, v ) -> ( k, normVal v )) [ ( "North", VNumber 100 ), ( "South", VNumber 200 ) ])
                    (List.map (\( k, v ) -> ( k, normVal v )) (Pivot.pivot { keyCol = 0, valueCol = 1, agg = Pivot.Max } (rng "A1:B4") pivotSheet))
        ]


pivotSheet : Sheet
pivotSheet =
    sheetWith [ ( "A1", "North" ), ( "B1", "100" ), ( "A2", "South" ), ( "B2", "200" ), ( "A3", "North" ), ( "B3", "50" ), ( "A4", "South" ), ( "B4", "30" ) ]



-- EXTENDED CONDITIONAL FORMATTING --------------------------------------------


condFmtTests : Test
condFmtTests =
    describe "range-aware conditional formatting"
        [ test "top-N highlights the largest values" <|
            \_ ->
                let
                    s =
                        rankSheet [ "10", "20", "30", "40", "50" ] (Style.TopN 2)
                in
                Expect.equal [ False, False, False, True, True ]
                    (List.map (\a -> hasHot a s) [ "A1", "A2", "A3", "A4", "A5" ])
        , test "bottom-N highlights the smallest values" <|
            \_ ->
                let
                    s =
                        rankSheet [ "10", "20", "30", "40", "50" ] (Style.BottomN 2)
                in
                Expect.equal [ True, True, False, False, False ]
                    (List.map (\a -> hasHot a s) [ "A1", "A2", "A3", "A4", "A5" ])
        , test "above-average highlights cells over the mean" <|
            \_ ->
                let
                    s =
                        rankSheet [ "10", "20", "30" ] Style.AboveAverage
                in
                Expect.equal [ False, False, True ] (List.map (\a -> hasHot a s) [ "A1", "A2", "A3" ])
        , test "duplicate highlights repeated values" <|
            \_ ->
                let
                    s =
                        rankSheet [ "x", "y", "x" ] Style.Duplicate
                in
                Expect.equal [ True, False, True ] (List.map (\a -> hasHot a s) [ "A1", "A2", "A3" ])
        , test "unique highlights one-off values" <|
            \_ ->
                let
                    s =
                        rankSheet [ "x", "y", "x" ] Style.UniqueValue
                in
                Expect.equal [ False, True, False ] (List.map (\a -> hasHot a s) [ "A1", "A2", "A3" ])
        ]


{-| A single-column sheet (A1..) with a rank rule over the whole column. -}
rankSheet : List String -> Style.RankKind -> Sheet
rankSheet values kind =
    let
        cells =
            List.indexedMap (\i v -> ( "A" ++ String.fromInt (i + 1), v )) values

        last =
            "A" ++ String.fromInt (List.length values)
    in
    sheetWith cells
        |> Sheet.addRankRule { range = rng ("A1:" ++ last), kind = kind, style = hotStyle }


hotStyle : Style.CellStyle
hotStyle =
    let
        base =
            Style.emptyStyle
    in
    { base | classes = [ "hot" ] }


hasHot : String -> Sheet -> Bool
hasHot a1 s =
    List.member "hot" (Sheet.renderedStyle (at a1) s).classes



-- DYNAMIC ARRAYS (SPILL) -----------------------------------------------------


spillTests : Test
spillTests =
    describe "dynamic-array transforms"
        [ test "unique keeps the first occurrence of each row" <|
            \_ ->
                Expect.equal [ [ VText "a" ], [ VText "b" ] ]
                    (Spill.unique [ [ VText "a" ], [ VText "b" ], [ VText "a" ] ])
        , test "sortBy orders rows by a column ascending" <|
            \_ ->
                Expect.equal [ [ VNumber 1 ], [ VNumber 2 ], [ VNumber 3 ] ]
                    (Spill.sortBy 0 True [ [ VNumber 3 ], [ VNumber 1 ], [ VNumber 2 ] ])
        , test "filter keeps matching rows" <|
            \_ ->
                Expect.equal [ [ VNumber 30 ], [ VNumber 40 ] ]
                    (Spill.filter (\row -> List.any (\v -> Value.compare v (VNumber 25) == GT) row) [ [ VNumber 10 ], [ VNumber 30 ], [ VNumber 40 ] ])
        , test "sequence counts up row-major" <|
            \_ ->
                Expect.equal (List.map (List.map normVal) [ [ VNumber 1, VNumber 2 ], [ VNumber 3, VNumber 4 ] ])
                    (List.map (List.map normVal) (Spill.sequence 2 2 1 1))
        , test "transpose flips rows and columns" <|
            \_ ->
                Expect.equal [ [ VNumber 1, VNumber 3 ], [ VNumber 2, VNumber 4 ] ]
                    (Spill.transpose [ [ VNumber 1, VNumber 2 ], [ VNumber 3, VNumber 4 ] ])
        , test "spillInto writes a block into empty cells" <|
            \_ ->
                case Sheet.spillInto (at "B1") (Spill.sequence 3 1 10 5) (Sheet.empty 10 5) of
                    Just s ->
                        Expect.equal (List.map normVal [ VNumber 10, VNumber 15, VNumber 20 ])
                            (List.map (\a -> normVal (valOf a (Sheet.recalcAll s))) [ "B1", "B2", "B3" ])

                    Nothing ->
                        Expect.fail "expected the spill to fit"
        , test "spillInto refuses to overwrite an occupied cell (#SPILL!)" <|
            \_ ->
                let
                    occupied =
                        sheetWith [ ( "B2", "x" ) ]
                in
                Expect.equal Nothing (Sheet.spillInto (at "B1") (Spill.sequence 3 1 10 5) occupied)
        ]



-- STATISTICS & FORECASTING ---------------------------------------------------


statsFn2Tests : Test
statsFn2Tests =
    describe "statistics & forecasting"
        [ test "CORREL of perfectly correlated data is 1" <|
            \_ -> expectVal (VNumber 1) (ev xy "=CORREL(A1:A3,B1:B3)")
        , test "SLOPE of y = 2x is 2" <|
            \_ -> expectVal (VNumber 2) (ev xy "=SLOPE(B1:B3,A1:A3)")
        , test "INTERCEPT of y = 2x is 0" <|
            \_ -> expectVal (VNumber 0) (ev xy "=INTERCEPT(B1:B3,A1:A3)")
        , test "RSQ of perfect fit is 1" <|
            \_ -> expectVal (VNumber 1) (ev xy "=RSQ(B1:B3,A1:A3)")
        , test "FORECAST extrapolates the line" <|
            \_ -> expectVal (VNumber 8) (ev xy "=FORECAST(4,B1:B3,A1:A3)")
        , test "GEOMEAN of 1,2,4 is 2" <|
            \_ -> expectVal (VNumber 2) (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 4 ) ] "=GEOMEAN(A1:A3)")
        , test "HARMEAN of 1,2,4" <|
            \_ -> Expect.equal (round4 (VNumber 1.7143)) (round4 (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 4 ) ] "=HARMEAN(A1:A3)"))
        , test "DEVSQ sums squared deviations" <|
            \_ -> expectVal (VNumber 8) (ev [ ( "A1", VNumber 2 ), ( "A2", VNumber 4 ), ( "A3", VNumber 6 ) ] "=DEVSQ(A1:A3)")
        ]


xy : List ( String, Value )
xy =
    [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ), ( "B1", VNumber 2 ), ( "B2", VNumber 4 ), ( "B3", VNumber 6 ) ]



-- DATE & TIME ----------------------------------------------------------------


dateFn2Tests : Test
dateFn2Tests =
    describe "date & time functions"
        [ test "TIME builds a day fraction" <|
            \_ -> expectVal (VNumber 0.5) (ev0 "=TIME(12,0,0)")
        , test "HOUR / MINUTE / SECOND read the time" <|
            \_ -> expectVal2 ( VNumber 13, VNumber 30 ) ( ev0 "=HOUR(TIME(13,30,45))", ev0 "=MINUTE(TIME(13,30,45))" )
        , test "SECOND reads the seconds" <|
            \_ -> expectVal (VNumber 45) (ev0 "=SECOND(TIME(13,30,45))")
        , test "EDATE clamps to the end of a shorter month" <|
            \_ -> expectVal2 (numPair (ev0 "=EDATE(DATE(2026,1,31),1)") (ev0 "=DATE(2026,2,28)")) ( VBool True, VBool True )
        , test "EOMONTH returns the last day of the month" <|
            \_ -> expectVal2 (numPair (ev0 "=EOMONTH(DATE(2026,1,15),0)") (ev0 "=DATE(2026,1,31)")) ( VBool True, VBool True )
        , test "NETWORKDAYS over any 7-day span is 5" <|
            \_ -> expectVal (VNumber 5) (ev0 "=NETWORKDAYS(DATE(2026,6,1),DATE(2026,6,7))")
        , test "WORKDAY lands on a weekday" <|
            \_ -> expectVal (VNumber 1) (ev0 "=IF(AND(WEEKDAY(WORKDAY(DATE(2026,6,3),1))>1,WEEKDAY(WORKDAY(DATE(2026,6,3),1))<7),1,0)")
        , test "YEARFRAC of a full year (basis 3) is 1" <|
            \_ -> expectVal (VNumber 1) (ev0 "=YEARFRAC(DATE(2026,1,1),DATE(2027,1,1),3)")
        ]


{-| `(eq, True)` so two computed serials can be compared without literal-vs-computed noise. -}
numPair : Value -> Value -> ( Value, Value )
numPair a b =
    ( VBool (normVal a == normVal b), VBool True )



-- DYNAMIC REFERENCES ---------------------------------------------------------


dynRefTests : Test
dynRefTests =
    describe "dynamic references"
        [ test "OFFSET reaches another cell" <|
            \_ -> expectVal (VNumber 3) (ev col123 "=OFFSET(A1,2,0)")
        , test "OFFSET with height/width yields a range an aggregate reads" <|
            \_ -> expectVal (VNumber 6) (ev col123 "=SUM(OFFSET(A1,0,0,3,1))")
        , test "INDIRECT resolves a text address" <|
            \_ -> expectVal (VNumber 2) (ev col123 "=INDIRECT(\"A2\")")
        , test "INDIRECT resolves a text range inside SUM" <|
            \_ -> expectVal (VNumber 6) (ev col123 "=SUM(INDIRECT(\"A1:A3\"))")
        , test "ADDRESS builds an absolute reference" <|
            \_ -> expectVal (VText "$C$2") (ev0 "=ADDRESS(2,3)")
        , test "ADDRESS with abs-num 4 is fully relative" <|
            \_ -> expectVal (VText "C2") (ev0 "=ADDRESS(2,3,4)")
        ]


col123 : List ( String, Value )
col123 =
    [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ) ]



-- CUSTOM NUMBER FORMATS ------------------------------------------------------


customFormatTests : Test
customFormatTests =
    describe "custom number formats"
        [ test "positive section" <|
            \_ -> Expect.equal "5" (Format.applyTextFormat "0;(0);zero" (VNumber 5))
        , test "negative section uses its own decoration" <|
            \_ -> Expect.equal "(5)" (Format.applyTextFormat "0;(0);zero" (VNumber -5))
        , test "zero section is literal text" <|
            \_ -> Expect.equal "zero" (Format.applyTextFormat "0;(0);zero" (VNumber 0))
        , test "literal suffix renders after the number" <|
            \_ -> Expect.equal "12 kg" (Format.applyTextFormat "0 \"kg\"" (VNumber 12))
        , test "trailing comma scales by thousands" <|
            \_ -> Expect.equal "1,235" (Format.applyTextFormat "#,##0," (VNumber 1234567))
        , test "fraction format" <|
            \_ -> Expect.equal "2 1/2" (Format.applyTextFormat "# ?/?" (VNumber 2.5))
        , test "fraction reduces" <|
            \_ -> Expect.equal "3/4" (Format.applyTextFormat "?/?" (VNumber 0.75))
        , test "colorOf reads the negative section's colour" <|
            \_ -> Expect.equal (Just "#d93025") (Format.colorOf "0;[Red](0)" (VNumber -5))
        , test "colorOf is Nothing for a positive value with no colour" <|
            \_ -> Expect.equal Nothing (Format.colorOf "0;[Red](0)" (VNumber 5))
        , test "format-driven colour reaches the rendered cell" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "-5" ) ] |> Sheet.setFormat (at "A1") (Format.Custom "0;[Red](0)")
                in
                Expect.equal True (List.member ( "color", "#d93025" ) (Sheet.renderedStyle (at "A1") s).inline)
        ]



-- WHAT-IF ANALYSIS -----------------------------------------------------------


analysisTests : Test
analysisTests =
    describe "what-if analysis"
        [ test "goalSeek solves for the input" <|
            \_ ->
                let
                    base =
                        sheetWith [ ( "A1", "1" ), ( "B1", "=A1*A1" ) ]
                in
                case Analysis.goalSeek (at "B1") 16 (at "A1") base of
                    Just solved ->
                        Expect.equal (round2 (VNumber 16)) (round2 (valOf "B1" solved))

                    Nothing ->
                        Expect.fail "goal seek did not converge"
        , test "goalSeek lands the changing cell near the root" <|
            \_ ->
                let
                    base =
                        sheetWith [ ( "A1", "1" ), ( "B1", "=A1*A1" ) ]
                in
                case Analysis.goalSeek (at "B1") 16 (at "A1") base of
                    Just solved ->
                        Expect.equal (round2 (VNumber 4)) (round2 (valOf "A1" solved))

                    Nothing ->
                        Expect.fail "goal seek did not converge"
        , test "one-variable data table" <|
            \_ ->
                let
                    base =
                        sheetWith [ ( "A1", "0" ), ( "B1", "=A1*10" ) ]
                in
                Expect.equal (List.map normVal [ VNumber 10, VNumber 20, VNumber 30 ])
                    (List.map normVal (Analysis.dataTable1 (at "B1") (at "A1") [ 1, 2, 3 ] base))
        , test "two-variable data table" <|
            \_ ->
                let
                    base =
                        sheetWith [ ( "A1", "0" ), ( "A2", "0" ), ( "B1", "=A1*A2" ) ]
                in
                Expect.equal (List.map (List.map normVal) [ [ VNumber 20, VNumber 40 ], [ VNumber 30, VNumber 60 ] ])
                    (List.map (List.map normVal) (Analysis.dataTable2 (at "B1") (at "A1") (at "A2") [ 2, 3 ] [ 10, 20 ] base))
        ]



-- CHARTS ---------------------------------------------------------------------


chartTests : Test
chartTests =
    describe "chart geometry"
        [ test "bars are fractions of the maximum" <|
            \_ -> Expect.equal [ 0.25, 0.5, 1.0 ] (Chart.bars [ 10, 20, 40 ])
        , test "bars of all-zero are flat" <|
            \_ -> Expect.equal [ 0, 0 ] (Chart.bars [ 0, 0 ])
        , test "pie slices are cumulative fractions" <|
            \_ -> Expect.equal [ ( 0, 0.25 ), ( 0.25, 0.5 ), ( 0.5, 1.0 ) ] (Chart.pieSlices [ 1, 1, 2 ])
        , test "line points span 0..1 with inverted y" <|
            \_ -> Expect.equal (normPts [ ( 0, 1 ), ( 1, 0 ) ]) (normPts (Chart.linePoints [ 0, 10 ]))
        ]


{-| Pass-through, kept so call sites read in terms of points. The string round-trip it once did
(to make a literal `1` and a computed `1.0` compare equal) is no longer needed now that the
compiler coerces numbers under structural `==`. -}
normPts : List ( Float, Float ) -> List ( Float, Float )
normPts pts =
    pts


{-| The workspace document adapter: its JSON codec must round-trip the raw cells (and the sheet
recompute its formulas) so saved spreadsheets reload intact. Also pins the Workspace + Spreadsheet
co-compilation (a shared constructor name would crash this interpreter). -}
sheetDocTests : Test
sheetDocTests =
    describe "SheetDoc (workspace document)"
        [ test "codec round-trips raw cells and recomputed formulas" <|
            \_ ->
                let
                    doc =
                        SheetDoc.config.empty

                    json =
                        E.encode 0 (SheetDoc.config.codec.encode doc)

                    decoded =
                        D.decodeString SheetDoc.config.codec.decoder json
                in
                case decoded of
                    Ok d ->
                        Expect.equal
                            ( Sheet.rawAt (at "B4") doc.sheet, Sheet.displayString (at "B4") doc.sheet )
                            ( Sheet.rawAt (at "B4") d.sheet, Sheet.displayString (at "B4") d.sheet )

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]


-- DYNAMIC ARRAYS (live spilling) ---------------------------------------------


{-| The raw Float at a cell (a sentinel if it isn't a number), for tolerance comparisons. -}
floatAt : String -> Sheet -> Float
floatAt a1 s =
    case valOf a1 s of
        VNumber x ->
            x

        _ ->
            -999999


rangeOf : String -> String -> Range
rangeOf a b =
    { start = at a, end = at b }


dynArrayTests : Test
dynArrayTests =
    describe "dynamic arrays (live spilling)"
        [ test "SEQUENCE spills a column" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "=SEQUENCE(3)" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 3 ) ( valOf "A1" s, valOf "A3" s )
        , test "SEQUENCE rows × cols spills a block" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "=SEQUENCE(2,2,1,1)" ) ]
                in
                expectVal2 ( VNumber 2, VNumber 4 ) ( valOf "B1" s, valOf "B2" s )
        , test "SORT spills a sorted block" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "3" ), ( "A2", "1" ), ( "A3", "2" ), ( "D1", "=SORT(A1:A3)" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 3 ) ( valOf "D1" s, valOf "D3" s )
        , test "SORT descending" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=SORT(A1:A3,1,-1)" ) ]
                in
                expectVal (VNumber 3) (valOf "D1" s)
        , test "UNIQUE keeps first occurrences" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "x" ), ( "A2", "y" ), ( "A3", "x" ), ( "A4", "z" ), ( "D1", "=UNIQUE(A1:A4)" ) ]
                in
                Expect.equal ( VText "x", VText "y", VText "z" ) ( valOf "D1" s, valOf "D2" s, valOf "D3" s )
        , test "FILTER keeps masked rows" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "10" ), ( "A2", "20" ), ( "A3", "30" )
                            , ( "B1", "1" ), ( "B2", "0" ), ( "B3", "1" )
                            , ( "D1", "=FILTER(A1:A3,B1:B3)" )
                            ]
                in
                expectVal2 ( VNumber 10, VNumber 30 ) ( valOf "D1" s, valOf "D2" s )
        , test "TRANSPOSE turns a row into a column" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "C1", "3" ), ( "D1", "=TRANSPOSE(A1:C1)" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 3 ) ( valOf "D1" s, valOf "D3" s )
        , test "SORTBY orders one range by another" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "apple" ), ( "A2", "pear" ), ( "A3", "fig" )
                            , ( "B1", "3" ), ( "B2", "1" ), ( "B3", "2" )
                            , ( "D1", "=SORTBY(A1:A3,B1:B3)" )
                            ]
                in
                Expect.equal ( VText "pear", VText "fig", VText "apple" ) ( valOf "D1" s, valOf "D2" s, valOf "D3" s )
        , test "a spill feeds a plain-range aggregate" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=SEQUENCE(3)" ), ( "F1", "=SUM(D1:D3)" ) ]
                in
                expectVal (VNumber 6) (valOf "F1" s)
        , test "a spill that overruns an occupied cell is #SPILL!" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "3" ), ( "A2", "1" ), ( "A3", "2" ), ( "D1", "=SORT(A1:A3)" ), ( "D2", "blocker" ) ]
                in
                expectVal (VError Value.Spill) (valOf "D1" s)
        , test "editing a spill source re-spills (recalc settles)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "D1", "=SORT(A1:A2,1,-1)" ) ]
                            |> Sheet.setRaw (at "A2") "9"
                            |> Sheet.recalcAll
                in
                expectVal2 ( VNumber 9, VNumber 1 ) ( valOf "D1" s, valOf "D2" s )
        ]



-- ARRAY SHAPING --------------------------------------------------------------


arrayShapeTests : Test
arrayShapeTests =
    describe "array shaping"
        [ test "HSTACK joins blocks side by side" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "1" ), ( "A2", "2" ), ( "B1", "3" ), ( "B2", "4" ), ( "D1", "=HSTACK(A1:A2,B1:B2)" ) ]
                in
                expectVal2 ( VNumber 3, VNumber 2 ) ( valOf "E1" s, valOf "D2" s )
        , test "VSTACK stacks blocks vertically" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "B1", "3" ), ( "D1", "=VSTACK(A1:A1,B1:B1)" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 3 ) ( valOf "D1" s, valOf "D2" s )
        , test "CHOOSECOLS picks a column" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" ), ( "D1", "=CHOOSECOLS(A1:B2,2)" ) ]
                in
                expectVal2 ( VNumber 2, VNumber 4 ) ( valOf "D1" s, valOf "D2" s )
        , test "CHOOSEROWS picks rows (negative = from end)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "10" ), ( "A2", "20" ), ( "A3", "30" ), ( "D1", "=CHOOSEROWS(A1:A3,-1)" ) ]
                in
                expectVal (VNumber 30) (valOf "D1" s)
        , test "TAKE keeps the first rows" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=TAKE(A1:A3,2)" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 2 ) ( valOf "D1" s, valOf "D2" s )
        , test "DROP removes the first rows" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=DROP(A1:A3,1)" ) ]
                in
                expectVal2 ( VNumber 2, VNumber 3 ) ( valOf "D1" s, valOf "D2" s )
        ]



-- REGRESSION ARRAYS ----------------------------------------------------------


regressionTests : Test
regressionTests =
    describe "regression arrays"
        [ test "LINEST returns slope then intercept" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "2" ), ( "A2", "4" ), ( "A3", "6" )
                            , ( "B1", "1" ), ( "B2", "2" ), ( "B3", "3" )
                            , ( "D1", "=LINEST(A1:A3,B1:B3)" )
                            ]
                in
                expectVal2 ( VNumber 2, VNumber 0 ) ( valOf "D1" s, valOf "E1" s )
        , test "TREND predicts along the fitted line" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "2" ), ( "A2", "4" ), ( "A3", "6" )
                            , ( "B1", "1" ), ( "B2", "2" ), ( "B3", "3" )
                            , ( "C1", "4" ), ( "C2", "5" )
                            , ( "D1", "=TREND(A1:A3,B1:B3,C1:C2)" )
                            ]
                in
                expectVal2 ( VNumber 8, VNumber 10 ) ( valOf "D1" s, valOf "D2" s )
        , test "GROWTH fits an exponential" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "2" ), ( "A2", "4" ), ( "A3", "8" ), ( "D1", "=GROWTH(A1:A3)" ) ]
                in
                Expect.equal True (abs (floatAt "D3" s - 8) < 1.0e-6)
        ]



-- LET ------------------------------------------------------------------------


letTests : Test
letTests =
    describe "LET local bindings"
        [ test "binds a name and uses it" <|
            \_ -> expectVal (VNumber 6) (ev0 "=LET(x,5,x+1)")
        , test "later bindings see earlier ones" <|
            \_ -> expectVal (VNumber 9) (ev [ ( "A1", VNumber 3 ) ] "=LET(x,A1,y,x*2,x+y)")
        , test "a local shadows nothing outside its LET" <|
            \_ -> expectVal (VError NameErr) (ev0 "=x+LET(x,1,x)")
        , test "LET works inside a sheet recalc" <|
            \_ -> expectVal (VNumber 100) (valOf "A1" (sheetWith [ ( "A1", "=LET(p,10,p*p)" ) ]))
        ]



-- SPILL REFERENCE (A1#) ------------------------------------------------------


spillRefTests : Test
spillRefTests =
    describe "spill reference A1#"
        [ test "parses and renders the # operator" <|
            \_ ->
                case Parser.parseFormula "=A1#" of
                    Ok e ->
                        Expect.equal "=A1#" (Render.formula e)

                    Err msg ->
                        Expect.fail msg
        , test "SUM over a spill range" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=SEQUENCE(3)" ), ( "F1", "=SUM(D1#)" ) ]
                in
                expectVal (VNumber 6) (valOf "F1" s)
        , test "a spill reference itself spills" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=SEQUENCE(3)" ), ( "H1", "=D1#" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 3 ) ( valOf "H1" s, valOf "H3" s )
        , test "# on a cell with no spill is #REF!" <|
            \_ ->
                expectVal (VError RefErr) (valOf "F1" (sheetWith [ ( "A1", "5" ), ( "F1", "=A1#" ) ]))
        ]



-- ICON SETS ------------------------------------------------------------------


iconSetTests : Test
iconSetTests =
    describe "icon-set conditional formatting"
        [ test "iconLevel buckets by threshold" <|
            \_ ->
                let
                    set =
                        { range = rangeOf "A1" "A3", style = Style.ThreeArrows, lowMax = 3, midMax = 6 }
                in
                Expect.equal ( 0, 1, 2 ) ( Style.iconLevel set 1, Style.iconLevel set 5, Style.iconLevel set 9 )
        , test "iconView returns glyph and colour" <|
            \_ -> Expect.equal ( "↑", "#188038" ) (Style.iconView Style.ThreeArrows 2)
        , test "iconAt picks the cell's icon" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "5" ), ( "A3", "9" ) ]
                            |> Sheet.addIconSet { range = rangeOf "A1" "A3", style = Style.ThreeArrows, lowMax = 3, midMax = 6 }
                in
                Expect.equal
                    ( Just ( "↓", "#d93025" ), Just ( "↑", "#188038" ) )
                    ( Sheet.iconAt (at "A1") s, Sheet.iconAt (at "A3") s )
        , test "iconAt is Nothing off-range or for text" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "hi" ) ]
                            |> Sheet.addIconSet { range = rangeOf "A1" "A3", style = Style.ThreeSymbols, lowMax = 3, midMax = 6 }
                in
                Expect.equal ( Nothing, Nothing ) ( Sheet.iconAt (at "B1") s, Sheet.iconAt (at "C9") s )
        ]


-- LAMBDA & HIGHER-ORDER ------------------------------------------------------


lambdaTests : Test
lambdaTests =
    describe "LAMBDA & higher-order helpers"
        [ test "MAP applies a lambda elementwise (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=MAP(A1:A3,LAMBDA(x,x*x))" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 9 ) ( valOf "D1" s, valOf "D3" s )
        , test "MAP over two arrays" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "1" ), ( "A2", "2" ), ( "B1", "10" ), ( "B2", "20" )
                            , ( "D1", "=MAP(A1:A2,B1:B2,LAMBDA(a,b,a+b))" )
                            ]
                in
                expectVal2 ( VNumber 11, VNumber 22 ) ( valOf "D1" s, valOf "D2" s )
        , test "REDUCE folds to a scalar" <|
            \_ ->
                expectVal (VNumber 6) (valOf "D1" (sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=REDUCE(0,A1:A3,LAMBDA(a,v,a+v))" ) ]))
        , test "SCAN emits the running accumulator (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=SCAN(0,A1:A3,LAMBDA(a,v,a+v))" ) ]
                in
                expectVal2 ( VNumber 1, VNumber 6 ) ( valOf "D1" s, valOf "D3" s )
        , test "MAKEARRAY builds a block from indices" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=MAKEARRAY(2,2,LAMBDA(r,c,r*10+c))" ) ]
                in
                Expect.equal
                    ( normVal (VNumber 11), normVal (VNumber 12), normVal (VNumber 22) )
                    ( normVal (valOf "D1" s), normVal (valOf "E1" s), normVal (valOf "E2" s) )
        , test "BYROW reduces each row to a column" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" ), ( "D1", "=BYROW(A1:B2,LAMBDA(r,SUM(r)))" ) ]
                in
                expectVal2 ( VNumber 3, VNumber 7 ) ( valOf "D1" s, valOf "D2" s )
        , test "BYCOL reduces each column to a row" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" ), ( "D1", "=BYCOL(A1:B2,LAMBDA(c,SUM(c)))" ) ]
                in
                expectVal2 ( VNumber 4, VNumber 6 ) ( valOf "D1" s, valOf "E1" s )
        ]



-- NAMED LAMBDAS (custom functions) -------------------------------------------


namedLambdaTests : Test
namedLambdaTests =
    describe "named lambdas (custom functions)"
        [ test "a defined lambda is callable by name" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "100" ) ]
                            |> Sheet.defineLambda "DISCOUNT" "=LAMBDA(p,p*0.9)"
                            |> Sheet.setRaw (at "B1") "=DISCOUNT(A1)"
                            |> Sheet.recalcAll
                in
                expectVal (VNumber 90) (valOf "B1" s)
        , test "a multi-argument custom function" <|
            \_ ->
                let
                    s =
                        sheetWith []
                            |> Sheet.defineLambda "ADD" "=LAMBDA(a,b,a+b)"
                            |> Sheet.setRaw (at "B1") "=ADD(2,3)"
                            |> Sheet.recalcAll
                in
                expectVal (VNumber 5) (valOf "B1" s)
        ]



-- ARRAY BROADCASTING ---------------------------------------------------------


broadcastTests : Test
broadcastTests =
    describe "array-broadcasting operators"
        [ test "scalar broadcasts over a range (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=A1:A3*2" ) ]
                in
                expectVal2 ( VNumber 2, VNumber 6 ) ( valOf "D1" s, valOf "D3" s )
        , test "two ranges combine elementwise" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "1" ), ( "A2", "2" ), ( "B1", "10" ), ( "B2", "20" ), ( "D1", "=A1:A2+B1:B2" ) ]
                in
                expectVal2 ( VNumber 11, VNumber 22 ) ( valOf "D1" s, valOf "D2" s )
        , test "a plain scalar expression does not spill" <|
            \_ -> expectVal (VNumber 6) (valOf "D1" (sheetWith [ ( "D1", "=2*3" ) ]))
        ]



-- STRUCTURED TABLE REFERENCES ------------------------------------------------


tableFixture : Sheet
tableFixture =
    sheetWith
        [ ( "A1", "Item" ), ( "B1", "Qty" ), ( "A2", "x" ), ( "B2", "5" ), ( "A3", "y" ), ( "B3", "7" ) ]
        |> Sheet.defineTable "SALES" (rangeOf "A1" "B3") False


tableTests : Test
tableTests =
    describe "structured table references"
        [ test "a column aggregates" <|
            \_ ->
                let
                    s =
                        tableFixture |> Sheet.setRaw (at "D1") "=SUM(SALES[Qty])" |> Sheet.recalcAll
                in
                expectVal (VNumber 12) (valOf "D1" s)
        , test "a column reference spills its data" <|
            \_ ->
                let
                    s =
                        tableFixture |> Sheet.setRaw (at "D1") "=SALES[Qty]" |> Sheet.recalcAll
                in
                expectVal2 ( VNumber 5, VNumber 7 ) ( valOf "D1" s, valOf "D2" s )
        , test "@ reads this row's cell" <|
            \_ ->
                let
                    s =
                        tableFixture |> Sheet.setRaw (at "D2") "=SALES[@Qty]" |> Sheet.recalcAll
                in
                expectVal (VNumber 5) (valOf "D2" s)
        , test "#Headers gives the header row" <|
            \_ ->
                let
                    s =
                        tableFixture |> Sheet.setRaw (at "D1") "=COUNTA(SALES[#Headers])" |> Sheet.recalcAll
                in
                expectVal (VNumber 2) (valOf "D1" s)
        , test "#Totals with a totals row" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "Item" ), ( "B1", "Qty" ), ( "A2", "x" ), ( "B2", "5" ), ( "A3", "y" ), ( "B3", "7" ), ( "A4", "Total" ), ( "B4", "12" ) ]
                            |> Sheet.defineTable "T" (rangeOf "A1" "B4") True
                            |> Sheet.setRaw (at "D1") "=SUM(T[#Totals])"
                            |> Sheet.recalcAll
                in
                expectVal (VNumber 12) (valOf "D1" s)
        ]



-- MODERN TEXT FUNCTIONS ------------------------------------------------------


textFn2Tests : Test
textFn2Tests =
    describe "modern text functions"
        [ test "TEXTSPLIT splits into a row (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=TEXTSPLIT(\"a,b,c\",\",\")" ) ]
                in
                Expect.equal ( VText "a", VText "c" ) ( valOf "D1" s, valOf "F1" s )
        , test "TEXTSPLIT with a row delimiter makes a 2-D block" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=TEXTSPLIT(\"a,b;c,d\",\",\",\";\")" ) ]
                in
                Expect.equal ( VText "a", VText "d" ) ( valOf "D1" s, valOf "E2" s )
        , test "TEXTBEFORE / TEXTAFTER" <|
            \_ -> Expect.equal ( VText "a", VText "b-c" ) ( ev0 "=TEXTBEFORE(\"a-b-c\",\"-\")", ev0 "=TEXTAFTER(\"a-b-c\",\"-\")" )
        , test "TEXTBEFORE with an instance count" <|
            \_ -> Expect.equal (VText "a-b") (ev0 "=TEXTBEFORE(\"a-b-c\",\"-\",2)")
        , test "ARRAYTOTEXT joins a range" <|
            \_ -> Expect.equal (VText "1, 2, 3") (valOf "D1" (sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "3" ), ( "D1", "=ARRAYTOTEXT(A1:A3)" ) ]))
        ]



-- ARRAY RESHAPING ------------------------------------------------------------


reshapeTests : Test
reshapeTests =
    describe "array reshaping"
        [ test "WRAPROWS folds a vector into rows" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=WRAPROWS(SEQUENCE(6),2)" ) ]
                in
                Expect.equal
                    ( normVal (VNumber 1), normVal (VNumber 2), normVal (VNumber 3) )
                    ( normVal (valOf "D1" s), normVal (valOf "E1" s), normVal (valOf "D2" s) )
        , test "WRAPCOLS folds a vector into columns" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=WRAPCOLS(SEQUENCE(6),2)" ) ]
                in
                Expect.equal
                    ( normVal (VNumber 1), normVal (VNumber 2), normVal (VNumber 3) )
                    ( normVal (valOf "D1" s), normVal (valOf "D2" s), normVal (valOf "E1" s) )
        , test "EXPAND pads to a larger block" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "D1", "=EXPAND(A1:A2,3,2,0)" ) ]
                in
                Expect.equal
                    ( normVal (VNumber 1), normVal (VNumber 0), normVal (VNumber 0) )
                    ( normVal (valOf "D1" s), normVal (valOf "E1" s), normVal (valOf "D3" s) )
        , test "XMATCH finds a position" <|
            \_ -> expectVal (VNumber 2) (ev [ ( "A1", VText "x" ), ( "A2", VText "y" ), ( "A3", VText "z" ) ] "=XMATCH(\"y\",A1:A3)")
        ]



-- FORMULA AUDITING -----------------------------------------------------------


auditFixture : Sheet
auditFixture =
    sheetWith [ ( "A1", "5" ), ( "B1", "=A1*2" ), ( "C1", "=ISFORMULA(B1)" ), ( "D1", "=ISFORMULA(A1)" ), ( "E1", "=FORMULATEXT(B1)" ) ]


auditTests : Test
auditTests =
    describe "formula auditing"
        [ test "ISFORMULA distinguishes formulas from literals" <|
            \_ -> Expect.equal ( VBool True, VBool False ) ( valOf "C1" auditFixture, valOf "D1" auditFixture )
        , test "FORMULATEXT returns the source" <|
            \_ -> Expect.equal (VText "=A1*2") (valOf "E1" auditFixture)
        , test "ERROR.TYPE codes the error" <|
            \_ -> expectVal2 ( VNumber 7, VNumber 2 ) ( ev0 "=ERROR.TYPE(NA())", ev0 "=ERROR.TYPE(1/0)" )
        , test "ERROR.TYPE of a non-error is #N/A" <|
            \_ -> expectVal (VError NA) (ev0 "=ERROR.TYPE(5)")
        , test "tracePrecedents lists a formula's inputs" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "C1", "=A1+B1" ) ]
                in
                Expect.equal [ at "A1", at "B1" ] (List.sortBy (\r -> ( r.row, r.col )) (Sheet.tracePrecedents (at "C1") s))
        , test "traceDependents lists who reads a cell" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "C1", "=A1+1" ) ]
                in
                Expect.equal [ at "C1" ] (Sheet.traceDependents (at "A1") s)
        ]


-- GROUPED AGGREGATION --------------------------------------------------------


groupByTests : Test
groupByTests =
    describe "GROUPBY / PIVOTBY"
        [ test "GROUPBY sums values per key (spills, sorted)" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "North" ), ( "A2", "South" ), ( "A3", "North" ), ( "A4", "South" )
                            , ( "B1", "10" ), ( "B2", "20" ), ( "B3", "30" ), ( "B4", "40" )
                            , ( "D1", "=GROUPBY(A1:A4,B1:B4,SUM)" )
                            ]
                in
                Expect.equal
                    ( VText "North", normVal (VNumber 40), VText "South", normVal (VNumber 60) )
                    ( valOf "D1" s, normVal (valOf "E1" s), valOf "D2" s, normVal (valOf "E2" s) )
        , test "GROUPBY with a LAMBDA aggregator" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "x" ), ( "A2", "x" ), ( "A3", "y" )
                            , ( "B1", "2" ), ( "B2", "3" ), ( "B3", "9" )
                            , ( "D1", "=GROUPBY(A1:A3,B1:B3,LAMBDA(v,MAX(v)))" )
                            ]
                in
                expectVal2 ( VNumber 3, VNumber 9 ) ( valOf "E1" s, valOf "E2" s )
        , test "PIVOTBY builds a crosstab" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "N" ), ( "A2", "S" ), ( "A3", "N" )
                            , ( "B1", "a" ), ( "B2", "a" ), ( "B3", "b" )
                            , ( "C1", "1" ), ( "C2", "2" ), ( "C3", "4" )
                            , ( "E1", "=PIVOTBY(A1:A3,B1:B3,C1:C3,SUM)" )
                            ]
                in
                -- header row: ["", "a", "b"]; N row: ["N", 1, 4]; S row: ["S", 2, 0]
                Expect.equal
                    ( VText "a", normVal (VNumber 1), normVal (VNumber 4) )
                    ( valOf "F1" s, normVal (valOf "F2" s), normVal (valOf "G2" s) )
        ]



-- DATABASE FUNCTIONS ---------------------------------------------------------


dbSheet : Sheet
dbSheet =
    sheetWith
        [ ( "A1", "Item" ), ( "B1", "Qty" )
        , ( "A2", "apple" ), ( "B2", "5" )
        , ( "A3", "pear" ), ( "B3", "7" )
        , ( "A4", "apple" ), ( "B4", "3" )
        , ( "G1", "Item" ), ( "G2", "apple" )
        ]


withDForm : String -> Sheet
withDForm formula =
    dbSheet |> Sheet.setRaw (at "D1") formula |> Sheet.recalcAll


dbFnTests : Test
dbFnTests =
    describe "database functions"
        [ test "DSUM over matching rows" <|
            \_ -> expectVal (VNumber 8) (valOf "D1" (withDForm "=DSUM(A1:B4,\"Qty\",G1:G2)"))
        , test "DCOUNT counts matching numeric cells" <|
            \_ -> expectVal (VNumber 2) (valOf "D1" (withDForm "=DCOUNT(A1:B4,\"Qty\",G1:G2)"))
        , test "DAVERAGE averages matching rows" <|
            \_ -> expectVal (VNumber 4) (valOf "D1" (withDForm "=DAVERAGE(A1:B4,\"Qty\",G1:G2)"))
        , test "DMAX of matching rows" <|
            \_ -> expectVal (VNumber 5) (valOf "D1" (withDForm "=DMAX(A1:B4,\"Qty\",G1:G2)"))
        , test "DGET requires a unique match" <|
            \_ -> expectVal (VError NumErr) (valOf "D1" (withDForm "=DGET(A1:B4,\"Qty\",G1:G2)"))
        , test "field by 1-based index" <|
            \_ -> expectVal (VNumber 8) (valOf "D1" (withDForm "=DSUM(A1:B4,2,G1:G2)"))
        ]



-- REGEX ----------------------------------------------------------------------


regexTests : Test
regexTests =
    describe "regex functions"
        [ test "REGEXTEST matches a digit run" <|
            \_ -> Expect.equal ( VBool True, VBool False ) ( ev0 "=REGEXTEST(\"abc123\",\"\\d+\")", ev0 "=REGEXTEST(\"abc\",\"\\d+\")" )
        , test "REGEXEXTRACT returns the first group" <|
            \_ -> Expect.equal (VText "42") (ev0 "=REGEXEXTRACT(\"order-42\",\"(\\d+)\")")
        , test "REGEXEXTRACT with a class and alternation" <|
            \_ -> Expect.equal ( VText "bar", VBool True ) ( ev0 "=REGEXEXTRACT(\"foo@bar.com\",\"@(\\w+)\")", ev0 "=REGEXTEST(\"cat\",\"cat|dog\")" )
        , test "REGEXREPLACE replaces all matches" <|
            \_ -> Expect.equal (VText "a#b#c#") (ev0 "=REGEXREPLACE(\"a1b2c3\",\"\\d\",\"#\")")
        , test "REGEXREPLACE with backreferences" <|
            \_ -> Expect.equal (VText "Smith John") (ev0 "=REGEXREPLACE(\"John Smith\",\"(\\w+) (\\w+)\",\"$2 $1\")")
        , test "case-insensitive flag" <|
            \_ -> Expect.equal ( VBool False, VBool True ) ( ev0 "=REGEXTEST(\"ABC\",\"abc\")", ev0 "=REGEXTEST(\"ABC\",\"abc\",1)" )
        , test "anchors" <|
            \_ -> Expect.equal ( VBool True, VBool False ) ( ev0 "=REGEXTEST(\"hello\",\"^h\")", ev0 "=REGEXTEST(\"hello\",\"^e\")" )
        , test "character ranges and quantifiers" <|
            \_ -> Expect.equal (VText "2024") (ev0 "=REGEXEXTRACT(\"y2024m06\",\"([0-9]+)\")")
        ]



-- STATISTICAL DEPTH ----------------------------------------------------------


statFn3Tests : Test
statFn3Tests =
    describe "statistical depth"
        [ test "FREQUENCY buckets into bins (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "1" ), ( "A2", "3" ), ( "A3", "5" ), ( "A4", "7" )
                            , ( "B1", "2" ), ( "B2", "4" ), ( "B3", "6" )
                            , ( "D1", "=FREQUENCY(A1:A4,B1:B3)" )
                            ]
                in
                Expect.equal
                    ( normVal (VNumber 1), normVal (VNumber 1), normVal (VNumber 1) )
                    ( normVal (valOf "D1" s), normVal (valOf "D3" s), normVal (valOf "D4" s) )
        , test "MODE.MULT returns every mode (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "2" ), ( "A3", "2" ), ( "A4", "3" ), ( "A5", "3" ), ( "A6", "4" ), ( "D1", "=MODE.MULT(A1:A6)" ) ]
                in
                expectVal2 ( VNumber 2, VNumber 3 ) ( valOf "D1" s, valOf "D2" s )
        , test "PERCENTRANK" <|
            \_ -> expectVal (VNumber 0.5) (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ), ( "A4", VNumber 4 ), ( "A5", VNumber 5 ) ] "=PERCENTRANK(A1:A5,3)")
        , test "TRIMMEAN drops the tails" <|
            \_ ->
                expectVal (VNumber 5.5)
                    (ev (List.indexedMap (\i n -> ( "A" ++ String.fromInt (i + 1), VNumber n )) [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]) "=TRIMMEAN(A1:A10,0.2)")
        , test "STANDARDIZE" <|
            \_ -> expectVal (VNumber 1) (ev0 "=STANDARDIZE(5,3,2)")
        , test "COVARIANCE.P" <|
            \_ ->
                let
                    v =
                        ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ), ( "B1", VNumber 2 ), ( "B2", VNumber 4 ), ( "B3", VNumber 6 ) ] "=COVARIANCE.P(A1:A3,B1:B3)"
                in
                case v of
                    VNumber x ->
                        Expect.equal True (abs (x - 1.3333333) < 0.001)

                    _ ->
                        Expect.fail "not a number"
        ]



-- AGGREGATE ------------------------------------------------------------------


aggregateTests : Test
aggregateTests =
    describe "AGGREGATE"
        [ test "sum (function 9)" <|
            \_ -> expectVal (VNumber 6) (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ) ] "=AGGREGATE(9,0,A1:A3)")
        , test "option 6 ignores errors" <|
            \_ -> expectVal (VNumber 4) (valOf "D1" (sheetWith [ ( "A1", "1" ), ( "A2", "=1/0" ), ( "A3", "3" ), ( "D1", "=AGGREGATE(9,6,A1:A3)" ) ]))
        , test "option 0 propagates an error" <|
            \_ -> expectVal (VError DivZero) (valOf "D1" (sheetWith [ ( "A1", "1" ), ( "A2", "=1/0" ), ( "A3", "3" ), ( "D1", "=AGGREGATE(9,0,A1:A3)" ) ]))
        , test "max (function 4)" <|
            \_ -> expectVal (VNumber 3) (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ) ] "=AGGREGATE(4,0,A1:A3)")
        , test "LARGE (function 14) with k" <|
            \_ -> expectVal (VNumber 2) (ev [ ( "A1", VNumber 1 ), ( "A2", VNumber 2 ), ( "A3", VNumber 3 ) ] "=AGGREGATE(14,6,A1:A3,2)")
        ]



-- JSON INTEROP ---------------------------------------------------------------


jsonTests : Test
jsonTests =
    describe "JSON interop"
        [ test "importObjects lays out a table" <|
            \_ ->
                let
                    s =
                        Json.importObjects "[{\"name\":\"Ann\",\"qty\":5},{\"name\":\"Bob\",\"qty\":7}]" (at "A1") (Sheet.empty 20 8)
                in
                Expect.equal
                    ( VText "name", VText "Ann", normVal (VNumber 7) )
                    ( valOf "A1" s, valOf "A2" s, normVal (valOf "B3" s) )
        , test "importObjects unions keys across objects" <|
            \_ ->
                let
                    s =
                        Json.importObjects "[{\"a\":1},{\"a\":2,\"b\":3}]" (at "A1") (Sheet.empty 20 8)
                in
                Expect.equal ( VText "a", VText "b", normVal (VNumber 3) ) ( valOf "A1" s, valOf "B1" s, normVal (valOf "B3" s) )
        , test "exportObjects emits array-of-objects" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "name" ), ( "B1", "qty" ), ( "A2", "Ann" ), ( "B2", "5" ) ]
                in
                Expect.equal "[{\"name\":\"Ann\",\"qty\":5}]" (Json.exportObjects (rangeOf "A1" "B2") s)
        , test "import then export round-trips" <|
            \_ ->
                let
                    s =
                        Json.importObjects "[{\"name\":\"Ann\",\"qty\":5}]" (at "A1") (Sheet.empty 20 8)
                in
                Expect.equal "[{\"name\":\"Ann\",\"qty\":5}]" (Json.exportObjects (rangeOf "A1" "B2") s)
        ]



-- DEPENDENCY EDGES (structured & spill refs) ---------------------------------


depEdgeTests : Test
depEdgeTests =
    describe "dependency edges for indirect refs"
        [ test "a structured reference makes the table cells precedents" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "Qty" ), ( "A2", "5" ), ( "A3", "7" ) ]
                            |> Sheet.defineTable "SALES" (rangeOf "A1" "A3") False
                            |> Sheet.setRaw (at "C1") "=SUM(SALES[Qty])"
                            |> Sheet.recalcAll
                in
                Expect.equal True (List.member (at "A2") (Sheet.tracePrecedents (at "C1") s))
        , test "editing a table cell reaches the structured-ref consumer" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "Qty" ), ( "A2", "5" ), ( "A3", "7" ) ]
                            |> Sheet.defineTable "SALES" (rangeOf "A1" "A3") False
                            |> Sheet.setRaw (at "C1") "=SUM(SALES[Qty])"
                            |> Sheet.recalcAll
                in
                Expect.equal True (List.member (at "C1") (Sheet.traceDependents (at "A2") s))
        , test "a spill reference depends on the spilled block" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=SEQUENCE(3)" ), ( "F1", "=SUM(D1#)" ) ]
                in
                Expect.equal True (List.member (at "D3") (Sheet.tracePrecedents (at "F1") s))
        , test "a spilled cell displays its value (not blank)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "D1", "=SEQUENCE(3)" ) ]
                in
                Expect.equal ( "1", "2", "3" )
                    ( Sheet.displayString (at "D1") s, Sheet.displayString (at "D2") s, Sheet.displayString (at "D3") s )
        ]


-- CELL BORDERS ---------------------------------------------------------------


thinBlack : Style.Border
thinBlack =
    { style = Style.Thin, color = "#000000" }


hasInline : String -> Sheet -> String -> Bool
hasInline a1 sheet prop =
    List.any (\( p, _ ) -> p == prop) (Sheet.renderedStyle (at a1) sheet).inline


borderTests : Test
borderTests =
    describe "cell borders"
        [ test "allBorders sets every edge" <|
            \_ ->
                let
                    b =
                        Sheet.borderAt (at "B2") (Sheet.allBorders (rangeOf "A1" "C3") thinBlack (Sheet.empty 10 10))
                in
                Expect.equal ( True, True, True, True )
                    ( b.top /= Nothing, b.right /= Nothing, b.bottom /= Nothing, b.left /= Nothing )
        , test "outlineBorders only borders the perimeter" <|
            \_ ->
                let
                    s =
                        Sheet.outlineBorders (rangeOf "A1" "C3") thinBlack (Sheet.empty 10 10)

                    corner =
                        Sheet.borderAt (at "A1") s

                    middle =
                        Sheet.borderAt (at "B2") s
                in
                Expect.equal ( True, True, Style.noBorders ) ( corner.top /= Nothing, corner.left /= Nothing, middle )
        , test "borders render as inline declarations" <|
            \_ ->
                let
                    s =
                        Sheet.allBorders (rangeOf "A1" "A1") thinBlack (Sheet.empty 10 10)
                in
                Expect.equal True (hasInline "A1" s "border-top")
        , test "edgeCss maps the style" <|
            \_ -> Expect.equal "1px solid #000000" (Style.edgeCss thinBlack)
        ]



-- FORMULA-BASED CONDITIONAL FORMATTING ---------------------------------------


formulaCondTests : Test
formulaCondTests =
    describe "formula-based conditional formatting"
        [ test "a per-row formula rule fires only where true" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "A2", "5" ), ( "A3", "3" ) ]
                            |> Sheet.addFormulaRule (rangeOf "A1" "A3") "=A1>2" (Style.toggleBold Style.emptyStyle)
                in
                Expect.equal ( False, True, True )
                    ( Style.isBold (Sheet.effectiveStyle (at "A1") s)
                    , Style.isBold (Sheet.effectiveStyle (at "A2") s)
                    , Style.isBold (Sheet.effectiveStyle (at "A3") s)
                    )
        , test "a cross-column formula rule (compare two columns)" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "3" ), ( "B1", "5" ), ( "A2", "8" ), ( "B2", "2" ) ]
                            |> Sheet.addFormulaRule (rangeOf "A1" "A2") "=A1>B1" (Style.toggleBold Style.emptyStyle)
                in
                Expect.equal ( False, True )
                    ( Style.isBold (Sheet.effectiveStyle (at "A1") s), Style.isBold (Sheet.effectiveStyle (at "A2") s) )
        ]



-- FORMULA AUTOCOMPLETE / SIGNATURE HELP --------------------------------------


suggestTests : Test
suggestTests =
    describe "formula suggestions"
        [ test "currentToken reads the identifier being typed" <|
            \_ -> Expect.equal ( "SU", "" ) ( Suggest.currentToken "=SU", Suggest.currentToken "=SUM(" )
        , test "matching finds functions by prefix" <|
            \_ -> Expect.equal True (List.any (\d -> d.name == "SUM") (Suggest.matching "su"))
        , test "matching is empty for an empty prefix" <|
            \_ -> Expect.equal [] (Suggest.matching "")
        , test "activeCall finds the function and argument index" <|
            \_ -> Expect.equal (Just { name = "SUM", argIndex = 1 }) (Suggest.activeCall "=SUM(1,2")
        , test "activeCall picks the innermost call" <|
            \_ -> Expect.equal (Just { name = "SUM", argIndex = 0 }) (Suggest.activeCall "=IF(A1>1, SUM(")
        , test "activeCall ignores commas inside strings" <|
            \_ -> Expect.equal (Just { name = "TEXTJOIN", argIndex = 1 }) (Suggest.activeCall "=TEXTJOIN(\",\",")
        , test "activeCall is Nothing outside any call" <|
            \_ -> Expect.equal Nothing (Suggest.activeCall "=1+2")
        , test "lookup returns the signature" <|
            \_ -> Expect.equal (Just "SUM(number1, [number2], …)") (Maybe.map .signature (Suggest.lookup "sum"))
        ]



-- CELL PROTECTION ------------------------------------------------------------


protectionTests : Test
protectionTests =
    describe "cell protection"
        [ test "locked + protected blocks editing; unlocked allows it" <|
            \_ ->
                let
                    s =
                        Sheet.empty 10 10
                            |> Sheet.setLocked (rangeOf "A1" "A1") True
                            |> Sheet.protectSheet True
                in
                Expect.equal ( False, True ) ( Sheet.isEditable (at "A1") s, Sheet.isEditable (at "B2") s )
        , test "no protection means everything is editable" <|
            \_ ->
                let
                    s =
                        Sheet.setLocked (rangeOf "A1" "A1") True (Sheet.empty 10 10)
                in
                Expect.equal True (Sheet.isEditable (at "A1") s)
        ]



-- AUTOFILTER -----------------------------------------------------------------


filterFixture : Sheet
filterFixture =
    sheetWith
        [ ( "A1", "West" ), ( "A2", "East" ), ( "A3", "West" ), ( "A4", "North" ) ]


filterTests : Test
filterTests =
    describe "autofilter"
        [ test "distinctValues lists sorted distinct values" <|
            \_ -> Expect.equal [ "East", "North", "West" ] (Sheet.distinctValues 0 (rangeOf "A1" "A4") filterFixture)
        , test "filteredOutRows hides rows not in the allowed set" <|
            \_ ->
                let
                    s =
                        Sheet.setColumnFilter 0 (Just [ "West" ]) filterFixture
                in
                Expect.equal [ 1, 3 ] (Sheet.filteredOutRows (rangeOf "A1" "A4") s)
        , test "clearing the filter hides nothing" <|
            \_ ->
                let
                    s =
                        filterFixture
                            |> Sheet.setColumnFilter 0 (Just [ "West" ])
                            |> Sheet.setColumnFilter 0 Nothing
                in
                Expect.equal [] (Sheet.filteredOutRows (rangeOf "A1" "A4") s)
        ]



-- NAME BOX -------------------------------------------------------------------


nameBoxTests : Test
nameBoxTests =
    describe "name box"
        [ test "nameForRange finds a defined name for an exact range" <|
            \_ ->
                let
                    s =
                        Sheet.defineName "Total" (rangeOf "B2" "B5") (Sheet.empty 10 10)
                in
                Expect.equal ( Just "TOTAL", Nothing )
                    ( Sheet.nameForRange (rangeOf "B2" "B5") s, Sheet.nameForRange (rangeOf "B2" "B6") s )
        ]



-- SCENARIO MANAGER -----------------------------------------------------------


scenarioFixture : Sheet
scenarioFixture =
    sheetWith [ ( "A1", "10" ), ( "B1", "=A1*2" ) ]


scenarioTests : Test
scenarioTests =
    describe "scenario manager"
        [ test "capture snapshots current inputs" <|
            \_ -> Expect.equal { name = "base", inputs = [ ( at "A1", "10" ) ] } (Scenarios.capture "base" [ at "A1" ] scenarioFixture)
        , test "apply sets inputs and recalculates" <|
            \_ ->
                let
                    s =
                        Scenarios.apply { name = "hi", inputs = [ ( at "A1", "25" ) ] } scenarioFixture
                in
                expectVal (VNumber 50) (valOf "B1" s)
        , test "summary compares scenarios against the unchanged sheet" <|
            \_ ->
                let
                    scs =
                        [ { name = "low", inputs = [ ( at "A1", "5" ) ] }
                        , { name = "high", inputs = [ ( at "A1", "100" ) ] }
                        ]
                in
                Expect.equal [ ( "low", [ "10" ] ), ( "high", [ "200" ] ) ] (Scenarios.summary scs [ at "B1" ] scenarioFixture)
        ]


-- shared float-tolerance helper for this round


near : Float -> Float -> Expect.Expectation
near expected actual =
    Expect.equal True (abs (expected - actual) < 0.001)


nnf : Float -> Float
nnf x =
    x



-- MATRIX / LINEAR ALGEBRA ----------------------------------------------------


matrixTests : Test
matrixTests =
    describe "matrix functions"
        [ test "MMULT multiplies two matrices (spills)" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" )
                            , ( "D1", "5" ), ( "E1", "6" ), ( "D2", "7" ), ( "E2", "8" )
                            , ( "G1", "=MMULT(A1:B2,D1:E2)" )
                            ]
                in
                Expect.equal
                    ( normVal (VNumber 19), normVal (VNumber 22), normVal (VNumber 43), normVal (VNumber 50) )
                    ( normVal (valOf "G1" s), normVal (valOf "H1" s), normVal (valOf "G2" s), normVal (valOf "H2" s) )
        , test "MUNIT builds an identity matrix" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "=MUNIT(2)" ) ]
                in
                Expect.equal ( normVal (VNumber 1), normVal (VNumber 0), normVal (VNumber 1) )
                    ( normVal (valOf "A1" s), normVal (valOf "B1" s), normVal (valOf "B2" s) )
        , test "MDETERM computes a determinant" <|
            \_ -> expectVal (VNumber -2) (valOf "G1" (sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" ), ( "G1", "=MDETERM(A1:B2)" ) ]))
        , test "MINVERSE inverts a matrix" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "4" ), ( "B1", "7" ), ( "A2", "2" ), ( "B2", "6" ), ( "G1", "=MINVERSE(A1:B2)" ) ]
                in
                near 0.6 (floatAt "G1" s)
        ]



-- FINANCIAL DEPTH ------------------------------------------------------------


financeFn2Tests : Test
financeFn2Tests =
    describe "financial depth"
        [ test "SLN straight-line depreciation" <|
            \_ -> expectVal (VNumber 1800) (ev0 "=SLN(10000,1000,5)")
        , test "DDB first and second period" <|
            \_ -> expectVal2 ( VNumber 4000, VNumber 2400 ) ( ev0 "=DDB(10000,1000,5,1)", ev0 "=DDB(10000,1000,5,2)" )
        , test "RATE recovers the period rate" <|
            \_ ->
                case ev0 "=RATE(12,-8.88,100)" of
                    VNumber r ->
                        near 0.01 r

                    _ ->
                        Expect.fail "not a number"
        , test "IPMT first-period interest" <|
            \_ ->
                case ev0 "=IPMT(0.008333333,1,12,1000)" of
                    VNumber x ->
                        near -8.3333 x

                    _ ->
                        Expect.fail "not a number"
        , test "XNPV of a doubling at 10% over a year is ~0" <|
            \_ ->
                case ev [ ( "A1", VNumber -100 ), ( "A2", VNumber 110 ), ( "B1", VNumber 0 ), ( "B2", VNumber 365 ) ] "=XNPV(0.1,A1:A2,B1:B2)" of
                    VNumber x ->
                        Expect.equal True (abs x < 0.01)

                    _ ->
                        Expect.fail "not a number"
        , test "XIRR of -100 -> 110 over a year is ~10%" <|
            \_ ->
                case ev [ ( "A1", VNumber -100 ), ( "A2", VNumber 110 ), ( "B1", VNumber 0 ), ( "B2", VNumber 365 ) ] "=XIRR(A1:A2,B1:B2)" of
                    VNumber x ->
                        near 0.1 x

                    _ ->
                        Expect.fail "not a number"
        ]



-- STATISTICAL DISTRIBUTIONS --------------------------------------------------


distributionTests : Test
distributionTests =
    describe "statistical distributions"
        [ test "standard-normal CDF at 0 is 0.5" <|
            \_ ->
                case ev0 "=NORM.S.DIST(0,TRUE)" of
                    VNumber x ->
                        near 0.5 x

                    _ ->
                        Expect.fail "no"
        , test "standard-normal CDF at 1.96 is ~0.975" <|
            \_ ->
                case ev0 "=NORM.S.DIST(1.96,TRUE)" of
                    VNumber x ->
                        near 0.975 x

                    _ ->
                        Expect.fail "no"
        , test "inverse standard-normal of 0.975 is ~1.96" <|
            \_ ->
                case ev0 "=NORM.S.INV(0.975)" of
                    VNumber x ->
                        near 1.96 x

                    _ ->
                        Expect.fail "no"
        , test "BINOM.DIST pmf" <|
            \_ -> expectVal (VNumber 0.3125) (ev0 "=BINOM.DIST(2,5,0.5,FALSE)")
        , test "BINOM.DIST cumulative to n is 1" <|
            \_ -> expectVal (VNumber 1) (ev0 "=BINOM.DIST(5,5,0.5,TRUE)")
        , test "POISSON pmf at 0" <|
            \_ ->
                case ev0 "=POISSON.DIST(0,1,FALSE)" of
                    VNumber x ->
                        near 0.367879 x

                    _ ->
                        Expect.fail "no"
        , test "EXPON cdf" <|
            \_ ->
                case ev0 "=EXPON.DIST(1,1,TRUE)" of
                    VNumber x ->
                        near 0.632120 x

                    _ ->
                        Expect.fail "no"
        ]



-- ENGINEERING, BASE & UNIT CONVERSION ----------------------------------------


engineeringTests : Test
engineeringTests =
    describe "engineering functions"
        [ test "DEC2BIN / DEC2HEX / DEC2OCT" <|
            \_ -> Expect.equal ( VText "1010", VText "FF", VText "10" ) ( ev0 "=DEC2BIN(10)", ev0 "=DEC2HEX(255)", ev0 "=DEC2OCT(8)" )
        , test "BIN2DEC / HEX2DEC / OCT2DEC" <|
            \_ -> expectVal2 ( VNumber 10, VNumber 255 ) ( ev0 "=BIN2DEC(1010)", ev0 "=HEX2DEC(\"FF\")" )
        , test "DEC2BIN with padding" <|
            \_ -> Expect.equal (VText "0010") (ev0 "=DEC2BIN(2,4)")
        , test "bitwise AND/OR/XOR" <|
            \_ -> Expect.equal (List.map normVal [ VNumber 1, VNumber 7, VNumber 6 ]) (List.map normVal [ ev0 "=BITAND(5,3)", ev0 "=BITOR(5,3)", ev0 "=BITXOR(5,3)" ])
        , test "bit shifts" <|
            \_ -> expectVal2 ( VNumber 8, VNumber 4 ) ( ev0 "=BITLSHIFT(1,3)", ev0 "=BITRSHIFT(16,2)" )
        , test "CONVERT length and time" <|
            \_ -> expectVal2 ( VNumber 1000, VNumber 60 ) ( ev0 "=CONVERT(1,\"km\",\"m\")", ev0 "=CONVERT(1,\"hr\",\"min\")" )
        , test "CONVERT temperature C to F" <|
            \_ -> expectVal (VNumber 32) (ev0 "=CONVERT(0,\"C\",\"F\")")
        ]



-- MULTI-FIELD PIVOT TABLE ----------------------------------------------------


ptSheet : Sheet
ptSheet =
    sheetWith
        [ ( "A1", "Region" ), ( "B1", "Product" ), ( "C1", "Sales" )
        , ( "A2", "East" ), ( "B2", "Apple" ), ( "C2", "10" )
        , ( "A3", "East" ), ( "B3", "Pear" ), ( "C3", "20" )
        , ( "A4", "West" ), ( "B4", "Apple" ), ( "C4", "30" )
        , ( "A5", "West" ), ( "B5", "Apple" ), ( "C5", "5" )
        ]


pivotTableTests : Test
pivotTableTests =
    describe "multi-field pivot table"
        [ test "single row field sums per group + grand total" <|
            \_ ->
                let
                    t =
                        Pivot.pivotTable { rowFields = [ 0 ], colField = Nothing, valueCol = 2, agg = Pivot.Sum } (rangeOf "A1" "C5") ptSheet

                    cell r c =
                        normVal (Maybe.withDefault VEmpty (List.head (List.drop c (Maybe.withDefault [] (List.head (List.drop r t))))))
                in
                -- header, East 30, West 35, Grand Total 65
                Expect.equal ( VText "East", normVal (VNumber 30), normVal (VNumber 65) )
                    ( cell 1 0 |> denorm, cell 1 1, cell 3 1 )
        , test "column field makes a crosstab" <|
            \_ ->
                let
                    t =
                        Pivot.pivotTable { rowFields = [ 0 ], colField = Just 1, valueCol = 2, agg = Pivot.Sum } (rangeOf "A1" "C5") ptSheet
                in
                -- header: Region, Apple, Pear, Total
                Expect.equal [ VText "Region", VText "Apple", VText "Pear", VText "Total" ] (Maybe.withDefault [] (List.head t))
        , test "two row fields add subtotals" <|
            \_ ->
                let
                    t =
                        Pivot.pivotTable { rowFields = [ 0, 1 ], colField = Nothing, valueCol = 2, agg = Pivot.Sum } (rangeOf "A1" "C5") ptSheet

                    rowLabels =
                        List.map (\row -> Value.toText (Maybe.withDefault VEmpty (List.head (List.drop 1 row)))) t
                in
                -- a subtotal row labelled "Total" appears for each first-field group
                Expect.equal True (List.member "Total" rowLabels)
        ]


denorm : Value -> Value
denorm v =
    v



-- RICHER CHARTS --------------------------------------------------------------


chart2Tests : Test
chart2Tests =
    describe "richer charts"
        [ test "scatterPoints normalise into the unit square (y inverted)" <|
            \_ -> Expect.equal (normPts [ ( 0, 1 ), ( 1, 0 ) ]) (normPts (Chart.scatterPoints [ ( 0, 0 ), ( 10, 10 ) ]))
        , test "stackBars stack to the tallest column total" <|
            \_ ->
                Expect.equal (List.map normPts [ [ ( 0, 0.25 ), ( 0.25, 0.25 ) ], [ ( 0, 0.5 ), ( 0.5, 0.5 ) ] ])
                    (List.map normPts (Chart.stackBars [ [ 1, 1 ], [ 2, 2 ] ]))
        , test "niceMax rounds up to 1/2/5 x power of ten" <|
            \_ -> Expect.equal (List.map nnf [ 2000, 10, 50 ]) (List.map nnf [ Chart.niceMax 1200, Chart.niceMax 7, Chart.niceMax 45 ])
        , test "gridLevels are evenly spaced from 1 to 0" <|
            \_ -> Expect.equal (normPts (List.map (\f -> ( f, 0 )) [ 1, 0.75, 0.5, 0.25, 0 ])) (normPts (List.map (\f -> ( f, 0 )) (Chart.gridLevels 4)))
        ]



-- DATA TOOLS -----------------------------------------------------------------


dataToolTests : Test
dataToolTests =
    describe "data tools"
        [ test "removeDuplicates compacts duplicate rows" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "x" ), ( "A2", "y" ), ( "A3", "x" ), ( "A4", "z" ) ]
                            |> Sheet.removeDuplicates (rangeOf "A1" "A4")
                in
                Expect.equal ( VText "x", VText "y", VText "z", VEmpty ) ( valOf "A1" s, valOf "A2" s, valOf "A3" s, valOf "A4" s )
        , test "textToColumns splits by a delimiter" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "a,b,c" ) ] |> Sheet.textToColumns (rangeOf "A1" "A1") "," |> Sheet.recalcAll
                in
                Expect.equal ( VText "a", VText "b", VText "c" ) ( valOf "A1" s, valOf "B1" s, valOf "C1" s )
        , test "transposeRange flips rows and columns" <|
            \_ ->
                let
                    s =
                        sheetWith [ ( "A1", "1" ), ( "B1", "2" ), ( "A2", "3" ), ( "B2", "4" ) ]
                            |> Sheet.transposeRange (rangeOf "A1" "B2") (at "D1")
                            |> Sheet.recalcAll
                in
                expectVal2 ( VNumber 3, VNumber 2 ) ( valOf "E1" s, valOf "D2" s )
        , test "sortByKeys sorts by two keys" <|
            \_ ->
                let
                    s =
                        sheetWith
                            [ ( "A1", "West" ), ( "B1", "2" ), ( "A2", "East" ), ( "B2", "9" ), ( "A3", "East" ), ( "B3", "1" ) ]
                            |> Sheet.sortByKeys (rangeOf "A1" "B3") [ ( 0, True ), ( 1, True ) ]
                in
                -- East/1, East/9, West/2
                Expect.equal ( VText "East", normVal (VNumber 1), VText "West" ) ( valOf "A1" s, normVal (valOf "B1" s), valOf "A3" s )
        ]
