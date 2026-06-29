module Spreadsheet.Suggest exposing
    ( FnDoc
    , catalog
    , lookup
    , currentToken
    , matching
    , ActiveCall
    , activeCall
    )

{-| Formula authoring help: function-name autocomplete and in-line signature hints, the
kind of thing a spreadsheet pops up while you type a formula.

It is pure data + string analysis, so it is unit-testable without a DOM. The host (the
formula bar / cell editor) feeds it the text typed so far and renders the results:

  - `currentToken` — the identifier being typed at the caret (the autocomplete prefix).
  - `matching` — the catalog entries whose name starts with that prefix.
  - `activeCall` — the function whose argument list the caret is currently inside, and
    which argument index — so the host can show `SUM(number1, [number2], …)` with the
    active argument highlighted.

@docs FnDoc, catalog, lookup, currentToken, matching, ActiveCall, activeCall

-}


{-| A documented function: its name, a human signature and a one-line summary. -}
type alias FnDoc =
    { name : String, signature : String, summary : String }


{-| A curated catalog covering the common functions (a representative subset of the ~195
the engine implements), enough to back autocomplete and signature help. -}
catalog : List FnDoc
catalog =
    [ { name = "SUM", signature = "SUM(number1, [number2], …)", summary = "Adds its arguments." }
    , { name = "AVERAGE", signature = "AVERAGE(number1, [number2], …)", summary = "The arithmetic mean of its arguments." }
    , { name = "COUNT", signature = "COUNT(value1, [value2], …)", summary = "Counts the numbers in the list." }
    , { name = "COUNTA", signature = "COUNTA(value1, [value2], …)", summary = "Counts the non-empty values." }
    , { name = "MIN", signature = "MIN(number1, [number2], …)", summary = "The smallest number." }
    , { name = "MAX", signature = "MAX(number1, [number2], …)", summary = "The largest number." }
    , { name = "ROUND", signature = "ROUND(number, num_digits)", summary = "Rounds to a number of digits." }
    , { name = "IF", signature = "IF(condition, then, [else])", summary = "Branches on a condition." }
    , { name = "IFS", signature = "IFS(cond1, val1, [cond2, val2], …)", summary = "The value for the first true condition." }
    , { name = "IFERROR", signature = "IFERROR(value, fallback)", summary = "Traps an error to a fallback." }
    , { name = "AND", signature = "AND(logical1, [logical2], …)", summary = "True when all arguments are true." }
    , { name = "OR", signature = "OR(logical1, [logical2], …)", summary = "True when any argument is true." }
    , { name = "VLOOKUP", signature = "VLOOKUP(key, range, col, [exact])", summary = "Looks a key up in the first column." }
    , { name = "INDEX", signature = "INDEX(range, row, [col])", summary = "The value at a position in a range." }
    , { name = "MATCH", signature = "MATCH(key, range, [type])", summary = "The position of a key in a range." }
    , { name = "XLOOKUP", signature = "XLOOKUP(key, lookup, return, [if_na])", summary = "Looks up a key and returns a parallel value." }
    , { name = "SUMIF", signature = "SUMIF(range, criterion, [sum_range])", summary = "Sums the cells meeting a criterion." }
    , { name = "SUMIFS", signature = "SUMIFS(sum_range, range1, crit1, …)", summary = "Sums with multiple criteria." }
    , { name = "COUNTIF", signature = "COUNTIF(range, criterion)", summary = "Counts the cells meeting a criterion." }
    , { name = "CONCAT", signature = "CONCAT(text1, [text2], …)", summary = "Joins text together." }
    , { name = "TEXTJOIN", signature = "TEXTJOIN(delim, skip_empty, text1, …)", summary = "Joins text with a delimiter." }
    , { name = "LEFT", signature = "LEFT(text, [count])", summary = "The leftmost characters of text." }
    , { name = "RIGHT", signature = "RIGHT(text, [count])", summary = "The rightmost characters of text." }
    , { name = "MID", signature = "MID(text, start, count)", summary = "A substring of text." }
    , { name = "LEN", signature = "LEN(text)", summary = "The length of text." }
    , { name = "SUBSTITUTE", signature = "SUBSTITUTE(text, old, new, [n])", summary = "Replaces text." }
    , { name = "REGEXEXTRACT", signature = "REGEXEXTRACT(text, pattern, [ci])", summary = "The first regex match (or group)." }
    , { name = "SORT", signature = "SORT(range, [col], [asc])", summary = "Sorts a range (spills)." }
    , { name = "FILTER", signature = "FILTER(range, include, [if_empty])", summary = "Keeps rows where include is true (spills)." }
    , { name = "UNIQUE", signature = "UNIQUE(range)", summary = "The distinct rows of a range (spills)." }
    , { name = "SEQUENCE", signature = "SEQUENCE(rows, [cols], [start], [step])", summary = "A block of sequential numbers (spills)." }
    , { name = "GROUPBY", signature = "GROUPBY(keys, values, fn)", summary = "Groups and aggregates (spills)." }
    , { name = "LAMBDA", signature = "LAMBDA(param1, …, body)", summary = "A reusable function value." }
    , { name = "MAP", signature = "MAP(array1, …, lambda)", summary = "Applies a lambda elementwise (spills)." }
    , { name = "LET", signature = "LET(name1, value1, …, calc)", summary = "Binds names inside one formula." }
    , { name = "DATE", signature = "DATE(year, month, day)", summary = "Builds a date serial." }
    , { name = "TODAY", signature = "TODAY()", summary = "Today's date serial." }
    , { name = "PMT", signature = "PMT(rate, nper, pv, [fv], [type])", summary = "A loan payment." }
    , { name = "NPV", signature = "NPV(rate, value1, …)", summary = "Net present value." }
    ]


{-| The catalog entry for an exact function name (case-insensitive), if any. -}
lookup : String -> Maybe FnDoc
lookup name =
    let
        up =
            String.toUpper name
    in
    List.head (List.filter (\d -> d.name == up) catalog)


{-| The identifier being typed at the end of the formula text — letters/digits/dots back
from the caret. Empty when the text ends in a non-identifier character. -}
currentToken : String -> String
currentToken text =
    String.fromList (List.reverse (takeIdentRev (List.reverse (String.toList text))))


takeIdentRev : List Char -> List Char
takeIdentRev chars =
    case chars of
        c :: rest ->
            if isIdentChar c then
                c :: takeIdentRev rest

            else
                []

        [] ->
            []


isIdentChar : Char -> Bool
isIdentChar c =
    Char.isAlphaNum c || c == '_' || c == '.'


{-| The catalog entries whose name starts with `prefix` (case-insensitive). An empty prefix
matches nothing (so an idle editor shows no popup). -}
matching : String -> List FnDoc
matching prefix =
    if prefix == "" then
        []

    else
        let
            up =
                String.toUpper prefix
        in
        List.filter (\d -> String.startsWith up d.name) catalog


{-| The function whose argument list the caret is inside, and the (0-based) argument index. -}
type alias ActiveCall =
    { name : String, argIndex : Int }


{-| Scan the formula text and find the innermost *named* open call at the caret (end of
text): which function, and which argument the caret sits in. `Nothing` when the caret is
not inside any function's parentheses. -}
activeCall : String -> Maybe ActiveCall
activeCall text =
    scan (String.toList text) "" []


scan : List Char -> String -> List ( String, Int ) -> Maybe ActiveCall
scan chars ident stack =
    case chars of
        [] ->
            firstNamed stack

        '"' :: rest ->
            scan (skipString rest) "" stack

        '(' :: rest ->
            scan rest "" (( String.toUpper ident, 0 ) :: stack)

        ')' :: rest ->
            scan rest "" (List.drop 1 stack)

        ',' :: rest ->
            scan rest "" (bumpTop stack)

        c :: rest ->
            if isIdentChar c then
                scan rest (ident ++ String.fromChar c) stack

            else
                scan rest "" stack


{-| Skip a string literal's characters up to and including the closing quote. -}
skipString : List Char -> List Char
skipString chars =
    case chars of
        '"' :: rest ->
            rest

        _ :: rest ->
            skipString rest

        [] ->
            []


bumpTop : List ( String, Int ) -> List ( String, Int )
bumpTop stack =
    case stack of
        ( name, n ) :: rest ->
            ( name, n + 1 ) :: rest

        [] ->
            []


{-| The topmost stack frame with a non-empty function name (skipping grouping parens). -}
firstNamed : List ( String, Int ) -> Maybe ActiveCall
firstNamed stack =
    case stack of
        ( name, n ) :: rest ->
            if name == "" then
                firstNamed rest

            else
                Just { name = name, argIndex = n }

        [] ->
            Nothing
