module Spreadsheet.Json exposing
    ( importObjects
    , exportObjects
    )

{-| Import and export a sheet block as **JSON array-of-objects** — the shape most web APIs
speak (`[{"name":"Ann","qty":5}, …]`).

`importObjects` parses such a string and lays it out as a table: a header row built from
the union of the objects' keys (in first-seen order), then one row per object.
`exportObjects` does the reverse, turning a range whose first row is headers into an array
of objects.

The JSON is parsed by a small hand-written recursive-descent parser rather than a `Json`
kernel, so it runs on every backend.

@docs importObjects, exportObjects

-}

import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Value as Value exposing (Value(..))



-- JSON VALUE -----------------------------------------------------------------


type JVal
    = JNull
    | JBool Bool
    | JNum Float
    | JStr String
    | JArr (List JVal)
    | JObj (List ( String, JVal ))



-- IMPORT ---------------------------------------------------------------------


{-| Parse a JSON array of objects and write it as a table with its top-left at `anchor`: a
header row (the union of all keys, first-seen order) then one row per object. Cells are
written as raw input (so numbers stay numeric) and the sheet is recalculated. A parse
failure or a non-array leaves the sheet unchanged. -}
importObjects : String -> Ref -> Sheet -> Sheet
importObjects src anchor sheet =
    case parse src of
        Just (JArr objects) ->
            let
                keys =
                    unionKeys objects

                headerEdits =
                    List.indexedMap
                        (\c k -> ( { col = anchor.col + c, row = anchor.row }, k ))
                        keys

                rowEdits =
                    List.concat
                        (List.indexedMap
                            (\r obj ->
                                List.indexedMap
                                    (\c k -> ( { col = anchor.col + c, row = anchor.row + r + 1 }, cellText (lookupKey k obj) ))
                                    keys
                            )
                            objects
                        )
            in
            Sheet.recalcAll (Sheet.setRawMany (headerEdits ++ rowEdits) sheet)

        _ ->
            sheet


unionKeys : List JVal -> List String
unionKeys objects =
    List.foldl (\obj acc -> List.foldl addKey acc (keysOf obj)) [] objects


addKey : String -> List String -> List String
addKey k acc =
    if List.member k acc then
        acc

    else
        acc ++ [ k ]


keysOf : JVal -> List String
keysOf jval =
    case jval of
        JObj pairs ->
            List.map Tuple.first pairs

        _ ->
            []


lookupKey : String -> JVal -> Maybe JVal
lookupKey k jval =
    case jval of
        JObj pairs ->
            case List.filter (\( kk, _ ) -> kk == k) pairs of
                ( _, v ) :: _ ->
                    Just v

                [] ->
                    Nothing

        _ ->
            Nothing


cellText : Maybe JVal -> String
cellText mv =
    case mv of
        Just (JStr s) ->
            s

        Just (JNum n) ->
            Value.toText (VNumber n)

        Just (JBool b) ->
            if b then
                "TRUE"

            else
                "FALSE"

        Just JNull ->
            ""

        Just other ->
            encode other

        Nothing ->
            ""



-- EXPORT ---------------------------------------------------------------------


{-| Turn a range whose first row is headers into a JSON array of objects (one per data
row), with numbers, booleans and blanks kept as JSON `number`/`bool`/`null`. -}
exportObjects : Range -> Sheet -> String
exportObjects range sheet =
    case Ref.rowsOf range of
        headerRow :: dataRows ->
            let
                headers =
                    List.map (\r -> Value.toText (Sheet.valueAt r sheet)) headerRow

                objs =
                    List.map (rowObject headers sheet) dataRows
            in
            "[" ++ String.join "," objs ++ "]"

        [] ->
            "[]"


rowObject : List String -> Sheet -> List Ref -> String
rowObject headers sheet rowRefs =
    let
        pairs =
            List.map2 (\h ref -> jsonStr h ++ ":" ++ jsonValue (Sheet.valueAt ref sheet)) headers rowRefs
    in
    "{" ++ String.join "," pairs ++ "}"


jsonValue : Value -> String
jsonValue v =
    case v of
        VNumber n ->
            Value.toText (VNumber n)

        VBool b ->
            if b then
                "true"

            else
                "false"

        VEmpty ->
            "null"

        VText s ->
            jsonStr s

        VError e ->
            jsonStr (Value.errorText e)


encode : JVal -> String
encode jval =
    case jval of
        JNull ->
            "null"

        JBool b ->
            if b then
                "true"

            else
                "false"

        JNum n ->
            Value.toText (VNumber n)

        JStr s ->
            jsonStr s

        JArr xs ->
            "[" ++ String.join "," (List.map encode xs) ++ "]"

        JObj pairs ->
            "{" ++ String.join "," (List.map (\( k, v ) -> jsonStr k ++ ":" ++ encode v) pairs) ++ "}"


jsonStr : String -> String
jsonStr s =
    "\"" ++ String.replace "\"" "\\\"" (String.replace "\\" "\\\\" s) ++ "\""



-- PARSER ---------------------------------------------------------------------


parse : String -> Maybe JVal
parse src =
    case parseValue (skipWs (String.toList src)) of
        Just ( v, rest ) ->
            if List.isEmpty (skipWs rest) then
                Just v

            else
                Nothing

        Nothing ->
            Nothing


parseValue : List Char -> Maybe ( JVal, List Char )
parseValue chars =
    case chars of
        [] ->
            Nothing

        '{' :: rest ->
            parseObject (skipWs rest) []

        '[' :: rest ->
            parseArray (skipWs rest) []

        '"' :: rest ->
            Maybe.map (\( s, more ) -> ( JStr s, more )) (parseString rest [])

        't' :: 'r' :: 'u' :: 'e' :: rest ->
            Just ( JBool True, rest )

        'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest ->
            Just ( JBool False, rest )

        'n' :: 'u' :: 'l' :: 'l' :: rest ->
            Just ( JNull, rest )

        c :: _ ->
            if c == '-' || Char.isDigit c then
                parseNumber chars

            else
                Nothing


parseObject : List Char -> List ( String, JVal ) -> Maybe ( JVal, List Char )
parseObject chars acc =
    case chars of
        '}' :: rest ->
            Just ( JObj (List.reverse acc), rest )

        '"' :: rest ->
            case parseString rest [] of
                Just ( key, afterKey ) ->
                    case skipWs afterKey of
                        ':' :: afterColon ->
                            case parseValue (skipWs afterColon) of
                                Just ( v, afterVal ) ->
                                    case skipWs afterVal of
                                        ',' :: more ->
                                            parseObject (skipWs more) (( key, v ) :: acc)

                                        '}' :: more ->
                                            Just ( JObj (List.reverse (( key, v ) :: acc)), more )

                                        _ ->
                                            Nothing

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


parseArray : List Char -> List JVal -> Maybe ( JVal, List Char )
parseArray chars acc =
    case chars of
        ']' :: rest ->
            Just ( JArr (List.reverse acc), rest )

        _ ->
            case parseValue (skipWs chars) of
                Just ( v, afterVal ) ->
                    case skipWs afterVal of
                        ',' :: more ->
                            parseArray (skipWs more) (v :: acc)

                        ']' :: more ->
                            Just ( JArr (List.reverse (v :: acc)), more )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing


parseString : List Char -> List Char -> Maybe ( String, List Char )
parseString chars acc =
    case chars of
        [] ->
            Nothing

        '"' :: rest ->
            Just ( String.fromList (List.reverse acc), rest )

        '\\' :: c :: rest ->
            parseString rest (unescape c :: acc)

        c :: rest ->
            parseString rest (c :: acc)


unescape : Char -> Char
unescape c =
    case c of
        'n' ->
            '\n'

        't' ->
            '\t'

        'r' ->
            '\u{000D}'

        _ ->
            c


parseNumber : List Char -> Maybe ( JVal, List Char )
parseNumber chars =
    let
        ( numChars, rest ) =
            spanNumber chars []
    in
    case String.toFloat (String.fromList numChars) of
        Just n ->
            Just ( JNum n, rest )

        Nothing ->
            Nothing


spanNumber : List Char -> List Char -> ( List Char, List Char )
spanNumber chars acc =
    case chars of
        c :: rest ->
            if Char.isDigit c || c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' then
                spanNumber rest (c :: acc)

            else
                ( List.reverse acc, chars )

        [] ->
            ( List.reverse acc, [] )


skipWs : List Char -> List Char
skipWs chars =
    case chars of
        c :: rest ->
            if c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' then
                skipWs rest

            else
                chars

        [] ->
            []
