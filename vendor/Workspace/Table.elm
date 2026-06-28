module Workspace.Table exposing
    ( fromCsv, fromJson, toCsv, toJson
    , column, isEmpty
    )

{-| The neutral [`Table`](Workspace-Types#Table) is how data crosses into and out of a document:
**import** a URL or a SQL result as a `Table`, **export** a step of a document as a `Table`. This
module converts between a `Table` and CSV / JSON text.

  - [`fromCsv`](#fromCsv) auto-detects a comma or tab delimiter and takes the first row as headers.
  - [`fromJson`](#fromJson) reads an array of objects, taking the union of keys (in first-seen
    order) as headers.
  - [`toCsv`](#toCsv) / [`toJson`](#toJson) render back out, quoting CSV fields when needed.

Hosts map their own document to/from a `Table`; the workspace handles the text.

@docs fromCsv, fromJson, toCsv, toJson
@docs column, isEmpty

-}

import Json.Decode as D
import Json.Encode as E
import Workspace.Types exposing (Table)


{-| Is the table empty (no rows)? -}
isEmpty : Table -> Bool
isEmpty table =
    List.isEmpty table.rows


{-| The values of one named column, in row order ("" where the column is missing in a row). -}
column : String -> Table -> List String
column name table =
    case indexOf name table.headers of
        Just i ->
            List.map (\row -> nth i row |> Maybe.withDefault "") table.rows

        Nothing ->
            []



-- CSV ------------------------------------------------------------------------


{-| Parse delimiter-separated text into a table. The delimiter is a tab if the first line has more
tabs than commas, otherwise a comma. The first non-empty line is the header row. -}
fromCsv : String -> Table
fromCsv text =
    let
        lines =
            String.lines (String.trim text)
                |> List.filter (\l -> String.trim l /= "")

        delim =
            chooseDelimiter lines
    in
    case lines of
        [] ->
            { headers = [], rows = [] }

        header :: body ->
            { headers = splitRow delim header
            , rows = List.map (splitRow delim) body
            }


chooseDelimiter : List String -> Char
chooseDelimiter lines =
    case lines of
        first :: _ ->
            if countChar '\t' first > countChar ',' first then
                '\t'

            else
                ','

        [] ->
            ','


countChar : Char -> String -> Int
countChar c s =
    String.foldl
        (\ch acc ->
            if ch == c then
                acc + 1

            else
                acc
        )
        0
        s


{-| Split one CSV line, honouring simple double-quoted fields (with `""` escapes). -}
splitRow : Char -> String -> List String
splitRow delim line =
    let
        step ch acc =
            if acc.quoted then
                if ch == '"' then
                    { acc | quoted = False }

                else
                    { acc | cur = acc.cur ++ String.fromChar ch }

            else if ch == '"' then
                { acc | quoted = True }

            else if ch == delim then
                { acc | fields = acc.fields ++ [ String.trim acc.cur ], cur = "" }

            else
                { acc | cur = acc.cur ++ String.fromChar ch }

        final =
            String.foldl step { fields = [], cur = "", quoted = False } line
    in
    final.fields ++ [ String.trim final.cur ]


{-| Render a table to CSV text (comma-separated, fields quoted when they contain a comma, quote or
newline). -}
toCsv : Table -> String
toCsv table =
    (table.headers :: table.rows)
        |> List.map (\row -> String.join "," (List.map csvField row))
        |> String.join "\n"


csvField : String -> String
csvField s =
    if String.contains "," s || String.contains "\"" s || String.contains "\n" s then
        "\"" ++ String.replace "\"" "\"\"" s ++ "\""

    else
        s



-- JSON -----------------------------------------------------------------------


{-| Parse a JSON array of objects into a table. -}
fromJson : String -> Result String Table
fromJson text =
    D.decodeString rowsDecoder text
        |> Result.mapError D.errorToString
        |> Result.map rowsToTable


{-| Each object becomes a list of (key, string-value) pairs, preserving key order. -}
rowsDecoder : D.Decoder (List (List ( String, String )))
rowsDecoder =
    D.list (D.keyValuePairs scalarDecoder)


scalarDecoder : D.Decoder String
scalarDecoder =
    D.oneOf
        [ D.string
        , D.map String.fromFloat D.float
        , D.map String.fromInt D.int
        , D.map boolToString D.bool
        , D.null ""
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


rowsToTable : List (List ( String, String )) -> Table
rowsToTable objs =
    let
        headers =
            List.foldl (\obj acc -> List.foldl addKey acc (List.map Tuple.first obj)) [] objs

        toRow obj =
            List.map (\h -> lookup h obj |> Maybe.withDefault "") headers
    in
    { headers = headers, rows = List.map toRow objs }


addKey : String -> List String -> List String
addKey k acc =
    if List.member k acc then
        acc

    else
        acc ++ [ k ]


{-| Render a table as a JSON array of objects (all values as strings). -}
toJson : Table -> String
toJson table =
    table.rows
        |> List.map
            (\row ->
                E.object (List.map2 (\h v -> ( h, E.string v )) table.headers row)
            )
        |> E.list identity
        |> E.encode 2



-- small list helpers ---------------------------------------------------------


indexOf : a -> List a -> Maybe Int
indexOf x xs =
    indexOfHelp 0 x xs


indexOfHelp : Int -> a -> List a -> Maybe Int
indexOfHelp i x xs =
    case xs of
        [] ->
            Nothing

        y :: rest ->
            if y == x then
                Just i

            else
                indexOfHelp (i + 1) x rest


nth : Int -> List a -> Maybe a
nth i xs =
    List.drop i xs |> List.head


lookup : a -> List ( a, b ) -> Maybe b
lookup k pairs =
    case pairs of
        [] ->
            Nothing

        ( a, b ) :: rest ->
            if a == k then
                Just b

            else
                lookup k rest
