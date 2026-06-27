module Spreadsheet.Export exposing
    ( tsv
    , markdown
    , html
    , json
    )

{-| Export a rectangular range of a `Sheet` to common text formats.

`tsv` and `markdown` and `html` use each cell's *displayed* text (so formats apply);
`json` uses the underlying typed value (numbers stay numbers, blanks become `null`). All
four take the range's first row as the header where the format has a header concept.

CSV lives in `Spreadsheet.Csv` (it round-trips back into a sheet); these are one-way
presentation exports.

@docs tsv, markdown, html, json

-}

import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Value as Value exposing (Value(..))


{-| Tab-separated values. Tabs and newlines inside a cell are flattened to spaces. -}
tsv : Range -> Sheet -> String
tsv range sheet =
    rowsOfText range sheet
        |> List.map (\row -> String.join "\t" (List.map flattenWhitespace row))
        |> String.join "\n"


{-| A GitHub-flavoured Markdown table; the range's first row is the header. -}
markdown : Range -> Sheet -> String
markdown range sheet =
    case rowsOfText range sheet of
        [] ->
            ""

        header :: body ->
            let
                sep =
                    List.map (\_ -> "---") header

                line cells =
                    "| " ++ String.join " | " (List.map mdCell cells) ++ " |"
            in
            String.join "\n" (line header :: line sep :: List.map line body)


mdCell : String -> String
mdCell s =
    s |> String.replace "|" "\\|" |> flattenWhitespace


{-| An HTML `<table>`; the first row becomes `<th>` header cells. -}
html : Range -> Sheet -> String
html range sheet =
    case rowsOfText range sheet of
        [] ->
            "<table></table>"

        header :: body ->
            let
                headRow =
                    "<tr>" ++ String.concat (List.map (tag "th") header) ++ "</tr>"

                bodyRows =
                    List.map (\row -> "<tr>" ++ String.concat (List.map (tag "td") row) ++ "</tr>") body
            in
            "<table>\n<thead>" ++ headRow ++ "</thead>\n<tbody>" ++ String.concat bodyRows ++ "</tbody>\n</table>"


tag : String -> String -> String
tag name s =
    "<" ++ name ++ ">" ++ escapeHtml s ++ "</" ++ name ++ ">"


escapeHtml : String -> String
escapeHtml s =
    s
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"


{-| A JSON array of rows, each an array of typed values: numbers as numbers, text as
strings, booleans as `true`/`false`, blanks as `null`, errors as their `#…!` string. -}
json : Range -> Sheet -> String
json range sheet =
    Ref.rowsOf range
        |> List.map (\rowRefs -> "[" ++ String.join "," (List.map (\r -> jsonValue (Sheet.valueAt r sheet)) rowRefs) ++ "]")
        |> (\rows -> "[" ++ String.join "," rows ++ "]")


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
            jsonString s

        VError e ->
            jsonString (Value.errorText e)


jsonString : String -> String
jsonString s =
    "\""
        ++ (s |> String.replace "\\" "\\\\" |> String.replace "\"" "\\\"" |> String.replace "\n" "\\n")
        ++ "\""



-- SHARED ---------------------------------------------------------------------


{-| The range as rows of display strings. -}
rowsOfText : Range -> Sheet -> List (List String)
rowsOfText range sheet =
    List.map (List.map (\r -> Sheet.displayString r sheet)) (Ref.rowsOf range)


flattenWhitespace : String -> String
flattenWhitespace s =
    s |> String.replace "\t" " " |> String.replace "\n" " "
