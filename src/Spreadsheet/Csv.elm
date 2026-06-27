module Spreadsheet.Csv exposing
    ( encode
    , decode
    , parse
    )

{-| CSV import and export for a `Sheet`.

`encode` writes the displayed values of a rectangular range as RFC-4180-ish CSV (fields
containing a comma, quote or newline are wrapped in double quotes, and embedded quotes are
doubled). `decode` parses CSV text and drops it into the sheet at a top-left anchor, one
field per cell, via `Sheet.setRaw` — so numeric-looking fields become numbers and a field
beginning with `=` becomes a formula, exactly as if typed. Recalculate after `decode`.

@docs encode, decode, parse

-}

import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)


{-| Render a range as CSV text (rows separated by `\n`). -}
encode : Range -> Sheet -> String
encode range sheet =
    Ref.rowsOf range
        |> List.map (\rowRefs -> String.join "," (List.map (\r -> field (Sheet.displayString r sheet)) rowRefs))
        |> String.join "\n"


field : String -> String
field s =
    if String.contains "," s || String.contains "\"" s || String.contains "\n" s then
        "\"" ++ String.replace "\"" "\"\"" s ++ "\""

    else
        s


{-| Parse CSV text and write it into the sheet with its top-left at `anchor`. Empty fields
clear their cell. -}
decode : Ref -> String -> Sheet -> Sheet
decode anchor text sheet =
    let
        rows =
            parse text
    in
    List.foldl
        (\( rowIdx, fields ) acc1 ->
            List.foldl
                (\( colIdx, value ) acc2 ->
                    Sheet.setRaw { col = anchor.col + colIdx, row = anchor.row + rowIdx } value acc2
                )
                acc1
                (List.indexedMap (\i v -> ( i, v )) fields)
        )
        sheet
        (List.indexedMap (\i fs -> ( i, fs )) rows)


{-| Parse CSV text into a list of rows, each a list of field strings. Honours quoted
fields with embedded commas, newlines and doubled quotes. -}
parse : String -> List (List String)
parse text =
    finish (loop (String.toList (normalizeNewlines text)) initState)


normalizeNewlines : String -> String
normalizeNewlines s =
    s |> String.replace "\u{000D}\n" "\n" |> String.replace "\u{000D}" "\n"


type alias State =
    { rows : List (List String)
    , row : List String
    , field : List Char
    , inQuotes : Bool
    }


initState : State
initState =
    { rows = [], row = [], field = [], inQuotes = False }


loop : List Char -> State -> State
loop chars st =
    case chars of
        [] ->
            st

        c :: rest ->
            if st.inQuotes then
                case c of
                    '"' ->
                        case rest of
                            '"' :: more ->
                                loop more { st | field = '"' :: st.field }

                            _ ->
                                loop rest { st | inQuotes = False }

                    _ ->
                        loop rest { st | field = c :: st.field }

            else
                case c of
                    '"' ->
                        loop rest { st | inQuotes = True }

                    ',' ->
                        loop rest (pushField st)

                    '\n' ->
                        loop rest (pushRow st)

                    _ ->
                        loop rest { st | field = c :: st.field }


pushField : State -> State
pushField st =
    { st | row = String.fromList (List.reverse st.field) :: st.row, field = [] }


pushRow : State -> State
pushRow st =
    let
        st2 =
            pushField st
    in
    { st2 | rows = List.reverse st2.row :: st2.rows, row = [] }


finish : State -> List (List String)
finish st =
    if List.isEmpty st.field && List.isEmpty st.row then
        List.reverse st.rows

    else
        List.reverse (pushRow st).rows
