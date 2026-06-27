module Spreadsheet.Ref exposing
    ( Ref
    , Range
    , colToString
    , colFromString
    , toA1
    , fromA1
    , rangeToA1
    , rangeFromA1
    , normalize
    , cellsOf
    , rowsOf
    , contains
    , width
    , height
    )

{-| Cell addressing in A1 style.

Columns and rows are stored **0-based** internally (`A` → col 0, the user-facing row
`1` → row 0) so they index naturally into the grid; the `toA1`/`fromA1` pair converts
to and from the familiar 1-based, letter-column display form. `$` absolute markers are
accepted and ignored on parse (copy/fill semantics live in the editor, not the model).

@docs Ref, Range
@docs colToString, colFromString, toA1, fromA1, rangeToA1, rangeFromA1
@docs normalize, cellsOf, rowsOf, contains, width, height

-}


{-| A single cell address. `col`/`row` are 0-based. -}
type alias Ref =
    { col : Int, row : Int }


{-| A rectangular block of cells, inclusive of both corners. Not necessarily
normalised — use `normalize` to get start ≤ end on both axes.
-}
type alias Range =
    { start : Ref, end : Ref }


{-| Spreadsheet column label for a 0-based index: `0 → "A"`, `25 → "Z"`, `26 → "AA"`. -}
colToString : Int -> String
colToString col =
    if col < 0 then
        ""

    else
        colToStringHelp col ""


colToStringHelp : Int -> String -> String
colToStringHelp n acc =
    let
        letter =
            String.fromChar (Char.fromCode (Char.toCode 'A' + modBy 26 n))

        next =
            n // 26 - 1
    in
    if next < 0 then
        letter ++ acc

    else
        colToStringHelp next (letter ++ acc)


{-| Parse a column label back to its 0-based index. `"A" → 0`, `"AA" → 26`. Case
insensitive; returns `Nothing` for non-letter input.
-}
colFromString : String -> Maybe Int
colFromString s =
    let
        chars =
            String.toList (String.toUpper s)
    in
    if List.isEmpty chars then
        Nothing

    else
        List.foldl
            (\c acc ->
                case acc of
                    Nothing ->
                        Nothing

                    Just n ->
                        if c >= 'A' && c <= 'Z' then
                            Just (n * 26 + (Char.toCode c - Char.toCode 'A' + 1))

                        else
                            Nothing
            )
            (Just 0)
            chars
            |> Maybe.map (\n -> n - 1)


{-| Render a ref in A1 display form, e.g. `{col=0,row=0} → "A1"`. -}
toA1 : Ref -> String
toA1 ref =
    colToString ref.col ++ String.fromInt (ref.row + 1)


{-| Parse an A1 string into a `Ref`. Tolerates `$` markers (`$A$1`). -}
fromA1 : String -> Maybe Ref
fromA1 raw =
    let
        s =
            String.replace "$" "" (String.trim raw)

        letters =
            String.toList s |> takeWhileAlpha

        digits =
            String.dropLeft (List.length letters) s
    in
    if List.isEmpty letters || String.isEmpty digits then
        Nothing

    else
        case ( colFromString (String.fromList letters), String.toInt digits ) of
            ( Just col, Just rowNum ) ->
                if rowNum >= 1 then
                    Just { col = col, row = rowNum - 1 }

                else
                    Nothing

            _ ->
                Nothing


takeWhileAlpha : List Char -> List Char
takeWhileAlpha chars =
    case chars of
        [] ->
            []

        c :: rest ->
            if Char.isAlpha c then
                c :: takeWhileAlpha rest

            else
                []


{-| Render a range as `A1:B5`. -}
rangeToA1 : Range -> String
rangeToA1 range =
    toA1 range.start ++ ":" ++ toA1 range.end


{-| Parse `A1:B5` into a `Range`. -}
rangeFromA1 : String -> Maybe Range
rangeFromA1 raw =
    case String.split ":" (String.trim raw) of
        [ a, b ] ->
            Maybe.map2 Range (fromA1 a) (fromA1 b)

        [ a ] ->
            fromA1 a |> Maybe.map (\r -> { start = r, end = r })

        _ ->
            Nothing


{-| Return a range with `start` the top-left and `end` the bottom-right corner. -}
normalize : Range -> Range
normalize range =
    { start =
        { col = min range.start.col range.end.col
        , row = min range.start.row range.end.row
        }
    , end =
        { col = max range.start.col range.end.col
        , row = max range.start.row range.end.row
        }
    }


{-| All cell refs in a range, row-major (left-to-right, top-to-bottom). -}
cellsOf : Range -> List Ref
cellsOf range =
    let
        n =
            normalize range
    in
    List.concatMap
        (\r ->
            List.map (\c -> { col = c, row = r })
                (List.range n.start.col n.end.col)
        )
        (List.range n.start.row n.end.row)


{-| Cells grouped by row — a 2D structure preserving the rectangle's shape, needed by
INDEX/MATCH/VLOOKUP and array-aware functions.
-}
rowsOf : Range -> List (List Ref)
rowsOf range =
    let
        n =
            normalize range
    in
    List.map
        (\r ->
            List.map (\c -> { col = c, row = r })
                (List.range n.start.col n.end.col)
        )
        (List.range n.start.row n.end.row)


{-| Is a cell inside the range? -}
contains : Range -> Ref -> Bool
contains range ref =
    let
        n =
            normalize range
    in
    ref.col >= n.start.col && ref.col <= n.end.col && ref.row >= n.start.row && ref.row <= n.end.row


{-| Column count of a range. -}
width : Range -> Int
width range =
    abs (range.end.col - range.start.col) + 1


{-| Row count of a range. -}
height : Range -> Int
height range =
    abs (range.end.row - range.start.row) + 1
