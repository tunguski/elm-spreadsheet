module Spreadsheet.Spill exposing
    ( unique
    , sortBy
    , filter
    , sequence
    , transpose
    )

{-| Dynamic-array transforms: functions that turn a block of values into another block,
the way `UNIQUE` / `SORT` / `FILTER` / `SEQUENCE` / `TRANSPOSE` "spill" a result in modern
spreadsheets.

These are pure on 2-D value matrices (`List (List Value)`). A host reads a range as a
matrix, transforms it here, and writes the result back into the grid with
`Spreadsheet.Sheet.spillInto` (which refuses to overwrite occupied cells — a `#SPILL!`).

@docs unique, sortBy, filter, sequence, transpose

-}

import Spreadsheet.Value as Value exposing (Value(..))


{-| Distinct rows, keeping the first occurrence's order. -}
unique : List (List Value) -> List (List Value)
unique rows =
    uniqueHelp [] rows


uniqueHelp : List (List Value) -> List (List Value) -> List (List Value)
uniqueHelp seen rows =
    case rows of
        [] ->
            []

        row :: rest ->
            if List.member row seen then
                uniqueHelp seen rest

            else
                row :: uniqueHelp (row :: seen) rest


{-| Sort rows by the value in column `col` (0-based), ascending when `asc`. -}
sortBy : Int -> Bool -> List (List Value) -> List (List Value)
sortBy col asc rows =
    List.sortWith
        (\a b ->
            let
                o =
                    Value.compare (cellAt col a) (cellAt col b)
            in
            if asc then
                o

            else
                flipOrder o
        )
        rows


{-| Keep the rows for which `keep` is true. -}
filter : (List Value -> Bool) -> List (List Value) -> List (List Value)
filter keep rows =
    List.filter keep rows


{-| A `rows × cols` block counting up from `start` by `step` (row-major), like `SEQUENCE`. -}
sequence : Int -> Int -> Float -> Float -> List (List Value)
sequence rows cols start step =
    List.map
        (\i ->
            List.map
                (\j -> VNumber (start + step * toFloat (i * cols + j)))
                (List.range 0 (cols - 1))
        )
        (List.range 0 (rows - 1))


{-| Flip a matrix across its diagonal (rows become columns). -}
transpose : List (List Value) -> List (List Value)
transpose rows =
    if List.isEmpty rows || List.any List.isEmpty rows then
        []

    else
        List.filterMap List.head rows :: transpose (List.map (List.drop 1) rows)


cellAt : Int -> List Value -> Value
cellAt col row =
    Maybe.withDefault VEmpty (List.head (List.drop col row))


flipOrder : Order -> Order
flipOrder o =
    case o of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
