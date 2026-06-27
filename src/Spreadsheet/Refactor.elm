module Spreadsheet.Refactor exposing
    ( translate
    , insertCols
    , deleteCols
    , insertRows
    , deleteRows
    , insertColsRange
    , deleteColsRange
    , insertRowsRange
    , deleteRowsRange
    )

{-| Rewrite the cell references inside a formula's syntax tree.

Two kinds of rewrite share this module:

  - **`translate`** shifts *relative* references by a fixed delta, leaving `$`-absolute
    coordinates pinned. This is the copy/paste and autofill rule: pasting `=A1+$B$1` one
    row down gives `=A2+$B$1`.

  - **`insertCols` / `deleteCols` / `insertRows` / `deleteRows`** shift references to
    account for a structural change to the grid. These move *every* coordinate at or past
    the change — absolute ones included — because the cell a `$`-reference points at has
    physically moved. A reference into a deleted band becomes `#REF!` (`Lit (VError
    RefErr)`); a range straddling the change grows or shrinks.

A reference that would move before column/row 0 (a relative shift off the top-left edge)
also becomes `#REF!`, matching Excel.

@docs translate
@docs insertCols, deleteCols, insertRows, deleteRows
@docs insertColsRange, deleteColsRange, insertRowsRange, deleteRowsRange

-}

import Spreadsheet.Ast exposing (Expr(..))
import Spreadsheet.Ref as Ref exposing (Abs, Range, Ref)
import Spreadsheet.Value exposing (Error(..), Value(..))



-- WALKING THE TREE -----------------------------------------------------------


{-| Rewrite every reference in `expr`. `fRef`/`fRange` return `Nothing` when the reference
no longer exists, in which case it is replaced by a `#REF!` literal. The `$` flags ride
along unchanged.

`touchCross` decides what happens to cross-sheet references: copy/fill (`translate`) shifts
their relative coordinates like any other ref (`touchCross = True`), but a structural edit
on *this* sheet must leave references to *other* sheets alone (`touchCross = False`). -}
mapRefs : Bool -> (Ref -> Abs -> Maybe Ref) -> (Range -> Abs -> Abs -> Maybe Range) -> Expr -> Expr
mapRefs touchCross fRef fRange e =
    case e of
        Lit _ ->
            e

        NameE _ ->
            e

        RefE ref abs ->
            case fRef ref abs of
                Just r2 ->
                    RefE r2 abs

                Nothing ->
                    Lit (VError RefErr)

        RangeE range startAbs endAbs ->
            case fRange range startAbs endAbs of
                Just r2 ->
                    RangeE r2 startAbs endAbs

                Nothing ->
                    Lit (VError RefErr)

        SheetRefE name ref abs ->
            if touchCross then
                case fRef ref abs of
                    Just r2 ->
                        SheetRefE name r2 abs

                    Nothing ->
                        Lit (VError RefErr)

            else
                e

        SheetRangeE name range startAbs endAbs ->
            if touchCross then
                case fRange range startAbs endAbs of
                    Just r2 ->
                        SheetRangeE name r2 startAbs endAbs

                    Nothing ->
                        Lit (VError RefErr)

            else
                e

        Unary op sub ->
            Unary op (mapRefs touchCross fRef fRange sub)

        Binary op a b ->
            Binary op (mapRefs touchCross fRef fRange a) (mapRefs touchCross fRef fRange b)

        Func name args ->
            Func name (List.map (mapRefs touchCross fRef fRange) args)



-- TRANSLATE (copy / fill) ----------------------------------------------------


{-| Shift relative references by `(dCol, dRow)`; absolute coordinates stay put. A relative
shift past the top-left edge yields `#REF!`. -}
translate : Int -> Int -> Expr -> Expr
translate dCol dRow =
    mapRefs True
        (\ref abs -> shiftRef dCol dRow abs ref)
        (\range startAbs endAbs ->
            Maybe.map2 (\s e -> { start = s, end = e })
                (shiftRef dCol dRow startAbs range.start)
                (shiftRef dCol dRow endAbs range.end)
        )


shiftRef : Int -> Int -> Abs -> Ref -> Maybe Ref
shiftRef dCol dRow abs ref =
    let
        nc =
            if abs.col then
                ref.col

            else
                ref.col + dCol

        nr =
            if abs.row then
                ref.row

            else
                ref.row + dRow
    in
    if nc < 0 || nr < 0 then
        Nothing

    else
        Just { col = nc, row = nr }



-- STRUCTURAL: INSERT ---------------------------------------------------------


{-| Rewrite a formula for `n` columns inserted before column `idx`. -}
insertCols : Int -> Int -> Expr -> Expr
insertCols idx n =
    mapRefs False
        (\ref _ -> Just { ref | col = insCoord idx n ref.col })
        (\range _ _ -> insertColsRange idx n range)


{-| Rewrite a formula for `n` rows inserted before row `idx`. -}
insertRows : Int -> Int -> Expr -> Expr
insertRows idx n =
    mapRefs False
        (\ref _ -> Just { ref | row = insCoord idx n ref.row })
        (\range _ _ -> insertRowsRange idx n range)


{-| Shift a range's columns for an insert (always succeeds — a range never disappears). -}
insertColsRange : Int -> Int -> Range -> Maybe Range
insertColsRange idx n range =
    Just
        { start = { col = insCoord idx n range.start.col, row = range.start.row }
        , end = { col = insCoord idx n range.end.col, row = range.end.row }
        }


{-| Shift a range's rows for an insert. -}
insertRowsRange : Int -> Int -> Range -> Maybe Range
insertRowsRange idx n range =
    Just
        { start = { col = range.start.col, row = insCoord idx n range.start.row }
        , end = { col = range.end.col, row = insCoord idx n range.end.row }
        }


insCoord : Int -> Int -> Int -> Int
insCoord idx n c =
    if c >= idx then
        c + n

    else
        c



-- STRUCTURAL: DELETE ---------------------------------------------------------


{-| Rewrite a formula for `n` columns deleted starting at column `idx`. -}
deleteCols : Int -> Int -> Expr -> Expr
deleteCols idx n =
    mapRefs False
        (\ref _ -> Maybe.map (\c -> { ref | col = c }) (delCoord idx n ref.col))
        (\range _ _ -> deleteColsRange idx n range)


{-| Rewrite a formula for `n` rows deleted starting at row `idx`. -}
deleteRows : Int -> Int -> Expr -> Expr
deleteRows idx n =
    mapRefs False
        (\ref _ -> Maybe.map (\r -> { ref | row = r }) (delCoord idx n ref.row))
        (\range _ _ -> deleteRowsRange idx n range)


{-| Clamp/shrink a range's columns for a delete; `Nothing` if the whole range is gone. -}
deleteColsRange : Int -> Int -> Range -> Maybe Range
deleteColsRange idx n range0 =
    let
        range =
            Ref.normalize range0

        s =
            delClampStart idx n range.start.col

        e =
            delClampEnd idx n range.end.col
    in
    if s > e then
        Nothing

    else
        Just
            { start = { col = s, row = range.start.row }
            , end = { col = e, row = range.end.row }
            }


{-| Clamp/shrink a range's rows for a delete; `Nothing` if the whole range is gone. -}
deleteRowsRange : Int -> Int -> Range -> Maybe Range
deleteRowsRange idx n range0 =
    let
        range =
            Ref.normalize range0

        s =
            delClampStart idx n range.start.row

        e =
            delClampEnd idx n range.end.row
    in
    if s > e then
        Nothing

    else
        Just
            { start = { col = range.start.col, row = s }
            , end = { col = range.end.col, row = e }
            }


{-| A single coordinate under a delete: `Nothing` if it sat in the deleted band. -}
delCoord : Int -> Int -> Int -> Maybe Int
delCoord idx n c =
    if c < idx then
        Just c

    else if c >= idx + n then
        Just (c - n)

    else
        Nothing


{-| A range *start* coordinate under a delete clamps up to the band's collapse point. -}
delClampStart : Int -> Int -> Int -> Int
delClampStart idx n c =
    if c < idx then
        c

    else if c >= idx + n then
        c - n

    else
        idx


{-| A range *end* coordinate under a delete clamps down to the last surviving cell. -}
delClampEnd : Int -> Int -> Int -> Int
delClampEnd idx n c =
    if c < idx then
        c

    else if c >= idx + n then
        c - n

    else
        idx - 1
