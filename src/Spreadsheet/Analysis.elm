module Spreadsheet.Analysis exposing
    ( goalSeek
    , dataTable1
    , dataTable2
    )

{-| What-if analysis over a `Sheet`: solving for an input, and tabulating a formula across
ranges of inputs.

These all work by substituting a value into an input cell, recomputing the sheet, and
reading a result — so they treat the sheet as a black-box function of its inputs.

  - `goalSeek` finds the input that drives a target cell to a chosen value (a secant-method
    root finder).
  - `dataTable1` / `dataTable2` tabulate a formula cell as one or two input cells vary,
    exactly like Excel's Data Table.

@docs goalSeek, dataTable1, dataTable2

-}

import Spreadsheet.Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Value as Value exposing (Value)


{-| Find the value for the `changing` cell that makes `target` equal `toValue`, returning
the solved sheet (recalculated) or `Nothing` if the search doesn't converge. -}
goalSeek : Ref -> Float -> Ref -> Sheet -> Maybe Sheet
goalSeek target toValue changing sheet =
    let
        evalAt x =
            case Value.toNumber (Sheet.valueAt target (withInput changing x sheet)) of
                Ok y ->
                    Just (y - toValue)

                Err _ ->
                    Nothing

        start =
            case Value.toNumber (Sheet.valueAt changing sheet) of
                Ok x ->
                    x

                Err _ ->
                    1
    in
    case secant evalAt start (start + 1) 0 of
        Just solution ->
            Just (withInput changing solution sheet)

        Nothing ->
            Nothing


withInput : Ref -> Float -> Sheet -> Sheet
withInput ref x sheet =
    Sheet.recalcAll (Sheet.setRaw ref (String.fromFloat x) sheet)


secant : (Float -> Maybe Float) -> Float -> Float -> Int -> Maybe Float
secant f xa xb iter =
    if iter > 100 then
        Nothing

    else
        case ( f xa, f xb ) of
            ( Just fa, Just fb ) ->
                if abs fb < 1.0e-7 then
                    Just xb

                else if fb == fa then
                    Nothing

                else
                    let
                        xc =
                            xb - fb * (xb - xa) / (fb - fa)
                    in
                    if isNaN xc || isInfinite xc then
                        Nothing

                    else
                        secant f xb xc (iter + 1)

            _ ->
                Nothing


{-| Tabulate `formula` as the single `input` cell takes each of `values`. -}
dataTable1 : Ref -> Ref -> List Float -> Sheet -> List Value
dataTable1 formula input values sheet =
    List.map (\v -> Sheet.valueAt formula (withInput input v sheet)) values


{-| Tabulate `formula` as `rowInput` takes each of `rowValues` (the result's rows) and
`colInput` takes each of `colValues` (the columns). -}
dataTable2 : Ref -> Ref -> Ref -> List Float -> List Float -> Sheet -> List (List Value)
dataTable2 formula rowInput colInput rowValues colValues sheet =
    List.map
        (\rv ->
            List.map
                (\cv ->
                    Sheet.valueAt formula
                        (Sheet.recalcAll
                            (Sheet.setRawMany
                                [ ( rowInput, String.fromFloat rv ), ( colInput, String.fromFloat cv ) ]
                                sheet
                            )
                        )
                )
                colValues
        )
        rowValues
