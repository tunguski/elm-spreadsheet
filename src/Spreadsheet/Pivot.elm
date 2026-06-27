module Spreadsheet.Pivot exposing
    ( Agg(..)
    , Config
    , pivot
    , aggName
    )

{-| Summarise a range by grouping its rows on one column and aggregating another — a
one-dimensional pivot table.

`pivot` takes the **data** rows of a range (no header), the index of the column to group
by (`keyCol`) and the column to summarise (`valueCol`), and an aggregate. It returns one
`(groupKey, value)` pair per distinct key, sorted by key — ready to render or export.

@docs Agg, Config, pivot, aggName

-}

import Dict
import Spreadsheet.Ref as Ref exposing (Range)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Value as Value exposing (Error(..), Value(..))


{-| How to combine the values within a group. -}
type Agg
    = Sum
    | Count
    | Average
    | Min
    | Max


{-| Which columns to group by / summarise, and how. `keyCol`/`valueCol` are absolute
column indices. -}
type alias Config =
    { keyCol : Int
    , valueCol : Int
    , agg : Agg
    }


{-| Group the range's rows by `keyCol` and aggregate `valueCol`, sorted by key. -}
pivot : Config -> Range -> Sheet -> List ( String, Value )
pivot config range sheet =
    let
        n =
            Ref.normalize range

        grouped =
            List.foldl
                (\r acc ->
                    let
                        key =
                            Value.toText (Sheet.valueAt { col = config.keyCol, row = r } sheet)

                        v =
                            Sheet.valueAt { col = config.valueCol, row = r } sheet
                    in
                    Dict.update key (\existing -> Just (v :: Maybe.withDefault [] existing)) acc
                )
                Dict.empty
                (List.range n.start.row n.end.row)
    in
    Dict.toList grouped
        |> List.map (\( k, vals ) -> ( k, aggregate config.agg (List.reverse vals) ))


aggregate : Agg -> List Value -> Value
aggregate agg vals =
    let
        nums =
            List.filterMap
                (\v ->
                    case v of
                        VNumber x ->
                            Just x

                        _ ->
                            Nothing
                )
                vals
    in
    case agg of
        Count ->
            VNumber (toFloat (List.length vals))

        Sum ->
            VNumber (List.sum nums)

        Average ->
            case nums of
                [] ->
                    VError DivZero

                _ ->
                    VNumber (List.sum nums / toFloat (List.length nums))

        Min ->
            case nums of
                [] ->
                    VError NA

                x :: xs ->
                    VNumber (List.foldl Basics.min x xs)

        Max ->
            case nums of
                [] ->
                    VError NA

                x :: xs ->
                    VNumber (List.foldl Basics.max x xs)


{-| A short label for an aggregate (for a pivot header). -}
aggName : Agg -> String
aggName agg =
    case agg of
        Sum ->
            "Sum"

        Count ->
            "Count"

        Average ->
            "Average"

        Min ->
            "Min"

        Max ->
            "Max"
