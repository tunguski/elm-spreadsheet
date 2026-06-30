module Spreadsheet.Pivot exposing
    ( Agg(..)
    , Config
    , pivot
    , aggName
    , TableConfig
    , pivotTable
    , ShowAs(..)
    , Slicer
    , Interactive
    , PivotMatrix
    , PivotRow
    , refresh
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


-- MULTI-FIELD PIVOT TABLE ----------------------------------------------------


{-| A multi-field pivot: one or more **row fields** (nested, with a subtotal per leading
group when there are two or more), an optional **column field** (turning it into a
crosstab), a value column and an aggregate. -}
type alias TableConfig =
    { rowFields : List Int
    , colField : Maybe Int
    , valueCol : Int
    , agg : Agg
    }


{-| Build the pivot as a renderable matrix: a header row (the source field-header texts,
then the column-field values and a `Total`, or just the value header), the body rows
(nested row keys with per-group subtotals), and a grand-total row. -}
pivotTable : TableConfig -> Range -> Sheet -> List (List Value)
pivotTable cfg range sheet =
    let
        n =
            Ref.normalize range

        headerRow =
            n.start.row

        dataRows =
            List.range (headerRow + 1) n.end.row

        txt r c =
            Value.toText (Sheet.valueAt { col = c, row = r } sheet)

        colKeys =
            case cfg.colField of
                Just cf ->
                    distinctSorted (List.map (\r -> txt r cf) dataRows)

                Nothing ->
                    []

        rowKeyOf r =
            List.map (\f -> txt r f) cfg.rowFields

        rowKeys =
            distinctSortedKeys (List.map rowKeyOf dataRows)

        matchesCol mck r =
            case ( cfg.colField, mck ) of
                ( Just cf, Just ck ) ->
                    txt r cf == ck

                _ ->
                    True

        aggOver pred mck =
            aggregate cfg.agg
                (List.filterMap
                    (\r ->
                        if pred r && matchesCol mck r then
                            Just (Sheet.valueAt { col = cfg.valueCol, row = r } sheet)

                        else
                            Nothing
                    )
                    dataRows
                )

        valueCells pred =
            case cfg.colField of
                Just _ ->
                    List.map (\ck -> aggOver pred (Just ck)) colKeys ++ [ aggOver pred Nothing ]

                Nothing ->
                    [ aggOver pred Nothing ]

        leafRow rk =
            List.map VText rk ++ valueCells (\r -> rowKeyOf r == rk)

        nFields =
            List.length cfg.rowFields

        body =
            if nFields >= 2 then
                List.concatMap
                    (\( prefix, leaves ) ->
                        List.map leafRow leaves
                            ++ [ subtotalLabel prefix
                                    ++ valueCells (\r -> List.take (nFields - 1) (rowKeyOf r) == prefix)
                               ]
                    )
                    (groupByPrefix rowKeys)

            else
                List.map leafRow rowKeys

        header =
            List.map (\f -> Sheet.valueAt { col = f, row = headerRow } sheet) cfg.rowFields
                ++ (case cfg.colField of
                        Just _ ->
                            List.map VText colKeys ++ [ VText "Total" ]

                        Nothing ->
                            [ Sheet.valueAt { col = cfg.valueCol, row = headerRow } sheet ]
                   )

        grand =
            (VText "Grand Total" :: List.repeat (nFields - 1) VEmpty)
                ++ valueCells (\_ -> True)
    in
    header :: (body ++ [ grand ])


{-| The leading label cells of a subtotal row: the prefix values, then “… Total” padded to
the row-field width. -}
subtotalLabel : List String -> List Value
subtotalLabel prefix =
    List.map VText prefix ++ [ VText "Total" ]


{-| Group sorted row keys by their prefix (all but the last field); contiguous because the
keys are sorted. -}
groupByPrefix : List (List String) -> List ( List String, List (List String) )
groupByPrefix keys =
    case keys of
        [] ->
            []

        first :: _ ->
            let
                prefix =
                    dropLast first

                ( same, rest ) =
                    spanPrefix prefix keys
            in
            ( prefix, same ) :: groupByPrefix rest


spanPrefix : List String -> List (List String) -> ( List (List String), List (List String) )
spanPrefix prefix keys =
    case keys of
        [] ->
            ( [], [] )

        k :: rest ->
            if dropLast k == prefix then
                let
                    ( same, others ) =
                        spanPrefix prefix rest
                in
                ( k :: same, others )

            else
                ( [], keys )


dropLast : List a -> List a
dropLast xs =
    List.take (max 0 (List.length xs - 1)) xs


distinctSorted : List String -> List String
distinctSorted xs =
    List.sort (dedup [] xs)


distinctSortedKeys : List (List String) -> List (List String)
distinctSortedKeys keys =
    List.sortWith compareStrLists (dedupKeys [] keys)


dedup : List String -> List String -> List String
dedup seen xs =
    case xs of
        [] ->
            List.reverse seen

        x :: rest ->
            if List.member x seen then
                dedup seen rest

            else
                dedup (x :: seen) rest


dedupKeys : List (List String) -> List (List String) -> List (List String)
dedupKeys seen xs =
    case xs of
        [] ->
            List.reverse seen

        x :: rest ->
            if List.member x seen then
                dedupKeys seen rest

            else
                dedupKeys (x :: seen) rest


compareStrLists : List String -> List String -> Order
compareStrLists a b =
    case ( a, b ) of
        ( x :: xs, y :: ys ) ->
            case Basics.compare x y of
                EQ ->
                    compareStrLists xs ys

                o ->
                    o

        ( [], [] ) ->
            EQ

        ( [], _ ) ->
            LT

        ( _, [] ) ->
            GT


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



-- INTERACTIVE PIVOT ----------------------------------------------------------


{-| A "show value as" display mode for the pivot body. -}
type ShowAs
    = Raw
    | PercentOfGrandTotal
    | PercentOfColumnTotal
    | RunningTotal


{-| A slicer: keep only rows whose value in `field` (an absolute column index) is one of
`allowed`. An empty `allowed` list imposes no restriction (the slicer is "all selected"). -}
type alias Slicer =
    { field : Int, allowed : List String }


{-| An interactive pivot bound to a source: one row field, an optional column field, a value
column and aggregate, a set of slicers (filters), and a display mode. `refresh` recomputes it
against the current sheet, so editing source cells and calling `refresh` again updates it. -}
type alias Interactive =
    { rowField : Int
    , colField : Maybe Int
    , valueCol : Int
    , agg : Agg
    , slicers : List Slicer
    , showAs : ShowAs
    }


{-| One body row of the result: the row key, the per-column aggregates, and the row total. -}
type alias PivotRow =
    { key : String, values : List Float, total : Float }


{-| A computed interactive pivot: the column keys (empty when there is no column field), the
body rows, the per-column totals and the grand total — all already transformed by `showAs`. -}
type alias PivotMatrix =
    { columns : List String
    , rows : List PivotRow
    , columnTotals : List Float
    , grandTotal : Float
    }


{-| Recompute the interactive pivot against the data rows of `range` (excluding the header
row), applying its slicers and display mode. -}
refresh : Interactive -> Range -> Sheet -> PivotMatrix
refresh cfg range sheet =
    let
        n =
            Ref.normalize range

        dataRows =
            List.range (n.start.row + 1) n.end.row

        txt c r =
            Value.toText (Sheet.valueAt { col = c, row = r } sheet)

        passesSlicers r =
            List.all
                (\s ->
                    List.isEmpty s.allowed || List.member (txt s.field r) s.allowed
                )
                cfg.slicers

        keptRows =
            List.filter passesSlicers dataRows

        columns =
            case cfg.colField of
                Just cf ->
                    distinctSorted (List.map (txt cf) keptRows)

                Nothing ->
                    []

        rowKeys =
            distinctSorted (List.map (txt cfg.rowField) keptRows)

        valueAtRow r =
            Sheet.valueAt { col = cfg.valueCol, row = r } sheet

        aggCell rowKey colKey =
            keptRows
                |> List.filter (\r -> txt cfg.rowField r == rowKey)
                |> List.filter (\r -> Maybe.map (\cf -> txt cf r == colKey) cfg.colField |> Maybe.withDefault True)
                |> List.map valueAtRow
                |> aggregate cfg.agg
                |> toFloatDefault

        rawValuesFor rowKey =
            case columns of
                [] ->
                    [ aggCell rowKey "" ]

                _ ->
                    List.map (aggCell rowKey) columns

        aggColumn colKey =
            keptRows
                |> List.filter (\r -> Maybe.map (\cf -> txt cf r == colKey) cfg.colField |> Maybe.withDefault True)
                |> List.map valueAtRow
                |> aggregate cfg.agg
                |> toFloatDefault

        columnTotalsRaw =
            case columns of
                [] ->
                    [ aggColumn "" ]

                _ ->
                    List.map aggColumn columns

        grandRaw =
            keptRows
                |> List.map valueAtRow
                |> aggregate cfg.agg
                |> toFloatDefault

        rawRows =
            List.map (\rk -> { key = rk, values = rawValuesFor rk, total = List.sum (rawValuesFor rk) }) rowKeys
    in
    applyShowAs cfg.showAs
        { columns = columns
        , rows = rawRows
        , columnTotals = columnTotalsRaw
        , grandTotal = grandRaw
        }


toFloatDefault : Value -> Float
toFloatDefault v =
    case Value.toNumber v of
        Ok x ->
            x

        Err _ ->
            0


applyShowAs : ShowAs -> PivotMatrix -> PivotMatrix
applyShowAs mode matrix =
    case mode of
        Raw ->
            matrix

        PercentOfGrandTotal ->
            mapCells (\_ x -> pct x matrix.grandTotal) matrix

        PercentOfColumnTotal ->
            mapCells (\colIndex x -> pct x (Maybe.withDefault 0 (listGet colIndex matrix.columnTotals))) matrix

        RunningTotal ->
            { matrix | rows = runningDown matrix.rows }


pct : Float -> Float -> Float
pct x total =
    if total == 0 then
        0

    else
        x / total * 100


{-| Transform every body cell by `f columnIndex value`; row totals and grand total are
recomputed/transformed consistently. -}
mapCells : (Int -> Float -> Float) -> PivotMatrix -> PivotMatrix
mapCells f matrix =
    { matrix
        | rows =
            List.map
                (\row ->
                    let
                        vs =
                            List.indexedMap f row.values
                    in
                    { row | values = vs, total = List.sum vs }
                )
                matrix.rows
        , columnTotals = List.indexedMap f matrix.columnTotals
    }


{-| Cumulative sum of each column down the rows. -}
runningDown : List PivotRow -> List PivotRow
runningDown rows =
    let
        step row ( accCols, out ) =
            let
                newCols =
                    List.map2 (+) (padTo (List.length row.values) accCols) row.values
            in
            ( newCols, { row | values = newCols, total = List.sum newCols } :: out )
    in
    List.foldl step ( [], [] ) rows
        |> Tuple.second
        |> List.reverse


padTo : Int -> List Float -> List Float
padTo n xs =
    xs ++ List.repeat (max 0 (n - List.length xs)) 0


listGet : Int -> List a -> Maybe a
listGet i xs =
    List.head (List.drop i xs)
