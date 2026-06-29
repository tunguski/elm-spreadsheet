module Spreadsheet.Chart exposing
    ( Kind(..)
    , bars
    , pieSlices
    , linePoints
    , scatterPoints
    , stackBars
    , niceMax
    , gridLevels
    )

{-| Pure chart geometry — turning a list of numbers into the proportions a renderer needs,
without any DOM or SVG. `Spreadsheet.View.chart` draws the result with plain CSS (flex bars,
a `conic-gradient` pie, a `clip-path` area), so it renders on every backend.

@docs Kind, bars, pieSlices, linePoints

-}


{-| The chart shapes the view can draw. -}
type Kind
    = Column
    | Bar
    | Pie
    | Line
    | Area
    | Scatter


{-| Each value as a fraction of the maximum (0…1) — the height/length of its bar. -}
bars : List Float -> List Float
bars values =
    case List.maximum (List.map (max 0) values) of
        Just m ->
            if m <= 0 then
                List.map (\_ -> 0) values

            else
                List.map (\v -> clamp 0 1 (max 0 v / m)) values

        Nothing ->
            []


{-| Cumulative `(start, end)` fractions (0…1) for each slice of a pie — the stops of a
`conic-gradient`. -}
pieSlices : List Float -> List ( Float, Float )
pieSlices values =
    let
        total =
            List.sum (List.map (max 0) values)
    in
    if total <= 0 then
        []

    else
        let
            ( acc, _ ) =
                List.foldl
                    (\v ( slices, start ) ->
                        let
                            next =
                                start + max 0 v / total
                        in
                        ( ( start, next ) :: slices, next )
                    )
                    ( [], 0 )
                    values
        in
        List.reverse acc


{-| `(x, y)` points in 0…1, `y` inverted so the largest value sits at the top — the
vertices of a line/area chart. -}
linePoints : List Float -> List ( Float, Float )
linePoints values =
    let
        n =
            List.length values

        lo =
            Maybe.withDefault 0 (List.minimum values)

        hi =
            Maybe.withDefault 1 (List.maximum values)

        norm v =
            if hi <= lo then
                0.5

            else
                (v - lo) / (hi - lo)
    in
    List.indexedMap
        (\i v ->
            ( if n <= 1 then
                0

              else
                toFloat i / toFloat (n - 1)
            , 1 - norm v
            )
        )
        values


{-| `(x, y)` pairs mapped into the unit square: x by the x-range, y inverted by the
y-range (so larger y sits higher). The vertices of a scatter plot. -}
scatterPoints : List ( Float, Float ) -> List ( Float, Float )
scatterPoints points =
    let
        xs =
            List.map Tuple.first points

        ys =
            List.map Tuple.second points

        ( xlo, xhi ) =
            ( Maybe.withDefault 0 (List.minimum xs), Maybe.withDefault 1 (List.maximum xs) )

        ( ylo, yhi ) =
            ( Maybe.withDefault 0 (List.minimum ys), Maybe.withDefault 1 (List.maximum ys) )

        norm lo hi v =
            if hi <= lo then
                0.5

            else
                (v - lo) / (hi - lo)
    in
    List.map (\( x, y ) -> ( norm xlo xhi x, 1 - norm ylo yhi y )) points


{-| For a stacked column chart, each column is a list of series values; this returns, per
column, the `(start, height)` fractions of each segment — all normalised to the largest
column total, so the tallest stack fills the chart. -}
stackBars : List (List Float) -> List (List ( Float, Float ))
stackBars columns =
    let
        total col =
            List.sum (List.map (max 0) col)

        maxTotal =
            Maybe.withDefault 0 (List.maximum (List.map total columns))
    in
    if maxTotal <= 0 then
        List.map (List.map (\_ -> ( 0, 0 ))) columns

    else
        List.map
            (\col ->
                let
                    ( segments, _ ) =
                        List.foldl
                            (\v ( segs, start ) ->
                                let
                                    h =
                                        max 0 v / maxTotal
                                in
                                ( ( start, h ) :: segs, start + h )
                            )
                            ( [], 0 )
                            col
                in
                List.reverse segments
            )
            columns


{-| A "nice" axis maximum at or above `m` (1/2/5 × a power of ten), for a readable value
axis. -}
niceMax : Float -> Float
niceMax m =
    if m <= 0 then
        1

    else
        let
            mag =
                toFloat (10 ^ floor (logBase 10 m))

            f =
                m / mag
        in
        if f <= 1 then
            mag

        else if f <= 2 then
            2 * mag

        else if f <= 5 then
            5 * mag

        else
            10 * mag


{-| `n` evenly-spaced gridline fractions from 1 (top) down to 0 — e.g. `n = 4` →
`[1, 0.75, 0.5, 0.25, 0]`. -}
gridLevels : Int -> List Float
gridLevels n =
    if n <= 0 then
        [ 0 ]

    else
        List.map (\i -> toFloat (n - i) / toFloat n) (List.range 0 n)
