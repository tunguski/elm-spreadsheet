module Spreadsheet.Chart exposing
    ( Kind(..)
    , bars
    , pieSlices
    , linePoints
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
