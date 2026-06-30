module Spreadsheet.Forecast exposing
    ( Method(..), Config, defaultConfig
    , Model, fit, forecast, fitAndForecast
    , detectSeason
    , confidenceInterval
    , Accuracy, accuracy
    )

{-| **Time-series forecasting** by exponential smoothing — the engine behind Excel's
`FORECAST.ETS` family, as a pure module (like `Analysis`, `Stats` and `Solver`).

It implements Holt–Winters: a `Linear` (double-exponential, level + trend) model for
non-seasonal series, and `Additive` / `Multiplicative` triple-exponential models that also
carry a repeating seasonal component of a given period. From a fitted `Model` you can roll the
forecast forward any number of steps, attach a prediction interval, and measure in-sample
accuracy. `detectSeason` recovers a likely period by autocorrelation (Excel's
`FORECAST.ETS.SEASONALITY`).

@docs Method, Config, defaultConfig
@docs Model, fit, forecast, fitAndForecast
@docs detectSeason
@docs confidenceInterval
@docs Accuracy, accuracy

-}


{-| How the seasonal component (if any) enters the model. `Linear` is Holt's method with no
seasonality; `Additive` adds a seasonal offset; `Multiplicative` scales by a seasonal factor
(use it when the swing grows with the level). -}
type Method
    = Linear
    | Additive
    | Multiplicative


{-| Smoothing configuration: the `method`, the seasonal `period` (ignored for `Linear`), and
the three smoothing constants `alpha` (level), `beta` (trend) and `gamma` (season), each in
`0..1`. -}
type alias Config =
    { method : Method
    , period : Int
    , alpha : Float
    , beta : Float
    , gamma : Float
    }


{-| A reasonable default for a seasonal series of the given period (additive, moderately
reactive). Pass `period = 1` together with `Linear` for a non-seasonal series. -}
defaultConfig : Int -> Config
defaultConfig period =
    { method =
        if period <= 1 then
            Linear

        else
            Additive
    , period = period
    , alpha = 0.5
    , beta = 0.1
    , gamma = 0.3
    }


{-| A fitted model: the final smoothing state plus the in-sample one-step-ahead `fitted`
series (handy for residuals / accuracy). -}
type alias Model =
    { config : Config
    , level : Float
    , trend : Float
    , seasonals : List Float
    , fitted : List Float
    , observations : Int
    }


type alias State =
    { level : Float
    , trend : Float
    , seasonals : List Float
    , fitted : List Float
    }


{-| Fit a model to `series`. Returns `Nothing` if there isn't enough data — at least two
points for `Linear`, or two full seasons (`2 × period`) for a seasonal method. -}
fit : Config -> List Float -> Maybe Model
fit config series =
    let
        n =
            List.length series

        m =
            max 1 config.period
    in
    case config.method of
        Linear ->
            if n < 2 then
                Nothing

            else
                Just (runLinear config series n)

        _ ->
            if n < 2 * m then
                Nothing

            else
                Just (runSeasonal config series n m)


runLinear : Config -> List Float -> Int -> Model
runLinear config series n =
    case series of
        x0 :: x1 :: rest ->
            let
                -- Seed L=x0, T=x1-x0 and recur from x1 onward (classic Holt). `fitted` is
                -- seeded with x0 so it stays index-aligned with `series` for residuals.
                final =
                    List.foldl (linearStep config)
                        { level = x0, trend = x1 - x0, seasonals = [ 0 ], fitted = [ x0 ] }
                        (x1 :: rest)
            in
            { config = config
            , level = final.level
            , trend = final.trend
            , seasonals = [ 0 ]
            , fitted = List.reverse final.fitted
            , observations = n
            }

        _ ->
            { config = config, level = 0, trend = 0, seasonals = [ 0 ], fitted = [], observations = n }


linearStep : Config -> Float -> State -> State
linearStep config x st =
    let
        predicted =
            st.level + st.trend

        newLevel =
            config.alpha * x + (1 - config.alpha) * (st.level + st.trend)

        newTrend =
            config.beta * (newLevel - st.level) + (1 - config.beta) * st.trend
    in
    { st | level = newLevel, trend = newTrend, fitted = predicted :: st.fitted }


runSeasonal : Config -> List Float -> Int -> Int -> Model
runSeasonal config series n m =
    let
        firstSeason =
            List.take m series

        secondSeason =
            List.take m (List.drop m series)

        level0 =
            meanOf firstSeason

        trend0 =
            (meanOf secondSeason - level0) / toFloat m

        seasonals0 =
            case config.method of
                Multiplicative ->
                    List.map (\x -> safeDiv x level0) firstSeason

                _ ->
                    List.map (\x -> x - level0) firstSeason

        final =
            List.foldl (seasonalStep config)
                { level = level0, trend = trend0, seasonals = seasonals0, fitted = [] }
                series
    in
    { config = config
    , level = final.level
    , trend = final.trend
    , seasonals = final.seasonals
    , fitted = List.reverse final.fitted
    , observations = n
    }


seasonalStep : Config -> Float -> State -> State
seasonalStep config x st =
    let
        sOld =
            Maybe.withDefault 0 (List.head st.seasonals)

        ( predicted, newLevel, newSeason ) =
            case config.method of
                Multiplicative ->
                    let
                        l =
                            config.alpha * safeDiv x sOld + (1 - config.alpha) * (st.level + st.trend)
                    in
                    ( (st.level + st.trend) * sOld
                    , l
                    , config.gamma * safeDiv x l + (1 - config.gamma) * sOld
                    )

                _ ->
                    let
                        l =
                            config.alpha * (x - sOld) + (1 - config.alpha) * (st.level + st.trend)
                    in
                    ( st.level + st.trend + sOld
                    , l
                    , config.gamma * (x - l) + (1 - config.gamma) * sOld
                    )

        newTrend =
            config.beta * (newLevel - st.level) + (1 - config.beta) * st.trend
    in
    { level = newLevel
    , trend = newTrend
    , seasonals = List.drop 1 st.seasonals ++ [ newSeason ]
    , fitted = predicted :: st.fitted
    }


{-| Forecast the next `horizon` steps beyond the fitted data. -}
forecast : Int -> Model -> List Float
forecast horizon model =
    let
        m =
            List.length model.seasonals
    in
    List.range 1 horizon
        |> List.map
            (\h ->
                let
                    seasonal =
                        Maybe.withDefault 0 (listGet (modBy m (h - 1)) model.seasonals)
                in
                case model.config.method of
                    Linear ->
                        model.level + toFloat h * model.trend

                    Multiplicative ->
                        (model.level + toFloat h * model.trend) * seasonal

                    Additive ->
                        model.level + toFloat h * model.trend + seasonal
            )


{-| Convenience: fit and forecast in one call. -}
fitAndForecast : Config -> Int -> List Float -> Maybe (List Float)
fitAndForecast config horizon series =
    fit config series |> Maybe.map (forecast horizon)


{-| The most likely seasonal period of `series`, found by maximizing the autocorrelation over
lags `2 … n/2`. Returns `1` (i.e. no seasonality) when no lag shows positive autocorrelation. -}
detectSeason : List Float -> Int
detectSeason series =
    let
        n =
            List.length series

        mean =
            meanOf series

        denom =
            List.sum (List.map (\x -> (x - mean) ^ 2) series)

        acf lag =
            if denom == 0 then
                0

            else
                let
                    pairs =
                        List.map2 (\a b -> (a - mean) * (b - mean))
                            (List.drop lag series)
                            series
                in
                List.sum pairs / denom

        scored =
            List.range 2 (n // 2)
                |> List.map (\lag -> ( lag, acf lag ))
                |> List.filter (\( _, s ) -> s > 0)
                |> List.sortBy (\( _, s ) -> negate s)
    in
    case scored of
        ( lag, _ ) :: _ ->
            lag

        [] ->
            1


{-| The half-width of a prediction interval at confidence `level` (e.g. `0.95`) for a forecast
`horizon` steps ahead: `z · σ · √horizon`, where `σ` is the in-sample residual standard
deviation against the `series` the model was fit on. Add and subtract it from the point
forecast. -}
confidenceInterval : Float -> Int -> List Float -> Model -> Float
confidenceInterval level horizon series model =
    let
        residuals =
            List.map2 (-) series model.fitted

        sigma =
            sqrt (meanOf (List.map (\r -> r * r) residuals))
    in
    zForLevel level * sigma * sqrt (toFloat (max 1 horizon))


{-| In-sample fit quality. -}
type alias Accuracy =
    { mae : Float, rmse : Float, mape : Float }


{-| Mean absolute error, root-mean-square error and mean absolute percentage error of the
model's one-step-ahead fitted values against the original series it was fit on. -}
accuracy : Model -> List Float -> Accuracy
accuracy model series =
    let
        errs =
            List.map2 (-) series model.fitted

        n =
            toFloat (max 1 (List.length errs))

        mape =
            List.map2 (\actual err -> abs (safeDiv err actual)) series errs
                |> List.sum
                |> (\s -> s / n * 100)
    in
    { mae = List.sum (List.map abs errs) / n
    , rmse = sqrt (List.sum (List.map (\e -> e * e) errs) / n)
    , mape = mape
    }


-- HELPERS ------------------------------------------------------------------


zForLevel : Float -> Float
zForLevel level =
    -- common critical values; falls back to ~95%.
    if level >= 0.99 then
        2.576

    else if level >= 0.975 then
        2.241

    else if level >= 0.95 then
        1.96

    else if level >= 0.9 then
        1.645

    else
        1.0


meanOf : List Float -> Float
meanOf xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)


safeDiv : Float -> Float -> Float
safeDiv a b =
    if b == 0 then
        0

    else
        a / b


listGet : Int -> List a -> Maybe a
listGet i xs =
    List.head (List.drop i xs)
