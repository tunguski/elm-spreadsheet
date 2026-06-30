module Spreadsheet.Stats exposing
    ( Descriptive, describe
    , HistogramBin, histogram
    , movingAverage, exponentialSmoothing
    , correlationMatrix, covarianceMatrix
    , RankEntry, rankAndPercentile
    , TTest, tTestPaired, tTestEqualVar, tTestUnequalVar
    , ZTest, zTest
    , FTest, fTest
    , Anova, anovaSingleFactor
    , Regression, regress
    , studentTtwoTail, fUpperTail, normalTwoTail
    )

{-| A pure **Data Analysis ToolPak**: the procedures Excel hides behind its Data Analysis
add-in, expressed as plain functions over `List Float` and float matrices so they unit-test
without a workbook. Everything here is self-contained — it ships its own small numerics core
(error function, log-gamma, the regularized incomplete beta and the Student-t / F tail
probabilities) so the p-values are real, not stubs.


# Descriptive

@docs Descriptive, describe


# Histogram & smoothing

@docs HistogramBin, histogram
@docs movingAverage, exponentialSmoothing


# Association

@docs correlationMatrix, covarianceMatrix
@docs RankEntry, rankAndPercentile


# Inference

@docs TTest, tTestPaired, tTestEqualVar, tTestUnequalVar
@docs ZTest, zTest
@docs FTest, fTest
@docs Anova, anovaSingleFactor
@docs Regression, regress


# Distribution tails (exposed for reuse)

@docs studentTtwoTail, fUpperTail, normalTwoTail

-}

-- DESCRIPTIVE --------------------------------------------------------------


{-| The full descriptive-statistics summary Excel produces, using **sample** variance /
standard deviation (the `n-1` denominator) and Excel's `SKEW` / `KURT` definitions. -}
type alias Descriptive =
    { count : Int
    , sum : Float
    , mean : Float
    , median : Float
    , mode : Maybe Float
    , variance : Float
    , standardDeviation : Float
    , standardError : Float
    , skewness : Float
    , kurtosis : Float
    , minimum : Float
    , maximum : Float
    , range : Float
    }


{-| Summarize a sample. `Nothing` for an empty input. -}
describe : List Float -> Maybe Descriptive
describe xs =
    case xs of
        [] ->
            Nothing

        _ ->
            let
                n =
                    List.length xs

                nf =
                    toFloat n

                total =
                    List.sum xs

                m =
                    total / nf

                sorted =
                    List.sort xs

                var =
                    if n < 2 then
                        0

                    else
                        List.sum (List.map (\x -> (x - m) ^ 2) xs) / (nf - 1)

                sd =
                    sqrt var
            in
            Just
                { count = n
                , sum = total
                , mean = m
                , median = medianOf sorted
                , mode = modeOf xs
                , variance = var
                , standardDeviation = sd
                , standardError = sd / sqrt nf
                , skewness = skewOf xs m sd n
                , kurtosis = kurtOf xs m sd n
                , minimum = Maybe.withDefault m (List.minimum xs)
                , maximum = Maybe.withDefault m (List.maximum xs)
                , range = Maybe.withDefault 0 (List.maximum xs) - Maybe.withDefault 0 (List.minimum xs)
                }


medianOf : List Float -> Float
medianOf sorted =
    let
        n =
            List.length sorted

        mid =
            n // 2
    in
    if n == 0 then
        0

    else if modBy 2 n == 1 then
        Maybe.withDefault 0 (listGet mid sorted)

    else
        (Maybe.withDefault 0 (listGet (mid - 1) sorted) + Maybe.withDefault 0 (listGet mid sorted)) / 2


modeOf : List Float -> Maybe Float
modeOf xs =
    let
        counts =
            List.foldl
                (\x acc ->
                    case acc of
                        ( seen, best ) ->
                            let
                                c =
                                    1 + List.length (List.filter ((==) x) xs)
                            in
                            if List.member x seen then
                                ( seen, best )

                            else
                                ( x :: seen, ( x, c ) :: best )
                )
                ( [], [] )
                xs
                |> Tuple.second
    in
    case List.sortBy (\( _, c ) -> negate c) counts of
        ( v, c ) :: _ ->
            if c > 1 then
                Just v

            else
                Nothing

        [] ->
            Nothing


skewOf : List Float -> Float -> Float -> Int -> Float
skewOf xs m sd n =
    if n < 3 || sd == 0 then
        0

    else
        let
            nf =
                toFloat n

            s =
                List.sum (List.map (\x -> ((x - m) / sd) ^ 3) xs)
        in
        nf / ((nf - 1) * (nf - 2)) * s


kurtOf : List Float -> Float -> Float -> Int -> Float
kurtOf xs m sd n =
    if n < 4 || sd == 0 then
        0

    else
        let
            nf =
                toFloat n

            s =
                List.sum (List.map (\x -> ((x - m) / sd) ^ 4) xs)

            a =
                nf * (nf + 1) / ((nf - 1) * (nf - 2) * (nf - 3))

            b =
                3 * (nf - 1) ^ 2 / ((nf - 2) * (nf - 3))
        in
        a * s - b



-- HISTOGRAM & SMOOTHING ----------------------------------------------------


{-| One histogram bar: the half-open bin `(lowerBound, upperBound]`, how many values fall in
it, and the running cumulative count. -}
type alias HistogramBin =
    { lowerBound : Float, upperBound : Float, count : Int, cumulative : Int }


{-| Bucket `xs` into `binCount` equal-width bins spanning min..max, with cumulative counts.
A degenerate spread (all values equal, or `binCount < 1`) yields a single bin. -}
histogram : Int -> List Float -> List HistogramBin
histogram binCount xs =
    case ( List.minimum xs, List.maximum xs ) of
        ( Just lo, Just hi ) ->
            let
                k =
                    max 1 binCount

                width =
                    if hi == lo then
                        1

                    else
                        (hi - lo) / toFloat k

                binOf x =
                    if hi == lo then
                        0

                    else
                        min (k - 1) (floor ((x - lo) / width))

                countIn i =
                    List.length (List.filter (\x -> binOf x == i) xs)

                bins =
                    List.map countIn (List.range 0 (k - 1))
            in
            List.foldl
                (\( i, c ) ( running, acc ) ->
                    let
                        cum =
                            running + c
                    in
                    ( cum
                    , { lowerBound = lo + toFloat i * width
                      , upperBound = lo + toFloat (i + 1) * width
                      , count = c
                      , cumulative = cum
                      }
                        :: acc
                    )
                )
                ( 0, [] )
                (List.indexedMap Tuple.pair bins)
                |> Tuple.second
                |> List.reverse

        _ ->
            []


{-| Trailing simple moving average with window `k`: the i-th output is the mean of the `k`
values ending at position i, so the result has `length - k + 1` entries (Excel leaves the
leading `k-1` as `#N/A`; here they are simply absent). -}
movingAverage : Int -> List Float -> List Float
movingAverage k xs =
    if k < 1 then
        []

    else
        let
            n =
                List.length xs

            windowAt i =
                List.take k (List.drop i xs)
        in
        List.range 0 (n - k)
            |> List.map (\i -> List.sum (windowAt i) / toFloat k)


{-| Single exponential smoothing with smoothing constant `alpha` (Excel's tool takes the
*damping factor* `1 - alpha`). `s₀ = x₀`; `sₜ = α·xₜ + (1-α)·sₜ₋₁`. -}
exponentialSmoothing : Float -> List Float -> List Float
exponentialSmoothing alpha xs =
    case xs of
        [] ->
            []

        first :: rest ->
            List.reverse
                (List.foldl
                    (\x acc ->
                        case acc of
                            prev :: _ ->
                                (alpha * x + (1 - alpha) * prev) :: acc

                            [] ->
                                [ x ]
                    )
                    [ first ]
                    rest
                )



-- ASSOCIATION --------------------------------------------------------------


{-| Pearson **correlation** matrix of the given variables (each inner list is one variable's
observations). Symmetric, `1` on the diagonal. -}
correlationMatrix : List (List Float) -> List (List Float)
correlationMatrix vars =
    List.map (\a -> List.map (\b -> correlation a b) vars) vars


{-| Population **covariance** matrix (the `÷n` form Excel's Covariance tool uses). -}
covarianceMatrix : List (List Float) -> List (List Float)
covarianceMatrix vars =
    List.map (\a -> List.map (\b -> covariancePop a b) vars) vars


covariancePop : List Float -> List Float -> Float
covariancePop a b =
    let
        n =
            toFloat (min (List.length a) (List.length b))

        ma =
            meanOf a

        mb =
            meanOf b
    in
    if n == 0 then
        0

    else
        List.sum (List.map2 (\x y -> (x - ma) * (y - mb)) a b) / n


correlation : List Float -> List Float -> Float
correlation a b =
    let
        sa =
            sqrt (covariancePop a a)

        sb =
            sqrt (covariancePop b b)
    in
    if sa == 0 || sb == 0 then
        0

    else
        covariancePop a b / (sa * sb)


{-| One row of the Rank-and-Percentile table. -}
type alias RankEntry =
    { index : Int, value : Float, rank : Int, percent : Float }


{-| Excel's Rank and Percentile tool: largest value is rank 1 (ties share a rank), and
`percent` is the inclusive percentile rank `(values strictly below) / (n-1)`, in 0..1.
Returned sorted by rank (largest value first); `index` is the original 0-based position. -}
rankAndPercentile : List Float -> List RankEntry
rankAndPercentile xs =
    let
        n =
            List.length xs

        denom =
            toFloat (max 1 (n - 1))
    in
    xs
        |> List.indexedMap
            (\i v ->
                { index = i
                , value = v
                , rank = 1 + List.length (List.filter (\o -> o > v) xs)
                , percent = toFloat (List.length (List.filter (\o -> o < v) xs)) / denom
                }
            )
        |> List.sortBy .rank



-- INFERENCE ----------------------------------------------------------------


{-| A t-test outcome: the statistic, its degrees of freedom, and the one- and two-tailed
p-values. -}
type alias TTest =
    { t : Float, df : Float, pOneTail : Float, pTwoTail : Float }


{-| Paired (dependent) two-sample t-test on the element-wise differences. -}
tTestPaired : List Float -> List Float -> Maybe TTest
tTestPaired a b =
    let
        diffs =
            List.map2 (-) a b

        n =
            List.length diffs
    in
    if n < 2 then
        Nothing

    else
        let
            md =
                meanOf diffs

            sd =
                sqrt (sampleVar diffs)

            nf =
                toFloat n

            t =
                md / (sd / sqrt nf)

            df =
                nf - 1
        in
        Just (tResult t df)


{-| Two-sample t-test assuming **equal** variances (pooled). -}
tTestEqualVar : List Float -> List Float -> Maybe TTest
tTestEqualVar a b =
    let
        n1 =
            List.length a

        n2 =
            List.length b
    in
    if n1 < 2 || n2 < 2 then
        Nothing

    else
        let
            ( m1, m2 ) =
                ( meanOf a, meanOf b )

            ( v1, v2 ) =
                ( sampleVar a, sampleVar b )

            ( f1, f2 ) =
                ( toFloat n1, toFloat n2 )

            sp =
                sqrt (((f1 - 1) * v1 + (f2 - 1) * v2) / (f1 + f2 - 2))

            t =
                (m1 - m2) / (sp * sqrt (1 / f1 + 1 / f2))
        in
        Just (tResult t (f1 + f2 - 2))


{-| Two-sample t-test assuming **unequal** variances (Welch), with Welch–Satterthwaite df. -}
tTestUnequalVar : List Float -> List Float -> Maybe TTest
tTestUnequalVar a b =
    let
        n1 =
            List.length a

        n2 =
            List.length b
    in
    if n1 < 2 || n2 < 2 then
        Nothing

    else
        let
            ( m1, m2 ) =
                ( meanOf a, meanOf b )

            ( v1, v2 ) =
                ( sampleVar a, sampleVar b )

            ( f1, f2 ) =
                ( toFloat n1, toFloat n2 )

            ( s1, s2 ) =
                ( v1 / f1, v2 / f2 )

            t =
                (m1 - m2) / sqrt (s1 + s2)

            df =
                (s1 + s2) ^ 2 / (s1 ^ 2 / (f1 - 1) + s2 ^ 2 / (f2 - 1))
        in
        Just (tResult t df)


tResult : Float -> Float -> TTest
tResult t df =
    let
        p2 =
            studentTtwoTail t df
    in
    { t = t, df = df, pOneTail = p2 / 2, pTwoTail = p2 }


{-| A z-test outcome (one- and two-tailed p-values). -}
type alias ZTest =
    { z : Float, pOneTail : Float, pTwoTail : Float }


{-| One-sample z-test of the sample mean against `mu` with a **known** population `sigma`. -}
zTest : Float -> Float -> List Float -> Maybe ZTest
zTest mu sigma xs =
    let
        n =
            List.length xs
    in
    if n < 1 || sigma <= 0 then
        Nothing

    else
        let
            z =
                (meanOf xs - mu) / (sigma / sqrt (toFloat n))

            p2 =
                normalTwoTail z
        in
        Just { z = z, pOneTail = p2 / 2, pTwoTail = p2 }


{-| An F-test for equality of two variances. -}
type alias FTest =
    { f : Float, df1 : Int, df2 : Int, pTwoTail : Float }


{-| Two-tailed F-test that two samples have the same variance (Excel `F.TEST`). -}
fTest : List Float -> List Float -> Maybe FTest
fTest a b =
    let
        n1 =
            List.length a

        n2 =
            List.length b
    in
    if n1 < 2 || n2 < 2 then
        Nothing

    else
        let
            ( v1, v2 ) =
                ( sampleVar a, sampleVar b )
        in
        if v2 == 0 then
            Nothing

        else
            let
                f =
                    v1 / v2

                pu =
                    fUpperTail f (toFloat (n1 - 1)) (toFloat (n2 - 1))
            in
            Just
                { f = f
                , df1 = n1 - 1
                , df2 = n2 - 1
                , pTwoTail = min 1 (2 * min pu (1 - pu))
                }


{-| Single-factor ANOVA result (the classic ANOVA table). -}
type alias Anova =
    { ssBetween : Float
    , ssWithin : Float
    , ssTotal : Float
    , dfBetween : Int
    , dfWithin : Int
    , msBetween : Float
    , msWithin : Float
    , f : Float
    , pValue : Float
    }


{-| One-way ANOVA across the given groups. `Nothing` unless there are ≥2 groups and more
observations than groups. -}
anovaSingleFactor : List (List Float) -> Maybe Anova
anovaSingleFactor groups0 =
    let
        groups =
            List.filter (not << List.isEmpty) groups0

        k =
            List.length groups

        nTotal =
            List.sum (List.map List.length groups)
    in
    if k < 2 || nTotal <= k then
        Nothing

    else
        let
            grand =
                meanOf (List.concat groups)

            ssBetween =
                List.sum
                    (List.map
                        (\g -> toFloat (List.length g) * (meanOf g - grand) ^ 2)
                        groups
                    )

            ssWithin =
                List.sum
                    (List.map
                        (\g ->
                            let
                                mg =
                                    meanOf g
                            in
                            List.sum (List.map (\x -> (x - mg) ^ 2) g)
                        )
                        groups
                    )

            dfB =
                k - 1

            dfW =
                nTotal - k

            msB =
                ssBetween / toFloat dfB

            msW =
                ssWithin / toFloat dfW

            f =
                msB / msW
        in
        Just
            { ssBetween = ssBetween
            , ssWithin = ssWithin
            , ssTotal = ssBetween + ssWithin
            , dfBetween = dfB
            , dfWithin = dfW
            , msBetween = msB
            , msWithin = msW
            , f = f
            , pValue = fUpperTail f (toFloat dfB) (toFloat dfW)
            }


{-| Ordinary-least-squares **multiple regression** summary. -}
type alias Regression =
    { coefficients : List Float
    , standardErrors : List Float
    , tStats : List Float
    , rSquared : Float
    , adjustedRSquared : Float
    , standardError : Float
    , fStat : Float
    , observations : Int
    }


{-| Regress response `y` on the `predictors` (each inner list is one predictor's column),
fitting an intercept. `coefficients` is `[intercept, b₁, b₂, …]`. `Nothing` if the system is
under-determined or singular. -}
regress : List Float -> List (List Float) -> Maybe Regression
regress y predictors =
    let
        n =
            List.length y

        kp =
            List.length predictors

        -- design matrix rows: [1, x1, x2, ...]
        rows =
            List.indexedMap
                (\i _ -> 1 :: List.map (\col -> Maybe.withDefault 0 (listGet i col)) predictors)
                y

        cols =
            kp + 1
    in
    if n <= cols then
        Nothing

    else
        let
            xt =
                transpose rows

            xtx =
                matMul xt rows

            xty =
                matVec xt y
        in
        case invert xtx of
            Nothing ->
                Nothing

            Just inv ->
                let
                    beta =
                        matVec inv xty

                    fitted =
                        List.map (\r -> dot r beta) rows

                    resid =
                        List.map2 (-) y fitted

                    sse =
                        List.sum (List.map (\e -> e * e) resid)

                    ybar =
                        meanOf y

                    sst =
                        List.sum (List.map (\v -> (v - ybar) ^ 2) y)

                    dfRes =
                        n - cols

                    mse =
                        sse / toFloat dfRes

                    r2 =
                        if sst == 0 then
                            0

                        else
                            1 - sse / sst

                    diag =
                        List.indexedMap (\i r -> Maybe.withDefault 0 (listGet i r)) inv

                    se =
                        List.map (\d -> sqrt (mse * d)) diag
                in
                Just
                    { coefficients = beta
                    , standardErrors = se
                    , tStats = List.map2 (\bv s -> safeDiv bv s) beta se
                    , rSquared = r2
                    , adjustedRSquared = 1 - (1 - r2) * toFloat (n - 1) / toFloat dfRes
                    , standardError = sqrt mse
                    , fStat = (r2 / toFloat kp) / ((1 - r2) / toFloat dfRes)
                    , observations = n
                    }



-- DISTRIBUTION TAILS -------------------------------------------------------


{-| Two-tailed Student-t p-value for statistic `t` on `df` degrees of freedom. -}
studentTtwoTail : Float -> Float -> Float
studentTtwoTail t df =
    if df <= 0 then
        1

    else
        incompleteBeta (df / (df + t * t)) (df / 2) 0.5


{-| Upper-tail probability `P(F > f)` for the F-distribution with `df1`, `df2`. -}
fUpperTail : Float -> Float -> Float -> Float
fUpperTail f df1 df2 =
    if f <= 0 then
        1

    else
        incompleteBeta (df2 / (df2 + df1 * f)) (df2 / 2) (df1 / 2)


{-| Two-tailed standard-normal p-value for a z-score. -}
normalTwoTail : Float -> Float
normalTwoTail z =
    2 * (1 - normalCdf (abs z))



-- NUMERICS CORE ------------------------------------------------------------


normalCdf : Float -> Float
normalCdf z =
    0.5 * (1 + erf (z / sqrt 2))


erf : Float -> Float
erf x =
    let
        t =
            1 / (1 + 0.3275911 * abs x)

        y =
            1 - (((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t) * e ^ (-(x * x))
    in
    if x < 0 then
        -y

    else
        y


{-| Log-gamma via the Lanczos approximation. Arguments here are always positive (halves of
sample sizes), so the reflection formula isn't needed. -}
lgamma : Float -> Float
lgamma x =
    let
        g =
            7

        coeffs =
            [ 0.99999999999980993
            , 676.5203681218851
            , -1259.1392167224028
            , 771.32342877765313
            , -176.61502916214059
            , 12.507343278686905
            , -0.13857109526572012
            , 9.9843695780195716e-6
            , 1.5056327351493116e-7
            ]

        xm1 =
            x - 1

        a =
            List.foldl (\( i, c ) acc -> acc + c / (xm1 + toFloat i))
                (Maybe.withDefault 0 (List.head coeffs))
                (List.indexedMap (\i c -> ( i + 1, c )) (List.drop 1 coeffs))

        tt =
            xm1 + g + 0.5
    in
    0.5 * logBase e (2 * pi) + (xm1 + 0.5) * logBase e tt - tt + logBase e a


{-| Regularized incomplete beta `Iₓ(a, b)` (Numerical Recipes `betai`). -}
incompleteBeta : Float -> Float -> Float -> Float
incompleteBeta x a b =
    if x <= 0 then
        0

    else if x >= 1 then
        1

    else
        let
            bt =
                e ^ (lgamma (a + b) - lgamma a - lgamma b + a * logBase e x + b * logBase e (1 - x))
        in
        if x < (a + 1) / (a + b + 2) then
            bt * betacf x a b / a

        else
            1 - bt * betacf (1 - x) b a / b


betacf : Float -> Float -> Float -> Float
betacf x a b =
    let
        qab =
            a + b

        qap =
            a + 1

        qam =
            a - 1

        d0 =
            1 / fpguard (1 - qab * x / qap)

        stepM m st =
            let
                m2 =
                    toFloat (2 * m)

                mf =
                    toFloat m

                aa1 =
                    mf * (b - mf) * x / ((qam + m2) * (a + m2))

                d1 =
                    1 / fpguard (1 + aa1 * st.d)

                c1 =
                    fpguard (1 + aa1 / st.c)

                h1 =
                    st.h * d1 * c1

                aa2 =
                    -(a + mf) * (qab + mf) * x / ((a + m2) * (qap + m2))

                d2 =
                    1 / fpguard (1 + aa2 * d1)

                c2 =
                    fpguard (1 + aa2 / c1)
            in
            { c = c2, d = d2, h = h1 * d2 * c2 }
    in
    .h (List.foldl stepM { c = 1, d = d0, h = d0 } (List.range 1 200))


fpguard : Float -> Float
fpguard v =
    if abs v < 1.0e-30 then
        1.0e-30

    else
        v



-- SMALL HELPERS ------------------------------------------------------------


meanOf : List Float -> Float
meanOf xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)


sampleVar : List Float -> Float
sampleVar xs =
    let
        n =
            List.length xs
    in
    if n < 2 then
        0

    else
        let
            m =
                meanOf xs
        in
        List.sum (List.map (\x -> (x - m) ^ 2) xs) / toFloat (n - 1)


safeDiv : Float -> Float -> Float
safeDiv a b =
    if b == 0 then
        0

    else
        a / b


listGet : Int -> List a -> Maybe a
listGet i xs =
    List.head (List.drop i xs)



-- FLOAT MATRIX (for regression) --------------------------------------------


transpose : List (List Float) -> List (List Float)
transpose m =
    case m of
        [] ->
            []

        [] :: _ ->
            []

        _ ->
            List.map (\r -> Maybe.withDefault 0 (List.head r)) m
                :: transpose (List.map (List.drop 1) m)


dot : List Float -> List Float -> Float
dot a b =
    List.sum (List.map2 (*) a b)


matMul : List (List Float) -> List (List Float) -> List (List Float)
matMul a b =
    let
        bt =
            transpose b
    in
    List.map (\row -> List.map (\col -> dot row col) bt) a


matVec : List (List Float) -> List Float -> List Float
matVec a v =
    List.map (\row -> dot row v) a


{-| Invert a square matrix by Gauss–Jordan elimination with partial pivoting. `Nothing` if
singular. -}
invert : List (List Float) -> Maybe (List (List Float))
invert m =
    let
        n =
            List.length m

        augmented =
            List.indexedMap
                (\i row -> row ++ List.map (\j -> boolToFloat (i == j)) (List.range 0 (n - 1)))
                m
    in
    gaussJordan n 0 augmented
        |> Maybe.map (List.map (List.drop n))


gaussJordan : Int -> Int -> List (List Float) -> Maybe (List (List Float))
gaussJordan n col rows =
    if col >= n then
        Just rows

    else
        case pivotIndex col col rows of
            Nothing ->
                Nothing

            Just pv ->
                let
                    swapped =
                        swap col pv rows

                    pivotRow =
                        Maybe.withDefault [] (listGet col swapped)

                    pivotVal =
                        Maybe.withDefault 1 (listGet col pivotRow)

                    normRow =
                        List.map (\x -> x / pivotVal) pivotRow

                    eliminated =
                        List.indexedMap
                            (\i row ->
                                if i == col then
                                    normRow

                                else
                                    let
                                        factor =
                                            Maybe.withDefault 0 (listGet col row)
                                    in
                                    List.map2 (\r p -> r - factor * p) row normRow
                            )
                            swapped
                in
                gaussJordan n (col + 1) eliminated


pivotIndex : Int -> Int -> List (List Float) -> Maybe Int
pivotIndex startRow col rows =
    let
        candidates =
            List.indexedMap Tuple.pair rows
                |> List.filter (\( i, _ ) -> i >= startRow)
                |> List.filter (\( _, r ) -> abs (Maybe.withDefault 0 (listGet col r)) > 1.0e-12)
                |> List.sortBy (\( _, r ) -> negate (abs (Maybe.withDefault 0 (listGet col r))))
    in
    case candidates of
        ( i, _ ) :: _ ->
            Just i

        [] ->
            Nothing


swap : Int -> Int -> List a -> List a
swap i j xs =
    case ( listGet i xs, listGet j xs ) of
        ( Just vi, Just vj ) ->
            List.indexedMap
                (\k x ->
                    if k == i then
                        vj

                    else if k == j then
                        vi

                    else
                        x
                )
                xs

        _ ->
            xs


boolToFloat : Bool -> Float
boolToFloat b =
    if b then
        1

    else
        0
