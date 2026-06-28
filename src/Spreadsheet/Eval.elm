module Spreadsheet.Eval exposing
    ( Context
    , Lambda
    , noLocals
    , noSpill
    , noLambda
    , noTableRange
    , noTableTotals
    , noFormulaText
    , parseLambda
    , eval
    , evalString
    , evalMatrix
    )

{-| Evaluate a parsed formula against a sheet.

`Context` is the only thing the evaluator knows about the sheet: how to read another
cell's already-computed value (`lookup`) and which cell it is computing (`self`, for
`ROW()`/`COLUMN()`). Keeping the dependency that narrow is what lets the recalculator
(`Spreadsheet.Recalc`) evaluate cells in topological order and feed each one a `lookup`
that only ever returns finished values.

Operators and the strict function table do the bulk of the work; a handful of forms that
must be lazy (IF, IFERROR, IFS, SWITCH, CHOOSE) or that need the *reference* rather than
its value (ROW, COLUMN) are intercepted here before arguments are evaluated.

@docs Context, eval, evalString

-}

import Dict exposing (Dict)
import Spreadsheet.Ast exposing (BinaryOp(..), Expr(..), UnaryOp(..))
import Spreadsheet.Format as Format
import Spreadsheet.Functions as Functions exposing (Arg(..))
import Spreadsheet.Parser as Parser
import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Spill as Spill
import Spreadsheet.Value as Value exposing (Error(..), Value(..))


{-| What the evaluator needs from the sheet: read another cell's value (`lookup`), know
which cell is being computed (`self`), resolve a defined name to its range (`names`), read
a cell on another sheet of the workbook (`external sheetName ref`), look up a `LET`-bound
local (`locals`), and resolve a spill anchor to the block that spilled from it (`spill`,
for the `A1#` operator). -}
type alias Context =
    { lookup : Ref -> Value
    , self : Ref
    , names : String -> Maybe Range
    , external : String -> Ref -> Value
    , locals : String -> Maybe Arg
    , spill : Ref -> Maybe Range
    , lambda : String -> Maybe Lambda
    , tableRange : String -> Maybe Range
    , tableTotals : String -> Bool
    , formulaText : Ref -> Maybe String
    }


{-| A parsed `LAMBDA(param1, …, body)`: the parameter names and the body expression. Used
by the higher-order helpers (`MAP`/`REDUCE`/…) and by named lambdas (`Sheet.defineLambda`)
to evaluate a body with arguments bound to the parameters. -}
type alias Lambda =
    { params : List String
    , body : Expr
    }


{-| A `locals` resolver with no bindings — the default outside a `LET` or lambda. -}
noLocals : String -> Maybe Arg
noLocals _ =
    Nothing


{-| A `spill` resolver that knows of no spilled blocks — the default when a sheet has no
dynamic arrays (or when evaluating outside one). -}
noSpill : Ref -> Maybe Range
noSpill _ =
    Nothing


{-| A `lambda` resolver with no named lambdas. -}
noLambda : String -> Maybe Lambda
noLambda _ =
    Nothing


{-| A `tableRange` resolver that knows of no tables. -}
noTableRange : String -> Maybe Range
noTableRange _ =
    Nothing


{-| A `tableTotals` resolver: no table has a totals row. -}
noTableTotals : String -> Bool
noTableTotals _ =
    False


{-| A `formulaText` resolver: no cell is known to be a formula. -}
noFormulaText : Ref -> Maybe String
noFormulaText _ =
    Nothing


{-| Parse a `LAMBDA(...)` formula string into a `Lambda` (the leading `=` is optional). -}
parseLambda : String -> Maybe Lambda
parseLambda src =
    case Parser.parseFormula src of
        Ok expr ->
            asLambda expr

        Err _ ->
            Nothing


{-| Parse and evaluate a formula string (the leading `=` is optional). A parse failure
becomes `#ERROR!`. -}
evalString : Context -> String -> Value
evalString ctx src =
    case Parser.parseFormula src of
        Ok expr ->
            eval ctx expr

        Err _ ->
            VError Parse


{-| Evaluate an already-parsed expression. -}
eval : Context -> Expr -> Value
eval ctx expr =
    case expr of
        Lit v ->
            v

        RefE ref _ ->
            ctx.lookup ref

        RangeE range _ _ ->
            scalarOfRange ctx range

        NameE name ->
            case ctx.locals name of
                Just arg ->
                    scalarOfArg arg

                Nothing ->
                    case ctx.names name of
                        Just range ->
                            if range.start == range.end then
                                ctx.lookup range.start

                            else
                                scalarOfRange ctx range

                        Nothing ->
                            VError NameErr

        StructRefE tableName selector ->
            case structScalar ctx tableName selector of
                Just v ->
                    v

                Nothing ->
                    VError RefErr

        SpillRefE anchor ->
            case ctx.spill anchor of
                Just range ->
                    scalarOfRange ctx range

                Nothing ->
                    VError RefErr

        SheetRefE sheetName ref _ ->
            ctx.external sheetName ref

        SheetRangeE sheetName range _ _ ->
            case Ref.cellsOf range of
                first :: _ ->
                    ctx.external sheetName first

                [] ->
                    VError RefErr

        Unary op sub ->
            applyUnary op (eval ctx sub)

        Binary op a b ->
            applyBinary op (eval ctx a) (eval ctx b)

        Func name args ->
            if isRefForm name then
                case refFormRange ctx name args of
                    Just range ->
                        scalarOfRange ctx range

                    Nothing ->
                        VError RefErr

            else if name == "LET" then
                evalLet ctx args

            else if name == "REDUCE" then
                evalReduce ctx args

            else if isArrayForm name then
                -- Used where a scalar is expected, an array result collapses to its
                -- top-left cell (the implicit-intersection rule).
                case arrayResult ctx name args of
                    Just matrix ->
                        topLeftOf matrix

                    Nothing ->
                        VError ValueErr

            else
                evalFunc ctx name args


{-| `OFFSET`/`INDIRECT` produce *references* at evaluation time rather than plain values,
so they are resolved here (and in `evalArg`) to a range the surrounding formula reads. -}
isRefForm : String -> Bool
isRefForm name =
    name == "OFFSET" || name == "INDIRECT"


refFormRange : Context -> String -> List Expr -> Maybe Range
refFormRange ctx name args =
    case name of
        "OFFSET" ->
            offsetRange ctx args

        "INDIRECT" ->
            indirectRange ctx args

        _ ->
            Nothing


offsetRange : Context -> List Expr -> Maybe Range
offsetRange ctx args =
    case args of
        baseE :: rest ->
            case baseRefOf baseE of
                Just base ->
                    let
                        n =
                            Ref.normalize base

                        start =
                            { col = n.start.col + intArg ctx 1 0 rest
                            , row = n.start.row + intArg ctx 0 0 rest
                            }

                        height =
                            intArg ctx 2 (Ref.height n) rest

                        width =
                            intArg ctx 3 (Ref.width n) rest
                    in
                    if start.col < 0 || start.row < 0 then
                        Nothing

                    else
                        Just { start = start, end = { col = start.col + width - 1, row = start.row + height - 1 } }

                Nothing ->
                    Nothing

        _ ->
            Nothing


baseRefOf : Expr -> Maybe Range
baseRefOf expr =
    case expr of
        RefE ref _ ->
            Just { start = ref, end = ref }

        RangeE range _ _ ->
            Just range

        _ ->
            Nothing


intArg : Context -> Int -> Int -> List Expr -> Int
intArg ctx i default args =
    case List.head (List.drop i args) of
        Just e ->
            case Value.toNumber (eval ctx e) of
                Ok x ->
                    round x

                Err _ ->
                    default

        Nothing ->
            default


indirectRange : Context -> List Expr -> Maybe Range
indirectRange ctx args =
    case args of
        textE :: _ ->
            Ref.rangeFromA1 (Value.toText (eval ctx textE))

        _ ->
            Nothing



-- DYNAMIC ARRAYS -------------------------------------------------------------
-- The "spilling" functions return a 2-D block rather than a scalar. `evalMatrix` is the
-- entry point the sheet uses to decide whether a formula spills (and into what); in a
-- scalar position the block collapses to its top-left cell, and in an argument position
-- it becomes a `Matrix` the aggregates already understand.


{-| If an expression produces a dynamic-array block — a spilling function (`SORT`,
`FILTER`, `SEQUENCE`, `HSTACK`, `LINEST`, …) or a spill reference (`A1#`) — return that
block. The sheet calls this on each formula cell to materialise spills. Plain scalars,
ranges and aggregates return `Nothing` (they don't spill). -}
evalMatrix : Context -> Expr -> Maybe (List (List Value))
evalMatrix ctx expr =
    case expr of
        SpillRefE anchor ->
            Maybe.map (matrixViaLookup ctx) (ctx.spill anchor)

        StructRefE tableName selector ->
            Maybe.map (matrixViaLookup ctx) (structRange ctx tableName selector)

        Func name args ->
            if isArrayForm name then
                arrayResult ctx name args

            else
                Nothing

        Binary op a b ->
            broadcastBinary ctx op a b

        Unary op sub ->
            Maybe.map (List.map (List.map (applyUnary op))) (evalMatrix ctx sub)

        _ ->
            Nothing


{-| A view of an operand as a 2-D block, for elementwise (broadcasting) operators. Unlike
`evalMatrix`, a bare range or a range-valued name counts as an array here. -}
arrayOperand : Context -> Expr -> Maybe (List (List Value))
arrayOperand ctx e =
    case e of
        RangeE range _ _ ->
            Just (matrixViaLookup ctx range)

        NameE name ->
            case ctx.locals name of
                Just (Matrix m) ->
                    Just m

                Just _ ->
                    Nothing

                Nothing ->
                    Maybe.map (matrixViaLookup ctx) (ctx.names name)

        _ ->
            evalMatrix ctx e


{-| Elementwise (broadcasting) binary operator over array operands. Returns `Nothing` when
*neither* side is an array (so the scalar evaluator handles it instead). A scalar (or 1×1)
operand broadcasts across the other; a row or column vector broadcasts along its singleton
axis; out-of-shape cells become `#N/A`. -}
broadcastBinary : Context -> BinaryOp -> Expr -> Expr -> Maybe (List (List Value))
broadcastBinary ctx op a b =
    case ( arrayOperand ctx a, arrayOperand ctx b ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( ma, mb ) ->
            let
                am =
                    Maybe.withDefault [ [ eval ctx a ] ] ma

                bm =
                    Maybe.withDefault [ [ eval ctx b ] ] mb

                rows =
                    max (List.length am) (List.length bm)

                cols =
                    max (matWidth am) (matWidth bm)
            in
            Just
                (List.map
                    (\i -> List.map (\j -> applyBinary op (broadcastAt am i j) (broadcastAt bm i j)) (List.range 0 (cols - 1)))
                    (List.range 0 (rows - 1))
                )


{-| Read cell `(i, j)` of a matrix with broadcasting: a single-row matrix repeats its row,
a single-column matrix repeats its column; anything out of range is `#N/A`. -}
broadcastAt : List (List Value) -> Int -> Int -> Value
broadcastAt m i j =
    let
        r =
            if List.length m == 1 then
                0

            else
                i

        c =
            if matWidth m == 1 then
                0

            else
                j
    in
    case List.head (List.drop r m) of
        Just row ->
            Maybe.withDefault (VError NA) (List.head (List.drop c row))

        Nothing ->
            VError NA


matrixViaLookup : Context -> Range -> List (List Value)
matrixViaLookup ctx range =
    List.map (List.map ctx.lookup) (Ref.rowsOf range)


{-| The names of the spilling array functions. -}
isArrayForm : String -> Bool
isArrayForm name =
    List.member name
        [ "UNIQUE", "SORT", "SORTBY", "FILTER", "SEQUENCE", "TRANSPOSE"
        , "HSTACK", "VSTACK", "CHOOSEROWS", "CHOOSECOLS", "TAKE", "DROP", "TOROW", "TOCOL"
        , "LINEST", "TREND", "GROWTH"
        , "MAP", "SCAN", "MAKEARRAY", "BYROW", "BYCOL"
        , "TEXTSPLIT", "WRAPROWS", "WRAPCOLS", "EXPAND"
        ]


{-| Evaluate a spilling array function to its 2-D result. -}
arrayResult : Context -> String -> List Expr -> Maybe (List (List Value))
arrayResult ctx name args =
    case name of
        "UNIQUE" ->
            Maybe.map Spill.unique (argMatrixAt ctx 0 args)

        "TRANSPOSE" ->
            Maybe.map Spill.transpose (argMatrixAt ctx 0 args)

        "SORT" ->
            Maybe.map
                (\m -> Spill.sortBy (intArg ctx 1 1 args - 1) (floatArg ctx 2 1 args >= 0) m)
                (argMatrixAt ctx 0 args)

        "SORTBY" ->
            sortByResult ctx args

        "FILTER" ->
            filterResult ctx args

        "SEQUENCE" ->
            Just
                (Spill.sequence
                    (max 0 (intArg ctx 0 1 args))
                    (max 1 (intArg ctx 1 1 args))
                    (floatArg ctx 2 1 args)
                    (floatArg ctx 3 1 args)
                )

        "HSTACK" ->
            Just (hstack (List.map (argMatrix ctx) args))

        "VSTACK" ->
            Just (vstack (List.map (argMatrix ctx) args))

        "CHOOSEROWS" ->
            chooseLines ctx args True

        "CHOOSECOLS" ->
            chooseLines ctx args False

        "TAKE" ->
            Just (takeDrop ctx args True)

        "DROP" ->
            Just (takeDrop ctx args False)

        "TOROW" ->
            Maybe.map (\m -> [ List.concat m ]) (argMatrixAt ctx 0 args)

        "TOCOL" ->
            Maybe.map (\m -> List.map (\v -> [ v ]) (List.concat m)) (argMatrixAt ctx 0 args)

        "LINEST" ->
            linestResult ctx args

        "TREND" ->
            trendResult ctx args False

        "GROWTH" ->
            trendResult ctx args True

        "MAP" ->
            mapResult ctx args

        "SCAN" ->
            scanResult ctx args

        "MAKEARRAY" ->
            makeArrayResult ctx args

        "BYROW" ->
            byLineResult ctx args True

        "BYCOL" ->
            byLineResult ctx args False

        "TEXTSPLIT" ->
            textSplitResult ctx args

        "WRAPROWS" ->
            Maybe.map (\m -> wrap (intArg ctx 1 1 args) (padOf ctx args) True (List.concat m)) (argMatrixAt ctx 0 args)

        "WRAPCOLS" ->
            Maybe.map (\m -> wrap (intArg ctx 1 1 args) (padOf ctx args) False (List.concat m)) (argMatrixAt ctx 0 args)

        "EXPAND" ->
            expandResult ctx args

        _ ->
            Nothing


argMatrix : Context -> Expr -> List (List Value)
argMatrix ctx e =
    Functions.matrixOf (evalArg ctx e)


argMatrixAt : Context -> Int -> List Expr -> Maybe (List (List Value))
argMatrixAt ctx i args =
    Maybe.map (argMatrix ctx) (List.head (List.drop i args))


floatArg : Context -> Int -> Float -> List Expr -> Float
floatArg ctx i default args =
    case List.head (List.drop i args) of
        Just e ->
            case Value.toNumber (eval ctx e) of
                Ok x ->
                    x

                Err _ ->
                    default

        Nothing ->
            default


intArgMaybe : Context -> Int -> List Expr -> Maybe Int
intArgMaybe ctx i args =
    List.head (List.drop i args)
        |> Maybe.andThen (\e -> Result.toMaybe (Value.toNumber (eval ctx e)))
        |> Maybe.map round


topLeftOf : List (List Value) -> Value
topLeftOf matrix =
    case matrix of
        row :: _ ->
            case row of
                v :: _ ->
                    v

                [] ->
                    VError NA

        [] ->
            VError NA


sortByResult : Context -> List Expr -> Maybe (List (List Value))
sortByResult ctx args =
    case ( argMatrixAt ctx 0 args, argMatrixAt ctx 1 args ) of
        ( Just rows, Just byM ) ->
            let
                asc =
                    floatArg ctx 2 1 args >= 0

                keys =
                    List.map (\r -> Maybe.withDefault VEmpty (List.head r)) byM

                paired =
                    List.map2 (\r k -> ( k, r )) rows keys
            in
            Just
                (List.map Tuple.second
                    (List.sortWith
                        (\( ka, _ ) ( kb, _ ) ->
                            let
                                o =
                                    Value.compare ka kb
                            in
                            if asc then
                                o

                            else
                                flipOrder o
                        )
                        paired
                    )
                )

        _ ->
            Nothing


filterResult : Context -> List Expr -> Maybe (List (List Value))
filterResult ctx args =
    case ( argMatrixAt ctx 0 args, argMatrixAt ctx 1 args ) of
        ( Just rows, Just maskM ) ->
            let
                mask =
                    List.map (\r -> truthy (Maybe.withDefault VEmpty (List.head r))) maskM

                kept =
                    List.map2 (\r keep -> ( keep, r )) rows mask
                        |> List.filter Tuple.first
                        |> List.map Tuple.second
            in
            if List.isEmpty kept then
                case List.head (List.drop 2 args) of
                    Just e ->
                        Just [ [ eval ctx e ] ]

                    Nothing ->
                        Just [ [ VError NA ] ]

            else
                Just kept

        _ ->
            Nothing


truthy : Value -> Bool
truthy v =
    case v of
        VBool b ->
            b

        VNumber n ->
            n /= 0

        _ ->
            case Value.toBool v of
                Ok b ->
                    b

                Err _ ->
                    False


hstack : List (List (List Value)) -> List (List Value)
hstack mats =
    let
        h =
            Maybe.withDefault 0 (List.maximum (List.map List.length mats))
    in
    List.map (\i -> List.concatMap (rowAt i) mats) (List.range 0 (h - 1))


rowAt : Int -> List (List Value) -> List Value
rowAt i m =
    case List.head (List.drop i m) of
        Just row ->
            row

        Nothing ->
            List.repeat (matWidth m) VEmpty


matWidth : List (List Value) -> Int
matWidth m =
    Maybe.withDefault 0 (List.maximum (List.map List.length m))


vstack : List (List (List Value)) -> List (List Value)
vstack mats =
    let
        w =
            Maybe.withDefault 0 (List.maximum (List.map matWidth mats))
    in
    List.map (\row -> row ++ List.repeat (max 0 (w - List.length row)) VEmpty) (List.concat mats)


chooseLines : Context -> List Expr -> Bool -> Maybe (List (List Value))
chooseLines ctx args isRows =
    case args of
        first :: idxExprs ->
            let
                m =
                    if isRows then
                        argMatrix ctx first

                    else
                        Spill.transpose (argMatrix ctx first)

                len =
                    List.length m

                idxs =
                    List.map (\e -> round (Result.withDefault 0 (Value.toNumber (eval ctx e)))) idxExprs

                picked =
                    List.filterMap (\i -> List.head (List.drop (resolveIndex i len) m)) idxs
            in
            Just
                (if isRows then
                    picked

                 else
                    Spill.transpose picked
                )

        _ ->
            Nothing


{-| Turn a 1-based index (negative counts from the end) into a 0-based offset. -}
resolveIndex : Int -> Int -> Int
resolveIndex i len =
    if i < 0 then
        len + i

    else
        i - 1


takeDrop : Context -> List Expr -> Bool -> List (List Value)
takeDrop ctx args isTake =
    case args of
        first :: _ ->
            let
                m =
                    argMatrix ctx first

                afterRows =
                    case intArgMaybe ctx 1 args of
                        Just n ->
                            sliceLines isTake n m

                        Nothing ->
                            m
            in
            case intArgMaybe ctx 2 args of
                Just n ->
                    Spill.transpose (sliceLines isTake n (Spill.transpose afterRows))

                Nothing ->
                    afterRows

        [] ->
            []


sliceLines : Bool -> Int -> List a -> List a
sliceLines isTake n lines =
    let
        len =
            List.length lines

        an =
            abs n
    in
    if isTake then
        if n >= 0 then
            List.take an lines

        else
            List.drop (max 0 (len - an)) lines

    else if n >= 0 then
        List.drop an lines

    else
        List.take (max 0 (len - an)) lines


numbersOfArg : Context -> Int -> List Expr -> Maybe (List Float)
numbersOfArg ctx i args =
    Maybe.map (\m -> List.filterMap numOf (List.concat m)) (argMatrixAt ctx i args)


numOf : Value -> Maybe Float
numOf v =
    case v of
        VNumber x ->
            Just x

        _ ->
            Nothing


linestResult : Context -> List Expr -> Maybe (List (List Value))
linestResult ctx args =
    case ( numbersOfArg ctx 0 args, numbersOfArg ctx 1 args ) of
        ( Just ys, Just xs ) ->
            case linreg ys xs of
                Just ( slope, intercept ) ->
                    Just [ [ VNumber slope, VNumber intercept ] ]

                Nothing ->
                    Just [ [ VError DivZero, VError DivZero ] ]

        _ ->
            Nothing


trendResult : Context -> List Expr -> Bool -> Maybe (List (List Value))
trendResult ctx args isGrowth =
    case numbersOfArg ctx 0 args of
        Just ysRaw ->
            let
                ys =
                    if isGrowth then
                        List.map (logBase e) ysRaw

                    else
                        ysRaw

                xs =
                    case numbersOfArg ctx 1 args of
                        Just xv ->
                            xv

                        Nothing ->
                            List.map toFloat (List.range 1 (List.length ysRaw))

                newxs =
                    case numbersOfArg ctx 2 args of
                        Just nv ->
                            nv

                        Nothing ->
                            xs
            in
            case linreg ys xs of
                Just ( slope, intercept ) ->
                    Just
                        (List.map
                            (\x ->
                                let
                                    yp =
                                        intercept + slope * x
                                in
                                [ VNumber
                                    (if isGrowth then
                                        e ^ yp

                                     else
                                        yp
                                    )
                                ]
                            )
                            newxs
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| Ordinary-least-squares fit of `ys` on `xs`: `Just (slope, intercept)`, or `Nothing`
when there are fewer than two points or the x's have no spread. -}
linreg : List Float -> List Float -> Maybe ( Float, Float )
linreg ys xs =
    let
        pairs =
            List.map2 (\y x -> ( y, x )) ys xs

        n =
            toFloat (List.length pairs)
    in
    if List.length pairs < 2 then
        Nothing

    else
        let
            mx =
                List.sum (List.map Tuple.second pairs) / n

            my =
                List.sum (List.map Tuple.first pairs) / n

            sxx =
                List.sum (List.map (\( _, x ) -> (x - mx) ^ 2) pairs)

            sxy =
                List.sum (List.map (\( y, x ) -> (x - mx) * (y - my)) pairs)
        in
        if sxx == 0 then
            Nothing

        else
            Just ( sxy / sxx, my - sxy / sxx * mx )


flipOrder : Order -> Order
flipOrder o =
    case o of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT



-- LAMBDA & HIGHER-ORDER FUNCTIONS --------------------------------------------


{-| Recognise `LAMBDA(param1, …, body)` and split it into its parameter names and body. -}
asLambda : Expr -> Maybe Lambda
asLambda expr =
    case expr of
        Func "LAMBDA" args ->
            case List.reverse args of
                body :: revParams ->
                    let
                        params =
                            List.filterMap nameOfExpr revParams
                    in
                    if List.length params == List.length revParams then
                        Just { params = List.reverse params, body = body }

                    else
                        Nothing

                [] ->
                    Nothing

        _ ->
            Nothing


nameOfExpr : Expr -> Maybe String
nameOfExpr e =
    case e of
        NameE n ->
            Just n

        _ ->
            Nothing


{-| Evaluate a lambda body with its parameters bound to the given arguments. -}
applyLambda : Context -> Lambda -> List Arg -> Value
applyLambda ctx lam argv =
    evalWithEnv ctx (Dict.fromList (List.map2 (\p a -> ( p, a )) lam.params argv)) lam.body


{-| `MAP(array1, …, lambda)`: apply the lambda elementwise across one or more equal-shaped
arrays, producing a result of the first array's shape. -}
mapResult : Context -> List Expr -> Maybe (List (List Value))
mapResult ctx args =
    case List.reverse args of
        lambdaE :: revArrays ->
            case asLambda lambdaE of
                Just lam ->
                    let
                        arrays =
                            List.map (argMatrix ctx) (List.reverse revArrays)
                    in
                    case arrays of
                        first :: _ ->
                            Just
                                (List.indexedMap
                                    (\i row ->
                                        List.indexedMap
                                            (\j _ -> applyLambda ctx lam (List.map (\m -> Scalar (broadcastAt m i j)) arrays))
                                            row
                                    )
                                    first
                                )

                        [] ->
                            Nothing

                Nothing ->
                    Nothing

        [] ->
            Nothing


{-| `REDUCE(init, array, lambda(acc, value))`: left-fold the array into a single value. -}
evalReduce : Context -> List Expr -> Value
evalReduce ctx args =
    case args of
        initE :: arrayE :: lambdaE :: _ ->
            case asLambda lambdaE of
                Just lam ->
                    List.foldl
                        (\v acc -> applyLambda ctx lam [ Scalar acc, Scalar v ])
                        (eval ctx initE)
                        (List.concat (argMatrix ctx arrayE))

                Nothing ->
                    VError ValueErr

        _ ->
            VError ValueErr


{-| `SCAN(init, array, lambda(acc, value))`: like `REDUCE`, but emit each running
accumulator, spilling a block of the array's shape. -}
scanResult : Context -> List Expr -> Maybe (List (List Value))
scanResult ctx args =
    case args of
        initE :: arrayE :: lambdaE :: _ ->
            case asLambda lambdaE of
                Just lam ->
                    let
                        m =
                            argMatrix ctx arrayE

                        step v ( acc, outs ) =
                            let
                                nv =
                                    applyLambda ctx lam [ Scalar acc, Scalar v ]
                            in
                            ( nv, outs ++ [ nv ] )

                        ( _, flat ) =
                            List.foldl step ( eval ctx initE, [] ) (List.concat m)
                    in
                    Just (reshapeLike m flat)

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| `MAKEARRAY(rows, cols, lambda(r, c))`: build a `rows × cols` block from a lambda of the
(1-based) row and column index. -}
makeArrayResult : Context -> List Expr -> Maybe (List (List Value))
makeArrayResult ctx args =
    case args of
        rowsE :: colsE :: lambdaE :: _ ->
            case asLambda lambdaE of
                Just lam ->
                    let
                        r =
                            max 0 (round (Result.withDefault 0 (Value.toNumber (eval ctx rowsE))))

                        c =
                            max 0 (round (Result.withDefault 0 (Value.toNumber (eval ctx colsE))))
                    in
                    Just
                        (List.map
                            (\i -> List.map (\j -> applyLambda ctx lam [ Scalar (VNumber (toFloat i)), Scalar (VNumber (toFloat j)) ]) (List.range 1 c))
                            (List.range 1 r)
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| `BYROW(array, lambda(row))` / `BYCOL(array, lambda(col))`: apply the lambda to each row
(or column) — passed as a 1-D array — collapsing to a single column (or row) of results. -}
byLineResult : Context -> List Expr -> Bool -> Maybe (List (List Value))
byLineResult ctx args isRow =
    case args of
        arrayE :: lambdaE :: _ ->
            case asLambda lambdaE of
                Just lam ->
                    let
                        m0 =
                            argMatrix ctx arrayE

                        lines =
                            if isRow then
                                m0

                            else
                                Spill.transpose m0

                        results =
                            List.map (\line -> applyLambda ctx lam [ Matrix [ line ] ]) lines
                    in
                    Just
                        (if isRow then
                            List.map (\v -> [ v ]) results

                         else
                            [ results ]
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Re-chunk a flat list into a matrix with the same row sizes as `template`. -}
reshapeLike : List (List a) -> List a -> List (List a)
reshapeLike template flat =
    case template of
        [] ->
            []

        row :: rest ->
            let
                n =
                    List.length row
            in
            List.take n flat :: reshapeLike rest (List.drop n flat)



-- TEXT & RESHAPE ARRAYS ------------------------------------------------------


{-| `TEXTSPLIT(text, colDelim, [rowDelim])`: split text into a row, or (with a row
delimiter) a 2-D block. -}
textSplitResult : Context -> List Expr -> Maybe (List (List Value))
textSplitResult ctx args =
    case args of
        textE :: colDelimE :: rest ->
            let
                s =
                    Value.toText (eval ctx textE)

                colD =
                    Value.toText (eval ctx colDelimE)

                rowD =
                    case rest of
                        dE :: _ ->
                            Value.toText (eval ctx dE)

                        [] ->
                            ""
            in
            Just
                (if rowD == "" then
                    [ List.map VText (splitBy colD s) ]

                 else
                    List.map (\line -> List.map VText (splitBy colD line)) (splitBy rowD s)
                )

        _ ->
            Nothing


splitBy : String -> String -> List String
splitBy d s =
    if d == "" then
        [ s ]

    else
        String.split d s


padOf : Context -> List Expr -> Value
padOf ctx args =
    case List.head (List.drop 2 args) of
        Just e ->
            eval ctx e

        Nothing ->
            VError NA


{-| `WRAPROWS`/`WRAPCOLS`: fold a 1-D vector into a block `count` wide (rows) or tall
(cols), padding the short final line. -}
wrap : Int -> Value -> Bool -> List Value -> List (List Value)
wrap count pad isRows flat =
    let
        rows =
            List.map (\c -> c ++ List.repeat (max 0 (count - List.length c)) pad) (chunk count flat)
    in
    if isRows then
        rows

    else
        Spill.transpose rows


chunk : Int -> List a -> List (List a)
chunk n xs =
    if n <= 0 then
        [ xs ]

    else
        case xs of
            [] ->
                []

            _ ->
                List.take n xs :: chunk n (List.drop n xs)


{-| `EXPAND(array, rows, [cols], [pad])`: grow a block to the given size, padding new
cells. -}
expandResult : Context -> List Expr -> Maybe (List (List Value))
expandResult ctx args =
    case argMatrixAt ctx 0 args of
        Just m ->
            let
                r =
                    Maybe.withDefault (List.length m) (intArgMaybe ctx 1 args)

                c =
                    Maybe.withDefault (matWidth m) (intArgMaybe ctx 2 args)

                pad =
                    case List.head (List.drop 3 args) of
                        Just e ->
                            eval ctx e

                        Nothing ->
                            VError NA
            in
            Just
                (List.map
                    (\i -> List.map (\j -> elemOr pad m i j) (List.range 0 (c - 1)))
                    (List.range 0 (r - 1))
                )

        Nothing ->
            Nothing


elemOr : Value -> List (List Value) -> Int -> Int -> Value
elemOr pad m i j =
    case List.head (List.drop i m) of
        Just row ->
            Maybe.withDefault pad (List.head (List.drop j row))

        Nothing ->
            pad



-- STRUCTURED TABLE REFERENCES ------------------------------------------------


{-| The range a structured reference resolves to (a column, the headers, the data body,
the totals row or the whole table), or `Nothing` for a `@`-this-row reference (which is a
single cell, handled in scalar position). -}
structRange : Context -> String -> String -> Maybe Range
structRange ctx tableName selector =
    case ctx.tableRange tableName of
        Just range ->
            let
                n =
                    Ref.normalize range

                hasTotals =
                    ctx.tableTotals tableName

                headerRow =
                    n.start.row

                dataStart =
                    headerRow + 1

                dataEnd =
                    if hasTotals then
                        n.end.row - 1

                    else
                        n.end.row
            in
            case String.toUpper selector of
                "#ALL" ->
                    Just n

                "#HEADERS" ->
                    Just { start = n.start, end = { col = n.end.col, row = headerRow } }

                "#DATA" ->
                    dataBodyRange n dataStart dataEnd

                "#TOTALS" ->
                    if hasTotals then
                        Just { start = { col = n.start.col, row = n.end.row }, end = n.end }

                    else
                        Nothing

                up ->
                    if String.startsWith "@" up then
                        Nothing

                    else
                        columnRange ctx n dataStart dataEnd selector

        Nothing ->
            Nothing


dataBodyRange : Range -> Int -> Int -> Maybe Range
dataBodyRange n dataStart dataEnd =
    if dataEnd >= dataStart then
        Just { start = { col = n.start.col, row = dataStart }, end = { col = n.end.col, row = dataEnd } }

    else
        Nothing


{-| The data range of the column whose header matches `colName` (case-insensitive). -}
columnRange : Context -> Range -> Int -> Int -> String -> Maybe Range
columnRange ctx n dataStart dataEnd colName =
    case columnIndex ctx n colName of
        Just c ->
            if dataEnd >= dataStart then
                Just { start = { col = c, row = dataStart }, end = { col = c, row = dataEnd } }

            else
                Nothing

        Nothing ->
            Nothing


columnIndex : Context -> Range -> String -> Maybe Int
columnIndex ctx n colName =
    List.head
        (List.filter
            (\c -> sameText (Value.toText (ctx.lookup { col = c, row = n.start.row })) colName)
            (List.range n.start.col n.end.col)
        )


sameText : String -> String -> Bool
sameText a b =
    String.toUpper (String.trim a) == String.toUpper (String.trim b)


{-| A structured reference in scalar position: a `@`-reference reads the current row's cell
in that column; anything else collapses its range to the top-left cell. -}
structScalar : Context -> String -> String -> Maybe Value
structScalar ctx tableName selector =
    if String.startsWith "@" selector then
        thisRowCell ctx tableName (String.dropLeft 1 selector)

    else
        Maybe.map (\r -> ctx.lookup (Ref.normalize r).start) (structRange ctx tableName selector)


structScalarOnly : Context -> String -> String -> Value
structScalarOnly ctx tableName selector =
    Maybe.withDefault (VError RefErr) (structScalar ctx tableName selector)


thisRowCell : Context -> String -> String -> Maybe Value
thisRowCell ctx tableName colName =
    case ctx.tableRange tableName of
        Just range ->
            let
                n =
                    Ref.normalize range
            in
            case columnIndex ctx n colName of
                Just c ->
                    Just (ctx.lookup { col = c, row = ctx.self.row })

                Nothing ->
                    Just (VError RefErr)

        Nothing ->
            Nothing



-- LET ------------------------------------------------------------------------


{-| `LET(name1, value1, …, calc)`: bind each `name` to its evaluated `value` (later
bindings can use earlier ones), then evaluate the final `calc` with those names in scope. -}
evalLet : Context -> List Expr -> Value
evalLet ctx args =
    letBind ctx Dict.empty args


letBind : Context -> Dict String Arg -> List Expr -> Value
letBind ctx env args =
    case args of
        [ finalE ] ->
            evalWithEnv ctx env finalE

        (NameE name) :: valueE :: rest ->
            letBind ctx (Dict.insert name (evalArgWithEnv ctx env valueE) env) rest

        _ ->
            VError ValueErr


evalWithEnv : Context -> Dict String Arg -> Expr -> Value
evalWithEnv ctx env e =
    eval (withLocals env ctx) e


evalArgWithEnv : Context -> Dict String Arg -> Expr -> Arg
evalArgWithEnv ctx env e =
    evalArg (withLocals env ctx) e


{-| Extend a context's `locals` with an environment of bindings (used by `LET` and the
lambda helpers). -}
withLocals : Dict String Arg -> Context -> Context
withLocals env ctx =
    { ctx | locals = extendLocals env ctx }


extendLocals : Dict String Arg -> Context -> String -> Maybe Arg
extendLocals env ctx n =
    case Dict.get n env of
        Just v ->
            Just v

        Nothing ->
            ctx.locals n


{-| The scalar view of an argument: a scalar passes through; a matrix collapses to its
top-left cell (implicit intersection). -}
scalarOfArg : Arg -> Value
scalarOfArg arg =
    case arg of
        Scalar v ->
            v

        Matrix rows ->
            topLeftOf rows


{-| A range used where a scalar is expected collapses to its top-left cell. -}
scalarOfRange : Context -> Range -> Value
scalarOfRange ctx range =
    case Ref.cellsOf range of
        first :: _ ->
            ctx.lookup first

        [] ->
            VError RefErr


evalFunc : Context -> String -> List Expr -> Value
evalFunc ctx name args =
    case name of
        "#NAME" ->
            VError NameErr

        "IF" ->
            evalIf ctx args

        "IFERROR" ->
            evalIfError isAnyError ctx args

        "IFNA" ->
            evalIfError (\v -> v == VError NA) ctx args

        "IFS" ->
            evalIfs ctx args

        "SWITCH" ->
            evalSwitch ctx args

        "CHOOSE" ->
            evalChoose ctx args

        "ROW" ->
            VNumber (toFloat (refRowCol (\r -> r.row) ctx args + 1))

        "COLUMN" ->
            VNumber (toFloat (refRowCol (\r -> r.col) ctx args + 1))

        "TEXT" ->
            evalText ctx args

        "ISFORMULA" ->
            VBool (isFormulaCell ctx args)

        "FORMULATEXT" ->
            case Maybe.andThen ctx.formulaText (refArg args) of
                Just t ->
                    VText t

                Nothing ->
                    VError NA

        _ ->
            case ctx.lambda name of
                Just lam ->
                    applyLambda ctx lam (List.map (evalArg ctx) args)

                Nothing ->
                    Functions.call name (List.map (evalArg ctx) args)


{-| The `Ref` an audit function is asked about (its first argument). -}
refArg : List Expr -> Maybe Ref
refArg args =
    case args of
        (RefE ref _) :: _ ->
            Just ref

        (RangeE range _ _) :: _ ->
            Just (Ref.normalize range).start

        _ ->
            Nothing


isFormulaCell : Context -> List Expr -> Bool
isFormulaCell ctx args =
    case Maybe.andThen ctx.formulaText (refArg args) of
        Just _ ->
            True

        Nothing ->
            False


{-| Evaluate an argument expression, preserving 2-D shape for range references so that
INDEX/VLOOKUP/MATCH and the range-aware aggregates get the rectangle they need. -}
evalArg : Context -> Expr -> Arg
evalArg ctx expr =
    case expr of
        RangeE range _ _ ->
            matrixOf ctx range

        NameE name ->
            case ctx.locals name of
                Just arg ->
                    arg

                Nothing ->
                    case ctx.names name of
                        Just range ->
                            matrixOf ctx range

                        Nothing ->
                            Scalar (VError NameErr)

        StructRefE tableName selector ->
            case structRange ctx tableName selector of
                Just range ->
                    matrixOf ctx range

                Nothing ->
                    Scalar (structScalarOnly ctx tableName selector)

        SpillRefE anchor ->
            case ctx.spill anchor of
                Just range ->
                    matrixOf ctx range

                Nothing ->
                    Scalar (VError RefErr)

        SheetRangeE sheetName range _ _ ->
            Matrix (List.map (List.map (ctx.external sheetName)) (Ref.rowsOf range))

        Func name fargs ->
            if isRefForm name then
                case refFormRange ctx name fargs of
                    Just range ->
                        matrixOf ctx range

                    Nothing ->
                        Scalar (VError RefErr)

            else if isArrayForm name then
                case arrayResult ctx name fargs of
                    Just matrix ->
                        Matrix matrix

                    Nothing ->
                        Scalar (VError ValueErr)

            else
                Scalar (eval ctx expr)

        _ ->
            Scalar (eval ctx expr)


matrixOf : Context -> Range -> Arg
matrixOf ctx range =
    Matrix (List.map (List.map ctx.lookup) (Ref.rowsOf range))



-- LAZY SPECIAL FORMS ---------------------------------------------------------


evalIf : Context -> List Expr -> Value
evalIf ctx args =
    case args of
        cond :: thenE :: rest ->
            case Value.toBool (eval ctx cond) of
                Ok True ->
                    eval ctx thenE

                Ok False ->
                    case rest of
                        elseE :: _ ->
                            eval ctx elseE

                        [] ->
                            VBool False

                Err er ->
                    VError er

        _ ->
            VError ValueErr


evalIfError : (Value -> Bool) -> Context -> List Expr -> Value
evalIfError isTrap ctx args =
    case args of
        valueE :: fallbackE :: _ ->
            let
                v =
                    eval ctx valueE
            in
            if isTrap v then
                eval ctx fallbackE

            else
                v

        _ ->
            VError ValueErr


evalIfs : Context -> List Expr -> Value
evalIfs ctx args =
    case args of
        cond :: result :: rest ->
            case Value.toBool (eval ctx cond) of
                Ok True ->
                    eval ctx result

                Ok False ->
                    evalIfs ctx rest

                Err er ->
                    VError er

        _ ->
            VError NA


evalSwitch : Context -> List Expr -> Value
evalSwitch ctx args =
    case args of
        subjectE :: rest ->
            switchCases (eval ctx subjectE) ctx rest

        _ ->
            VError ValueErr


switchCases : Value -> Context -> List Expr -> Value
switchCases subject ctx args =
    case args of
        caseE :: resultE :: rest ->
            if Value.equalValue (eval ctx caseE) subject then
                eval ctx resultE

            else
                switchCases subject ctx rest

        [ defaultE ] ->
            eval ctx defaultE

        [] ->
            VError NA


evalChoose : Context -> List Expr -> Value
evalChoose ctx args =
    case args of
        indexE :: choices ->
            case Value.toNumber (eval ctx indexE) of
                Ok idx ->
                    case List.head (List.drop (round idx - 1) choices) of
                        Just chosen ->
                            eval ctx chosen

                        Nothing ->
                            VError ValueErr

                Err er ->
                    VError er

        _ ->
            VError ValueErr


evalText : Context -> List Expr -> Value
evalText ctx args =
    case args of
        valueE :: fmtE :: _ ->
            VText (Format.applyTextFormat (Value.toText (eval ctx fmtE)) (eval ctx valueE))

        _ ->
            VError ValueErr


refRowCol : (Ref -> Int) -> Context -> List Expr -> Int
refRowCol field ctx args =
    case args of
        (RefE ref _) :: _ ->
            field ref

        (RangeE range _ _) :: _ ->
            field (Ref.normalize range).start

        _ ->
            field ctx.self


isAnyError : Value -> Bool
isAnyError =
    Value.isError



-- OPERATORS ------------------------------------------------------------------


applyUnary : UnaryOp -> Value -> Value
applyUnary op v =
    case v of
        VError _ ->
            v

        _ ->
            case op of
                Neg ->
                    numericUnary negate v

                Pos ->
                    numericUnary identity v

                PercentOf ->
                    numericUnary (\x -> x / 100) v


numericUnary : (Float -> Float) -> Value -> Value
numericUnary f v =
    case Value.toNumber v of
        Ok n ->
            VNumber (f n)

        Err er ->
            VError er


applyBinary : BinaryOp -> Value -> Value -> Value
applyBinary op a b =
    case ( a, b ) of
        ( VError _, _ ) ->
            a

        ( _, VError _ ) ->
            b

        _ ->
            case op of
                Add ->
                    arith (+) a b

                Sub ->
                    arith (-) a b

                Mul ->
                    arith (*) a b

                Div ->
                    divide a b

                Pow ->
                    arith (^) a b

                Concat ->
                    VText (Value.toText a ++ Value.toText b)

                Eq ->
                    VBool (Value.equalValue a b)

                Ne ->
                    VBool (not (Value.equalValue a b))

                Lt ->
                    compareOp a b [ LT ]

                Gt ->
                    compareOp a b [ GT ]

                Le ->
                    compareOp a b [ LT, EQ ]

                Ge ->
                    compareOp a b [ GT, EQ ]


arith : (Float -> Float -> Float) -> Value -> Value -> Value
arith f a b =
    case ( Value.toNumber a, Value.toNumber b ) of
        ( Ok x, Ok y ) ->
            let
                r =
                    f x y
            in
            if isNaN r || isInfinite r then
                VError NumErr

            else
                VNumber r

        ( Err er, _ ) ->
            VError er

        ( _, Err er ) ->
            VError er


divide : Value -> Value -> Value
divide a b =
    case ( Value.toNumber a, Value.toNumber b ) of
        ( Ok x, Ok y ) ->
            if y == 0 then
                VError DivZero

            else
                VNumber (x / y)

        ( Err er, _ ) ->
            VError er

        ( _, Err er ) ->
            VError er


compareOp : Value -> Value -> List Order -> Value
compareOp a b accepted =
    VBool (List.member (Value.compare a b) accepted)
