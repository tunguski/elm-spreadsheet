module Spreadsheet.Eval exposing
    ( Context
    , noLocals
    , noSpill
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
    , locals : String -> Maybe Value
    , spill : Ref -> Maybe Range
    }


{-| A `locals` resolver with no bindings — the default outside a `LET`. -}
noLocals : String -> Maybe Value
noLocals _ =
    Nothing


{-| A `spill` resolver that knows of no spilled blocks — the default when a sheet has no
dynamic arrays (or when evaluating outside one). -}
noSpill : Ref -> Maybe Range
noSpill _ =
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
                Just v ->
                    v

                Nothing ->
                    case ctx.names name of
                        Just range ->
                            if range.start == range.end then
                                ctx.lookup range.start

                            else
                                scalarOfRange ctx range

                        Nothing ->
                            VError NameErr

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

        Func name args ->
            if isArrayForm name then
                arrayResult ctx name args

            else
                Nothing

        _ ->
            Nothing


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



-- LET ------------------------------------------------------------------------


{-| `LET(name1, value1, …, calc)`: bind each `name` to its evaluated `value` (later
bindings can use earlier ones), then evaluate the final `calc` with those names in scope. -}
evalLet : Context -> List Expr -> Value
evalLet ctx args =
    letBind ctx Dict.empty args


letBind : Context -> Dict String Value -> List Expr -> Value
letBind ctx env args =
    case args of
        [ finalE ] ->
            evalWithEnv ctx env finalE

        (NameE name) :: valueE :: rest ->
            letBind ctx (Dict.insert name (evalWithEnv ctx env valueE) env) rest

        _ ->
            VError ValueErr


evalWithEnv : Context -> Dict String Value -> Expr -> Value
evalWithEnv ctx env e =
    eval { ctx | locals = extendLocals env ctx } e


extendLocals : Dict String Value -> Context -> String -> Maybe Value
extendLocals env ctx n =
    case Dict.get n env of
        Just v ->
            Just v

        Nothing ->
            ctx.locals n


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

        _ ->
            Functions.call name (List.map (evalArg ctx) args)


{-| Evaluate an argument expression, preserving 2-D shape for range references so that
INDEX/VLOOKUP/MATCH and the range-aware aggregates get the rectangle they need. -}
evalArg : Context -> Expr -> Arg
evalArg ctx expr =
    case expr of
        RangeE range _ _ ->
            matrixOf ctx range

        NameE name ->
            case ctx.locals name of
                Just v ->
                    Scalar v

                Nothing ->
                    case ctx.names name of
                        Just range ->
                            matrixOf ctx range

                        Nothing ->
                            Scalar (VError NameErr)

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
