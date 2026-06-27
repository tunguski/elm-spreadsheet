module Spreadsheet.Eval exposing
    ( Context
    , eval
    , evalString
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

import Spreadsheet.Ast exposing (BinaryOp(..), Expr(..), UnaryOp(..))
import Spreadsheet.Format as Format
import Spreadsheet.Functions as Functions exposing (Arg(..))
import Spreadsheet.Parser as Parser
import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Value as Value exposing (Error(..), Value(..))


{-| What the evaluator needs from the sheet: read another cell's value (`lookup`), know
which cell is being computed (`self`), resolve a defined name to its range (`names`), and
read a cell on another sheet of the workbook (`external sheetName ref`). -}
type alias Context =
    { lookup : Ref -> Value
    , self : Ref
    , names : String -> Maybe Range
    , external : String -> Ref -> Value
    }


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
            case ctx.names name of
                Just range ->
                    if range.start == range.end then
                        ctx.lookup range.start

                    else
                        scalarOfRange ctx range

                Nothing ->
                    VError NameErr

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
            evalFunc ctx name args


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
            case ctx.names name of
                Just range ->
                    matrixOf ctx range

                Nothing ->
                    Scalar (VError NameErr)

        SheetRangeE sheetName range _ _ ->
            Matrix (List.map (List.map (ctx.external sheetName)) (Ref.rowsOf range))

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
