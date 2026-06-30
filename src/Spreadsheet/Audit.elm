module Spreadsheet.Audit exposing
    ( evaluateSteps
    , exprToString
    )

{-| Formula **auditing**: an "Evaluate Formula" stepper, like the dialog in Excel that
replaces one sub-expression at a time with its value until the whole formula collapses to a
result.

`evaluateSteps` reduces the leftmost-innermost reducible node by one step at a time and
records the formula rendering after each step, so the returned list reads as a trace from the
original formula down to its final value. `exprToString` is the (precedence-aware) renderer
that turns an expression back into formula text.

@docs evaluateSteps
@docs exprToString

-}

import Spreadsheet.Ast exposing (BinaryOp(..), Expr(..), UnaryOp(..))
import Spreadsheet.Eval as Eval
import Spreadsheet.Ref as Ref
import Spreadsheet.Value as Value exposing (Value(..))


{-| The trace of `expr` evaluated against `ctx`: each entry is the formula rendering after one
more sub-expression has been reduced. The first entry is the original formula, the last is its
value. -}
evaluateSteps : Eval.Context -> Expr -> List String
evaluateSteps ctx expr =
    stepLoop ctx expr 0 []


stepLoop : Eval.Context -> Expr -> Int -> List String -> List String
stepLoop ctx expr depth acc =
    let
        acc2 =
            exprToString expr :: acc
    in
    if depth > 200 then
        List.reverse acc2

    else
        case reduceOnce ctx expr of
            Just next ->
                stepLoop ctx next (depth + 1) acc2

            Nothing ->
                List.reverse acc2


{-| Apply exactly one reduction to the leftmost-innermost reducible node, or `Nothing` when
`expr` is already a value (or an irreducible terminal like a bare range). -}
reduceOnce : Eval.Context -> Expr -> Maybe Expr
reduceOnce ctx expr =
    case expr of
        Lit _ ->
            Nothing

        RefE _ _ ->
            Just (Lit (Eval.eval ctx expr))

        NameE _ ->
            Just (Lit (Eval.eval ctx expr))

        SheetRefE _ _ _ ->
            Just (Lit (Eval.eval ctx expr))

        RangeE _ _ _ ->
            Nothing

        SheetRangeE _ _ _ _ ->
            Nothing

        StructRefE _ _ ->
            Nothing

        SpillRefE _ ->
            Nothing

        Unary op x ->
            case reduceOnce ctx x of
                Just x2 ->
                    Just (Unary op x2)

                Nothing ->
                    Just (Lit (Eval.eval ctx expr))

        Binary op l r ->
            case reduceOnce ctx l of
                Just l2 ->
                    Just (Binary op l2 r)

                Nothing ->
                    case reduceOnce ctx r of
                        Just r2 ->
                            Just (Binary op l r2)

                        Nothing ->
                            Just (Lit (Eval.eval ctx expr))

        Func name args ->
            case reduceFirst ctx args of
                Just args2 ->
                    Just (Func name args2)

                Nothing ->
                    Just (Lit (Eval.eval ctx expr))


{-| Reduce the first reducible argument (so nested calls collapse innermost-first). -}
reduceFirst : Eval.Context -> List Expr -> Maybe (List Expr)
reduceFirst ctx args =
    case args of
        [] ->
            Nothing

        a :: rest ->
            case reduceOnce ctx a of
                Just a2 ->
                    Just (a2 :: rest)

                Nothing ->
                    Maybe.map (\rest2 -> a :: rest2) (reduceFirst ctx rest)



-- RENDERING ----------------------------------------------------------------


{-| Render an expression back to formula text (without a leading `=`), parenthesizing only
where operator precedence requires it. -}
exprToString : Expr -> String
exprToString expr =
    render 0 expr


render : Int -> Expr -> String
render parentPrec expr =
    case expr of
        Lit v ->
            litToString v

        RefE ref abs ->
            Ref.toA1Abs abs ref

        RangeE range startAbs endAbs ->
            Ref.toA1Abs startAbs range.start ++ ":" ++ Ref.toA1Abs endAbs range.end

        NameE name ->
            name

        SpillRefE ref ->
            Ref.toA1 ref ++ "#"

        StructRefE table selector ->
            table ++ "[" ++ selector ++ "]"

        SheetRefE sheet ref abs ->
            sheet ++ "!" ++ Ref.toA1Abs abs ref

        SheetRangeE sheet range startAbs endAbs ->
            sheet ++ "!" ++ Ref.toA1Abs startAbs range.start ++ ":" ++ Ref.toA1Abs endAbs range.end

        Unary op x ->
            unaryString op (render 100 x)

        Func name args ->
            name ++ "(" ++ String.join "," (List.map (render 0) args) ++ ")"

        Binary op l r ->
            let
                prec =
                    binPrec op

                rendered =
                    render prec l ++ binString op ++ render (prec + 1) r
            in
            if prec < parentPrec then
                "(" ++ rendered ++ ")"

            else
                rendered


litToString : Value -> String
litToString value =
    case value of
        VText s ->
            "\"" ++ s ++ "\""

        VNumber n ->
            Value.numberToString n

        VBool b ->
            if b then
                "TRUE"

            else
                "FALSE"

        VEmpty ->
            ""

        VError e ->
            Value.errorText e


unaryString : UnaryOp -> String -> String
unaryString op rendered =
    case op of
        Neg ->
            "-" ++ rendered

        Pos ->
            "+" ++ rendered

        PercentOf ->
            rendered ++ "%"


binPrec : BinaryOp -> Int
binPrec op =
    case op of
        Eq ->
            1

        Ne ->
            1

        Lt ->
            1

        Gt ->
            1

        Le ->
            1

        Ge ->
            1

        Concat ->
            2

        Add ->
            3

        Sub ->
            3

        Mul ->
            4

        Div ->
            4

        Pow ->
            5


binString : BinaryOp -> String
binString op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Pow ->
            "^"

        Concat ->
            "&"

        Eq ->
            "="

        Ne ->
            "<>"

        Lt ->
            "<"

        Gt ->
            ">"

        Le ->
            "<="

        Ge ->
            ">="
