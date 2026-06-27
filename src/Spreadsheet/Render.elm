module Spreadsheet.Render exposing
    ( expr
    , formula
    )

{-| Serialize a parsed `Expr` back to formula text — the inverse of `Spreadsheet.Parser`.

This is what makes the *structural* features work: copy/paste, autofill and
insert/delete-row/column all rewrite a formula's syntax tree (shifting references,
turning deleted ones into `#REF!`) and then need the result as text again to store as the
cell's raw input and show in the formula bar.

Rendering is precedence-aware: each node reports its operator precedence, and a child is
parenthesised only when it binds *looser* than its parent would require — so `=A1+B1*2`
round-trips without spurious parentheses, while `=(A1+B1)*2` keeps them. Power is
right-associative and unary minus binds tighter than power, matching the parser.

@docs expr, formula

-}

import Spreadsheet.Ast exposing (BinaryOp(..), Expr(..), UnaryOp(..))
import Spreadsheet.Ref as Ref
import Spreadsheet.Value as Value exposing (Value(..))


{-| Render an expression as a formula body (no leading `=`). -}
expr : Expr -> String
expr e =
    Tuple.first (render e)


{-| Render an expression as a complete formula, with the leading `=`. -}
formula : Expr -> String
formula e =
    "=" ++ expr e


{-| Render to `(text, precedence)`; the precedence lets callers decide on parentheses. -}
render : Expr -> ( String, Int )
render e =
    case e of
        Lit v ->
            ( lit v, atomP )

        RefE ref abs ->
            ( Ref.toA1Abs abs ref, atomP )

        RangeE range startAbs endAbs ->
            ( Ref.toA1Abs startAbs range.start ++ ":" ++ Ref.toA1Abs endAbs range.end, atomP )

        NameE name ->
            ( name, atomP )

        SheetRefE sheetName ref abs ->
            ( sheetName ++ "!" ++ Ref.toA1Abs abs ref, atomP )

        SheetRangeE sheetName range startAbs endAbs ->
            ( sheetName ++ "!" ++ Ref.toA1Abs startAbs range.start ++ ":" ++ Ref.toA1Abs endAbs range.end, atomP )

        Func name args ->
            ( name ++ "(" ++ String.join "," (List.map expr args) ++ ")", atomP )

        Unary op sub ->
            case op of
                Neg ->
                    ( "-" ++ looserThan unaryP sub, unaryP )

                Pos ->
                    ( "+" ++ looserThan unaryP sub, unaryP )

                PercentOf ->
                    ( looserThan postfixP sub ++ "%", postfixP )

        Binary op a b ->
            let
                p =
                    binPrec op

                sym =
                    binSym op
            in
            if op == Pow then
                -- right associative
                ( looserOrEq p a ++ sym ++ looserThan p b, p )

            else
                -- left associative
                ( looserThan p a ++ sym ++ looserOrEq p b, p )


{-| Render a child, parenthesising it when it binds *strictly looser* than `bound`. -}
looserThan : Int -> Expr -> String
looserThan bound child =
    let
        ( text, p ) =
            render child
    in
    if p < bound then
        "(" ++ text ++ ")"

    else
        text


{-| Render a child, parenthesising it when it binds looser than *or as loosely as* `bound`
(used for the side of an operator where equal precedence still needs grouping). -}
looserOrEq : Int -> Expr -> String
looserOrEq bound child =
    let
        ( text, p ) =
            render child
    in
    if p <= bound then
        "(" ++ text ++ ")"

    else
        text


lit : Value -> String
lit v =
    case v of
        VText s ->
            "\"" ++ String.replace "\"" "\"\"" s ++ "\""

        VNumber n ->
            Value.toText (VNumber n)

        VBool b ->
            if b then
                "TRUE"

            else
                "FALSE"

        VEmpty ->
            ""

        VError err ->
            Value.errorText err


binSym : BinaryOp -> String
binSym op =
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


binPrec : BinaryOp -> Int
binPrec op =
    case op of
        Eq ->
            comparisonP

        Ne ->
            comparisonP

        Lt ->
            comparisonP

        Gt ->
            comparisonP

        Le ->
            comparisonP

        Ge ->
            comparisonP

        Concat ->
            concatP

        Add ->
            addP

        Sub ->
            addP

        Mul ->
            mulP

        Div ->
            mulP

        Pow ->
            powP



-- PRECEDENCE LEVELS (higher binds tighter), mirroring Spreadsheet.Parser -------


comparisonP : Int
comparisonP =
    30


concatP : Int
concatP =
    40


addP : Int
addP =
    50


mulP : Int
mulP =
    60


powP : Int
powP =
    70


unaryP : Int
unaryP =
    80


postfixP : Int
postfixP =
    90


atomP : Int
atomP =
    100
