module Spreadsheet.Ast exposing
    ( Expr(..)
    , UnaryOp(..)
    , BinaryOp(..)
    )

{-| The abstract syntax of a parsed formula.

A formula is an expression tree. Leaves are literals, single-cell references and
range references; internal nodes are unary/binary operators and function calls.
Evaluation (`Spreadsheet.Eval`) walks this tree against a sheet, and dependency
extraction (`Spreadsheet.Deps`) walks it collecting every `RefE`/`RangeE`.

@docs Expr, UnaryOp, BinaryOp

-}

import Spreadsheet.Ref exposing (Range, Ref)
import Spreadsheet.Value exposing (Value)


{-| A formula expression. -}
type Expr
    = Lit Value
    | RefE Ref
    | RangeE Range
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Func String (List Expr)


{-| Prefix `-` and postfix `%`. -}
type UnaryOp
    = Neg
    | Pos
    | PercentOf


{-| The infix operators, in spreadsheet flavour: `&` concatenates, `^` is power,
`<>` is "not equal". -}
type BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Pow
    | Concat
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
