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

import Spreadsheet.Ref exposing (Abs, Range, Ref)
import Spreadsheet.Value exposing (Value)


{-| A formula expression.

`RefE`/`RangeE` carry their `$`-absoluteness flags alongside the resolved coordinates, so
copy/fill can shift only the relative parts and the serializer can reproduce the `$`
markers. `NameE` is a defined-name reference (e.g. `TaxRate`) resolved against the sheet's
name table at evaluation time. `SpillRefE` is a **spill reference** (`A1#`): it stands for
the whole dynamic-array block that spilled from anchor `A1`, resolved against the sheet's
recorded spill ranges at evaluation time. `SheetRefE`/`SheetRangeE` are **cross-sheet**
references (`Data!A1`, `Data!A1:B5`) carrying the other sheet's name; they are resolved
against the workbook (`Spreadsheet.Workbook`) rather than the current sheet. -}
type Expr
    = Lit Value
    | RefE Ref Abs
    | RangeE Range Abs Abs
    | NameE String
    | SpillRefE Ref
    | SheetRefE String Ref Abs
    | SheetRangeE String Range Abs Abs
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
