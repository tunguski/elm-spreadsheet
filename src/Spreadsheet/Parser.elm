module Spreadsheet.Parser exposing (parse, parseFormula)

{-| Turn a formula string into an `Expr` tree.

This is a hand-written tokenizer plus a precedence-climbing recursive-descent parser —
no `elm/parser` dependency, so it compiles cleanly on every backend. The grammar is the
usual spreadsheet one:

    comparison := concat   ( ( "=" | "<>" | "<" | ">" | "<=" | ">=" ) concat )*
    concat     := add      ( "&" add )*
    add        := mul      ( ( "+" | "-" ) mul )*
    mul        := power    ( ( "*" | "/" ) power )*
    power      := unary     ( "^" power )?            -- right associative
    unary      := ( "+" | "-" )* postfix
    postfix    := primary   ( "%" )*
    primary    := number | string | "TRUE" | "FALSE"
                | ref ( ":" ref )?                     -- single cell or range
                | name "(" ( expr ( "," expr )* )? ")" -- function call
                | "(" expr ")"

Per Excel, unary minus binds *tighter* than `^` (so `-2^2 = 4`).

@docs parse, parseFormula

-}

import Spreadsheet.Ast exposing (BinaryOp(..), Expr(..), UnaryOp(..))
import Spreadsheet.Ref as Ref
import Spreadsheet.Value as Value exposing (Value(..))



-- TOKENS ---------------------------------------------------------------------


type Token
    = TNum Float
    | TStr String
    | TIdent String
    | TLParen
    | TRParen
    | TComma
    | TColon
    | TPlus
    | TMinus
    | TStar
    | TSlash
    | TCaret
    | TAmp
    | TPercent
    | TEq
    | TNe
    | TLt
    | TGt
    | TLe
    | TGe


tokenize : String -> Result String (List Token)
tokenize src =
    tokenizeHelp (String.toList src) []


tokenizeHelp : List Char -> List Token -> Result String (List Token)
tokenizeHelp chars acc =
    case chars of
        [] ->
            Ok (List.reverse acc)

        c :: rest ->
            if c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' then
                tokenizeHelp rest acc

            else if c == '(' then
                tokenizeHelp rest (TLParen :: acc)

            else if c == ')' then
                tokenizeHelp rest (TRParen :: acc)

            else if c == ',' then
                tokenizeHelp rest (TComma :: acc)

            else if c == ':' then
                tokenizeHelp rest (TColon :: acc)

            else if c == '+' then
                tokenizeHelp rest (TPlus :: acc)

            else if c == '-' then
                tokenizeHelp rest (TMinus :: acc)

            else if c == '*' then
                tokenizeHelp rest (TStar :: acc)

            else if c == '/' then
                tokenizeHelp rest (TSlash :: acc)

            else if c == '^' then
                tokenizeHelp rest (TCaret :: acc)

            else if c == '&' then
                tokenizeHelp rest (TAmp :: acc)

            else if c == '%' then
                tokenizeHelp rest (TPercent :: acc)

            else if c == '=' then
                tokenizeHelp rest (TEq :: acc)

            else if c == '<' then
                case rest of
                    '>' :: more ->
                        tokenizeHelp more (TNe :: acc)

                    '=' :: more ->
                        tokenizeHelp more (TLe :: acc)

                    _ ->
                        tokenizeHelp rest (TLt :: acc)

            else if c == '>' then
                case rest of
                    '=' :: more ->
                        tokenizeHelp more (TGe :: acc)

                    _ ->
                        tokenizeHelp rest (TGt :: acc)

            else if c == '"' then
                case scanString rest [] of
                    Ok ( str, more ) ->
                        tokenizeHelp more (TStr str :: acc)

                    Err e ->
                        Err e

            else if Char.isDigit c || (c == '.' && startsDigit rest) then
                let
                    ( numChars, more ) =
                        scanNumber (c :: rest)
                in
                case String.toFloat (String.fromList numChars) of
                    Just n ->
                        tokenizeHelp more (TNum n :: acc)

                    Nothing ->
                        Err ("bad number: " ++ String.fromList numChars)

            else if Char.isAlpha c || c == '$' || c == '_' then
                let
                    ( idChars, more ) =
                        scanIdent (c :: rest)
                in
                tokenizeHelp more (TIdent (String.fromList idChars) :: acc)

            else
                Err ("unexpected character: " ++ String.fromChar c)


startsDigit : List Char -> Bool
startsDigit chars =
    case chars of
        c :: _ ->
            Char.isDigit c

        [] ->
            False


scanString : List Char -> List Char -> Result String ( String, List Char )
scanString chars acc =
    case chars of
        [] ->
            Err "unterminated string"

        '"' :: '"' :: rest ->
            -- Escaped quote: "" inside a string literal is a single ".
            scanString rest ('"' :: acc)

        '"' :: rest ->
            Ok ( String.fromList (List.reverse acc), rest )

        c :: rest ->
            scanString rest (c :: acc)


scanNumber : List Char -> ( List Char, List Char )
scanNumber chars =
    takeWhileWith numberCont False chars []


numberCont : Char -> Bool -> ( Bool, Bool )
numberCont c seenExp =
    -- Accept digits, a single decimal point, an exponent marker and a sign right
    -- after it. `seenExp` lets `1e-3` keep its `-`.
    if Char.isDigit c || c == '.' then
        ( True, seenExp )

    else if c == 'e' || c == 'E' then
        ( True, True )

    else if (c == '+' || c == '-') && seenExp then
        ( True, False )

    else
        ( False, seenExp )


takeWhileWith : (Char -> Bool -> ( Bool, Bool )) -> Bool -> List Char -> List Char -> ( List Char, List Char )
takeWhileWith step state chars acc =
    case chars of
        [] ->
            ( List.reverse acc, [] )

        c :: rest ->
            let
                ( keep, nextState ) =
                    step c state
            in
            if keep then
                takeWhileWith step nextState rest (c :: acc)

            else
                ( List.reverse acc, c :: rest )


scanIdent : List Char -> ( List Char, List Char )
scanIdent chars =
    case chars of
        [] ->
            ( [], [] )

        c :: rest ->
            if Char.isAlphaNum c || c == '$' || c == '_' || c == '.' then
                let
                    ( more, remaining ) =
                        scanIdent rest
                in
                ( c :: more, remaining )

            else
                ( [], chars )



-- PARSER ---------------------------------------------------------------------


{-| Parse a formula *without* the leading `=`. Returns the expression tree or a message. -}
parse : String -> Result String Expr
parse src =
    case tokenize src of
        Err e ->
            Err e

        Ok tokens ->
            case parseComparison tokens of
                Err e ->
                    Err e

                Ok ( expr, rest ) ->
                    if List.isEmpty rest then
                        Ok expr

                    else
                        Err "unexpected trailing input"


{-| Parse a full formula, tolerating an optional leading `=` (or `+`/`-` as some
spreadsheets allow to start a formula). -}
parseFormula : String -> Result String Expr
parseFormula raw =
    let
        s =
            String.trim raw
    in
    if String.startsWith "=" s then
        parse (String.dropLeft 1 s)

    else
        parse s


type alias P =
    List Token -> Result String ( Expr, List Token )


parseComparison : P
parseComparison tokens =
    parseBinaryLevel parseConcat comparisonOp tokens


comparisonOp : Token -> Maybe BinaryOp
comparisonOp t =
    case t of
        TEq ->
            Just Eq

        TNe ->
            Just Ne

        TLt ->
            Just Lt

        TGt ->
            Just Gt

        TLe ->
            Just Le

        TGe ->
            Just Ge

        _ ->
            Nothing


parseConcat : P
parseConcat tokens =
    parseBinaryLevel parseAdd concatOp tokens


concatOp : Token -> Maybe BinaryOp
concatOp t =
    case t of
        TAmp ->
            Just Concat

        _ ->
            Nothing


parseAdd : P
parseAdd tokens =
    parseBinaryLevel parseMul addOp tokens


addOp : Token -> Maybe BinaryOp
addOp t =
    case t of
        TPlus ->
            Just Add

        TMinus ->
            Just Sub

        _ ->
            Nothing


parseMul : P
parseMul tokens =
    parseBinaryLevel parsePower mulOp tokens


mulOp : Token -> Maybe BinaryOp
mulOp t =
    case t of
        TStar ->
            Just Mul

        TSlash ->
            Just Div

        _ ->
            Nothing


{-| Shared left-associative binary level: parse a higher-precedence operand, then fold
in `(op operand)*`. -}
parseBinaryLevel : P -> (Token -> Maybe BinaryOp) -> P
parseBinaryLevel next matchOp tokens =
    case next tokens of
        Err e ->
            Err e

        Ok ( left, rest ) ->
            parseBinaryLevelHelp next matchOp left rest


parseBinaryLevelHelp : P -> (Token -> Maybe BinaryOp) -> Expr -> List Token -> Result String ( Expr, List Token )
parseBinaryLevelHelp next matchOp left tokens =
    case tokens of
        t :: rest ->
            case matchOp t of
                Just op ->
                    case next rest of
                        Err e ->
                            Err e

                        Ok ( right, rest2 ) ->
                            parseBinaryLevelHelp next matchOp (Binary op left right) rest2

                Nothing ->
                    Ok ( left, tokens )

        [] ->
            Ok ( left, tokens )


{-| Power `^`, right associative. Both operands are unary expressions; since unary binds
*tighter* than power here (Excel semantics), `-2^2` parses as `(-2)^2 = 4` and `2^-2`
works too. -}
parsePower : P
parsePower tokens =
    case parseUnary tokens of
        Err e ->
            Err e

        Ok ( left, rest ) ->
            case rest of
                TCaret :: more ->
                    case parsePower more of
                        Err e ->
                            Err e

                        Ok ( right, rest2 ) ->
                            Ok ( Binary Pow left right, rest2 )

                _ ->
                    Ok ( left, rest )


{-| Unary prefix `+`/`-` (right-recursive so `--1` works), binding tighter than `^`. -}
parseUnary : P
parseUnary tokens =
    case tokens of
        TMinus :: rest ->
            case parseUnary rest of
                Err e ->
                    Err e

                Ok ( e, rest2 ) ->
                    Ok ( Unary Neg e, rest2 )

        TPlus :: rest ->
            case parseUnary rest of
                Err e ->
                    Err e

                Ok ( e, rest2 ) ->
                    Ok ( Unary Pos e, rest2 )

        _ ->
            parsePostfix tokens


{-| Postfix `%` (possibly repeated): `50%` → `0.5`. -}
parsePostfix : P
parsePostfix tokens =
    case parsePrimary tokens of
        Err e ->
            Err e

        Ok ( e, rest ) ->
            Ok (applyPercents e rest)


applyPercents : Expr -> List Token -> ( Expr, List Token )
applyPercents e tokens =
    case tokens of
        TPercent :: rest ->
            applyPercents (Unary PercentOf e) rest

        _ ->
            ( e, tokens )


parsePrimary : P
parsePrimary tokens =
    case tokens of
        (TNum n) :: rest ->
            Ok ( Lit (VNumber n), rest )

        (TStr s) :: rest ->
            Ok ( Lit (VText s), rest )

        TLParen :: rest ->
            case parseComparison rest of
                Err e ->
                    Err e

                Ok ( inner, rest2 ) ->
                    case rest2 of
                        TRParen :: rest3 ->
                            Ok ( inner, rest3 )

                        _ ->
                            Err "expected )"

        (TIdent name) :: TLParen :: rest ->
            parseCall name rest

        (TIdent name) :: rest ->
            parseIdentifier name rest

        _ ->
            Err "expected a value"


parseIdentifier : String -> List Token -> Result String ( Expr, List Token )
parseIdentifier name tokens =
    case String.toUpper name of
        "TRUE" ->
            Ok ( Lit (VBool True), tokens )

        "FALSE" ->
            Ok ( Lit (VBool False), tokens )

        _ ->
            case Ref.fromA1Abs name of
                Just ( startRef, startAbs ) ->
                    case tokens of
                        TColon :: (TIdent endName) :: rest ->
                            case Ref.fromA1Abs endName of
                                Just ( endRef, endAbs ) ->
                                    Ok ( RangeE { start = startRef, end = endRef } startAbs endAbs, rest )

                                Nothing ->
                                    Err ("bad range end: " ++ endName)

                        _ ->
                            Ok ( RefE startRef startAbs, tokens )

                Nothing ->
                    -- An unknown bare name → resolved against the sheet's name table at
                    -- eval time (or #NAME? if undefined).
                    Ok ( NameE (String.toUpper name), tokens )


parseCall : String -> List Token -> Result String ( Expr, List Token )
parseCall name tokens =
    case tokens of
        TRParen :: rest ->
            Ok ( Func (String.toUpper name) [], rest )

        _ ->
            case parseArgs tokens [] of
                Err e ->
                    Err e

                Ok ( args, rest ) ->
                    Ok ( Func (String.toUpper name) args, rest )


parseArgs : List Token -> List Expr -> Result String ( List Expr, List Token )
parseArgs tokens acc =
    case parseComparison tokens of
        Err e ->
            Err e

        Ok ( arg, rest ) ->
            case rest of
                TComma :: more ->
                    parseArgs more (arg :: acc)

                TRParen :: more ->
                    Ok ( List.reverse (arg :: acc), more )

                _ ->
                    Err "expected , or ) in argument list"
