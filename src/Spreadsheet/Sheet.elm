module Spreadsheet.Sheet exposing
    ( Sheet
    , Cell
    , Parsed(..)
    , empty
    , dims
    , get
    , valueAt
    , rawAt
    , formatAt
    , setRaw
    , setRawMany
    , setFormat
    , setStyle
    , addConditional
    , addColorScale
    , addDataBar
    , recalcAll
    , recalcFrom
    , recalcOrder
    , evalAndSet
    , markCircular
    , dirtyClosure
    , precedentsOf
    , displayString
    , baseStyleAt
    , effectiveStyle
    , renderedStyle
    , conditionalInline
    , key
    , keyToRef
    , formulaCells
    )

{-| The spreadsheet model and its (synchronous) recalculation engine.

A `Sheet` is an **opaque** value wrapping a sparse `Dict` of cells keyed by `(col, row)`,
plus the conditional-format rules, colour scales and data bars layered over them. Callers
read and write it only through the functions here — which both encapsulates the model and
sidesteps a compiler limitation in this backend around deeply-nested cross-module record
aliases.

Each `Cell` keeps the raw user input, its *parsed* form (a literal or a formula `Expr` —
parsed once, on entry, not on every recalc), its last computed `Value`, a number `Format`
and a static `CellStyle`.

Recalculation is dependency-correct: `recalcFrom` walks the dependency graph to find the
cells affected by a change, topologically sorts them so each is computed only after its
precedents, marks any cycles `#CIRC!`, and evaluates the rest in order. The same ordering
machinery (`recalcOrder` / `evalAndSet`) is what `Spreadsheet.Recalc` drives one batch at
a time for the async, visible-first path — so sync and async always agree.

@docs Sheet, Cell, Parsed
@docs empty, dims, get, valueAt, rawAt, formatAt
@docs setRaw, setRawMany, setFormat, setStyle, addConditional, addColorScale, addDataBar
@docs recalcAll, recalcFrom, recalcOrder, evalAndSet, markCircular, dirtyClosure, precedentsOf
@docs displayString, baseStyleAt, effectiveStyle, renderedStyle, conditionalInline
@docs key, keyToRef, formulaCells

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Spreadsheet.Ast exposing (Expr)
import Spreadsheet.Deps as Deps
import Spreadsheet.Eval as Eval
import Spreadsheet.Format as Format exposing (Format)
import Spreadsheet.Parser as Parser
import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Style as Style exposing (CellStyle, ColorScale, DataBar, Rendered, Rule)
import Spreadsheet.Value as Value exposing (Value(..))


{-| One cell. `parsed` is derived from `raw` once at entry; `value` is the last computed
result. -}
type alias Cell =
    { raw : String
    , parsed : Parsed
    , value : Value
    , format : Format
    , style : CellStyle
    }


{-| The parsed form of a cell's input. -}
type Parsed
    = PLiteral Value
    | PFormula Expr
    | PInvalid


{-| The whole sheet, opaque. -}
type Sheet
    = Sheet Model


type alias Model =
    { cells : Dict ( Int, Int ) Cell
    , rows : Int
    , cols : Int
    , conditionals : List Rule
    , colorScales : List ColorScale
    , dataBars : List DataBar
    }


{-| An empty sheet of the given dimensions. -}
empty : Int -> Int -> Sheet
empty rows cols =
    Sheet
        { cells = Dict.empty
        , rows = rows
        , cols = cols
        , conditionals = []
        , colorScales = []
        , dataBars = []
        }


{-| `(rows, cols)` of the sheet. -}
dims : Sheet -> ( Int, Int )
dims (Sheet m) =
    ( m.rows, m.cols )


{-| The `(col, row)` dictionary key for a ref. -}
key : Ref -> ( Int, Int )
key ref =
    ( ref.col, ref.row )


{-| Recover a ref from a key. -}
keyToRef : ( Int, Int ) -> Ref
keyToRef ( c, r ) =
    { col = c, row = r }


{-| Look up a cell. -}
get : Ref -> Sheet -> Maybe Cell
get ref (Sheet m) =
    Dict.get (key ref) m.cells


{-| The computed value at a ref — `VEmpty` if the cell is absent. -}
valueAt : Ref -> Sheet -> Value
valueAt ref (Sheet m) =
    case Dict.get (key ref) m.cells of
        Just cell ->
            cell.value

        Nothing ->
            VEmpty


{-| The raw input at a ref (empty string if absent). -}
rawAt : Ref -> Sheet -> String
rawAt ref sheet =
    Maybe.withDefault "" (Maybe.map .raw (get ref sheet))


{-| The number format at a ref. -}
formatAt : Ref -> Sheet -> Format
formatAt ref sheet =
    Maybe.withDefault Format.General (Maybe.map .format (get ref sheet))



-- EDITS ----------------------------------------------------------------------


{-| Parse a raw input string into its `Parsed` form and an initial value (for literals;
formulas start at `VEmpty` until recalculated). A leading `=` marks a formula. -}
parseInput : String -> ( Parsed, Value )
parseInput raw =
    if String.startsWith "=" (String.trimLeft raw) then
        case Parser.parseFormula raw of
            Ok expr ->
                ( PFormula expr, VEmpty )

            Err _ ->
                ( PInvalid, VError Value.Parse )

    else
        let
            v =
                Value.fromString raw
        in
        ( PLiteral v, v )


{-| Set a cell's raw input *without* recalculating (use `recalcFrom`/`recalcAll`, or the
async stepper, afterwards). Returns the updated sheet. An empty string removes the cell. -}
setRaw : Ref -> String -> Sheet -> Sheet
setRaw ref raw ((Sheet m) as sheet) =
    if raw == "" then
        Sheet { m | cells = Dict.remove (key ref) m.cells }

    else
        let
            ( parsed, value ) =
                parseInput raw

            existing =
                get ref sheet

            cell =
                { raw = raw
                , parsed = parsed
                , value =
                    case parsed of
                        PFormula _ ->
                            Maybe.withDefault value (Maybe.map .value existing)

                        _ ->
                            value
                , format = Maybe.withDefault Format.General (Maybe.map .format existing)
                , style = Maybe.withDefault Style.emptyStyle (Maybe.map .style existing)
                }
        in
        Sheet { m | cells = Dict.insert (key ref) cell m.cells }


{-| Set many raw inputs at once (no recalc). -}
setRawMany : List ( Ref, String ) -> Sheet -> Sheet
setRawMany edits sheet =
    List.foldl (\( ref, raw ) acc -> setRaw ref raw acc) sheet edits


{-| Set a cell's number format (creating an empty cell if needed). -}
setFormat : Ref -> Format -> Sheet -> Sheet
setFormat ref fmt sheet =
    updateCell ref (\cell -> { cell | format = fmt }) sheet


{-| Set a cell's static style. -}
setStyle : Ref -> CellStyle -> Sheet -> Sheet
setStyle ref style sheet =
    updateCell ref (\cell -> { cell | style = style }) sheet


updateCell : Ref -> (Cell -> Cell) -> Sheet -> Sheet
updateCell ref f ((Sheet m) as sheet) =
    let
        cell =
            Maybe.withDefault emptyCell (get ref sheet)
    in
    Sheet { m | cells = Dict.insert (key ref) (f cell) m.cells }


emptyCell : Cell
emptyCell =
    { raw = ""
    , parsed = PLiteral VEmpty
    , value = VEmpty
    , format = Format.General
    , style = Style.emptyStyle
    }


{-| Append a conditional-format rule. -}
addConditional : Rule -> Sheet -> Sheet
addConditional rule (Sheet m) =
    Sheet { m | conditionals = m.conditionals ++ [ rule ] }


{-| Append a colour-scale rule. -}
addColorScale : ColorScale -> Sheet -> Sheet
addColorScale cs (Sheet m) =
    Sheet { m | colorScales = m.colorScales ++ [ cs ] }


{-| Append a data-bar rule. -}
addDataBar : DataBar -> Sheet -> Sheet
addDataBar db (Sheet m) =
    Sheet { m | dataBars = m.dataBars ++ [ db ] }



-- DEPENDENCIES ---------------------------------------------------------------


{-| Every formula cell's key. -}
formulaCells : Sheet -> List ( Int, Int )
formulaCells (Sheet m) =
    Dict.toList m.cells
        |> List.filterMap
            (\( k, cell ) ->
                case cell.parsed of
                    PFormula _ ->
                        Just k

                    _ ->
                        Nothing
            )


{-| The cells a given cell reads directly (its precedents). -}
precedentsOf : Sheet -> ( Int, Int ) -> List ( Int, Int )
precedentsOf (Sheet m) k =
    case Dict.get k m.cells of
        Just cell ->
            case cell.parsed of
                PFormula expr ->
                    Deps.precedents expr

                _ ->
                    []

        Nothing ->
            []


{-| The reverse graph: cell key → keys of formula cells that read it. -}
dependentsMap : Sheet -> Dict ( Int, Int ) (List ( Int, Int ))
dependentsMap sheet =
    List.foldl
        (\fk acc ->
            List.foldl
                (\pk inner ->
                    Dict.update pk
                        (\existing -> Just (fk :: Maybe.withDefault [] existing))
                        inner
                )
                acc
                (precedentsOf sheet fk)
        )
        Dict.empty
        (formulaCells sheet)


{-| All cells affected by a change to `changed`: the changed cells plus everything that
(transitively) depends on them. -}
dirtyClosure : List Ref -> Sheet -> List ( Int, Int )
dirtyClosure changed sheet =
    let
        deps =
            dependentsMap sheet

        seeds =
            List.map key changed
    in
    bfs deps seeds (Set.fromList seeds) seeds


bfs :
    Dict ( Int, Int ) (List ( Int, Int ))
    -> List ( Int, Int )
    -> Set ( Int, Int )
    -> List ( Int, Int )
    -> List ( Int, Int )
bfs deps frontier seen acc =
    case frontier of
        [] ->
            acc

        _ ->
            let
                next =
                    List.concatMap (\k -> Maybe.withDefault [] (Dict.get k deps)) frontier
                        |> List.filter (\k -> not (Set.member k seen))
                        |> dedupKeys

                seen2 =
                    List.foldl Set.insert seen next
            in
            bfs deps next seen2 (acc ++ next)


dedupKeys : List ( Int, Int ) -> List ( Int, Int )
dedupKeys ks =
    Set.toList (Set.fromList ks)



-- RECALCULATION --------------------------------------------------------------


{-| Topologically order a set of dirty keys (precedents first) and report the keys caught
in a cycle. -}
recalcOrder : List ( Int, Int ) -> Sheet -> ( List ( Int, Int ), Set ( Int, Int ) )
recalcOrder dirty sheet =
    -- Only formula cells need (re)evaluating; literal cells in the dirty set are already
    -- final. Dropping them keeps the work queue tight so async batches and visible-first
    -- prioritisation aren't spent on no-ops.
    Deps.topoSort (precedentsOf sheet) (List.filter (isFormulaKey sheet) dirty)


isFormulaKey : Sheet -> ( Int, Int ) -> Bool
isFormulaKey (Sheet m) k =
    case Dict.get k m.cells of
        Just cell ->
            case cell.parsed of
                PFormula _ ->
                    True

                _ ->
                    False

        Nothing ->
            False


{-| Evaluate a single formula cell against the sheet's current values and store the
result. Literal cells are left untouched. -}
evalAndSet : ( Int, Int ) -> Sheet -> Sheet
evalAndSet k ((Sheet m) as sheet) =
    case Dict.get k m.cells of
        Just cell ->
            case cell.parsed of
                PFormula expr ->
                    let
                        ctx =
                            { lookup = \r -> valueAt r sheet
                            , self = keyToRef k
                            }

                        value =
                            Eval.eval ctx expr
                    in
                    Sheet { m | cells = Dict.insert k { cell | value = value } m.cells }

                _ ->
                    sheet

        Nothing ->
            sheet


{-| Mark the given keys as `#CIRC!`. -}
markCircular : List ( Int, Int ) -> Sheet -> Sheet
markCircular ks sheet =
    List.foldl
        (\k (Sheet m) ->
            case Dict.get k m.cells of
                Just cell ->
                    Sheet { m | cells = Dict.insert k { cell | value = VError Value.Circular } m.cells }

                Nothing ->
                    Sheet m
        )
        sheet
        ks


{-| Synchronously recompute every formula cell in the sheet, in dependency order. -}
recalcAll : Sheet -> Sheet
recalcAll sheet =
    runRecalc (formulaCells sheet) sheet


{-| Synchronously recompute the cells affected by changes to `changed`. -}
recalcFrom : List Ref -> Sheet -> Sheet
recalcFrom changed sheet =
    runRecalc (dirtyClosure changed sheet) sheet


runRecalc : List ( Int, Int ) -> Sheet -> Sheet
runRecalc dirty sheet =
    let
        ( ordered, cyclic ) =
            recalcOrder dirty sheet

        evaluated =
            List.foldl evalAndSet sheet ordered
    in
    markCircular (Set.toList cyclic) evaluated



-- DISPLAY --------------------------------------------------------------------


{-| The formatted display string for a cell. -}
displayString : Ref -> Sheet -> String
displayString ref sheet =
    case get ref sheet of
        Just cell ->
            Format.format cell.format cell.value

        Nothing ->
            ""


{-| A cell's own (static) style. -}
baseStyleAt : Ref -> Sheet -> CellStyle
baseStyleAt ref sheet =
    Maybe.withDefault Style.emptyStyle (Maybe.map .style (get ref sheet))


{-| The effective style for a cell: its static style with every matching conditional-format
rule layered on top (in declaration order). -}
effectiveStyle : Ref -> Sheet -> CellStyle
effectiveStyle ref ((Sheet m) as sheet) =
    let
        value =
            valueAt ref sheet

        applicable =
            List.filter
                (\rule ->
                    Ref.contains rule.range ref && Style.matches rule.condition value
                )
                m.conditionals
    in
    List.foldl (\rule acc -> Style.mergeStyle acc rule.style)
        (baseStyleAt ref sheet)
        applicable


{-| The fully-resolved style for a cell as render-ready classes + inline declarations:
the effective `CellStyle` rendered, plus any data-driven colour-scale/data-bar inline.
The view uses this so it never needs the `CellStyle` record itself. -}
renderedStyle : Ref -> Sheet -> Rendered
renderedStyle ref sheet =
    let
        base =
            Style.render (effectiveStyle ref sheet) (valueAt ref sheet)
    in
    { base | inline = base.inline ++ conditionalInline ref sheet }


{-| Inline, data-driven declarations for a cell: colour-scale backgrounds and data-bar
fills, which depend on the cell's value relative to its range and so cannot be expressed
as fixed classes. -}
conditionalInline : Ref -> Sheet -> List ( String, String )
conditionalInline ref sheet =
    let
        value =
            valueAt ref sheet
    in
    case Value.toNumber value of
        Err _ ->
            []

        Ok n ->
            scaleInline ref n sheet ++ barInline ref n sheet


scaleInline : Ref -> Float -> Sheet -> List ( String, String )
scaleInline ref n (Sheet m) =
    List.concatMap
        (\cs ->
            if Ref.contains cs.range ref then
                case rangeExtent cs.range (Sheet m) of
                    Just ( lo, hi ) ->
                        let
                            t =
                                if hi <= lo then
                                    0.5

                                else
                                    (n - lo) / (hi - lo)
                        in
                        [ ( "background-color", Style.lerpColor cs.low cs.high t ) ]

                    Nothing ->
                        []

            else
                []
        )
        m.colorScales


barInline : Ref -> Float -> Sheet -> List ( String, String )
barInline ref n (Sheet m) =
    List.concatMap
        (\db ->
            if Ref.contains db.range ref then
                case rangeExtent db.range (Sheet m) of
                    Just ( lo, hi ) ->
                        let
                            pct =
                                Style.dataBarPercent (min lo 0) hi n
                        in
                        [ ( "background"
                          , "linear-gradient(to right, "
                                ++ db.color
                                ++ " "
                                ++ String.fromInt (round pct)
                                ++ "%, transparent "
                                ++ String.fromInt (round pct)
                                ++ "%)"
                          )
                        ]

                    Nothing ->
                        []

            else
                []
        )
        m.dataBars


{-| The numeric min and max over a range's cells (ignoring non-numbers). -}
rangeExtent : Range -> Sheet -> Maybe ( Float, Float )
rangeExtent range sheet =
    let
        nums =
            Ref.cellsOf range
                |> List.filterMap
                    (\ref ->
                        case valueAt ref sheet of
                            VNumber x ->
                                Just x

                            _ ->
                                Nothing
                    )
    in
    case nums of
        [] ->
            Nothing

        first :: rest ->
            Just
                ( List.foldl min first rest
                , List.foldl max first rest
                )
