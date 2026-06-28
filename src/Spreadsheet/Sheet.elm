module Spreadsheet.Sheet exposing
    ( Sheet
    , Cell
    , Parsed(..)
    , empty
    , dims
    , get
    , valueAt
    , spillRangeAt
    , isSpilled
    , rawAt
    , formatAt
    , setRaw
    , setRawMany
    , setFormat
    , setStyle
    , addConditional
    , addRankRule
    , addColorScale
    , addDataBar
    , addIconSet
    , iconAt
    , recalcAll
    , recalcAllWith
    , recalcFrom
    , recalcOrder
    , evalAndSet
    , evalAndSetWith
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
    , occupiedRefs
    , valuesOf
    , defaultColWidth
    , colWidth
    , setColWidth
    , insertRows
    , deleteRows
    , insertCols
    , deleteCols
    , copyPaste
    , cutPaste
    , fillDown
    , fillRight
    , fillSeries
    , spillInto
    , sortRange
    , filterRows
    , defineName
    , clearName
    , nameOf
    , definedNames
    , setNote
    , noteAt
    , mergeCells
    , unmerge
    , mergeAnchorAt
    , mergeContaining
    , isCovered
    , addValidation
    , validationAt
    , validate
    , isInvalid
    , dropdownAt
    , SparkKind(..)
    , Spark
    , setSparkline
    , sparklineAt
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
import Spreadsheet.Refactor as Refactor
import Spreadsheet.Render as Render
import Spreadsheet.Style as Style exposing (CellStyle, ColorScale, DataBar, Rendered, Rule)
import Spreadsheet.Validation as Validation
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
    , rankRules : List Style.RankRule
    , colorScales : List ColorScale
    , dataBars : List DataBar
    , colWidths : Dict Int Int
    , names : Dict String Range
    , notes : Dict ( Int, Int ) String
    , merges : List Range
    , validations : List Valid
    , sparklines : Dict ( Int, Int ) SparkSpec
    , iconSets : List Style.IconSet
    , spills : Dict ( Int, Int ) Value
    , spillAnchors : Dict ( Int, Int ) Range
    }


{-| A validation rule scoped to a range. -}
type alias Valid =
    { range : Range, rule : Validation.Rule }


{-| A sparkline attached to a cell: a tiny chart of a range, drawn in the cell. -}
type SparkKind
    = SparkBar
    | SparkLine


type alias SparkSpec =
    { range : Range, kind : SparkKind, color : String }


{-| A sparkline ready to render: the chart kind, colour and the numeric series. -}
type alias Spark =
    { kind : SparkKind, color : String, values : List Float }


{-| An empty sheet of the given dimensions. -}
empty : Int -> Int -> Sheet
empty rows cols =
    Sheet
        { cells = Dict.empty
        , rows = rows
        , cols = cols
        , conditionals = []
        , rankRules = []
        , colorScales = []
        , dataBars = []
        , colWidths = Dict.empty
        , names = Dict.empty
        , notes = Dict.empty
        , merges = []
        , validations = []
        , sparklines = Dict.empty
        , iconSets = []
        , spills = Dict.empty
        , spillAnchors = Dict.empty
        }


{-| The width (px) a column has when the user hasn't resized it. -}
defaultColWidth : Int
defaultColWidth =
    92


{-| The current width (px) of a column. -}
colWidth : Int -> Sheet -> Int
colWidth col (Sheet m) =
    Maybe.withDefault defaultColWidth (Dict.get col m.colWidths)


{-| Set a column's width (px), clamped to a sensible minimum. -}
setColWidth : Int -> Int -> Sheet -> Sheet
setColWidth col px (Sheet m) =
    Sheet { m | colWidths = Dict.insert col (max 32 px) m.colWidths }


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


{-| The computed value at a ref. A real cell wins; otherwise a value that *spilled* into
the cell from a dynamic array; otherwise `VEmpty`. -}
valueAt : Ref -> Sheet -> Value
valueAt ref (Sheet m) =
    case Dict.get (key ref) m.cells of
        Just cell ->
            cell.value

        Nothing ->
            case Dict.get (key ref) m.spills of
                Just v ->
                    v

                Nothing ->
                    VEmpty


{-| If `ref` is the anchor of a live dynamic-array spill, the block it spilled into (so the
view can outline it and `A1#` can resolve it). -}
spillRangeAt : Ref -> Sheet -> Maybe Range
spillRangeAt ref (Sheet m) =
    Dict.get (key ref) m.spillAnchors


{-| True when a cell holds a value that spilled into it from a dynamic array anchored
elsewhere (i.e. it is a non-anchor spill cell). -}
isSpilled : Ref -> Sheet -> Bool
isSpilled ref (Sheet m) =
    Dict.member (key ref) m.spills && not (Dict.member (key ref) m.cells)


{-| A range's current values as a 2-D matrix (row-major), e.g. to feed `Spreadsheet.Spill`. -}
valuesOf : Range -> Sheet -> List (List Value)
valuesOf range sheet =
    List.map (List.map (\r -> valueAt r sheet)) (Ref.rowsOf range)


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


{-| Append a range-aware conditional rule (top/bottom-N, above/below average, duplicate/
unique). -}
addRankRule : Style.RankRule -> Sheet -> Sheet
addRankRule rule (Sheet m) =
    Sheet { m | rankRules = m.rankRules ++ [ rule ] }


{-| Append a colour-scale rule. -}
addColorScale : ColorScale -> Sheet -> Sheet
addColorScale cs (Sheet m) =
    Sheet { m | colorScales = m.colorScales ++ [ cs ] }


{-| Append a data-bar rule. -}
addDataBar : DataBar -> Sheet -> Sheet
addDataBar db (Sheet m) =
    Sheet { m | dataBars = m.dataBars ++ [ db ] }


{-| Append an icon-set conditional format. -}
addIconSet : Style.IconSet -> Sheet -> Sheet
addIconSet set (Sheet m) =
    Sheet { m | iconSets = m.iconSets ++ [ set ] }


{-| The `(glyph, colour)` icon a cell earns under the first icon set covering it (numbers
only). -}
iconAt : Ref -> Sheet -> Maybe ( String, String )
iconAt ref ((Sheet m) as sheet) =
    case valueAt ref sheet of
        VNumber x ->
            findFirst (\s -> Ref.contains s.range ref) m.iconSets
                |> Maybe.map (\s -> Style.iconView s.style (Style.iconLevel s x))

        _ ->
            Nothing



-- DEPENDENCIES ---------------------------------------------------------------


{-| Every occupied cell's ref, in row-major order (top-to-bottom, left-to-right). -}
occupiedRefs : Sheet -> List Ref
occupiedRefs (Sheet m) =
    Dict.keys m.cells
        |> List.sortBy (\( c, r ) -> ( r, c ))
        |> List.map keyToRef


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
                    Deps.precedentsWith (resolveNameKeys m) expr

                _ ->
                    []

        Nothing ->
            []


{-| Cell keys a defined name stands for (empty if undefined). -}
resolveNameKeys : Model -> String -> List ( Int, Int )
resolveNameKeys m name =
    case Dict.get name m.names of
        Just range ->
            List.map key (Ref.cellsOf range)

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
evalAndSet =
    evalAndSetWith noExternal


{-| A resolver that fails every cross-sheet reference — the default when a sheet is
recalculated outside a workbook. -}
noExternal : String -> Ref -> Value
noExternal _ _ =
    VError Value.RefErr


{-| Like `evalAndSet`, but with a resolver for cross-sheet (`Sheet!A1`) references — the
hook `Spreadsheet.Workbook` uses to evaluate references into other sheets. -}
evalAndSetWith : (String -> Ref -> Value) -> ( Int, Int ) -> Sheet -> Sheet
evalAndSetWith external k ((Sheet m) as sheet) =
    case Dict.get k m.cells of
        Just cell ->
            case cell.parsed of
                PFormula expr ->
                    let
                        value =
                            Eval.eval (ctxFor external sheet (keyToRef k)) expr
                    in
                    Sheet { m | cells = Dict.insert k { cell | value = value } m.cells }

                _ ->
                    sheet

        Nothing ->
            sheet


{-| Build the evaluator context for computing the cell `self` against the sheet's current
values (including any live spills) and the given cross-sheet resolver. -}
ctxFor : (String -> Ref -> Value) -> Sheet -> Ref -> Eval.Context
ctxFor external ((Sheet m) as sheet) self =
    { lookup = \r -> valueAt r sheet
    , self = self
    , names = \name -> Dict.get name m.names
    , external = external
    , locals = Eval.noLocals
    , spill = \anchor -> Dict.get (key anchor) m.spillAnchors
    }


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


{-| Synchronously recompute every formula cell in the sheet, in dependency order, then
materialise any dynamic-array spills (re-evaluating to a fixed point so a formula that
reads a spilled block sees the spilled values). -}
recalcAll : Sheet -> Sheet
recalcAll sheet =
    recalcAllWith noExternal sheet


{-| Like `recalcAll`, but cross-sheet references resolve through `external` — used by
`Spreadsheet.Workbook` to recompute one sheet against the workbook's current values. -}
recalcAllWith : (String -> Ref -> Value) -> Sheet -> Sheet
recalcAllWith external sheet =
    settle external spillFuel (clearSpillState sheet)


{-| Synchronously recompute the cells affected by changes to `changed` (and re-settle any
spills the change touched).

A dynamic array's spilled cells aren't real cells, so an incremental closure can't see the
ones a change feeds; recomputing every formula and re-spilling to a fixed point keeps the
result correct. Sheets that are large enough to need true incrementality use the async
`Spreadsheet.Recalc` path, which is unchanged. -}
recalcFrom : List Ref -> Sheet -> Sheet
recalcFrom _ sheet =
    recalcAllWith noExternal sheet


{-| The maximum number of evaluate-then-spill rounds before giving up (matching the
workbook's fix-point cap). Chains of spills feeding spills settle in far fewer. -}
spillFuel : Int
spillFuel =
    25


clearSpillState : Sheet -> Sheet
clearSpillState (Sheet m) =
    Sheet { m | spills = Dict.empty, spillAnchors = Dict.empty }


{-| Evaluate every formula in order, then materialise spills; repeat until the spilled
state stops changing (or `fuel` runs out). -}
settle : (String -> Ref -> Value) -> Int -> Sheet -> Sheet
settle external fuel sheet =
    let
        evaluated =
            evalAllFormulas external sheet

        spilled =
            computeSpills external evaluated
    in
    if fuel <= 0 || spillState evaluated == spillState spilled then
        spilled

    else
        settle external (fuel - 1) spilled


spillState : Sheet -> ( Dict ( Int, Int ) Value, Dict ( Int, Int ) Range )
spillState (Sheet m) =
    ( m.spills, m.spillAnchors )


evalAllFormulas : (String -> Ref -> Value) -> Sheet -> Sheet
evalAllFormulas external sheet =
    let
        ( ordered, cyclic ) =
            recalcOrder (formulaCells sheet) sheet

        evaluated =
            List.foldl (evalAndSetWith external) sheet ordered
    in
    markCircular (Set.toList cyclic) evaluated


{-| Recompute the spilled cells from scratch: clear the old spill state, then for every
formula whose result is a dynamic-array block, write the block's cells (minus the anchor)
into the spill layer — or mark the anchor `#SPILL!` if the block would overwrite an
occupied cell. -}
computeSpills : (String -> Ref -> Value) -> Sheet -> Sheet
computeSpills external sheet =
    List.foldl (spillOne external) (clearSpillState sheet) (formulaCells sheet)


spillOne : (String -> Ref -> Value) -> ( Int, Int ) -> Sheet -> Sheet
spillOne external k ((Sheet m) as sheet) =
    case Dict.get k m.cells of
        Just cell ->
            case cell.parsed of
                PFormula expr ->
                    case Eval.evalMatrix (ctxFor external sheet (keyToRef k)) expr of
                        Just matrix ->
                            if isSpillable matrix then
                                spillMatrix (keyToRef k) matrix sheet

                            else
                                sheet

                        Nothing ->
                            sheet

                _ ->
                    sheet

        Nothing ->
            sheet


{-| A result spills only if it occupies more than one cell. -}
isSpillable : List (List Value) -> Bool
isSpillable matrix =
    let
        rows =
            List.length matrix

        cols =
            Maybe.withDefault 0 (List.maximum (List.map List.length matrix))
    in
    rows * cols > 1


spillMatrix : Ref -> List (List Value) -> Sheet -> Sheet
spillMatrix anchor matrix ((Sheet m) as sheet) =
    let
        children =
            List.concat
                (List.indexedMap
                    (\dr row ->
                        List.indexedMap
                            (\dc v -> ( ( anchor.col + dc, anchor.row + dr ), v ))
                            row
                    )
                    matrix
                )
                |> List.filter (\( ck, _ ) -> ck /= ( anchor.col, anchor.row ))

        collides =
            List.any (\( ck, _ ) -> Dict.member ck m.cells) children

        height =
            List.length matrix

        width =
            Maybe.withDefault 0 (List.maximum (List.map List.length matrix))
    in
    if collides then
        case Dict.get (key anchor) m.cells of
            Just cell ->
                Sheet { m | cells = Dict.insert (key anchor) { cell | value = VError Value.Spill } m.cells }

            Nothing ->
                sheet

    else
        Sheet
            { m
                | spills = List.foldl (\( ck, v ) acc -> Dict.insert ck v acc) m.spills children
                , spillAnchors =
                    Dict.insert (key anchor)
                        { start = anchor
                        , end = { col = anchor.col + width - 1, row = anchor.row + height - 1 }
                        }
                        m.spillAnchors
            }



-- STRUCTURAL EDITS -----------------------------------------------------------
-- Insert/delete whole rows or columns. Cells shift to their new positions, every
-- formula's references are rewritten (a reference into a deleted band becomes #REF!),
-- and the layered ranges (conditional formats, colour scales, data bars, named ranges)
-- and column widths move with them. Recalculate afterwards.


{-| Insert `n` blank rows before row `at`. -}
insertRows : Int -> Int -> Sheet -> Sheet
insertRows at n (Sheet m) =
    let
        keyMap ( c, r ) =
            Just ( c, insAt at n r )
    in
    Sheet
        (adjustRanges (Refactor.insertRowsRange at n)
            { m
                | cells = remapCells keyMap (Refactor.insertRows at n) m.cells
                , notes = remapKeyDict keyMap m.notes
                , rows = m.rows + n
            }
        )


{-| Delete `n` rows starting at row `at`. -}
deleteRows : Int -> Int -> Sheet -> Sheet
deleteRows at n (Sheet m) =
    let
        keyMap ( c, r ) =
            Maybe.map (\r2 -> ( c, r2 )) (delAt at n r)
    in
    Sheet
        (adjustRanges (Refactor.deleteRowsRange at n)
            { m
                | cells = remapCells keyMap (Refactor.deleteRows at n) m.cells
                , notes = remapKeyDict keyMap m.notes
                , rows = max 1 (m.rows - n)
            }
        )


{-| Insert `n` blank columns before column `at`. -}
insertCols : Int -> Int -> Sheet -> Sheet
insertCols at n (Sheet m) =
    let
        keyMap ( c, r ) =
            Just ( insAt at n c, r )
    in
    Sheet
        (adjustRanges (Refactor.insertColsRange at n)
            { m
                | cells = remapCells keyMap (Refactor.insertCols at n) m.cells
                , notes = remapKeyDict keyMap m.notes
                , cols = m.cols + n
                , colWidths = remapIntKeys (\c -> Just (insAt at n c)) m.colWidths
            }
        )


{-| Delete `n` columns starting at column `at`. -}
deleteCols : Int -> Int -> Sheet -> Sheet
deleteCols at n (Sheet m) =
    let
        keyMap ( c, r ) =
            Maybe.map (\c2 -> ( c2, r )) (delAt at n c)
    in
    Sheet
        (adjustRanges (Refactor.deleteColsRange at n)
            { m
                | cells = remapCells keyMap (Refactor.deleteCols at n) m.cells
                , notes = remapKeyDict keyMap m.notes
                , cols = max 1 (m.cols - n)
                , colWidths = remapIntKeys (delAt at n) m.colWidths
            }
        )


{-| A coordinate's new position after `n` units are inserted before `at`. -}
insAt : Int -> Int -> Int -> Int
insAt at n c =
    if c >= at then
        c + n

    else
        c


{-| A coordinate's new position after `n` units are deleted from `at` (Nothing if it sat
in the deleted band). -}
delAt : Int -> Int -> Int -> Maybe Int
delAt at n c =
    if c < at then
        Just c

    else if c >= at + n then
        Just (c - n)

    else
        Nothing


{-| Rebuild the cell dict, moving each cell's key and rewriting its formula; drop cells
whose key maps to `Nothing`. -}
remapCells : (( Int, Int ) -> Maybe ( Int, Int )) -> (Expr -> Expr) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
remapCells keyMap fExpr cells =
    Dict.foldl
        (\k cell acc ->
            case keyMap k of
                Just nk ->
                    Dict.insert nk (rewriteFormulaCell fExpr cell) acc

                Nothing ->
                    acc
        )
        Dict.empty
        cells


remapIntKeys : (Int -> Maybe Int) -> Dict Int v -> Dict Int v
remapIntKeys f d =
    Dict.foldl
        (\k v acc ->
            case f k of
                Just nk ->
                    Dict.insert nk v acc

                Nothing ->
                    acc
        )
        Dict.empty
        d


{-| Move the keys of a `(col,row)`-keyed dict (notes), dropping any that map to Nothing. -}
remapKeyDict : (( Int, Int ) -> Maybe ( Int, Int )) -> Dict ( Int, Int ) v -> Dict ( Int, Int ) v
remapKeyDict f d =
    Dict.foldl
        (\k v acc ->
            case f k of
                Just nk ->
                    Dict.insert nk v acc

                Nothing ->
                    acc
        )
        Dict.empty
        d


{-| Apply a formula transform to a cell, refreshing its raw text from the rewritten tree
and clearing its cached value (recompute on the next recalc). Literals are untouched. -}
rewriteFormulaCell : (Expr -> Expr) -> Cell -> Cell
rewriteFormulaCell f cell =
    case cell.parsed of
        PFormula expr ->
            let
                e2 =
                    f expr
            in
            { cell | parsed = PFormula e2, raw = Render.formula e2, value = VEmpty }

        _ ->
            cell


{-| Move every layered range (conditional rules, colour scales, data bars, named ranges)
through `f`, dropping any that the change removed entirely. -}
adjustRanges : (Range -> Maybe Range) -> Model -> Model
adjustRanges f m =
    { m
        | conditionals = List.filterMap (\rule -> Maybe.map (\rng -> { rule | range = rng }) (f rule.range)) m.conditionals
        , colorScales = List.filterMap (\cs -> Maybe.map (\rng -> { cs | range = rng }) (f cs.range)) m.colorScales
        , dataBars = List.filterMap (\db -> Maybe.map (\rng -> { db | range = rng }) (f db.range)) m.dataBars
        , rankRules = List.filterMap (\rr -> Maybe.map (\rng -> { rr | range = rng }) (f rr.range)) m.rankRules
        , iconSets = List.filterMap (\is -> Maybe.map (\rng -> { is | range = rng }) (f is.range)) m.iconSets
        , merges = List.filterMap f m.merges
        , validations = List.filterMap (\vd -> Maybe.map (\rng -> { vd | range = rng }) (f vd.range)) m.validations
        , names =
            Dict.toList m.names
                |> List.filterMap (\( nm, rng ) -> Maybe.map (\r -> ( nm, r )) (f rng))
                |> Dict.fromList
    }



-- CLIPBOARD ------------------------------------------------------------------


{-| Copy the `from` block and paste it with its top-left at `to`. **Copy semantics**:
relative references in the pasted formulas shift by the paste offset, `$`-absolute ones
stay pinned (`=A1+$B$1` pasted one row down becomes `=A2+$B$1`). Recalculate afterwards. -}
copyPaste : Range -> Ref -> Sheet -> Sheet
copyPaste from to (Sheet m) =
    let
        src =
            Ref.normalize from

        dCol =
            to.col - src.start.col

        dRow =
            to.row - src.start.row

        cells2 =
            List.foldl
                (\r acc ->
                    let
                        dk =
                            ( r.col + dCol, r.row + dRow )
                    in
                    case Dict.get (key r) m.cells of
                        Just cell ->
                            Dict.insert dk (translateCell dCol dRow cell) acc

                        Nothing ->
                            Dict.remove dk acc
                )
                m.cells
                (Ref.cellsOf src)
    in
    Sheet { m | cells = cells2 }


{-| Cut the `from` block and paste it with its top-left at `to`. **Move semantics**: the
cells move verbatim — their own references are *not* shifted (a moved formula keeps
pointing at the same cells) — and the source is cleared. (References elsewhere that point
into the moved block are left as-is — a documented simplification.) -}
cutPaste : Range -> Ref -> Sheet -> Sheet
cutPaste from to (Sheet m) =
    let
        src =
            Ref.normalize from

        dCol =
            to.col - src.start.col

        dRow =
            to.row - src.start.row

        moved =
            Ref.cellsOf src

        cleared =
            List.foldl (\r acc -> Dict.remove (key r) acc) m.cells moved

        placed =
            List.foldl
                (\r acc ->
                    let
                        dk =
                            ( r.col + dCol, r.row + dRow )
                    in
                    case Dict.get (key r) m.cells of
                        Just cell ->
                            Dict.insert dk cell acc

                        Nothing ->
                            Dict.remove dk acc
                )
                cleared
                moved
    in
    Sheet { m | cells = placed }


{-| Copy a cell to a new position offset by `(dCol, dRow)`, translating a formula's
relative references; literals are copied verbatim. -}
translateCell : Int -> Int -> Cell -> Cell
translateCell dCol dRow cell =
    case cell.parsed of
        PFormula expr ->
            let
                e2 =
                    Refactor.translate dCol dRow expr
            in
            { cell | parsed = PFormula e2, raw = Render.formula e2, value = VEmpty }

        _ ->
            cell



-- AUTOFILL -------------------------------------------------------------------


{-| Fill a range downward from its top row: each row below is the top row copied down,
with relative references shifted (so `=A1*2` becomes `=A2*2`, …). Recalculate afterwards. -}
fillDown : Range -> Sheet -> Sheet
fillDown range (Sheet m) =
    let
        n =
            Ref.normalize range

        cells2 =
            List.foldl
                (\r acc ->
                    List.foldl
                        (\c a -> fillCell m ( c, n.start.row ) ( 0, r - n.start.row ) ( c, r ) a)
                        acc
                        (List.range n.start.col n.end.col)
                )
                m.cells
                (List.range (n.start.row + 1) n.end.row)
    in
    Sheet { m | cells = cells2 }


{-| Fill a range rightward from its leftmost column (relative references shift across). -}
fillRight : Range -> Sheet -> Sheet
fillRight range (Sheet m) =
    let
        n =
            Ref.normalize range

        cells2 =
            List.foldl
                (\c acc ->
                    List.foldl
                        (\r a -> fillCell m ( n.start.col, r ) ( c - n.start.col, 0 ) ( c, r ) a)
                        acc
                        (List.range n.start.row n.end.row)
                )
                m.cells
                (List.range (n.start.col + 1) n.end.col)
    in
    Sheet { m | cells = cells2 }


{-| Copy `srcKey`'s cell to `destKey`, translating a formula by `(dCol, dRow)`; clears the
destination when the source is empty. -}
fillCell : Model -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
fillCell m srcKey ( dCol, dRow ) destKey acc =
    case Dict.get srcKey m.cells of
        Just cell ->
            Dict.insert destKey (translateCell dCol dRow cell) acc

        Nothing ->
            Dict.remove destKey acc


{-| Fill a numeric/date **series** vertically over a range: per column, read the leading
run of numbers as the seed, infer the step from the first two (or 1 for a single seed),
and write `first + step·i` down the whole column — preserving each cell's number format,
so a date-formatted column extrapolates as a date series. -}
fillSeries : Range -> Sheet -> Sheet
fillSeries range sheet =
    let
        n =
            Ref.normalize range
    in
    List.foldl (fillSeriesCol n) sheet (List.range n.start.col n.end.col)


fillSeriesCol : Range -> Int -> Sheet -> Sheet
fillSeriesCol n col sheet =
    let
        rows =
            List.range n.start.row n.end.row

        seeds =
            leadingNumbers col rows sheet
    in
    case seeds of
        [] ->
            sheet

        first :: rest ->
            let
                step =
                    case rest of
                        second :: _ ->
                            second - first

                        [] ->
                            1
            in
            List.foldl
                (\( i, r ) acc ->
                    setRaw { col = col, row = r } (Value.toText (VNumber (first + step * toFloat i))) acc
                )
                sheet
                (List.indexedMap (\i r -> ( i, r )) rows)


{-| Write a computed 2-D block (a dynamic-array result, see `Spreadsheet.Spill`) with its
top-left at `anchor`, as literal cells. Returns `Nothing` if any non-anchor target cell is
already occupied — the `#SPILL!` condition. Recalculate afterwards. -}
spillInto : Ref -> List (List Value) -> Sheet -> Maybe Sheet
spillInto anchor matrix sheet =
    let
        targets =
            List.concat
                (List.indexedMap
                    (\dr row ->
                        List.indexedMap
                            (\dc v -> ( { col = anchor.col + dc, row = anchor.row + dr }, v ))
                            row
                    )
                    matrix
                )

        collides =
            List.any (\( ref, _ ) -> ref /= anchor && rawAt ref sheet /= "") targets
    in
    if collides then
        Nothing

    else
        Just (List.foldl (\( ref, v ) acc -> setRaw ref (Value.toText v) acc) sheet targets)


{-| The leading run of numeric values down a column within the given rows. -}
leadingNumbers : Int -> List Int -> Sheet -> List Float
leadingNumbers col rows sheet =
    case rows of
        [] ->
            []

        r :: rest ->
            case valueAt { col = col, row = r } sheet of
                VNumber x ->
                    x :: leadingNumbers col rest sheet

                _ ->
                    []



-- SORT & FILTER --------------------------------------------------------------


{-| Sort the rows of a range by the values in column `keyCol`, ascending or descending.
Whole cells (input, format, style) move together; references are not rewritten, so this is
intended for ranges of data. Recalculate afterwards. -}
sortRange : Range -> Int -> Bool -> Sheet -> Sheet
sortRange range keyCol ascending ((Sheet m) as sheet) =
    let
        n =
            Ref.normalize range

        rows =
            List.range n.start.row n.end.row

        cols =
            List.range n.start.col n.end.col

        rowData =
            List.map
                (\r ->
                    { sortKey = valueAt { col = keyCol, row = r } sheet
                    , cells = List.filterMap (\c -> Maybe.map (\cell -> ( c, cell )) (Dict.get ( c, r ) m.cells)) cols
                    }
                )
                rows

        sorted =
            List.sortWith
                (\a b ->
                    let
                        o =
                            Value.compare a.sortKey b.sortKey
                    in
                    if ascending then
                        o

                    else
                        flipOrder o
                )
                rowData

        cleared =
            List.foldl (\r acc -> List.foldl (\c a -> Dict.remove ( c, r ) a) acc cols) m.cells rows

        placed =
            List.foldl
                (\( r, rd ) acc -> List.foldl (\( c, cell ) a -> Dict.insert ( c, r ) cell a) acc rd.cells)
                cleared
                (List.map2 (\r rd -> ( r, rd )) rows sorted)
    in
    Sheet { m | cells = placed }


flipOrder : Order -> Order
flipOrder o =
    case o of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| The rows of a range whose key-column value satisfies `pred` — the rest are the ones a
view would hide. Returns absolute row indices, top to bottom. -}
filterRows : Range -> Int -> (Value -> Bool) -> Sheet -> List Int
filterRows range keyCol pred sheet =
    let
        n =
            Ref.normalize range
    in
    List.filter (\r -> pred (valueAt { col = keyCol, row = r } sheet))
        (List.range n.start.row n.end.row)



-- NAMED RANGES ---------------------------------------------------------------


{-| Define (or redefine) a name for a cell/range. Names are case-insensitive and usable in
any formula, e.g. `defineName "TaxRate" (range "B1") sheet` then `=Price*TaxRate`. -}
defineName : String -> Range -> Sheet -> Sheet
defineName name range (Sheet m) =
    Sheet { m | names = Dict.insert (String.toUpper name) (Ref.normalize range) m.names }


{-| Remove a defined name. -}
clearName : String -> Sheet -> Sheet
clearName name (Sheet m) =
    Sheet { m | names = Dict.remove (String.toUpper name) m.names }


{-| Look up the range a name resolves to. -}
nameOf : String -> Sheet -> Maybe Range
nameOf name (Sheet m) =
    Dict.get (String.toUpper name) m.names


{-| Every defined name with its range. -}
definedNames : Sheet -> List ( String, Range )
definedNames (Sheet m) =
    Dict.toList m.names



-- NOTES ----------------------------------------------------------------------


{-| Attach a note/comment to a cell (an empty string removes it). -}
setNote : Ref -> String -> Sheet -> Sheet
setNote ref note (Sheet m) =
    if note == "" then
        Sheet { m | notes = Dict.remove (key ref) m.notes }

    else
        Sheet { m | notes = Dict.insert (key ref) note m.notes }


{-| The note on a cell, if any. -}
noteAt : Ref -> Sheet -> Maybe String
noteAt ref (Sheet m) =
    Dict.get (key ref) m.notes



-- SPARKLINES -----------------------------------------------------------------


{-| Attach a sparkline (mini chart of `range`) to a cell. -}
setSparkline : Ref -> Range -> SparkKind -> String -> Sheet -> Sheet
setSparkline ref range kind color (Sheet m) =
    Sheet { m | sparklines = Dict.insert (key ref) { range = range, kind = kind, color = color } m.sparklines }


{-| The sparkline at a cell, resolved to its current numeric series. -}
sparklineAt : Ref -> Sheet -> Maybe Spark
sparklineAt ref ((Sheet m) as sheet) =
    Dict.get (key ref) m.sparklines
        |> Maybe.map
            (\spec ->
                { kind = spec.kind
                , color = spec.color
                , values =
                    List.filterMap numberOfValue
                        (List.map (\r -> valueAt r sheet) (Ref.cellsOf spec.range))
                }
            )



-- MERGED CELLS ---------------------------------------------------------------


{-| Merge a range into one block: the top-left cell is the anchor (it keeps its value);
the other cells are cleared and hidden. Any existing merge overlapping the range is
replaced. -}
mergeCells : Range -> Sheet -> Sheet
mergeCells range ((Sheet m) as sheet) =
    let
        n =
            Ref.normalize range

        kept =
            List.filter (\mr -> not (rangesOverlap mr n)) m.merges

        covered =
            List.filter (\r -> r /= n.start) (Ref.cellsOf n)

        cleared =
            List.foldl (\r acc -> setRaw r "" acc) sheet covered
    in
    case cleared of
        Sheet m2 ->
            Sheet { m2 | merges = n :: kept }


{-| Remove the merge that contains `ref` (no-op if the cell isn't merged). -}
unmerge : Ref -> Sheet -> Sheet
unmerge ref (Sheet m) =
    Sheet { m | merges = List.filter (\mr -> not (Ref.contains mr ref)) m.merges }


{-| If `ref` is the top-left anchor of a merge, the merged range (so the view can span it). -}
mergeAnchorAt : Ref -> Sheet -> Maybe Range
mergeAnchorAt ref (Sheet m) =
    findFirst (\mr -> (Ref.normalize mr).start == ref) m.merges


{-| The merge containing `ref` (anchor or covered), if any. -}
mergeContaining : Ref -> Sheet -> Maybe Range
mergeContaining ref (Sheet m) =
    findFirst (\mr -> Ref.contains mr ref) m.merges


{-| Is `ref` a non-anchor cell of a merge (hidden by the view)? -}
isCovered : Ref -> Sheet -> Bool
isCovered ref sheet =
    case mergeContaining ref sheet of
        Just mr ->
            (Ref.normalize mr).start /= ref

        Nothing ->
            False


rangesOverlap : Range -> Range -> Bool
rangesOverlap a b =
    let
        x =
            Ref.normalize a

        y =
            Ref.normalize b
    in
    x.start.col <= y.end.col && x.end.col >= y.start.col && x.start.row <= y.end.row && x.end.row >= y.start.row


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst pred xs =
    List.head (List.filter pred xs)



-- DATA VALIDATION ------------------------------------------------------------


{-| Attach a validation rule to a range. -}
addValidation : Range -> Validation.Rule -> Sheet -> Sheet
addValidation range rule (Sheet m) =
    Sheet { m | validations = m.validations ++ [ { range = Ref.normalize range, rule = rule } ] }


{-| The validation rule in force at a cell (the first whose range contains it). -}
validationAt : Ref -> Sheet -> Maybe Validation.Rule
validationAt ref (Sheet m) =
    findFirst (\vd -> Ref.contains vd.range ref) m.validations
        |> Maybe.map .rule


{-| Would `input` be accepted in this cell? (True when there is no rule.) -}
validate : Ref -> String -> Sheet -> Bool
validate ref input sheet =
    case validationAt ref sheet of
        Just rule ->
            Validation.check rule (Value.fromString input)

        Nothing ->
            True


{-| Does the cell's *current* value violate its validation rule? (For flagging.) -}
isInvalid : Ref -> Sheet -> Bool
isInvalid ref sheet =
    case validationAt ref sheet of
        Just rule ->
            not (Validation.check rule (valueAt ref sheet))

        Nothing ->
            False


{-| The dropdown choices for a cell, if its validation is a list rule. -}
dropdownAt : Ref -> Sheet -> Maybe (List String)
dropdownAt ref sheet =
    validationAt ref sheet |> Maybe.andThen Validation.options



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
rule layered on top (value-based rules first, then range-aware rank rules), in declaration
order. -}
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

        ranked =
            List.filter (\rr -> rankApplies ref rr sheet) m.rankRules
    in
    List.foldl (\rule acc -> Style.mergeStyle acc rule.style)
        (List.foldl (\rule acc -> Style.mergeStyle acc rule.style) (baseStyleAt ref sheet) applicable)
        ranked


{-| Does a cell qualify under a range-aware rank rule? Examines the whole range's values. -}
rankApplies : Ref -> Style.RankRule -> Sheet -> Bool
rankApplies ref rule sheet =
    if not (Ref.contains rule.range ref) then
        False

    else
        let
            vals =
                List.map (\r -> valueAt r sheet) (Ref.cellsOf rule.range)

            nums =
                List.filterMap numberOfValue vals

            thisVal =
                valueAt ref sheet

            thisText =
                Value.toText thisVal
        in
        case rule.kind of
            Style.TopN k ->
                withNumber thisVal (\x -> not (List.isEmpty nums) && x >= nthLargest k nums)

            Style.BottomN k ->
                withNumber thisVal (\x -> not (List.isEmpty nums) && x <= nthSmallest k nums)

            Style.AboveAverage ->
                withNumber thisVal (\x -> meanGreater x nums)

            Style.BelowAverage ->
                withNumber thisVal (\x -> meanLess x nums)

            Style.Duplicate ->
                thisText /= "" && countOccurrences thisText vals >= 2

            Style.UniqueValue ->
                thisText /= "" && countOccurrences thisText vals == 1


numberOfValue : Value -> Maybe Float
numberOfValue v =
    case v of
        VNumber x ->
            Just x

        _ ->
            Nothing


withNumber : Value -> (Float -> Bool) -> Bool
withNumber v f =
    case v of
        VNumber x ->
            f x

        _ ->
            False


nthLargest : Int -> List Float -> Float
nthLargest k nums =
    let
        descending =
            List.reverse (List.sort nums)
    in
    Maybe.withDefault (Maybe.withDefault 0 (List.minimum nums))
        (List.head (List.drop (min (k - 1) (List.length nums - 1)) descending))


nthSmallest : Int -> List Float -> Float
nthSmallest k nums =
    Maybe.withDefault (Maybe.withDefault 0 (List.maximum nums))
        (List.head (List.drop (min (k - 1) (List.length nums - 1)) (List.sort nums)))


meanGreater : Float -> List Float -> Bool
meanGreater x nums =
    case nums of
        [] ->
            False

        _ ->
            x > List.sum nums / toFloat (List.length nums)


meanLess : Float -> List Float -> Bool
meanLess x nums =
    case nums of
        [] ->
            False

        _ ->
            x < List.sum nums / toFloat (List.length nums)


countOccurrences : String -> List Value -> Int
countOccurrences text vals =
    List.length (List.filter (\v -> Value.toText v == text) vals)


{-| The fully-resolved style for a cell as render-ready classes + inline declarations:
the effective `CellStyle` rendered, plus any data-driven colour-scale/data-bar inline.
The view uses this so it never needs the `CellStyle` record itself. -}
renderedStyle : Ref -> Sheet -> Rendered
renderedStyle ref sheet =
    let
        base =
            Style.render (effectiveStyle ref sheet) (valueAt ref sheet)
    in
    { base | inline = base.inline ++ conditionalInline ref sheet ++ formatColorInline ref sheet }


{-| A colour an in-cell number format asks for (e.g. `[Red]` for negatives), emitted inline. -}
formatColorInline : Ref -> Sheet -> List ( String, String )
formatColorInline ref sheet =
    case formatAt ref sheet of
        Format.Custom code ->
            case Format.colorOf code (valueAt ref sheet) of
                Just color ->
                    [ ( "color", color ) ]

                Nothing ->
                    []

        _ ->
            []


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
