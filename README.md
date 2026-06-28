# elm-spreadsheet

A spreadsheet **logic and view layer** in Elm (built for the [elm-lang](../../) compiler).
It gives you a recalculating cell engine — values, ~170 formula functions, number formats,
conditional styling, structural editing (insert/delete, copy/paste, autofill), multiple
sheets with cross-sheet references, what-if analysis (Goal Seek, data tables), live
**dynamic arrays** that spill (`SORT`/`FILTER`/`SEQUENCE`/`HSTACK`/`LINEST`, the `A1#`
spill operator, `LET`), **functional formulas** (`LAMBDA` + `MAP`/`REDUCE`/`BYROW`, custom
named functions, array broadcasting), **structured table references** (`Sales[Amount]`),
and analytics (pivots, sparklines, charts, icon sets) — plus a keyboard-driven,
class-styled HTML grid to render it. The engine is pure and effect-free, so it is fully
unit-tested without a browser (395 tests).

![demo](docs/screenshot.png)

## Highlights

- **Cell values & formulas.** Numbers, text, booleans, blanks and the standard `#…!`
  errors. A hand-written formula parser with Excel/Sheets semantics: operator precedence
  (`-2^2 = 4`, right-associative `^`), `&` concatenation, `%` postfix, `$` absolute refs,
  ranges (`A1:B5`).
- **~170 functions** across every category — math/trig (`SUM`, `ROUND`, `MOD`, `POWER`,
  `SIN`, `GCD`, …), statistics & forecasting (`AVERAGE`, `MEDIAN`, `PERCENTILE`, `RANK`,
  `CORREL`, `SLOPE`, `INTERCEPT`, `FORECAST`, `GEOMEAN`, …), multi-criteria (`SUMIFS`,
  `COUNTIFS`, `AVERAGEIFS`, `MINIFS`/`MAXIFS`), finance (`PMT`, `FV`, `PV`, `NPER`, `NPV`,
  `IRR`), logical (`IF`, `IFS`, `SWITCH`, …), text (`LEFT`, `MID`, `SUBSTITUTE`, …), lookup
  & dynamic references (`VLOOKUP`, `INDEX`/`MATCH`, `XLOOKUP`, `OFFSET`, `INDIRECT`,
  `ADDRESS`), `SUMPRODUCT`, `SUBTOTAL`, information (`ISNUMBER`, `TYPE`, …) and date/time
  (`DATE`, `EDATE`, `EOMONTH`, `WORKDAY`, `NETWORKDAYS`, `TIME`, `HOUR`, …). Lazy forms
  (`IF`/`IFERROR`) don't evaluate the untaken branch; aggregates propagate errors and ignore
  text in ranges, as Excel does.
- **Multiple sheets.** A `Workbook` of named sheets that reference one another with
  `Data!A1` / `Data!A1:B5` cross-sheet formulas, recomputed to a fixed point so chains
  across sheets settle.
- **Workbook features.** Merge a range into one block (the anchor spans it); attach a
  **note** to a cell; **data validation** (dropdown list, number range, text length,
  not-blank) that flags offending values; **find & replace** across cells; **frozen**
  header rows *and* columns; and one-click **export** to TSV, Markdown, HTML or JSON.
- **Dynamic arrays (live spilling).** A formula whose result is a 2-D block *spills* into
  the grid: `=SORT(A1:A9)`, `=FILTER(data, mask)`, `=UNIQUE(...)`, `=SEQUENCE(3,4)`,
  `=TRANSPOSE(...)`, the stackers `HSTACK`/`VSTACK`/`CHOOSEROWS`/`CHOOSECOLS`/`TAKE`/`DROP`,
  and the regression arrays `LINEST`/`TREND`/`GROWTH`. Spills **re-compute on every recalc**
  (edit a source cell and the block re-spills), refuse to overwrite an occupied cell
  (`#SPILL!`), and are addressable as a whole with the **spill operator** `A1#`
  (`=SUM(A1#)`). `LET(name, value, …, calc)` binds locals inside one formula.
- **Functional formulas.** `LAMBDA(params, body)` plus the higher-order helpers `MAP`,
  `REDUCE`, `SCAN`, `MAKEARRAY`, `BYROW` and `BYCOL` (e.g. `=MAP(A1:A9, LAMBDA(x, x*x))`).
  A lambda can be **named** as a reusable custom function (`Sheet.defineLambda "DISCOUNT"
  "=LAMBDA(p, p*0.9)"` → `=DISCOUNT(B2)`). Operators **broadcast** over arrays, so
  `=A1:A9*2` and `=A1:A9+B1:B9` evaluate elementwise and spill.
- **Structured tables.** Define a table over a range (`Sheet.defineTable`) and reference it
  by column: `Sales[Amount]` (the data column, spills), `Sales[@Qty]` (this row),
  `Sales[#Headers]`, `Sales[#Data]`, `Sales[#Totals]`, `Sales[#All]`.
- **Formula auditing.** `ISFORMULA`/`FORMULATEXT` inspect a cell's formula, `ERROR.TYPE`
  codes an error, and `Sheet.tracePrecedents`/`traceDependents` walk the dependency graph.
- **Analytics.** **Pivot** a range (group-by + sum/count/avg/min/max); range-aware
  **conditional formatting** (top/bottom-N, above/below average, duplicate/unique) and
  **icon sets** (arrows / traffic lights / symbols by threshold); in-cell **sparklines**;
  **charts** (column / bar / pie / line, drawn with pure CSS); and an **auto-filter**.
- **What-if analysis.** **Goal Seek** solves for the input that drives a target cell to a
  value; one- and two-variable **data tables** tabulate a formula across input ranges.
- **Custom number formats.** Multi-section codes (`positive;negative;zero;text`), fractions
  (`# ?/?`), thousands-scaling (trailing commas) and `[Red]`-style colour codes (surfaced as
  an inline cell colour).
- **Formatting.** `General`, `Number`, `Currency`, `Percent`, `Scientific`, `DateTime`
  and raw `Custom` Excel-style format codes (`#,##0.00`, `0.0%`, `yyyy-mm-dd`), shared
  with the `TEXT()` function.
- **Conditional & value styling.** Static cell styles — bold/italic/underline/strike,
  alignment, font family & size, text and fill colour — plus conditional-format **rules**
  (greater-than, between, text-contains, COUNTIF-style criteria, …), two-colour **scales**
  and **data bars**. Styling is expressed as **CSS classes** by default (so a host can
  restyle everything — the demo includes a Solarized-beige theme); only data-driven values
  (colour, font, size, bar widths) are emitted inline. A `with*/toggle*/…Of` API on
  `Style` backs a Word-style formatting toolbar in the demo.
- **Keyboard-driven grid with range selection.** The `View` grid is focusable and navigates
  like Excel/Sheets: arrow keys move the selection, a printable key starts editing the cell,
  **Enter** commits and moves down (**Shift+Enter** up), **Tab** commits and moves right
  (**Shift+Tab** left), **Esc** cancels, **Backspace/Delete** clears. Select a **range** by
  dragging, Shift-clicking or Shift-arrowing; **Ctrl+C/Ctrl+V** copy and paste a block (with
  relative-reference translation) and **Ctrl+Z / Ctrl+Shift+Z** undo and redo. Columns are
  resized by dragging the border on a column header.
- **Absolute & relative references.** `$A$1`, `$A1`, `A$1` are parsed, evaluated, displayed
  and honoured by copy/fill — the basis for spreadsheet-correct reference behaviour.
- **Structural editing.** Insert or delete whole rows and columns and every formula rewrites
  itself: references shift, ranges grow or shrink, and a reference into a deleted cell
  becomes `#REF!`. Conditional-format ranges, colour scales, data bars, named ranges and
  column widths all move with the change.
- **Clipboard & autofill.** Copy/paste with relative-reference translation (`=A1+$B$1`
  pasted a row down becomes `=A2+$B$1`), a verbatim cut/paste move, fill-down/right, and
  numeric/date **series** extrapolation.
- **Named ranges, sort & filter, CSV.** Define a name for a cell or range and use it in any
  formula (`=Price*TaxRate`); sort a data range by a key column (whole rows move) or query
  the rows a filter would keep; import and export rectangular ranges as RFC-4180-style CSV.
- **Sync *and* async recalculation.** `recalcAll`/`recalcFrom` recompute synchronously in
  dependency order (with circular-reference detection → `#CIRC!`). For very large sheets,
  `Spreadsheet.Recalc` slices the same work into per-frame **batches** and computes the
  **visible viewport first**, so the page never freezes.

## Layout

```
src/Spreadsheet/
  Value.elm      cell values, coercions, comparison
  Ref.elm        A1 addressing, ranges
  Ast.elm        formula syntax tree
  Parser.elm     tokenizer + precedence-climbing parser
  Functions.elm  the built-in function library
  Eval.elm       evaluator (operators, lazy/ref-aware forms, LET, LAMBDA, spilling, tables)
  Deps.elm       precedent extraction + topological sort
  Format.elm     number/date formatting + format-code interpreter
  Style.elm      cell styles, conditional rules, colour scales, data bars
  Render.elm     serialize a formula syntax tree back to text
  Refactor.elm   rewrite references for copy/fill and insert/delete row/col
  Validation.elm data-validation rules
  Sheet.elm      the (opaque) sheet model + sync recalc, structural edits,
                 clipboard, autofill, sort/filter, named ranges, merges,
                 notes, validation
  Recalc.elm     async, visible-first incremental recalculation
  Workbook.elm   multiple sheets + cross-sheet references + fix-point recalc
  Csv.elm        CSV import/export
  Export.elm     one-way export (TSV / Markdown / HTML / JSON)
  Find.elm       find & replace across cells
  Pivot.elm      group-by + aggregate (pivot tables)
  Spill.elm      dynamic-array matrix transforms (unique/sort/filter/sequence/transpose)
  Analysis.elm   what-if analysis (Goal Seek, data tables)
  Chart.elm      chart geometry (column/bar/pie/line)
  View.elm       the class-styled HTML grid (+ View.chart)
src/Main.elm     a single-page gallery of ~12 live, editable examples
src/spreadsheet.css   the default stylesheet (all ss-* classes)
test/SpreadsheetTest.elm   395 tests
```

The engine knows nothing about the DOM; `View`/`Main` are the only modules that import
`Html`. `Sheet` is opaque — callers use its functions, never its fields.

## Using the library

```elm
import Spreadsheet.Sheet as Sheet
import Spreadsheet.Ref as Ref

sheet =
    Sheet.empty 100 26
        |> Sheet.setRawMany
            [ ( cell "A1", "10" )
            , ( cell "A2", "20" )
            , ( cell "A3", "=SUM(A1:A2)*2" )
            ]
        |> Sheet.recalcAll

result =
    Sheet.displayString (cell "A3") sheet  -- "60"

cell a =
    Maybe.withDefault { col = 0, row = 0 } (Ref.fromA1 a)
```

To render it, hand a `Spreadsheet.View.Config` (viewport size, selection, edit buffer and
message callbacks) to `Spreadsheet.View.view`.

### Multiple sheets

```elm
import Spreadsheet.Workbook as Workbook

book =
    Workbook.init
        [ ( "Budget", budgetSheet )    -- holds the data
        , ( "Summary", summarySheet )  -- e.g. B2 = "=SUM(Budget!B2:B4)"
        ]
        |> Workbook.recalc             -- settle cross-sheet references to a fixed point

total =
    Workbook.valueAt "Summary" (cell "B2") book
```

### Async recalculation

```elm
( sheet1, state ) =
    Recalc.begin viewport [ changedRef ] sheet0   -- or Recalc.beginAll for a full pass

-- one batch per animation frame:
( sheet2, state2 ) =
    Recalc.step 64 state sheet1
-- …until Recalc.isDone state
```

`begin`/`beginAll` take a `Viewport`; the visible cells (and the precedents they need)
are moved to the front of the dependency-ordered queue, so the on-screen region settles
in the first frame or two while off-screen cells finish in the background.

## Build & test

```bash
ELM=../../elm.sh ./build.sh    # → build/elm-spreadsheet.html  (standalone, CSS inlined)
ELM=../../elm.sh ./test.sh     # → 395 pure-engine tests
```

`build.sh` post-processes the compiler's output to add a viewport meta tag and inline
`src/spreadsheet.css`, so the result is a single self-contained HTML file.

## Notes & simplifications

- Dates use a clean proleptic-Gregorian serial model where `DATE(1900,1,1) = 1`; it omits
  Excel's historical 1900-leap-year bug, so serials differ from Excel by one day after
  February 1900.
- A bare range used where a single value is expected collapses to its top-left cell
  (rather than spilling/array-broadcasting).
- `sortRange` moves cells verbatim and does **not** rewrite references, so it is intended
  for ranges of data; keep formula columns outside the sorted range (the demo sorts the
  data columns and lets the stationary `SUM` column recompute).
- `cutPaste` moves cells verbatim and clears the source, but does not update references
  elsewhere that pointed into the moved block.
- Cross-sheet recalculation is iterated to a fixed point, capped at 25 passes; a genuine
  *cross-sheet* reference cycle stops at the cap with its last values rather than a
  `#CIRC!` (within-sheet cycles are still detected and marked).
- Dynamic arrays **spill live**: a formula whose result is a block writes its cells into a
  separate spill layer that is recomputed (to a fixed point) on every `recalcAll`/`recalcFrom`,
  so editing a source re-spills automatically. The async `Spreadsheet.Recalc` path doesn't
  re-spill mid-stream — spills settle on the next full recalc. (`Spill` + `Sheet.spillInto`
  remain for writing a one-shot array as literals.)
- A `LET`/`LAMBDA` parameter name that happens to look like a cell reference (e.g. `AB12`)
  is parsed as that reference, not a local; use ordinary names.
- Structured references and named-lambda bodies don't contribute dependency-graph edges (the
  evaluator resolves them dynamically), so they rely on a full recalc to refresh — fine for
  the synchronous engine, which recomputes every formula; not tracked by the async path.
- Sparklines are drawn with plain `div`s (no SVG), so they render on every backend; a bar
  or dot-line chart rather than a true polyline.
- `SUBSTITUTE`'s optional instance argument and a few other deep Excel corners are
  documented simplifications.
