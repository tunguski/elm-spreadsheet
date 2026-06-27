#!/usr/bin/env bash
#
# test.sh — run the elm-spreadsheet headless test suite (pure engine: values, parser,
# functions, formatting, styling, dependency graph, sync + async recalculation).
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed
# to the runner must be absolute (computed here after we cd into the script's own dir).
#
#   ELM=../../elm.sh ./test.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
P="$(pwd)"

$ELM test "$P/test/SpreadsheetTest.elm" \
  "$P/src/Spreadsheet/Value.elm" "$P/src/Spreadsheet/Ref.elm" "$P/src/Spreadsheet/Ast.elm" \
  "$P/src/Spreadsheet/Parser.elm" "$P/src/Spreadsheet/Functions.elm" "$P/src/Spreadsheet/Format.elm" \
  "$P/src/Spreadsheet/Eval.elm" "$P/src/Spreadsheet/Deps.elm" "$P/src/Spreadsheet/Style.elm" \
  "$P/src/Spreadsheet/Render.elm" "$P/src/Spreadsheet/Refactor.elm" "$P/src/Spreadsheet/Validation.elm" \
  "$P/src/Spreadsheet/Sheet.elm" "$P/src/Spreadsheet/Recalc.elm" "$P/src/Spreadsheet/Csv.elm" \
  "$P/src/Spreadsheet/Export.elm" "$P/src/Spreadsheet/Find.elm"
