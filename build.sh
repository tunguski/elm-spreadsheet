#!/usr/bin/env bash
#
# build.sh — compile the elm-spreadsheet demo app to a standalone HTML file.
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed
# to `make` must be absolute (computed here after we cd into the script's own dir). Like the
# other elm-lang example apps we compile with --no-check.
#
#   ELM=../../elm.sh ./build.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
OUT="build"
P="$(pwd)"

mkdir -p "$OUT"
echo "Compiling elm-spreadsheet with: $ELM"
$ELM make "$P/src/Main.elm" --project="$P/elm.json" -o "$P/$OUT/elm-spreadsheet.html" --no-check

# The compiler owns the output's <head> (just charset + title), so we post-process it: add a
# viewport meta and inline src/spreadsheet.css as a <style> (the library's styling lives there
# as classes; the page stays a single self-contained HTML file). Idempotent on re-runs.
HTML="$P/$OUT/elm-spreadsheet.html"
CSSFILE="$P/src/spreadsheet.css" perl -0pi -e '
  if (index($_, q{name="viewport"}) < 0) {
    s#<meta charset="utf-8">#<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">#;
  }
  if (index($_, q{id="ss-app-css"}) < 0) {
    open(my $f, "<", $ENV{CSSFILE}) or die "no spreadsheet.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"ss-app-css\">".$css."</style></head>"#e;
  }
' "$HTML"
echo "Done -> $OUT/elm-spreadsheet.html"
