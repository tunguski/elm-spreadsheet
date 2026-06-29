#!/usr/bin/env bash
#
# build.sh — compile the elm-spreadsheet demo app to a standalone HTML file.
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed
# to `make` must be absolute (computed here after we cd into the script's own dir). We compile
# with the type checker ON (no --no-check): the whole app — View/SheetDoc/Examples and the
# vendored Workspace — type-checks clean, so the checker is a real gate again.
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
$ELM make "$P/src/Main.elm" --project="$P/elm.json" -o "$P/$OUT/elm-spreadsheet.html"

# The compiler owns the output's <head> (just charset + title), so we post-process it: add a
# viewport meta and inline src/spreadsheet.css as a <style> (the library's styling lives there
# as classes; the page stays a single self-contained HTML file). Idempotent on re-runs.
HTML="$P/$OUT/elm-spreadsheet.html"
TITLE="elm-spreadsheet" SITECSS="$P/assets/site.css" CSSFILE="$P/src/spreadsheet.css" perl -0pi -e '
  if (index($_, q{name="viewport"}) < 0) {
    s#<meta charset="utf-8">#<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">#;
  }
  s#<title>.*?</title>#"<title>".$ENV{TITLE}."</title>"#e;
  if (index($_, q{bootstrap-icons}) < 0) {
    s#</head>#<link rel="stylesheet" href="bootstrap-icons-1.11.3.css"></head>#;
  }
  if (index($_, q{id="wsite-css"}) < 0) {
    open(my $f, "<", $ENV{SITECSS}) or die "no site.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"wsite-css\">".$css."</style></head>"#e;
  }
  if (index($_, q{id="ss-app-css"}) < 0) {
    open(my $f, "<", $ENV{CSSFILE}) or die "no spreadsheet.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"ss-app-css\">".$css."</style></head>"#e;
  }
' "$HTML"

# Bootstrap Icons + the app logo are vendored (no CDN); ship them next to the page.
cp "$P/assets/bootstrap-icons-1.11.3.css" "$P/assets/bootstrap-icons-1.11.3.woff2" "$P/assets/logo.svg" "$P/$OUT/"
echo "Done -> $OUT/elm-spreadsheet.html"
