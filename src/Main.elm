module Main exposing (main)

{-| The elm-spreadsheet showcase — one page, many spreadsheets.

A single `Browser.element` app embeds several independent, editable spreadsheets, ordered
from the simplest possible use to the most involved (functions, number formats,
conditional styling, and finally an async, visible-first recalculation of a few thousand
formulas). Each example carries its own model (sheet, selection, edit buffer) and a short
description of what it demonstrates; double-click any cell to edit it and watch dependents
recompute. A link at the foot of the page opens the test report.

All spreadsheet logic lives in `Spreadsheet.*`; this module only wires examples to the
view and owns the recalculation lifecycle.

-}

import Browser
import Browser.Events
import Html exposing (Html, a, button, div, h1, h2, input, label, option, p, pre, section, select, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Spreadsheet.Format as Format exposing (Format)
import Spreadsheet.Recalc as Recalc
import Spreadsheet.Ref as Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Style as Style
import Spreadsheet.View as View


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = appView
        , subscriptions = subscriptions
        }



-- MODEL ----------------------------------------------------------------------


type alias Model =
    { examples : List Example }


{-| One embedded spreadsheet plus its UI state. -}
type alias Example =
    { id : Int
    , title : String
    , blurb : String
    , sheet : Sheet
    , selected : Ref
    , editing : Maybe ( Ref, String )
    , cols : Int
    , rows : Int
    , async : Bool
    , recalc : Recalc.State
    , status : String
    , toolbar : Bool
    , wrapClass : String
    , css : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { examples =
            [ exValues
            , exOperators
            , exFunctions
            , exFormats
            , exConditional
            , exStyling
            , exFormatting
            , exAsync
            ]
      }
    , Cmd.none
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = Select Int Ref
    | StartEdit Int Ref
    | EditInput Int String
    | EditKey Int String
    | LoadBig Int
    | Frame Float
      -- formatting toolbar (acts on the example's selected cell)
    | FmtBold Int
    | FmtItalic Int
    | FmtUnderline Int
    | FmtStrike Int
    | FmtAlign Int Style.Align
    | FmtColor Int String
    | FmtBackground Int String
    | FmtFont Int String
    | FmtSize Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select id ref ->
            ( mapExample id (\e -> { e | selected = ref, editing = Nothing }) model, Cmd.none )

        StartEdit id ref ->
            ( mapExample id (\e -> { e | selected = ref, editing = Just ( ref, Sheet.rawAt ref e.sheet ) }) model, Cmd.none )

        EditInput id txt ->
            ( mapExample id (\e -> { e | editing = Maybe.map (\( r, _ ) -> ( r, txt )) e.editing }) model, Cmd.none )

        EditKey id keyName ->
            case keyName of
                "Enter" ->
                    ( mapExample id commitEditing model, Cmd.none )

                "Escape" ->
                    ( mapExample id (\e -> { e | editing = Nothing }) model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadBig id ->
            ( mapExample id loadBig model, Cmd.none )

        FmtBold id ->
            ( restyle id Style.toggleBold model, Cmd.none )

        FmtItalic id ->
            ( restyle id Style.toggleItalic model, Cmd.none )

        FmtUnderline id ->
            ( restyle id Style.toggleUnderline model, Cmd.none )

        FmtStrike id ->
            ( restyle id Style.toggleStrike model, Cmd.none )

        FmtAlign id a ->
            ( restyle id (Style.withAlign a) model, Cmd.none )

        FmtColor id hex ->
            ( restyle id (Style.withColor hex) model, Cmd.none )

        FmtBackground id hex ->
            ( restyle id (Style.withBackground hex) model, Cmd.none )

        FmtFont id font ->
            ( restyle id
                (if font == "" then
                    Style.clearFontFamily

                 else
                    Style.withFontFamily font
                )
                model
            , Cmd.none
            )

        FmtSize id raw ->
            ( case String.toInt raw of
                Just n ->
                    restyle id (Style.withFontSize n) model

                Nothing ->
                    model
            , Cmd.none
            )

        Frame _ ->
            ( { model | examples = List.map stepExample model.examples }, Cmd.none )


mapExample : Int -> (Example -> Example) -> Model -> Model
mapExample id f model =
    { model
        | examples =
            List.map
                (\e ->
                    if e.id == id then
                        f e

                    else
                        e
                )
                model.examples
    }


{-| Apply a style transform to an example's currently-selected cell. Styling never
changes values, so no recalculation is needed. -}
restyle : Int -> (Style.CellStyle -> Style.CellStyle) -> Model -> Model
restyle id f model =
    mapExample id
        (\e ->
            { e | sheet = Sheet.setStyle e.selected (f (Sheet.baseStyleAt e.selected e.sheet)) e.sheet }
        )
        model


{-| Commit the in-progress edit and recalculate (sync, or async for the big example). -}
commitEditing : Example -> Example
commitEditing e =
    case e.editing of
        Just ( ref, txt ) ->
            recalcExample [ ref ] { e | editing = Nothing, sheet = Sheet.setRaw ref txt e.sheet }

        Nothing ->
            e


recalcExample : List Ref -> Example -> Example
recalcExample changed e =
    if e.async then
        let
            ( started, state ) =
                Recalc.begin (viewportOf e) changed e.sheet
        in
        { e | sheet = started, recalc = state, status = "Recalculating (async, visible-first)…" }

    else
        { e | sheet = Sheet.recalcFrom changed e.sheet }


stepExample : Example -> Example
stepExample e =
    if e.async && not (Recalc.isDone e.recalc) then
        let
            ( sheet2, state2 ) =
                Recalc.step asyncBatch e.recalc e.sheet

            ( done, total ) =
                Recalc.progress state2
        in
        { e
            | sheet = sheet2
            , recalc = state2
            , status =
                if Recalc.isDone state2 then
                    "Done — recalculated " ++ String.fromInt total ++ " formulas without blocking the page."

                else
                    "Recalculating… " ++ String.fromInt done ++ " / " ++ String.fromInt total ++ " (visible cells first)"
        }

    else
        e


asyncBatch : Int
asyncBatch =
    48


viewportOf : Example -> Recalc.Viewport
viewportOf e =
    { minCol = 0, minRow = 0, maxCol = e.cols - 1, maxRow = e.rows - 1 }



-- SUBSCRIPTIONS --------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.any (\e -> e.async && not (Recalc.isDone e.recalc)) model.examples then
        Browser.Events.onAnimationFrameDelta Frame

    else
        Sub.none



-- EXAMPLES -------------------------------------------------------------------


example : Int -> String -> String -> Int -> Int -> Sheet -> Example
example id title blurb cols rows sheet =
    { id = id
    , title = title
    , blurb = blurb
    , sheet = sheet
    , selected = { col = 0, row = 0 }
    , editing = Nothing
    , cols = cols
    , rows = rows
    , async = False
    , recalc = Recalc.idle
    , status = ""
    , toolbar = False
    , wrapClass = ""
    , css = Nothing
    }


build : Int -> Int -> List ( String, String ) -> Sheet
build rows cols cells =
    Sheet.empty rows cols
        |> Sheet.setRawMany (List.map (\( a, r ) -> ( ref a, r )) cells)
        |> Sheet.recalcAll


{-| 1 — the absolute basics: literals and one formula. -}
exValues : Example
exValues =
    example 1
        "Values & your first formula"
        "Cells hold numbers or text. Begin an entry with “=” to make it a formula. Here C2 is =A2*B2 and C4 sums the column with =SUM(C2:C3). Double-click A2 or B2, change a value, press Enter — the totals recompute instantly."
        4
        5
        (build 10 6
            [ ( "A1", "Item" ), ( "B1", "Price" ), ( "C1", "Subtotal" )
            , ( "A2", "Widget" ), ( "B2", "20" ), ( "C2", "=B2*3" )
            , ( "A3", "Gadget" ), ( "B3", "15" ), ( "C3", "=B3*5" )
            , ( "A4", "Total" ), ( "C4", "=SUM(C2:C3)" )
            ]
        )


{-| 2 — operators, precedence, ranges, percent and concatenation. -}
exOperators : Example
exOperators =
    example 2
        "Operators & ranges"
        "Full operator precedence (×÷ before +−, right-associative ^, a tighter unary minus so −2^2 = 4), the “&” text join and a trailing “%”. B-column formulas read the whole range A1:A5; edit any A cell to see them all react."
        4
        6
        (build 10 6
            [ ( "A1", "5" ), ( "A2", "8" ), ( "A3", "3" ), ( "A4", "10" ), ( "A5", "4" )
            , ( "C1", "sum" ), ( "B1", "=SUM(A1:A5)" )
            , ( "C2", "average" ), ( "B2", "=AVERAGE(A1:A5)" )
            , ( "C3", "max − min" ), ( "B3", "=MAX(A1:A5)-MIN(A1:A5)" )
            , ( "C4", "A1×2 + 50%" ), ( "B4", "=A1*2+50%" )
            , ( "C5", "joined" ), ( "B5", "=\"sum is \"&SUM(A1:A5)" )
            ]
        )


{-| 3 — a cross-section of the function library: logic, lookup, text, criteria. -}
exFunctions : Example
exFunctions =
    example 3
        "Functions: logic, lookup & text"
        "A grading table driven by nested IF, plus VLOOKUP and INDEX/MATCH to pull values out by key, COUNTIF over a criterion, and text functions. The library ships ~100 functions across math, stats, logic, text, lookup, info and date categories."
        7
        6
        (build 10 8
            [ ( "A1", "Name" ), ( "B1", "Score" ), ( "C1", "Grade" )
            , ( "A2", "Ann" ), ( "B2", "92" ), ( "C2", "=IF(B2>=90,\"A\",IF(B2>=80,\"B\",\"C\"))" )
            , ( "A3", "Bob" ), ( "B3", "77" ), ( "C3", "=IF(B3>=90,\"A\",IF(B3>=80,\"B\",\"C\"))" )
            , ( "A4", "Cy" ), ( "B4", "85" ), ( "C4", "=IF(B4>=90,\"A\",IF(B4>=80,\"B\",\"C\"))" )
            , ( "E1", "Bob’s score" ), ( "F1", "=VLOOKUP(\"Bob\",A2:B4,2,FALSE)" )
            , ( "E2", "Top scorer" ), ( "F2", "=INDEX(A2:A4,MATCH(MAX(B2:B4),B2:B4,0))" )
            , ( "E3", "# of A grades" ), ( "F3", "=COUNTIF(C2:C4,\"A\")" )
            , ( "E4", "Loudly" ), ( "F4", "=UPPER(A2)&\" WINS\"" )
            ]
        )


{-| 4 — number formats: currency, percent, thousands, dates, scientific. -}
exFormats : Example
exFormats =
    example 4
        "Number formats"
        "The same underlying number renders many ways — currency, percentage, thousands-grouped, a date from a serial value, and scientific notation. Formats are display-only; the stored value (and any formula reading it) is unchanged. Alignment follows the value type via a CSS class."
        3
        7
        (build 10 4
            [ ( "A1", "Currency" ), ( "B1", "1234.5" )
            , ( "A2", "Percent" ), ( "B2", "0.085" )
            , ( "A3", "Thousands" ), ( "B3", "12345678" )
            , ( "A4", "Date" ), ( "B4", "=DATE(2026,6,27)" )
            , ( "A5", "Scientific" ), ( "B5", "0.000123" )
            , ( "A6", "Plain" ), ( "B6", "=22/7" )
            ]
            |> Sheet.setFormat (ref "B1") (Format.Currency "$" 2)
            |> Sheet.setFormat (ref "B2") (Format.Percent 1)
            |> Sheet.setFormat (ref "B3") (Format.Number 0 True)
            |> Sheet.setFormat (ref "B4") (Format.DateTime "yyyy-mm-dd")
            |> Sheet.setFormat (ref "B5") (Format.Scientific 2)
        )


{-| 5 — conditional formatting: rules, a colour scale and a data bar. -}
exConditional : Example
exConditional =
    example 5
        "Conditional formatting, colour scales & data bars"
        "A quarterly sales grid. Negative growth turns red and strong growth green & bold (class-based rules). The quarter columns carry a low→high colour scale, and the Total column a proportional data bar. Edit a number and the colours re-derive across the whole range."
        8
        7
        (conditionalSheet ())


conditionalSheet : () -> Sheet
conditionalSheet _ =
    build 12 12
        [ ( "A1", "Region" ), ( "B1", "Q1" ), ( "C1", "Q2" ), ( "D1", "Q3" ), ( "E1", "Q4" ), ( "F1", "Total" ), ( "G1", "Growth" )
        , ( "A2", "North" ), ( "B2", "1200" ), ( "C2", "1500" ), ( "D2", "1700" ), ( "E2", "1600" ), ( "F2", "=SUM(B2:E2)" ), ( "G2", "=(E2-B2)/B2" )
        , ( "A3", "South" ), ( "B3", "900" ), ( "C3", "1100" ), ( "D3", "1000" ), ( "E3", "800" ), ( "F3", "=SUM(B3:E3)" ), ( "G3", "=(E3-B3)/B3" )
        , ( "A4", "East" ), ( "B4", "1400" ), ( "C4", "1350" ), ( "D4", "1500" ), ( "E4", "1800" ), ( "F4", "=SUM(B4:E4)" ), ( "G4", "=(E4-B4)/B4" )
        , ( "A5", "West" ), ( "B5", "1100" ), ( "C5", "800" ), ( "D5", "1250" ), ( "E5", "1450" ), ( "F5", "=SUM(B5:E5)" ), ( "G5", "=(E5-B5)/B5" )
        , ( "A6", "Total" ), ( "B6", "=SUM(B2:B5)" ), ( "C6", "=SUM(C2:C5)" ), ( "D6", "=SUM(D2:D5)" ), ( "E6", "=SUM(E2:E5)" ), ( "F6", "=SUM(F2:F5)" )
        ]
        |> withFormat (cells "B2" "F6") (Format.Number 0 True)
        |> withFormat (cells "G2" "G5") (Format.Percent 1)
        |> Sheet.addConditional
            { range = rangeOf "G2" "G5", condition = Style.LessThan 0, style = classStyle "ss-neg" }
        |> Sheet.addConditional
            { range = rangeOf "G2" "G5", condition = Style.GreaterEqual 0.2, style = classStyle "ss-pos" }
        |> Sheet.addColorScale { range = rangeOf "B2" "E5", low = "#fde0dc", high = "#1e8e3e" }
        |> Sheet.addDataBar { range = rangeOf "F2" "F5", color = "#c6dafc" }
        |> Sheet.recalcAll


{-| 6 — a custom CSS theme (Solarized beige), scoped to one grid, with its stylesheet shown. -}
exStyling : Example
exStyling =
    let
        e =
            example 6
                "Custom CSS theme"
                "The grid is plain markup with semantic ss-* classes, so a host page can restyle it entirely with its own CSS — no Elm change. Here a Solarized-beige theme is scoped to one container. The exact stylesheet applied is shown below the sheet."
                4
                6
                styledSheet
    in
    { e | wrapClass = "theme-solarized", css = Just solarizedCss }


styledSheet : Sheet
styledSheet =
    build 10 6
        [ ( "A1", "Roast" ), ( "B1", "Origin" ), ( "C1", "Cup" )
        , ( "A2", "Espresso" ), ( "B2", "Brazil" ), ( "C2", "3.20" )
        , ( "A3", "Pour-over" ), ( "B3", "Ethiopia" ), ( "C3", "4.50" )
        , ( "A4", "Cold brew" ), ( "B4", "Colombia" ), ( "C4", "4.00" )
        , ( "A5", "Average" ), ( "C5", "=AVERAGE(C2:C4)" )
        ]
        |> withFormat (cells "C2" "C5") (Format.Currency "$" 2)
        |> withStyle (cells "A1" "C1") (\s -> Style.withColor "#b58900" { s | bold = True })
        |> Sheet.recalcAll


solarizedCss : String
solarizedCss =
    """/* Solarized-beige theme, scoped to one container — overrides the ss-* classes. */
.theme-solarized .ss-table  { font-family: 'Iowan Old Style', Georgia, serif; }
.theme-solarized .ss-cell   { background: #fdf6e3; color: #657b83; border-color: #e9e1c8; }
.theme-solarized .ss-corner,
.theme-solarized .ss-col-header,
.theme-solarized .ss-row-header {
    background: #eee8d5; color: #586e75; border-color: #ded7bf;
}
.theme-solarized .ss-cell.ss-selected {
    outline-color: #268bd2; background: #f3ecd6;
}
.theme-solarized .ss-cell-input { background: #fdf6e3; color: #586e75; }"""


{-| 7 — a live formatting toolbar (font, size, bold/italic/underline/strike, colours, align). -}
exFormatting : Example
exFormatting =
    let
        e =
            example 7
                "Rich text formatting (toolbar)"
                "Click a cell, then use the toolbar to set the font, size, bold / italic / underline / strikethrough, text and fill colour, and alignment — the basics from Excel or Sheets. Structural styles become CSS classes; font, size and colours are data-driven inline. A few cells are pre-styled to start."
                5
                7
                formattingSheet
    in
    { e | toolbar = True, selected = ref "A1" }


formattingSheet : Sheet
formattingSheet =
    build 10 6
        [ ( "A1", "Quarterly Report" )
        , ( "A2", "Draft — confidential" )
        , ( "A4", "Revenue" ), ( "B4", "125000" )
        , ( "A5", "Growth" ), ( "B5", "0.18" )
        , ( "A7", "Edit me, then format!" )
        ]
        |> withStyle [ ref "A1" ] (\s -> Style.withFontSize 20 (Style.withColor "#1a73e8" { s | bold = True }))
        |> withStyle [ ref "A2" ] (\s -> Style.withColor "#cb4b16" { s | italic = True })
        |> Sheet.setFormat (ref "B4") (Format.Currency "$" 0)
        |> Sheet.setFormat (ref "B5") (Format.Percent 1)
        |> withStyle [ ref "B4" ] (Style.withBackground "#fff3cd")
        |> Sheet.recalcAll


{-| 8 — async, visible-first recalculation of a large sheet. -}
exAsync : Example
exAsync =
    { id = 8
    , title = "Big sheets without freezing (async)"
    , blurb = "Click “Load” to fill ~2,400 chained formulas (a running sum and two derived columns down 800 rows). Instead of one blocking pass, the engine recalculates in small batches across animation frames and does the on-screen rows first, so the page stays responsive and the visible region settles immediately."
    , sheet =
        build 820 8
            [ ( "A1", "Row" ), ( "B1", "×3" ), ( "C1", "Running Σ" ), ( "D1", "B+C" )
            , ( "A2", "Press “Load” above to populate this sheet." )
            ]
    , selected = { col = 0, row = 0 }
    , editing = Nothing
    , cols = 8
    , rows = 20
    , async = True
    , recalc = Recalc.idle
    , status = "Idle — nothing loaded yet."
    , toolbar = False
    , wrapClass = ""
    , css = Nothing
    }


bigRows : Int
bigRows =
    800


loadBig : Example -> Example
loadBig e =
    let
        edits =
            List.concatMap
                (\i ->
                    let
                        n =
                            String.fromInt (i + 1)
                    in
                    [ ( { col = 0, row = i }, String.fromInt (i + 1) )
                    , ( { col = 1, row = i }, "=A" ++ n ++ "*3" )
                    , ( { col = 2, row = i }
                      , if i == 0 then
                            "=A1"

                        else
                            "=C" ++ String.fromInt i ++ "+A" ++ n
                      )
                    , ( { col = 3, row = i }, "=B" ++ n ++ "+C" ++ n )
                    ]
                )
                (List.range 0 (bigRows - 1))

        loaded =
            Sheet.setRawMany edits e.sheet

        ( started, state ) =
            Recalc.beginAll (viewportOf e) loaded
    in
    { e | sheet = started, recalc = state, status = "Recalculating (async, visible-first)…" }



-- VIEW -----------------------------------------------------------------------


appView : Model -> Html Msg
appView model =
    div [ HA.class "page" ]
        [ header
        , div [ HA.class "gallery" ] (List.map exampleView model.examples)
        , footer
        ]


header : Html Msg
header =
    div [ HA.class "page-head" ]
        [ h1 [] [ text "elm-spreadsheet" ]
        , p [ HA.class "page-lead" ]
            [ text "A spreadsheet logic + view layer in Elm — values, ~100 formula functions, number formats, conditional styling, and sync/async recalculation. Every example below is a live, editable spreadsheet (double-click a cell to edit). They build up from the simplest use to the most involved." ]
        ]


exampleView : Example -> Html Msg
exampleView e =
    section [ HA.class "ex" ]
        (List.concat
            [ [ div [ HA.class "ex-head" ]
                    [ span [ HA.class "ex-num" ] [ text (String.fromInt e.id) ]
                    , h2 [ HA.class "ex-title" ] [ text e.title ]
                    ]
              , p [ HA.class "ex-blurb" ] [ text e.blurb ]
              ]
            , if e.toolbar then
                [ formattingToolbar e ]

              else
                []
            , [ asyncControls e ]
            , styleNode e.css
            , [ div [ HA.class (gridClass e) ] [ View.view (gridConfig e) e.sheet ] ]
            , cssPanel e.css
            ]
        )


gridClass : Example -> String
gridClass e =
    if e.wrapClass == "" then
        "ex-grid"

    else
        "ex-grid " ++ e.wrapClass


{-| Inject a scoped stylesheet for an example (used by the custom-theme example). The
same string is shown to the reader by `cssPanel`, so what's applied is exactly what's
displayed. -}
styleNode : Maybe String -> List (Html Msg)
styleNode maybeCss =
    case maybeCss of
        Just css ->
            [ Html.node "style" [] [ text css ] ]

        Nothing ->
            []


cssPanel : Maybe String -> List (Html Msg)
cssPanel maybeCss =
    case maybeCss of
        Just css ->
            [ div [ HA.class "css-panel" ]
                [ div [ HA.class "css-cap" ] [ text "The CSS applied to this grid:" ]
                , pre [ HA.class "code" ] [ text css ]
                ]
            ]

        Nothing ->
            []


{-| The rich-text formatting toolbar, acting on the example's selected cell. Active
states are read back from that cell's current style. -}
formattingToolbar : Example -> Html Msg
formattingToolbar e =
    let
        cur =
            Sheet.baseStyleAt e.selected e.sheet
    in
    div [ HA.class "fmt-toolbar" ]
        [ span [ HA.class "fmt-cell" ] [ text (Ref.toA1 e.selected) ]
        , let
            curFont =
                Maybe.withDefault "" (Style.fontFamilyOf cur)
          in
          select [ HA.class "fsel", HE.onInput (FmtFont e.id) ]
            (List.map (\( v, lbl ) -> option [ HA.value v, HA.selected (v == curFont) ] [ text lbl ]) fontOptions)
        , let
            curSize =
                Maybe.withDefault 13 (Style.fontSizeOf cur)
          in
          select [ HA.class "fsel fsel-sm", HE.onInput (FmtSize e.id) ]
            (List.map (\n -> option [ HA.value (String.fromInt n), HA.selected (n == curSize) ] [ text (String.fromInt n) ]) sizeOptions)
        , fmtBtn (Style.isBold cur) (FmtBold e.id) "fmt-b" "B"
        , fmtBtn (Style.isItalic cur) (FmtItalic e.id) "fmt-i" "I"
        , fmtBtn (Style.isUnderline cur) (FmtUnderline e.id) "fmt-u" "U"
        , fmtBtn (Style.isStrike cur) (FmtStrike e.id) "fmt-s" "S"
        , fmtBtn (Style.alignOf cur == Just Style.AlignLeft) (FmtAlign e.id Style.AlignLeft) "" "⇤"
        , fmtBtn (Style.alignOf cur == Just Style.AlignCenter) (FmtAlign e.id Style.AlignCenter) "" "↔"
        , fmtBtn (Style.alignOf cur == Just Style.AlignRight) (FmtAlign e.id Style.AlignRight) "" "⇥"
        , label [ HA.class "fcolor" ]
            [ span [ HA.class "fcolor-lbl" ] [ text "A" ]
            , input [ HA.type_ "color", HA.value (Maybe.withDefault "#202124" (Style.colorOf cur)), HE.onInput (FmtColor e.id) ] []
            ]
        , label [ HA.class "fcolor" ]
            [ span [ HA.class "fcolor-lbl" ] [ text "▉" ]
            , input [ HA.type_ "color", HA.value (Maybe.withDefault "#ffffff" (Style.backgroundOf cur)), HE.onInput (FmtBackground e.id) ] []
            ]
        ]


fmtBtn : Bool -> Msg -> String -> String -> Html Msg
fmtBtn active msg extra glyph =
    button
        [ HA.class
            (String.join " "
                (List.filter (\c -> c /= "")
                    [ "fbtn"
                    , extra
                    , if active then
                        "fbtn-on"

                      else
                        ""
                    ]
                )
            )
        , HE.onClick msg
        ]
        [ text glyph ]


fontOptions : List ( String, String )
fontOptions =
    [ ( "", "Default" )
    , ( "-apple-system, 'Segoe UI', Roboto, sans-serif", "Sans-serif" )
    , ( "Georgia, 'Times New Roman', serif", "Serif" )
    , ( "'Courier New', ui-monospace, monospace", "Monospace" )
    , ( "'Comic Sans MS', 'Segoe Print', cursive", "Comic" )
    ]


sizeOptions : List Int
sizeOptions =
    [ 10, 11, 12, 13, 14, 16, 18, 20, 24 ]


asyncControls : Example -> Html Msg
asyncControls e =
    if e.async then
        let
            ( done, total ) =
                Recalc.progress e.recalc

            pct =
                if total == 0 then
                    0

                else
                    done * 100 // total
        in
        div [ HA.class "ex-async" ]
            [ button [ HA.class "btn", HE.onClick (LoadBig e.id) ] [ text "Load ≈2,400 formulas" ]
            , div [ HA.class "progress" ]
                [ div [ HA.class "progress-fill", HA.style "width" (String.fromInt pct ++ "%") ] [] ]
            , span [ HA.class "ex-status" ] [ text e.status ]
            ]

    else
        text ""


gridConfig : Example -> View.Config Msg
gridConfig e =
    { viewCols = e.cols
    , viewRows = e.rows
    , selected = Just e.selected
    , editing = e.editing
    , onSelect = Select e.id
    , onStartEdit = StartEdit e.id
    , onEditInput = EditInput e.id
    , onEditKey = EditKey e.id
    }


footer : Html Msg
footer =
    div [ HA.class "page-foot" ]
        [ p []
            [ text "The whole engine is pure and effect-free, so it's covered by a headless test suite. " ]
        , a [ HA.class "tests-link", HA.href "tests.html" ]
            [ text "View the test report →" ]
        , p [ HA.class "page-foot-meta" ]
            [ text "Built with the "
            , a [ HA.href "https://github.com/tunguski/elm-lang", HA.class "inline-link" ] [ text "elm-lang" ]
            , text " compiler · "
            , a [ HA.href "https://github.com/tunguski/elm-spreadsheet", HA.class "inline-link" ] [ text "source on GitHub" ]
            ]
        ]



-- HELPERS --------------------------------------------------------------------


ref : String -> Ref
ref a1 =
    Maybe.withDefault { col = 0, row = 0 } (Ref.fromA1 a1)


rangeOf : String -> String -> Ref.Range
rangeOf a b =
    { start = ref a, end = ref b }


cells : String -> String -> List Ref
cells a b =
    Ref.cellsOf (rangeOf a b)


withFormat : List Ref -> Format -> Sheet -> Sheet
withFormat refs fmt sheet =
    List.foldl (\r acc -> Sheet.setFormat r fmt acc) sheet refs


withStyle : List Ref -> (Style.CellStyle -> Style.CellStyle) -> Sheet -> Sheet
withStyle refs f sheet =
    List.foldl (\r acc -> Sheet.setStyle r (f (Sheet.baseStyleAt r acc)) acc) sheet refs


classStyle : String -> Style.CellStyle
classStyle cls =
    let
        base =
            Style.emptyStyle
    in
    { base | classes = [ cls ], bold = True }
