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
import Html exposing (Html, a, button, div, h1, h2, p, section, span, text)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { examples =
            [ exValues
            , exOperators
            , exFunctions
            , exFormats
            , exConditional
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


{-| 6 — async, visible-first recalculation of a large sheet. -}
exAsync : Example
exAsync =
    { id = 6
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
    in
    recalcExample [] { e | sheet = loaded }



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
        [ div [ HA.class "ex-head" ]
            [ span [ HA.class "ex-num" ] [ text (String.fromInt e.id) ]
            , h2 [ HA.class "ex-title" ] [ text e.title ]
            ]
        , p [ HA.class "ex-blurb" ] [ text e.blurb ]
        , asyncControls e
        , div [ HA.class "ex-grid" ] [ View.view (gridConfig e) e.sheet ]
        ]


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


classStyle : String -> Style.CellStyle
classStyle cls =
    let
        base =
            Style.emptyStyle
    in
    { base | classes = [ cls ], bold = True }
