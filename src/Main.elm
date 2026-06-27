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
import Browser.Dom
import Browser.Events
import Html exposing (Html, a, button, div, h1, h2, input, label, option, p, pre, section, select, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import Spreadsheet.Export as Export
import Spreadsheet.Find as Find
import Spreadsheet.Format as Format exposing (Format)
import Spreadsheet.Recalc as Recalc
import Spreadsheet.Ref as Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Style as Style
import Spreadsheet.Validation as Validation
import Spreadsheet.View as View
import Task


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
    { examples : List Example
    , drag : Maybe Drag
    , selecting : Maybe Int
    }


{-| An in-progress column resize: which example/column, and the pointer/width it started
at so each mouse move computes an absolute new width. -}
type alias Drag =
    { exampleId : Int
    , col : Int
    , startX : Float
    , startW : Int
    }


{-| One embedded spreadsheet plus its UI state. -}
type alias Example =
    { id : Int
    , title : String
    , blurb : String
    , sheet : Sheet
    , selected : Ref
    , anchor : Ref
    , clip : Maybe Ref.Range
    , past : List Sheet
    , future : List Sheet
    , editing : Maybe ( Ref, String )
    , cols : Int
    , rows : Int
    , totalRows : Int
    , firstRow : Int
    , async : Bool
    , recalc : Recalc.State
    , status : String
    , toolbar : Bool
    , editTools : Bool
    , workbench : Bool
    , frozenCols : Int
    , findText : String
    , replaceText : String
    , exportPanel : String
    , dataRange : Ref.Range
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
            , exEdit
            , exWorkbook
            , exAsync
            ]
      , drag = Nothing
      , selecting = Nothing
      }
    , Cmd.none
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = CellDown Int Ref Bool
    | CellEnter Int Ref
    | SelectUp
    | Undo Int
    | Redo Int
    | CopySel Int
    | PasteSel Int
    | FillSel Int
    | StartEdit Int Ref
    | EditInput Int String
    | NavKey Int View.KeyEvent
    | EditKey Int View.KeyEvent
    | ResizeStart Int Int Float
    | ResizeMove Float
    | ResizeEnd
    | ScrollGrid Int Int
    | NoOp
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
      -- structural-edit toolbar (acts on the example's selected cell)
    | InsertRow Int
    | DeleteRow Int
    | InsertCol Int
    | DeleteCol Int
    | SortCol Int Bool
      -- workbench: validation dropdown, find/replace, export
    | Pick Int Ref String
    | FindInput Int String
    | ReplaceInput Int String
    | FindNext Int
    | ReplaceAllMsg Int
    | ExportAs Int String
    | CloseExport Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellDown id ref shift ->
            -- Mousedown commits any pending edit, sets/extends the selection and starts a
            -- drag-select; the grid takes keyboard focus so navigation continues.
            ( { model | selecting = Just id }
                |> mapExample id (\e -> selectCell ref shift (commitPending e))
            , focusGrid id
            )

        CellEnter id ref ->
            -- Extend the selection to the hovered cell while a drag is in progress.
            ( if model.selecting == Just id then
                mapExample id (\e -> { e | selected = ref }) model

              else
                model
            , Cmd.none
            )

        SelectUp ->
            ( { model | selecting = Nothing }, Cmd.none )

        Undo id ->
            ( mapExample id undo model, focusGrid id )

        Redo id ->
            ( mapExample id redo model, focusGrid id )

        CopySel id ->
            ( mapExample id copySel model, focusGrid id )

        PasteSel id ->
            ( mapExample id pasteSel model, focusGrid id )

        FillSel id ->
            ( mapExample id fillSel model, focusGrid id )

        StartEdit id ref ->
            ( mapExample id (\e -> { e | selected = ref, editing = Just ( ref, Sheet.rawAt ref e.sheet ) }) model
            , focusEdit id
            )

        EditInput id txt ->
            ( mapExample id (\e -> { e | editing = Maybe.map (\( r, _ ) -> ( r, txt )) e.editing }) model, Cmd.none )

        NavKey id ke ->
            navKey id ke model

        EditKey id ke ->
            editKey id ke model

        ResizeStart id col x ->
            ( { model | drag = Just { exampleId = id, col = col, startX = x, startW = colWidthOf id col model } }, Cmd.none )

        ResizeMove x ->
            ( applyDrag x model, Cmd.none )

        ResizeEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        ScrollGrid id scrollTop ->
            ( mapExample id
                (\e -> { e | firstRow = clamp 0 (max 0 (e.totalRows - e.rows)) (scrollTop // View.rowHeight) })
                model
            , Cmd.none
            )

        NoOp ->
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

        InsertRow id ->
            ( mapExample id (structuralEdit (\e -> Sheet.insertRows e.selected.row 1 e.sheet)) model, focusGrid id )

        DeleteRow id ->
            ( mapExample id (structuralEdit (\e -> Sheet.deleteRows e.selected.row 1 e.sheet)) model, focusGrid id )

        InsertCol id ->
            ( mapExample id (structuralEdit (\e -> Sheet.insertCols e.selected.col 1 e.sheet)) model, focusGrid id )

        DeleteCol id ->
            ( mapExample id (structuralEdit (\e -> Sheet.deleteCols e.selected.col 1 e.sheet)) model, focusGrid id )

        SortCol id ascending ->
            ( mapExample id (structuralEdit (\e -> Sheet.sortRange e.dataRange e.selected.col ascending e.sheet)) model, focusGrid id )

        Pick id ref value ->
            ( mapExample id
                (\e ->
                    let
                        e1 =
                            record e
                    in
                    recalcExample [ ref ] { e1 | editing = Nothing, sheet = Sheet.setRaw ref value e1.sheet }
                )
                model
            , focusGrid id
            )

        FindInput id t ->
            ( mapExample id (\e -> moveToFirstHit { e | findText = t }) model, Cmd.none )

        ReplaceInput id t ->
            ( mapExample id (\e -> { e | replaceText = t }) model, Cmd.none )

        FindNext id ->
            ( mapExample id moveToNextHit model, focusGrid id )

        ReplaceAllMsg id ->
            ( mapExample id replaceAllInExample model, Cmd.none )

        ExportAs id fmt ->
            ( mapExample id (\e -> { e | exportPanel = exportAs fmt e.dataRange e.sheet }) model, Cmd.none )

        CloseExport id ->
            ( mapExample id (\e -> { e | exportPanel = "" }) model, Cmd.none )

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


{-| The current selection block: the rectangle from the anchor to the active cell. -}
selectionOf : Example -> Ref.Range
selectionOf e =
    Ref.normalize { start = e.anchor, end = e.selected }



-- WORKBENCH (find / replace / export) ----------------------------------------


{-| The find query for an example's current search box. -}
findQuery : Example -> Find.Query
findQuery e =
    { text = e.findText, matchCase = False, wholeCell = False, inFormulas = False }


{-| The cells matching the current search (empty when the box is empty). -}
findHits : Example -> List Ref
findHits e =
    if e.findText == "" then
        []

    else
        Find.findAll (findQuery e) e.sheet


moveToFirstHit : Example -> Example
moveToFirstHit e =
    case findHits e of
        first :: _ ->
            { e | selected = first, anchor = first }

        [] ->
            e


moveToNextHit : Example -> Example
moveToNextHit e =
    let
        hits =
            findHits e
    in
    case hits of
        [] ->
            e

        first :: _ ->
            let
                next =
                    hits
                        |> List.filter (\h -> keyOrder h > keyOrder e.selected)
                        |> List.head
                        |> Maybe.withDefault first
            in
            { e | selected = next, anchor = next }


keyOrder : Ref -> ( Int, Int )
keyOrder ref =
    ( ref.row, ref.col )


replaceAllInExample : Example -> Example
replaceAllInExample e =
    if e.findText == "" then
        e

    else
        let
            e1 =
                record e
        in
        { e1 | sheet = Find.replaceAll (findQuery e1) e.replaceText e1.sheet }


exportAs : String -> Ref.Range -> Sheet -> String
exportAs fmt range sheet =
    case fmt of
        "tsv" ->
            Export.tsv range sheet

        "md" ->
            Export.markdown range sheet

        "html" ->
            Export.html range sheet

        "json" ->
            Export.json range sheet

        _ ->
            ""


{-| Point/extend the selection. Shift keeps the anchor (growing the block); a plain click
collapses it to the one cell. -}
selectCell : Ref -> Bool -> Example -> Example
selectCell ref shift e =
    if shift then
        { e | selected = ref }

    else
        { e | selected = ref, anchor = ref }


{-| Snapshot the sheet onto the undo stack (and drop the redo stack) before a mutation. -}
record : Example -> Example
record e =
    { e | past = List.take historyCap (e.sheet :: e.past), future = [] }


historyCap : Int
historyCap =
    100


undo : Example -> Example
undo e =
    case e.past of
        prev :: rest ->
            clampSel { e | sheet = prev, past = rest, future = e.sheet :: e.future, editing = Nothing }

        [] ->
            e


redo : Example -> Example
redo e =
    case e.future of
        next :: rest ->
            clampSel { e | sheet = next, future = rest, past = e.sheet :: e.past, editing = Nothing }

        [] ->
            e


{-| Keep the active cell and anchor inside the sheet's dimensions (they can fall outside
after an undo that changes the grid size). -}
clampSel : Example -> Example
clampSel e =
    let
        ( rows, cols ) =
            Sheet.dims e.sheet

        clampRef r =
            { col = clamp 0 (cols - 1) r.col, row = clamp 0 (rows - 1) r.row }
    in
    { e | selected = clampRef e.selected, anchor = clampRef e.anchor }


{-| Copy the current selection block to the example's clipboard. -}
copySel : Example -> Example
copySel e =
    { e | clip = Just (selectionOf e) }


{-| Paste the clipboard block with its top-left at the active cell (relative references
translate, `$`-absolute ones stay), recording an undo step. -}
pasteSel : Example -> Example
pasteSel e =
    case e.clip of
        Just src ->
            let
                e1 =
                    record e

                dest =
                    { start = e1.selected
                    , end =
                        { col = e1.selected.col + Ref.width src - 1
                        , row = e1.selected.row + Ref.height src - 1
                        }
                    }
            in
            recalcExample (Ref.cellsOf dest) { e1 | sheet = Sheet.copyPaste src e1.selected e1.sheet }

        Nothing ->
            e


{-| Fill the selection downward from its top row (relative references shift), recording an
undo step. -}
fillSel : Example -> Example
fillSel e =
    let
        e1 =
            record e

        sel =
            selectionOf e1
    in
    recalcExample (Ref.cellsOf sel) { e1 | sheet = Sheet.fillDown sel e1.sheet }


{-| Apply a structural edit (insert/delete/sort), recompute the whole sheet, and keep the
selection in range. These changes ripple across many cells, so a full recalc is simplest
and these example sheets are tiny. -}
structuralEdit : (Example -> Sheet) -> Example -> Example
structuralEdit f e =
    let
        e1 =
            record e

        sheet2 =
            Sheet.recalcAll (f e1)

        ( rows, cols ) =
            Sheet.dims sheet2

        clampRef r =
            { col = clamp 0 (cols - 1) r.col, row = clamp 0 (rows - 1) r.row }
    in
    { e1
        | sheet = sheet2
        , editing = Nothing
        , selected = clampRef e1.selected
        , anchor = clampRef e1.anchor
    }


{-| Commit the in-progress edit (if any) and recalculate, recording an undo step. -}
commitPending : Example -> Example
commitPending e =
    case e.editing of
        Just ( ref, txt ) ->
            let
                e1 =
                    record e
            in
            recalcExample [ ref ] { e1 | editing = Nothing, sheet = Sheet.setRaw ref txt e1.sheet }

        Nothing ->
            e



-- KEYBOARD NAVIGATION & EDITING ----------------------------------------------


{-| Keys while the grid is focused (not editing): arrows move, Tab/Enter move (after
nothing to commit), Backspace/Delete clear, and a printable key starts editing the cell
with that character — like Excel/Sheets. -}
navKey : Int -> View.KeyEvent -> Model -> ( Model, Cmd Msg )
navKey id ke model =
    if ke.ctrl || ke.meta then
        case String.toLower ke.key of
            "z" ->
                ( mapExample id
                    (if ke.shift then
                        redo

                     else
                        undo
                    )
                    model
                , Cmd.none
                )

            "y" ->
                ( mapExample id redo model, Cmd.none )

            "c" ->
                ( mapExample id copySel model, Cmd.none )

            "v" ->
                ( mapExample id pasteSel model, Cmd.none )

            _ ->
                ( model, Cmd.none )

    else
        case ke.key of
            "ArrowUp" ->
                ( mapExample id (arrowMove ke 0 (-1)) model, Cmd.none )

            "ArrowDown" ->
                ( mapExample id (arrowMove ke 0 1) model, Cmd.none )

            "ArrowLeft" ->
                ( mapExample id (arrowMove ke (-1) 0) model, Cmd.none )

            "ArrowRight" ->
                ( mapExample id (arrowMove ke 1 0) model, Cmd.none )

            "Tab" ->
                ( mapExample id (collapseMove (horiz ke) 0) model, Cmd.none )

            "Enter" ->
                ( mapExample id (collapseMove 0 (vert ke)) model, Cmd.none )

            "Backspace" ->
                ( mapExample id clearSelection model, Cmd.none )

            "Delete" ->
                ( mapExample id clearSelection model, Cmd.none )

            _ ->
                if String.length ke.key == 1 then
                    -- type-to-edit: open the cell seeded with the typed character
                    ( mapExample id (\e -> { e | editing = Just ( e.selected, ke.key ) }) model, focusEdit id )

                else
                    ( model, Cmd.none )


{-| Keys while editing a cell: Enter commits & moves down (Shift+Enter up), Tab commits &
moves right (Shift+Tab left), Escape cancels — Excel/Sheets behaviour. -}
editKey : Int -> View.KeyEvent -> Model -> ( Model, Cmd Msg )
editKey id ke model =
    case ke.key of
        "Enter" ->
            ( mapExample id (commitMove 0 (vert ke)) model, focusGrid id )

        "Tab" ->
            ( mapExample id (commitMove (horiz ke) 0) model, focusGrid id )

        "Escape" ->
            ( mapExample id (\e -> { e | editing = Nothing }) model, focusGrid id )

        _ ->
            ( model, Cmd.none )


horiz : View.KeyEvent -> Int
horiz ke =
    if ke.shift then
        -1

    else
        1


vert : View.KeyEvent -> Int
vert ke =
    if ke.shift then
        -1

    else
        1


moveSelected : Int -> Int -> Example -> Example
moveSelected dc dr e =
    { e
        | selected =
            { col = clamp 0 (e.cols - 1) (e.selected.col + dc)
            , row = clamp 0 (e.totalRows - 1) (e.selected.row + dr)
            }
    }


{-| Arrow key: Shift extends the selection (anchor stays), a plain arrow collapses it to
the moved cell. -}
arrowMove : View.KeyEvent -> Int -> Int -> Example -> Example
arrowMove ke dc dr e =
    let
        moved =
            moveSelected dc dr e
    in
    if ke.shift then
        moved

    else
        { moved | anchor = moved.selected }


{-| Tab/Enter: commit any edit, move, and collapse the selection to the one cell. -}
collapseMove : Int -> Int -> Example -> Example
collapseMove dc dr e =
    let
        moved =
            commitMove dc dr e
    in
    { moved | anchor = moved.selected }


commitMove : Int -> Int -> Example -> Example
commitMove dc dr e =
    moveSelected dc dr (commitPending e)


{-| Clear every cell in the selection block (recording an undo step). -}
clearSelection : Example -> Example
clearSelection e =
    let
        e1 =
            record e

        toClear =
            Ref.cellsOf (selectionOf e1)
    in
    recalcExample toClear { e1 | sheet = List.foldl (\r acc -> Sheet.setRaw r "" acc) e1.sheet toClear }



-- FOCUS & RESIZE -------------------------------------------------------------


gridDomId : Int -> String
gridDomId id =
    "ssgrid-" ++ String.fromInt id


editDomId : Int -> String
editDomId id =
    gridDomId id ++ "-edit"


focusGrid : Int -> Cmd Msg
focusGrid id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus (gridDomId id))


focusEdit : Int -> Cmd Msg
focusEdit id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus (editDomId id))


findExample : Int -> Model -> Maybe Example
findExample id model =
    List.head (List.filter (\e -> e.id == id) model.examples)


colWidthOf : Int -> Int -> Model -> Int
colWidthOf id col model =
    case findExample id model of
        Just e ->
            Sheet.colWidth col e.sheet

        Nothing ->
            Sheet.defaultColWidth


applyDrag : Float -> Model -> Model
applyDrag x model =
    case model.drag of
        Just d ->
            mapExample d.exampleId
                (\e -> { e | sheet = Sheet.setColWidth d.col (d.startW + round (x - d.startX)) e.sheet })
                model

        Nothing ->
            model


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
    Sub.batch
        [ if List.any (\e -> e.async && not (Recalc.isDone e.recalc)) model.examples then
            Browser.Events.onAnimationFrameDelta Frame

          else
            Sub.none

        -- While a column is being dragged, track the pointer globally so the drag keeps
        -- working even when the cursor leaves the header.
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove clientXDecoder
                    , Browser.Events.onMouseUp (Json.Decode.succeed ResizeEnd)
                    ]

            Nothing ->
                Sub.none

        -- End a drag-select wherever the mouse button is released.
        , case model.selecting of
            Just _ ->
                Browser.Events.onMouseUp (Json.Decode.succeed SelectUp)

            Nothing ->
                Sub.none
        ]


clientXDecoder : Json.Decode.Decoder Msg
clientXDecoder =
    Json.Decode.field "clientX" Json.Decode.float
        |> Json.Decode.andThen (\x -> Json.Decode.succeed (ResizeMove x))



-- EXAMPLES -------------------------------------------------------------------


example : Int -> String -> String -> Int -> Int -> Sheet -> Example
example id title blurb cols rows sheet =
    { id = id
    , title = title
    , blurb = blurb
    , sheet = sheet
    , selected = { col = 0, row = 0 }
    , anchor = { col = 0, row = 0 }
    , clip = Nothing
    , past = []
    , future = []
    , editing = Nothing
    , cols = cols
    , rows = rows
    , totalRows = rows
    , firstRow = 0
    , async = False
    , recalc = Recalc.idle
    , status = ""
    , toolbar = False
    , editTools = False
    , workbench = False
    , frozenCols = 0
    , findText = ""
    , replaceText = ""
    , exportPanel = ""
    , dataRange = { start = { col = 0, row = 0 }, end = { col = 0, row = 0 } }
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


{-| 8 — structural editing: insert/delete rows & columns (formulas auto-rewrite) and sort. -}
exEdit : Example
exEdit =
    let
        e =
            example 8
                "Edit structure: insert, delete & sort"
                "Select a range — drag across cells, Shift-click or Shift-arrow — then use the toolbar. Insert or delete a row/column and every formula rewrites itself: the Total column’s SUM ranges grow, shrink, and a reference into a deleted cell becomes #REF!, just like Excel. Copy/Paste translates relative references; Fill ↓ copies the top row down; Sort ↑/↓ reorders the expense rows by the selected column. Every change is undoable — Undo/Redo or Ctrl+Z / Ctrl+Shift+Z, and Ctrl+C / Ctrl+V to copy and paste."
                5
                6
                editSheet
    in
    { e | editTools = True, selected = ref "E2", anchor = ref "E2", dataRange = rangeOf "A2" "D4" }


editSheet : Sheet
editSheet =
    build 10 6
        [ ( "A1", "Item" ), ( "B1", "Jan" ), ( "C1", "Feb" ), ( "D1", "Mar" ), ( "E1", "Total" )
        , ( "A2", "Rent" ), ( "B2", "1200" ), ( "C2", "1200" ), ( "D2", "1200" ), ( "E2", "=SUM(B2:D2)" )
        , ( "A3", "Food" ), ( "B3", "400" ), ( "C3", "450" ), ( "D3", "420" ), ( "E3", "=SUM(B3:D3)" )
        , ( "A4", "Travel" ), ( "B4", "200" ), ( "C4", "0" ), ( "D4", "300" ), ( "E4", "=SUM(B4:D4)" )
        , ( "A5", "Total" ), ( "B5", "=SUM(B2:B4)" ), ( "C5", "=SUM(C2:C4)" ), ( "D5", "=SUM(D2:D4)" ), ( "E5", "=SUM(E2:E4)" )
        ]
        |> withStyle (cells "A1" "E1") (\s -> { s | bold = True })
        |> withStyle (cells "A5" "E5") (\s -> { s | bold = True })
        |> Sheet.recalcAll


{-| 9 — workbook features: a merged title, data validation (dropdown + range), a cell
note, find/replace, frozen first column and one-click export. -}
exWorkbook : Example
exWorkbook =
    let
        e =
            example 9
                "Workbook features: merge, validate, notes, find & export"
                "A small tracker showing several spreadsheet conveniences at once. The title spans four columns (a merged cell). The Status column is a validation dropdown; Budget is range-validated, so an out-of-range number is flagged red. D4 carries a note (hover the orange corner). The first column is frozen — scroll sideways and Task stays put. Use the find box to highlight and replace text, and the buttons to export the table as TSV, Markdown, HTML or JSON."
                7
                7
                workbookSheet
    in
    { e
        | workbench = True
        , frozenCols = 1
        , selected = ref "A2"
        , anchor = ref "A2"
        , dataRange = rangeOf "A2" "G6"
    }


workbookSheet : Sheet
workbookSheet =
    build 12 8
        [ ( "A1", "Q3 Project Tracker" )
        , ( "A2", "Task" ), ( "B2", "Owner" ), ( "C2", "Status" ), ( "D2", "Budget" ), ( "E2", "Vendor" ), ( "F2", "Due" ), ( "G2", "Priority" )
        , ( "A3", "Design" ), ( "B3", "Ann" ), ( "C3", "Done" ), ( "D3", "5000" ), ( "E3", "Acme" ), ( "F3", "Jul 10" ), ( "G3", "High" )
        , ( "A4", "Build" ), ( "B4", "Bob" ), ( "C4", "Active" ), ( "D4", "12000" ), ( "E4", "Globex" ), ( "F4", "Aug 02" ), ( "G4", "High" )
        , ( "A5", "Test" ), ( "B5", "Cy" ), ( "C5", "Todo" ), ( "D5", "3000" ), ( "E5", "Initech" ), ( "F5", "Aug 20" ), ( "G5", "Med" )
        , ( "A6", "Ship" ), ( "B6", "Dee" ), ( "C6", "Todo" ), ( "D6", "999999" ), ( "E6", "Umbrella" ), ( "F6", "Sep 01" ), ( "G6", "Low" )
        ]
        |> withStyle (cells "A1" "G1") (\s -> Style.withColor "#ffffff" { s | bold = True })
        |> withStyle [ ref "A1" ] (Style.withBackground "#1a73e8")
        |> withStyle (cells "A2" "G2") (\s -> { s | bold = True })
        |> Sheet.setFormat (ref "D3") (Format.Currency "$" 0)
        |> Sheet.setFormat (ref "D4") (Format.Currency "$" 0)
        |> Sheet.setFormat (ref "D5") (Format.Currency "$" 0)
        |> Sheet.setFormat (ref "D6") (Format.Currency "$" 0)
        |> wideColumns
        |> Sheet.mergeCells (rangeOf "A1" "G1")
        |> Sheet.addValidation (rangeOf "C3" "C6") (Validation.OneOf [ "Todo", "Active", "Done" ])
        |> Sheet.addValidation (rangeOf "D3" "D6") (Validation.NumberBetween 0 100000)
        |> Sheet.setNote (ref "D4") "Includes contractor fees"
        |> Sheet.recalcAll


{-| Widen the columns so the table overflows its container — to show the frozen column. -}
wideColumns : Sheet -> Sheet
wideColumns sheet =
    List.foldl (\c acc -> Sheet.setColWidth c 120 acc) sheet (List.range 0 6)


{-| 10 — async, visible-first recalculation of a large sheet. -}
exAsync : Example
exAsync =
    { id = 10
    , title = "Big sheets without freezing (async)"
    , blurb = "Click “Load” to fill ~2,400 chained formulas (a running sum and two derived columns down 800 rows) — then scroll through them. The grid is a viewport: only the ~20 rows on screen are ever in the DOM (the rest are spacer-backed), and the engine recalculates in small batches across animation frames, doing the visible rows first. So an 800-row sheet stays responsive and the on-screen region settles immediately."
    , sheet =
        build 820 8
            [ ( "A1", "Row" ), ( "B1", "×3" ), ( "C1", "Running Σ" ), ( "D1", "B+C" )
            , ( "A2", "Press “Load” above to populate this sheet, then scroll." )
            ]
    , selected = { col = 0, row = 0 }
    , anchor = { col = 0, row = 0 }
    , clip = Nothing
    , past = []
    , future = []
    , editing = Nothing
    , cols = 8
    , rows = 22
    , totalRows = 800
    , firstRow = 0
    , async = True
    , recalc = Recalc.idle
    , status = "Idle — nothing loaded yet."
    , toolbar = False
    , editTools = False
    , workbench = False
    , frozenCols = 0
    , findText = ""
    , replaceText = ""
    , exportPanel = ""
    , dataRange = { start = { col = 0, row = 0 }, end = { col = 0, row = 0 } }
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
        , div [ HA.class "gallery" ] (List.map (exampleView model.selecting) model.examples)
        , footer
        ]


header : Html Msg
header =
    div [ HA.class "page-head" ]
        [ h1 [] [ text "elm-spreadsheet" ]
        , p [ HA.class "page-lead" ]
            [ text "A spreadsheet logic + view layer in Elm — values, ~100 formula functions, number formats, conditional styling, and sync/async recalculation. Every example below is a live, editable spreadsheet: click a cell and use the arrow keys, type to edit, Tab/Enter to move, and drag a column border to resize. They build up from the simplest use to the most involved." ]
        ]


exampleView : Maybe Int -> Example -> Html Msg
exampleView selecting e =
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
            , if e.editTools then
                [ editToolbar e ]

              else
                []
            , if e.workbench then
                [ workbenchToolbar e ]

              else
                []
            , [ asyncControls e ]
            , styleNode e.css
            , [ div [ HA.class (gridClass e) ] [ View.view (gridConfig (selecting == Just e.id) e) e.sheet ] ]
            , exportPanelView e
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


{-| The structural-edit toolbar, acting on the example's selection. -}
editToolbar : Example -> Html Msg
editToolbar e =
    div [ HA.class "fmt-toolbar" ]
        [ span [ HA.class "fmt-cell" ] [ text (selectionLabel e) ]
        , editBtn True (InsertRow e.id) "＋ Row"
        , editBtn True (DeleteRow e.id) "－ Row"
        , editBtn True (InsertCol e.id) "＋ Col"
        , editBtn True (DeleteCol e.id) "－ Col"
        , span [ HA.class "fmt-sep" ] []
        , editBtn True (SortCol e.id True) "Sort ↑"
        , editBtn True (SortCol e.id False) "Sort ↓"
        , span [ HA.class "fmt-sep" ] []
        , editBtn True (CopySel e.id) "Copy"
        , editBtn (e.clip /= Nothing) (PasteSel e.id) "Paste"
        , editBtn True (FillSel e.id) "Fill ↓"
        , span [ HA.class "fmt-sep" ] []
        , editBtn (not (List.isEmpty e.past)) (Undo e.id) "↶ Undo"
        , editBtn (not (List.isEmpty e.future)) (Redo e.id) "↷ Redo"
        ]


{-| A range label, collapsed to a single ref when the selection is one cell. -}
selectionLabel : Example -> String
selectionLabel e =
    let
        sel =
            selectionOf e
    in
    if sel.start == sel.end then
        Ref.toA1 sel.start

    else
        Ref.rangeToA1 sel


editBtn : Bool -> Msg -> String -> Html Msg
editBtn enabled msg lbl =
    button [ HA.class "ebtn", HA.disabled (not enabled), HE.onClick msg ] [ text lbl ]


{-| The find/replace + export toolbar for the workbook example. -}
workbenchToolbar : Example -> Html Msg
workbenchToolbar e =
    let
        hits =
            List.length (findHits e)

        count =
            if e.findText == "" then
                ""

            else
                String.fromInt hits ++ " found"
    in
    div [ HA.class "fmt-toolbar" ]
        [ input [ HA.class "fsel wb-find", HA.placeholder "Find…", HA.value e.findText, HE.onInput (FindInput e.id) ] []
        , editBtn (hits > 0) (FindNext e.id) "Next"
        , span [ HA.class "wb-count" ] [ text count ]
        , input [ HA.class "fsel wb-find", HA.placeholder "Replace with…", HA.value e.replaceText, HE.onInput (ReplaceInput e.id) ] []
        , editBtn (hits > 0) (ReplaceAllMsg e.id) "Replace all"
        , span [ HA.class "fmt-sep" ] []
        , span [ HA.class "wb-label" ] [ text "Export:" ]
        , editBtn True (ExportAs e.id "tsv") "TSV"
        , editBtn True (ExportAs e.id "md") "Markdown"
        , editBtn True (ExportAs e.id "html") "HTML"
        , editBtn True (ExportAs e.id "json") "JSON"
        ]


exportPanelView : Example -> List (Html Msg)
exportPanelView e =
    if e.exportPanel == "" then
        []

    else
        [ div [ HA.class "css-panel" ]
            [ div [ HA.class "css-cap" ]
                [ text "Exported "
                , text (String.fromInt (Ref.height e.dataRange) ++ "×" ++ String.fromInt (Ref.width e.dataRange) ++ " range:")
                , button [ HA.class "ebtn wb-close", HE.onClick (CloseExport e.id) ] [ text "Close" ]
                ]
            , pre [ HA.class "code" ] [ text e.exportPanel ]
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


gridConfig : Bool -> Example -> View.Config Msg
gridConfig dragging e =
    { id = gridDomId e.id
    , viewCols = e.cols
    , viewRows = e.rows
    , totalRows = e.totalRows
    , firstRow = e.firstRow
    , selected = Just e.selected
    , selection = Just (selectionOf e)
    , dragging = dragging
    , editing = e.editing
    , highlights = findHits e
    , frozenCols = e.frozenCols
    , colWidth = \c -> Sheet.colWidth c e.sheet
    , onCellDown = CellDown e.id
    , onCellEnter = CellEnter e.id
    , onStartEdit = StartEdit e.id
    , onEditInput = EditInput e.id
    , onPick = Pick e.id
    , onNavKey = NavKey e.id
    , onEditKey = EditKey e.id
    , onResizeStart = ResizeStart e.id
    , onScroll = ScrollGrid e.id
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
