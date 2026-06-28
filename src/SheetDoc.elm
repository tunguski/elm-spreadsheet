module SheetDoc exposing (SheetDoc, SheetMsg, config)

{-| The **workspace document** for elm-spreadsheet: a single editable spreadsheet.

The persisted part is the sheet's raw cells (and its dimensions); the rest of the document is
transient UI state the editor needs — the selection, the in-progress edit and the scroll position.
That mirrors how elm-notebook's document carries transient carets alongside the saved notebook.

This is what lets the reusable [`Workspace`](Workspace) manage many spreadsheets — create, name,
search, copy, share, comment on, import into and export each one — with the engine
(`Spreadsheet.*`) and the grid view (`Spreadsheet.View`) reused unchanged.

-}

import Html exposing (Html, div, span, text)
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import Spreadsheet.Ref as Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.View as View
import Workspace
import Workspace.Types exposing (Table)



-- DOCUMENT -------------------------------------------------------------------


type alias SheetDoc =
    { sheet : Sheet
    , selected : Ref
    , anchor : Ref
    , editing : Maybe ( Ref, String )
    , firstRow : Int
    , cols : Int
    , viewRows : Int
    , totalRows : Int
    }


type SheetMsg
    = CellDown Ref Bool
    | CellEnter Ref
    | StartEdit Ref
    | EditInput String
    | Pick Ref String
    | NavKey View.KeyEvent
    | EditKey View.KeyEvent
    | Scroll Int
    | NoOp


{-| Wrap a recalculated sheet of the given dimensions into a fresh document (A1 selected). -}
mkDoc : Int -> Int -> Sheet -> SheetDoc
mkDoc rows cols sheet =
    { sheet = sheet
    , selected = { col = 0, row = 0 }
    , anchor = { col = 0, row = 0 }
    , editing = Nothing
    , firstRow = 0
    , cols = cols
    , viewRows = Basics.min 16 rows
    , totalRows = rows
    }


{-| A starter sheet for a brand-new document, so it isn't a blank grid. -}
empty : SheetDoc
empty =
    let
        rows =
            24

        cols =
            6
    in
    Sheet.empty rows cols
        |> Sheet.setRawMany
            (List.map (\( a, r ) -> ( cellRef a, r ))
                [ ( "A1", "Item" ), ( "B1", "Amount" )
                , ( "A2", "Coffee" ), ( "B2", "3.5" )
                , ( "A3", "Tea" ), ( "B3", "2" )
                , ( "A4", "Total" ), ( "B4", "=SUM(B2:B3)" )
                ]
            )
        |> Sheet.recalcAll
        |> mkDoc rows cols


{-| A1-style address → Ref (only the simple cases the starter sheet uses). -}
cellRef : String -> Ref
cellRef a =
    let
        letters =
            String.toList a |> List.filter Char.isAlpha

        digits =
            String.toList a |> List.filter Char.isDigit |> String.fromList

        col =
            List.foldl (\c acc -> acc * 26 + (Char.toCode (Char.toUpper c) - 64)) 0 letters - 1

        row =
            (String.toInt digits |> Maybe.withDefault 1) - 1
    in
    { col = Basics.max 0 col, row = Basics.max 0 row }



-- CODEC ----------------------------------------------------------------------


encode : SheetDoc -> E.Value
encode doc =
    let
        ( rows, cols ) =
            Sheet.dims doc.sheet
    in
    E.object
        [ ( "rows", E.int rows )
        , ( "cols", E.int cols )
        , ( "cells"
          , E.list (encodeCell doc.sheet) (Sheet.occupiedRefs doc.sheet)
          )
        ]


encodeCell : Sheet -> Ref -> E.Value
encodeCell sheet ref =
    E.object
        [ ( "r", E.int ref.row )
        , ( "c", E.int ref.col )
        , ( "v", E.string (Sheet.rawAt ref sheet) )
        ]


decoder : D.Decoder SheetDoc
decoder =
    D.map3 build
        (D.field "rows" D.int)
        (D.field "cols" D.int)
        (D.field "cells" (D.list cellDecoder))


cellDecoder : D.Decoder ( Ref, String )
cellDecoder =
    D.map3 (\r c v -> ( { col = c, row = r }, v ))
        (D.field "r" D.int)
        (D.field "c" D.int)
        (D.field "v" D.string)


build : Int -> Int -> List ( Ref, String ) -> SheetDoc
build rows cols cells =
    Sheet.empty rows cols
        |> Sheet.setRawMany cells
        |> Sheet.recalcAll
        |> mkDoc rows cols



-- UPDATE ---------------------------------------------------------------------


updateDoc : SheetMsg -> SheetDoc -> SheetDoc
updateDoc msg doc =
    case msg of
        CellDown ref shift ->
            selectCell ref shift (commitPending doc)

        CellEnter _ ->
            doc

        StartEdit ref ->
            { doc | selected = ref, editing = Just ( ref, Sheet.rawAt ref doc.sheet ) }

        EditInput txt ->
            { doc | editing = Maybe.map (\( r, _ ) -> ( r, txt )) doc.editing }

        Pick ref value ->
            commit ref value { doc | editing = Nothing }

        NavKey ke ->
            navKey ke doc

        EditKey ke ->
            editKey ke doc

        Scroll scrollTop ->
            { doc | firstRow = clamp 0 (Basics.max 0 (doc.totalRows - doc.viewRows)) (scrollTop // View.rowHeight) }

        NoOp ->
            doc


selectCell : Ref -> Bool -> SheetDoc -> SheetDoc
selectCell ref shift doc =
    if shift then
        { doc | selected = ref }

    else
        { doc | selected = ref, anchor = ref }


{-| Commit the in-progress edit (if any) and recompute its dependents. -}
commitPending : SheetDoc -> SheetDoc
commitPending doc =
    case doc.editing of
        Just ( ref, txt ) ->
            commit ref txt { doc | editing = Nothing }

        Nothing ->
            doc


commit : Ref -> String -> SheetDoc -> SheetDoc
commit ref txt doc =
    { doc | sheet = Sheet.recalcFrom [ ref ] (Sheet.setRaw ref txt doc.sheet) }


move : Int -> Int -> SheetDoc -> SheetDoc
move dc dr doc =
    let
        ( rows, cols ) =
            Sheet.dims doc.sheet

        sel =
            { col = clamp 0 (cols - 1) (doc.selected.col + dc)
            , row = clamp 0 (rows - 1) (doc.selected.row + dr)
            }
    in
    { doc | selected = sel, anchor = sel }


navKey : View.KeyEvent -> SheetDoc -> SheetDoc
navKey ke doc =
    case ke.key of
        "ArrowUp" ->
            move 0 -1 doc

        "ArrowDown" ->
            move 0 1 doc

        "ArrowLeft" ->
            move -1 0 doc

        "ArrowRight" ->
            move 1 0 doc

        "Tab" ->
            move
                (if ke.shift then
                    -1

                 else
                    1
                )
                0
                (commitPending doc)

        "Enter" ->
            move 0
                (if ke.shift then
                    -1

                 else
                    1
                )
                (commitPending doc)

        "Backspace" ->
            commit doc.selected "" doc

        "Delete" ->
            commit doc.selected "" doc

        _ ->
            if String.length ke.key == 1 then
                { doc | editing = Just ( doc.selected, ke.key ) }

            else
                doc


editKey : View.KeyEvent -> SheetDoc -> SheetDoc
editKey ke doc =
    case ke.key of
        "Enter" ->
            move 0
                (if ke.shift then
                    -1

                 else
                    1
                )
                (commitPending doc)

        "Tab" ->
            move
                (if ke.shift then
                    -1

                 else
                    1
                )
                0
                (commitPending doc)

        "Escape" ->
            { doc | editing = Nothing }

        _ ->
            doc



-- VIEW -----------------------------------------------------------------------


viewDoc : Workspace.EditorEnv -> SheetDoc -> Html SheetMsg
viewDoc env doc =
    div [ HA.class "sheetdoc" ]
        [ if env.commentsVisible && env.commentCount "sheet" > 0 then
            span [ HA.class "sheetdoc-marker" ]
                [ Html.i [ HA.class "bi bi-chat-dots" ] []
                , text (" " ++ String.fromInt (env.commentCount "sheet"))
                ]

          else
            text ""
        , View.view (gridConfig doc) doc.sheet
        ]


gridConfig : SheetDoc -> View.Config SheetMsg
gridConfig doc =
    { id = "sheetdoc-grid"
    , viewCols = doc.cols
    , viewRows = doc.viewRows
    , totalRows = doc.totalRows
    , firstRow = doc.firstRow
    , selected = Just doc.selected
    , selection = Just (Ref.normalize { start = doc.anchor, end = doc.selected })
    , dragging = False
    , editing = doc.editing
    , highlights = []
    , frozenCols = 0
    , frozenRows = 0
    , hiddenRows = []
    , colWidth = \c -> Sheet.colWidth c doc.sheet
    , onCellDown = CellDown
    , onCellEnter = CellEnter
    , onStartEdit = StartEdit
    , onEditInput = EditInput
    , onPick = Pick
    , onNavKey = NavKey
    , onEditKey = EditKey
    , onResizeStart = \_ _ -> NoOp
    , onScroll = Scroll
    }



-- COMMENTS / IMPORT / EXPORT -------------------------------------------------


elementsOf : SheetDoc -> List ( String, String )
elementsOf _ =
    [ ( "sheet", "The spreadsheet" ) ]


{-| Export the used region as a table (the first occupied row becomes the headers). -}
toTable : SheetDoc -> Maybe Table
toTable doc =
    case Sheet.occupiedRefs doc.sheet of
        [] ->
            Nothing

        refs ->
            let
                maxRow =
                    List.map .row refs |> List.maximum |> Maybe.withDefault 0

                maxCol =
                    List.map .col refs |> List.maximum |> Maybe.withDefault 0

                rowCells r =
                    List.map (\c -> Sheet.displayString { col = c, row = r } doc.sheet) (List.range 0 maxCol)
            in
            Just
                { headers = rowCells 0
                , rows = List.map rowCells (List.range 1 maxRow)
                }


{-| Replace the document with imported tabular data (headers as the first row). -}
fromTable : Table -> SheetDoc -> SheetDoc
fromTable table _ =
    let
        cols =
            Basics.max 6 (List.length table.headers)

        rows =
            Basics.max 24 (List.length table.rows + 4)

        rowEdits r cells =
            List.indexedMap (\c v -> ( { col = c, row = r }, v )) cells

        edits =
            rowEdits 0 table.headers
                ++ List.concat (List.indexedMap (\i cells -> rowEdits (i + 1) cells) table.rows)
    in
    Sheet.empty rows cols
        |> Sheet.setRawMany edits
        |> Sheet.recalcAll
        |> mkDoc rows cols



-- CONFIG ---------------------------------------------------------------------


config : Workspace.Config SheetDoc SheetMsg
config =
    { codec = { encode = encode, decoder = decoder }
    , empty = empty
    , kind = "spreadsheet"
    , activate = identity
    , viewDoc = viewDoc
    , updateDoc = updateDoc
    , elementsOf = elementsOf
    , toTable = toTable
    , onImport = Just fromTable
    }
