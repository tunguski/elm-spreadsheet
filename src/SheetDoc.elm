module SheetDoc exposing (SheetDoc, SheetMsg, config)

{-| The **workspace document** for elm-spreadsheet: a single editable spreadsheet.

The persisted part is the sheet's raw cells (and its dimensions); the rest of the document is
transient UI state the editor needs — the selection, the in-progress edit and the scroll position.
That mirrors how elm-notebook's document carries transient carets alongside the saved notebook.

This is what lets the reusable [`Workspace`](Workspace) manage many spreadsheets — create, name,
search, copy, share, comment on, import into and export each one — with the engine
(`Spreadsheet.*`) and the grid view (`Spreadsheet.View`) reused unchanged.

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, h4, input, li, p, section, span, strong, text, ul)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Spreadsheet.Ref as Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.View as View
import Workspace
import Workspace.I18n as WsI18n
import Workspace.Serialize as WSerialize
import Workspace.Types as WTypes exposing (DocRef, Selector(..), Table)



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
    , refs : List DocRef
    , refDraft : { binding : String, docId : String, source : String }
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
    | SetRefField String String
    | AddRef
    | RemoveRef Int


emptyRefDraft : { binding : String, docId : String, source : String }
emptyRefDraft =
    { binding = "", docId = "", source = "" }


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
    , refs = []
    , refDraft = emptyRefDraft
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
        , ( "refs", WSerialize.encodeRefs doc.refs )
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
    D.map4 build
        (D.field "rows" D.int)
        (D.field "cols" D.int)
        (D.field "cells" (D.list cellDecoder))
        (D.oneOf [ D.field "refs" WSerialize.refsDecoder, D.succeed [] ])


cellDecoder : D.Decoder ( Ref, String )
cellDecoder =
    D.map3 (\r c v -> ( { col = c, row = r }, v ))
        (D.field "r" D.int)
        (D.field "c" D.int)
        (D.field "v" D.string)


build : Int -> Int -> List ( Ref, String ) -> List DocRef -> SheetDoc
build rows cols cells refs =
    let
        base =
            Sheet.empty rows cols
                |> Sheet.setRawMany cells
                |> Sheet.recalcAll
                |> mkDoc rows cols
    in
    { base | refs = refs }



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

        SetRefField which value ->
            let
                d =
                    doc.refDraft

                d2 =
                    case which of
                        "binding" ->
                            { d | binding = value }

                        "docId" ->
                            { d | docId = value }

                        _ ->
                            { d | source = value }
            in
            { doc | refDraft = d2 }

        AddRef ->
            let
                d =
                    doc.refDraft

                binding =
                    String.trim d.binding

                docId =
                    String.trim d.docId

                source =
                    String.trim d.source
            in
            if binding == "" || docId == "" then
                doc

            else
                let
                    -- A "A1:C10" source is a spreadsheet range; anything else is treated as a
                    -- notebook step id; blank means the whole document.
                    selector =
                        if source == "" then
                            WholeDoc

                        else if String.contains ":" source then
                            RangeSel source

                        else
                            Step source
                in
                { doc
                    | refs = doc.refs ++ [ { binding = binding, docId = docId, selector = selector } ]
                    , refDraft = emptyRefDraft
                }

        RemoveRef index ->
            { doc | refs = dropIndex index doc.refs }


dropIndex : Int -> List a -> List a
dropIndex index xs =
    List.indexedMap Tuple.pair xs
        |> List.filter (\( i, _ ) -> i /= index)
        |> List.map Tuple.second


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
        , refsPanel doc
        ]


{-| The document-references panel: pull a range from another spreadsheet (or a step from a notebook)
into a local top-left cell. Pressing "Reload data" in the workspace toolbar re-fetches them. -}
refsPanel : SheetDoc -> Html SheetMsg
refsPanel doc =
    let
        d =
            doc.refDraft
    in
    section [ HA.class "sheetdoc-refs" ]
        [ h4 [ HA.class "sheetdoc-refs-title" ] [ text "External data references" ]
        , if List.isEmpty doc.refs then
            p [ HA.class "sheetdoc-refs-empty" ]
                [ text "Reference a range from another spreadsheet, or a step from a notebook, and drop it into a cell. Use the toolbar's Reload data to refresh." ]

          else
            ul [ HA.class "sheetdoc-refs-list" ] (List.indexedMap refRow doc.refs)
        , div [ HA.class "sheetdoc-refs-form" ]
            [ input [ HA.class "sheetdoc-ref-in", HA.placeholder "into cell (e.g. E1)", HA.value d.binding, HE.onInput (SetRefField "binding") ] []
            , input [ HA.class "sheetdoc-ref-in", HA.placeholder "document id", HA.value d.docId, HE.onInput (SetRefField "docId") ] []
            , input [ HA.class "sheetdoc-ref-in", HA.placeholder "range A1:C10 / step id", HA.value d.source, HE.onInput (SetRefField "source") ] []
            , button [ HA.class "sheetdoc-ref-add", HE.onClick AddRef ] [ text "Add reference" ]
            ]
        ]


refRow : Int -> DocRef -> Html SheetMsg
refRow index ref =
    li [ HA.class "sheetdoc-ref-item" ]
        [ strong [ HA.class "sheetdoc-ref-binding" ] [ text ref.binding ]
        , span [ HA.class "sheetdoc-ref-arrow" ] [ text " ← " ]
        , span [ HA.class "sheetdoc-ref-target" ]
            [ text (String.left 8 ref.docId ++ " · " ++ WTypes.selectorLabel ref.selector) ]
        , button [ HA.class "sheetdoc-ref-x", HA.title "Remove", HE.onClick (RemoveRef index) ] [ text "×" ]
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



-- CROSS-DOCUMENT REFERENCES --------------------------------------------------


{-| Satisfy another document's reference into this spreadsheet: a `RangeSel "A1:C10"` yields that
block as a table (its first row as headers); `WholeDoc` yields the whole used region. -}
provide : Selector -> SheetDoc -> Result String Table
provide selector doc =
    case selector of
        RangeSel a1 ->
            case Ref.rangeFromA1 a1 of
                Just range ->
                    Ok (rangeTable range doc.sheet)

                Nothing ->
                    Err ("not a valid range: " ++ a1)

        WholeDoc ->
            toTable doc |> Result.fromMaybe "the spreadsheet is empty"

        Step _ ->
            Err "a spreadsheet is addressed by range, not step"


rangeTable : Ref.Range -> Sheet -> Table
rangeTable range sheet =
    let
        grid =
            Ref.rowsOf range
                |> List.map (List.map (\ref -> Sheet.displayString ref sheet))
    in
    case grid of
        header :: body ->
            { headers = header, rows = body }

        [] ->
            { headers = [], rows = [] }


{-| Absorb the resolved reference tables: each reference's `binding` is a local top-left cell (e.g.
`E1`) where its table is dropped, headers included. Overwriting those cells is exactly the "reload
data" behaviour — pressing Reload re-fetches and re-drops fresh values. -}
absorb : Dict String Table -> SheetDoc -> SheetDoc
absorb tables doc =
    let
        edits =
            Dict.toList tables |> List.concatMap tableEdits

        tableEdits ( binding, table ) =
            case Ref.fromA1 binding of
                Just topLeft ->
                    placeTable topLeft table

                Nothing ->
                    []
    in
    { doc | sheet = Sheet.setRawMany edits doc.sheet }


placeTable : Ref -> Table -> List ( Ref, String )
placeTable topLeft table =
    (table.headers :: table.rows)
        |> List.indexedMap
            (\dr row ->
                List.indexedMap
                    (\dc v -> ( { col = topLeft.col + dc, row = topLeft.row + dr }, v ))
                    row
            )
        |> List.concat



-- CONFIG ---------------------------------------------------------------------


config : Workspace.Config SheetDoc SheetMsg
config =
    { codec = { encode = encode, decoder = decoder }
    , empty = empty
    , kind = "spreadsheet"
    , activate = \doc -> { doc | sheet = Sheet.recalcAll doc.sheet }
    , viewDoc = viewDoc
    , updateDoc = updateDoc
    , elementsOf = elementsOf
    , toTable = toTable
    , onImport = Just fromTable
    , t = WsI18n.en
    , templates = []
    , references = .refs
    , provide = provide
    , absorb = absorb
    , docSql = \_ -> Nothing
    }
