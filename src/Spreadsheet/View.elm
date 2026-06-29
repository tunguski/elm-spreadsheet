module Spreadsheet.View exposing
    ( Config
    , KeyEvent
    , rowHeight
    , view
    , chart
    )

{-| Render a `Sheet` to an interactive HTML grid.

The grid is keyboard-driven like Excel/Sheets. Its outer element is focusable
(`tabindex`), and while it holds focus a single keydown handler routes navigation —
arrow keys move the selection, a printable character starts editing the focused cell,
Tab/Enter move after committing. When a cell is being edited the focusable wrapper drops
its handler and a real `<input>` takes over (its own handler intercepts Tab/Enter/Escape
to commit or cancel), so the two never fight over a keystroke.

Columns are sized by a `<colgroup>` whose widths come from the sheet, and each column
header carries a drag handle that reports the pointer position back to the host.

The view holds no state of its own — a `Config msg` of callbacks and UI state drives it,
so the host (`Main`) owns selection, the edit buffer, focus and the resize lifecycle.
Appearance is almost entirely CSS classes (`ss-*`); only data-driven colour is inline.

@docs Config, KeyEvent, view

-}

import Html exposing (Html, div, input, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import Spreadsheet.Chart as Chart
import Spreadsheet.Ref as Ref exposing (Range, Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)


{-| A keyboard event reduced to what the grid cares about: the key name and the
modifier flags. -}
type alias KeyEvent =
    { key : String
    , shift : Bool
    , ctrl : Bool
    , meta : Bool
    }


{-| Everything the grid needs from the host. `id` must be unique per grid (it's the focus
target, and the edit input derives its id from it); `colWidth` supplies each column's
pixel width. -}
type alias Config msg =
    { id : String
    , viewCols : Int
    , viewRows : Int
    , totalRows : Int
    , firstRow : Int
    , selected : Maybe Ref
    , selection : Maybe Range
    , dragging : Bool
    , editing : Maybe ( Ref, String )
    , highlights : List Ref
    , frozenCols : Int
    , frozenRows : Int
    , hiddenRows : List Int
    , colWidth : Int -> Int
    , onCellDown : Ref -> Bool -> msg
    , onCellEnter : Ref -> msg
    , onStartEdit : Ref -> msg
    , onEditInput : String -> msg
    , onPick : Ref -> String -> msg
    , onNavKey : KeyEvent -> msg
    , onEditKey : KeyEvent -> msg
    , onResizeStart : Int -> Float -> msg
    , onScroll : Int -> msg
    }


{-| The rendered pixel height of one grid row (must match the stylesheet). The host uses
it to convert a scroll offset to a first-visible row, and the view uses it to size the
spacer rows that stand in for the off-screen rows. -}
rowHeight : Int
rowHeight =
    28


{-| Render the grid. -}
view : Config msg -> Sheet -> Html msg
view config sheet =
    let
        -- Virtualise rows: only the window [firstRow, last] is in the DOM; spacer rows
        -- stand in for the rest so the scrollbar reflects the full sheet height and the
        -- visible block sits at the right scroll offset. Keeps a huge sheet cheap to draw.
        firstRow =
            clamp 0 (max 0 (config.totalRows - 1)) config.firstRow

        last =
            min (firstRow + config.viewRows - 1) (config.totalRows - 1)

        topSpacer =
            spacerRow config (firstRow * rowHeight)

        bottomSpacer =
            spacerRow config ((config.totalRows - 1 - last) * rowHeight)

        visibleRows =
            List.filter (\r -> not (List.member r config.hiddenRows)) (List.range firstRow last)

        bodyRows =
            topSpacer
                ++ List.map (dataRow config sheet) visibleRows
                ++ bottomSpacer
    in
    div
        (List.append
            [ HA.id config.id, HA.tabindex 0, HA.class "ss-grid-focus", scrollHandler config ]
            (if config.editing == Nothing then
                [ navHandler config ]

             else
                []
            )
        )
        [ table [ HA.class "ss-table" ]
            [ colGroup config
            , thead [] [ headerRow config ]
            , tbody [] bodyRows
            ]
        ]


{-| Draw a chart (column / bar / pie / line) of a labelled numeric series, using only CSS
(flex bars, a `conic-gradient` pie, a `clip-path` area) so it renders without SVG. -}
chart : { kind : Chart.Kind, title : String, labels : List String, values : List Float, colors : List String } -> Html msg
chart cfg =
    div [ HA.class "ss-chart" ]
        [ div [ HA.class "ss-chart-title" ] [ text cfg.title ]
        , chartBody cfg
        , chartLegend cfg
        ]


chartBody : { kind : Chart.Kind, title : String, labels : List String, values : List Float, colors : List String } -> Html msg
chartBody cfg =
    case cfg.kind of
        Chart.Column ->
            plotted
                (div [ HA.class "ss-chart-cols" ]
                    (List.indexedMap
                        (\i frac ->
                            div [ HA.class "ss-chart-colwrap" ]
                                [ div [ HA.class "ss-chart-col", HA.style "height" (pct (frac * 100)), HA.style "background" (colorAt i cfg.colors) ] [] ]
                        )
                        (Chart.bars cfg.values)
                    )
                )

        Chart.Bar ->
            div [ HA.class "ss-chart-rows" ]
                (List.indexedMap
                    (\i frac ->
                        div [ HA.class "ss-chart-barrow" ]
                            [ div [ HA.class "ss-chart-bar", HA.style "width" (pct (frac * 100)), HA.style "background" (colorAt i cfg.colors) ] [] ]
                    )
                    (Chart.bars cfg.values)
                )

        Chart.Pie ->
            let
                stops =
                    String.join ", "
                        (List.indexedMap
                            (\i ( s, e ) -> colorAt i cfg.colors ++ " " ++ pct (s * 100) ++ " " ++ pct (e * 100))
                            (Chart.pieSlices cfg.values)
                        )
            in
            div [ HA.class "ss-chart-pie", HA.style "background" ("conic-gradient(" ++ stops ++ ")") ] []

        Chart.Line ->
            plotted (lineArea "ss-chart-area" cfg.values)

        Chart.Area ->
            plotted (lineArea "ss-chart-area ss-chart-filled" cfg.values)

        Chart.Scatter ->
            plotted
                (div [ HA.class "ss-chart-scatter" ]
                    (List.indexedMap
                        (\i ( x, y ) ->
                            div
                                [ HA.class "ss-chart-dot"
                                , HA.style "left" (pct (x * 100))
                                , HA.style "top" (pct (y * 100))
                                , HA.style "background" (colorAt i cfg.colors)
                                ]
                                []
                        )
                        (Chart.scatterPoints (List.indexedMap (\i v -> ( toFloat i, v )) cfg.values))
                    )
                )


{-| Wrap a cartesian chart body with a horizontal gridline overlay. -}
plotted : Html msg -> Html msg
plotted body =
    div [ HA.class "ss-chart-plot" ] [ gridlines, body ]


gridlines : Html msg
gridlines =
    div [ HA.class "ss-chart-grid" ]
        (List.map
            (\f -> div [ HA.class "ss-chart-gridline", HA.style "bottom" (pct (f * 100)) ] [])
            (Chart.gridLevels 4)
        )


{-| The filled-area body of a line/area chart (a `clip-path` polygon under the points). -}
lineArea : String -> List Float -> Html msg
lineArea cls values =
    let
        poly =
            "polygon(0% 100%, "
                ++ String.join ", " (List.map (\( x, y ) -> pct (x * 100) ++ " " ++ pct (y * 100)) (Chart.linePoints values))
                ++ ", 100% 100%)"
    in
    div [ HA.class "ss-chart-line" ]
        [ div [ HA.class cls, HA.style "clip-path" poly, HA.style "-webkit-clip-path" poly ] [] ]


chartLegend : { kind : Chart.Kind, title : String, labels : List String, values : List Float, colors : List String } -> Html msg
chartLegend cfg =
    div [ HA.class "ss-chart-legend" ]
        (List.indexedMap
            (\i label ->
                div [ HA.class "ss-chart-key" ]
                    [ span [ HA.class "ss-chart-swatch", HA.style "background" (colorAt i cfg.colors) ] []
                    , text label
                    ]
            )
            cfg.labels
        )


colorAt : Int -> List String -> String
colorAt i colors =
    case List.head (List.drop (modBy (max 1 (List.length colors)) i) colors) of
        Just c ->
            c

        Nothing ->
            "#1a73e8"


{-| A zero-content row that occupies `height` px, standing in for off-screen rows. -}
spacerRow : Config msg -> Int -> List (Html msg)
spacerRow config height =
    if height <= 0 then
        []

    else
        [ tr [ HA.class "ss-spacer" ]
            [ td
                [ HA.colspan (config.viewCols + 1)
                , HA.style "height" (px height)
                , HA.style "padding" "0"
                , HA.style "border" "0"
                ]
                []
            ]
        ]


{-| A `<colgroup>` setting the row-header column and each data column's width (the table
is `table-layout: fixed`, so these widths win). -}
colGroup : Config msg -> Html msg
colGroup config =
    Html.node "colgroup"
        []
        (colEl "40px"
            :: List.map (\c -> colEl (px (config.colWidth c))) (List.range 0 (config.viewCols - 1))
        )


colEl : String -> Html msg
colEl width =
    Html.node "col" [ HA.style "width" width ] []


px : Int -> String
px n =
    String.fromInt n ++ "px"


headerRow : Config msg -> Html msg
headerRow config =
    tr [ HA.class "ss-header-row" ]
        (th [ HA.class "ss-corner" ] []
            :: List.map (columnHeader config) (List.range 0 (config.viewCols - 1))
        )


columnHeader : Config msg -> Int -> Html msg
columnHeader config col =
    let
        frozen =
            if col < config.frozenCols then
                HA.class "ss-col-header ss-frozen-col"
                    :: List.map (\( p, v ) -> HA.style p v) (frozenOffset config { col = col, row = 0 })

            else
                [ HA.class "ss-col-header" ]
    in
    th frozen
        [ span [ HA.class "ss-col-label" ] [ text (Ref.colToString col) ]
        , div [ HA.class "ss-resize", resizeHandler config col ] []
        ]


dataRow : Config msg -> Sheet -> Int -> Html msg
dataRow config sheet row =
    let
        rowAttrs =
            if row < config.frozenRows then
                [ HA.class "ss-row ss-frozen-row", HA.style "top" (px (rowHeight * (row + 1))) ]

            else
                [ HA.class "ss-row" ]
    in
    tr rowAttrs
        (th [ HA.class "ss-row-header" ] [ text (String.fromInt (row + 1)) ]
            :: List.filterMap
                (\col ->
                    let
                        ref =
                            { col = col, row = row }
                    in
                    -- A cell covered by a merge is dropped; the anchor spans it.
                    if Sheet.isCovered ref sheet then
                        Nothing

                    else
                        Just (cell config sheet ref)
                )
                (List.range 0 (config.viewCols - 1))
        )


cell : Config msg -> Sheet -> Ref -> Html msg
cell config sheet ref =
    if isEditing config ref then
        editingCell config sheet ref

    else
        displayCell config sheet ref


isEditing : Config msg -> Ref -> Bool
isEditing config ref =
    case config.editing of
        Just ( editRef, _ ) ->
            editRef == ref

        Nothing ->
            False


editingCell : Config msg -> Sheet -> Ref -> Html msg
editingCell config sheet ref =
    let
        buffer =
            case config.editing of
                Just ( _, txt ) ->
                    txt

                Nothing ->
                    ""
    in
    case Sheet.dropdownAt ref sheet of
        Just opts ->
            -- A list-validation cell edits through a native dropdown.
            td [ HA.class "ss-cell ss-editing" ]
                [ select
                    [ HA.id (config.id ++ "-edit")
                    , HA.class "ss-cell-input ss-cell-select"
                    , HE.onInput (config.onPick ref)
                    ]
                    (option [ HA.value "" ] [ text "—" ]
                        :: List.map (\o -> option [ HA.value o, HA.selected (o == buffer) ] [ text o ]) opts
                    )
                ]

        Nothing ->
            td [ HA.class "ss-cell ss-editing" ]
                [ input
                    [ HA.id (config.id ++ "-edit")
                    , HA.class "ss-cell-input"
                    , HA.value buffer
                    , HE.onInput config.onEditInput
                    , editHandler config
                    ]
                    []
                ]


displayCell : Config msg -> Sheet -> Ref -> Html msg
displayCell config sheet ref =
    let
        rendered =
            Sheet.renderedStyle ref sheet

        isActive =
            config.selected == Just ref

        note =
            Sheet.noteAt ref sheet

        stateClasses =
            List.concat
                [ if isActive then
                    [ "ss-selected" ]

                  else
                    []
                , if inSelection config ref && not isActive then
                    [ "ss-in-range" ]

                  else
                    []
                , if Sheet.isInvalid ref sheet then
                    [ "ss-invalid" ]

                  else
                    []
                , if note /= Nothing then
                    [ "ss-noted" ]

                  else
                    []
                , if List.member ref config.highlights then
                    [ "ss-find" ]

                  else
                    []
                , if ref.col < config.frozenCols then
                    [ "ss-frozen-col" ]

                  else
                    []
                , if Sheet.isSpilled ref sheet then
                    [ "ss-spilled" ]

                  else
                    []
                , if Sheet.spillRangeAt ref sheet /= Nothing then
                    [ "ss-spill-anchor" ]

                  else
                    []
                ]

        classAttr =
            HA.class (String.join " " ("ss-cell" :: rendered.classes ++ stateClasses))

        inlineAttrs =
            List.map (\( prop, val ) -> HA.style prop val) (rendered.inline ++ frozenOffset config ref)

        spanAttrs =
            case Sheet.mergeAnchorAt ref sheet of
                Just range ->
                    [ HA.colspan (Ref.width range), HA.rowspan (Ref.height range) ]

                Nothing ->
                    []

        noteAttrs =
            case note of
                Just n ->
                    [ HA.title n ]

                Nothing ->
                    []

        dragAttrs =
            if config.dragging then
                [ cellEnterHandler config ref ]

            else
                []

        indicators =
            (case note of
                Just _ ->
                    [ span [ HA.class "ss-note-dot" ] [] ]

                Nothing ->
                    []
            )
                ++ (if Sheet.dropdownAt ref sheet /= Nothing then
                        [ span [ HA.class "ss-dd-caret" ] [ text "▾" ] ]

                    else
                        []
                   )
        content =
            case Sheet.sparklineAt ref sheet of
                Just spark ->
                    sparkline spark

                Nothing ->
                    div [ HA.class "ss-cell-content" ]
                        (iconSpan ref sheet ++ [ text (Sheet.displayString ref sheet) ])
    in
    td
        (classAttr
            :: cellDownHandler config ref
            :: HE.onDoubleClick (config.onStartEdit ref)
            :: (spanAttrs ++ noteAttrs ++ dragAttrs ++ inlineAttrs)
        )
        (content :: indicators)


{-| The icon-set glyph for a cell, if any: a small coloured symbol before the value. -}
iconSpan : Ref -> Sheet -> List (Html msg)
iconSpan ref sheet =
    case Sheet.iconAt ref sheet of
        Just ( glyph, color ) ->
            [ span [ HA.class "ss-cf-icon", HA.style "color" color ] [ text glyph ] ]

        Nothing ->
            []


{-| A tiny in-cell chart (bars or a dot-line) of a numeric series, drawn with plain divs
(no SVG) so it renders on every backend. -}
sparkline : Sheet.Spark -> Html msg
sparkline spark =
    let
        lo =
            Maybe.withDefault 0 (List.minimum spark.values)

        hi =
            Maybe.withDefault 1 (List.maximum spark.values)

        norm v =
            if hi <= lo then
                0.5

            else
                (v - lo) / (hi - lo)

        item v =
            case spark.kind of
                Sheet.SparkBar ->
                    div
                        [ HA.class "ss-spark-bar"
                        , HA.style "height" (pct (10 + norm v * 90))
                        , HA.style "background" spark.color
                        ]
                        []

                Sheet.SparkLine ->
                    div [ HA.class "ss-spark-col" ]
                        [ div
                            [ HA.class "ss-spark-dot"
                            , HA.style "margin-top" (pct ((1 - norm v) * 90))
                            , HA.style "background" spark.color
                            ]
                            []
                        ]
    in
    div [ HA.class "ss-spark" ] (List.map item spark.values)


pct : Float -> String
pct n =
    String.fromInt (round n) ++ "%"


{-| Sticky-left offset for a frozen column: the row-header width plus the widths of the
frozen columns to its left. -}
frozenOffset : Config msg -> Ref -> List ( String, String )
frozenOffset config ref =
    if ref.col < config.frozenCols then
        [ ( "left", px (40 + List.sum (List.map config.colWidth (List.range 0 (ref.col - 1)))) ) ]

    else
        []


{-| Is a cell within the highlighted selection block? -}
inSelection : Config msg -> Ref -> Bool
inSelection config ref =
    case config.selection of
        Just range ->
            Ref.contains range ref

        Nothing ->
            False



-- KEYBOARD & RESIZE DECODERS -------------------------------------------------
-- The runtime binds `Json.Decode.*` under that fully-qualified name, so we never alias
-- the module (an `as Decode` import emits an Unbound `Decode.andThen` at run time).


{-| Keydown handler for the focused grid (navigation mode). Fires `onNavKey` and prevents
the browser default only for the keys the grid actually handles. -}
navHandler : Config msg -> Html.Attribute msg
navHandler config =
    HE.preventDefaultOn "keydown"
        (keyEventDecoder
            |> Json.Decode.andThen
                (\ke ->
                    if navHandled ke then
                        Json.Decode.succeed ( config.onNavKey ke, True )

                    else
                        Json.Decode.fail "unhandled"
                )
        )


{-| Keydown handler for the edit input: intercept only Tab/Enter/Escape (commit/cancel);
every other key falls through to normal text entry. -}
editHandler : Config msg -> Html.Attribute msg
editHandler config =
    HE.preventDefaultOn "keydown"
        (keyEventDecoder
            |> Json.Decode.andThen
                (\ke ->
                    if List.member ke.key [ "Tab", "Enter", "Escape" ] then
                        Json.Decode.succeed ( config.onEditKey ke, True )

                    else
                        Json.Decode.fail "passthrough"
                )
        )


{-| Mousedown on a cell: begins a selection (or extends it when Shift is held), reporting
the cell and the shift-key state. -}
cellDownHandler : Config msg -> Ref -> Html.Attribute msg
cellDownHandler config ref =
    HE.on "mousedown"
        (Json.Decode.field "shiftKey" Json.Decode.bool
            |> Json.Decode.andThen (\sh -> Json.Decode.succeed (config.onCellDown ref sh))
        )


{-| Mouse entering a cell while a drag-select is in progress extends the selection. -}
cellEnterHandler : Config msg -> Ref -> Html.Attribute msg
cellEnterHandler config ref =
    HE.on "mouseenter" (Json.Decode.succeed (config.onCellEnter ref))


resizeHandler : Config msg -> Int -> Html.Attribute msg
resizeHandler config col =
    HE.preventDefaultOn "mousedown"
        (Json.Decode.field "clientX" Json.Decode.float
            |> Json.Decode.andThen (\x -> Json.Decode.succeed ( config.onResizeStart col x, True ))
        )


{-| Report the container's scroll offset (px) so the host can pick the first visible row. -}
scrollHandler : Config msg -> Html.Attribute msg
scrollHandler config =
    HE.on "scroll"
        (Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float
            |> Json.Decode.andThen (\y -> Json.Decode.succeed (config.onScroll (round y)))
        )


navHandled : KeyEvent -> Bool
navHandled ke =
    if ke.ctrl || ke.meta then
        -- the editing shortcuts the grid owns; let every other Ctrl/Cmd combo through
        List.member (String.toLower ke.key) [ "z", "y", "c", "v" ]

    else
        isArrow ke.key
            || List.member ke.key [ "Tab", "Enter", "Backspace", "Delete" ]
            || isPrintable ke.key


isArrow : String -> Bool
isArrow k =
    List.member k [ "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight" ]


isPrintable : String -> Bool
isPrintable k =
    String.length k == 1


keyEventDecoder : Json.Decode.Decoder KeyEvent
keyEventDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\k ->
                Json.Decode.field "shiftKey" Json.Decode.bool
                    |> Json.Decode.andThen
                        (\s ->
                            Json.Decode.field "ctrlKey" Json.Decode.bool
                                |> Json.Decode.andThen
                                    (\c ->
                                        Json.Decode.field "metaKey" Json.Decode.bool
                                            |> Json.Decode.andThen
                                                (\m ->
                                                    Json.Decode.succeed { key = k, shift = s, ctrl = c, meta = m }
                                                )
                                    )
                        )
            )
