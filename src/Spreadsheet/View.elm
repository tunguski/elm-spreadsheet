module Spreadsheet.View exposing
    ( Config
    , KeyEvent
    , rowHeight
    , view
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

import Html exposing (Html, div, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import Spreadsheet.Ref as Ref exposing (Ref)
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
    , editing : Maybe ( Ref, String )
    , colWidth : Int -> Int
    , onSelect : Ref -> msg
    , onStartEdit : Ref -> msg
    , onEditInput : String -> msg
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

        bodyRows =
            topSpacer
                ++ List.map (dataRow config sheet) (List.range firstRow last)
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
    th [ HA.class "ss-col-header" ]
        [ span [ HA.class "ss-col-label" ] [ text (Ref.colToString col) ]
        , div [ HA.class "ss-resize", resizeHandler config col ] []
        ]


dataRow : Config msg -> Sheet -> Int -> Html msg
dataRow config sheet row =
    tr [ HA.class "ss-row" ]
        (th [ HA.class "ss-row-header" ] [ text (String.fromInt (row + 1)) ]
            :: List.map (\col -> cell config sheet { col = col, row = row }) (List.range 0 (config.viewCols - 1))
        )


cell : Config msg -> Sheet -> Ref -> Html msg
cell config sheet ref =
    if isEditing config ref then
        editingCell config ref

    else
        displayCell config sheet ref


isEditing : Config msg -> Ref -> Bool
isEditing config ref =
    case config.editing of
        Just ( editRef, _ ) ->
            editRef == ref

        Nothing ->
            False


editingCell : Config msg -> Ref -> Html msg
editingCell config ref =
    let
        buffer =
            case config.editing of
                Just ( _, txt ) ->
                    txt

                Nothing ->
                    ""
    in
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

        selectedClass =
            if config.selected == Just ref then
                [ "ss-selected" ]

            else
                []

        classAttr =
            HA.class (String.join " " ("ss-cell" :: rendered.classes ++ selectedClass))

        inlineAttrs =
            List.map (\( prop, val ) -> HA.style prop val) rendered.inline
    in
    td
        (classAttr
            :: HE.onClick (config.onSelect ref)
            :: HE.onDoubleClick (config.onStartEdit ref)
            :: inlineAttrs
        )
        [ div [ HA.class "ss-cell-content" ] [ text (Sheet.displayString ref sheet) ] ]



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
    isArrow ke.key
        || List.member ke.key [ "Tab", "Enter", "Backspace", "Delete" ]
        || (isPrintable ke.key && not ke.ctrl && not ke.meta)


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
