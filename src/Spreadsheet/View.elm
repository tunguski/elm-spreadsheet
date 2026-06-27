module Spreadsheet.View exposing
    ( Config
    , view
    )

{-| Render a `Sheet` to HTML.

The view is a reusable component driven by a `Config msg` of callbacks and UI state — it
holds no state of its own, so the host (`Main`) owns selection, the edit buffer and the
recalculation lifecycle. Appearance is expressed almost entirely as **CSS classes**
(`ss-*`, defined in `spreadsheet.css`) so a host page can restyle the grid without
touching Elm; only data-driven colour (colour scales, data bars) is emitted inline,
because no fixed class can express a continuous value.

Only the requested viewport rectangle (`viewCols` × `viewRows` from the top-left) is
rendered, so a huge sheet stays cheap to draw — the same window the async recalculator
prioritises.

@docs Config, view

-}

import Html exposing (Html, div, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import Spreadsheet.Ref as Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)


{-| Everything the grid needs from the host: how many cells to show, what's selected/being
edited, and the messages to emit on interaction. -}
type alias Config msg =
    { viewCols : Int
    , viewRows : Int
    , selected : Maybe Ref
    , editing : Maybe ( Ref, String )
    , onSelect : Ref -> msg
    , onStartEdit : Ref -> msg
    , onEditInput : String -> msg
    , onEditKey : String -> msg
    }


{-| Render the grid. -}
view : Config msg -> Sheet -> Html msg
view config sheet =
    table [ HA.class "ss-table" ]
        [ thead [] [ headerRow config ]
        , tbody [] (List.map (dataRow config sheet) (List.range 0 (config.viewRows - 1)))
        ]


headerRow : Config msg -> Html msg
headerRow config =
    tr [ HA.class "ss-header-row" ]
        (th [ HA.class "ss-corner" ] []
            :: List.map columnHeader (List.range 0 (config.viewCols - 1))
        )


columnHeader : Int -> Html msg
columnHeader col =
    th [ HA.class "ss-col-header" ] [ text (Ref.colToString col) ]


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
            [ HA.class "ss-cell-input"
            , HA.value buffer
            , HA.autofocus True
            , HE.onInput config.onEditInput
            , onKeyDown config.onEditKey
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


{-| Emit the pressed key's name (e.g. "Enter", "Escape") so the host can commit/cancel.

NB: the runtime binds `Json.Decode.*` under that fully-qualified name; an `as Decode`
import alias would emit `Decode.andThen` and fail with `Unbound: Decode.andThen` at run
time. So we qualify it in full. -}
onKeyDown : (String -> msg) -> Html.Attribute msg
onKeyDown toMsg =
    HE.on "keydown"
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen (\k -> Json.Decode.succeed (toMsg k))
        )
