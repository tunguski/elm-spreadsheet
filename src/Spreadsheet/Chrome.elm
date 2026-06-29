module Spreadsheet.Chrome exposing
    ( Menu, MenuItem(..)
    , ToolItem(..)
    , Dialog
    , MenuBarConfig, menuBar
    , toolbar
    , dialog
    )

{-| A configurable application **chrome** for a spreadsheet: a menu bar, a toolbar and the
modal dialogs they open — modelled on web office suites (Google Sheets / Excel for the
web).

Everything here is **declarative and data-driven**. The host describes the menus and
toolbar as plain data whose leaves carry the host's own `msg`, decides *whether* a menu bar
and/or toolbar are shown (just don't render the ones you don't want), and owns the small bit
of UI state these need — which menu is currently open, and which dialog (if any) is
showing. The module renders them as class-styled HTML with no inline styles and no effects,
so it stays pure and a host stylesheet can restyle all of it.

A typical layout is

    div []
        [ if model.showMenuBar then Chrome.menuBar menuBarConfig else text ""
        , if model.showToolbar then Chrome.toolbar toolItems else text ""
        , View.view gridConfig sheet
        , case model.dialog of
            Just d -> Chrome.dialog d
            Nothing -> text ""
        ]

**Closing menus on selection.** Selecting a menu item emits that item's `msg`; the host
should clear its "open menu" state when handling any such action (and on the backdrop click
the module emits `onClose` for you). Routing every menu/toolbar action through one wrapper
message makes that a single line in `update`.

@docs Menu, MenuItem, ToolItem, Dialog, MenuBarConfig, menuBar, toolbar, dialog

-}

import Html exposing (Html, button, div, h3, span, text)
import Html.Attributes as HA
import Html.Events as HE



-- MENUS ----------------------------------------------------------------------


{-| A top-level menu: a label and the items that drop down from it. -}
type alias Menu msg =
    { label : String
    , items : List (MenuItem msg)
    }


{-| One entry in a drop-down menu:

  - `Item` — a normal command, with an optional keyboard-shortcut hint and an `enabled` flag.
  - `Check` — a command that shows a tick when `checked` (a toggle, e.g. “✓ Toolbar”).
  - `SubMenu` — a nested fly-out of more items (revealed on hover).
  - `Divider` — a horizontal rule grouping the items above and below.

-}
type MenuItem msg
    = Item { label : String, shortcut : String, enabled : Bool, onSelect : msg }
    | Check { label : String, checked : Bool, onSelect : msg }
    | SubMenu String (List (MenuItem msg))
    | Divider


{-| What the menu bar needs from the host: the menus, which one is `open` (by label), and
how to toggle a menu open and to close all menus. -}
type alias MenuBarConfig msg =
    { menus : List (Menu msg)
    , open : Maybe String
    , onOpen : String -> msg
    , onClose : msg
    }


{-| Render the menu bar. When a menu is open, a transparent full-screen backdrop is laid
under the drop-down so a click anywhere else emits `onClose`. -}
menuBar : MenuBarConfig msg -> Html msg
menuBar config =
    div [ HA.class "ss-menubar" ]
        (backdrop config
            :: List.map (topMenu config) config.menus
        )


backdrop : MenuBarConfig msg -> Html msg
backdrop config =
    case config.open of
        Just _ ->
            div [ HA.class "ss-menu-backdrop", HE.onClick config.onClose ] []

        Nothing ->
            text ""


topMenu : MenuBarConfig msg -> Menu msg -> Html msg
topMenu config menu =
    let
        isOpen =
            config.open == Just menu.label
    in
    div [ HA.class "ss-menu" ]
        [ button
            [ HA.class
                (if isOpen then
                    "ss-menu-label ss-menu-label-open"

                 else
                    "ss-menu-label"
                )
            , HE.onClick (config.onOpen menu.label)
            ]
            [ text menu.label ]
        , if isOpen then
            div [ HA.class "ss-menu-panel" ] (List.map (menuItem config) menu.items)

          else
            text ""
        ]


menuItem : MenuBarConfig msg -> MenuItem msg -> Html msg
menuItem config entry =
    case entry of
        Item it ->
            button
                [ HA.class "ss-menu-item"
                , HA.disabled (not it.enabled)
                , HE.onClick it.onSelect
                ]
                [ span [ HA.class "ss-menu-item-label" ] [ text it.label ]
                , span [ HA.class "ss-menu-item-shortcut" ] [ text it.shortcut ]
                ]

        Check it ->
            button
                [ HA.class "ss-menu-item"
                , HE.onClick it.onSelect
                ]
                [ span [ HA.class "ss-menu-check" ]
                    [ text
                        (if it.checked then
                            "✓"

                         else
                            ""
                        )
                    ]
                , span [ HA.class "ss-menu-item-label" ] [ text it.label ]
                ]

        SubMenu label items ->
            div [ HA.class "ss-menu-sub" ]
                [ button [ HA.class "ss-menu-item ss-menu-item-sub" ]
                    [ span [ HA.class "ss-menu-item-label" ] [ text label ]
                    , span [ HA.class "ss-menu-item-shortcut" ] [ text "▸" ]
                    ]
                , div [ HA.class "ss-menu-subpanel" ] (List.map (menuItem config) items)
                ]

        Divider ->
            div [ HA.class "ss-menu-divider" ] []



-- TOOLBAR --------------------------------------------------------------------


{-| One control on the toolbar:

  - `Tool` — an icon button command, disabled when `enabled` is false.
  - `Toggle` — an icon button that shows an active (pressed) state.
  - `Pick` — a small drop-down (`value`, `[(value, label)]`), e.g. a number-format chooser.
  - `Gap` — a thin separator between groups of controls.

-}
type ToolItem msg
    = Tool { icon : String, title : String, enabled : Bool, onClick : msg }
    | Toggle { icon : String, title : String, active : Bool, onClick : msg }
    | Pick { title : String, value : String, options : List ( String, String ), onPick : String -> msg }
    | Gap


{-| Render a toolbar from a flat list of controls (use `Gap` to separate groups). -}
toolbar : List (ToolItem msg) -> Html msg
toolbar items =
    div [ HA.class "ss-toolbar" ] (List.map toolItem items)


toolItem : ToolItem msg -> Html msg
toolItem item =
    case item of
        Tool t ->
            button
                [ HA.class "ss-tool"
                , HA.title t.title
                , HA.disabled (not t.enabled)
                , HE.onClick t.onClick
                ]
                [ text t.icon ]

        Toggle t ->
            button
                [ HA.class
                    (if t.active then
                        "ss-tool ss-tool-on"

                     else
                        "ss-tool"
                    )
                , HA.title t.title
                , HE.onClick t.onClick
                ]
                [ text t.icon ]

        Pick p ->
            Html.select
                [ HA.class "ss-tool-pick", HA.title p.title, HE.onInput p.onPick ]
                (List.map
                    (\( v, lbl ) -> Html.option [ HA.value v, HA.selected (v == p.value) ] [ text lbl ])
                    p.options
                )

        Gap ->
            span [ HA.class "ss-tool-gap" ] []



-- DIALOG ---------------------------------------------------------------------


{-| A modal dialog: a title, a body (arbitrary content), a row of footer `actions`
(buttons), and the message to close it (the × button). -}
type alias Dialog msg =
    { title : String
    , body : List (Html msg)
    , actions : List (Html msg)
    , onClose : msg
    }


{-| Render a dialog as a centered modal over a dimmed overlay. -}
dialog : Dialog msg -> Html msg
dialog d =
    div [ HA.class "ss-dialog-overlay" ]
        [ div [ HA.class "ss-dialog" ]
            [ div [ HA.class "ss-dialog-head" ]
                [ h3 [ HA.class "ss-dialog-title" ] [ text d.title ]
                , button [ HA.class "ss-dialog-x", HE.onClick d.onClose ] [ text "✕" ]
                ]
            , div [ HA.class "ss-dialog-body" ] d.body
            , div [ HA.class "ss-dialog-actions" ] d.actions
            ]
        ]
