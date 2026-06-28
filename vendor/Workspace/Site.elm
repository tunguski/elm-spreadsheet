module Workspace.Site exposing
    ( Config, Landing, Model, Msg
    , program
    , workspaceLink
    )

{-| A turn-key **site** around a [`Workspace`](Workspace): a landing page (logo, eyebrow, title,
descriptive text and an app-specific custom area) plus the live workspace under a top navbar, wired
together with hash-based URL routing so the page is reload-safe, links are shareable and the browser
Back / Forward buttons work.

Every site built on the workspace (elm-notebook, elm-svg, elm-spreadsheet and the elm-workspace demo
itself) has the same shape — a marketing landing with a "go to the workspace" link, and the workspace
proper at `#workspace` / `#<uuid>`. This module captures that shape once, so each host's `Main` only
declares *what is different*: its document [`Config`](Workspace#Config), its landing content, and a
few strings (title, namespace, logo, eyebrow, lead).

    main =
        Workspace.Site.program
            { title = "elm-notebook"
            , namespace = "elm-notebook"
            , logo = "logo.svg"
            , eyebrow = "elm · data exploration"
            , lead =
                [ Html.text "A Jupyter-style notebook. Open the "
                , Workspace.Site.workspaceLink [ Html.text "Workspace" ]
                , Html.text " to save and organise many notebooks."
                ]
            , repoUrl = "https://github.com/tunguski/elm-notebook"
            , workspace = Notebook.Workspace.config
            , context = { user = "me", groups = [] }
            , landing = { init = …, update = …, view = …, copyToWorkspace = … }
            }

The localStorage [`Backend`](Workspace-Backend#Backend) is built from `namespace`; the page `<title>`
is set at build time (the runtime has no title primitive on `Browser.element`).

@docs Config, Landing, Model, Msg
@docs program
@docs workspaceLink

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, footer, h1, header, img, nav, p, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Time
import Workspace
import Workspace.Backend exposing (Backend, Context)
import Workspace.Browser



-- CONFIG ---------------------------------------------------------------------


{-| Everything that differs between sites.

  - `title` — the app name (shown in the hero `<h1>` and the workspace navbar brand).
  - `namespace` — localStorage namespace; the browser backend is built from it.
  - `logo` — path to the app's `logo.svg` (shown in the hero and the navbar).
  - `eyebrow` — the small uppercase chip above the title.
  - `lead` — the hero's descriptive paragraph; drop a [`workspaceLink`](#workspaceLink) in it.
  - `repoUrl` — the project's GitHub URL (used in the footer).
  - `workspace` — the document [`Config`](Workspace#Config) for the managed document.
  - `context` — the acting [`Context`](Workspace-Backend#Context) (site uses a local pseudo-user).
  - `landing` — the app-specific landing content (see [`Landing`](#Landing)).

-}
type alias Config doc docMsg lmodel lmsg =
    { title : String
    , namespace : String
    , logo : String
    , eyebrow : String
    , lead : List (Html (Msg docMsg lmsg))
    , repoUrl : String
    , workspace : Workspace.Config doc docMsg
    , context : Context
    , landing : Landing doc lmodel lmsg
    }


{-| The app-specific landing content shown under the hero on `#`.

`copyToWorkspace lmsg lmodel` lets the landing hand a document to the workspace: return `Just doc`
and the site creates+opens it in the workspace (e.g. elm-notebook's "Copy to workspace"); return
`Nothing` and the message runs through `update` as usual.

-}
type alias Landing doc lmodel lmsg =
    { init : lmodel
    , update : lmsg -> lmodel -> ( lmodel, Cmd lmsg )
    , subscriptions : lmodel -> Sub lmsg
    , view : lmodel -> Html lmsg
    , copyToWorkspace : lmsg -> lmodel -> Maybe doc
    }



-- MODEL ----------------------------------------------------------------------


{-| The site state: which route is showing, the workspace, the landing's own state, and the last
URL hash we wrote (so polling only re-routes on a real change). -}
type alias Model doc lmodel =
    { route : Route
    , ws : Workspace.Model doc
    , landing : lmodel
    , hash : String
    }


type Route
    = Examples
    | Wsp


{-| Site messages, generic over the document message and the landing message. -}
type Msg docMsg lmsg
    = WsMsg (Workspace.Msg docMsg)
    | LandingMsg lmsg
    | GoTo Route
    | GotHash String
    | Poll



-- PROGRAM --------------------------------------------------------------------


{-| Build the whole `Browser.element` program from a [`Config`](#Config). -}
program : Config doc docMsg lmodel lmsg -> Program () (Model doc lmodel) (Msg docMsg lmsg)
program config =
    let
        backend =
            Workspace.Browser.backend config.namespace
    in
    Browser.element
        { init = init config backend
        , update = update config backend
        , view = view config backend
        , subscriptions = subscriptions config
        }


init : Config doc docMsg lmodel lmsg -> Backend (Workspace.Msg docMsg) -> () -> ( Model doc lmodel, Cmd (Msg docMsg lmsg) )
init config backend _ =
    let
        ( ws, wsCmd ) =
            Workspace.init backend
    in
    ( { route = Examples, ws = ws, landing = config.landing.init, hash = "" }
    , Cmd.batch [ Cmd.map WsMsg wsCmd, Nav.getHash GotHash ]
    )



-- UPDATE ---------------------------------------------------------------------


update : Config doc docMsg lmodel lmsg -> Backend (Workspace.Msg docMsg) -> Msg docMsg lmsg -> Model doc lmodel -> ( Model doc lmodel, Cmd (Msg docMsg lmsg) )
update config backend msg model =
    case msg of
        GotHash raw ->
            -- the URL changed (initial load, or Back/Forward) — route from it, don't write it back.
            -- Ignore polls while a document id is being minted (the URL is mid-transition then).
            if pendingCreate model then
                ( model, Cmd.none )

            else
                let
                    h =
                        normalizeHash raw
                in
                if h == toHash model then
                    ( { model | hash = h }, Cmd.none )

                else
                    applyHash backend h { model | hash = h }

        Poll ->
            ( model, Nav.getHash GotHash )

        _ ->
            let
                ( next, cmd ) =
                    updateInner config backend msg model

                desired =
                    toHash next
            in
            -- don't write the hash mid-create (open is not set until the UUID arrives), else the URL
            -- would flicker to #workspace and a poll could read that stale value
            if pendingCreate next || desired == next.hash then
                ( next, cmd )

            else
                ( { next | hash = desired }, Cmd.batch [ cmd, Nav.setHash desired ] )


updateInner : Config doc docMsg lmodel lmsg -> Backend (Workspace.Msg docMsg) -> Msg docMsg lmsg -> Model doc lmodel -> ( Model doc lmodel, Cmd (Msg docMsg lmsg) )
updateInner config backend msg model =
    case msg of
        WsMsg m ->
            let
                ( ws, cmd ) =
                    Workspace.update config.workspace backend config.context m model.ws
            in
            ( { model | ws = ws }, Cmd.map WsMsg cmd )

        LandingMsg m ->
            case config.landing.copyToWorkspace m model.landing of
                Just doc ->
                    -- the landing handed us a document — create+open it in the workspace
                    let
                        ( ws, cmd ) =
                            Workspace.createFrom doc model.ws
                    in
                    ( { model | ws = ws, route = Wsp }, Cmd.map WsMsg cmd )

                Nothing ->
                    let
                        ( landing, lcmd ) =
                            config.landing.update m model.landing
                    in
                    ( { model | landing = landing }, Cmd.map LandingMsg lcmd )

        GoTo route ->
            ( { model | route = route }, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Is a new-document id currently being minted (so the URL should be left alone for a moment)? -}
pendingCreate : Model doc lmodel -> Bool
pendingCreate model =
    case model.ws.pending of
        Just _ ->
            True

        Nothing ->
            False


subscriptions : Config doc docMsg lmodel lmsg -> Model doc lmodel -> Sub (Msg docMsg lmsg)
subscriptions config model =
    Sub.batch
        [ Sub.map WsMsg (Workspace.subscriptions model.ws)
        , Sub.map LandingMsg (config.landing.subscriptions model.landing)

        -- poll the URL hash so the browser Back / Forward buttons change the view
        , Time.every 400 (always Poll)
        ]



-- ROUTING --------------------------------------------------------------------


{-| The hash this model should show: `` for the landing, `workspace` for the list, the document id
for an open document. -}
toHash : Model doc lmodel -> String
toHash model =
    case model.route of
        Examples ->
            ""

        Wsp ->
            case model.ws.open of
                Just stored ->
                    stored.meta.id

                Nothing ->
                    "workspace"


{-| Route from a hash read off the URL (already normalised); `model.hash` is assumed up to date. -}
applyHash : Backend (Workspace.Msg docMsg) -> String -> Model doc lmodel -> ( Model doc lmodel, Cmd (Msg docMsg lmsg) )
applyHash backend h model =
    if h == "" then
        ( { model | route = Examples }, Cmd.none )

    else if h == "workspace" then
        ( { model | route = Wsp }, Cmd.none )

    else
        -- any other hash is a document id (a uuid)
        ( { model | route = Wsp }
        , Cmd.map WsMsg (Workspace.openDocument backend h)
        )


normalizeHash : String -> String
normalizeHash raw =
    raw |> dropPrefixChar '#' |> dropPrefixChar '/'


dropPrefixChar : Char -> String -> String
dropPrefixChar c s =
    if String.startsWith (String.fromChar c) s then
        String.dropLeft 1 s

    else
        s



-- VIEW -----------------------------------------------------------------------


view : Config doc docMsg lmodel lmsg -> Backend (Workspace.Msg docMsg) -> Model doc lmodel -> Html (Msg docMsg lmsg)
view config backend model =
    div [ HA.class "wsite-app" ]
        [ case model.route of
            Examples ->
                div []
                    [ heroView config
                    , Html.map LandingMsg (config.landing.view model.landing)
                    ]

            Wsp ->
                div []
                    [ navView config
                    , Html.map WsMsg (Workspace.view config.workspace backend config.context model.ws)
                    ]
        , footerView config
        ]


{-| The landing hero: logo, eyebrow chip, title and the lead paragraph (which may contain a
[`workspaceLink`](#workspaceLink)). -}
heroView : Config doc docMsg lmodel lmsg -> Html (Msg docMsg lmsg)
heroView config =
    header [ HA.class "wsite-hero" ]
        [ div [ HA.class "wsite-hero-inner" ]
            [ img [ HA.class "wsite-hero-logo", HA.src config.logo, HA.alt "" ] []
            , span [ HA.class "wsite-eyebrow" ] [ text config.eyebrow ]
            , h1 [] [ text config.title ]
            , p [ HA.class "wsite-lead" ] config.lead
            ]
        ]


{-| The workspace top navbar (no distinct background): the logo + app name, linking home. -}
navView : Config doc docMsg lmodel lmsg -> Html (Msg docMsg lmsg)
navView config =
    nav [ HA.class "wsite-nav" ]
        [ button [ HA.class "wsite-brand", HE.onClick (GoTo Examples), HA.title "Back to home" ]
            [ img [ HA.class "wsite-logo", HA.src config.logo, HA.alt "" ] []
            , span [ HA.class "wsite-brand-name" ] [ text config.title ]
            ]
        ]


footerView : Config doc docMsg lmodel lmsg -> Html (Msg docMsg lmsg)
footerView config =
    footer [ HA.class "wsite-foot" ]
        [ span []
            [ text (config.title ++ " — part of the ")
            , a [ HA.href "https://github.com/tunguski/elm-lang" ] [ text "elm-lang" ]
            , text " ecosystem, built on a reusable "
            , a [ HA.href "https://github.com/tunguski/elm-workspace" ] [ text "elm-workspace" ]
            , text "."
            ]
        , div [ HA.class "wsite-foot-links" ]
            [ a [ HA.href "tests.html" ] [ text "Test report" ]
            , a [ HA.href config.repoUrl ] [ text "GitHub" ]
            , a [ HA.href "https://tunguski.github.io/" ] [ text "More projects" ]
            ]
        ]


{-| A link, for use inside the hero `lead`, that switches to the workspace view. -}
workspaceLink : List (Html (Msg docMsg lmsg)) -> Html (Msg docMsg lmsg)
workspaceLink children =
    button [ HA.class "wsite-inline-link", HE.onClick (GoTo Wsp) ] children
