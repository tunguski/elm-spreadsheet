module Workspace exposing
    ( Model, Msg, Config, DocCodec, EditorEnv
    , init, update, view, subscriptions
    , openDocument, createFrom
    )

{-| A reusable **workspace** around a document: create, name, open, search, copy, delete and set
permissions on many documents; comment on their elements; import data from a URL or a database
query; export a document to CSV / JSON / Excel. Persistence and the optional database/export
effects are injected as a [`Backend`](Workspace-Backend#Backend); the acting user as a
[`Context`](Workspace-Backend#Context).

The workspace is generic over the **document** it manages — a notebook, a spreadsheet, a chart
spec, a report. The host supplies a [`Config`](#Config): a [`DocCodec`](#DocCodec) (JSON for the
document), an `empty` document, and editor hooks (`viewDoc` / `updateDoc`), plus a few small maps
that let the workspace offer comments, import and export for that document type.

Embed it like any TEA component: `Html.map`/`Cmd.map` a wrapper message, and route that message's
payload through [`update`](#update).

@docs Model, Msg, Config, DocCodec, EditorEnv
@docs init, update, view, subscriptions
@docs openDocument

-}

import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, h2, h3, header, input, label, li, option, p, section, select, span, strong, text, textarea, ul)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Random
import Url
import Workspace.Backend exposing (Backend, Context)
import Workspace.Comment as Comment
import Workspace.Permissions as Permissions
import Workspace.Serialize as Serialize
import Workspace.Table as Table
import Workspace.Types as Types
    exposing
        ( Access
        , Comments
        , Id
        , Meta
        , Principal(..)
        , Stored
        , Table
        , Visibility(..)
        )



-- CONFIG ---------------------------------------------------------------------


{-| JSON for the host document. -}
type alias DocCodec doc =
    { encode : doc -> E.Value
    , decoder : D.Decoder doc
    }


{-| Read-only information the workspace hands the host's editor view, so it can show comment markers
where conversation lives. -}
type alias EditorEnv =
    { comments : Comments
    , commentsVisible : Bool
    , commentCount : String -> Int
    }


{-| Everything the workspace needs to manage a particular document type.

  - `codec` / `empty` — persist and create the document.
  - `kind` — a short label stored on each document ("notebook", "chart", …).
  - `viewDoc` / `updateDoc` — the host's editor (its own `Html`/message).
  - `activate` — run after a document is created or loaded (e.g. a notebook re-runs its cells);
    use `identity` if nothing is needed.
  - `elementsOf` — the `(key, label)` of the document's commentable elements.
  - `toTable` — a document as a table for export (`Nothing` ⇒ no export offered).
  - `onImport` — apply an imported / queried table to the document (`Nothing` ⇒ no import offered).

-}
type alias Config doc docMsg =
    { codec : DocCodec doc
    , empty : doc
    , kind : String
    , activate : doc -> doc
    , viewDoc : EditorEnv -> doc -> Html docMsg
    , updateDoc : docMsg -> doc -> doc
    , elementsOf : doc -> List ( String, String )
    , toTable : doc -> Maybe Table
    , onImport : Maybe (Table -> doc -> doc)
    }



-- MODEL ----------------------------------------------------------------------


type Page
    = Browsing
    | Editing


type Dialog
    = NoDialog
    | PermissionsDialog
    | ImportDialog
    | QueryDialog


type Role
    = OwnersRole
    | ReadersRole


{-| The workspace state. Generic over the document type. -}
type alias Model doc =
    { metas : List Meta
    , open : Maybe (Stored doc)
    , page : Page
    , search : String
    , dialog : Dialog
    , commentsVisible : Bool
    , drafts : Dict String String
    , principalDraft : String
    , principalKind : String
    , urlDraft : String
    , urlFormat : String
    , sqlDraft : String
    , notice : Maybe String
    , pending : Maybe doc
    }


{-| Start the workspace: an empty model that immediately asks the backend for the document index. -}
init : Backend (Msg docMsg) -> ( Model doc, Cmd (Msg docMsg) )
init backend =
    ( { metas = []
      , open = Nothing
      , page = Browsing
      , search = ""
      , dialog = NoDialog
      , commentsVisible = False
      , drafts = Dict.empty
      , principalDraft = ""
      , principalKind = "user"
      , urlDraft = ""
      , urlFormat = "json"
      , sqlDraft = ""
      , notice = Nothing
      , pending = Nothing
      }
    , backend.listMetas GotMetas
    )


{-| Open a specific document by id (e.g. from a deep link). -}
openDocument : Backend (Msg docMsg) -> Id -> Cmd (Msg docMsg)
openDocument backend id =
    backend.load id GotDoc


{-| Create and open a **new document from an existing value** — e.g. a host copying a standalone
playground into the workspace. Mints a fresh UUID, then (via the returned `Cmd`) adds and opens it.
Call it directly and merge the returned model/`Cmd` into your own update. -}
createFrom : doc -> Model doc -> ( Model doc, Cmd (Msg docMsg) )
createFrom doc model =
    ( { model | pending = Just doc }, Random.generate CreatedStaged uuidGenerator )



-- UPDATE ---------------------------------------------------------------------


{-| The workspace's messages, generic over the host's document message (carried by `DocMsg`). -}
type Msg docMsg
    = NoOp
    | GotMetas (Result String (List Meta))
    | GotDoc (Result String String)
    | GotForDuplicate Id (Result String String)
    | New
    | CreateFresh Id
    | CreatedStaged Id
    | StartDuplicate Id Id
    | Open Id
    | Close
    | Delete Id
    | CopyDoc Id
    | SetSearch String
    | Rename String
    | DocMsg docMsg
    | OpenDialog Dialog
    | CloseDialog
    | SetVisibility String
    | SetPrincipalDraft String
    | SetPrincipalKind String
    | AddPrincipal Role
    | RemovePrincipal Role Principal
    | ToggleComments
    | SetDraft String String
    | SubmitComment String (Maybe Int)
    | SetUrlDraft String
    | SetUrlFormat String
    | SubmitImport
    | GotImport (Result String String)
    | SetSqlDraft String
    | RunQuery
    | GotQuery (Result String Table)
    | ExportExcel
    | DismissNotice


{-| Advance the workspace. `Config` and `Backend` come from the host; `Context` is the acting user. -}
update : Config doc docMsg -> Backend (Msg docMsg) -> Context -> Msg docMsg -> Model doc -> ( Model doc, Cmd (Msg docMsg) )
update config backend ctx msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotMetas (Ok metas) ->
            ( { model | metas = metas }, Cmd.none )

        GotMetas (Err e) ->
            ( { model | notice = Just ("Could not list documents: " ++ e) }, Cmd.none )

        GotDoc (Ok json) ->
            case decodeStored config json of
                Ok stored ->
                    ( { model
                        | open = Just (Types.setDoc (config.activate stored.doc) stored)
                        , page = Editing
                        , dialog = NoDialog
                        , notice = Nothing
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model | notice = Just ("Could not open document: " ++ e) }, Cmd.none )

        GotDoc (Err e) ->
            ( { model | notice = Just e }, Cmd.none )

        GotForDuplicate newId (Ok json) ->
            case decodeStored config json of
                Ok stored ->
                    let
                        meta =
                            { id = newId
                            , name = "Copy of " ++ displayName stored.meta.name
                            , kind = stored.meta.kind
                            , access = Types.defaultAccess ctx.user
                            }

                        copy =
                            { meta = meta, doc = stored.doc, comments = Dict.empty }

                        metas =
                            model.metas ++ [ meta ]
                    in
                    ( { model | metas = metas, notice = Just ("Copied to “" ++ meta.name ++ "”.") }
                    , Cmd.batch [ saveDoc config backend copy, backend.saveIndex metas ]
                    )

                Err e ->
                    ( { model | notice = Just ("Could not copy: " ++ e) }, Cmd.none )

        GotForDuplicate _ (Err e) ->
            ( { model | notice = Just ("Could not copy: " ++ e) }, Cmd.none )

        New ->
            ( model, Random.generate CreateFresh uuidGenerator )

        CreateFresh id ->
            openNew config backend ctx id config.empty model

        CreatedStaged id ->
            case model.pending of
                Just doc ->
                    openNew config backend ctx id doc { model | pending = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        StartDuplicate sourceId newId ->
            ( model, backend.load sourceId (GotForDuplicate newId) )

        Open id ->
            ( model, backend.load id GotDoc )

        Close ->
            ( { model | page = Browsing, open = Nothing, dialog = NoDialog }, backend.listMetas GotMetas )

        Delete id ->
            let
                metas =
                    List.filter (\m -> m.id /= id) model.metas

                closed =
                    case model.open of
                        Just s ->
                            s.meta.id == id

                        Nothing ->
                            False
            in
            ( { model
                | metas = metas
                , open =
                    if closed then
                        Nothing

                    else
                        model.open
                , page =
                    if closed then
                        Browsing

                    else
                        model.page
              }
            , Cmd.batch [ backend.delete id, backend.saveIndex metas ]
            )

        CopyDoc id ->
            ( model, Random.generate (StartDuplicate id) uuidGenerator )

        SetSearch q ->
            ( { model | search = q }, Cmd.none )

        Rename name ->
            withOpen model <|
                \stored ->
                    let
                        meta =
                            Types.setName name stored.meta

                        updated =
                            Types.setMeta meta stored
                    in
                    persist config backend { model | open = Just updated, metas = replaceMeta meta model.metas }

        DocMsg dm ->
            withOpen model <|
                \stored ->
                    persist config backend
                        { model | open = Just (Types.setDoc (config.updateDoc dm stored.doc) stored) }

        OpenDialog dialog ->
            ( { model | dialog = dialog, notice = Nothing }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = NoDialog }, Cmd.none )

        ToggleComments ->
            ( { model | commentsVisible = not model.commentsVisible }, Cmd.none )

        SetVisibility v ->
            updateAccess config backend model (Types.setVisibility (parseVisibility v))

        SetPrincipalDraft s ->
            ( { model | principalDraft = s }, Cmd.none )

        SetPrincipalKind k ->
            ( { model | principalKind = k }, Cmd.none )

        AddPrincipal role ->
            let
                name =
                    String.trim model.principalDraft
            in
            if name == "" then
                ( model, Cmd.none )

            else
                let
                    p =
                        if model.principalKind == "group" then
                            Group name

                        else
                            User name

                    f =
                        case role of
                            OwnersRole ->
                                Types.addOwner p

                            ReadersRole ->
                                Types.addReader p
                in
                updateAccess config backend { model | principalDraft = "" } f

        RemovePrincipal role p ->
            let
                f =
                    case role of
                        OwnersRole ->
                            Types.removeOwner p

                        ReadersRole ->
                            Types.removeReader p
            in
            updateAccess config backend model f

        SetDraft key val ->
            ( { model | drafts = Dict.insert key val model.drafts }, Cmd.none )

        SubmitComment elementKey parent ->
            let
                key =
                    draftKey elementKey parent

                body =
                    Dict.get key model.drafts |> Maybe.withDefault "" |> String.trim
            in
            if body == "" then
                ( model, Cmd.none )

            else
                withOpen model <|
                    \stored ->
                        let
                            comments =
                                case parent of
                                    Just pid ->
                                        Comment.reply elementKey pid ctx.user body stored.comments

                                    Nothing ->
                                        Comment.add elementKey ctx.user body stored.comments
                        in
                        persist config
                            backend
                            { model
                                | open = Just (Types.setComments comments stored)
                                , drafts = Dict.remove key model.drafts
                            }

        SetUrlDraft s ->
            ( { model | urlDraft = s }, Cmd.none )

        SetUrlFormat f ->
            ( { model | urlFormat = f }, Cmd.none )

        SubmitImport ->
            if String.trim model.urlDraft == "" then
                ( { model | notice = Just "Enter a URL to import." }, Cmd.none )

            else
                ( model, backend.fetchUrl (String.trim model.urlDraft) GotImport )

        GotImport (Ok body) ->
            let
                table =
                    if model.urlFormat == "csv" then
                        Ok (Table.fromCsv body)

                    else
                        Table.fromJson body
            in
            case table of
                Ok t ->
                    applyImport config backend { model | dialog = NoDialog, urlDraft = "" } t

                Err e ->
                    ( { model | notice = Just ("Could not parse data: " ++ e) }, Cmd.none )

        GotImport (Err e) ->
            ( { model | notice = Just ("Import failed: " ++ e) }, Cmd.none )

        SetSqlDraft s ->
            ( { model | sqlDraft = s }, Cmd.none )

        RunQuery ->
            case backend.query of
                Just runQuery ->
                    if String.trim model.sqlDraft == "" then
                        ( { model | notice = Just "Enter a query." }, Cmd.none )

                    else
                        ( model, runQuery (String.trim model.sqlDraft) GotQuery )

                Nothing ->
                    ( { model | notice = Just "This workspace has no database connection." }, Cmd.none )

        GotQuery (Ok table) ->
            applyImport config backend { model | dialog = NoDialog } table

        GotQuery (Err e) ->
            ( { model | notice = Just ("Query failed: " ++ e) }, Cmd.none )

        ExportExcel ->
            case ( backend.exportExcel, Maybe.andThen (\s -> config.toTable s.doc) model.open ) of
                ( Just doExport, Just table ) ->
                    ( model, doExport { filename = exportName model ++ ".xlsx", table = table } )

                _ ->
                    ( { model | notice = Just "Excel export is not available here." }, Cmd.none )

        DismissNotice ->
            ( { model | notice = Nothing }, Cmd.none )



-- UPDATE HELPERS -------------------------------------------------------------


{-| Add a new document (with id `id` and body `doc`) to the workspace and open it. -}
openNew : Config doc docMsg -> Backend (Msg docMsg) -> Context -> Id -> doc -> Model doc -> ( Model doc, Cmd (Msg docMsg) )
openNew config backend ctx id doc model =
    let
        meta =
            Types.newMeta id "" config.kind ctx.user

        stored =
            { meta = meta, doc = doc, comments = Dict.empty }

        metas =
            model.metas ++ [ meta ]
    in
    ( { model | metas = metas, open = Just stored, page = Editing, dialog = NoDialog, notice = Nothing }
    , Cmd.batch [ saveDoc config backend stored, backend.saveIndex metas ]
    )


withOpen : Model doc -> (Stored doc -> ( Model doc, Cmd (Msg docMsg) )) -> ( Model doc, Cmd (Msg docMsg) )
withOpen model f =
    case model.open of
        Just stored ->
            f stored

        Nothing ->
            ( model, Cmd.none )


updateAccess : Config doc docMsg -> Backend (Msg docMsg) -> Model doc -> (Access -> Access) -> ( Model doc, Cmd (Msg docMsg) )
updateAccess config backend model f =
    withOpen model <|
        \stored ->
            let
                meta =
                    Types.setAccess (f stored.meta.access) stored.meta

                updated =
                    Types.setMeta meta stored
            in
            persist config backend { model | open = Just updated, metas = replaceMeta meta model.metas }


applyImport : Config doc docMsg -> Backend (Msg docMsg) -> Model doc -> Table -> ( Model doc, Cmd (Msg docMsg) )
applyImport config backend model table =
    case ( config.onImport, model.open ) of
        ( Just apply, Just stored ) ->
            persist config backend
                { model | open = Just (Types.setDoc (config.activate (apply table stored.doc)) stored) }

        _ ->
            ( { model | notice = Just "This document does not support importing data." }, Cmd.none )


{-| Persist the open document and the index after a change. -}
persist : Config doc docMsg -> Backend (Msg docMsg) -> Model doc -> ( Model doc, Cmd (Msg docMsg) )
persist config backend model =
    case model.open of
        Just stored ->
            ( model, Cmd.batch [ saveDoc config backend stored, backend.saveIndex model.metas ] )

        Nothing ->
            ( model, backend.saveIndex model.metas )


saveDoc : Config doc docMsg -> Backend (Msg docMsg) -> Stored doc -> Cmd (Msg docMsg)
saveDoc config backend stored =
    backend.save stored.meta.id (E.encode 0 (Serialize.encodeStored config.codec.encode stored))


decodeStored : Config doc docMsg -> String -> Result String (Stored doc)
decodeStored config json =
    D.decodeString (Serialize.storedDecoder config.codec.decoder) json
        |> Result.mapError D.errorToString


replaceMeta : Meta -> List Meta -> List Meta
replaceMeta meta metas =
    List.map
        (\m ->
            if m.id == meta.id then
                meta

            else
                m
        )
        metas


parseVisibility : String -> Visibility
parseVisibility s =
    if s == "public" then
        Public

    else
        Private


draftKey : String -> Maybe Int -> String
draftKey elementKey parent =
    case parent of
        Just pid ->
            elementKey ++ "/" ++ String.fromInt pid

        Nothing ->
            elementKey


displayName : String -> String
displayName name =
    if String.trim name == "" then
        "Untitled"

    else
        name


exportName : Model doc -> String
exportName model =
    case model.open of
        Just stored ->
            sanitize (displayName stored.meta.name)

        Nothing ->
            "document"


sanitize : String -> String
sanitize name =
    String.toList name
        |> List.map
            (\c ->
                if Char.isAlphaNum c then
                    c

                else
                    '-'
            )
        |> String.fromList


{-| A random v4-style UUID, e.g. `3f2504e0-4f89-41d3-9a0c-0305e82c3301`. Each document is created
with one, so it has a stable, unique, URL-safe id (used as `#doc/<id>` in the browser hosts). -}
uuidGenerator : Random.Generator String
uuidGenerator =
    Random.list 32 (Random.int 0 15)
        |> Random.map (List.indexedMap versioned >> List.map hexChar >> String.fromList >> dashed)


{-| Force the version nibble (index 12 → 4) and the variant nibble (index 16 → 8..b). -}
versioned : Int -> Int -> Int
versioned i n =
    if i == 12 then
        4

    else if i == 16 then
        8 + modBy 4 n

    else
        n


hexChar : Int -> Char
hexChar n =
    String.toList "0123456789abcdef"
        |> List.drop (modBy 16 n)
        |> List.head
        |> Maybe.withDefault '0'


dashed : String -> String
dashed s =
    String.join "-"
        [ String.slice 0 8 s
        , String.slice 8 12 s
        , String.slice 12 16 s
        , String.slice 16 20 s
        , String.slice 20 32 s
        ]



-- SUBSCRIPTIONS --------------------------------------------------------------


{-| The workspace has no subscriptions of its own. -}
subscriptions : Model doc -> Sub (Msg docMsg)
subscriptions _ =
    Sub.none



-- VIEW -----------------------------------------------------------------------


{-| What the injected backend can do, derived once for the view (avoids comparing `Maybe`-of-function
fields with `==`, which Elm forbids). -}
type alias Caps =
    { hasQuery : Bool
    , hasExcel : Bool
    }


caps : Backend (Msg docMsg) -> Caps
caps backend =
    { hasQuery =
        case backend.query of
            Just _ ->
                True

            Nothing ->
                False
    , hasExcel =
        case backend.exportExcel of
            Just _ ->
                True

            Nothing ->
                False
    }


{-| Render the workspace (the browse list, or the open document's editor, plus any dialog). The
backend is passed so the view can gate the database/Excel actions on what it supports. -}
view : Config doc docMsg -> Backend (Msg docMsg) -> Context -> Model doc -> Html (Msg docMsg)
view config backend ctx model =
    let
        c =
            caps backend
    in
    div [ HA.class "ws-root" ]
        [ noticeBar model.notice
        , case model.page of
            Browsing ->
                browseView config ctx model

            Editing ->
                editorView config c ctx model
        , dialogView config c ctx model
        ]


noticeBar : Maybe String -> Html (Msg docMsg)
noticeBar notice =
    case notice of
        Just message ->
            div [ HA.class "ws-notice" ]
                [ span [] [ text message ]
                , button [ HA.class "ws-notice-x", HE.onClick DismissNotice ] [ text "×" ]
                ]

        Nothing ->
            text ""



-- BROWSE ---------------------------------------------------------------------


browseView : Config doc docMsg -> Context -> Model doc -> Html (Msg docMsg)
browseView config ctx model =
    let
        rows =
            visibleMetas ctx model
    in
    section [ HA.class "ws-browse" ]
        [ div [ HA.class "ws-browse-bar" ]
            [ input
                [ HA.class "ws-search"
                , HA.placeholder "Search documents…"
                , HA.value model.search
                , HE.onInput SetSearch
                ]
                []
            , button [ HA.class "ws-btn ws-btn-primary", HE.onClick New ] [ text "+ New" ]
            ]
        , if List.isEmpty rows then
            div [ HA.class "ws-empty" ]
                [ text
                    (if model.search == "" then
                        "No documents yet — create one to get started."

                     else
                        "No documents match your search."
                    )
                ]

          else
            ul [ HA.class "ws-list" ] (List.map (metaRow ctx) rows)
        ]


metaRow : Context -> Meta -> Html (Msg docMsg)
metaRow ctx meta =
    li [ HA.class "ws-row" ]
        [ div [ HA.class "ws-row-main", HE.onClick (Open meta.id) ]
            [ span [ HA.class "ws-row-name" ] [ text (displayName meta.name) ]
            , span [ HA.class "ws-row-kind" ] [ text meta.kind ]
            , visibilityBadge meta.access.visibility
            ]
        , div [ HA.class "ws-row-actions" ]
            [ iconButton "ws-icon" "Open" "bi-folder2-open" (Open meta.id)
            , iconButton "ws-icon" "Make a copy" "bi-files" (CopyDoc meta.id)
            , if Permissions.canWrite ctx meta.access then
                iconButton "ws-icon ws-icon-danger" "Delete" "bi-trash" (Delete meta.id)

              else
                text ""
            ]
        ]


iconButton : String -> String -> String -> Msg docMsg -> Html (Msg docMsg)
iconButton cls titleText icon msg =
    button [ HA.class ("ws-btn " ++ cls), HA.title titleText, HE.onClick msg ]
        [ Html.i [ HA.class ("bi " ++ icon) ] [] ]


visibilityBadge : Visibility -> Html (Msg docMsg)
visibilityBadge v =
    span
        [ HA.class
            ("ws-badge "
                ++ (if v == Public then
                        "ws-badge-public"

                    else
                        "ws-badge-private"
                   )
            )
        ]
        [ text (Types.visibilityLabel v) ]


visibleMetas : Context -> Model doc -> List Meta
visibleMetas ctx model =
    let
        q =
            String.toLower (String.trim model.search)
    in
    model.metas
        |> List.filter (\m -> Permissions.canRead ctx m.access)
        |> List.filter
            (\m ->
                q == "" || String.contains q (String.toLower (displayName m.name))
            )



-- EDITOR ---------------------------------------------------------------------


editorView : Config doc docMsg -> Caps -> Context -> Model doc -> Html (Msg docMsg)
editorView config c ctx model =
    case model.open of
        Nothing ->
            browseView config ctx model

        Just stored ->
            let
                writable =
                    Permissions.canWrite ctx stored.meta.access

                env =
                    { comments = stored.comments
                    , commentsVisible = model.commentsVisible
                    , commentCount = \k -> Comment.countFor k stored.comments
                    }
            in
            section [ HA.class "ws-editor" ]
                [ editorBar config c ctx model stored writable
                , div [ HA.class "ws-editor-body" ]
                    [ div [ HA.class "ws-doc" ]
                        [ Html.map DocMsg (config.viewDoc env stored.doc) ]
                    , if model.commentsVisible then
                        commentsPanel config ctx stored model.drafts

                      else
                        text ""
                    ]
                ]


editorBar : Config doc docMsg -> Caps -> Context -> Model doc -> Stored doc -> Bool -> Html (Msg docMsg)
editorBar config c ctx model stored writable =
    let
        commentTotal =
            Comment.total stored.comments

        table =
            config.toTable stored.doc
    in
    div [ HA.class "ws-editor-bar" ]
        [ button [ HA.class "ws-btn", HE.onClick Close ] [ text "← All documents" ]
        , input
            [ HA.class "ws-name"
            , HA.value stored.meta.name
            , HA.placeholder "Untitled"
            , HA.disabled (not writable)
            , HE.onInput Rename
            ]
            []
        , div [ HA.class "ws-editor-tools" ]
            [ button
                [ HA.class
                    ("ws-btn"
                        ++ (if model.commentsVisible then
                                " ws-btn-on"

                            else
                                ""
                           )
                    )
                , HA.title "Show / hide comments"
                , HE.onClick ToggleComments
                ]
                [ Html.i [ HA.class "bi bi-chat-dots" ] []
                , text
                    (if commentTotal > 0 then
                        " " ++ String.fromInt commentTotal

                     else
                        ""
                    )
                ]
            , button [ HA.class "ws-btn", HE.onClick (OpenDialog PermissionsDialog) ] [ text "Share" ]
            , case config.onImport of
                Just _ ->
                    button [ HA.class "ws-btn", HE.onClick (OpenDialog ImportDialog) ] [ text "Import URL" ]

                Nothing ->
                    text ""
            , queryButton config
            , exportLinks config c model stored table
            , button [ HA.class "ws-btn", HE.onClick (CopyDoc stored.meta.id) ] [ text "Copy" ]
            , if writable then
                button [ HA.class "ws-btn ws-btn-danger", HE.onClick (Delete stored.meta.id) ] [ text "Delete" ]

              else
                text ""
            ]
        ]


queryButton : Config doc docMsg -> Html (Msg docMsg)
queryButton config =
    case config.onImport of
        Just _ ->
            button [ HA.class "ws-btn", HE.onClick (OpenDialog QueryDialog) ] [ text "SQL" ]

        Nothing ->
            text ""


exportLinks : Config doc docMsg -> Caps -> Model doc -> Stored doc -> Maybe Table -> Html (Msg docMsg)
exportLinks config c model stored table =
    case table of
        Just t ->
            span [ HA.class "ws-export" ]
                [ downloadLink "CSV" (exportName model ++ ".csv") "text/csv" (Table.toCsv t)
                , downloadLink "JSON" (exportName model ++ ".json") "application/json" (Table.toJson t)
                , if c.hasExcel then
                    button [ HA.class "ws-btn", HE.onClick ExportExcel ] [ text "Excel" ]

                  else
                    text ""
                ]

        Nothing ->
            text ""


downloadLink : String -> String -> String -> String -> Html (Msg docMsg)
downloadLink labelText filename mime content =
    a
        [ HA.class "ws-btn ws-dl"
        , HA.href ("data:" ++ mime ++ ";charset=utf-8," ++ Url.percentEncode content)
        , HA.attribute "download" filename
        ]
        [ text labelText ]



-- COMMENTS PANEL -------------------------------------------------------------


commentsPanel : Config doc docMsg -> Context -> Stored doc -> Dict String String -> Html (Msg docMsg)
commentsPanel config ctx stored drafts =
    div [ HA.class "ws-comments" ]
        [ h3 [ HA.class "ws-comments-title" ] [ text "Comments" ]
        , div [] (List.map (commentElement ctx stored.comments drafts) (config.elementsOf stored.doc))
        ]


commentElement : Context -> Comments -> Dict String String -> ( String, String ) -> Html (Msg docMsg)
commentElement ctx comments drafts ( key, elabel ) =
    let
        threads =
            Dict.get key comments |> Maybe.withDefault []
    in
    div [ HA.class "ws-comment-el" ]
        [ div [ HA.class "ws-comment-el-label" ] [ text elabel ]
        , div [ HA.class "ws-thread" ] (List.map (commentNode ctx key drafts) threads)
        , composer drafts key Nothing "Add a comment…"
        ]


commentNode : Context -> String -> Dict String String -> Types.Comment -> Html (Msg docMsg)
commentNode ctx key drafts node =
    div [ HA.class "ws-comment" ]
        [ div [ HA.class "ws-comment-head" ]
            [ strong [] [ text (Types.commentAuthor node) ] ]
        , div [ HA.class "ws-comment-body" ] [ text (Types.commentBody node) ]
        , div [ HA.class "ws-replies" ]
            (List.map (commentNode ctx key drafts) (Types.commentReplies node))
        , composer drafts key (Just (Types.commentId node)) "Reply…"
        ]


composer : Dict String String -> String -> Maybe Int -> String -> Html (Msg docMsg)
composer drafts key parent ph =
    let
        dkey =
            draftKey key parent
    in
    div [ HA.class "ws-composer" ]
        [ input
            [ HA.class "ws-composer-input"
            , HA.placeholder ph
            , HA.value (Dict.get dkey drafts |> Maybe.withDefault "")
            , HE.onInput (SetDraft dkey)
            ]
            []
        , button [ HA.class "ws-btn", HE.onClick (SubmitComment key parent) ] [ text "Post" ]
        ]



-- DIALOGS --------------------------------------------------------------------


dialogView : Config doc docMsg -> Caps -> Context -> Model doc -> Html (Msg docMsg)
dialogView config c ctx model =
    case ( model.dialog, model.open ) of
        ( PermissionsDialog, Just stored ) ->
            modal "Sharing & permissions" (permissionsBody model stored.meta.access)

        ( ImportDialog, _ ) ->
            modal "Import data from a URL" (importBody model)

        ( QueryDialog, _ ) ->
            modal "Run a SQL query" (queryBody c model)

        _ ->
            text ""


modal : String -> Html (Msg docMsg) -> Html (Msg docMsg)
modal title body =
    -- NB: the backdrop deliberately has no click-to-close handler — relying on click bubbling here
    -- closed the dialog whenever a form control inside it was clicked. Close via × or the buttons.
    div [ HA.class "ws-overlay" ]
        [ div [ HA.class "ws-modal" ]
            [ div [ HA.class "ws-modal-head" ]
                [ h2 [ HA.class "ws-modal-title" ] [ text title ]
                , button [ HA.class "ws-modal-x", HE.onClick CloseDialog ] [ text "×" ]
                ]
            , div [ HA.class "ws-modal-body" ] [ body ]
            ]
        ]


permissionsBody : Model doc -> Access -> Html (Msg docMsg)
permissionsBody model access =
    div []
        [ div [ HA.class "ws-field" ]
            [ label [ HA.class "ws-label" ] [ text "Visibility" ]
            , select [ HA.class "ws-select", HE.onInput SetVisibility ]
                [ option [ HA.value "private", HA.selected (access.visibility == Private) ] [ text "Private — only people below" ]
                , option [ HA.value "public", HA.selected (access.visibility == Public) ] [ text "Public — any logged-in user can read" ]
                ]
            ]
        , principalSection "Owners (can edit & share)" OwnersRole access.owners
        , principalSection "Readers (can view)" ReadersRole access.readers
        , div [ HA.class "ws-field" ]
            [ label [ HA.class "ws-label" ] [ text "Add someone" ]
            , div [ HA.class "ws-add-row" ]
                [ select [ HA.class "ws-select ws-select-kind", HE.onInput SetPrincipalKind ]
                    [ option [ HA.value "user", HA.selected (model.principalKind == "user") ] [ text "User" ]
                    , option [ HA.value "group", HA.selected (model.principalKind == "group") ] [ text "Group" ]
                    ]
                , input
                    [ HA.class "ws-input"
                    , HA.placeholder "id or group name"
                    , HA.value model.principalDraft
                    , HE.onInput SetPrincipalDraft
                    ]
                    []
                , button [ HA.class "ws-btn", HE.onClick (AddPrincipal OwnersRole) ] [ text "+ Owner" ]
                , button [ HA.class "ws-btn", HE.onClick (AddPrincipal ReadersRole) ] [ text "+ Reader" ]
                ]
            ]
        , div [ HA.class "ws-modal-actions" ]
            [ button [ HA.class "ws-btn ws-btn-primary", HE.onClick CloseDialog ] [ text "Done" ] ]
        ]


principalSection : String -> Role -> List Principal -> Html (Msg docMsg)
principalSection title role principals =
    div [ HA.class "ws-field" ]
        [ label [ HA.class "ws-label" ] [ text title ]
        , if List.isEmpty principals then
            div [ HA.class "ws-muted" ] [ text "—" ]

          else
            div [ HA.class "ws-chips" ] (List.map (principalChip role) principals)
        ]


principalChip : Role -> Principal -> Html (Msg docMsg)
principalChip role p =
    span [ HA.class "ws-chip" ]
        [ text (Types.principalLabel p)
        , button [ HA.class "ws-chip-x", HE.onClick (RemovePrincipal role p) ] [ text "×" ]
        ]


importBody : Model doc -> Html (Msg docMsg)
importBody model =
    div []
        [ div [ HA.class "ws-field" ]
            [ label [ HA.class "ws-label" ] [ text "Data URL" ]
            , input
                [ HA.class "ws-input"
                , HA.placeholder "https://example.com/data.json"
                , HA.value model.urlDraft
                , HE.onInput SetUrlDraft
                ]
                []
            ]
        , div [ HA.class "ws-field" ]
            [ label [ HA.class "ws-label" ] [ text "Format" ]
            , select [ HA.class "ws-select", HE.onInput SetUrlFormat ]
                [ option [ HA.value "json", HA.selected (model.urlFormat == "json") ] [ text "JSON (array of objects)" ]
                , option [ HA.value "csv", HA.selected (model.urlFormat == "csv") ] [ text "CSV / TSV" ]
                ]
            ]
        , div [ HA.class "ws-modal-actions" ]
            [ button [ HA.class "ws-btn ws-btn-primary", HE.onClick SubmitImport ] [ text "Import" ]
            , button [ HA.class "ws-btn", HE.onClick CloseDialog ] [ text "Cancel" ]
            ]
        , p [ HA.class "ws-muted" ] [ text "The URL must be reachable from the browser (CORS-enabled)." ]
        ]


queryBody : Caps -> Model doc -> Html (Msg docMsg)
queryBody c model =
    div []
        [ div [ HA.class "ws-field" ]
            [ label [ HA.class "ws-label" ] [ text "SQL" ]
            , textarea
                [ HA.class "ws-textarea"
                , HA.attribute "rows" "5"
                , HA.placeholder "SELECT name, age FROM users WHERE age > 18"
                , HA.value model.sqlDraft
                , HE.onInput SetSqlDraft
                ]
                []
            ]
        , div [ HA.class "ws-modal-actions" ]
            [ button
                [ HA.class "ws-btn ws-btn-primary"
                , HA.disabled (not c.hasQuery)
                , HE.onClick RunQuery
                ]
                [ text "Run query" ]
            , button [ HA.class "ws-btn", HE.onClick CloseDialog ] [ text "Cancel" ]
            ]
        , if c.hasQuery then
            p [ HA.class "ws-muted" ] [ text "The query runs on the workspace's database and the result is added to the document." ]

          else
            p [ HA.class "ws-muted" ] [ text "This workspace has no database connection, so running is disabled. With a database backend (e.g. in the bbx app) this runs the query and adds the result to the document." ]
        ]
