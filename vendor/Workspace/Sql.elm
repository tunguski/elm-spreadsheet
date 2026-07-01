module Workspace.Sql exposing
    ( SqlDoc, SqlMsg, empty, previewLimit
    , update, view
    , encode, decoder
    , setResult, provide
    )

{-| A reusable **SQL-query document**: its content is one plain SQL query, and its output is that
query's result as a [`Table`](Workspace-Types#Table). Other documents (notebooks, spreadsheets) can
reference a SQL document to pull its result in — so a query written once is reused everywhere.

The query runs through the host's [`Backend.query`](Workspace-Backend#Backend): on the public site
there is no database, so the run action is disabled and the document just holds its text; in **bbx**
the backend executes it against the real database. Running is an *effect*, but the reference engine
([`Workspace.Refs`](Workspace-Refs)) is *pure* — so the last result is **cached** on the document
(via [`setResult`](#setResult)) and [`provide`](#provide) hands that cache to referrers. Re-running
refreshes the cache.

For **presentation** the preview keeps at most [`previewLimit`](#previewLimit) (100) rows; the full
count is remembered so the view can say "showing 100 of 5,000".

This module is a plain document type — data, a codec, a pure `update` and a `view`. The effectful
"Run" lives in the workspace shell (it owns the backend), which stores the result back with
`setResult`.

@docs SqlDoc, SqlMsg, empty, previewLimit
@docs update, view
@docs encode, decoder
@docs setResult, provide

-}

import Html exposing (Html, div, p, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Workspace.Types exposing (Selector(..), Table)


{-| The most rows a SQL document previews (and caches for referrers). Executing a query for display
is capped here so a huge result stays presentable and cheap to store. -}
previewLimit : Int
previewLimit =
    100


{-| A SQL-query document: the query text, the last result (capped to [`previewLimit`](#previewLimit)
rows, `Nothing` until first run), and the full row count that result came from. -}
type alias SqlDoc =
    { sql : String
    , cached : Maybe Table
    , total : Int
    }


{-| A blank query document. -}
empty : SqlDoc
empty =
    { sql = "SELECT * FROM ", cached = Nothing, total = 0 }


{-| The only pure edit: change the query text. (Running is an effect the shell performs.) -}
type SqlMsg
    = SetSql String


{-| Apply a pure edit. -}
update : SqlMsg -> SqlDoc -> SqlDoc
update msg doc =
    case msg of
        SetSql s ->
            { doc | sql = s, cached = doc.cached, total = doc.total }


{-| Store a fresh query result: cache the first [`previewLimit`](#previewLimit) rows for display and
for referrers, and remember the total the result had. Called by the shell after `Backend.query`. -}
setResult : Table -> SqlDoc -> SqlDoc
setResult result doc =
    { doc
        | cached = Just { headers = result.headers, rows = List.take previewLimit result.rows }
        , total = List.length result.rows
    }


{-| Satisfy a reference to this document: only the whole result is meaningful, and only once the
query has been run. This is the SQL document's `provide` for [`Workspace.Refs`](Workspace-Refs). -}
provide : Selector -> SqlDoc -> Result String Table
provide selector doc =
    case selector of
        WholeDoc ->
            case doc.cached of
                Just t ->
                    Ok t

                Nothing ->
                    Err "query has not been run yet"

        _ ->
            Err "a SQL query has no steps or ranges — reference the whole result"



-- VIEW -----------------------------------------------------------------------


{-| The editor: a query textarea and a preview of the cached result (or a hint to run it). The Run
button lives in the shell toolbar because it needs the backend. -}
view : SqlDoc -> Html SqlMsg
view doc =
    div [ HA.class "sql-doc" ]
        [ textarea
            [ HA.class "sql-editor"
            , HA.attribute "rows" "6"
            , HA.attribute "spellcheck" "false"
            , HA.placeholder "SELECT name, age FROM users WHERE age > 18 ORDER BY age DESC"
            , HA.value doc.sql
            , HE.onInput SetSql
            ]
            []
        , resultView doc
        ]


resultView : SqlDoc -> Html SqlMsg
resultView doc =
    case doc.cached of
        Nothing ->
            p [ HA.class "sql-hint" ]
                [ text "Run this query to preview its result. The result is cached so other documents can reference it." ]

        Just t ->
            div [ HA.class "sql-result" ]
                [ resultCaption doc t
                , resultTable t
                ]


resultCaption : SqlDoc -> Table -> Html SqlMsg
resultCaption doc t =
    let
        shown =
            List.length t.rows

        label =
            if doc.total > shown then
                "Showing " ++ String.fromInt shown ++ " of " ++ String.fromInt doc.total ++ " rows (preview capped at " ++ String.fromInt previewLimit ++ ")"

            else
                String.fromInt shown ++ " row" ++ plural shown
    in
    p [ HA.class "sql-caption" ] [ text label ]


resultTable : Table -> Html SqlMsg
resultTable t =
    table [ HA.class "sql-table" ]
        [ thead [] [ tr [] (List.map (\h -> th [] [ text h ]) t.headers) ]
        , tbody [] (List.map (\row -> tr [] (List.map (\c -> td [] [ text c ]) row)) t.rows)
        ]


plural : Int -> String
plural n =
    if n == 1 then
        ""

    else
        "s"



-- JSON -----------------------------------------------------------------------


{-| Encode a SQL document (text + cached result + total). -}
encode : SqlDoc -> E.Value
encode doc =
    E.object
        (( "sql", E.string doc.sql )
            :: ( "total", E.int doc.total )
            :: (case doc.cached of
                    Just t ->
                        [ ( "cached", encodeTable t ) ]

                    Nothing ->
                        []
               )
        )


{-| Decode a SQL document. -}
decoder : D.Decoder SqlDoc
decoder =
    D.map3 (\s c tot -> { sql = s, cached = c, total = tot })
        (D.field "sql" D.string)
        (D.oneOf [ D.field "cached" (D.map Just tableDecoder), D.succeed Nothing ])
        (D.oneOf [ D.field "total" D.int, D.succeed 0 ])


encodeTable : Table -> E.Value
encodeTable t =
    E.object
        [ ( "headers", E.list E.string t.headers )
        , ( "rows", E.list (E.list E.string) t.rows )
        ]


tableDecoder : D.Decoder Table
tableDecoder =
    D.map2 (\h r -> { headers = h, rows = r })
        (D.field "headers" (D.list D.string))
        (D.field "rows" (D.list (D.list D.string)))
