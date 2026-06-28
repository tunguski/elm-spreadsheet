module Workspace.Browser exposing (backend)

{-| A [`Backend`](Workspace-Backend#Backend) that keeps documents in the browser's persistent
storage (`localStorage`) — the implementation the public **site** uses.

Each document is stored under its own key (`<ns>:doc:<id>`) and a small index of metadata under
`<ns>:index`, so the workspace can list documents without loading every body. Pass a `namespace`
so different apps sharing a browser (elm-notebook, elm-svg, …) keep separate stores.

There is no database and no export service in the browser, so `query` and `exportExcel` are
`Nothing` — the workspace shows those actions disabled. URL import works (via `Http`).

@docs backend

-}

import Http
import Json.Decode as D
import Json.Encode as E
import Storage
import Workspace.Backend exposing (Backend)
import Workspace.Serialize as Serialize
import Workspace.Types exposing (Id, Meta)


{-| Build a localStorage-backed backend under the given key namespace. -}
backend : String -> Backend msg
backend namespace =
    { listMetas = listMetas namespace
    , load = load namespace
    , save = save namespace
    , delete = delete namespace
    , saveIndex = saveIndex namespace
    , fetchUrl = fetchUrl
    , query = Nothing
    , exportExcel = Nothing
    }


indexKey : String -> String
indexKey ns =
    ns ++ ":index"


docKey : String -> Id -> String
docKey ns id =
    ns ++ ":doc:" ++ id


listMetas : String -> (Result String (List Meta) -> msg) -> Cmd msg
listMetas ns tagger =
    Storage.load (indexKey ns)
        (\maybe ->
            case maybe of
                Nothing ->
                    tagger (Ok [])

                Just "" ->
                    tagger (Ok [])

                Just json ->
                    tagger (D.decodeString Serialize.indexDecoder json |> Result.mapError D.errorToString)
        )


load : String -> Id -> (Result String String -> msg) -> Cmd msg
load ns id tagger =
    Storage.load (docKey ns id)
        (\maybe ->
            case maybe of
                Just "" ->
                    tagger (Err "Document not found")

                Just json ->
                    tagger (Ok json)

                Nothing ->
                    tagger (Err "Document not found")
        )


save : String -> Id -> String -> Cmd msg
save ns id json =
    Storage.save (docKey ns id) json


delete : String -> Id -> Cmd msg
delete ns id =
    -- localStorage has no "remove" primitive bound here; clearing the value is equivalent — the
    -- index (the source of truth for what exists) no longer references it.
    Storage.save (docKey ns id) ""


saveIndex : String -> List Meta -> Cmd msg
saveIndex ns metas =
    Storage.save (indexKey ns) (E.encode 0 (Serialize.encodeIndex metas))


fetchUrl : String -> (Result String String -> msg) -> Cmd msg
fetchUrl url tagger =
    Http.get
        { url = url
        , expect = Http.expectString (\res -> tagger (Result.mapError httpErrorToString res))
        }


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl u ->
            "Bad URL: " ++ u

        Http.Timeout ->
            "The request timed out"

        Http.NetworkError ->
            "Network error (is the URL reachable and CORS-enabled?)"

        Http.BadStatus code ->
            "Server returned status " ++ String.fromInt code

        Http.BadBody body ->
            "Unexpected response body: " ++ body
