module Workspace.Backend exposing (Backend, Context)

{-| The two records the **host** injects so the workspace component runs unchanged everywhere.

[`Context`](#Context) is who is acting. [`Backend`](#Backend) is how documents are persisted and
how the optional database/export effects are performed — a record of effect builders, generic over
the host's `msg`, so it carries no import cycle with the component's own `Msg`.

  - The public **site** supplies [`Workspace.Browser.backend`](Workspace-Browser#backend)
    (localStorage) with `query = Nothing` and `exportExcel = Nothing`.
  - **bbx** will supply an HTTP backend that talks to its database, with `query` and `exportExcel`
    wired — and the component will not change.

Reads (`listMetas`, `load`, `fetchUrl`, `query`) take a result-tagging message. Writes (`save`,
`delete`, `saveIndex`) are fire-and-forget: the component's in-memory model is the source of truth
and updates optimistically. Documents cross this boundary as **raw JSON strings**, so `Backend`
stays free of the host's `doc` type; the component (de)serialises with the host's codec. New
document ids are random UUIDs minted by the component, so there is no `newId` to supply.

@docs Backend, Context

-}

import Workspace.Types exposing (Id, Meta, Table)


{-| Who is acting: the current user and the groups they belong to. The site injects a local
pseudo-user; bbx injects the logged-in user. -}
type alias Context =
    { user : String
    , groups : List String
    }


{-| The host's persistence + optional effects. -}
type alias Backend msg =
    { listMetas : (Result String (List Meta) -> msg) -> Cmd msg
    , load : Id -> (Result String String -> msg) -> Cmd msg
    , save : Id -> String -> Cmd msg
    , delete : Id -> Cmd msg
    , saveIndex : List Meta -> Cmd msg
    , fetchUrl : String -> (Result String String -> msg) -> Cmd msg
    , query : Maybe (String -> (Result String Table -> msg) -> Cmd msg)
    , exportExcel : Maybe ({ filename : String, table : Table } -> Cmd msg)
    }
