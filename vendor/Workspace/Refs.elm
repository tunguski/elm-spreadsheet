module Workspace.Refs exposing
    ( RefError(..), Resolver, Resolution
    , topoOrder, cycleFrom
    , resolve, closureIds, refErrorLabel
    )

{-| The **cross-document reference** engine: a document may pull data out of other documents (a
notebook step's result, a spreadsheet range, a SQL query's rows), named by a
[`DocRef`](Workspace-Types#DocRef). Those references form a directed graph over documents, and this
module is the part that keeps that graph honest and turns it into data.

Two jobs, both pure so they are unit-tested headlessly:

1.  **Cycle detection** ([`topoOrder`](#topoOrder) / [`cycleFrom`](#cycleFrom)). A document that
    (directly or transitively) references itself can never be evaluated — resolving it would require
    its own result as an input. The workspace must refuse to process such a graph and show the loop
    instead. `topoOrder` returns `Err` with the offending cycle, or an evaluation order with every
    document's dependencies ahead of it.

2.  **Resolution** ([`resolve`](#resolve)). Given the loaded closure of documents (the open one plus
    everything it reaches) and a [`Resolver`](#Resolver) of host hooks, walk the documents in
    dependency order: for each, gather its referenced tables (its dependencies are already resolved,
    so their results are available), `absorb` them into the document, and `activate` it (run it).
    The open document comes back with its references filled in.

The engine is generic over the host `doc`: it never inspects a document, only calls the host's
`references` / `provide` / `absorb` / `activate`. That is what lets one heterogeneous host (bbx)
resolve a notebook that reads a spreadsheet that reads a SQL query, while each standalone site
resolves references among its own document type.

@docs RefError, Resolver, Resolution
@docs topoOrder, cycleFrom
@docs resolve, closureIds, refErrorLabel

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Workspace.Types as Types exposing (DocRef, Id, Selector, Stored, Table)


{-| Why a reference graph could not be resolved. Currently only a cycle blocks resolution; a missing
or unreadable target is reported as a per-reference warning instead (see
[`Resolution`](#Resolution)), so the rest of the document still evaluates. -}
type RefError
    = Cycle (List Id)


{-| The host hooks the engine needs, one per document type (a heterogeneous host dispatches inside
each on the document's variant):

  - `references` — the document's outgoing references.
  - `provide` — satisfy a [`Selector`](Workspace-Types#Selector) against a document, yielding its
    table (or an error string if the step/range does not exist).
  - `absorb` — inject the resolved reference tables (keyed by each reference's `binding`) into a
    document, so evaluating it can see them.
  - `activate` — evaluate the document (run its cells / recalc its cells). Runs after `absorb`.

-}
type alias Resolver doc =
    { references : doc -> List DocRef
    , provide : Selector -> doc -> Result String Table
    , absorb : Dict String Table -> doc -> doc
    , activate : doc -> doc
    }


{-| The outcome of a successful [`resolve`](#resolve): every document in the closure with its
references absorbed and itself activated, plus any soft warnings (a reference whose target was
missing from the closure or whose selector did not resolve). -}
type alias Resolution doc =
    { docs : Dict Id (Stored doc)
    , warnings : List String
    }



-- GRAPH ----------------------------------------------------------------------


{-| A depth-first search from `root` looking for a back-edge; returns the nodes on the first cycle
found (in encounter order), or `Nothing` if everything reachable from `root` is acyclic. `edges`
gives a node's out-neighbours. -}
cycleFrom : (Id -> List Id) -> Id -> Maybe (List Id)
cycleFrom edges root =
    dfsCycle edges root [] Set.empty
        |> Tuple.first


dfsCycle : (Id -> List Id) -> Id -> List Id -> Set Id -> ( Maybe (List Id), Set Id )
dfsCycle edges node path done =
    if List.member node path then
        -- `node` is on the active path: the cycle is from its earlier occurrence forward to here.
        ( Just (cycleSlice node (List.reverse (node :: path))), done )

    else if Set.member node done then
        ( Nothing, done )

    else
        let
            ( found, done2 ) =
                List.foldl
                    (\child ( acc, d ) ->
                        case acc of
                            Just _ ->
                                ( acc, d )

                            Nothing ->
                                dfsCycle edges child (node :: path) d
                    )
                    ( Nothing, done )
                    (edges node)
        in
        ( found, Set.insert node done2 )


{-| The slice of `chain` from the first occurrence of `node` to its end (the cycle body). -}
cycleSlice : Id -> List Id -> List Id
cycleSlice node chain =
    case chain of
        [] ->
            []

        x :: rest ->
            if x == node then
                chain

            else
                cycleSlice node rest


{-| A topological order of everything reachable from `roots`, dependencies first (a node appears
after every node it points to). `Err` carries a cycle if the graph has one. -}
topoOrder : (Id -> List Id) -> List Id -> Result (List Id) (List Id)
topoOrder edges roots =
    let
        visitAll acc =
            List.foldl
                (\root state ->
                    case state of
                        Err e ->
                            Err e

                        Ok s ->
                            visit edges root [] s
                )
                (Ok acc)
                roots
    in
    case visitAll { temp = Set.empty, done = Set.empty, order = [] } of
        Err e ->
            Err e

        Ok final ->
            -- `order` was built dependents-first (each node prepended after its children); reverse
            -- for dependencies-first.
            Ok (List.reverse final.order)


type alias Visit =
    { temp : Set Id, done : Set Id, order : List Id }


visit : (Id -> List Id) -> Id -> List Id -> Visit -> Result (List Id) Visit
visit edges node path state =
    if Set.member node state.done then
        Ok state

    else if Set.member node state.temp then
        Err (cycleSlice node (List.reverse (node :: path)))

    else
        let
            marked =
                { state | temp = Set.insert node state.temp }

            children =
                List.foldl
                    (\child acc ->
                        case acc of
                            Err e ->
                                Err e

                            Ok s ->
                                visit edges child (node :: path) s
                    )
                    (Ok marked)
                    (edges node)
        in
        case children of
            Err e ->
                Err e

            Ok s ->
                Ok
                    { s
                        | temp = Set.remove node s.temp
                        , done = Set.insert node s.done
                        , order = node :: s.order
                    }



-- RESOLUTION -----------------------------------------------------------------


{-| The ids of every document reachable from `root` through references present in `cache` (including
`root` itself). Used by the shell to know which documents to load. -}
closureIds : (Id -> List Id) -> Id -> List Id
closureIds edges root =
    Set.toList (reach edges root Set.empty)


reach : (Id -> List Id) -> Id -> Set Id -> Set Id
reach edges node seen =
    if Set.member node seen then
        seen

    else
        List.foldl (\child acc -> reach edges child acc) (Set.insert node seen) (edges node)


{-| Resolve the open document (`openId`) and its dependency closure held in `cache`.

Returns `Err (Cycle …)` if the closure contains a loop — the caller shows it and does **not**
evaluate. Otherwise every document is absorbed + activated in dependency order and returned; a
reference to a document not in `cache`, or a selector that does not resolve, becomes a `warning`
rather than a hard failure. -}
resolve : Resolver doc -> Id -> Dict Id (Stored doc) -> Result RefError (Resolution doc)
resolve r openId cache =
    let
        edges id =
            case Dict.get id cache of
                Just stored ->
                    r.references stored.doc
                        |> List.map .docId
                        |> List.filter (\d -> Dict.member d cache)

                Nothing ->
                    []
    in
    case topoOrder edges [ openId ] of
        Err cyc ->
            Err (Cycle cyc)

        Ok order ->
            Ok (List.foldl (resolveOne r cache) { docs = Dict.empty, warnings = [] } order)


resolveOne : Resolver doc -> Dict Id (Stored doc) -> Id -> Resolution doc -> Resolution doc
resolveOne r cache id acc =
    case Dict.get id cache of
        Nothing ->
            acc

        Just stored ->
            let
                ( tables, warnings ) =
                    List.foldl (gather r acc.docs) ( Dict.empty, acc.warnings ) (r.references stored.doc)

                evaluated =
                    r.activate (r.absorb tables stored.doc)
            in
            { docs = Dict.insert id (Types.setDoc evaluated stored) acc.docs
            , warnings = warnings
            }


gather : Resolver doc -> Dict Id (Stored doc) -> DocRef -> ( Dict String Table, List String ) -> ( Dict String Table, List String )
gather r resolved ref ( tables, warnings ) =
    case Dict.get ref.docId resolved of
        Just target ->
            case r.provide ref.selector target.doc of
                Ok table ->
                    ( Dict.insert ref.binding table tables, warnings )

                Err e ->
                    ( tables, warnings ++ [ ref.binding ++ ": " ++ e ] )

        Nothing ->
            ( tables, warnings ++ [ "referenced document " ++ ref.docId ++ " is not available" ] )


{-| A human sentence describing a [`RefError`](#RefError) for the cycle banner. The cycle list
already closes the loop (its first and last id are the same), so it reads directly as `A → B → A`. -}
refErrorLabel : RefError -> String
refErrorLabel err =
    case err of
        Cycle ids ->
            "Reference cycle: " ++ String.join " → " ids
