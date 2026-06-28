module Spreadsheet.Deps exposing
    ( precedents
    , precedentsWith
    , topoSort
    )

{-| Dependency analysis over formula expressions.

`precedents` walks an `Expr` and returns every cell it reads — single refs directly,
and ranges expanded to their cells. The recalculator uses this to build a dependency
graph; `topoSort` then orders a set of dirty cells so that each is computed only after
everything it depends on, and reports any cells caught in a cycle (so they can be marked
`#CIRC!`).

@docs precedents, topoSort

-}

import Set exposing (Set)
import Spreadsheet.Ast exposing (Expr(..))
import Spreadsheet.Ref as Ref exposing (Ref)


{-| Every cell address an expression references, as `(col, row)` keys. Ranges are
expanded to all their member cells. Defined names contribute nothing (use
`precedentsWith` to resolve them). -}
precedents : Expr -> List ( Int, Int )
precedents expr =
    precedentsWith (\_ -> []) expr


{-| Like `precedents`, but a resolver maps each defined name to the cell keys it stands
for, so a formula referencing a named range still depends on that range's cells. -}
precedentsWith : (String -> List ( Int, Int )) -> Expr -> List ( Int, Int )
precedentsWith resolveName expr =
    precedentsHelp resolveName expr []


precedentsHelp : (String -> List ( Int, Int )) -> Expr -> List ( Int, Int ) -> List ( Int, Int )
precedentsHelp resolveName expr acc =
    case expr of
        Lit _ ->
            acc

        RefE ref _ ->
            ( ref.col, ref.row ) :: acc

        RangeE range _ _ ->
            List.foldl (\r a -> ( r.col, r.row ) :: a) acc (Ref.cellsOf range)

        NameE name ->
            resolveName name ++ acc

        SpillRefE ref ->
            -- A spill reference depends (at least) on its anchor; the rest of the spilled
            -- block is filled in by the same anchor, so the fix-point recalc settles it.
            ( ref.col, ref.row ) :: acc

        SheetRefE _ _ _ ->
            -- Cross-sheet refs are not local precedents; the workbook resolves them.
            acc

        SheetRangeE _ _ _ _ ->
            acc

        Unary _ sub ->
            precedentsHelp resolveName sub acc

        Binary _ a b ->
            precedentsHelp resolveName a (precedentsHelp resolveName b acc)

        Func _ args ->
            List.foldl (precedentsHelp resolveName) acc args


{-| Topologically sort the given keys using `depsOf` (which must return the precedents of
each key). Returns `(ordered, cyclic)`: `ordered` is a dependency-respecting order of the
acyclic part (dependencies first), and `cyclic` is the set of keys that take part in (or
depend on) a cycle.

Implemented as an iterative Kahn-style sort over the induced subgraph (only edges between
keys in the input set matter — external precedents are already-final values).
-}
topoSort : (( Int, Int ) -> List ( Int, Int )) -> List ( Int, Int ) -> ( List ( Int, Int ), Set ( Int, Int ) )
topoSort depsOf keys =
    let
        keySet =
            Set.fromList keys

        -- For each key, only its precedents that are *also* dirty keys constrain order.
        inducedDeps key =
            depsOf key
                |> List.filter (\k -> Set.member k keySet)
                |> dedup
    in
    kahn inducedDeps keys


kahn : (( Int, Int ) -> List ( Int, Int )) -> List ( Int, Int ) -> ( List ( Int, Int ), Set ( Int, Int ) )
kahn inducedDeps keys =
    kahnLoop inducedDeps keys Set.empty []


kahnLoop :
    (( Int, Int ) -> List ( Int, Int ))
    -> List ( Int, Int )
    -> Set ( Int, Int )
    -> List ( Int, Int )
    -> ( List ( Int, Int ), Set ( Int, Int ) )
kahnLoop inducedDeps remaining done acc =
    case remaining of
        [] ->
            ( List.reverse acc, Set.empty )

        _ ->
            let
                ( ready, notReady ) =
                    List.partition
                        (\key ->
                            List.all (\d -> Set.member d done) (inducedDeps key)
                        )
                        remaining
            in
            if List.isEmpty ready then
                -- Everything left is in or downstream of a cycle.
                ( List.reverse acc, Set.fromList notReady )

            else
                kahnLoop inducedDeps
                    notReady
                    (List.foldl Set.insert done ready)
                    (List.reverse ready ++ acc)


dedup : List ( Int, Int ) -> List ( Int, Int )
dedup xs =
    Set.toList (Set.fromList xs)
