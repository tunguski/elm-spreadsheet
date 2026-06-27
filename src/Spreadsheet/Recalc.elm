module Spreadsheet.Recalc exposing
    ( Viewport
    , State
    , idle
    , isDone
    , progress
    , remaining
    , begin
    , beginAll
    , step
    )

{-| Incremental, viewport-prioritised recalculation — the async path.

A very large sheet can have tens of thousands of formula cells; recomputing them all in
one synchronous pass would block the UI thread and freeze the page. This module slices
the same dependency-ordered work `Spreadsheet.Sheet` does synchronously into **batches**,
so the host (`Main`) can run one batch per animation frame and keep the page responsive.

It also recalculates **what the user can see first**: `begin` takes a `Viewport` and moves
the visible dirty cells — and the precedents they transitively need — to the front of the
work queue, still in dependency order. So the on-screen region settles within the first
frame or two even while a long tail of off-screen cells is still being computed.

Typical use:

    ( sheet1, state ) = Recalc.begin viewport changedRefs sheet0
    -- each frame:
    ( sheet2, state2 ) = Recalc.step batchSize state1 sheet1
    -- stop when Recalc.isDone state2

@docs Viewport, State, idle, isDone, progress, remaining, begin, beginAll, step

-}

import Set exposing (Set)
import Spreadsheet.Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)


{-| The currently-visible cell rectangle (inclusive), in 0-based col/row coordinates. -}
type alias Viewport =
    { minCol : Int
    , minRow : Int
    , maxCol : Int
    , maxRow : Int
    }


{-| The work queue: cells still to compute (in dependency order, visible-first) and a
running total for progress reporting. -}
type alias State =
    { pending : List ( Int, Int )
    , total : Int
    , doneCount : Int
    }


{-| A finished/empty state. -}
idle : State
idle =
    { pending = [], total = 0, doneCount = 0 }


{-| Is all work complete? -}
isDone : State -> Bool
isDone state =
    List.isEmpty state.pending


{-| `(completed, total)` cells — for a progress indicator. -}
progress : State -> ( Int, Int )
progress state =
    ( state.doneCount, state.total )


{-| How many cells remain. -}
remaining : State -> Int
remaining state =
    List.length state.pending


{-| Begin an incremental recalc for the cells affected by `changed`, prioritising the
viewport. Returns the sheet with any cycles already marked `#CIRC!` and the work queue. -}
begin : Viewport -> List Ref -> Sheet -> ( Sheet, State )
begin viewport changed sheet =
    start viewport (Sheet.dirtyClosure changed sheet) sheet


{-| Begin an incremental *full* recalc (every formula cell), prioritising the viewport. -}
beginAll : Viewport -> Sheet -> ( Sheet, State )
beginAll viewport sheet =
    start viewport (Sheet.formulaCells sheet) sheet


start : Viewport -> List ( Int, Int ) -> Sheet -> ( Sheet, State )
start viewport dirty sheet =
    let
        ( ordered, cyclic ) =
            Sheet.recalcOrder dirty sheet

        marked =
            Sheet.markCircular (Set.toList cyclic) sheet

        prioritized =
            prioritize viewport ordered marked
    in
    ( marked
    , { pending = prioritized
      , total = List.length prioritized
      , doneCount = 0
      }
    )


{-| Compute up to `batch` cells from the front of the queue, updating the sheet. -}
step : Int -> State -> Sheet -> ( Sheet, State )
step batch state sheet =
    let
        ( now, later ) =
            splitAt (max 1 batch) state.pending

        evaluated =
            List.foldl Sheet.evalAndSet sheet now
    in
    ( evaluated
    , { state
        | pending = later
        , doneCount = state.doneCount + List.length now
      }
    )



-- PRIORITISATION -------------------------------------------------------------


{-| Reorder the dependency-sorted work so that visible cells and the precedents they need
come first, while preserving the topological order within each part (so dependencies are
still computed before dependents). -}
prioritize : Viewport -> List ( Int, Int ) -> Sheet -> List ( Int, Int )
prioritize viewport ordered sheet =
    let
        orderedSet =
            Set.fromList ordered

        visible =
            List.filter (inViewport viewport) ordered

        need =
            precedentClosure sheet orderedSet (Set.fromList visible)

        ( priority, rest ) =
            List.partition (\k -> Set.member k need) ordered
    in
    priority ++ rest


inViewport : Viewport -> ( Int, Int ) -> Bool
inViewport vp ( c, r ) =
    c >= vp.minCol && c <= vp.maxCol && r >= vp.minRow && r <= vp.maxRow


{-| Grow a seed set to include every precedent (within the dirty set) reachable from it. -}
precedentClosure : Sheet -> Set ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
precedentClosure sheet universe seed =
    let
        next =
            Set.foldl
                (\k acc ->
                    List.foldl Set.insert acc
                        (List.filter (\p -> Set.member p universe) (Sheet.precedentsOf sheet k))
                )
                seed
                seed
    in
    if Set.size next == Set.size seed then
        seed

    else
        precedentClosure sheet universe next


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )
