module Spreadsheet.Solver exposing
    ( Goal(..), ConstraintOp(..), Constraint, Problem, Solution
    , solve
    , minimize
    )

{-| Constrained optimization over a `Sheet` — the big sibling of `Analysis.goalSeek`.

Where Goal Seek drives a single target cell to a value by varying **one** input, the Solver
optimizes an objective cell (maximize it, minimize it, or hit a target value) by varying
**several** input cells at once, subject to a list of `≤` / `≥` / `=` constraints on other
cells. Like the rest of the what-if machinery it treats the sheet as a black-box function of
its inputs: it writes candidate numbers into the variable cells, recalculates, and reads the
objective and constraint cells back.

The engine is a derivative-free **Nelder–Mead** simplex search wrapped in a **penalty
continuation**: constraints are folded into the score as a squared-violation penalty whose
weight is ramped up over a few outer passes, which pushes the solution onto the feasible
boundary where linear optima live while still handling smooth nonlinear models. It needs no
gradients, so the objective can be any formula the engine can evaluate.

@docs Goal, ConstraintOp, Constraint, Problem, Solution
@docs solve
@docs minimize

-}

import Spreadsheet.Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Value as Value


{-| What to do with the objective cell. -}
type Goal
    = Maximize
    | Minimize
    | TargetValue Float


{-| The relation a constraint cell must satisfy against its bound. -}
type ConstraintOp
    = LessEq
    | GreaterEq
    | EqualTo


{-| A single constraint: the value of `cell` must be `op` `bound` (e.g. `cell ≤ 10`). The
cell is normally a formula of the variable cells, so the constraint can be any computed
quantity, not just a bound on a variable. -}
type alias Constraint =
    { cell : Ref, op : ConstraintOp, bound : Float }


{-| A model to solve: optimize `objective` toward `goal` by varying `variables`, subject to
`constraints`. -}
type alias Problem =
    { objective : Ref
    , goal : Goal
    , variables : List Ref
    , constraints : List Constraint
    }


{-| The outcome: the recalculated `sheet` with the solved inputs in place, the chosen
`values` per variable, the achieved `objective`, and whether every constraint is satisfied
(`feasible`) within tolerance. -}
type alias Solution =
    { sheet : Sheet
    , values : List ( Ref, Float )
    , objective : Float
    , feasible : Bool
    }


{-| Solve `problem` against `sheet`, starting from the variables' current values. Returns
`Nothing` only when there are no variables or the objective never evaluates to a number. -}
solve : Problem -> Sheet -> Maybe Solution
solve problem sheet =
    if List.isEmpty problem.variables then
        Nothing

    else
        let
            start =
                List.map (\ref -> currentNum ref sheet) problem.variables

            best =
                List.foldl
                    (\mu x0 -> minimize (penalizedScore problem sheet mu) x0)
                    start
                    [ 1.0e1, 1.0e2, 1.0e4, 1.0e6 ]

            solved =
                applyVars problem.variables best sheet
        in
        case Value.toNumber (Sheet.valueAt problem.objective solved) of
            Ok objVal ->
                Just
                    { sheet = solved
                    , values = List.map2 Tuple.pair problem.variables best
                    , objective = objVal
                    , feasible = feasibleAt problem solved 1.0e-4
                    }

            Err _ ->
                Nothing



-- SCORING ------------------------------------------------------------------


currentNum : Ref -> Sheet -> Float
currentNum ref sheet =
    Result.withDefault 0 (Value.toNumber (Sheet.valueAt ref sheet))


applyVars : List Ref -> List Float -> Sheet -> Sheet
applyVars refs xs sheet =
    Sheet.recalcAll
        (Sheet.setRawMany (List.map2 (\r x -> ( r, String.fromFloat x )) refs xs) sheet)


{-| The penalized objective at `xs`: the base goal plus `mu` times the sum of squared
constraint violations. A cell that fails to evaluate scores as effectively infinite, so the
search avoids regions the engine can't compute. -}
penalizedScore : Problem -> Sheet -> Float -> List Float -> Float
penalizedScore problem sheet mu xs =
    let
        solved =
            applyVars problem.variables xs sheet
    in
    case Value.toNumber (Sheet.valueAt problem.objective solved) of
        Err _ ->
            1.0e18

        Ok obj ->
            let
                base =
                    case problem.goal of
                        Maximize ->
                            negate obj

                        Minimize ->
                            obj

                        TargetValue t ->
                            (obj - t) ^ 2

                penalty =
                    List.sum (List.map (violationSq solved) problem.constraints)
            in
            base + mu * penalty


violationSq : Sheet -> Constraint -> Float
violationSq sheet c =
    case Value.toNumber (Sheet.valueAt c.cell sheet) of
        Err _ ->
            1.0e9

        Ok g ->
            let
                v =
                    case c.op of
                        LessEq ->
                            max 0 (g - c.bound)

                        GreaterEq ->
                            max 0 (c.bound - g)

                        EqualTo ->
                            g - c.bound
            in
            v * v


feasibleAt : Problem -> Sheet -> Float -> Bool
feasibleAt problem sheet tol =
    List.all (\c -> violationSq sheet c <= tol * tol + tol) problem.constraints



-- NELDER–MEAD --------------------------------------------------------------


type alias Vertex =
    { p : List Float, v : Float }


{-| Minimize `f` from the starting point `x0` with a Nelder–Mead simplex search. Exposed
because it is a useful derivative-free minimizer in its own right (and keeps the optimizer
testable without a sheet). -}
minimize : (List Float -> Float) -> List Float -> List Float
minimize f x0 =
    let
        n =
            List.length x0

        initial =
            x0 :: List.map (\i -> nudge i x0) (List.range 0 (n - 1))

        simplex0 =
            List.map (\p -> { p = p, v = f p }) initial
    in
    .p (best (iterate f simplex0 (200 + 40 * n)))


nudge : Int -> List Float -> List Float
nudge i xs =
    let
        base =
            Maybe.withDefault 0 (listGet i xs)

        h =
            0.05 * abs base + 0.01
    in
    setAt i (base + h) xs


iterate : (List Float -> Float) -> List Vertex -> Int -> List Vertex
iterate f simplex iter =
    if iter <= 0 then
        simplex

    else
        let
            sorted =
                List.sortBy .v simplex
        in
        if converged sorted then
            sorted

        else
            iterate f (step f sorted) (iter - 1)


{-| One Nelder–Mead move on a sorted simplex (best first, worst last): reflect the worst
vertex through the centroid of the rest, then expand / contract / shrink as the textbook
algorithm prescribes. -}
step : (List Float -> Float) -> List Vertex -> List Vertex
step f sorted =
    let
        worst =
            Maybe.withDefault { p = [], v = 0 } (lastOf sorted)

        rest =
            dropLast sorted

        secondWorst =
            Maybe.withDefault worst (lastOf rest)

        bestV =
            Maybe.withDefault worst (List.head sorted)

        centroid =
            centroidOf (List.map .p rest)

        reflected =
            vertex f (along centroid worst.p 1.0)
    in
    if reflected.v < bestV.v then
        let
            expanded =
                vertex f (along centroid worst.p 2.0)
        in
        replaceWorst rest
            (if expanded.v < reflected.v then
                expanded

             else
                reflected
            )

    else if reflected.v < secondWorst.v then
        replaceWorst rest reflected

    else if reflected.v < worst.v then
        -- outside contraction
        let
            contracted =
                vertex f (along centroid worst.p 0.5)
        in
        if contracted.v <= reflected.v then
            replaceWorst rest contracted

        else
            shrink f bestV sorted

    else
        -- inside contraction
        let
            contracted =
                vertex f (along centroid worst.p -0.5)
        in
        if contracted.v < worst.v then
            replaceWorst rest contracted

        else
            shrink f bestV sorted


replaceWorst : List Vertex -> Vertex -> List Vertex
replaceWorst rest newVertex =
    newVertex :: rest


shrink : (List Float -> Float) -> Vertex -> List Vertex -> List Vertex
shrink f bestV sorted =
    List.map
        (\vx ->
            if vx.p == bestV.p then
                vx

            else
                vertex f (vlerp bestV.p vx.p 0.5)
        )
        sorted


vertex : (List Float -> Float) -> List Float -> Vertex
vertex f p =
    { p = p, v = f p }


best : List Vertex -> Vertex
best simplex =
    Maybe.withDefault { p = [], v = 0 } (List.head (List.sortBy .v simplex))


converged : List Vertex -> Bool
converged sorted =
    case ( List.head sorted, lastOf sorted ) of
        ( Just lo, Just hi ) ->
            abs (hi.v - lo.v) <= 1.0e-10 + 1.0e-8 * abs lo.v

        _ ->
            True



-- VECTOR HELPERS -----------------------------------------------------------


{-| `centroid + factor * (centroid - point)` — the reflection/expansion line used by
Nelder–Mead (factor 1 reflects, 2 expands, ±0.5 contracts). -}
along : List Float -> List Float -> Float -> List Float
along centroid point factor =
    List.map2 (\c x -> c + factor * (c - x)) centroid point


vlerp : List Float -> List Float -> Float -> List Float
vlerp a b t =
    List.map2 (\x y -> x + t * (y - x)) a b


centroidOf : List (List Float) -> List Float
centroidOf points =
    case points of
        [] ->
            []

        first :: _ ->
            let
                n =
                    toFloat (List.length points)
            in
            List.map (\s -> s / n)
                (List.foldl (List.map2 (+)) (List.map (always 0) first) points)


listGet : Int -> List a -> Maybe a
listGet i xs =
    List.head (List.drop i xs)


setAt : Int -> a -> List a -> List a
setAt i value xs =
    List.indexedMap
        (\j x ->
            if i == j then
                value

            else
                x
        )
        xs


lastOf : List a -> Maybe a
lastOf xs =
    List.head (List.reverse xs)


dropLast : List a -> List a
dropLast xs =
    List.take (List.length xs - 1) xs
