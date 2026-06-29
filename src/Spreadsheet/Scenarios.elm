module Spreadsheet.Scenarios exposing
    ( Scenario
    , capture
    , apply
    , summary
    )

{-| What-if **scenario manager**: a scenario is a named set of values for a handful of
input cells (e.g. "Optimistic", "Pessimistic"), and the manager lets you apply one or lay
several side by side against a row of result cells.

It builds on the pure, immutable `Sheet`: applying a scenario returns a *new* sheet, so a
comparison can run each scenario against the same starting sheet without disturbing it.

@docs Scenario, capture, apply, summary

-}

import Spreadsheet.Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)


{-| A named scenario: the raw input each of its cells should take. -}
type alias Scenario =
    { name : String, inputs : List ( Ref, String ) }


{-| Snapshot the current raw inputs of the given cells as a named scenario (so you can
restore the present values later). -}
capture : String -> List Ref -> Sheet -> Scenario
capture name refs sheet =
    { name = name, inputs = List.map (\r -> ( r, Sheet.rawAt r sheet )) refs }


{-| Apply a scenario: set its input cells and recalculate, returning the new sheet. -}
apply : Scenario -> Sheet -> Sheet
apply scenario sheet =
    Sheet.recalcAll (Sheet.setRawMany scenario.inputs sheet)


{-| A comparison table: for each scenario, the display values the `resultRefs` take once it
is applied — each computed against the unchanged starting `sheet`, so the scenarios don't
interfere. Returns `(scenarioName, resultDisplays)` rows ready to render. -}
summary : List Scenario -> List Ref -> Sheet -> List ( String, List String )
summary scenarios resultRefs sheet =
    List.map
        (\scenario ->
            let
                applied =
                    apply scenario sheet
            in
            ( scenario.name, List.map (\r -> Sheet.displayString r applied) resultRefs )
        )
        scenarios
