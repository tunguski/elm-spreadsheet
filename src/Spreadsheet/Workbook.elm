module Spreadsheet.Workbook exposing
    ( Workbook
    , init
    , sheetNames
    , activeName
    , active
    , setActive
    , get
    , put
    , addSheet
    , removeSheet
    , valueAt
    , recalc
    )

{-| A workbook: several named `Sheet`s that can reference one another.

A formula on one sheet may read a cell on another with a `Sheet!A1` (or `Sheet!A1:B5`)
reference; `Spreadsheet.Eval` resolves those through a workbook-supplied resolver.
`recalc` recomputes every sheet to a fixed point so cross-sheet chains settle: each pass
recalculates the sheets in order against the workbook's current values, repeating until
nothing changes (or a small cap, which also bounds any cross-sheet cycle).

Sheet names are case-insensitive (`Data!A1` finds `data`); the display order and casing
are preserved for the tab strip.

@docs Workbook, init, sheetNames, activeName, active, setActive
@docs get, put, addSheet, removeSheet, valueAt, recalc

-}

import Dict exposing (Dict)
import Spreadsheet.Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)
import Spreadsheet.Value exposing (Error(..), Value(..))


{-| A set of named sheets with a tab order and an active sheet. -}
type Workbook
    = Workbook
        { sheets : Dict String Sheet
        , order : List String
        , active : String
        }


{-| Build a workbook from `(name, sheet)` pairs; the first is active. -}
init : List ( String, Sheet ) -> Workbook
init pairs =
    Workbook
        { sheets = Dict.fromList (List.map (\( n, s ) -> ( String.toUpper n, s )) pairs)
        , order = List.map Tuple.first pairs
        , active = Maybe.withDefault "" (List.head (List.map Tuple.first pairs))
        }


{-| Sheet names in tab order (original casing). -}
sheetNames : Workbook -> List String
sheetNames (Workbook w) =
    w.order


{-| The active sheet's name. -}
activeName : Workbook -> String
activeName (Workbook w) =
    w.active


{-| The active sheet, if any. -}
active : Workbook -> Maybe Sheet
active (Workbook w) =
    Dict.get (String.toUpper w.active) w.sheets


{-| Make a sheet active (no-op if the name is unknown). -}
setActive : String -> Workbook -> Workbook
setActive name (Workbook w) =
    if List.any (\n -> String.toUpper n == String.toUpper name) w.order then
        Workbook { w | active = name }

    else
        Workbook w


{-| Look up a sheet by name (case-insensitive). -}
get : String -> Workbook -> Maybe Sheet
get name (Workbook w) =
    Dict.get (String.toUpper name) w.sheets


{-| Replace an existing sheet (recalculate afterwards). -}
put : String -> Sheet -> Workbook -> Workbook
put name sheet (Workbook w) =
    if Dict.member (String.toUpper name) w.sheets then
        Workbook { w | sheets = Dict.insert (String.toUpper name) sheet w.sheets }

    else
        Workbook w


{-| Add a new sheet at the end of the tab order (no-op if the name exists). -}
addSheet : String -> Sheet -> Workbook -> Workbook
addSheet name sheet (Workbook w) =
    if Dict.member (String.toUpper name) w.sheets then
        Workbook w

    else
        Workbook { w | sheets = Dict.insert (String.toUpper name) sheet w.sheets, order = w.order ++ [ name ] }


{-| Remove a sheet; if it was active, the first remaining sheet becomes active. -}
removeSheet : String -> Workbook -> Workbook
removeSheet name (Workbook w) =
    let
        u =
            String.toUpper name

        order2 =
            List.filter (\n -> String.toUpper n /= u) w.order
    in
    Workbook
        { w
            | sheets = Dict.remove u w.sheets
            , order = order2
            , active =
                if String.toUpper w.active == u then
                    Maybe.withDefault "" (List.head order2)

                else
                    w.active
        }


{-| The current value of a cell on a named sheet (`#REF!` if the sheet is unknown). -}
valueAt : String -> Ref -> Workbook -> Value
valueAt name ref wb =
    case get name wb of
        Just sheet ->
            Sheet.valueAt ref sheet

        Nothing ->
            VError RefErr



-- RECALCULATION --------------------------------------------------------------


{-| Recompute every sheet to a fixed point so cross-sheet references settle. -}
recalc : Workbook -> Workbook
recalc wb =
    recalcLoop maxPasses wb


maxPasses : Int
maxPasses =
    25


recalcLoop : Int -> Workbook -> Workbook
recalcLoop n wb =
    let
        next =
            onePass wb
    in
    if n <= 0 || sheetsEqual wb next then
        next

    else
        recalcLoop (n - 1) next


{-| Recalculate each sheet once, in tab order, against the workbook's current values (so a
later sheet already sees an earlier one's fresh results within the same pass). -}
onePass : Workbook -> Workbook
onePass (Workbook w0) =
    List.foldl
        (\name (Workbook acc) ->
            case Dict.get (String.toUpper name) acc.sheets of
                Just sheet ->
                    let
                        resolver sn ref =
                            valueAt sn ref (Workbook acc)

                        recomputed =
                            Sheet.recalcAllWith resolver sheet
                    in
                    Workbook { acc | sheets = Dict.insert (String.toUpper name) recomputed acc.sheets }

                Nothing ->
                    Workbook acc
        )
        (Workbook w0)
        w0.order


sheetsEqual : Workbook -> Workbook -> Bool
sheetsEqual (Workbook a) (Workbook b) =
    a.sheets == b.sheets
