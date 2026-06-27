module Spreadsheet.Find exposing
    ( Query
    , defaults
    , findAll
    , replaceAll
    )

{-| Find (and replace) text across a sheet's cells.

A `Query` carries the needle and the usual options: case sensitivity, whole-cell vs
substring, and whether to look in each cell's **raw input** (`inFormulas = True`) or its
**displayed value**. `findAll` returns the matching cells in row-major order.

`replaceAll` always rewrites the **raw input** (you can't meaningfully edit a formatted
display), recalculating afterwards — so it matches and replaces against the formula/literal
text regardless of the query's `inFormulas` flag.

@docs Query, defaults, findAll, replaceAll

-}

import Spreadsheet.Ref exposing (Ref)
import Spreadsheet.Sheet as Sheet exposing (Sheet)


{-| A search request. -}
type alias Query =
    { text : String
    , matchCase : Bool
    , wholeCell : Bool
    , inFormulas : Bool
    }


{-| A blank, case-insensitive, substring, search-values query. -}
defaults : Query
defaults =
    { text = "", matchCase = False, wholeCell = False, inFormulas = False }


{-| Every cell whose searchable text matches the query, in row-major order. -}
findAll : Query -> Sheet -> List Ref
findAll query sheet =
    List.filter (\ref -> matches query (searchText query ref sheet)) (Sheet.occupiedRefs sheet)


searchText : Query -> Ref -> Sheet -> String
searchText query ref sheet =
    if query.inFormulas then
        Sheet.rawAt ref sheet

    else
        Sheet.displayString ref sheet


matches : Query -> String -> Bool
matches query hay =
    if String.isEmpty query.text then
        False

    else
        let
            ( h, n ) =
                if query.matchCase then
                    ( hay, query.text )

                else
                    ( String.toLower hay, String.toLower query.text )
        in
        if query.wholeCell then
            h == n

        else
            String.contains n h


{-| Replace every match in the cells' raw input and recalculate. With `wholeCell`, a
matching cell's whole input is replaced; otherwise each occurrence of the needle is. -}
replaceAll : Query -> String -> Sheet -> Sheet
replaceAll query replacement sheet =
    let
        rawQuery =
            { query | inFormulas = True }

        targets =
            findAll rawQuery sheet
    in
    Sheet.recalcAll
        (List.foldl
            (\ref acc -> Sheet.setRaw ref (replaceIn query replacement (Sheet.rawAt ref acc)) acc)
            sheet
            targets
        )


replaceIn : Query -> String -> String -> String
replaceIn query replacement raw =
    if query.wholeCell then
        replacement

    else if query.matchCase then
        String.replace query.text replacement raw

    else
        ciReplace query.text replacement raw


{-| Case-insensitive replace-all, preserving the surrounding original-case text. -}
ciReplace : String -> String -> String -> String
ciReplace needle repl hay =
    if String.isEmpty needle then
        hay

    else
        let
            n =
                String.length needle

            idxs =
                nonOverlapping n (String.indexes (String.toLower needle) (String.toLower hay))
        in
        build hay repl n idxs 0


build : String -> String -> Int -> List Int -> Int -> String
build hay repl n idxs pos =
    case idxs of
        [] ->
            String.dropLeft pos hay

        i :: rest ->
            String.slice pos i hay ++ repl ++ build hay repl n rest (i + n)


nonOverlapping : Int -> List Int -> List Int
nonOverlapping n idxs =
    case idxs of
        [] ->
            []

        i :: rest ->
            i :: nonOverlapping n (List.filter (\j -> j >= i + n) rest)
