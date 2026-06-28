module Workspace.Db exposing
    ( Query, Cond
    , from, select, where_, orderBy, descBy, limit, offset
    , eq, neq, gt, gte, lt, lte, like, and_, or_, raw
    , toSql
    )

{-| A small, Elm-style **query builder** for the SQL feature, composed with the forward-pipe
operator and rendered to a SQL string:

    Db.from "users"
        |> Db.where_ (Db.gt "age" "18")
        |> Db.select [ "name", "age" ]
        |> Db.orderBy "age"
        |> Db.limit 20
        |> Db.toSql
    --> "SELECT name, age FROM users WHERE age > 18 ORDER BY age ASC LIMIT 20"

The workspace runs the resulting string through the host's database backend (see
[`Backend.query`](Workspace-Backend#Backend)); on the browser site there is no database, so the
SQL action is shown disabled. The builder itself is pure and host-independent, so bbx (and a future
report tool) construct queries the same way.

Values in conditions are rendered safely-ish: strings are single-quoted with embedded quotes
doubled, numbers bare. This is a convenience builder, not a substitute for parameterised queries on
a real server.

@docs Query, Cond
@docs from, select, where_, orderBy, descBy, limit, offset
@docs eq, neq, gt, gte, lt, lte, like, and_, or_, raw
@docs toSql

-}


{-| A query under construction. -}
type Query
    = Query
        { table : String
        , columns : List String
        , conds : List Cond
        , order : List ( String, Bool )
        , limit_ : Maybe Int
        , offset_ : Maybe Int
        }


{-| A boolean condition for a `WHERE` clause. -}
type Cond
    = Cmp String String String
    | And Cond Cond
    | Or Cond Cond
    | Raw String


{-| Start a query against a table (selects `*` until [`select`](#select) narrows it). -}
from : String -> Query
from table =
    Query
        { table = table
        , columns = []
        , conds = []
        , order = []
        , limit_ = Nothing
        , offset_ = Nothing
        }


{-| Choose the columns to return. -}
select : List String -> Query -> Query
select columns (Query q) =
    Query { q | columns = columns }


{-| Add a `WHERE` condition (multiple `where_`s are AND-ed together). -}
where_ : Cond -> Query -> Query
where_ cond (Query q) =
    Query { q | conds = q.conds ++ [ cond ] }


{-| Order ascending by a column. -}
orderBy : String -> Query -> Query
orderBy col (Query q) =
    Query { q | order = q.order ++ [ ( col, True ) ] }


{-| Order descending by a column. -}
descBy : String -> Query -> Query
descBy col (Query q) =
    Query { q | order = q.order ++ [ ( col, False ) ] }


{-| Limit the number of rows. -}
limit : Int -> Query -> Query
limit n (Query q) =
    Query { q | limit_ = Just n }


{-| Skip the first `n` rows. -}
offset : Int -> Query -> Query
offset n (Query q) =
    Query { q | offset_ = Just n }



-- CONDITIONS -----------------------------------------------------------------


{-| `col = value`. The value is auto-quoted: wrap a bare number string and it stays bare; anything
non-numeric is single-quoted. -}
eq : String -> String -> Cond
eq col v =
    Cmp col "=" (lit v)


{-| `col <> value`. -}
neq : String -> String -> Cond
neq col v =
    Cmp col "<>" (lit v)


{-| `col > value`. -}
gt : String -> String -> Cond
gt col v =
    Cmp col ">" (lit v)


{-| `col >= value`. -}
gte : String -> String -> Cond
gte col v =
    Cmp col ">=" (lit v)


{-| `col < value`. -}
lt : String -> String -> Cond
lt col v =
    Cmp col "<" (lit v)


{-| `col <= value`. -}
lte : String -> String -> Cond
lte col v =
    Cmp col "<=" (lit v)


{-| `col LIKE 'pattern'`. -}
like : String -> String -> Cond
like col pattern =
    Cmp col "LIKE" (quote pattern)


{-| Combine two conditions with `AND`. -}
and_ : Cond -> Cond -> Cond
and_ a b =
    And a b


{-| Combine two conditions with `OR`. -}
or_ : Cond -> Cond -> Cond
or_ a b =
    Or a b


{-| An escape hatch: a raw SQL condition fragment, used verbatim. -}
raw : String -> Cond
raw s =
    Raw s



-- RENDER ---------------------------------------------------------------------


{-| Render the query to a SQL string. -}
toSql : Query -> String
toSql (Query q) =
    let
        cols =
            if List.isEmpty q.columns then
                "*"

            else
                String.join ", " q.columns

        whereClause =
            case q.conds of
                [] ->
                    ""

                _ ->
                    " WHERE " ++ String.join " AND " (List.map renderCond q.conds)

        orderClause =
            case q.order of
                [] ->
                    ""

                _ ->
                    " ORDER BY "
                        ++ String.join ", "
                            (List.map
                                (\( col, asc ) ->
                                    col
                                        ++ (if asc then
                                                " ASC"

                                            else
                                                " DESC"
                                           )
                                )
                                q.order
                            )

        limitClause =
            case q.limit_ of
                Just n ->
                    " LIMIT " ++ String.fromInt n

                Nothing ->
                    ""

        offsetClause =
            case q.offset_ of
                Just n ->
                    " OFFSET " ++ String.fromInt n

                Nothing ->
                    ""
    in
    "SELECT " ++ cols ++ " FROM " ++ q.table ++ whereClause ++ orderClause ++ limitClause ++ offsetClause


renderCond : Cond -> String
renderCond cond =
    case cond of
        Cmp col op v ->
            col ++ " " ++ op ++ " " ++ v

        And a b ->
            "(" ++ renderCond a ++ " AND " ++ renderCond b ++ ")"

        Or a b ->
            "(" ++ renderCond a ++ " OR " ++ renderCond b ++ ")"

        Raw s ->
            s


{-| A value literal: numeric text stays bare, everything else is single-quoted. -}
lit : String -> String
lit v =
    if isNumeric v then
        v

    else
        quote v


quote : String -> String
quote s =
    "'" ++ String.replace "'" "''" s ++ "'"


isNumeric : String -> Bool
isNumeric s =
    case String.toFloat s of
        Just _ ->
            True

        Nothing ->
            False
