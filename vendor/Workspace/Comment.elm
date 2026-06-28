module Workspace.Comment exposing
    ( add, reply, deleteThread
    , countFor, total, nextId
    )

{-| Threaded comments on a document's elements — add a top-level comment to an element, reply to
any comment to any depth, and count them (for the markers that show where conversation lives).

Comments are stored as [`Comments`](Workspace-Types#Comments): a `Dict` from an element's string id
to that element's root threads. All of this is pure; the views live in
[`Workspace.View`](Workspace-View).

@docs add, reply, deleteThread
@docs countFor, total, nextId

-}

import Dict
import Workspace.Types as Types exposing (Comment, Comments)


{-| Add a new top-level comment to an element's thread list. -}
add : String -> String -> String -> Comments -> Comments
add elementKey author body comments =
    let
        c =
            Types.comment (nextId comments) author body

        existing =
            Dict.get elementKey comments |> Maybe.withDefault []
    in
    Dict.insert elementKey (existing ++ [ c ]) comments


{-| Reply to the comment with `parentId` under `elementKey`. A no-op if the parent is not found. -}
reply : String -> Int -> String -> String -> Comments -> Comments
reply elementKey parentId author body comments =
    let
        child =
            Types.comment (nextId comments) author body

        existing =
            Dict.get elementKey comments |> Maybe.withDefault []
    in
    Dict.insert elementKey (List.map (insertReply parentId child) existing) comments


{-| Drop an entire element's thread (e.g. when its element is deleted). -}
deleteThread : String -> Comments -> Comments
deleteThread elementKey comments =
    Dict.remove elementKey comments


insertReply : Int -> Comment -> Comment -> Comment
insertReply parentId child node =
    let
        replies =
            Types.commentReplies node
    in
    if Types.commentId node == parentId then
        Types.withReplies (replies ++ [ child ]) node

    else
        Types.withReplies (List.map (insertReply parentId child) replies) node


{-| How many comments (including all nested replies) an element has. -}
countFor : String -> Comments -> Int
countFor elementKey comments =
    Dict.get elementKey comments
        |> Maybe.withDefault []
        |> List.map countNode
        |> List.sum


{-| The total number of comments across the whole document. -}
total : Comments -> Int
total comments =
    Dict.values comments
        |> List.concat
        |> List.map countNode
        |> List.sum


countNode : Comment -> Int
countNode node =
    1 + List.sum (List.map countNode (Types.commentReplies node))


{-| The next free comment id (one past the largest id anywhere in the document). -}
nextId : Comments -> Int
nextId comments =
    1 + maxId comments


maxId : Comments -> Int
maxId comments =
    Dict.values comments
        |> List.concat
        |> List.map maxNode
        |> List.maximum
        |> Maybe.withDefault 0


maxNode : Comment -> Int
maxNode node =
    List.maximum (Types.commentId node :: List.map maxNode (Types.commentReplies node))
        |> Maybe.withDefault (Types.commentId node)
