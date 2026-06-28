module Workspace.Permissions exposing
    ( canRead, canWrite, isOwner, isReader, matches
    )

{-| Who may do what with a document, decided purely from a [`Context`](Workspace-Backend#Context)
(the acting user and their groups) and a document's [`Access`](Workspace-Types#Access).

The rules:

  - **Owners** may read and write. A principal matches the context if it is the current `User`, or
    a `Group` the user belongs to.
  - **Readers** may read.
  - A **Public** document is readable by any logged-in user (a non-empty `user`).

Locally (the browser site) there is one real user, so these mostly gate the UI; with a database
backend the same rules govern every notebook the user can see.

@docs canRead, canWrite, isOwner, isReader, matches

-}

import Workspace.Backend exposing (Context)
import Workspace.Types as Types exposing (Access, Principal(..))


{-| Does the context match this principal — the same user, or a member of the group? -}
matches : Context -> Principal -> Bool
matches ctx p =
    case p of
        User u ->
            u == ctx.user

        Group g ->
            List.member g ctx.groups


{-| Is the context an owner of this access? -}
isOwner : Context -> Access -> Bool
isOwner ctx access =
    List.any (matches ctx) access.owners


{-| Is the context an explicit reader of this access? -}
isReader : Context -> Access -> Bool
isReader ctx access =
    List.any (matches ctx) access.readers


{-| May the context edit (and delete, and change permissions of) this document? -}
canWrite : Context -> Access -> Bool
canWrite ctx access =
    isOwner ctx access


{-| May the context open this document? -}
canRead : Context -> Access -> Bool
canRead ctx access =
    isOwner ctx access
        || isReader ctx access
        || (access.visibility == Types.Public && ctx.user /= "")
