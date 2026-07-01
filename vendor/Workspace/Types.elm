module Workspace.Types exposing
    ( Id, Visibility(..), Principal(..), Access, Meta, Stored, Table, Comment(..)
    , Comments
    , Selector(..), DocRef
    , defaultAccess, newMeta, emptyTable
    , selectorKey, selectorLabel, docRefLabel
    , principalKey, principalLabel, principalName, visibilityLabel
    , setName, setKind, setAccess
    , setVisibility, addOwner, removeOwner, addReader, removeReader
    , setDoc, setComments, setMeta
    , comment, commentWith, commentId, commentAuthor, commentBody, commentReplies
    , withReplies
    )

{-| The data types every workspace is built from, with all their **record updaters**.

The library is generic over the document it manages: a notebook, a spreadsheet, a chart spec, a
report. The workspace itself only knows about a document's **metadata** ([`Meta`](#Meta) — id,
name, kind, access), its **permissions** ([`Access`](#Access)), the **comments** threaded onto its
elements ([`Comments`](#Comments)), and a neutral tabular shape ([`Table`](#Table)) used for import
and export. The document value rides along opaquely inside [`Stored`](#Stored).

All record updates live here, in the module that owns the aliases: the elm-lang JS backend
miscompiles a record update on a record alias imported from another module (the un-updated fields
come back `undefined`), so callers reach for these updaters rather than `{ r | … }` at their own
site.

@docs Id, Visibility, Principal, Access, Meta, Stored, Table, Comment, Comments
@docs Selector, DocRef
@docs defaultAccess, newMeta, emptyTable
@docs selectorKey, selectorLabel, docRefLabel
@docs principalKey, principalLabel, principalName, visibilityLabel
@docs setName, setKind, setAccess
@docs setVisibility, addOwner, removeOwner, addReader, removeReader
@docs setDoc, setComments, setMeta
@docs comment, commentWith, commentId, commentAuthor, commentBody, commentReplies, withReplies

-}

import Dict exposing (Dict)


{-| A document's identifier (assigned by the backend). -}
type alias Id =
    String


{-| Who may see a document beyond its explicit owners/readers. `Public` means every logged-in
user is a reader. -}
type Visibility
    = Private
    | Public


{-| A principal in an access list — a single user, or a named group. Groups only bite with a
database backend (where group membership is known); locally they are still editable metadata. -}
type Principal
    = User String
    | Group String


{-| Who may read and write a document. The creator is the sole owner by default (see
[`defaultAccess`](#defaultAccess)). -}
type alias Access =
    { owners : List Principal
    , readers : List Principal
    , visibility : Visibility
    }


{-| The metadata the workspace lists, searches and persists for each document. `kind` records the
host ("notebook", "chart", …) so a single store can hold several document types. -}
type alias Meta =
    { id : Id
    , name : String
    , kind : String
    , access : Access
    }


{-| A complete stored document: its metadata, the opaque host document, and its comments. -}
type alias Stored doc =
    { meta : Meta
    , doc : doc
    , comments : Comments
    }


{-| A neutral table — the lingua franca for importing data into a document and exporting a step
out of one. Hosts map their document (or a step of it) to and from this. -}
type alias Table =
    { headers : List String
    , rows : List (List String)
    }


{-| What part of a referenced document a [`DocRef`](#DocRef) pulls in:

  - `WholeDoc` — the whole document as one table (a SQL query's result, a note's lines).
  - `Step key` — a named step of a notebook (the `key` is the step's stable id), taking its result.
  - `RangeSel a1` — a rectangular range of a spreadsheet in `A1:C10` form.

The referenced document decides how to satisfy a selector (a host's `provide`); the workspace only
routes it. Selectors are deliberately stringly-typed so they cross the document boundary and the
JSON envelope without the library knowing any host's addressing scheme. -}
type Selector
    = WholeDoc
    | Step String
    | RangeSel String


{-| An outgoing **reference** from one document to another: pull the data named by `selector` out of
document `docId` and expose it locally under `binding` (a variable name for a notebook, a target
range for a spreadsheet). The reference graph these form must stay acyclic; see
[`Workspace.Refs`](Workspace-Refs). -}
type alias DocRef =
    { binding : String
    , docId : Id
    , selector : Selector
    }


{-| A threaded comment: replies are themselves comments, to any depth. -}
type Comment
    = Comment
        { id : Int
        , author : String
        , body : String
        , replies : List Comment
        }


{-| Comments attached to a document, keyed by an element's string id (a cell id, a cell address, a
report block — whatever the host names its elements). Each entry is that element's root thread. -}
type alias Comments =
    Dict String (List Comment)



-- CONSTRUCTORS ---------------------------------------------------------------


{-| The default access for a freshly created document: its creator is the only owner, no extra
readers, private. -}
defaultAccess : String -> Access
defaultAccess creator =
    { owners = [ User creator ], readers = [], visibility = Private }


{-| A new metadata record for a document created by `creator`. -}
newMeta : Id -> String -> String -> String -> Meta
newMeta id name kind creator =
    { id = id, name = name, kind = kind, access = defaultAccess creator }


{-| The empty table. -}
emptyTable : Table
emptyTable =
    { headers = [], rows = [] }



-- SELECTOR / REFERENCE HELPERS -----------------------------------------------


{-| A stable, round-trippable string key for a selector (used by the JSON codec and for de-duping). -}
selectorKey : Selector -> String
selectorKey selector =
    case selector of
        WholeDoc ->
            "doc"

        Step key ->
            "step:" ++ key

        RangeSel a1 ->
            "range:" ++ a1


{-| A short human label for a selector, e.g. "whole document", "step abc", "A1:C10". -}
selectorLabel : Selector -> String
selectorLabel selector =
    case selector of
        WholeDoc ->
            "whole document"

        Step key ->
            "step " ++ key

        RangeSel a1 ->
            a1


{-| A one-line label for a reference, e.g. `orders ← A1:C10`. -}
docRefLabel : DocRef -> String
docRefLabel ref =
    ref.binding ++ " ← " ++ selectorLabel ref.selector



-- PRINCIPAL HELPERS ----------------------------------------------------------


{-| A stable key for a principal (for de-duping / membership). -}
principalKey : Principal -> String
principalKey p =
    case p of
        User u ->
            "u:" ++ u

        Group g ->
            "g:" ++ g


{-| The bare name of a principal. -}
principalName : Principal -> String
principalName p =
    case p of
        User u ->
            u

        Group g ->
            g


{-| A human label like `@ada` (user) or `#engineers` (group). -}
principalLabel : Principal -> String
principalLabel p =
    case p of
        User u ->
            "@" ++ u

        Group g ->
            "#" ++ g


{-| "Private" / "Public". -}
visibilityLabel : Visibility -> String
visibilityLabel v =
    case v of
        Private ->
            "Private"

        Public ->
            "Public"



-- META UPDATERS --------------------------------------------------------------


{-| Rename a document. -}
setName : String -> Meta -> Meta
setName name meta =
    { meta | name = name }


{-| Set a document's kind. -}
setKind : String -> Meta -> Meta
setKind kind meta =
    { meta | kind = kind }


{-| Replace a document's access. -}
setAccess : Access -> Meta -> Meta
setAccess access meta =
    { meta | access = access }



-- ACCESS UPDATERS ------------------------------------------------------------


{-| Set visibility. -}
setVisibility : Visibility -> Access -> Access
setVisibility v access =
    { access | visibility = v }


{-| Add an owner (idempotent). -}
addOwner : Principal -> Access -> Access
addOwner p access =
    { access | owners = addPrincipal p access.owners }


{-| Remove an owner. -}
removeOwner : Principal -> Access -> Access
removeOwner p access =
    { access | owners = List.filter (\q -> principalKey q /= principalKey p) access.owners }


{-| Add a reader (idempotent). -}
addReader : Principal -> Access -> Access
addReader p access =
    { access | readers = addPrincipal p access.readers }


{-| Remove a reader. -}
removeReader : Principal -> Access -> Access
removeReader p access =
    { access | readers = List.filter (\q -> principalKey q /= principalKey p) access.readers }


addPrincipal : Principal -> List Principal -> List Principal
addPrincipal p list =
    if List.any (\q -> principalKey q == principalKey p) list then
        list

    else
        list ++ [ p ]



-- STORED UPDATERS ------------------------------------------------------------


{-| Replace the document inside a stored record. -}
setDoc : doc -> Stored doc -> Stored doc
setDoc doc stored =
    { stored | doc = doc }


{-| Replace the comments inside a stored record. -}
setComments : Comments -> Stored doc -> Stored doc
setComments comments stored =
    { stored | comments = comments }


{-| Replace the metadata inside a stored record. -}
setMeta : Meta -> Stored doc -> Stored doc
setMeta meta stored =
    { stored | meta = meta }



-- COMMENT HELPERS ------------------------------------------------------------


{-| A fresh comment with no replies. -}
comment : Int -> String -> String -> Comment
comment id author body =
    Comment { id = id, author = author, body = body, replies = [] }


{-| Build a comment with its replies (used by the decoder). -}
commentWith : Int -> String -> String -> List Comment -> Comment
commentWith id author body replies =
    Comment { id = id, author = author, body = body, replies = replies }


{-| The id of a comment. -}
commentId : Comment -> Int
commentId (Comment r) =
    r.id


{-| The author of a comment. -}
commentAuthor : Comment -> String
commentAuthor (Comment r) =
    r.author


{-| The body of a comment. -}
commentBody : Comment -> String
commentBody (Comment r) =
    r.body


{-| The direct replies of a comment. -}
commentReplies : Comment -> List Comment
commentReplies (Comment r) =
    r.replies


{-| Replace a comment's replies (the only mutation the comment record needs; lives here to keep the
update inside the owning module). -}
withReplies : List Comment -> Comment -> Comment
withReplies replies (Comment r) =
    Comment { r | replies = replies }
