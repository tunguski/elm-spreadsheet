module Workspace.Serialize exposing
    ( encodeStored, storedDecoder
    , encodeMeta, metaDecoder
    , encodeIndex, indexDecoder
    , encodeComments, commentsDecoder
    )

{-| JSON for everything the workspace persists: a document's [`Meta`](Workspace-Types#Meta) (with
its [`Access`](Workspace-Types#Access)), its threaded [`Comments`](Workspace-Types#Comments), and
the [`Stored`](Workspace-Types#Stored) envelope that wraps the host document.

The host document is opaque to the library, so [`encodeStored`](#encodeStored) /
[`storedDecoder`](#storedDecoder) take the host's own encoder/decoder for it. The browser backend
also stores a small **index** (a list of `Meta`) so the workspace can list documents without
loading every body.

@docs encodeStored, storedDecoder
@docs encodeMeta, metaDecoder
@docs encodeIndex, indexDecoder
@docs encodeComments, commentsDecoder

-}

import Dict
import Json.Decode as D
import Json.Encode as E
import Workspace.Types as Types
    exposing
        ( Access
        , Comment
        , Comments
        , Meta
        , Principal(..)
        , Stored
        , Visibility(..)
        )



-- STORED ---------------------------------------------------------------------


{-| Encode a stored document, delegating its inner `doc` to the host's encoder. -}
encodeStored : (doc -> E.Value) -> Stored doc -> E.Value
encodeStored encodeDoc stored =
    E.object
        [ ( "version", E.int 1 )
        , ( "meta", encodeMeta stored.meta )
        , ( "doc", encodeDoc stored.doc )
        , ( "comments", encodeComments stored.comments )
        ]


{-| Decode a stored document, delegating its inner `doc` to the host's decoder. -}
storedDecoder : D.Decoder doc -> D.Decoder (Stored doc)
storedDecoder docDecoder =
    D.map3 (\meta doc comments -> { meta = meta, doc = doc, comments = comments })
        (D.field "meta" metaDecoder)
        (D.field "doc" docDecoder)
        (D.oneOf [ D.field "comments" commentsDecoder, D.succeed Dict.empty ])



-- META + ACCESS --------------------------------------------------------------


{-| Encode a metadata record. -}
encodeMeta : Meta -> E.Value
encodeMeta meta =
    E.object
        [ ( "id", E.string meta.id )
        , ( "name", E.string meta.name )
        , ( "kind", E.string meta.kind )
        , ( "access", encodeAccess meta.access )
        ]


{-| Decode a metadata record. -}
metaDecoder : D.Decoder Meta
metaDecoder =
    D.map4 (\id name kind access -> { id = id, name = name, kind = kind, access = access })
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.oneOf [ D.field "kind" D.string, D.succeed "document" ])
        (D.oneOf [ D.field "access" accessDecoder, D.succeed (Types.defaultAccess "") ])


encodeAccess : Access -> E.Value
encodeAccess access =
    E.object
        [ ( "owners", E.list encodePrincipal access.owners )
        , ( "readers", E.list encodePrincipal access.readers )
        , ( "visibility", encodeVisibility access.visibility )
        ]


accessDecoder : D.Decoder Access
accessDecoder =
    D.map3 (\owners readers visibility -> { owners = owners, readers = readers, visibility = visibility })
        (D.field "owners" (D.list principalDecoder))
        (D.field "readers" (D.list principalDecoder))
        (D.field "visibility" visibilityDecoder)


encodePrincipal : Principal -> E.Value
encodePrincipal p =
    case p of
        User u ->
            E.object [ ( "t", E.string "user" ), ( "name", E.string u ) ]

        Group g ->
            E.object [ ( "t", E.string "group" ), ( "name", E.string g ) ]


principalDecoder : D.Decoder Principal
principalDecoder =
    D.map2 Tuple.pair (D.field "t" D.string) (D.field "name" D.string)
        |> D.map
            (\( t, name ) ->
                if t == "group" then
                    Group name

                else
                    User name
            )


encodeVisibility : Visibility -> E.Value
encodeVisibility v =
    case v of
        Private ->
            E.string "private"

        Public ->
            E.string "public"


visibilityDecoder : D.Decoder Visibility
visibilityDecoder =
    D.string
        |> D.map
            (\s ->
                if s == "public" then
                    Public

                else
                    Private
            )



-- COMMENTS -------------------------------------------------------------------


{-| Encode the comments of a document. -}
encodeComments : Comments -> E.Value
encodeComments comments =
    E.dict identity (E.list encodeComment) comments


{-| Decode the comments of a document. -}
commentsDecoder : D.Decoder Comments
commentsDecoder =
    D.dict (D.list commentDecoder)


encodeComment : Comment -> E.Value
encodeComment c =
    E.object
        [ ( "id", E.int (Types.commentId c) )
        , ( "author", E.string (Types.commentAuthor c) )
        , ( "body", E.string (Types.commentBody c) )
        , ( "replies", E.list encodeComment (Types.commentReplies c) )
        ]


commentDecoder : D.Decoder Comment
commentDecoder =
    D.map4 Types.commentWith
        (D.field "id" D.int)
        (D.field "author" D.string)
        (D.field "body" D.string)
        (D.field "replies" (D.list (D.lazy (\_ -> commentDecoder))))



-- INDEX ----------------------------------------------------------------------


{-| Encode the document index (the list of metadata the browser backend keeps). -}
encodeIndex : List Meta -> E.Value
encodeIndex metas =
    E.object [ ( "version", E.int 1 ), ( "documents", E.list encodeMeta metas ) ]


{-| Decode the document index. -}
indexDecoder : D.Decoder (List Meta)
indexDecoder =
    D.oneOf
        [ D.field "documents" (D.list metaDecoder)
        , D.list metaDecoder
        ]
