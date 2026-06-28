module Main exposing (main)

{-| The elm-spreadsheet site — a [`Workspace.Site`](Workspace-Site).

The landing (`#`) is the showcase gallery of live, editable example spreadsheets (see
[`Examples`](Examples)). The workspace (`#workspace`, `#<uuid>`) lets visitors create and manage
their own spreadsheets, saved in the browser, over the [`SheetDoc`](SheetDoc) document — the same
engine (`Spreadsheet.*`) and grid view (`Spreadsheet.View`) reused under the reusable workspace.

All the routing, navbar, hero and footer chrome lives in [`Workspace.Site`](Workspace-Site); this
module only declares what is specific to elm-spreadsheet.

-}

import Examples
import Html exposing (text)
import SheetDoc exposing (SheetDoc, SheetMsg)
import Workspace.Site


main : Program () (Workspace.Site.Model SheetDoc Examples.Model) (Workspace.Site.Msg SheetMsg Examples.Msg)
main =
    Workspace.Site.program
        { title = "elm-spreadsheet"
        , namespace = "elm-spreadsheet"
        , logo = "logo.svg"
        , eyebrow = "elm · spreadsheet engine"
        , lead =
            [ text "A spreadsheet logic + view layer in Elm — values, ~120 formula functions, number "
            , text "formats, conditional styling, multiple sheets and sync/async recalculation. The "
            , text "examples below are live and editable; open the "
            , Workspace.Site.workspaceLink [ text "Workspace" ]
            , text " to create and save your own spreadsheets."
            ]
        , repoUrl = "https://github.com/tunguski/elm-spreadsheet"
        , workspace = SheetDoc.config
        , context = { user = "me", groups = [] }
        , landing =
            { init = Tuple.first (Examples.init ())
            , update = Examples.update
            , subscriptions = Examples.subscriptions
            , view = Examples.view
            , copyToWorkspace = \_ _ -> Nothing
            }
        }
