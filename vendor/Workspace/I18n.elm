module Workspace.I18n exposing (T, en)

{-| Translations for the workspace **chrome** — every visible UI string the workspace itself renders
(buttons, dialogs, notices, placeholders, labels). The host document's own editor strings are not
covered here; those come from the host's `viewDoc`.

A host picks a language by passing a [`T`](#T) record on its `Config` (`t = Workspace.I18n.en`).
Interpolated messages expose only the literal prefix as a field; the workspace appends the dynamic
part (e.g. `config.t.couldNotCopy ++ e`).

@docs T, en

-}


{-| One field per visible chrome string. -}
type alias T =
    { newButton : String
    , allDocuments : String
    , searchDocuments : String
    , noDocumentsMatch : String
    , noDocumentsYet : String
    , open : String
    , makeACopy : String
    , copy : String
    , delete : String
    , untitled : String
    , copyOf : String
    , share : String
    , comments : String
    , showHideComments : String
    , addAComment : String
    , post : String
    , reply : String
    , sharingPermissions : String
    , visibility : String
    , visibilityPrivate : String
    , visibilityPublic : String
    , ownersLabel : String
    , readersLabel : String
    , addSomeone : String
    , addOwner : String
    , addReader : String
    , idOrGroupName : String
    , user : String
    , group : String
    , done : String
    , cancel : String
    , import_ : String
    , importDataFromUrl : String
    , importUrl : String
    , dataUrl : String
    , format : String
    , formatJsonArray : String
    , formatCsvTsv : String
    , json : String
    , csv : String
    , enterUrlToImport : String
    , urlMustBeReachable : String
    , runSqlQuery : String
    , runQuery : String
    , sql : String
    , enterAQuery : String
    , queryRunsOnDatabase : String
    , noDatabaseRunningDisabled : String
    , noDatabaseConnection : String
    , reloadData : String
    , reloadReferencedData : String
    , doesNotSupportImporting : String
    , excel : String
    , excelNotAvailable : String
    , copiedTo : String
    , couldNotCopy : String
    , couldNotListDocuments : String
    , couldNotOpenDocument : String
    , couldNotParseData : String
    , importFailed : String
    , queryFailed : String
    , newNotebook : String
    }


{-| English chrome strings — the workspace's defaults. -}
en : T
en =
    { newButton = "+ New"
    , allDocuments = "← All documents"
    , searchDocuments = "Search documents…"
    , noDocumentsMatch = "No documents match your search."
    , noDocumentsYet = "No documents yet — create one to get started."
    , open = "Open"
    , makeACopy = "Make a copy"
    , copy = "Copy"
    , delete = "Delete"
    , untitled = "Untitled"
    , copyOf = "Copy of "
    , share = "Share"
    , comments = "Comments"
    , showHideComments = "Show / hide comments"
    , addAComment = "Add a comment…"
    , post = "Post"
    , reply = "Reply…"
    , sharingPermissions = "Sharing & permissions"
    , visibility = "Visibility"
    , visibilityPrivate = "Private — only people below"
    , visibilityPublic = "Public — any logged-in user can read"
    , ownersLabel = "Owners (can edit & share)"
    , readersLabel = "Readers (can view)"
    , addSomeone = "Add someone"
    , addOwner = "+ Owner"
    , addReader = "+ Reader"
    , idOrGroupName = "id or group name"
    , user = "User"
    , group = "Group"
    , done = "Done"
    , cancel = "Cancel"
    , import_ = "Import"
    , importDataFromUrl = "Import data from a URL"
    , importUrl = "Import URL"
    , dataUrl = "Data URL"
    , format = "Format"
    , formatJsonArray = "JSON (array of objects)"
    , formatCsvTsv = "CSV / TSV"
    , json = "JSON"
    , csv = "CSV"
    , enterUrlToImport = "Enter a URL to import."
    , urlMustBeReachable = "The URL must be reachable from the browser (CORS-enabled)."
    , runSqlQuery = "Run a SQL query"
    , runQuery = "Run query"
    , sql = "SQL"
    , enterAQuery = "Enter a query."
    , queryRunsOnDatabase = "The query runs on the workspace's database and the result is added to the document."
    , noDatabaseRunningDisabled = "This workspace has no database connection, so running is disabled. With a database backend (e.g. in the bbx app) this runs the query and adds the result to the document."
    , noDatabaseConnection = "This workspace has no database connection."
    , reloadData = "Reload data"
    , reloadReferencedData = "Re-fetch the documents this one references and refresh its data."
    , doesNotSupportImporting = "This document does not support importing data."
    , excel = "Excel"
    , excelNotAvailable = "Excel export is not available here."
    , copiedTo = "Copied to “"
    , couldNotCopy = "Could not copy: "
    , couldNotListDocuments = "Could not list documents: "
    , couldNotOpenDocument = "Could not open document: "
    , couldNotParseData = "Could not parse data: "
    , importFailed = "Import failed: "
    , queryFailed = "Query failed: "
    , newNotebook = "New notebook"
    }
