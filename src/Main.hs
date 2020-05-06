{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc ((<+>), Pretty (..))
import qualified Data.Text.Prettyprint.Doc.Util as Pretty
import qualified Data.Time as Time
import qualified Database.Selda as Selda
import Database.Selda (Attr ((:-)))
import qualified Database.Selda.SQLite as Selda
import Options (Command (..), Options (..), Settings (..), getOptions)
import Prelude hiding (id)


data Task = Task
  { id :: Selda.ID Task
  , description :: Text
  , created :: Time.UTCTime
  , modified :: Time.UTCTime
  , due :: Maybe Time.UTCTime
  } deriving stock (Generic, Show)
    deriving anyclass Selda.SqlRow


formatRfc3339 :: Time.FormatTime t => t -> Text
formatRfc3339 = toText . Time.formatTime locale format where
  locale = Time.defaultTimeLocale
  format = Time.iso8601DateFormat (Just "%H:%M:%SZ")


prettyTaskRow :: Task -> Pretty.Doc ann
prettyTaskRow Task{..} =
  Pretty.concatWith (Pretty.surround " │ ")
    [ Pretty.fill 3 . pretty . show @Text $ id
    , pretty (formatRfc3339 created)
    , pretty (formatRfc3339 modified)
    , Pretty.fill 20 . pretty . maybe "n/a" formatRfc3339 $ due
    , pretty description
    ]


prettyTaskDetail :: Task -> Pretty.Doc ann
prettyTaskDetail Task{..} =
  Pretty.vsep
    [ Pretty.fill 11 "ID" <+> pretty (show @Text id)
    , Pretty.fill 11 "Description" <+> Pretty.align (Pretty.reflow description)
    , Pretty.fill 11 "Created" <+> pretty (formatRfc3339 created)
    , Pretty.fill 11 "Modified" <+> pretty (formatRfc3339 modified)
    , Pretty.fill 11 "Due" <+> pretty (maybe "n/a" formatRfc3339 due)
    ]


data Status = Open | Closed
  deriving stock (Generic, Enum, Bounded, Read, Show)
  deriving anyclass Selda.SqlType


tasksTable :: Selda.Table Task
tasksTable = Selda.table "tasks" [ #id :- Selda.autoPrimary ]


initialize :: FilePath -> IO ()
initialize database =
  Selda.withSQLite database (Selda.tryCreateTable tasksTable)


main :: IO ()
main = do
  Options Settings{..} command <- getOptions
  initialize database
  case command of
    Add description -> do
      now <- Time.getCurrentTime
      Selda.withSQLite database do
        Selda.insert_ tasksTable
          [ Task
              { id = Selda.def
              , description
              , created = now
              , modified = now
              , due = Nothing
              }
          ]

    Delete (Selda.toId -> id) ->
      Selda.withSQLite database do
        rows <- Selda.deleteFrom tasksTable (#id `Selda.is` id)
        putTextLn ("Deleted " <> show rows <> " rows")

    Show (Selda.toId -> id) -> do
      Selda.withSQLite database do
        tasks <- Selda.query do
          task <- Selda.select tasksTable
          Selda.restrict (task & #id `Selda.is` id)
          pure task
        case tasks of
          [task] -> print (prettyTaskDetail task)
          [] -> die "Task not found"
          _ -> die ("Expected 1 row, but received " <> show (length tasks))

    List ->
      Selda.withSQLite database do
        tasks <- Selda.query (Selda.select tasksTable)
        putTextLn "ID  │ CREATED              │ MODIFIED             │ DUE                  │ DESCRIPTION"
        mapM_ (print . prettyTaskRow) tasks
