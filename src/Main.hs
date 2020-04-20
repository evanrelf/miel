{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc (Pretty (..))
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


instance Pretty Task where
  pretty Task{..} =
    Pretty.concatWith (Pretty.surround " | ")
      [ Pretty.fill 3 . pretty . show @Text $ id
      , pretty (formatRfc3339 created)
      , pretty (formatRfc3339 modified)
      , Pretty.fill 20 . pretty . maybe "n/a" formatRfc3339 $ due
      , pretty (description)
      ]


data Status = Open | Closed
  deriving stock (Generic, Enum, Bounded, Read, Show)
  deriving anyclass Selda.SqlType


tasksTable :: Selda.Table Task
tasksTable = Selda.table "tasks" [ #id :- Selda.autoPrimary ]


main :: IO ()
main = do
  Options Settings{..} command <- getOptions
  case command of
    Add description -> do
      now <- Time.getCurrentTime
      Selda.withSQLite database do
        Selda.tryCreateTable tasksTable
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
        Selda.tryCreateTable tasksTable
        rows <- Selda.deleteFrom tasksTable (#id `Selda.is` id)
        putTextLn ("Deleted " <> show rows <> " rows")

    Show (Selda.toId -> id) -> do
      Selda.withSQLite database do
        Selda.tryCreateTable tasksTable
        tasks <- Selda.query do
          task <- Selda.select tasksTable
          Selda.restrict (task & #id `Selda.is` id)
          pure task
        case tasks of
          [task] -> print (pretty task)
          [] -> die "Task not found"
          _ -> die ("Expected 1 row, but received " <> show (length tasks))

    List ->
      Selda.withSQLite database do
        Selda.tryCreateTable tasksTable
        tasks <- Selda.query (Selda.select tasksTable)
        putTextLn "ID  | CREATED              | MODIFIED             | DUE                  | DESCRIPTION"
        mapM_ (print . pretty) tasks
