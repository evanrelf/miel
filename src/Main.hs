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
  } deriving stock (Generic, Show)
    deriving anyclass Selda.SqlRow


instance Pretty Task where
  pretty Task{..} =
    let
      formatRfc3339 :: Time.FormatTime t => t -> Text
      formatRfc3339 = toText . Time.formatTime locale format where
        locale = Time.defaultTimeLocale
        format = Time.iso8601DateFormat (Just "%H:%M:%SZ")
    in
    Pretty.concatWith (Pretty.surround " | ")
      [ Pretty.fill 3 . pretty . show @Text $ id
      , pretty (formatRfc3339 created)
      , pretty (formatRfc3339 modified)
      , pretty (description)
      ]


data Status = Open | Closed
  deriving stock (Generic, Enum, Bounded, Read, Show)
  deriving anyclass Selda.SqlType


tasks :: Selda.Table Task
tasks = Selda.table "tasks" [ #id :- Selda.autoPrimary ]


main :: IO ()
main = do
  Options Settings{..} command <- getOptions
  case command of
    Add description -> do
      now <- Time.getCurrentTime
      Selda.withSQLite database do
        Selda.tryCreateTable tasks
        Selda.insert_ tasks
          [ Task{ id = Selda.def, description, created = now, modified = now }
          ]

    Delete (Selda.toId -> id) ->
      Selda.withSQLite database do
        Selda.tryCreateTable tasks
        rows <- Selda.deleteFrom tasks (#id `Selda.is` id)
        putTextLn ("Deleted " <> show rows <> " rows")

    List ->
      Selda.withSQLite database do
        Selda.tryCreateTable tasks
        Selda.query (Selda.select tasks) >>= mapM_ (print . pretty)
