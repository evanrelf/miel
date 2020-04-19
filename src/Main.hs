{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Database.Selda as Selda
import Database.Selda (Attr ((:-)))
import qualified Database.Selda.SQLite as Selda
import Options (Command (..), Options (..), Settings (..), getOptions)
import Prelude hiding (id)


data Task = Task
  { id :: Selda.ID Task
  , description :: Text
  } deriving stock (Generic, Show)
    deriving anyclass Selda.SqlRow


data Status = Open | Closed
  deriving stock (Generic, Enum, Bounded, Read, Show)
  deriving anyclass Selda.SqlType


tasks :: Selda.Table Task
tasks = Selda.table "tasks" [ #id :- Selda.autoPrimary ]


main :: IO ()
main = do
  Options Settings{..} command <- getOptions
  case command of
    Add description ->
      Selda.withSQLite database do
        Selda.tryCreateTable tasks
        Selda.insert_ tasks
          [ Task{ id = Selda.def, description }
          ]

    Remove (Selda.toId -> id) ->
      Selda.withSQLite database do
        Selda.tryCreateTable tasks
        rows <- Selda.deleteFrom tasks (#id `Selda.is` id)
        putTextLn ("Deleted " <> show rows <> " rows")

    List ->
      Selda.withSQLite database do
        Selda.tryCreateTable tasks
        Selda.query (Selda.select tasks) >>= mapM_ print

