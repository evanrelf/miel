{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Database.Selda as Selda
import Database.Selda (Attr ((:-)))
import qualified Database.Selda.SQLite as Selda
import qualified Options.Applicative as OA
import Prelude hiding (id)


data Task = Task
  { id :: Selda.ID Task
  , description :: Text
  , status :: Status
  } deriving stock (Generic, Show)
    deriving anyclass Selda.SqlRow


data Status = Open | Closed
  deriving stock (Generic, Enum, Bounded, Read, Show)
  deriving anyclass Selda.SqlType


data Options = Options Command
  deriving stock Show


data Command
  = Add Text
  | List
  deriving stock Show


parseAdd :: OA.Parser Command
parseAdd = Add . unwords <$> some (OA.strArgument (OA.metavar "DESCRIPTION"))


parseCommand :: OA.Parser Command
parseCommand = OA.hsubparser $ mconcat
  [ OA.command "add" (OA.info parseAdd mempty)
  , OA.command "list" (OA.info (pure List) mempty)
  ]


parseOptions :: OA.Parser Options
parseOptions = Options <$> parseCommand


getOptions :: IO Options
getOptions = OA.execParser (OA.info (OA.helper <*> parseOptions) mempty)


tasks :: Selda.Table Task
tasks = Selda.table "tasks" [ #id :- Selda.autoPrimary ]


main :: IO ()
main = do
  let sqlite = "miel.sqlite3"
  Options command <- getOptions
  case command of
    Add description ->
      Selda.withSQLite sqlite do
        Selda.tryCreateTable tasks
        Selda.insert_ tasks
          [ Task{ id = Selda.def, description, status = Open }
          ]

    List ->
      Selda.withSQLite sqlite do
        Selda.tryCreateTable tasks
        Selda.query (Selda.select tasks) >>= mapM_ print
