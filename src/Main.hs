{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Database.Selda as Selda
import Database.Selda (Attr ((:-)))
import qualified Database.Selda.SQLite as Selda
import qualified Options.Applicative as OA
import qualified System.Random as Random
import Prelude hiding (id)


data Task = Task
  { id :: Int
  , description :: Text
  , status :: Status
  } deriving stock (Generic, Show)
    deriving anyclass Selda.SqlRow


data Status = Open | Closed
  deriving stock (Generic, Enum, Bounded, Read, Show)
  deriving anyclass Selda.SqlType


data Options
  = Add Text
  deriving Show


parseAdd :: OA.Parser Text
parseAdd = unwords <$> some (OA.strArgument (OA.metavar "DESCRIPTION"))


parseOptions :: OA.Parser Options
parseOptions = OA.hsubparser $ mconcat
  [ OA.command "add" (OA.info (Add <$> parseAdd) mempty)
  ]


getOptions :: IO Options
getOptions = OA.execParser (OA.info (OA.helper <*> parseOptions) mempty)


tasks :: Selda.Table Task
tasks = Selda.table "tasks" [ #id :- Selda.primary ]


main :: IO ()
main = do
  Add description <- getOptions
  id <- Random.randomRIO (1, 999)
  let cliTask = Task{ id, description, status = Open }
  Selda.withSQLite "miel.db" do
    Selda.tryCreateTable tasks
    Selda.insert_ tasks [ cliTask ]
    results <- Selda.query (Selda.select tasks)
    mapM_ print results
