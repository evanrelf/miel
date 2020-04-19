{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import qualified Database.SQLite.Simple.Ok as SQLite
import Database.SQLite.Simple.QQ (sql)
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Options.Applicative as OA
import qualified System.Random as Random
import Prelude hiding (id)


data Task = Task
  { id :: Int
  , description :: Text
  , status :: Status
  } deriving stock (Generic, Show)


instance SQLite.ToRow Task where
  toRow Task{..} = SQLite.toRow (id, description, status)


instance SQLite.FromRow Task where
  fromRow = Task <$> SQLite.field <*> SQLite.field <*> SQLite.field


data Status = Open | Closed
  deriving (Read, Show)


instance SQLite.ToField Status where
  toField = SQLite.toField . show @Text


instance SQLite.FromField Status where
  fromField f@(SQLite.fieldData -> SQLite.SQLText text) =
    case readMaybe (toString text) of
      Just t -> SQLite.Ok t
      Nothing ->
        SQLite.returnError
          SQLite.ConversionFailed
          f
          ("Status must be Open or Closed, got " <> toString text)
  fromField f =
    SQLite.returnError
      SQLite.ConversionFailed
      f
      "expecting a SQLText column type"


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


main :: IO ()
main = do
  Add description <- getOptions
  id <- Random.randomRIO (1, 999)
  let task = Task{ id, description, status = Open }
  conn <- SQLite.open "miel.db"
  SQLite.execute_
    conn
    [sql|
      CREATE TABLE IF NOT EXISTS tasks
        ( id INTEGER PRIMARY KEY
        , description TEXT
        , status TEXT
        )
    |]
  SQLite.execute
    conn
    [sql|INSERT INTO tasks (id, description, status) VALUES (?, ?, ?)|]
    task
  results <- SQLite.query_ @Task conn [sql|SELECT * FROM tasks|]
  mapM_ print results
  SQLite.close conn
