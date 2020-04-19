{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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


data Options = Options Settings Command
  deriving stock Show


data Settings = Settings
  { database :: FilePath
  } deriving stock Show


data Command
  = Add Text
  | Remove Int
  | List
  deriving stock Show


parseAdd :: OA.Parser Command
parseAdd = Add . unwords <$> some (OA.strArgument (OA.metavar "DESCRIPTION"))


parseRemove :: OA.Parser Command
parseRemove = Remove <$> OA.argument OA.auto (OA.metavar "ID")


parseList :: OA.Parser Command
parseList = pure List


parseCommand :: OA.Parser Command
parseCommand = OA.hsubparser $ mconcat
  [ OA.command "add" (OA.info parseAdd (OA.progDesc "Add task"))
  , OA.command "remove" (OA.info parseRemove (OA.progDesc "Remove task"))
  , OA.command "list" (OA.info parseList (OA.progDesc "List tasks"))
  ]


parseSettings :: OA.Parser Settings
parseSettings = do
  database <- OA.strOption $ mconcat
    [ OA.metavar "PATH"
    , OA.help "Path to SQLite database"
    , OA.long "database"
    , OA.short 'd'
    , OA.value "miel.sqlite3"
    , OA.hidden
    ]
  pure Settings{..}


parseOptions :: OA.Parser Options
parseOptions = Options <$> parseSettings <*> parseCommand


getOptions :: IO Options
getOptions = OA.execParser (OA.info (OA.helper <*> parseOptions) mempty)


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
          [ Task{ id = Selda.def, description, status = Open }
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

