{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
  ( Options (..)
  , Settings (..)
  , Command (..)
  , getOptions
  )
where

import Options.Applicative


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


parseAddCommand :: Parser Command
parseAddCommand = Add . unwords <$> some (strArgument (metavar "DESCRIPTION"))


parseRemoveCommand :: Parser Command
parseRemoveCommand = Remove <$> argument auto (metavar "ID")


parseListCommand :: Parser Command
parseListCommand = pure List


parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "add"
      $ info parseAddCommand
      $ progDesc "Add task" <> forwardOptions
  , command "remove"
      $ info parseRemoveCommand
      $ progDesc "Remove task"
  , command "list"
      $ info parseListCommand
      $ progDesc "List tasks"
  ]


parseSettings :: Parser Settings
parseSettings = do
  database <- strOption $ mconcat
    [ metavar "PATH"
    , help "Path to SQLite database"
    , long "database"
    , short 'd'
    , value "miel.sqlite3"
    , hidden
    ]
  pure Settings{..}


parseOptions :: Parser Options
parseOptions = Options <$> parseSettings <*> parseCommand


getOptions :: IO Options
getOptions = do
  let parser = helper <*> parseOptions
  let infoMod = mempty
  let prefsMod = showHelpOnError
  customExecParser (prefs prefsMod) (info parser infoMod)
