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
  | Delete Int
  | Show Int
  | List
  deriving stock Show


parseAddCommand :: Parser Command
parseAddCommand = Add . unwords <$> some (strArgument (metavar "DESCRIPTION"))


parseDeleteCommand :: Parser Command
parseDeleteCommand = Delete <$> argument auto (metavar "ID")


parseShowCommand :: Parser Command
parseShowCommand = Show <$> argument auto (metavar "ID")


parseListCommand :: Parser Command
parseListCommand = pure List


parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "add"
      $ info parseAddCommand
      $ progDesc "Add task" <> forwardOptions
  , command "delete"
      $ info parseDeleteCommand
      $ progDesc "Delete task"
  , command "show"
      $ info parseShowCommand
      $ progDesc "Show single task"
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
    , showDefault
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
