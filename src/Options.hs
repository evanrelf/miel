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


parseAdd :: Parser Command
parseAdd = Add . unwords <$> some (strArgument (metavar "DESCRIPTION"))


parseRemove :: Parser Command
parseRemove = Remove <$> argument auto (metavar "ID")


parseList :: Parser Command
parseList = pure List


parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "add" (info parseAdd (progDesc "Add task" <> forwardOptions))
  , command "remove" (info parseRemove (progDesc "Remove task"))
  , command "list" (info parseList (progDesc "List tasks"))
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
