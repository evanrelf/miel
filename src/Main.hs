module Main (main) where

import qualified Options.Applicative as OA
import Prelude hiding (id)


data Task = Task
  { id :: Int
  , description :: Text
  , status :: Status
  } deriving Show


data Status = Open | Closed
  deriving Show


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
  getOptions >>= print
