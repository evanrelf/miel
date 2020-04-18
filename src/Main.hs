module Main (main) where

import Prelude hiding (id)


data Task = Task
  { id :: Int
  , description :: Text
  , status :: Status
  } deriving Show


data Status = Open | Closed
  deriving Show


main :: IO ()
main = do
  print Task{id = 1, description = "Do the dishes", status = Open}
