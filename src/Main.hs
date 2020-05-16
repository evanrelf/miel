{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Char as Char
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc ((<+>), Pretty (..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Ansi
import qualified Data.Text.Prettyprint.Doc.Util as Pretty
import qualified Data.Time as Time
import qualified Database.Selda as Selda
import Database.Selda (Attr ((:-)))
import qualified Database.Selda.SQLite as Selda
import Options (Command (..), Options (..), Settings (..), getOptions)
import System.IO (hPutStrLn)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, satisfy, sepBy1)
import Text.Megaparsec.Char (space, space1, string)
import Prelude hiding (id)


type Parser = Parsec Void Text


data Task = Task
  { id :: Selda.ID Task
  , description :: Text
  , created :: Time.UTCTime
  , modified :: Time.UTCTime
  , due :: Maybe Time.UTCTime
  } deriving stock (Generic, Show)
    deriving anyclass Selda.SqlRow


instance Aeson.ToJSON Task where
  toJSON Task{..} = Aeson.object
    [ "id" .= Selda.fromId id
    , "description" .= description
    , "created" .= formatRfc3339 created
    , "modified" .= formatRfc3339 modified
    , "due" .= fmap formatRfc3339 due
    ]


data InputTime
  = Now
  deriving stock Show


formatIso8601 :: Time.FormatTime t => t -> Text
formatIso8601 = toText . Time.formatTime locale format
  where
    locale = Time.defaultTimeLocale
    format = Time.iso8601DateFormat Nothing


formatRfc3339 :: Time.FormatTime t => t -> Text
formatRfc3339 = toText . Time.formatTime locale format
  where
    locale = Time.defaultTimeLocale
    format = Time.iso8601DateFormat (Just "%H:%M:%SZ")


prettyTaskRow :: Task -> Pretty.Doc ann
prettyTaskRow Task{..} =
  Pretty.concatWith (Pretty.surround " │ ")
    [ Pretty.fill 3 . pretty . show @Text $ id
    , pretty (formatIso8601 created)
    , pretty (formatIso8601 modified)
    , Pretty.fill 20 . pretty . maybe "n/a" formatRfc3339 $ due
    , pretty description
    ]


prettyTaskDetail :: Task -> Pretty.Doc ann
prettyTaskDetail Task{..} =
  Pretty.vsep
    [ "ID         " <+> pretty (show @Text id)
    , "Description" <+> Pretty.align (Pretty.reflow description)
    , "Created    " <+> pretty (formatRfc3339 created)
    , "Modified   " <+> pretty (formatRfc3339 modified)
    , "Due        " <+> pretty (maybe "n/a" formatRfc3339 due)
    ]


rowHeading :: Pretty.Doc Ansi.AnsiStyle
rowHeading = Pretty.annotate Ansi.underlined
  "ID  │ Created    │ Modified   │ Due                  │ Description"


timeParser :: Parser InputTime
timeParser = string "due:" *> string "now" $> Now


inputParser :: Parser (Text, Maybe InputTime)
inputParser = do
  space
  (ws, ts) <- fmap partitionEithers $ flip sepBy1 space1 $ asum
    [ Right <$> timeParser
    , Left . toText <$> many (satisfy (\c -> Char.isPrint c && not (Char.isSpace c))) -- TODO
    ]
  space
  void eof
  if length ts > 1 then
    fail "More than one due date"
  else
    pure (unwords ws, viaNonEmpty head ts)


tasksTable :: Selda.Table Task
tasksTable = Selda.table "tasks" [ #id :- Selda.autoPrimary ]


initialize :: FilePath -> IO ()
initialize database =
  Selda.withSQLite database (Selda.tryCreateTable tasksTable)


handleAdd :: Settings -> Text -> IO ()
handleAdd Settings{database} input =
  case parse inputParser "input" input of
    Left parseErrorBundle -> do
      hPutStrLn stderr (errorBundlePretty parseErrorBundle)
      exitFailure

    Right (description, dueInputTime) -> do
      now <- Time.getCurrentTime
      let due =
            case dueInputTime of
              Nothing -> Nothing
              Just Now -> Just now
      Selda.withSQLite database do
        Selda.insert_ tasksTable
          [ Task
              { id = Selda.def
              , description
              , created = now
              , modified = now
              , due
              }
          ]


handleDelete :: Settings -> Int -> IO ()
handleDelete Settings{database} (Selda.toId -> id) =
  Selda.withSQLite database do
    rows <- Selda.deleteFrom tasksTable (#id `Selda.is` id)
    putTextLn ("Deleted " <> show rows <> " row(s)")


handleShow :: Settings -> Int -> IO ()
handleShow Settings{database} (Selda.toId -> id) =
  Selda.withSQLite database do
    tasks <- Selda.query do
      task <- Selda.select tasksTable
      Selda.restrict (task & #id `Selda.is` id)
      pure task
    case tasks of
      [task] -> print (prettyTaskDetail task)
      [] -> die "Task not found"
      _ -> die ("Expected 1 row, but received " <> show (length tasks))


handleEdit :: Settings -> Int -> IO ()
handleEdit Settings{database} (Selda.toId -> id) =
  Selda.withSQLite database do
    tasks <- Selda.query do
      task <- Selda.select tasksTable
      Selda.restrict (task & #id `Selda.is` id)
      pure task
    case tasks of
      [task] -> putLBSLn (Aeson.encodePretty task)
      [] -> die "Task not found"
      _ -> die ("Expected 1 row, but received " <> show (length tasks))


handleList :: Settings -> IO ()
handleList Settings{database} =
  Selda.withSQLite database do
    tasks <- Selda.query (Selda.select tasksTable)
    liftIO (Ansi.putDoc (rowHeading <> "\n"))
    mapM_ (print . prettyTaskRow) tasks


main :: IO ()
main = do
  Options settings@Settings{database} command <- getOptions

  initialize database

  case command of
    Add input ->
      handleAdd settings input

    Delete id ->
      handleDelete settings id

    Show id ->
      handleShow settings id

    Edit id ->
      handleEdit settings id

    List ->
      handleList settings
