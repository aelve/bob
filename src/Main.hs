{-# LANGUAGE
RecordWildCards
  #-}


module Main where


-- General
import Data.Maybe
import Data.Foldable
-- Lenses
import Lens.Micro hiding (set)
-- Text
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
-- GUI
import Graphics.UI.Gtk
-- Files
import System.FilePath
-- Data files
import Paths_bob


data Rule = Rule {
  ruleName     :: Text,
  ruleMappings :: [(Text, Text)] }

readRule :: Text -> Maybe Rule
readRule s = case T.lines s of
  [name, from, to] -> Just Rule {
    ruleName     = name,
    ruleMappings = over (each.both) T.singleton (T.zip from to) }
  _other -> Nothing

readRules :: Text -> [Rule]
readRules = mapMaybe readRule . T.splitOn (T.pack "\n\n")

matchRule :: Text -> Rule -> Maybe (Text, Text)
matchRule query Rule{..} = do
  result <- lookup query ruleMappings
  return (ruleName, result)

matchRules :: Text -> [Rule] -> [(Text, Text)]
matchRules query = mapMaybe (matchRule query)

runGUI :: [Rule] -> IO ()
runGUI rules = do
  initGUI

  window <- windowNew
  window `on` objectDestroy $ mainQuit
  set window [
    windowTitle := "Bob",
    windowGravity := GravityCenter,
    windowWindowPosition := WinPosCenter,
    windowDefaultWidth := 200,
    windowDefaultHeight := 200 ]

  searchEntry <- entryNew

  -- create a new list model
  model <- listStoreNew []
  view <- treeViewNewWithModel model

  treeViewSetHeadersVisible view True

  -- add a couple columns
  colChar <- treeViewColumnNew
  colRule <- treeViewColumnNew

  treeViewColumnSetTitle colChar "Character"
  treeViewColumnSetTitle colRule "Rule"

  rendererChar <- cellRendererTextNew
  rendererRule <- cellRendererTextNew

  cellLayoutPackStart colChar rendererChar True
  cellLayoutPackStart colRule rendererRule True

  cellLayoutSetAttributes colChar rendererChar model $ \row ->
    [ cellText := snd row ]
  cellLayoutSetAttributes colRule rendererRule model $ \row ->
    [ cellText := fst row ]
  layout <- tableNew 2 1 False  -- 2 rows, 1 column, autostretching = off
  tableAttachDefaults layout searchEntry 0 1 0 1
  tableAttachDefaults layout view 0 1 1 2
  window `containerAdd` layout

  set layout [
    tableChildYOptions searchEntry := [Fill] ]

  treeViewAppendColumn view colChar
  treeViewAppendColumn view colRule

  searchEntry `on` editableChanged $ do
    query <- get searchEntry entryText
    listStoreClear model
    for_ (matchRules query rules) $ \(name, result) ->
      listStoreAppend model (name, result)

  widgetShowAll window
  mainGUI

main :: IO ()
main = do
  dataDir <- getDataDir
  rules <- readRules <$> T.readFile (dataDir </> "rules/main.rules")
  runGUI rules
