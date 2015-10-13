{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
ScopedTypeVariables,
FlexibleContexts
  #-}


module Main (main) where


-- General
import Data.Maybe
import Data.Foldable
import Control.Monad
-- Lists
import Data.List (intercalate, dropWhileEnd)
-- Monads
import Control.Monad.IO.Class (liftIO)
-- Containers
import Data.Map (Map)
import qualified Data.Map as M
-- Text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.ICU.Char as T
import Text.Printf
-- GUI
import Graphics.UI.Gtk
-- Bob-specific
import Bob



runGUI :: [Rule] -> Map Entity [Text] -> IO ()
runGUI rules names = do
  initGUI

  window <- windowNew
  window `on` objectDestroy $ mainQuit
  set window [
    windowTitle := ("Bob" :: Text),
    windowGravity := GravityCenter,
    windowWindowPosition := WinPosCenter,
    windowDefaultWidth := 200,
    windowDefaultHeight := 200 ]

  searchEntry <- entryNew

  -- create a new list model
  model <- listStoreNew []
  view <- treeViewNewWithModel model

  treeViewSetHeadersVisible view True

  -- add some columns
  let addColumn (title :: Text) = do
        column <- treeViewColumnNew
        treeViewColumnSetTitle column title
        renderer <- cellRendererTextNew
        cellLayoutPackStart column renderer True
        treeViewAppendColumn view column
        return (column, renderer)

  (columnChar,        columnRendererChar)        <- addColumn "Character"
  (columnRule,        columnRendererRule)        <- addColumn "Rule"
  (columnName,        columnRendererName)        <- addColumn "Name"
  (columnUnicodeName, columnRendererUnicodeName) <- addColumn "Unicode name"

  cellLayoutSetAttributes columnChar columnRendererChar model $
    \(rule, entity) -> [ cellText := entity ]
  cellLayoutSetAttributes columnRule columnRendererRule model $
    \(rule, entity) -> [ cellText := rule ]
  cellLayoutSetAttributes columnName columnRendererName model $
    \(rule, entity) -> [
      cellText := T.intercalate " / " (M.findWithDefault [] entity names) ]
  cellLayoutSetAttributes columnUnicodeName columnRendererUnicodeName model $
    \(rule, entity) -> [
      cellText := if T.length entity == 1
                    then T.charName (T.head entity)
                    else "" ]

  layout <- tableNew 2 1 False  -- 2 rows, 1 column, autostretching = off
  tableAttachDefaults layout searchEntry 0 1 0 1
  tableAttachDefaults layout view 0 1 1 2
  window `containerAdd` layout

  set layout [
    tableChildYOptions searchEntry := [Fill] ]

  searchEntry `on` editableChanged $ do
    query <- get searchEntry entryText
    listStoreClear model
    let matches = matchAndSortRules query rules
    for_ matches $ \(name, result) ->
      listStoreAppend model (name, result)
    unless (null matches) $
      treeViewSetCursor view [0] Nothing

  let getVariantsNumber = length <$> listStoreToList model

  let choose mbIndex = do
        variantsNumber <- getVariantsNumber
        unless (variantsNumber == 0) $ do
          row <- listStoreGetValue model (fromMaybe 0 mbIndex)
          clipboard <- clipboardGet selectionClipboard
          clipboardSetText clipboard (snd row)
          clipboardStore clipboard
          widgetDestroy window

  view `on` rowActivated $ \path _ -> choose (listToMaybe path)

  let getSelectedRow = listToMaybe . fst <$> treeViewGetCursor view

  searchEntry `on` keyPressEvent $ do
    key <- T.unpack <$> eventKeyName
    mbIndex <- liftIO getSelectedRow
    variantsNumber <- liftIO getVariantsNumber
    let indexPrev Nothing  = if variantsNumber == 0 then [] else [0]
        indexPrev (Just i) = [max 0 (i-1)]
    let indexNext Nothing  = if variantsNumber == 0 then [] else [0]
        indexNext (Just i) = [min (variantsNumber-1) (i+1)]
    liftIO $ case key of
      "Up"   -> do treeViewSetCursor view (indexPrev mbIndex) Nothing
                   return True
      "Down" -> do treeViewSetCursor view (indexNext mbIndex) Nothing
                   return True
      _other -> return False

  searchEntry `on` entryActivated $
    choose =<< getSelectedRow

  widgetShowAll window
  mainGUI

main :: IO ()
main = do
  (rules, names, errors) <- readData
  unless (null errors) $ do
    putStrLn (unparagraphs errors)
    printf "Overall warnings: %d\n" (length errors)
  runGUI rules names

-- | Separate paragraphs with blank lines.
unparagraphs :: [String] -> String
unparagraphs =
  intercalate "\n" . map (++ "\n") .
  map (dropWhile (== '\n')) . map (dropWhileEnd (== '\n'))
