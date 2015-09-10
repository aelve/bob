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
-- Monads
import Control.Monad.IO.Class (liftIO)
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- GUI
import Graphics.UI.Gtk
-- Bob-specific
import Bob



runGUI :: [Rule] -> IO ()
runGUI rules = do
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

  -- add a couple columns
  colChar <- treeViewColumnNew
  colRule <- treeViewColumnNew

  treeViewColumnSetTitle colChar ("Character" :: Text)
  treeViewColumnSetTitle colRule ("Rule" :: Text)

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
    let matches = matchRules query rules
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
  (rules, mbErrors) <- readRules
  case mbErrors of
    Nothing     -> return ()
    Just errors -> putStr errors
  runGUI rules
