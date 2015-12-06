{-# LANGUAGE
NoImplicitPrelude,
RecordWildCards,
OverloadedStrings,
ScopedTypeVariables,
FlexibleContexts
  #-}


module Main (main) where


-- General
import BasePrelude hiding (on)
-- Lenses
import Lens.Micro.GHC hiding (set)
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
import Numeric (showHex)
-- GUI
import Graphics.UI.Gtk
-- Browser
import Web.Browser
-- Bob-specific
import Bob


runGUI :: [Rule] -> Map Entity Text -> IO ()
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
  (columnDescription, columnRendererDescription) <- addColumn "Description"
  (columnUnicodeName, columnRendererUnicodeName) <- addColumn "Unicode name"

  cellLayoutSetAttributes columnChar columnRendererChar model $
    \(_mbNote, entity) -> [ cellText := entity ]
  cellLayoutSetAttributes columnDescription columnRendererDescription model $
    \(mbNote, entity) -> [
      cellText := let name = case M.lookup entity names of
                        Nothing -> []
                        Just x  -> [x]
                      note = case mbNote of
                        Nothing -> []
                        Just x  -> ["[" <> x <> "]"]
                  in  T.unwords (name ++ note) ]
  cellLayoutSetAttributes columnUnicodeName columnRendererUnicodeName model $
    \(_mbNote, entity) -> [
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
    let matches = matchRules query rules
    for_ matches $ \((name, result), _priority) ->
      listStoreAppend model (name, result)
    unless (null matches) $
      treeViewSetCursor view [0] Nothing

  let getVariantsCount = length <$> listStoreToList model

  let choose mbIndex = do
        variantsCount <- getVariantsCount
        unless (variantsCount == 0) $ do
          row <- listStoreGetValue model (fromMaybe 0 mbIndex)
          clipboard <- clipboardGet selectionClipboard
          clipboardSetText clipboard (snd row)
          clipboardStore clipboard
          widgetDestroy window

  view `on` rowActivated $ \path _ -> choose (listToMaybe path)

  -- TODO: rename “view” to something better

  let getSelectedRow = listToMaybe . fst <$> treeViewGetCursor view

  viewMenu <- do
    menu <- menuNew
    infoItem <- menuItemNewWithLabel ("See information" :: Text)
    infoItem `on` menuItemActivated $ do
      mbI <- getSelectedRow
      let i = case mbI of
                Nothing -> error "tried to show menu but nothing is selected"
                Just x  -> x
      (_note, entity) <- listStoreGetValue model i
      if T.length entity == 1
        then let code = showHex (fromEnum (T.head entity)) ""
             in  void $ openBrowser ("http://unicode-table.com/en/" ++ code)
        else putStrLn "can't look up entities that aren't single characters"
    menuShellAppend menu infoItem
    widgetShowAll menu
    return menu

  view `on` buttonPressEvent $ tryEvent $ do
    time <- eventTime
    SingleClick <- eventClick
    RightButton <- eventButton
    coords <- over each round <$> eventCoordinates
    liftIO $ do
      mbPath <- treeViewGetPathAtPos view coords
      case mbPath of
        Nothing -> return ()
        Just (path, _, _) -> do
          treeViewSetCursor view path Nothing
          menuPopup viewMenu (Just (RightButton, time))

  view `on` popupMenuSignal $ do
    -- TODO: the menu should appear at the selected thing
    -- (also, a question: what should be done if the selected thing has
    -- been scrolled away and isn't even visible?)
    menuPopup viewMenu (Just (OtherButton 0, currentTime))
    return True

  searchEntry `on` keyPressEvent $ do
    key <- T.unpack <$> eventKeyName
    mbIndex <- liftIO getSelectedRow
    variantsCount <- liftIO getVariantsCount
    let indexPrev Nothing  = if variantsCount == 0 then [] else [0]
        indexPrev (Just i) = [max 0 (i-1)]
    let indexNext Nothing  = if variantsCount == 0 then [] else [0]
        indexNext (Just i) = [min (variantsCount-1) (i+1)]
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
