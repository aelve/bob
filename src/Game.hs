{-# LANGUAGE
NoImplicitPrelude,
OverloadedStrings
  #-}


module Main (main) where


-- General
import BasePrelude hiding (on)
-- Monads
import Control.Monad.IO.Class (liftIO)
-- Text
import Data.Text (Text)
import qualified Data.Text.ICU.Char as T
import Text.Printf
-- Catching exceptions
import Control.Spoon
-- Random
import System.Random
-- GUI
import Graphics.UI.Gtk


main :: IO ()
main = do
  initGUI

  window <- windowNew
  window `on` objectDestroy $ mainQuit
  set window [
    windowTitle := ("Bob game" :: Text),
    windowGravity := GravityCenter,
    windowWindowPosition := WinPosCenter,
    windowDefaultWidth := 500,
    windowDefaultHeight := 500 ]

  -- On Escape, exit.
  window `on` keyPressEvent $ tryEvent $ do
    "Escape" <- eventKeyName
    liftIO mainQuit

  label <- labelNew (Nothing :: Maybe Text)
  window `containerAdd` label

  let arrows = filter ((== Just T.Arrows) . spoon . T.blockCode) ['\0'..]

  char <- newIORef ""
  keys <- newIORef ""

  window `on` keyPressEvent $ do
    key <- eventKeyVal
    for_ (keyToChar key) $ \c ->
      liftIO $ modifyIORef keys (++ [c])
    return False
  
  flip timeoutAdd 4000 $ do
    do c <- readIORef char
       k <- readIORef keys
       unless (null c || null k) $
         appendFile "game.log" (printf "%s = %s\n" c k)
    x <- randomChoose arrows
    writeIORef char [x]
    writeIORef keys ""
    set label [
      labelText := (printf "<span font='70'>%c</span>" x :: String),
      labelUseMarkup := True ]    
    return True

  widgetShowAll window
  mainGUI

randomChoose :: [a] -> IO a
randomChoose xs = do
  n <- randomRIO (0, length xs - 1)
  return (xs !! n)
