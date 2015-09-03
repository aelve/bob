{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
ExtendedDefaultRules
  #-}


module Main (main) where


-- General
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad
-- Lenses
import Lens.Micro hiding (set)
-- Lists
import Data.List.Split hiding (oneOf)
import Data.List (permutations)
-- Text
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Char
-- Parsing
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Text
-- Containers
import qualified Data.Map as M
import Data.Map (Map)
import GHC.Exts (fromList)
-- GUI
import Graphics.UI.Gtk
-- liftIO
import Control.Monad.IO.Class (liftIO)
-- Clipboard
import System.Hclip
-- Files
import System.FilePath
import System.Directory
-- Data files
import Paths_bob


type Mapping = Map Text Text

data Generator
  = Literal Text
  | Permutation [Text]
  deriving (Show)

spaced = between spaces spaces

literal :: Parser Text
literal = spaced $
  T.pack <$> asum [
    some literalChar,
    between (char '"') (char '"') (many quotedChar) ]
  where
    literalChar = satisfy $ \x ->
      or [isSymbol x, isPunctuation x, isAlphaNum x] &&
      x `notElem` ("\"'()[]{}\\" :: String)
    quotedChar = satisfy $ \x ->
      not $ or [isSpace x, x == '"', x == '\\']

generator :: Parser Generator
generator = spaced $ asum [
  Literal <$> literal,
  Permutation <$> between (char '{') (char '}') (some literal) ]

data Matcher
  = Zip Text Text
  | ManyToOne [Generator] Text
  deriving (Show)

evalGenerator :: Generator -> [Text]
evalGenerator (Literal x) = [x]
evalGenerator (Permutation xs) = map mconcat (permutations xs)

evalMatcher :: Matcher -> Mapping
evalMatcher (Zip a b) = fromList (zip (T.chunksOf 1 a) (T.chunksOf 1 b))
evalMatcher (ManyToOne gs y) = fromList (zip xs (repeat y))
  where xs = concatMap evalGenerator gs

matcher :: Parser Matcher
matcher = asum [zipP, manyToOneP]
  where
    zipP = do
      string "zip"
      a <- literal
      b <- literal
      return (Zip a b)
    manyToOneP = do
      x <- literal
      spaced (string "=")
      g <- many generator
      return (ManyToOne g x)

data Rule = Rule {
  ruleName    :: Text,
  ruleMapping :: Mapping }

readMatcher :: Text -> Either String Mapping
readMatcher s = case parse matcher "" s of
  Left err -> Left (show err)
  Right m  -> Right (evalMatcher m)

readRule :: Text -> ([String], Maybe Rule)
readRule s = case T.lines s of
  [] -> (["empty rule"], Nothing)
  (name:rest) -> do
    -- A matcher is a line + all lines following it that are indented with
    -- spaces.
    let isIndented line = " " `T.isPrefixOf` line
    let splitter = dropInitBlank $ keepDelimsL $ whenElt (not . isIndented)
    let matchers = split splitter rest
    let (errors, mappings) = partitionEithers $
          map (readMatcher . mconcat) matchers
    let rule = Rule {
          ruleName    = name,
          ruleMapping = mconcat mappings }
    (errors, Just rule)

readRules :: Text -> ([String], [Rule])
readRules s = (concatMap fst rules, mapMaybe snd rules)
  where
    rules = map readRule (T.splitOn (T.pack "\n\n") s)

matchRule :: Text -> Rule -> Maybe (Text, Text)
matchRule query Rule{..} = do
  result <- M.lookup query ruleMapping
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

  let getVariantsNumber = length <$> listStoreToList model

  let choose mbIndex = do
        variantsNumber <- getVariantsNumber
        unless (variantsNumber == 0) $ do
          row <- listStoreGetValue model (fromMaybe 0 mbIndex)
          setClipboard (T.unpack (snd row))
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
  dataDir <- getDataDir
  ruleFiles <- filter ((== ".rules") . takeExtensions) <$>
               getDirectoryContents (dataDir </> "rules")
  rules <- fmap concat . for ruleFiles $ \ruleFile -> do
    let path = dataDir </> "rules" </> ruleFile
    (errors, rules) <- readRules <$> T.readFile path
    unless (null errors) $ do
      putStrLn ""
      putStrLn ("errors while parsing rules in " ++ ruleFile)
      mapM_ (putStrLn . ("  " ++)) errors
    return rules
  runGUI rules
