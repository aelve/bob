{-# LANGUAGE
RecordWildCards,
OverloadedStrings,
ScopedTypeVariables
  #-}


module Main (main) where


-- General
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative
import Control.Monad
-- Lenses
import Lens.Micro.GHC hiding (set)
-- Lists
import Data.List (permutations)
-- Text
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Char
-- Parsing
import Text.Parsec hiding ((<|>), many, optional)
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
  | AnyOf [Generator]
  | Permutation [Generator]
  deriving (Show)

spaced :: Parser a -> Parser a
spaced = between spaces spaces

literalP :: Parser Text
literalP = spaced $
  T.pack <$> asum [
    some literalChar,
    between (char '"') (char '"') (many quotedChar) ]
  where
    literalChar = satisfy $ \x ->
      or [isSymbol x, isPunctuation x, isAlphaNum x] &&
      x `notElem` ("\"()[]{}" :: String)
    quotedChar = asum [
      string "\\\"" >> pure '"',
      string "\\\\" >> pure '\\',
      satisfy $ \x -> not $ or [isSpace x, x == '"', x == '\\'] ]

generatorP :: Parser Generator
generatorP = spaced $ asum [
  Literal <$> literalP,
  AnyOf <$> between (char '(') (char ')') (some generatorP),
  Permutation <$> between (char '{') (char '}') (some generatorP) ]

data Matcher
  = Zip Text Text (Maybe Generator)
  | ManyToOne Generator Text
  deriving (Show)

evalGenerator :: Generator -> [Text]
evalGenerator (Literal x) = [x]
evalGenerator (AnyOf gs) = concatMap evalGenerator gs
evalGenerator (Permutation gs) = do
  perm :: [[Text]] <- permutations (map evalGenerator gs)
  chosen :: [Text] <- sequence perm
  return (mconcat chosen)

evalMatcher :: Matcher -> Mapping
evalMatcher (Zip lineA lineB mbGen) = do
  let as = T.chunksOf 1 lineA
      bs = T.chunksOf 1 lineB
  let additions = maybe [""] evalGenerator mbGen
  fromList $ concat $ do
    (a, b) <- zip as bs
    addition <- additions
    return [(addition <> a, b), (a <> addition, b)]
evalMatcher (ManyToOne g y) = fromList (zip (evalGenerator g) (repeat y))

matcherP :: Parser Matcher
matcherP = foldedLine $ asum [zipP, manyToOneP]
  where
    zipP = do
      string "zip"
      lineA <- literalP
      lineB <- literalP
      mbGen <- optional generatorP
      return (Zip lineA lineB mbGen)
    manyToOneP = do
      notFollowedBy space
      x <- literalP
      spaced (string "=")
      g <- many generatorP
      return (ManyToOne (AnyOf g) x)

data Rule = Rule {
  ruleName    :: Text,
  ruleMapping :: Mapping }
  deriving (Show)

ruleP :: Parser Rule
ruleP = do
  name <- currentLine
  mappings <- some (evalMatcher <$> try matcherP)
  -- Find matches assigned to more than one thing (e.g. “<<” meaning both ‘↞’
  -- and ‘↢’) – it's allowed in different rules, but not in the same rule.
  let combinedMapping = M.unionsWith (++) $
        over (each.each) (:[]) mappings
  let mappingErrors = do
        (k, vs) <- M.toList combinedMapping
        guard (length vs /= 1)
        return $ printf
          "“%s” in rule “%s” corresponds to more than 1 thing: %s"
          (T.unpack k) (T.unpack name)
          (T.unpack (T.intercalate ", " vs))
  unless (null mappingErrors) $
    fail (unlines mappingErrors)
  return $ Rule {
    ruleName    = name,
    ruleMapping = mconcat mappings }

rulesP :: Parser [Rule]
rulesP = ruleP `sepBy1` some newline

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
    windowTitle := ("Bob" :: String),
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

  treeViewColumnSetTitle colChar ("Character" :: String)
  treeViewColumnSetTitle colRule ("Rule" :: String)

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
    res <- parse rulesP ruleFile <$> T.readFile path
    case res of
      Left err -> putStrLn (show err) >> return []
      Right rules -> return rules
  runGUI rules

currentLine :: Parser Text
currentLine = T.pack <$> anyChar `manyTill` try (eof <|> void endOfLine)

-- | Run a parser on the next folded line, where folded line is the current
-- line + all consecutive lines indented further than the current line.
foldedLine :: Parser a -> Parser a
foldedLine p = do
  initialPos <- getPosition
  let column = sourceColumn initialPos
  line <- currentLine
  rest <- many (replicateM_ column (char ' ') >> currentLine)
  input <- getInput
  pos <- getPosition
  setInput (T.unlines (line : rest))
  setPosition initialPos
  result <- p
  eof
  setInput input
  setPosition pos
  return result
