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


-- | A thing that we use for search (like “->”).
type Pattern = Text

-- | A thing that we search for (like “→”).
type Entity = Text

type RuleName = Text

type Mapping = Map Pattern Entity

data Generator
  = Literal Pattern
  | AnyOf [Generator]
  | Permutation [Generator]
  deriving (Show)

literalP :: Parser Pattern
literalP = T.pack <$> asum [
  some literalChar,
  between (char '\'') (char '\'') (many quotedChar) ]
  where
    literalChar = satisfy $ \x ->
      or [isSymbol x, isPunctuation x, isAlphaNum x] &&
      x `notElem` ("\"'()[]{}" :: String)
    quotedChar = asum [
      try (string "''") >> pure '\'',
      satisfy $ \x -> not $ or [isSpace x, x == '\''] ]

generatorP :: Parser Generator
generatorP = many (char ' ') *> asum [
  Literal <$> literalP,
  AnyOf <$> between (char '(') (char ')') (some generatorP),
  Permutation <$> between (char '{') (char '}') (some generatorP) ]

data Matcher
  = Zip Text Text (Maybe Generator)
  | ManyToOne Generator Entity
  deriving (Show)

evalGenerator :: Generator -> [Pattern]
evalGenerator (Literal x) = [x]
evalGenerator (AnyOf gs) = concatMap evalGenerator gs
evalGenerator (Permutation gs) = do
  perm :: [[Pattern]] <- permutations (map evalGenerator gs)
  chosen :: [Pattern] <- sequence perm
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
matcherP = asum [zipP, manyToOneP]
  where
    -- Literals/generators/etc can go on a new line, but then they have to be
    -- indented. 'breaker' is a parser which either parses -a newline + some
    -- indentation-, or -just some spaces-.
    breaker = try (newline >> some (char ' ')) <|> some (char ' ')
    zipP = do
      string "zip"
      lineA <- breaker *> literalP
      lineB <- breaker *> literalP
      when (T.length lineA /= T.length lineB) $
        fail "lengths of zipped rows don't match"
      mbGen <- optional (breaker *> generatorP)
      return (Zip lineA lineB mbGen)
    manyToOneP = do
      x <- literalP
      breaker *> string "="
      g <- many (breaker *> generatorP)
      return (ManyToOne (AnyOf g) x)

data Rule = Rule {
  ruleName    :: RuleName,
  ruleMapping :: Mapping }
  deriving (Show)

ruleP :: Parser Rule
ruleP = do
  name <- currentLine
  mappings <- (evalMatcher <$> matcherP) `endBy1` newline
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

matchRule :: Pattern -> Rule -> Maybe (RuleName, Entity)
matchRule query Rule{..} = do
  result <- M.lookup query ruleMapping
  return (ruleName, result)

matchRules :: Pattern -> [Rule] -> [(RuleName, Entity)]
matchRules query = mapMaybe (matchRule query)

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
currentLine = asum [
  endOfLine >> pure "",
  do x  <- anyChar
     xs <- anyChar `manyTill` try (eof <|> void endOfLine)
     pure (T.pack (x:xs)) ]

