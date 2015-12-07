{-# LANGUAGE
NoImplicitPrelude,
TupleSections,
RecordWildCards,
ScopedTypeVariables,
OverloadedStrings,
RankNTypes,
FlexibleContexts
  #-}


module Bob
(
  Pattern,
  Entity,
  Priority(..),
  RuleNote,
  Rule(..),
  Warning,
  readRuleFile,
  readData,
  matchRules,
)
where


-- General
import BasePrelude hiding (try)
import Numeric.Natural
-- Monads
import Control.Monad.Writer
-- Lenses
import Lens.Micro.GHC hiding ((&))
-- Text
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
-- Parsing
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.Lexer (integer)
-- Containers
import qualified Data.Map as M
import Data.Map (Map)
-- Files
import System.FilePath
import System.Directory
-- Data files
import Paths_bob


-- | A thing that we use for search (like “->”).
type Pattern = Text

-- | A thing that we search for (like “→”).
type Entity = Text

{- |
How high should the entity be in the list of results when it's searched by some pattern? @Top 1@ means “best match”, @Top 2@ means “best match or second best match”, etc. 'Whatever' means that you don't care.
-}
data Priority = Top Natural | Whatever
  deriving (Show, Eq)

instance Ord Priority where
  compare Whatever Whatever = EQ
  compare Whatever (Top _)  = GT
  compare (Top _) Whatever  = LT
  compare (Top a) (Top b)   = compare a b

type RuleNote = Text

-- | Entities corresponding to a pattern (like “->” leads to “→”). There can
-- be several entities corresponding to a pattern even inside a single rule.
type EntitiesMap = Map Pattern [(Entity, Priority)]

-- | All patterns that an entity corresponds to (like “→” leads to “->”, “>”).
type PatternsMap = Map Entity [(Pattern, Priority)]

toPatternsMap :: EntitiesMap -> PatternsMap
toPatternsMap m = fromListMulti $ do
  (pattern, entities) <- M.toList m
  (entity, priority) <- entities
  return (entity, (pattern, priority))

data Generator
  = Literal Pattern           -- abc or 'abc'
  | AnyOf [Generator]         -- (a b c)
  | Sequence [Generator]      -- a b c
  | Permutation [Generator]   -- {a b c}
  | Reference Entity          -- `name`
  | Variable                  -- ()
  deriving (Show)

priorityP :: WarnParser Priority
priorityP = lexeme (whatever <|> top)
  where
    whatever = char 'X' $> Whatever
    top = do
      x <- integer
      when (x == 0) $
        fail "priority can't be 0"
      return (Top (fromInteger x))

literalP :: WarnParser Pattern
literalP = T.pack <$> (literalString <|> quotedString)
  where
    -- A literal string is just some chars, like «abc».
    literalString = some literalChar
    literalChar = satisfy $ \x ->
      (isSymbol x || isPunctuation x || isAlphaNum x) &&
      x `notElem` ("\"'`()[]{}#" :: String)
    -- A quoted string looks like «'abc'» and can contain characters like
    -- «()» that can't be in an ordinary literal string.
    quotedString = singleQuotes (many quotedChar)
    quotedChar = choice [
      try (string "''") >> pure '\'',
      satisfy $ \x -> not (isSpace x) && x /= '\'' ]

generatorP :: WarnParser Generator
generatorP = lexeme $ do
  let singleGenerator = choice [
        Literal <$> literalP,
        Variable <$ try (string "()"),
        AnyOf <$> parens (some generatorP),
        Permutation <$> braces (some generatorP),
        Reference <$> backticks literalP ]
  gens <- some singleGenerator
  return $ case gens of
    [gen] -> gen
    _     -> Sequence gens

type Generators = [(Generator, Priority)]

data Matcher
  = Zip [(Pattern, Entity)] Generators
  | ManyToOne Generators Entity
  | Order Generator [Entity]
  deriving (Show)

evalGenerator
  :: PatternsMap     -- ^ Already generated patterns (needed for 'Reference')
  -> Maybe Text      -- ^ Variable value (needed for 'Variable')
  -> Generator       -- ^ Generator to evaluate
  -> Warn [Pattern]
evalGenerator _   _   (Literal x) = return [x]
evalGenerator psm var (AnyOf gs) = concat <$> mapM (evalGenerator psm var) gs
evalGenerator psm var (Sequence gs) = do
  ps :: [[Pattern]] <- mapM (evalGenerator psm var) gs
  return $ do
    chosen :: [Pattern] <- sequence ps
    return (mconcat chosen)
evalGenerator psm var (Permutation gs) = do
  ps :: [[Pattern]] <- mapM (evalGenerator psm var) gs
  return $ do
    perm :: [[Pattern]] <- permutations ps
    chosen :: [Pattern] <- sequence perm
    return (mconcat chosen)
evalGenerator psm _ (Reference x) = case M.lookup x psm of
  Nothing -> do
    warn (printf "‘%s’ was referenced but wasn't defined yet" (T.unpack x))
    return []
  -- The entity should be included as its own pattern, hence “x :”.
  Just pats -> return (x : map fst pats)
evalGenerator _ var Variable = case var of
  Nothing -> do
    warn "there's a variable in the rule but no value provided for it"
    return []
  Just x -> return [x]

evalGenerators
  :: PatternsMap                 -- ^ Already generated patterns
  -> Maybe Text                  -- ^ Variable value
  -> Generators                  -- ^ Pairs of (generator, priority)
  -> Warn [(Pattern, Priority)]
evalGenerators psm var gens = do
  groups :: [([Pattern], Priority)] <- (each._1) (evalGenerator psm var) gens
  -- Now groups have to be expanded, and patterns from later groups should
  -- replace earlier patterns. See https://github.com/aelve/bob/issues/47.
  let pats :: [(Pattern, Priority)]
      pats = [(p, f) | (ps, f) <- groups, p <- ps]
  -- To leave only the last occurrence of each pattern it's enough to convert
  -- the list to a Map and back, because that's how Map's fromList works.
  return $ M.toList (M.fromList pats)

{-
Note that a matcher can generate several entities for the same pattern, and sometimes it's even desirable – e.g. here “uU” can mean both “Ŭ” and “ŭ”:

@
zip AaEeIiOoUu
    ĂăĔĕĬĭŎŏŬŭ
    7: {(U u) ()}
@
-}
evalMatcher
  :: PatternsMap        -- ^ Already generated patterns
  -> Matcher            -- ^ Matcher to evaluate
  -> Warn EntitiesMap
evalMatcher psm (Zip pairs gens) = do
  results <- for pairs $ \(a, b) -> do
    patterns <- evalGenerators psm (Just a) gens
    return [(pattern, (b, priority)) | (pattern, priority) <- patterns]
  return $ fromListMulti (concat results)
evalMatcher psm (ManyToOne gens entity) = do
  patterns <- evalGenerators psm Nothing gens
  return $ fromListMulti $ do
    (pattern, priority) <- patterns
    return (pattern, (entity, priority))
evalMatcher psm (Order gen entities) = do
  patterns <- evalGenerator psm Nothing gen
  return $ fromListMulti $ do
    pattern <- patterns
    (entity, priority) <- zip entities [1..]
    return (pattern, (entity, Top priority))

generatorLineP :: WarnParser (Generator, Priority)
generatorLineP = do
  priority <- priorityP
  symbol ":"
  gens <- some generatorP
  return (AnyOf gens, priority)

matcherP :: WarnParser Matcher
matcherP = choice [zipP, manyToOneP, orderP]
  where
    zipP = do
      symbol "zip"
      lineA <- (T.chunksOf 1 <$> literalP) <* nextLine
      lineB <- indentation *> (T.chunksOf 1 <$> literalP) <* nextLine
      when (length lineA /= length lineB) $
        warn "lengths of zipped rows don't match"
      gens <- some (indentation *> generatorLineP <* nextLine)
      return (Zip (zip lineA lineB) gens)
    manyToOneP = do
      x <- try (lexeme literalP <* symbol "=")
      -- The 1st generator doesn't have any indentation, because it's on the
      -- same line as “=”.
      gen1 <- generatorLineP <* nextLine
      gens <- many (indentation *> generatorLineP <* nextLine)
      return (ManyToOne (gen1:gens) x)
    orderP = do
      gen <- try (generatorP <* symbol ":")
      entities <- some (lexeme literalP) <* nextLine
      return (Order gen entities)

data Rule = Rule {
  ruleNote     :: Maybe RuleNote,
  ruleEntities :: EntitiesMap }
  deriving (Eq, Show)

ruleP :: PatternsMap -> WarnParser Rule
ruleP scope = do
  pos <- getPosition
  mbNote <- optional $ do
    -- TODO: think up something better about rule names (also see ‘comment’)
    try (symbol "####")
    currentLine
  many comment
  let header = printf "warnings in rule at %s:" (show pos)
  groupWarnings header $ do
    matchers <- some matcherP
    -- Evaluate all matchers, combining generated patterns as we go along and
    -- passing them to each evaluator (so that references could be resolved).
    let go :: PatternsMap    -- ^ all entities in scope
           -> EntitiesMap    -- ^ all generated entities so far
           -> Int            -- ^ matcher number
           -> [Matcher]      -- ^ matchers left to process
           -> Warn EntitiesMap
        go _psm entitiesMap _ [] = return entitiesMap
        go  psm entitiesMap i (matcher:rest) = do
          -- TODO: what is entityMap? what is entitiesMap? it's unclear and
          -- should be explained
          entityMap <- evalMatcher psm matcher
          -- TODO: rename i to matcherIndex
          when ("" `M.member` entityMap) $
            warn $ printf "matcher #%d contains an empty pattern" i
          go (M.unionWith union psm (toPatternsMap entityMap))
             (M.unionWith union entityMap entitiesMap)
             (i+1) rest
    entitiesMap <- go scope mempty 1 matchers
    -- Return the rule.
    let rule = Rule {
          ruleNote     = mbNote,
          ruleEntities = entitiesMap }
    return rule

-- TODO: explain the format of ruleFileP, ruleP, etc
ruleFileP :: WarnParser [Rule]
ruleFileP = do
  many (blankline <|> comment)
  go mempty
  where
    go psm = option [] $ do
      rule <- ruleP psm
      let psm' = M.unionWith (++) psm (toPatternsMap (ruleEntities rule))
      choice [
        paragraphSeparator *> ((rule:) <$> go psm'),
        pure [rule] ]

{- |
A parser for files with character names. The format is as follows:

> entity1 = name1
> entity2 = name2
>
> entity3 = name3
>
> ...
-}
namesFileP :: WarnParser (Map Entity Text)
namesFileP = M.fromList . concat <$> some line `sepBy1` blankline
  where
    line = do
      entity <- lexeme literalP
      symbol "="
      name <- currentLine
      return (entity, name)

{- |
Find all patterns and associated entities.
-}
extractPatterns :: [Rule] -> [(Pattern, [(Entity, Priority)])]
extractPatterns = M.toList . M.unionsWith (++) . map ruleEntities

{- |
Find all entities and associated patterns.
-}
extractEntities :: [Rule] -> [(Entity, [(Pattern, Priority)])]
extractEntities = M.toList . M.unionsWith (++) .
                  map (toPatternsMap . ruleEntities)

{- |
Sort and group entities by priority. When an entity has several priorities, pick lowest.
-}
groupEntities :: [(Entity, Priority)] -> [([Entity], Priority)]
groupEntities =
  map (map entity &&& priority . head) . groupWith priority .
  map (entity . head &&& minimum . map priority) . groupWith entity
  where
    entity = fst
    priority = snd

{- |
This code checks whether all priorities are satisfied – that is, for each pattern and entity it checks whether the pattern finds the entity in top N matches (where N is entity's priority). It does so by enumerating all patterns, then taking all entities that some specific pattern finds, then ordering them in layers like this

  * “x”, “y” have priority <= 1
  * “x”, “y”, “m” have priority <= 4
  * “x”, “y”, “m”, “3”, “a” have priority <= 7

and finally outputting a warning for each layer that has more entities than its priority allows.
-}
checkPriorities :: [Rule] -> [Warning]
checkPriorities = mapMaybe checkPattern . extractPatterns
  where
    -- Discard a group if its priority isn't 'Top'.
    isTopPriority :: ([Entity], Priority) -> Maybe ([Entity], Int)
    isTopPriority (x, Top n) = Just (x, fromIntegral n)
    isTopPriority _          = Nothing
    -- Include earlier groups into later groups:
    -- if 1st group has priority 2 (i.e. “should be in first 2 matches”)
    -- and 2nd group has priority 3 (“should be in first 3 matches”)
    -- then members of the 1st group should be added to the 2nd group as well
    layers :: [([Entity], Int)] -> [([Entity], Int)]
    layers = map (concatMap fst &&& snd.last) . drop 1 . inits
    -- Find out whether a group is a good on (i.e. if it has priority N, it
    -- should have not more than N members).
    isGood :: ([Entity], Int) -> Bool
    isGood (entities, priority) = length entities <= priority
    -- Put it all together (and print warnings).
    checkPattern :: (Pattern, [(Entity, Priority)]) -> Maybe Warning
    checkPattern (pattern, pairs)
      | null warnings = Nothing
      | otherwise     = Just (unlines (header : warnings))
      where
        warnings = map generateWarning .
                   filter (not . isGood) .
                   layers . mapMaybe isTopPriority . groupEntities
                     $ pairs
        header = printf "‘%s’ finds:" (T.unpack pattern)
        generateWarning (entities, priority) =
          printf "  %d entities with priority %d or less: %s"
                 (length entities) priority
                 (unwords (map prettyChar entities))

{- |
This code finds entities that can't be entered using just the symbols on a standard keyboard.
-}
checkAscii :: [Rule] -> [Warning]
checkAscii rules =
  rules
    -- Get a list of entities and corresponding patterns.
    & extractEntities
    & map (\(e, xs) -> (e, map fst xs))
    -- Find entities that don't have at least 1 ASCII pattern.
    & filter (\(_, ps) -> all (not . isGoodPattern) ps)
    -- Generate warnings.
    & map (\(e, ps) -> printf "‘%s’ can't be found using only ASCII; \
                              \patterns that find it are: %s"
                              (prettyChar e) (T.unpack (T.unwords ps)))
  where
    isGoodPattern = T.all (\c -> isAscii c && isPrint c)

matchRule :: Pattern -> Rule -> [((Maybe RuleNote, Entity), Priority)]
matchRule query Rule{..} = do
  (entity, priority) <- M.findWithDefault [] query ruleEntities
  return ((ruleNote, entity), priority)

{- |
Find all matches for a pattern, sort results by priority. (When the same entity is matched by several rules, pick smallest priority.)

TODO: warn about the same pattern–entity pair having more than one rule note.
-}
matchRules :: Pattern -> [Rule] -> [((Maybe RuleNote, Entity), Priority)]
matchRules query rules =
  rules
    -- Do the matching.
    & concatMap (matchRule query)
    -- Group results by entity.
    & groupWith (snd . fst)
    -- For each entity, pick lowest priority and any non-empty rule note.
    & map (\xs -> let entity     = snd (fst (head xs))
                      priorities = map snd xs
                      notes      = map (fst . fst) xs
                  in  ((asum notes, entity), minimum priorities))
    -- Sort entities by priority.
    & sortOn snd

readRuleFile :: Maybe FilePath -> Text
             -> Either ParseError ([Rule], Maybe Warning)
readRuleFile mbName = warnParse (ruleFileP <* eof) (fromMaybe "" mbName)

-- | Returns rules, names, and warnings\/parsing errors (if there were any).
readData :: IO ([Rule], Map Entity Text, [Warning])
readData = do
  dataDir <- (</> "data") <$> getDataDir

  -- Read rule files.
  ruleFiles <- filter ((== ".rules") . takeExtensions) <$>
               getDirectoryContents dataDir
  -- TODO: rule files should be in their own folder and have extension “.txt”
  ruleResults <- for ruleFiles $ \ruleFile -> do
    let path = dataDir </> ruleFile
    res <- readRuleFile (Just ruleFile) <$> T.readFile path
    return $ case res of
      Left err -> ([], Just (show err))
      Right (rules, warning) -> (rules, warning)
  let rules = concat (map fst ruleResults)
      ruleErrors = checkPriorities rules ++ checkAscii rules ++
                   catMaybes (map snd ruleResults)

  -- Read entities' names.
  (names, nameErrors) <- do
    let path = dataDir </> "names.txt"
    res <- warnParse (namesFileP <* eof) "names.txt" <$> T.readFile path
    return $ case res of
      Left err -> (mempty, [show err])
      Right (names, warning) -> (names, maybeToList warning)

  -- TODO: warn when an entity doesn't have a name or has several names

  -- Return everything.
  return (rules, names, nameErrors ++ ruleErrors)

currentLine :: WarnParser Text
currentLine = choice [
  eol >> pure "",
  do x  <- anyChar
     xs <- anyChar `manyTill` try (eof <|> void eol)
     pure (T.pack (x:xs)) ]

type Warning = String

type Warn a = forall m. MonadWriter [Warning] m => m a

warn :: Warning -> Warn ()
warn s = tell [s]

type WarnParser a = WriterT [Warning] Parser a

groupWarnings :: String -> WarnParser a -> WarnParser a
groupWarnings title = censor $ \s ->
  if null s then [] else title : map ("  " ++) s

warnParse :: WarnParser a -> FilePath -> Text ->
             Either ParseError (a, Maybe Warning)
warnParse p src s = case parse (runWriterT p) src s of
  Left err -> Left err
  Right (x, []) -> Right (x, Nothing)
  Right (x, ws) -> Right (x, Just (unlines ws))

-- Closing parens/braces aren't allowed to skip trailing space, because
-- otherwise “(...) (...)” and “(...)(...)” would mean the same thing, and
-- they definitely don't mean the same thing. However, opening parens/braces
-- can skip trailing space.
parens, braces, singleQuotes, backticks :: WarnParser a -> WarnParser a
parens       = between (symbol "(") (string ")")
braces       = between (symbol "{") (string "}")
singleQuotes = between (string "'") (string "'")
backticks    = between (string "`") (string "`")

lexeme :: WarnParser a -> WarnParser a
lexeme = Lexer.lexeme (skipMany (char ' '))

symbol :: String -> WarnParser String
symbol = Lexer.symbol (skipMany (char ' '))

{- |
Create a map from a list of pairs. When several values correspond to the same key, they are concatenated.

>>> fromListConcat [(1, "foo"), (2, "bar"), (1, "baz")]
{1: "foobaz", 2: "bar"}
-}
fromListConcat :: (Ord a, Monoid b) => [(a, b)] -> Map a b
fromListConcat = M.fromListWith (<>)

{- |
Create a map from a list of pairs. When several values correspond to the same key, they are all put into a list.

>>> fromListConcat [(1, "foo"), (2, "bar"), (1, "baz")]
{1: ["foo", "baz"], 2: ["bar"]}
-}
fromListMulti :: Ord a => [(a, b)] -> Map a [b]
fromListMulti = fromListConcat . over (each._2) (:[])

prettyChar :: Entity -> String
prettyChar x
  | T.all good x = T.unpack x
  | otherwise    = "‘" ++ T.unpack x ++ "’"
  where good c = isAlphaNum c || isSymbol c

comment :: WarnParser ()
comment = void $ do
  try $ do
    skipMany (char ' ')
    string "#"
    -- We don't want to treat “####” as a comment because lets you add notes
    -- to rules (in the beginning of the rule – look at ruleP). This is a
    -- hack kinda.
    --
    -- TODO: think up something better about rule notes
    notFollowedBy (string "###")
  anyChar `manyTill` try (void eol <|> eof)

-- | Call this when you've parsed everything you needed from the current line
-- and now have to skip to the next line.
nextLine :: WarnParser ()
nextLine = do
  skipMany (char ' ')
  eof <|> void eol <|> comment
  skipMany comment

indentation :: WarnParser ()
indentation = skipSome (char ' ')

blankline :: WarnParser ()
blankline = void eol

paragraphSeparator :: WarnParser ()
paragraphSeparator = void $ do
  -- There has to be a blank line between 2 paragraphs, and there can also be
  -- comments. Comments right after the paragraph are allowed, but there
  -- still has to be a blank line somewhere.
  many comment
  blankline
  many (blankline <|> comment)
