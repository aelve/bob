{-# LANGUAGE
OverloadedStrings,
RecordWildCards
  #-}


module Main where


-- General
import Data.Maybe
import Data.Foldable
-- Lenses
import Lens.Micro
-- Text
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)


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
readRules = mapMaybe readRule . T.splitOn "\n\n"

matchRule :: Text -> Rule -> Maybe (Text, Text)
matchRule query Rule{..} = do
  result <- lookup query ruleMappings
  return (ruleName, result)

matchRules :: Text -> [Rule] -> [(Text, Text)]
matchRules query = mapMaybe (matchRule query)

main :: IO ()
main = do
  rules <- readRules <$> T.readFile "rules/rules"
  query <- T.getLine
  putStrLn ""
  for_ (matchRules query rules) $ \(name, result) ->
    printf "%s    (%s)\n" (T.unpack result) (T.unpack name)
