module Tmpl
  ( parseVariables,
    replaceVariables,
    replaceTemplates,
  )
where

import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import System.FilePath (takeDirectory)
import Text.Regex.TDFA

type Vars = Map String String

whitespace = " \t\r"

trimBefore :: String -> String
trimBefore = dropWhile (`elem` whitespace)

trimAfter :: String -> String
trimAfter = reverse . trimBefore . reverse

trim :: String -> String
trim = trimAfter . trimBefore

-- read variables from a variable file. The file is expected to be in the format
-- key=value, one per line. Newlines will be ignored.
parseVariables :: String -> Vars
parseVariables contents =
  M.fromList $ map parseLine $ filter (not . null) $ lines contents
  where
    parseLine line =
      let (key, value) = break (== '=') line
       in (trim key, trim (drop 1 value))

-- replace all variables in a template file with their values using regex
replaceVariables :: Vars -> String -> String
replaceVariables vars template = snd $ last $ takeWhile fst $ iterate replace (True, template)
  where
    replace (_, text) =
      let (before, _, after, matches) = text =~ "\\{\\{[ \t\r]*\"([.0-9a-zA-Z/-]+)\"[ \t\r]*}}" :: (String, String, String, [String])
       in if not (null matches)
            then (True, before <> M.findWithDefault "" (head matches) vars <> after)
            else (False, text)

-- parse a template file and recursively replace all template sections with their
-- respective files relative to the current template file
replaceTemplates :: String -> IO String
replaceTemplates template = do
  let dir = takeDirectory template
  text <- readFile template
  snd <$> iterateWhileM fst (replace dir) (True, text)
  where
    replace dir (_, text) =
      let (before, _, after, matches) = text =~ "\\{\\{[ \t\r]*template \"([.0-9a-zA-Z/-]+)\"[ \t\r]*}}" :: (String, String, String, [String])
       in if not (null matches)
            then do
              let path = dir <> "/" <> head matches
              text' <- replaceTemplates path
              return (True, before <> text' <> after)
            else return (False, text)

iterateWhileM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateWhileM p f x = do
  x' <- f x
  if p x'
    then do
      iterateWhileM p f x'
    else return x
