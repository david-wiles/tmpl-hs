module Tmpl
  ( parseVariables,
    templateReplace,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (isPrefixOf)
import System.FilePath (takeDirectory)

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
parseVariables :: String -> IO Vars
parseVariables filename = do
  contents <- readFile filename
  return $ M.fromList $ map parseLine $ filter (not . null) $ lines contents
  where
    parseLine :: String -> (String, String)
    parseLine line = (trim key, trim (drop 1 value))
      where
        (key, value) = break (== '=') line

-- replace all variables in a template file with their values
variableReplace :: Vars -> String -> String
variableReplace vars template = foldl replace template $ M.toList vars
  where
    replace template (key, value) = 
      replaceAll (\s -> s "{{ \"" <> key <> "\" }}") value template

replaceAll :: (String -> Bool) -> String -> String -> String
replaceAll _ _ [] = []
replaceAll from to str@(x : xs)
  | from str = to ++ replaceAll from to (drop (length from) str)
  | otherwise = x : replaceAll from to xs

-- parse a template file and recursively replace all template sections with their
-- respective files relative to the current template file. Replace templates, 
-- then variables
templateReplace :: Vars -> String -> String -> IO String
templateReplace _ _ [] = return []
templateReplace vars cwd template@(t : ts) =
  if "{{ template \"" `isPrefixOf` template
    then do
      let (filename, tmpl) = break (== '"') $ drop 13 template
          path = cwd <> "/" <> filename
      text <- readFile path
      replacement <- templateReplace vars (takeDirectory path) text
      rest <- templateReplace vars cwd (drop 4 tmpl)
      return $ variableReplace vars $ replacement <> rest
    else do
      rest <- templateReplace vars cwd ts
      return $ variableReplace vars $ [t] <> rest
