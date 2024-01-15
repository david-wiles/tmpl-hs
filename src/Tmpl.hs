module Tmpl
  ( parseVariables,
    templateReplace,
  )
where

import Data.List (isPrefixOf)
import System.FilePath (takeDirectory)

whitespace = " \t\n\r"

trimBefore :: String -> String
trimBefore = dropWhile (`elem` whitespace)

trimAfter :: String -> String
trimAfter = reverse . trimBefore . reverse

trim :: String -> String
trim = trimAfter . trimBefore

-- read variables from a variable file. The file is expected to be in the format
-- key=value, one per line. Newlines will be ignored.
parseVariables :: String -> IO [(String, String)]
parseVariables filename = do
  contents <- readFile filename
  return $ map parseLine $ filter (not . null) $ lines contents
  where
    parseLine :: String -> (String, String)
    parseLine line = (trim key, trim (drop 1 value))
      where
        (key, value) = break (== '=') line

-- replace all variables in a template file with their values
variableReplace :: [(String, String)] -> String -> String
variableReplace vars template = foldl replace template vars
  where
    replace template (key, value) = replaceAll ("{{ \"" <> key <> "\" }}") value template

replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll from to str@(x : xs)
  | from `isPrefixOf` str = to ++ replaceAll from to (drop (length from) str)
  | otherwise = x : replaceAll from to xs

-- parse a template file and recursively replace all template sections with their
-- respective files relative to the current template file
templateReplace :: [(String, String)] -> String -> String -> IO String
templateReplace _ _ [] = return []
templateReplace vars cwd template@(t : ts) =
  if "{{ template \"" `isPrefixOf` template
    then do
      let (filename, tmpl) = break (== '"') $ drop 13 template
          path = cwd <> "/" <> filename
      text <- readFile path
      replacement <- templateReplace vars (takeDirectory path) (variableReplace vars text)
      rest <- templateReplace vars cwd (variableReplace vars $ drop 4 tmpl)
      return $ replacement <> rest
    else do
      rest <- templateReplace vars cwd ts
      return $ variableReplace vars $ [t] <> rest
