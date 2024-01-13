module Tmpl
  ( parseVariables,
    templateReplace,
  )
where

import Data.List (isPrefixOf)

trim :: String -> String
trim = 
  takeWhile (not . (`elem` whitespace)) . dropWhile (`elem` whitespace)
  where
    whitespace = " \t\n\r"

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
    replace template (key, value) = replaceAll ("{{ \"" ++ key ++ "\" }}") value template

replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll from to str@(x : xs)
  | from `isPrefixOf` str = to ++ replaceAll from to (drop (length from) str)
  | otherwise = x : replaceAll from to xs

-- parse a template file and recursively replace all template sections with their
-- respective files relative to the current directory
templateReplace :: String -> [(String, String)] -> IO String
templateReplace template@(t : ts) vars =
  if "{{ template \"" `isPrefixOf` template
    then do
      let (filename, tmpl) = break (== '"') $ drop 13 template
      text <- readFile filename
      replacement <- templateReplace (variableReplace vars text) vars
      rest <- templateReplace tmpl vars
      return $ replacement <> rest
    else return [t] <> templateReplace ts vars
