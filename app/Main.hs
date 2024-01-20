module Main where

import Control.Monad (mapM)
import Data.List (foldl)
import qualified Data.Map as M
import Options.Applicative
import Options.Applicative.Help (string)
import Tmpl (parseVariables, templateReplace)

explain =
  "This template engine resembles the Go template engine. To replace\n\
  \a section with another template file, use\n\
  \\n\
  \{{ template \"<filename>\" }}\n\
  \\n\
  \To use a variable defined in the variable file, use\n\
  \\n\
  \{{ \"<key>\" }}\n\
  \\n\
  \to replace it with the value associated with the key. If the key is\n\
  \not found, it will be replaced with an empty string."

data Arguments = Arguments
  { templateFile :: String,
    variableFiles :: [String],
    outputFile :: String
  }
  deriving (Show)

opts :: Parser Arguments
opts =
  Arguments
    <$> argument
      str
      ( metavar "TEMPLATE"
          <> help "Template file to use"
      )
    <*> many
      ( strOption
          ( long
              "varfiles"
              <> metavar "VARFILES"
              <> short 'v'
              <> help "Variable files"
          )
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> value "/dev/stdout"
          <> help "Output file to write to. Defaults to stdout"
      )

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch s = go [] ch s
  where
    go acc ch (x : xs) =
      if x == ch then acc : splitOn ch xs else go (acc <> [x]) ch xs

run :: Arguments -> IO ()
run (Arguments tf vf o) = do
  parsed <- mapM parseVariables vf
  let vars = foldl M.union M.empty parsed
  t <- readFile tf
  text <- templateReplace vars "./" t
  writeFile o text

main :: IO ()
main = run =<< execParser parserOpts
  where
    parserOpts = info (opts <**> helper) (fullDesc <> progDescDoc (Just (string explain)))