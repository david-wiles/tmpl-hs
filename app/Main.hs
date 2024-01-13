module Main where

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
    variableFile :: String,
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
    <*> strOption
      ( long "varfile"
          <> metavar "VARFILE"
          <> short 'v'
          <> help "Variable file to use"
      )
    <*> option
      auto
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> value "/dev/stdout"
          <> help "Output file to write to. Defaults to stdout"
      )

run :: Arguments -> IO ()
run (Arguments tf vf o) = do
  vars <- parseVariables vf
  t <- readFile tf
  text <- templateReplace t vars
  writeFile o text

main :: IO ()
main = run =<< execParser parserOpts
  where
    parserOpts = info (opts <**> helper) (fullDesc <> progDescDoc (Just (string explain)))