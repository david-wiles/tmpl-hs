module Main where

import qualified TmplSpec
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec TmplSpec.parseVariablesSpec
  hspec TmplSpec.replaceTemplateSpec
  hspec TmplSpec.replaceVariablesSpec