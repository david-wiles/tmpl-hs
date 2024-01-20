module TmplSpec where

import qualified Data.Map as M
import Test.Hspec ( Spec, describe, it, shouldBe, shouldReturn )
import Tmpl (parseVariables, templateReplace)

parseVariablesSpec :: Spec
parseVariablesSpec = do
  describe "parseVariables" $ do
    it "parses a variable file" $ do
      vars <- parseVariables "test/vars.txt"
      vars
        `shouldBe` M.fromList [ ("key1", "value1"),
                     ("key2", "value2"),
                     ("key3", "var with a space"),
                     ("one.one", "1"),
                     ("two.two", "2")
                   ]

templateReplaceSpec :: Spec
templateReplaceSpec = do
  describe "templateReplace" $ do
    it "replaces variables in a template" $ do
      vars <- parseVariables "test/vars.txt"
      template <- readFile "test/template.txt"
      ref <- readFile "test/reference.txt"
      let result = templateReplace vars "./test/" template
      result `shouldReturn` ref