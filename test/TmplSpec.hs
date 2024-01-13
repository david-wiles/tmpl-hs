module TmplSpec where

import Tmpl (parseVariables, templateReplace)
import Test.Hspec


parseVariablesSpec :: Spec
parseVariablesSpec = do
  describe "parseVariables" $ do
    it "parses a variable file" $ do
      vars <- parseVariables "test/vars.txt"
      vars `shouldBe` [("key1", "value1"), ("key2", "value2")]