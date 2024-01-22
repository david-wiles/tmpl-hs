module TmplSpec where

import qualified Data.Map as M
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Tmpl (parseVariables, replaceTemplates, replaceVariables)

parseVariablesSpec :: Spec
parseVariablesSpec = do
  describe "parseVariables" $ do
    it "parses a variable file" $ do
      text <- readFile "test/vars.txt"
      parseVariables text
        `shouldBe` M.fromList
          [ ("key1", "value1"),
            ("key2", "value2"),
            ("key3", "var with a space"),
            ("one.one", "1"),
            ("two.two", "2")
          ]

replaceVariablesSpec :: Spec
replaceVariablesSpec = do
  describe "replaceVariables" $ do
    it "replaces variables in a template" $ do
      varf <- readFile "test/vars.txt"
      let template = "{{ \"key1\" }} {{ \"key2\" }} {{ \"key3\" }} {{ \"one.one\" }} {{ \"two.two\" }}"
          ref = "value1 value2 var with a space 1 2"
          vars = parseVariables varf
      replaceVariables vars template `shouldBe` ref

replaceTemplateSpec :: Spec
replaceTemplateSpec = do
  describe "replaceTemplates" $ do
    it "replaces templates in a template" $ do
      varf <- readFile "test/vars.txt"
      let vars = parseVariables varf
      ref <- readFile "test/reference.txt"
      template <- replaceTemplates "test/template.txt"
      replaceVariables vars template `shouldBe` ref