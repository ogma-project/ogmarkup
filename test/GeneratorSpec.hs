{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GeneratorSpec where

import           Text.Shakespeare.Text
import           Data.Maybe

import           Data.Text                        (Text)
import           Test.Hspec

import           Text.Ogmarkup.Private.Config
import           Text.Ogmarkup.Private.Typography

import qualified Text.Ogmarkup.Private.Ast        as Ast
import qualified Text.Ogmarkup.Private.Generator  as Gen

spec :: Spec
spec = describe "format" $ do
          it "should deal with raw input" $
            Gen.runGenerator (Gen.format sentence) testConf
              `shouldBe` "Bonjour toi."

testConf :: GenConf Text
testConf = GenConf frenchTypo
                   (\ doc        -> [st|[doc]#{doc}[/doc]|])
                   (\ story      -> [st|[story]#{story}[/story]|])
                   (\ cls aside  -> case cls of
                                      Just c  -> [st|[aside-#{c}]#{aside}[/aside-#{c}]|]
                                      Nothing -> [st|[aside]#{aside}[/aside]|])
                   (\ par        -> [st|[par]#{par}[/par]|])
                   (\ teller     -> [st|[tell]#{teller}[/tell]|])
                   (\ auth dial  -> [st|[dial-#{auth}]#{dial}[/dial-#{auth}]|])
                   (\ auth thou  -> [st|[thou-#{auth}]#{thou}[/thou-#{auth}]|])
                   (\ rep        -> [st|[reply]#{rep}[/reply]|])
                   "[br]"
                   (\ txt        -> [st|[em]#{txt}[/em]|])
                   (\ txt        -> [st|[strong]#{txt}[/strong]|])
                   (maybe "anonymous" id)
                   testPrintSpace

testPrintSpace :: Space -> Text 
testPrintSpace None = ""
testPrintSpace Normal = " "
testPrintSpace Nbsp = "_"

sentence :: Ast.Format Text
sentence = Ast.Raw [ Ast.Word "Bonjour"
                   , Ast.Word "toi"
                   , Ast.Punctuation Ast.Point ]
