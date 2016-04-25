{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
spec = do describe "format" $ do
            it "should deal with raw input" $
              Gen.runGenerator (Gen.format sentence) TestConf
                `shouldBe` "Bonjour toi."

          describe "component" $ do
            it "should deal with thought" $
              Gen.runGenerator (Gen.component False False (Ast.Thought simpleReply Nothing))
                               TestConf
                `shouldBe` "[thou-anonymous][reply]Bonjour toi.[/reply][/thou-anonymous]"

data TestConf = TestConf

instance GenConf TestConf Text where
  typography _ = frenchTypo

  documentTemplate _ doc = [st|[doc]#{doc}[/doc]|]

  errorTemplate _ err = [st|[error]#{err}[/error]|]

  storyTemplate _ story = [st|[story]#{story}[/story]|]

  asideTemplate _ (Just cls) aside = [st|[aside-#{cls}]#{aside}[/aside-#{cls}]|]
  asideTemplate _ _ aside = [st|[aside]#{aside}[/aside]|]

  paragraphTemplate _ par = [st|[par]#{par}[/par]|]

  tellerTemplate _ teller = [st|[tell]#{teller}[/tell]|]

  dialogueTemplate _ auth dial = [st|[dial-#{auth}]#{dial}[/dial-#{auth}]|]

  thoughtTemplate _ auth thou = [st|[thou-#{auth}]#{thou}[/thou-#{auth}]|]

  replyTemplate _ rep = [st|[reply]#{rep}[/reply]|]

  betweenDialogue _ = "[br]"

  emphTemplate _ txt = [st|[em]#{txt}[/em]|]

  strongEmphTemplate _ txt = [st|[strong]#{txt}[/strong]|]

  authorNormalize _ = maybe "anonymous" id

  printSpace _ None = ""
  printSpace _ Normal = " "
  printSpace _ Nbsp = "_"

sentence :: Ast.Format Text
sentence = Ast.Raw [ Ast.Word "Bonjour"
                   , Ast.Word "toi"
                   , Ast.Punctuation Ast.Point ]

simpleReply :: Ast.Reply Text
simpleReply = Ast.Simple [ sentence ]
