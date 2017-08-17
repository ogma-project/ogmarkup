{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OgmarkupSpec where

import           Data.Text                        (Text)
import           Test.Hspec
import           Text.Shakespeare.Text

import           Text.Ogmarkup

spec :: Spec
spec = do
  describe "ogmarkup" $ do
    it "should merge paragraphs with consecutive dialogues" $
      ogmarkup consecutiveDialogues TestConf `shouldBe`
        "[doc][story][par][dial-anonymous]“[reply]This is a test.[/reply]”[/dial-anonymous][br][dial-anonymous]“[reply]May it works.[/reply]”[/dial-anonymous][/par][/story][/doc]"
    it "should not merge other paragraphs" $
      ogmarkup notConsecutiveDialogues TestConf `shouldBe`
        "[doc][story][par][dial-anonymous]“[reply]This is a test.[/reply]”[/dial-anonymous][/par][par]May it works.[/par][/story][/doc]"


consecutiveDialogues :: Text
consecutiveDialogues = "[This is a test.]\n\n[May it works.]"

notConsecutiveDialogues :: Text
notConsecutiveDialogues = "[This is a test.]\n\nMay it works."

data TestConf = TestConf

instance GenConf TestConf Text where
  typography _ = englishTypo

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
