module Parser (
  specialcharSpec,
  wordSpec,
  textSpec,
  rawSpec,
  strongemphSpec,
  weakemphSpec,
  formatSpec,
  dialogSpec,
  tellerSpec,
  componentSpec,
  paragraphSpec,
  ) where

import Test.Hspec
import Text.ParserCombinators.Parsec
import Data.Either

import Ogmarkup.Ast
import Ogmarkup.Parser

specialcharSpec :: Spec
specialcharSpec = do
  describe "specialchar" $ do
    it "should return the relevant character" $ do
      (parse specialchar "(Spec)" "!") `shouldBe` (Right ExclamationMark)

    it "should return a suspension points rather than several points" $ do
      (parse specialchar "(Spec)" "....") `shouldBe` (Right SuspensionPoints)

    it "should fail if the output is not a special character" $ do
      (parse specialchar "(Spec)" "x") `shouldSatisfy` isLeft

wordSpec :: Spec
wordSpec = do
  describe "word" $ do
    it "should return the relevant word" $ do
      (parse word "(Spec)" "bonjour") `shouldBe` (Right $ Word "bonjour")
    it "should fail if the output is a special character" $ do
      (parse specialchar "(Spec)" "*") `shouldSatisfy` isLeft

textSpec :: Spec
textSpec = do
  describe "text" $ do
    it "should return a word if the output is a word" $ do
      (parse text "(Spec)" "bonjour") `shouldBe` (Right $ Word "bonjour")
    it "should return a special character if the output is a special character" $ do
      (parse text "(Spec)" "?") `shouldBe` (Right $ QuestionMark)
    it "should consume white spaces" $ do
      (parse text "(Spec)" "     bonjour      ") `shouldBe` (Right $ Word "bonjour")

rawSpec :: Spec
rawSpec = do
  describe "raw" $ do
    it "should return a list of text" $ do
      (parse raw "(Spec)" "bonjour madame!") `shouldBe` (Right $ Raw [Word "bonjour",
                                                                      Word "madame",
                                                                      ExclamationMark])
    it "should stop whith a format marker" $ do
      (parse raw "(Spec)" "bonjour madame*!") `shouldBe` (Right $ Raw [Word "bonjour",
                                                                      Word "madame"])

    it "should stop whith a dialog marker" $ do
      (parse raw "(Spec)" "bonjour madame|!") `shouldBe` (Right $ Raw [Word "bonjour",
                                                                      Word "madame"])

    it "should consume inner whitespaces" $ do
      (parse raw "(Spec)" "bonjour   madame !")
        `shouldBe` (Right $ Raw [Word "bonjour", Word "madame", ExclamationMark])

strongemphSpec :: Spec
strongemphSpec = do
  describe "strongemph" $ do
    it "should return a list of text" $ do
      (parse strongemph "(Spec)" "**bonjour madame!**")
        `shouldBe` (Right $ StrongEmph [Word "bonjour", Word "madame", ExclamationMark])

    it "should consume inner white spaces" $ do
      (parse strongemph "(Spec)" "**  bonjour madame!  **")
        `shouldBe` (Right $ StrongEmph [Word "bonjour", Word "madame", ExclamationMark])

    it "should fail if the end marker is missing before the end" $ do
      (parse strongemph "(Spec)" "**bonjour madame!")
        `shouldSatisfy` isLeft

    it "... or before another marker" $ do
      (parse strongemph "(Spec)" "**bonjour madame!|")
        `shouldSatisfy` isLeft

    it "... or incomplete" $ do
      (parse strongemph "(Spec)" "**bonjour madame!*")
        `shouldSatisfy` isLeft

weakemphSpec :: Spec
weakemphSpec = do
  describe "weakemph" $ do
    it "should return a list of text" $ do
      (parse weakemph "(Spec)" "*bonjour madame!*")
        `shouldBe` (Right $ WeakEmph [Word "bonjour", Word "madame", ExclamationMark])

    it "should consume inner white spaces" $ do
      (parse weakemph "(Spec)" "*  bonjour madame!  *")
        `shouldBe` (Right $ WeakEmph [Word "bonjour", Word "madame", ExclamationMark])

    it "should fail if the end marker is missing before the end" $ do
      (parse weakemph "(Spec)" "*bonjour madame!")
        `shouldSatisfy` isLeft

    it "... or before another marker" $ do
      (parse weakemph "(Spec)" "*bonjour madame!|")
        `shouldSatisfy` isLeft

formatSpec :: Spec
formatSpec = do
  describe "format" $ do
    it "should return a strong emphasis" $ do
      (parse format "(Spec)" "**bonjour madame?**")
        `shouldBe` (Right $ StrongEmph [Word "bonjour", Word "madame", QuestionMark])

    it "should return a weak emphasis" $ do
      (parse format "(Spec)" "*bonjour madame?*")
        `shouldBe` (Right $ WeakEmph [Word "bonjour", Word "madame", QuestionMark])

    it "should return a raw text" $ do
      (parse format "(Spec)" "bonjour madame?")
        `shouldBe` (Right $ Raw [Word "bonjour", Word "madame", QuestionMark])

    it "should stop with a marker" $ do
      (parse format "(Spec)" "bonjour] madame?")
        `shouldBe` (Right $ Raw [Word "bonjour"])

    it "should consume outter whitespaces (1/2)" $ do
      (parse format "(Spec)" " **bonjour** ")
        `shouldBe` (Right $ StrongEmph [Word "bonjour"])

    it "should consume outter whitespaces (2/2)" $ do
      (parse format "(Spec)" "  bonjour ")
        `shouldBe` (Right $ Raw [Word "bonjour"])

    it "should failed when no closing format marker is found (1/2)" $ do
      (parse format "(Spec)" "**bonjour madame?")
        `shouldSatisfy` isLeft

    it "should failed when no closing format marker is found (2/2)" $ do
      (parse format "(Spec)" "**bonjour madame?<")
        `shouldSatisfy` isLeft

dialogSpec :: Spec
dialogSpec = do
  describe "dialog" $ do
    it "should parse a simple dialog" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour](tata)")
        `shouldBe` (Right $ Simple [Raw $ [Word "bonjour"]] "tata")

    it "should parse a dialog 'with say'" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour |dit-il| comment](toto)")
        `shouldBe` (Right $ WithSay [Raw [Word "bonjour"]]
                                    [Raw [Word "dit-il"]]
                                    [Raw [Word "comment"]]
                                    "toto")

    it "should deal with multiple formats" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour *madame*](titi)")
        `shouldBe` (Right $ Simple [Raw [Word "bonjour"],
                                    WeakEmph [Word "madame"]]
                                   "titi")

    it "should deal with no second part in with say dialog" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour, |cria-t-il|](tutu)")
        `shouldBe` (Right $ WithSay [Raw [Word "bonjour", Comma]]
                                    [Raw [Word "cria-t-il"]]
                                    []
                                    "tutu")

    it "should fail when required (1/5)" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour")
        `shouldSatisfy` isLeft

    it "should fail when required (2/5)" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour |madame")
        `shouldSatisfy` isLeft

    it "should fail when required (3/5)" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour |madame| monsieur")
        `shouldSatisfy` isLeft

    it "should fail when required (4/5)" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour |madame]")
        `shouldSatisfy` isLeft

    it "should fail when required (5/5)" $ do
      (parse (dialog '[' ']') "(Spec)" "[bonjour madame](truc")
        `shouldSatisfy` isLeft

tellerSpec :: Spec
tellerSpec = do
  describe "teller" $ do
    it "should parse a list of formats" $ do
      (parse teller "(Spec)" "bonjour, comment **allez vous**?")
        `shouldBe` (Right . Teller $ [Raw [Word "bonjour",
                                           Comma,
                                           Word "comment"],
                                      StrongEmph [Word "allez",
                                                  Word "vous"],
                                      Raw [QuestionMark]])

    it "should stop when it encounters a dialog marker" $ do
      (parse teller "(Spec)" "bonjour: [comment **allez vous**?")
        `shouldBe` (Right . Teller $ [Raw [Word "bonjour", Colon]])

    it "should fail when it encounters a closing dialog marker" $ do
      (parse teller "(Spec)" "bonjour: ]comment **allez vous**?")
        `shouldSatisfy` isLeft

    it "should fail when it encounters a closing dialog marker" $ do
      (parse teller "(Spec)" "bonjour: |comment **allez vous**?")
        `shouldSatisfy` isLeft

componentSpec :: Spec
componentSpec = do
  describe "component" $ do
    it "should return a teller text" $ do
      (parse component "(Spec)" "bonjour, je suis un *homme*! [")
        `shouldBe` (Right $ Teller [Raw [Word "bonjour",
                                         Comma,
                                         Word "je",
                                         Word "suis",
                                         Word "un"],
                                    WeakEmph [Word "homme"],
                                    Raw [ExclamationMark]])

    it "should deal with white spaces" $ do
      (parse component "(Spec)" "   bonjour, je suis un *homme*! [")
        `shouldBe` (Right $ Teller [Raw [Word "bonjour",
                                         Comma,
                                         Word "je",
                                         Word "suis",
                                         Word "un"],
                                    WeakEmph [Word "homme"],
                                    Raw [ExclamationMark]])

    it "should parse inner dialog" $ do
      (parse component "(Spec)" "<Franchement.. |pensa-t-il.|>(chose)")
        `shouldBe` (Right . Thought $ WithSay [Raw [Word "Franchement",
                                                    SuspensionPoints]]
                                              [Raw [Word "pensa-t-il", Point]]
                                              []
                                              "chose")

    it "should parse inner dialog with whitespaces" $ do
      (parse component "(Spec)" "   <Franchement.. |pensa-t-il.|     >(chose)")
        `shouldBe` (Right . Thought $ WithSay [Raw [Word "Franchement",
                                                    SuspensionPoints]]
                                              [Raw [Word "pensa-t-il", Point]]
                                              []
                                              "chose")

    it "should fail if a dialog is put inside a dialog" $ do
      (parse component "(Spec)" "<Franchement.. |pensa-t-il [Je](tyty)|>(tété)")
        `shouldSatisfy` isLeft

paragraphSpec :: Spec
paragraphSpec = do
  describe "paragraph" $ do
    it "should parse a complete paragraph" $ do
      parse paragraph "(Spec)" "Il faisait beau. [Salut, |cria l'homme,| comment va ?](homme) Je me tus...."
        `shouldBe` (Right $ Story [Teller [Raw [Word "Il", Word "faisait", Word "beau", Point]],
                                  (Audible $ WithSay [Raw [Word "Salut", Comma]]
                                                    [Raw [Word "cria", Word "l'homme", Comma]]
                                                    [Raw [Word "comment", Word "va", QuestionMark]]
                                                    "homme"),
                                  Teller [Raw [Word "Je", Word "me", Word "tus", SuspensionPoints]]])

    it "should parse a complete paragraph with white spaces" $ do
      parse paragraph "(Spec)" "   Il faisait beau. [Salut, |cria l'homme,| comment va ?](homme) Je me tus...."
        `shouldBe` (Right $ Story [Teller [Raw [Word "Il", Word "faisait", Word "beau", Point]],
                                  (Audible $ WithSay [Raw [Word "Salut", Comma]]
                                                    [Raw [Word "cria", Word "l'homme", Comma]]
                                                    [Raw [Word "comment", Word "va", QuestionMark]]
                                                    "homme"),
                                  Teller [Raw [Word "Je", Word "me", Word "tus", SuspensionPoints]]])

    it "should fail if needed" $ do
      parse paragraph "(Spec)" "Il faisait beau. Salut, |cria l'homme,| comment va ?](homme) Je me tus...."
        `shouldSatisfy` isLeft
