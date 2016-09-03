{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Data.Either
import           Data.Text                     (Text)
import           Test.Hspec
import           Text.ParserCombinators.Parsec

import qualified Text.Ogmarkup.Private.Ast     as Ast
import qualified Text.Ogmarkup.Private.Parser  as Parser

shouldParse x b = x `shouldBe` Right b

shouldFail x = x `shouldSatisfy` isLeft

spec :: Spec
spec = do
    describe "atom" $ do
      it "should not parse an empty string" $ shouldFail (Parser.parse Parser.atom "" "")

      it "should parse one word" $ Parser.parse Parser.atom "" hiStr `shouldParse` hiAtom

      it "should parse one punctuation mark" $
        Parser.parse Parser.atom "" exclamationStr `shouldParse` exclamationAtom

    describe "format" $ do
      it "should parse one quote" $
        Parser.parse Parser.format "" quoteStr `shouldParse` quoteFormat

      it "should support french quotes" $ do
        Parser.parse Parser.format "" frenchQuoteStr `shouldParse` frenchQuoteFormat

      it "should fail if the quote is ill-formed (no closing quote)" $
        shouldFail (Parser.parse Parser.format "" illQuoteStr)

      it "should parse nested formats" $
        Parser.parse Parser.format "" nestedFormatsStr `shouldParse` nestedFormatsFormat

      it "should fail with nested same format" $ do
        shouldFail (Parser.parse Parser.format "" nestedEmphStr)
        shouldFail (Parser.parse Parser.format "" nestedStrongEmphStr)

    describe "reply" $ do
      it "should accept spaces at the beginning of dialogs" $ do
        Parser.parse (Parser.reply '[' ']') "" dialogStartingWithSpaceStr `shouldParse` dialogStartingWithSpaceReply
        Parser.parse (Parser.reply '[' ']') "" clauseStartingWithSpaceStr `shouldParse` clauseStartingWithSpaceReply

    describe "ill-formed paragraphs" $ do
      it "ill-formed components should be accepted as-is" $ do
        Parser.parse Parser.component "" illQuoteStr  `shouldParse` Ast.IllFormed illQuoteStr
        Parser.parse Parser.component "" nestedEmphStr  `shouldParse` Ast.IllFormed nestedEmphStr
        Parser.parse Parser.component "" nestedStrongEmphStr  `shouldParse` Ast.IllFormed nestedStrongEmphStr
      it "an ill-formed paragraph should not prevent parsing correctly the others" $ do
        Parser.parse Parser.story "" secondParagraphIllFormed `shouldParse` secondParagraphIllFormedPartialCompilation

    describe "document" $ do
      it "should try its best to compile an ill-formed document" $ do
        Parser.parse Parser.document "" (storyStr ++ "\n\n" ++ asideIllFormed) `shouldParse` ([storyAst], asideIllFormed)

hiStr = "hi"
hiAtom = Ast.Word "hi"

exclamationStr = "!"
exclamationAtom = Ast.Punctuation Ast.Exclamation

quoteStr = "\"hi everyone.\""
quoteFormat = Ast.Quote [Ast.Raw [hiAtom, Ast.Word "everyone", Ast.Punctuation Ast.Point]]

frenchQuoteStr = "« hi everyone. »"
frenchQuoteFormat = Ast.Quote [Ast.Raw [hiAtom, Ast.Word "everyone", Ast.Punctuation Ast.Point]]

illQuoteStr = "\"hi"

nestedFormatsStr    = "*hi.. \"everyone\".*"
nestedFormatsFormat = Ast.Emph [ Ast.Raw [ Ast.Word "hi"
                                         , Ast.Punctuation Ast.SuspensionPoints
                                         ]
                               , Ast.Quote [ Ast.Raw [ Ast.Word "everyone" ] ]
                               , Ast.Raw [ Ast.Punctuation Ast.Point ]
                               ]

nestedEmphStr = "*hi \"*miss*\"*"
nestedStrongEmphStr = "+hi \"+miss+\"+"

dialogStartingWithSpaceStr    = "[ hi]"
dialogStartingWithSpaceReply  = Ast.Simple [ Ast.Raw [hiAtom] ]

clauseStartingWithSpaceStr    = "[hi| he said|]"
clauseStartingWithSpaceReply  = Ast.WithSay [  Ast.Raw [hiAtom] ]
                                            [  Ast.Raw [ Ast.Word "he"
                                                       , Ast.Word "said"
                                                       ]
                                            ]
                                            [ ]

secondParagraphIllFormed = hiStr ++ "\n\n" ++ illQuoteStr ++ "\n\n" ++ hiStr
secondParagraphIllFormedPartialCompilation =
    Ast.Story [ [ Ast.Teller [ Ast.Raw [ hiAtom ] ] ]
              , [ Ast.IllFormed illQuoteStr ]
              , [ Ast.Teller [ Ast.Raw [ hiAtom ] ] ]
              ]

storyStr = hiStr
storyAst = Ast.Story [ [ Ast.Teller [ Ast.Raw [ hiAtom ] ] ] ]

asideIllFormed = "_______letter______\n\n_______letter______\n\nTest.\n\n____________"
