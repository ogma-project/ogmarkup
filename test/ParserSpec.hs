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
spec = describe "atom" $ do
    it "should not parse an empty string" $ shouldFail (Parser.parse Parser.atom "" "")

    it "should parse one word" $ Parser.parse Parser.atom "" hiStr `shouldParse` hiAtom

    it "should parse one punctuation mark" $
      Parser.parse Parser.atom "" exclamationStr `shouldParse` exclamationAtom

    it "should parse one quote" $
      Parser.parse Parser.format "" quoteStr `shouldParse` quoteFormat

    it "should fail if the quote is ill-formed (no closing quote)" $
      shouldFail (Parser.parse Parser.format "" illQuoteStr)

    it "should parse nested formats" $
      Parser.parse Parser.format "" nestedFormatsStr `shouldParse` nestedFormatsFormat

    it "should fail with nested same format" $ do
      shouldFail (Parser.parse Parser.format "" nestedEmphStr)
      shouldFail (Parser.parse Parser.format "" nestedStrongEmphStr)

hiStr = "hi"
hiAtom = Ast.Word "hi"

exclamationStr = "!"
exclamationAtom = Ast.Punctuation Ast.Exclamation

quoteStr = "\"hi everyone.\""
quoteFormat = Ast.Quote [Ast.Raw [hiAtom, Ast.Word "everyone", Ast.Punctuation Ast.Point]]

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
