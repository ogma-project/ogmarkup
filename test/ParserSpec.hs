{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ParserSpec where

import           Data.Either
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

import qualified Text.Ogmarkup.Private.Ast     as Ast
import qualified Text.Ogmarkup.Private.Parser  as Parser

parse' :: (Show b, Eq b)
      => Parser.OgmarkupParser String b
      -> String
      -> String
      -> Either (ParseError Char Dec) b
parse' = Parser.parse

spec :: Spec
spec = do
    describe "atom" $ do
      it "should not parse an empty string" $ parse' Parser.atom "" `shouldFailOn` ""

      it "should parse one word" $ parse' Parser.atom "" hiStr `shouldParse` hiAtom

      it "should parse one punctuation mark" $
        parse' Parser.atom "" exclamationStr `shouldParse` exclamationAtom

    describe "format" $ do
      it "should parse one quote" $
        parse' Parser.format "" quoteStr `shouldParse` quoteFormat

      it "should support french quotes" $ do
        parse' Parser.format "" frenchQuoteStr `shouldParse` frenchQuoteFormat

      it "should fail if the quote is ill-formed (no closing quote)" $
        parse' Parser.format "" `shouldFailOn` illQuoteStr

      it "should parse nested formats" $
        parse' Parser.format "" nestedFormatsStr `shouldParse` nestedFormatsFormat

      it "should fail with nested same format" $ do
        parse' Parser.format "" `shouldFailOn` nestedEmphStr
        parse' Parser.format "" `shouldFailOn` nestedStrongEmphStr

    describe "reply" $ do
      it "should accept spaces at the beginning of dialogs" $ do
        parse' (Parser.reply '[' ']') "" dialogStartingWithSpaceStr `shouldParse` dialogStartingWithSpaceReply
        parse' (Parser.reply '[' ']') "" clauseStartingWithSpaceStr `shouldParse` clauseStartingWithSpaceReply

    describe "section" $ do
      it "should parse aside with class" $ do
        parse' Parser.section "" asideWithClassStr `shouldParse` asideWithClassAst

      it "should parse aside without class" $ do
        parse' Parser.section "" asideWithoutClassStr `shouldParse` asideWithoutClassAst

    describe "ill-formed paragraphs" $ do
      it "ill-formed components should be accepted as-is" $ do
        parse' Parser.component "" illQuoteStr  `shouldParse` Ast.IllFormed illQuoteStr
        parse' Parser.component "" nestedEmphStr  `shouldParse` Ast.IllFormed nestedEmphStr
        parse' Parser.component "" nestedStrongEmphStr  `shouldParse` Ast.IllFormed nestedStrongEmphStr
      it "an ill-formed paragraph should not prevent parsing correctly the others" $ do
        parse' Parser.story "" secondParagraphIllFormed `shouldParse` secondParagraphIllFormedPartialCompilation

    describe "document" $ do
      it "should try its best to compile an ill-formed document" $ do
        parse' Parser.document "" (storyStr ++ "\n\n" ++ asideIllFormed) `shouldParse`
           [ storyAst
           , Ast.Failing "_______letter______"
           , Ast.Aside (Just "letter") [ [ Ast.Teller [ Ast.Raw [ Ast.Word "Test"
                                                                , Ast.Punctuation Ast.Point
                                                                ]
                                                      ]
                                         ]
                                       ]
           ]

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

asideWithClassStr = "____class_____\n\n" ++ hiStr ++ "\n_______"
asideWithClassAst = Ast.Aside (Just "class") [ [ Ast.Teller [ Ast.Raw [ hiAtom ] ] ] ]

asideWithoutClassStr = "_________\n\n" ++ hiStr ++ "\n_______"
asideWithoutClassAst = Ast.Aside Nothing [ [ Ast.Teller [ Ast.Raw [ hiAtom ] ] ] ]

asideIllFormed = "_______letter______\n\n_______letter______\n\nTest.\n\n____________"
