{-|
Module      : Text.Ogmarkup.Private.Parser
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

This module provides several parsers that can be used in order to
extract the 'Ast' of an Ogmarkup document.

Please consider that only 'document' should be used outside this
module.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Ogmarkup.Private.Parser where

import           Control.Monad
import           Data.String
import Text.ParserCombinators.Parsec hiding (parse)

import qualified Text.Ogmarkup.Private.Ast     as Ast

-- | Keep track of the currently opened formats.
data ParserState = ParserState { -- | Already parsing text with emphasis
                                 parseWithEmph        :: Bool
                                 -- |_Already parsing text with strong
                                 --   emphasis
                               , parseWithStrongEmph  :: Bool
                                 -- |_Already parsing a quote
                               , parseWithinQuote     :: Bool
                               }

-- | Update the 'ParserState' to guard against nested emphasis.
enterEmph :: OgmarkupParser ()
enterEmph = do st <- getState
               if parseWithEmph st
                 then fail "guard against nested emphasis"
                 else do setState st { parseWithEmph = True }
                         return ()

-- | Update the 'ParserState' to be able to parse input with emphasis
-- again.
leaveEmph :: OgmarkupParser ()
leaveEmph = do st <- getState
               if parseWithEmph st
                 then do setState st { parseWithEmph = False }
                         return ()
                 else fail "cannot leave emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested strong emphasis.
enterStrongEmph :: OgmarkupParser ()
enterStrongEmph = do st <- getState
                     if parseWithStrongEmph st
                       then fail "guard against nested strong emphasis"
                       else do setState st { parseWithStrongEmph = True }
                               return ()

-- | Update the 'ParserState' to be able to parse input with strong emphasis
-- again.
leaveStrongEmph :: OgmarkupParser ()
leaveStrongEmph = do st <- getState
                     if parseWithStrongEmph st
                       then do setState st { parseWithStrongEmph = False }
                               return ()
                       else fail "cannot leave strong emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested quoted inputs.
enterQuote :: OgmarkupParser ()
enterQuote = do st <- getState
                if parseWithinQuote st
                  then fail "guard against nested quotes"
                  else do setState st { parseWithinQuote = True }
                          return ()

-- | Update the 'ParserState' to be able to parse input with an input
-- surrounded by quotes again.
leaveQuote :: OgmarkupParser ()
leaveQuote = do st <- getState
                if parseWithinQuote st
                  then do setState st { parseWithinQuote = False }
                          return ()
                  else fail "cannot leave quote when you did not enter"

-- | A initial ParserState instance to be used at the begining of
-- a document parsing.
initParserState :: ParserState
initParserState = ParserState False False False

-- | An ogmarkup parser processes 'Char' tokens and carries a 'ParserState'
type OgmarkupParser = GenParser Char ParserState

-- | A wrapper around the 'runParser' function of Parsec. It uses
-- 'initParserState' as an initial state.
parse :: OgmarkupParser a -> String -> String -> Either ParseError a
parse ogma = runParser ogma initParserState

-- | Try its best to parse an ogmarkup document. When it encounters an
--   error, it returns an Ast and the remaining input.
--
--   See 'Ast.Document'.
document :: IsString a
         => OgmarkupParser (Ast.Document a, String)
document = do spaces
              sects <- many (try section)
              input <- getInput

              return (sects, input)

-- | See 'Ast.Section'.
section :: IsString a
           => OgmarkupParser (Ast.Section a)
section = aside <|> story

-- | See 'Ast.Aside'.
aside :: IsString a
         => OgmarkupParser (Ast.Section a)
aside = do asideSeparator
           cls <- optionMaybe asideClass
           spaces
           ps <- many1 (paragraph <* spaces)
           asideSeparator
           manyTill space (skip (char '\n') <|> eof)
           spaces

           return $ Ast.Aside cls ps
  where
    asideClass :: IsString a
               => OgmarkupParser a
    asideClass = do a <- many1 letter
                    asideSeparator

                    return $ fromString a

-- | See 'Ast.Story'.
story :: IsString a
      => OgmarkupParser (Ast.Section a)
story = Ast.Story `fmap` many1 (paragraph <* spaces)

-- | See 'Ast.Paragraph'.
paragraph :: IsString a
          => OgmarkupParser (Ast.Paragraph a)
paragraph = many1 component <* blank

-- | See 'Ast.Component'.
component :: IsString a
          => OgmarkupParser (Ast.Component a)
component = try (dialogue <|> thought <|> teller) <|> illformed

-- | See 'Ast.IllFormed'.
illformed :: IsString a
          => OgmarkupParser (Ast.Component a)
illformed = Ast.IllFormed `fmap` restOfParagraph

-- | Parse the rest of the current paragraph with no regards for the
-- ogmarkup syntax. This Parser is used when the document is ill-formed, to
-- find a new point of synchronisation.
restOfParagraph :: IsString a
                => OgmarkupParser a
restOfParagraph = do lookAhead anyToken
                     notFollowedBy endOfParagraph
                     str <- manyTill anyToken (lookAhead $ try endOfParagraph)
                     return $ fromString str

-- | See 'Ast.Teller'.
teller :: IsString a
       => OgmarkupParser (Ast.Component a)
teller = Ast.Teller `fmap` many1 format

-- | See 'Ast.Dialogue'.
dialogue :: IsString a
         => OgmarkupParser (Ast.Component a)
dialogue = talk '[' ']' Ast.Dialogue

-- | See 'Ast.Thought'.
thought :: IsString a
        => OgmarkupParser (Ast.Component a)
thought = talk '<' '>' Ast.Thought

-- | @'talk' c c' constr@ wraps a reply surrounded by @c@ and @c'@ inside
--   @constr@ (either 'Ast.Dialogue' or 'Ast.Thought').
talk :: IsString a
     => Char -- ^ A character to mark the begining of a reply
     -> Char -- ^ A character to mark the end of a reply
     -> (Ast.Reply a -> Maybe a -> Ast.Component a) -- ^ Either 'Ast.Dialogue' or 'Ast.Thought' according to the situation.
     -> OgmarkupParser (Ast.Component a)
talk c c' constructor = do
  rep <- reply c c'
  auth <- optionMaybe characterName
  blank

  return $ constructor rep auth

-- | Parse the name of the character which speaks or thinks. According to
-- the ogmarkup syntax, it is surrounded by parentheses.
characterName :: IsString a
           => OgmarkupParser a
characterName = do
  char '('
  notFollowedBy (char ')') <?> "Empty character names are not allowed"
  auth <- manyTill anyToken (char ')') <?> "Missing closing )"

  return $ fromString auth

-- | 'reply' parses a 'Ast.Reply'.
reply :: IsString a
      => Char
      -> Char
      -> OgmarkupParser (Ast.Reply a)
reply c c' = do char c
                blank
                p1 <- many1 format
                x <- oneOf ['|', c']

                case x of '|' -> do blank
                                    ws <- many1 format
                                    char '|' <?> "Missing | to close the with say"
                                    blank
                                    p2 <- many format
                                    char c'

                                    return $ Ast.WithSay p1 ws p2
                          _ -> return $ Ast.Simple p1

-- | See 'Ast.Format'.
format :: IsString a
       => OgmarkupParser (Ast.Format a)
format = choice [ raw
                , emph
                , strongEmph
                , quote
                ]

-- | See 'Ast.Raw'.
raw :: IsString a
    => OgmarkupParser (Ast.Format a)
raw = Ast.Raw `fmap` many1 atom 

-- | See 'Ast.Emph'.
emph :: IsString a
     => OgmarkupParser (Ast.Format a)
emph = do char '*'
          blank
          enterEmph
          f <- format
          fs <- manyTill format (char '*' >> blank)
          leaveEmph
          return . Ast.Emph $ (f:fs)

-- | See 'Ast.StrongEmph'.
strongEmph :: IsString a
           => OgmarkupParser (Ast.Format a)
strongEmph = do char '+'
                blank
                enterStrongEmph
                f <- format
                fs <- manyTill format (char '+' >> blank)
                leaveStrongEmph
                return . Ast.StrongEmph $ (f:fs)

-- | See 'Ast.Quote'.
quote :: IsString a
      => OgmarkupParser (Ast.Format a)
quote = do char '"'
           blank
           enterQuote
           f <- format
           fs <- manyTill format (char '"' >> blank)
           leaveQuote
           return . Ast.Quote $ (f:fs)

-- | See 'Ast.Atom'.
atom :: IsString a
     => OgmarkupParser (Ast.Atom a)
atom = (mark <|> longword <|> word) <* blank

-- | See 'Ast.Word'. This parser does not consume the following spaces, so
--   the caller needs to take care of it.
word :: IsString a
     => OgmarkupParser (Ast.Atom a)
word = do lookAhead anyToken -- not the end of the parser
          notFollowedBy endOfWord

          str <- manyTill anyToken (lookAhead $ try endOfWord)

          return $ Ast.Word (fromString str)
  where
    specChar = "\"«»`+*[]<>|_\'’"

    endOfWord :: OgmarkupParser ()
    endOfWord =     eof <|> skip space <|> skip (oneOf specChar) <|> skip mark

-- | Wrap a raw string surrounded by @`@ inside a 'Ast.Word'.
--
--   >>> parse longword "" "`test *ei*`"
--   Right (Ast.Word "test *ei*")
--
--   Therefore, @`@ can be used to insert normally reserved symbol
--   inside a generated document.
longword :: IsString a
         => OgmarkupParser (Ast.Atom a)
longword = do char '`'
              notFollowedBy (char '`') <?> "empty raw string are not accepted"
              str <- manyTill anyToken (char '`')
              return $ Ast.Word (fromString str)

-- | See 'Ast.Punctuation'. Be aware that 'mark' does not parse the quotes
--   because they are processed 'quote'.
mark :: OgmarkupParser (Ast.Atom a)
mark = Ast.Punctuation `fmap` (semicolon
        <|> colon
        <|> question
        <|> exclamation
        <|> try longDash
        <|> try dash
        <|> hyphen
        <|> comma
        <|> apostrophe
        <|> try suspensionPoints
        <|> point)
  where
    parseMark p m = p >> return m

    semicolon        = parseMark (char ';') Ast.Semicolon
    colon            = parseMark (char ':') Ast.Colon
    question         = parseMark (char '?') Ast.Question
    exclamation      = parseMark (char '!') Ast.Exclamation
    longDash         = parseMark (string "—" <|> string "---") Ast.LongDash
    dash             = parseMark (string "–" <|> string "--") Ast.Dash
    hyphen           = parseMark (char '-') Ast.Hyphen
    comma            = parseMark (char ',') Ast.Comma
    point            = parseMark (char '.') Ast.Point
    apostrophe       = parseMark (char '\'' <|> char '’') Ast.Apostrophe
    suspensionPoints = parseMark (string ".." >> many (char '.')) Ast.SuspensionPoints

-- | See 'Ast.OpenQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
openQuote :: OgmarkupParser ()
openQuote = do char '«' <|> char '"'
               blank

-- | See 'Ast.CloseQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
closeQuote :: OgmarkupParser ()
closeQuote = do char '»' <|> char '"'
                blank

-- | An aside section (see 'Ast.Aside') is a particular region
--   surrounded by two lines of underscores (at least three).
--   This parser consumes one such line.
asideSeparator :: OgmarkupParser ()
asideSeparator = do string "__"
                    many1 (char '_')

                    return ()

-- | The end of a paragraph is the end of the document or two blank lines
-- or an aside separator, that is a line of underscores.
endOfParagraph :: OgmarkupParser ()
endOfParagraph = try betweenTwoSections
                 <|> asideSeparator
                 <|> eof
  where
    betweenTwoSections :: OgmarkupParser ()
    betweenTwoSections = do count 2 $ manyTill space (eof <|> skip (char '\n'))
                            spaces

-- | This parser consumes all the white spaces until it finds either an aside
--   surrounding marker (see 'Ast.Aside'), the end of the document or
--   one blank line. The latter marks the end of the current paragraph.
blank :: OgmarkupParser ()
blank = optional (notFollowedBy endOfParagraph >> spaces)

-- | @skip p@ parses @p@ and skip the result
skip :: OgmarkupParser a -> OgmarkupParser ()
skip = void
