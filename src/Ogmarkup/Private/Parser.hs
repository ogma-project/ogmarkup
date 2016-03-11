{-# LANGUAGE OverloadedStrings #-}

-- | This module provides several parsers that can be used in order to
--   extract the 'Ast' of an Ogmarkup document.
--
--   Please consider that only 'document' should be used outside this
--   module.
module Ogmarkup.Private.Parser where

import           Control.Monad
import           Data.String
import           Text.ParserCombinators.Parsec

import qualified Ogmarkup.Private.Ast          as Ast

-- | See 'Ast.Document'.
document :: IsString a
         => GenParser Char st (Ast.Document a)
document = do spaces
              sects <- many1 section
              eof

              return sects

-- | See 'Ast.Section'.
section :: IsString a
           => GenParser Char st (Ast.Section a)
section = aside <|> story

-- | See 'Ast.Aside'.
aside :: IsString a
         => GenParser Char st (Ast.Section a)
aside = do asideSeparator
           spaces
           ps <- many1 (paragraph <* spaces)
           asideSeparator
           manyTill space (skip (char '\n') <|> eof)
           spaces

           return $ Ast.Aside ps

-- | See 'Ast.Story'.
story :: IsString a
      => GenParser Char st (Ast.Section a)
story = Ast.Story `fmap` many1 (paragraph <* spaces)

-- | See 'Ast.Paragraph'.
paragraph :: IsString a
          => GenParser Char st (Ast.Paragraph a)
paragraph = many1 component <* blank

-- | See 'Ast.Component'.
component :: IsString a
          => GenParser Char st (Ast.Component a)
component = dialogue <|> thought <|> teller

-- | See 'Ast.Teller'.
teller :: IsString a
       => GenParser Char st (Ast.Component a)
teller = Ast.Teller `fmap` many1 format

-- | See 'Ast.Dialogue'.
dialogue :: IsString a
         => GenParser Char st (Ast.Component a)
dialogue = talk '[' ']' Ast.Dialogue

-- | See 'Ast.Thought'.
thought :: IsString a
        => GenParser Char st (Ast.Component a)
thought = talk '<' '>' Ast.Thought

-- | @'talk' c c' constr@ wrap a reply surrounded by @c@ and @c'@ inside
--   @constr@ (either 'Ast.Dialogue' or 'Ast.Thought').
talk :: IsString a
     => Char -- ^ A character to mark the begining of a reply
     -> Char -- ^ A character to mark the end of a reply
     -> (Ast.Reply a -> a -> Ast.Component a) -- ^ Either 'Ast.Dialogue' or 'Ast.Thought' according to the situation.
     -> GenParser Char st (Ast.Component a)
talk c c' constructor = do
  rep <- reply c c'
  char '(' <?> "Missing character name"
  notFollowedBy (char ')') <?> "Empty character names are not allowed"
  author <- manyTill anyToken (char ')') <?> "Missing closing )"
  blank

  return $ constructor rep (fromString author)

-- | 'reply' parses a 'Ast.Reply'.
reply :: IsString a
      => Char
      -> Char
      -> GenParser Char st (Ast.Reply a)
reply c c' = do char c
                p1 <- many1 format
                x <- oneOf ['|', c']

                case x of '|' -> do ws <- many1 format
                                    char '|' <?> "Missing | to close the with say"
                                    blank
                                    p2 <- many format
                                    char c'

                                    return $ Ast.WithSay p1 ws p2
                          _ -> return $ Ast.Simple p1

-- | See 'Ast.Format'.
format :: IsString a
       => GenParser Char st (Ast.Format a)
format = try strongEmph <|> emph <|> raw

-- | See 'Ast.Raw'.
raw :: IsString a
    => GenParser Char st (Ast.Format a)
raw = Ast.Raw `fmap` many1 collection

-- | See 'Ast.Emph'.
emph :: IsString a
     => GenParser Char st (Ast.Format a)
emph = do char '*'
          blank
          col <- many1 collection
          char '*' <?> "Missing * to close emphasis"
          blank

          return $ Ast.Emph col

-- | See 'Ast.StrongEmph'.
strongEmph :: IsString a
           => GenParser Char st (Ast.Format a)
strongEmph = do char '+'
                blank
                col <- many1 collection
                char '+' <?> "Missing * to close emphasis"
                blank

                return $ Ast.StrongEmph col

-- | See 'Ast.Collection'.
collection :: IsString a
           => GenParser Char st (Ast.Collection a)
collection = quote <|> text

-- | See 'Ast.Quote'.
quote :: IsString a
      => GenParser Char st (Ast.Collection a)
quote = do openQuote
           atoms <- many1 atom
           closeQuote <?> "A previously opened quote needs to be cloded"

           return $ Ast.Quote atoms

-- | See 'Ast.Text'.
text :: IsString a
     => GenParser Char st (Ast.Collection a)
text = Ast.Text `fmap` many1 atom

-- | See 'Ast.Atom'.
atom :: IsString a
     => GenParser Char st (Ast.Atom a)
atom = (mark <|> longword <|> word) <* blank

-- | See 'Ast.Word'. This parser does not consume the following spaces, so
--   the caller needs to take care of it.
word :: IsString a
     => GenParser Char st (Ast.Atom a)
word = do lookAhead anyToken -- not the end of the parser
          notFollowedBy endOfWord

          str <- manyTill anyToken (lookAhead $ try endOfWord)

          return $ Ast.Word (fromString str)
  where
    specChar = "\"«»`+*[]<>|_"

    endOfWord :: GenParser Char st ()
    endOfWord =     eof <|> skip space <|> skip (oneOf specChar) <|> skip mark

-- | Wrap a raw string surrounded by @`@ inside a 'Ast.Word'.
--
--   >>> parse longword "" "`test *ei*`"
--   Right (Ast.Word "test *ei*")
--
--   Therefore, @`@ can be used to insert normally reserved symbol
--   inside a generated document.
longword :: IsString a
         => GenParser Char st (Ast.Atom a)
longword = do char '`'
              notFollowedBy (char '`') <?> "empty raw string are not accepted"
              str <- manyTill anyToken (char '`')
              return $ Ast.Word (fromString str)

-- | See 'Ast.Punctuation'. Be aware that 'mark' does not parse the quotes
--   because they are processed 'quote'.
mark :: GenParser Char st (Ast.Atom a)
mark = Ast.Punctuation `fmap` (semicolon
        <|> colon
        <|> question
        <|> exclamation
        <|> try longDash
        <|> try dash
        <|> hyphen
        <|> comma
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
    suspensionPoints = parseMark (string ".." >> many (char '.')) Ast.SuspensionPoints

-- | See 'Ast.OpenQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
openQuote :: GenParser Char st ()
openQuote = do char '«' <|> char '"'
               blank

-- | See 'Ast.CloseQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
closeQuote :: GenParser Char st ()
closeQuote = do char '»' <|> char '"'
                blank

-- | An aside section (see 'Ast.Aside') is a particular region
--   surrounded by two lines of underscores (at least three).
--   This parser consumes one such line.
asideSeparator :: GenParser Char st ()
asideSeparator = do string "__"
                    many1 (char '_')

                    return ()


-- | This parser consumes all the white spaces until it finds either an aside
--   surrounding marker (see 'Ast.Aside'), the end of the document or
--   one blank line. The latter marks the end of the current paragraph.
blank :: GenParser Char st ()
blank = optional (notFollowedBy endOfParagraph >> spaces)
  where
    betweenTwoSections :: GenParser Char st ()
    betweenTwoSections = do count 2 $ manyTill space (eof <|> skip (char '\n'))
                            spaces
    endOfParagraph :: GenParser Char st ()
    endOfParagraph = try betweenTwoSections
                        <|> asideSeparator -- maybe we need to add (spaces *> ... <* spaces)
                        <|> eof

-- | @skip p@ parses @p@ and skip the result
skip :: GenParser Char st a -> GenParser Char st ()
skip = void
