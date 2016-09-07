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

{-# LANGUAGE TypeFamilies #-}

module Text.Ogmarkup.Private.Parser where

import           Text.Megaparsec
import           Control.Monad.State
import           Data.String

import qualified Text.Ogmarkup.Private.Ast     as Ast

-- | Keep track of the currently opened formats.
data ParserState = ParserState { -- | Already parsing text with emphasis
                                 parseWithEmph        :: Bool
                                 -- | Already parsing text with strong
                                 --   emphasis
                               , parseWithStrongEmph  :: Bool
                                 -- | Already parsing a quote
                               , parseWithinQuote     :: Bool
                               }

-- | An ogmarkup parser processes 'Char' tokens and carries a 'ParserState'.
type OgmarkupParser a = StateT ParserState (Parsec Dec a)

-- | Update the 'ParserState' to guard against nested emphasis.
enterEmph :: Stream a
          => OgmarkupParser a ()
enterEmph = do st <- get
               if parseWithEmph st
                 then fail "guard against nested emphasis"
                 else do put st { parseWithEmph = True }
                         return ()

-- | Update the 'ParserState' to be able to parse input with emphasis
-- again.
leaveEmph :: Stream a
          => OgmarkupParser a ()
leaveEmph = do st <- get
               if parseWithEmph st
                 then do put st { parseWithEmph = False }
                         return ()
                 else fail "cannot leave emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested strong emphasis.
enterStrongEmph :: Stream a
                => OgmarkupParser a ()
enterStrongEmph = do st <- get
                     if parseWithStrongEmph st
                       then fail "guard against nested strong emphasis"
                       else do put st { parseWithStrongEmph = True }
                               return ()

-- | Update the 'ParserState' to be able to parse input with strong emphasis
-- again.
leaveStrongEmph :: Stream a
                => OgmarkupParser a ()
leaveStrongEmph = do st <- get
                     if parseWithStrongEmph st
                       then do put st { parseWithStrongEmph = False }
                               return ()
                       else fail "cannot leave strong emphasis when you did not enter"

-- | Update the 'ParserState' to guard against nested quoted inputs.
enterQuote :: Stream a
           => OgmarkupParser a ()
enterQuote = do st <- get
                if parseWithinQuote st
                  then fail "guard against nested quotes"
                  else do put st { parseWithinQuote = True }
                          return ()

-- | Update the 'ParserState' to be able to parse an input
-- surrounded by quotes again.
leaveQuote :: Stream a
           => OgmarkupParser a ()
leaveQuote = do st <- get
                if parseWithinQuote st
                  then do put st { parseWithinQuote = False }
                          return ()
                  else fail "cannot leave quote when you did not enter"

-- | A initial ParserState instance to be used at the begining of
-- a document parsing.
initParserState :: ParserState
initParserState = ParserState False False False

-- | A wrapper around the 'runParser' function of Megaparsec. It uses
-- 'initParserState' as an initial state.
parse :: (Stream a, Token a ~ Char)
      => OgmarkupParser a b
      -> String
      -> a
      -> Either (ParseError (Token a) Dec) b
parse ogma file = runParser (evalStateT ogma initParserState) file

-- | Try its best to parse an ogmarkup document. When it encounters an
--   error, it returns an Ast and the remaining input.
--
--   See 'Ast.Document'.
document :: (Stream a, Token a ~ Char, IsString b)
         => OgmarkupParser a (Ast.Document b, a)
document = do space
              sects <- many (try section)
              input <- getInput

              return (sects, input)

-- | See 'Ast.Section'.
section :: (Stream a, Token a ~ Char, IsString b)
           => OgmarkupParser a (Ast.Section b)
section = aside <|> story

-- | See 'Ast.Aside'.
aside :: (Stream a, Token a ~ Char, IsString b)
         => OgmarkupParser a (Ast.Section b)
aside = do asideSeparator
           cls <- optional asideClass
           space
           ps <- some (paragraph <* space)
           asideSeparator
           manyTill space (skip (char '\n') <|> eof)
           space

           return $ Ast.Aside cls ps
  where
    asideClass :: (Stream a, Token a ~ Char, IsString b)
               => OgmarkupParser a b
    asideClass = do cls <- some letterChar
                    asideSeparator

                    return $ fromString cls

-- | See 'Ast.Story'.
story :: (Stream a, Token a ~ Char, IsString b)
      => OgmarkupParser a (Ast.Section b)
story = Ast.Story `fmap` some (paragraph <* space)

-- | See 'Ast.Paragraph'.
paragraph :: (Stream a, Token a ~ Char, IsString b)
          => OgmarkupParser a (Ast.Paragraph b)
paragraph = some component <* blank

-- | See 'Ast.Component'.
component :: (Stream a, Token a ~ Char, IsString b)
          => OgmarkupParser a (Ast.Component b)
component = try (dialogue <|> thought <|> teller) <|> illformed

-- | See 'Ast.IllFormed'.
illformed :: (Stream a, Token a ~ Char, IsString b)
          => OgmarkupParser a (Ast.Component b)
illformed = Ast.IllFormed `fmap` restOfParagraph

-- | Parse the rest of the current paragraph with no regards for the
-- ogmarkup syntax. This Parser is used when the document is ill-formed, to
-- find a new point of synchronization.
restOfParagraph :: (Stream a, Token a ~ Char, IsString b)
                => OgmarkupParser a b
restOfParagraph = do lookAhead anyChar
                     notFollowedBy endOfParagraph
                     str <- manyTill anyChar (lookAhead $ try endOfParagraph)
                     return $ fromString str

-- | See 'Ast.Teller'.
teller :: (Stream a, Token a ~ Char, IsString b)
       => OgmarkupParser a (Ast.Component b)
teller = Ast.Teller `fmap` some format

-- | See 'Ast.Dialogue'.
dialogue :: (Stream a, Token a ~ Char, IsString b)
         => OgmarkupParser a (Ast.Component b)
dialogue = talk '[' ']' Ast.Dialogue

-- | See 'Ast.Thought'.
thought :: (Stream a, Token a ~ Char, IsString b)
        => OgmarkupParser a (Ast.Component b)
thought = talk '<' '>' Ast.Thought

-- | @'talk' c c' constr@ wraps a reply surrounded by @c@ and @c'@ inside
--   @constr@ (either 'Ast.Dialogue' or 'Ast.Thought').
talk :: (Stream a, Token a ~ Char, IsString b)
     => Char -- ^ A character to mark the begining of a reply
     -> Char -- ^ A character to mark the end of a reply
     -> (Ast.Reply b -> Maybe b -> Ast.Component b) -- ^ Either 'Ast.Dialogue' or 'Ast.Thought' according to the situation
     -> OgmarkupParser a (Ast.Component b)
talk c c' constructor = do
  rep <- reply c c'
  auth <- optional characterName
  blank

  return $ constructor rep auth

-- | Parse the name of the character which speaks or thinks. According to
-- the ogmarkup syntax, it is surrounded by parentheses.
characterName :: (Stream a, Token a ~ Char, IsString b)
           => OgmarkupParser a b
characterName = do
  char '('
  notFollowedBy (char ')') <?> "Empty character names are not allowed"
  auth <- manyTill anyChar (char ')') <?> "Missing closing )"

  return $ fromString auth

-- | 'reply' parses a 'Ast.Reply'.
reply :: (Stream a, Token a ~ Char, IsString b)
      => Char
      -> Char
      -> OgmarkupParser a (Ast.Reply b)
reply c c' = do char c
                blank
                p1 <- some format
                x <- oneOf ['|', c']

                case x of '|' -> do blank
                                    ws <- some format
                                    char '|' <?> "Missing | to close the with say"
                                    blank
                                    p2 <- many format
                                    char c'

                                    return $ Ast.WithSay p1 ws p2
                          _ -> return $ Ast.Simple p1

-- | See 'Ast.Format'.
format :: (Stream a, Token a ~ Char, IsString b)
       => OgmarkupParser a (Ast.Format b)
format = choice [ raw
                , emph
                , strongEmph
                , quote
                ]

-- | See 'Ast.Raw'.
raw :: (Stream a, Token a ~ Char, IsString b)
    => OgmarkupParser a (Ast.Format b)
raw = Ast.Raw `fmap` some atom

-- | See 'Ast.Emph'.
emph :: (Stream a, Token a ~ Char, IsString b)
     => OgmarkupParser a (Ast.Format b)
emph = do char '*'
          blank
          enterEmph
          f <- format
          fs <- manyTill format (char '*' >> blank)
          leaveEmph
          return . Ast.Emph $ (f:fs)

-- | See 'Ast.StrongEmph'.
strongEmph :: (Stream a, Token a ~ Char, IsString b)
           => OgmarkupParser a (Ast.Format b)
strongEmph = do char '+'
                blank
                enterStrongEmph
                f <- format
                fs <- manyTill format (char '+' >> blank)
                leaveStrongEmph
                return . Ast.StrongEmph $ (f:fs)

-- | See 'Ast.Quote'.
quote :: (Stream a, Token a ~ Char, IsString b)
      => OgmarkupParser a (Ast.Format b)
quote = do openQuote
           enterQuote
           f <- format
           fs <- manyTill format closeQuote
           leaveQuote
           return . Ast.Quote $ (f:fs)

-- | See 'Ast.Atom'.
atom :: (Stream a, Token a ~ Char, IsString b)
     => OgmarkupParser a (Ast.Atom b)
atom = (mark <|> longword <|> word) <* blank

-- | See 'Ast.Word'. This parser does not consume the following spaces, so
--   the caller needs to take care of it.
word :: (Stream a, Token a ~ Char, IsString b)
     => OgmarkupParser a (Ast.Atom b)
word = do notFollowedBy endOfWord
          str <- manyTill anyChar (lookAhead $ try endOfWord)
          return $ Ast.Word (fromString str)
  where
    endOfWord :: (Stream a, Token a ~ Char)
              => OgmarkupParser a ()
    endOfWord = eof <|> (skip spaceChar) <|> (skip $ oneOf specChar) <|> (skip mark)
    specChar = "\"«»`+*[]<>|_\'’"


-- | Wrap a raw string surrounded by @`@ inside a 'Ast.Word'.
--
--   >>> parse longword "" "`test *ei*`"
--   Right (Ast.Word "test *ei*")
--
--   Therefore, @`@ can be used to insert normally reserved symbol
--   inside a generated document.
longword :: (Stream a, Token a ~ Char, IsString b)
         => OgmarkupParser a (Ast.Atom b)
longword = do char '`'
              notFollowedBy (char '`') <?> "empty raw string are not accepted"
              str <- manyTill anyChar (char '`')
              return $ Ast.Word (fromString str)

-- | See 'Ast.Punctuation'. Be aware that 'mark' does not parse the quotes
--   because they are processed 'quote'.
mark :: (Stream a, Token a ~ Char)
     => OgmarkupParser a (Ast.Atom b)
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
openQuote :: (Stream a, Token a ~ Char)
          => OgmarkupParser a ()
openQuote = do char '«' <|> char '"'
               blank

-- | See 'Ast.CloseQuote'. This parser consumes the following blank (see 'blank')
--   and skip the result.
closeQuote :: (Stream a, Token a ~ Char)
           => OgmarkupParser a ()
closeQuote = do char '»' <|> char '"'
                blank

-- | An aside section (see 'Ast.Aside') is a particular region
--   surrounded by two lines of underscores (at least three).
--   This parser consumes one such line.
asideSeparator :: (Stream a, Token a ~ Char)
               => OgmarkupParser a ()
asideSeparator = do string "__"
                    some (char '_')
                    return ()

-- | The end of a paragraph is the end of the document or two blank lines
-- or an aside separator, that is a line of underscores.
endOfParagraph :: (Stream a, Token a ~ Char)
               => OgmarkupParser a ()
endOfParagraph = try betweenTwoSections
                 <|> asideSeparator
                 <|> eof
  where
    betweenTwoSections :: (Stream a, Token a ~ Char)
                       => OgmarkupParser a ()
    betweenTwoSections = do count 2 $ manyTill spaceChar (eof <|> skip (char '\n'))
                            space


-- | This parser consumes all the white spaces until it finds either an aside
--   surrounding marker (see 'Ast.Aside'), the end of the document or
--   one blank line. The latter marks the end of the current paragraph.
blank :: (Stream a, Token a ~ Char)
      => OgmarkupParser a ()
blank = do skip $ optional (notFollowedBy endOfParagraph >> space)


-- | @skip p@ parses @p@ and skips the result.
skip :: (Stream a)
     => OgmarkupParser a b
     -> OgmarkupParser a ()
skip =  (>> return ())
