module Ogmarkup.Parser (
  specialchar,
  word,
  text,
  raw,
  strongemph,
  weakemph,
  paragraph,
  format,
  dialog,
  teller,
  component,
  ) where

import Text.ParserCombinators.Parsec
import Ogmarkup.Ast

textmarker :: GenParser Char st ()
textmarker = oneOf "«»—?!;:.," >> return ()

formatmarker :: GenParser Char st ()
formatmarker = oneOf "*|"  >> return ()

opendialogmarker :: GenParser Char st ()
opendialogmarker = oneOf "[<" >> return ()

closedialogmarker :: GenParser Char st ()
closedialogmarker = oneOf ">]" >> return ()

dialogmarker :: GenParser Char st ()
dialogmarker = opendialogmarker <|> closedialogmarker

marker :: GenParser Char st ()
marker = (space >> return ()) <|> textmarker <|> formatmarker <|> dialogmarker <|> eof

ogmaLookAhead parser = (lookAhead $ try parser) >> return ()

manyTillEof parser till = manyTill parser (ogmaLookAhead till <|> eof)

many1TillEof parser till = do
  notFollowedBy till
  ps <- manyTill parser (ogmaLookAhead till <|> eof)
  
  return ps

paragraph :: GenParser Char st OgmaParagraph
paragraph = manyTill component eof >>= return . Story

component :: GenParser Char st OgmaComponent
component = spaces *> (try audible <|> try thought <|> teller) <* spaces

audible :: GenParser Char st OgmaComponent
audible = dialog '[' ']' >>= return . Audible

thought :: GenParser Char st OgmaComponent
thought = dialog '<' '>' >>= return . Thought

teller :: GenParser Char st OgmaComponent
teller =  many1TillEof format opendialogmarker >>= return . Teller

dialog :: Char -> Char -> GenParser Char st OgmaDialog
dialog c e = do
  char c
  d <- many1TillEof format (char e <|> char '|')
  try (do
          char '|'
          i <- many1TillEof format (char '|')
          char '|'
          spaces
          d' <- manyTillEof format (char e)
          char e
          char '('
          a <- many letter
          char ')'

          return $ WithSay d i d' a) <|> (do
                                           char e
                                           char '('
                                           a <- many letter
                                           char ')'
                                           return $ Simple d a)

format :: GenParser Char st OgmaFormat
format =  spaces *> (try strongemph <|> try weakemph <|> raw) <* spaces

weakemph :: GenParser Char st OgmaFormat
weakemph = do
  char '*'
  t <- many1TillEof text (char '*')
  char '*'

  return $ WeakEmph t

strongemph :: GenParser Char st OgmaFormat
strongemph = do
  string "**"
  t <- many1TillEof text (string "**")
  string "**"

  return $ StrongEmph t
  
raw :: GenParser Char st OgmaFormat
raw = (many1TillEof text (formatmarker <|> dialogmarker)) >>= return . Raw

text :: GenParser Char st OgmaText
text = spaces *> (specialchar <|> (notFollowedBy marker >> word)) <* spaces

word :: GenParser Char st OgmaText
word = do
  w <- many1TillEof anyChar marker
  return $ Word w

specialchar :: GenParser Char st OgmaText
specialchar = colon
  <|> semicolon
  <|> longDash
  <|> openQuote
  <|> closeQuote
  <|> exclamationMark
  <|> questionMark
  <|> comma
  <|> try suspension
  <|> point

parseSpecial :: String -> OgmaText -> GenParser Char st OgmaText
parseSpecial s r = string s >> return r

questionMark    = parseSpecial "?" QuestionMark
exclamationMark = parseSpecial "!" ExclamationMark
openQuote       = parseSpecial "«" OpenQuote
closeQuote      = parseSpecial "»" CloseQuote
longDash        = parseSpecial "—" LongDash
semicolon       = parseSpecial ";" Semicolon
colon           = parseSpecial ":" Colon
comma           = parseSpecial "," Comma
point           = parseSpecial "." Point
suspension      = string ".." >> many (char '.') >> return SuspensionPoints
