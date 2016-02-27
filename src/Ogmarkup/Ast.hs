module Ogmarkup.Ast (
  OgmaText (
       Word,
       Semicolon,
       Colon,
       OpenQuote,
       CloseQuote,
       QuestionMark,
       ExclamationMark,
       LongDash,
       Comma,
       Point,
       SuspensionPoints
       ),
  OgmaFormat (
      Raw,
      WeakEmph,
      StrongEmph
      ),
  OgmaDialog (Simple,
              WithSay
             ),
  OgmaComponent (Teller,
                 Audible,
                 Thought
                ),
  OgmaParagraph (Story,
                 Quote
                )
  ) where

data OgmaText =
  Word String
  | Semicolon
  | Colon
  | OpenQuote
  | CloseQuote
  | QuestionMark
  | ExclamationMark
  | LongDash
  | Comma
  | Point
  | SuspensionPoints
    deriving (Show,Eq)

data OgmaFormat =
  Raw [OgmaText]
  | WeakEmph [OgmaText]
  | StrongEmph [OgmaText]
    deriving (Show,Eq)

data OgmaDialog =
  Simple [OgmaFormat] String
  | WithSay [OgmaFormat] [OgmaFormat] [OgmaFormat] String
    deriving (Show,Eq)

data OgmaComponent =
  Teller [OgmaFormat]
  | Audible OgmaDialog -- String
  | Thought OgmaDialog -- String
    deriving (Show,Eq)

data OgmaParagraph =
  Story [OgmaComponent]
  | Quote [[OgmaComponent]] OgmaFormat
    deriving (Show,Eq)
