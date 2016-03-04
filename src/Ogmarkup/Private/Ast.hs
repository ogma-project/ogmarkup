module Ogmarkup.Private.Ast where

import Data.Text (Text)

type Document = [Section]

data Section =
    Story [Paragraph]
  | Aside [Paragraph]
    deriving (Eq,Show)

type Paragraph = [Component]

data Component =
    Teller [Format]
  | Dialogue Reply Text
  | Thought Reply Text
    deriving (Eq,Show)

data Reply =
  -- | "Good morning."
  Simple [Format]
  -- | "Good morning," she says. "How are you?"
  | WithSay [Format] [Format] [Format]
    deriving (Eq,Show)

-- | A formatted sequence of 'Collection's.
data Format =
  Raw [Collection] -- ^ No particular emphasis is required on this sequence
  | Emph [Collection] -- ^ Surrounded by @*@.
  | StrongEmph [Collection] -- ^ Surrounded by @**@.
    deriving (Show,Eq)


-- | A sequence of 'Atom'.
--  
--   @'Text' ['Word' "hi", 'Word' "miss", 'Punctuation' 'Exclamation']@
--
--   represents the string
--
--   @Hi miss!@
--
--   whereas
--  
--   @'Quote' ['Word' "hi", 'Word' "miss", 'Punctuation' 'Exclamation']@
--
--   represents the string
--
--   @"Hi miss!"@
data Collection =
    Text [Atom] -- ^ A regular text composed of several 'Atom'.
  | Quote [Atom] -- ^ A sequence of 'Atom' surrounded by 'OpenQuote' and 'CloseQuote'.
    deriving (Show,Eq)

-- | An Atom is the atomic component of a Ogmarkup document. It can
--   be either a punctuation mark or a word, that is a string.
data Atom =
    Word Text -- ^ A wrapped string
  | Punctuation Mark -- ^ Note that, by construction, 'OpenQuote' and
                     --   'CloseQuote' are not valid 'Mark' values
                     --   here.  Indeed, they are implicit with the
                     --   'Quote' constructor. This choice allows to
                     --   enforce that an opened quote needs to be
                     --   closed.
    deriving (Show,Eq)


-- | Mostly in order to deal with typographic spaces, main
--   punctuation marks are tokenized during the parsing of an
--   Ogmarkup document.
data Mark =
    Semicolon -- ^ The character @;@
  | Colon -- ^ The character @,@
  | Question -- ^ The character @?@
  | Exclamation -- ^ The character @!@
  | OpenQuote -- ^ The characters @"@ or @«@
  | CloseQuote -- ^ The characters @"@ @»@
  | Dash -- ^ The character – or the sequence @--@
  | LongDash -- ^ The character — or the sequence @---@
  | Comma -- ^ The character @,@
  | Point -- ^ The character @.@
  | Hyphen -- ^ The character @-@
  | SuspensionPoints -- ^ Two or more @.@ or the character …
    deriving (Show, Eq)
