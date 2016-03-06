module Ogmarkup.Private.Ast where

type Document a = [Section a]

data Section a =
    Story [Paragraph a]
  | Aside [Paragraph a]
    deriving (Eq,Show)

type Paragraph a = [Component a]

data Component a =
    Teller [Format a]
  | Dialogue (Reply a) a
  | Thought (Reply a) a
    deriving (Eq,Show)

data Reply a =
  -- | "Good morning."
  Simple [Format a]
  -- | "Good morning," she says. "How are you?"
  | WithSay [Format a] [Format a] [Format a]
    deriving (Eq,Show)

-- | A formatted sequence of 'Collection's.
data Format a =
  Raw [Collection a] -- ^ No particular emphasis is required on this sequence
  | Emph [Collection a] -- ^ Surrounded by @*@.
  | StrongEmph [Collection a] -- ^ Surrounded by @**@.
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
data Collection a =
    Text [Atom a] -- ^ A regular text composed of several 'Atom'.
  | Quote [Atom a] -- ^ A sequence of 'Atom' surrounded by 'OpenQuote' and 'CloseQuote'.
    deriving (Show,Eq)

-- | An Atom is the atomic component of a Ogmarkup document. It can
--   be either a punctuation mark or a word, that is a string.
data Atom a =
    Word a -- ^ A wrapped string
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
