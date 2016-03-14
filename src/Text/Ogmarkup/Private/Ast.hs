module Text.Ogmarkup.Private.Ast where

-- | A ogmarkup document internal representation waiting to be used in order
--   to generate an output.
type Document a = [Section a]

data Section a =
    Story [Paragraph a] -- ^ The story as it goes
  | Aside [Paragraph a] -- ^ Something else. Maybe a letter, a flashback, etc.
    deriving (Eq,Show)

type Paragraph a = [Component a]

data Component a =
    Teller [Format a]    -- ^ A narrative description
  | Dialogue (Reply a) a -- ^ A dialogue reply
  | Thought (Reply a) a  -- ^ Inner dialogue of the character.
    deriving (Eq,Show)

-- | A character line of dialogue. A reply may contain a descriptive part, which
--   is not part of what the character actually says or thinks. We call the
--   latter a "with say" reply untill someone gives use a better name for it.
data Reply a =
  -- | A reply of the form: "Good morning."
  Simple [Format a]
  -- | A reply of the form: "Good morning," she says. "How are you?"
  | WithSay [Format a] [Format a] [Format a]
    deriving (Eq,Show)

-- | A formatted sequence of 'Collection's.
data Format a =
  Raw [Collection a]          -- ^ No particular emphasis is required on this sequence
  | Emph [Collection a]       -- ^ Surrounded by @*@.
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

-- | An Atom is the atomic component of a Ogmarkup document. It can be either a
--   punctuation mark or a word, that is a string.
--
--   Note that, by construction, 'OpenQuote' and 'CloseQuote' are not valid
--   'Mark' values here.  Indeed, they are implicit with the 'Quote'
--   constructor. This choice allows to enforce that an opened quote needs to
--   be closed.
data Atom a =
    Word a           -- ^ A wrapped string
  | Punctuation Mark -- ^ A punctuation mark
    deriving (Show,Eq)


-- | Mostly in order to deal with typographic spaces, main
--   punctuation marks are tokenized during the parsing of an
--   Ogmarkup document.
data Mark =
    Semicolon        -- ^ The character @;@
  | Colon            -- ^ The character @,@
  | Question         -- ^ The character @?@
  | Exclamation      -- ^ The character @!@
  | OpenQuote        -- ^ The characters @"@ or @«@
  | CloseQuote       -- ^ The characters @"@ @»@
  | Dash             -- ^ The character – or the sequence @--@
  | LongDash         -- ^ The character — or the sequence @---@
  | Comma            -- ^ The character @,@
  | Point            -- ^ The character @.@
  | Hyphen           -- ^ The character @-@
  | SuspensionPoints -- ^ Two or more @.@ or the character …
    deriving (Show, Eq)
