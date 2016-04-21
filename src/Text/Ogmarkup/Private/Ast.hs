{-|
Module      : Text.Ogmarkup.Private.Ast
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

An abstract representation of an ogmarkup document.
-}

module Text.Ogmarkup.Private.Ast where

-- | A ogmarkup document internal representation waiting to be used in order
--   to generate an output.
type Document a = [Section a]

-- | A Section within an ogmarkup document is a sequence of paragraph. It
-- can be part of the story or an aside section like a letter, a song, etc.
-- We make the distinction between the two case because we want to be able
-- to apply different style regarding the situation.
data Section a =
    Story [Paragraph a]           -- ^ The story as it goes
  | Aside (Maybe a) [Paragraph a] -- ^ Something else. Maybe a letter, a flashback, etc.
  | Failing a
    deriving (Eq,Show)

-- | A Paragraph is just a sequence of Component.
type Paragraph a = [Component a]

-- | A Component is either a narrative text, a line dialogue of a character or
-- a character inner thought.
--
-- We also embed an error component in case we failed to parse a valid
-- component. This way, we can resume parsing when we find a new paragraph.
data Component a =
    Teller [Format a]            -- ^ A narrative description
  | Dialogue (Reply a) (Maybe a) -- ^ A dialogue reply
  | Thought (Reply a) (Maybe a)  -- ^ Inner dialogue of the character.
  | IllFormed a                  -- ^ If none of the above matched, then output
                                 --   what follows as-is, the parsing will
                                 --   be resumed at the next paragraph
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

-- | A nested formatted text
data Format a =
  Raw [Atom a]                -- ^ No particular emphasis is required on this sequence
  | Emph [Format a]           -- ^ Surrounded by @*@.
  | StrongEmph [Format a]     -- ^ Surrounded by @**@.
  | Quote [Format a]
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
  | OpenQuote        -- ^ The character @"@
  | CloseQuote       -- ^ The character @"@
  | Dash             -- ^ The character – or the sequence @--@
  | LongDash         -- ^ The character — or the sequence @---@
  | Comma            -- ^ The character @,@
  | Point            -- ^ The character @.@
  | Hyphen           -- ^ The character @-@
  | SuspensionPoints -- ^ Two or more @.@ or the character …
  | Apostrophe       -- ^ The characters @'@ or @’@
    deriving (Show, Eq)
