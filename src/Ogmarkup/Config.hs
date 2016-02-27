module Ogmarkup.Config where

import qualified Ogmarkup.Private.Ast as Ast

-- | Deal with typographic spaces, especially when it comes to
--   separate two texts. Because Space derives Ord, it is possible
--   to use min and max to determine which one to use in case of
--   a conflict.
data Space =
  Normal -- ^ A normal space that can be turned into a newline for displaying.
  | Nbsp -- ^ A non breakable space, it cannot be turned into a newline.
  | None -- ^ No space at all.
    deriving (Eq,Ord)

-- | A Typography is a data type that tells the caller what space
--   she should privileged before and after a text.
newtype Typography = Typography { decide :: Ast.Atom -> (Space, Space, String) }

-- | From a Typography, it gives the space to privilege before the
--   input Text.
beforeAtom :: Typography
           -> Ast.Atom
           -> Space
beforeAtom t o = case decide t o of (r, _, _) -> r

-- | From a Typography, it gives the space to privilege after the
--   input Text.
afterAtom :: Typography
          -> Ast.Atom
          -> Space
afterAtom t o = case decide t o of (_, r, _) -> r

-- | Normalize the input in order to add it to a generated String.
normalizeAtom :: Typography
              -> Ast.Atom
              -> String
normalizeAtom t o = case decide t o of (_, _, r) -> r

-- | The French typography. It can be used with several generation
-- approach, as it stay very generic.
frenchTypo :: Typography
frenchTypo = Typography t
  where
    t :: Ast.Atom -> (Space, Space, String)
    t (Ast.Word w) = (Normal, Normal, w)
    t (Ast.Punctuation Ast.Semicolon) = (Nbsp, Normal, ";")
    t (Ast.Punctuation Ast.Colon) = (Nbsp, Normal, ":")
    t (Ast.Punctuation Ast.OpenQuote) = (Normal, Nbsp, "«")
    t (Ast.Punctuation Ast.CloseQuote) = (Nbsp, Normal, "»")
    t (Ast.Punctuation Ast.Question) = (Nbsp, Normal, "?")
    t (Ast.Punctuation Ast.Exclamation) = (Nbsp, Normal, "!")
    t (Ast.Punctuation Ast.LongDash) = (Normal, Normal, "—")
    t (Ast.Punctuation Ast.Dash) = (None, None, "–")
    t (Ast.Punctuation Ast.Hyphen) = (None, None, "-")
    t (Ast.Punctuation Ast.Comma) = (None, Normal, ",")
    t (Ast.Punctuation Ast.Point) = (None, Normal, ".")
    t (Ast.Punctuation Ast.SuspensionPoints) = (None, Normal, "…")

-- | A data type to carry out the generation configuration. In
--   particular, it works well to define a Typography and some
--   marker such as HTML tags.
data GenConf = GenConf { -- | The Typography to use for the generation
                         typography :: Typography,
                         -- | A tag to open a paragraph.
                         beginParagraph :: String,
                         -- | A tag to close a paragraph.
                         endParagraph :: String,
                         -- | A tag to open an audible dialog part.
                         beginDialogue :: (String -> String),
                         -- | A tag to close an audible dialog part.
                         endDialogue :: (String -> String),
                         -- | A tag to open a thought.
                         beginThought :: (String -> String),
                         -- | A tag to end a thought.
                         endThought :: (String -> String),
                         -- | A function to transform the author name
                         --   into something else: a CSS class, for
                         --   instance, or an HTML color code.
                         authorNormalize :: String -> String,
                         -- | A template function that takes the
                         --   normalized author as a parameter. The
                         --   result is used as a opening tag
                         --   for dialog.
                         beginReply :: String,
                         -- | A template function to render the
                         --   closing tag for dialog.
                         endReply :: String,
                         -- | Opening tag for weak emphasis.
                         beginWeakEmph :: String,
                         -- | Closing tag for weak emphasis.
                         endWeakEmph :: String,
                         -- | Opening tag for strong emphasis.
                         beginStrongEmph :: String,
                         -- | Closing tag for strong emphasis.
                         endStrongEmph :: String,
                         -- | A function to render typographic spaces.
                         printSpace :: Space -> String }
