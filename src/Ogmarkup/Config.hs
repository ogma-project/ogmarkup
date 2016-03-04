{-# LANGUAGE OverloadedStrings #-}

module Ogmarkup.Config where

import Data.Text (Text)
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
newtype Typography = Typography { decide :: Ast.Atom -> (Space, Space, Text) }

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

-- | Normalize the input in order to add it to a generated Text.
normalizeAtom :: Typography
              -> Ast.Atom
              -> Text
normalizeAtom t o = case decide t o of (_, _, r) -> r

-- | The French typography. It can be used with several generation
-- approach, as it stay very generic.
frenchTypo :: Typography
frenchTypo = Typography t
  where
    t :: Ast.Atom -> (Space, Space, Text)
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

type Tag = Text -> Text

-- | A data type to carry out the generation configuration. In
--   particular, it works well to define a Typography and some
--   marker such as HTML tags.
data GenConf = GenConf { -- | The Typography to use for the generation
                         typography :: Typography,
                         documentTag :: Tag,
                         storyTag :: Tag,
                         asideTag :: Tag,
                         paragraphTag :: Tag,
                         tellerTag :: Tag,
                         dialogueTag :: Text -> Tag,
                         thoughtTag :: Text -> Tag,
                         replyTag :: Tag,
                         betweenDialogue :: Text,
                         emphTag :: Tag,
                         strongEmphTag :: Tag,
                         authorNormalize :: Text -> Text,
                         printSpace :: Space -> Text }
