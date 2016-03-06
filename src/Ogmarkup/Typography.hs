{-# LANGUAGE OverloadedStrings #-}

module Ogmarkup.Typography where

import Data.String
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
data Typography a = Typography {
  decide :: Ast.Atom a -> (Space, Space, a),
  openDialogue :: Bool -> Maybe Ast.Mark,
  closeDialogue :: Bool -> Maybe Ast.Mark
  }

-- | From a Typography, it gives the space to privilege before the
--   input Text.
beforeAtom :: Typography a
           -> Ast.Atom a
           -> Space
beforeAtom t o = case decide t o of (r, _, _) -> r

-- | From a Typography, it gives the space to privilege after the
--   input Text.
afterAtom :: Typography a
          -> Ast.Atom a
          -> Space
afterAtom t o = case decide t o of (_, r, _) -> r

-- | Normalize the input in order to add it to a generated Text.
normalizeAtom :: Typography a
              -> Ast.Atom a
              -> a
normalizeAtom t o = case decide t o of (_, _, r) -> r

-- | The French typography. It can be used with several generation
-- approach, as it stay very generic.
frenchTypo :: IsString a => Typography a
frenchTypo = Typography t prevT nextT
  where
    t :: IsString a => Ast.Atom a -> (Space, Space, a)
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

    prevT True = Just (Ast.LongDash)
    prevT False = Just (Ast.OpenQuote)

    nextT True = Nothing
    nextT False = Just (Ast.CloseQuote)

englishTypo :: IsString a => Typography a
englishTypo = Typography t (pure $ Just Ast.OpenQuote) (pure $ Just Ast.CloseQuote)
  where
    t :: IsString a => Ast.Atom a -> (Space, Space, a)
    t (Ast.Word w) = (Normal, Normal, w)
    t (Ast.Punctuation Ast.Semicolon) = (None, Normal, ";")
    t (Ast.Punctuation Ast.Colon) = (None, Normal, ":")
    t (Ast.Punctuation Ast.OpenQuote) = (Normal, None, "“")
    t (Ast.Punctuation Ast.CloseQuote) = (None, Normal, "”")
    t (Ast.Punctuation Ast.Question) = (None, Normal, "?")
    t (Ast.Punctuation Ast.Exclamation) = (None, Normal, "!")
    t (Ast.Punctuation Ast.LongDash) = (Normal, None, "—")
    t (Ast.Punctuation Ast.Dash) = (None, None, "–")
    t (Ast.Punctuation Ast.Hyphen) = (None, None, "-")
    t (Ast.Punctuation Ast.Comma) = (None, Normal, ",")
    t (Ast.Punctuation Ast.Point) = (None, Normal, ".")
    t (Ast.Punctuation Ast.SuspensionPoints) = (None, Normal, "…")
