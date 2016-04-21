{-|
Module      : Text.Ogmarkup.Private.Typography
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

This module provides the 'Typography' datatype along with two default instance
for French and English.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Ogmarkup.Private.Typography where

import           Data.String
import qualified Text.Ogmarkup.Private.Ast as Ast

-- * Inner spaces representation

-- | Deal with typographic spaces, especially when it comes to
--   separate two texts. Because Space derives Ord, it is possible
--   to use min and max to determine which one to use in case of
--   a conflict.
data Space =
  Normal -- ^ A normal space that can be turned into a newline for displaying.
  | Nbsp -- ^ A non breakable space, it cannot be turned into a newline.
  | None -- ^ No space at all.
    deriving (Eq,Ord)

-- * Typography definition

-- | A Typography is a data type that tells the caller what space
--   she should privileged before and after a text.
data Typography a = Typography {
  decide        :: Ast.Mark -> (Space, Space, a), -- ^ For a given 'Ast.Mark',
                                                  --  returns a tuple with the
                                                  --  spaces to use before
                                                  --  and after the
                                                  --  punctuation mark and
                                                  --  its output value.
  openDialogue  :: Bool -> Maybe Ast.Mark,        -- ^ Which mark to use to
                                                  -- open a dialogue. If
                                                  -- the parameter is True,
                                                  -- there were another
                                                  -- dialogue just before.
  closeDialogue :: Bool -> Maybe Ast.Mark         -- ^ Which mark to use to
                                                  -- close a dialogue. If
                                                  -- the parameter is True,
                                                  -- there is another
                                                  -- dialogue just after.
  }

-- | Apply the function to each 'Ast.Mark' output value
instance Functor Typography where
  f `fmap` (Typography d o c) = let d' m = let (s1, s2, x) = d m in (s1, s2, f x)
                                in Typography d' o c

-- | From a Typography, it gives the space to privilege before the
--   input Text.
beforeAtom :: Typography a
           -> Ast.Atom a
           -> Space
beforeAtom t (Ast.Punctuation m) = case decide t m of (r, _, _) -> r
beforeAtom t _ = Normal

-- | From a Typography, it gives the space to privilege after the
--   input Text.
afterAtom :: Typography a
          -> Ast.Atom a
          -> Space
afterAtom t (Ast.Punctuation m) = case decide t m of (_, r, _) -> r
afterAtom t _ = Normal

-- | Normalize the input in order to add it to a generated Text.
normalizeAtom :: Typography a
              -> Ast.Atom a
              -> a
normalizeAtom t (Ast.Punctuation m) = case decide t m of (_, _, r) -> r
normalizeAtom t (Ast.Word w) = w

-- * Ready-to-use Typography

-- | A proposal for the French typography. It can be used with several generation
--   approach, as it stay very generic. Required the output type to be an
--   instance of 'IsString'.
frenchTypo :: IsString a => Typography a
frenchTypo = Typography t prevT nextT
  where
    t :: IsString a => Ast.Mark -> (Space, Space, a)
    t Ast.Semicolon = (Nbsp, Normal, ";")
    t Ast.Colon = (Nbsp, Normal, ":")
    t Ast.OpenQuote = (Normal, Nbsp, "«")
    t Ast.CloseQuote = (Nbsp, Normal, "»")
    t Ast.Question = (Nbsp, Normal, "?")
    t Ast.Exclamation = (Nbsp, Normal, "!")
    t Ast.LongDash = (Normal, Normal, "—")
    t Ast.Dash = (None, None, "–")
    t Ast.Hyphen = (None, None, "-")
    t Ast.Comma = (None, Normal, ",")
    t Ast.Point = (None, Normal, ".")
    t Ast.Apostrophe = (None, None, "’")
    t Ast.SuspensionPoints = (None, Normal, "…")

    prevT True = Just Ast.LongDash
    prevT False = Just Ast.OpenQuote

    nextT True = Nothing
    nextT False = Just Ast.CloseQuote

-- | A proposal for the English typography. It can be used with several generation
--   approach, as it stay very generic. Required the output type to be an
--   instance of 'IsString'.
englishTypo :: IsString a => Typography a
englishTypo = Typography t (pure $ Just Ast.OpenQuote) (pure $ Just Ast.CloseQuote)
  where
    t :: IsString a => Ast.Mark -> (Space, Space, a)
    t Ast.Semicolon = (None, Normal, ";")
    t Ast.Colon = (None, Normal, ":")
    t Ast.OpenQuote = (Normal, None, "“")
    t Ast.CloseQuote = (None, Normal, "”")
    t Ast.Question = (None, Normal, "?")
    t Ast.Exclamation = (None, Normal, "!")
    t Ast.LongDash = (Normal, None, "—")
    t Ast.Dash = (None, None, "–")
    t Ast.Hyphen = (None, None, "-")
    t Ast.Comma = (None, Normal, ",")
    t Ast.Point = (None, Normal, ".")
    t Ast.Apostrophe = (None, None, "'")
    t Ast.SuspensionPoints = (None, Normal, "…")
