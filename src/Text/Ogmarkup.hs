{-|
Module      : Text.Ogmarkup
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

The ogmarkup library provides an ogmarkup document compiler. This module is the
only one you should need to import in your project.

The library is still in an early stage of development, hence the "experimental"
stability. Be aware the exposed interface may change in future realase.
-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Text.Ogmarkup
    (
      -- * Parse and Generate
      ogmarkup,

      -- * Generation Configuration
      Conf.GenConf (..),
      Conf.Template,

      -- * Typography
      Typo.frenchTypo,
      Typo.englishTypo,
      Typo.Typography (..),

      -- * Punctuation marks and space
      Ast.Mark (..),
      Typo.Space (..),
    ) where

import           Data.String
import           Data.List hiding (uncons)
import           Data.Monoid
import           Text.Megaparsec

import qualified Text.Ogmarkup.Private.Config     as Conf
import qualified Text.Ogmarkup.Private.Ast        as Ast
import qualified Text.Ogmarkup.Private.Generator  as Gen
import qualified Text.Ogmarkup.Private.Parser     as Parser
import qualified Text.Ogmarkup.Private.Typography as Typo

-- | From a String, parse and generate an output according to a generation configuration.
--   The inner definitions of the parser and the generator imply that the output
--   type has to be an instance of the 'IsString' and 'Monoid' classes.
ogmarkup :: (Stream a, Token a ~ Char, IsString (Tokens a), IsString a, Eq a, Monoid a, IsString b, Monoid b, Conf.GenConf c b)
         => a         -- ^ The input string
         -> c         -- ^ The generator configuration
         -> b
ogmarkup input conf = case Parser.parse Parser.document "" input of
                        Right ast -> Gen.runGenerator (Gen.document $ merge ast) conf
                        Left _    -> error "failed to parse an ogmarkup document even with best effort"
  where merge :: Ast.Document a -> Ast.Document a
        merge ((Ast.Story x):rest) = (Ast.Story $ mergep x):rest
        merge ((Ast.Aside cls x):rest) = (Ast.Aside cls (mergep x)):rest
        merge (x:rest) = x:(merge rest)
        merge [] = []

        mergep :: [Ast.Paragraph a] -> [Ast.Paragraph a]
        mergep (x@(_:_):y@((Ast.Dialogue _ _):_):rest) =
          case last x of
            Ast.Dialogue _ _ -> mergep $ (x `mappend` y):rest
            _                -> x:(mergep $ y:rest)
        mergep (x:y:rest) = x:(mergep $ y:rest)
        mergep x = x
