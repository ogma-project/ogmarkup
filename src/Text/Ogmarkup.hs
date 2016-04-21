{-|
Module      : Text.Ogmarkup
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

The ogmarkup library provides an ogmarkup document compiler. This module is the
only one you should need to import in your project.

The library is still in a early stage of development, hence the "experimental"
stability. Be aware the exposed interface may change in future realase.
-}

module Text.Ogmarkup
    (
      -- * Parse and Generate
      Strategy (..),
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
import           Data.List
import           Text.ParserCombinators.Parsec
import           Data.Monoid

import qualified Text.Ogmarkup.Private.Config     as Conf
import qualified Text.Ogmarkup.Private.Ast        as Ast
import qualified Text.Ogmarkup.Private.Generator  as Gen
import qualified Text.Ogmarkup.Private.Parser     as Parser
import qualified Text.Ogmarkup.Private.Typography as Typo

-- | With the best-effort compilation feature of ogmarkup, when the parser
--   encounters an error, it can apply two different strategies with the
--   remaining input to find a new synchronization point. It can search
--   character by character or line by line.
data Strategy =
    ByLine
  | ByChar

-- | From a String, parse and generate an output according to a generation configuration.
--   The inner definitions of the parser and the generator implies the output
--   type has to be an instance of the 'IsString' and 'Monoid' classes.
ogmarkup :: (IsString a, Monoid a, Conf.GenConf c a)
         => Strategy       -- ^ Best-effort compilation strategy
         -> String         -- ^ The input string
         -> c              -- ^ The generator configuration
         -> a
ogmarkup be input conf = Gen.runGenerator (Gen.document (_ogmarkup be "" input [])) conf
  where
    _ogmarkup :: (IsString a, Monoid a)
              => Strategy -- best-effort compilation strategy
              -> String   -- acc
              -> String   -- input
              -> Ast.Document a
              -> Ast.Document a

    _ogmarkup _ "" "" ast = ast

    _ogmarkup _ acc "" ast = ast `mappend` [Ast.Failing . fromString $ acc]

    _ogmarkup be acc input ast =
      case Parser.parse Parser.document "" input of
        Right (ast', input') ->
          if input == input'  -- nothing has been parsed
          then let (c, rst) = applyStrat be input
               in _ogmarkup be (acc `mappend` c) rst ast
          else if acc == ""
               then _ogmarkup be [] input' (ast `mappend` ast')
               else let f = Ast.Failing . fromString $ acc
                    in _ogmarkup be [] input' (ast `mappend` (f:ast'))
        Left err -> error $ show err

    applyStrat :: Strategy
               -> String
               -> (String, String)

    applyStrat _ [] = ([], [])
    applyStrat ByLine input = break (== '\n') input
    applyStrat ByChar (c:rst) = ([c], rst)
