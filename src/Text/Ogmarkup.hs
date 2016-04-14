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
import           Text.ParserCombinators.Parsec

import qualified Text.Ogmarkup.Private.Config            as Conf
import qualified Text.Ogmarkup.Private.Ast       as Ast
import qualified Text.Ogmarkup.Private.Generator as Gen
import qualified Text.Ogmarkup.Private.Parser    as Parser
import qualified Text.Ogmarkup.Private.Typography as Typo

-- | From a String, parse and generate an output according to a generation configuration.
--   The inner definitions of the parser and the generator implies the output
--   type has to be an instance of the 'IsString' and 'Monoid' classes.
ogmarkup :: (IsString a, Monoid a, Conf.GenConf c a)
         => String
         -> c
         -> a
ogmarkup input conf = let res = Parser.parse Parser.document
                                             ""
                                             input
                      in case res of Right ast -> Gen.runGenerator (Gen.document ast) conf
                                     Left err -> error $ show err
