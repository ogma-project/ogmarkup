module Text.Ogmarkup
    (
      Ast.Mark (..),
      ogmarkup
    ) where

import           Data.String
import           Text.ParserCombinators.Parsec

import qualified Text.Ogmarkup.Config            as Conf
import qualified Text.Ogmarkup.Private.Ast       as Ast
import qualified Text.Ogmarkup.Private.Generator as Gen
import qualified Text.Ogmarkup.Private.Parser    as Parser

ogmarkup :: (IsString a, Monoid a) => String
         -> Conf.GenConf a
         -> Either ParseError a
ogmarkup input conf = let res = parse Parser.document "" input
                      in case res of Right ast -> Right $ Gen.generate conf ast
                                     Left err -> Left err
