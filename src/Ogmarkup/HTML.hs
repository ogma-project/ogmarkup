{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Ogmarkup.HTML (
  generateHTML,
  ) where

import Text.Shakespeare.Text
import Data.Text (Text)
import Ogmarkup.Typography
import Ogmarkup.Config
import qualified Ogmarkup.Private.Ast as Ast
import Ogmarkup.Private.Generator
import Ogmarkup.Private.Parser
import Text.ParserCombinators.Parsec

htmlPrintSpace :: Space -> Text
htmlPrintSpace None = ""
htmlPrintSpace Normal = " "
htmlPrintSpace Nbsp = "&nbsp;"

mkSpan :: Text -> Text
mkSpan clss = [st|<span class="#{clss}">|]

htmlConf :: Typography
         -> GenConf
htmlConf typo =
  GenConf typo
          (\doc -> [st|<article>#{doc}</article>|])
          id
          (\aside -> [st|<blockquote>#{aside}</blockquote>|])
          (\paragraph -> [st|<p>#{paragraph}</p>|])
          id
          (\a -> \dialogue -> [st|<span class="dialogue by-#{a}">#{dialogue}</span>|])
          (\a -> \thought -> [st|<span class="thought by-#{a}">#{thought}</span>|])
          (\reply -> [st|<span class="reply">#{reply}</span>|])
          "</p><p>"
          (\text -> [st|<emph>#{text}</emph>|])
          (\text -> [st|<strong>#{text}</strong>|])
          id
          htmlPrintSpace

-- | From an Ogma AST, generate a HTML body.
generateHTML :: Typography
             -> String
             -> Either ParseError Text
generateHTML typo input = let res = parse document "" input
                          in case res of Right ast -> Right $ generate (htmlConf typo) ast
                                         Left err -> Left err
