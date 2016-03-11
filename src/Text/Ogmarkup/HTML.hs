{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Text.Ogmarkup.HTML (
  generateHTML,
  ) where

import           Data.Text                       (Text)
import           Text.Ogmarkup.Config
import qualified Text.Ogmarkup.Private.Ast       as Ast
import qualified Text.Ogmarkup.Private.Generator as Generator
import qualified Text.Ogmarkup.Private.Parser    as Parser
import           Text.Ogmarkup.Typography
import           Text.ParserCombinators.Parsec
import           Text.Shakespeare.Text

htmlPrintSpace :: Space -> Text
htmlPrintSpace None = ""
htmlPrintSpace Normal = " "
htmlPrintSpace Nbsp = "&nbsp;"

mkSpan :: Text -> Text
mkSpan clss = [st|<span class="#{clss}">|]

htmlConf :: Typography Text
         -> GenConf Text
htmlConf typo =
  GenConf typo
          (\doc -> [st|<article>#{doc}</article>|])
          id
          (\aside -> [st|<blockquote>#{aside}</blockquote>|])
          (\paragraph -> [st|<p>#{paragraph}</p>|])
          id
          (\a dialogue -> [st|<span class="dialogue by-#{a}">#{dialogue}</span>|])
          (\a thought -> [st|<span class="thought by-#{a}">#{thought}</span>|])
          (\reply -> [st|<span class="reply">#{reply}</span>|])
          "</p><p>"
          (\text -> [st|<emph>#{text}</emph>|])
          (\text -> [st|<strong>#{text}</strong>|])
          id
          htmlPrintSpace

-- | From an Ogma AST, generate a HTML body.
generateHTML :: Typography Text
             -> String
             -> Either ParseError Text
generateHTML typo input = let res = parse Parser.document "" input
                          in case res of Right ast -> Right $ Generator.generate (htmlConf typo) ast
                                         Left err -> Left err
