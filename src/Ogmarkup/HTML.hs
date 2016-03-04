{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Ogmarkup.HTML (
  generateHTML,
  ) where

import Text.Shakespeare.Text
import Data.Text (Text)
import Ogmarkup.Config
import qualified Ogmarkup.Private.Ast as Ast
import Ogmarkup.Private.Generator

htmlPrintSpace :: Space -> Text
htmlPrintSpace None = ""
htmlPrintSpace Normal = " "
htmlPrintSpace Nbsp = "&nbsp;"

mkSpan :: Text -> Text
mkSpan clss = [st|<span class="#{clss}">|]

htmlFrenchConf :: GenConf
htmlFrenchConf = GenConf frenchTypo
  (\doc -> [st|<article>#{doc}</article>|])
  id
  (\aside -> [st|<blockquote>#{aside}</blockquote>|])
  (\paragraph -> [st|<p>#{paragraph}</p>|])
  id
  (\a -> \dialogue -> [st|<span class="dialogue by-#{a}">#{dialogue}</span>|])
  (\a -> \thought -> [st|<span class="thought by-#{a}">#{thought}</span>|])
  (\reply -> [st|<span>#{reply}</span>|])
  "</p><p>"
  (\text -> [st|<emph>#{text}</emph>|])
  (\text -> [st|<strong>#{text}</strong>|])
  id
  htmlPrintSpace

-- | From an Ogma AST, generate a HTML body.
generateHTML :: Ast.Document
             -> Text
generateHTML = generate htmlFrenchConf
