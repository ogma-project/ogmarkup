{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Ogmarkup.HTML (
  generateHTML,
  ) where

import Text.Shakespeare.Text
import Data.Text (Text, append)
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
                         "<p>"
                         "</p>"
                         (\a -> mkSpan $ [st|dialogue by-#{a}|])
                         (pure "</span>")
                         (\a -> mkSpan $ [st|"thought by-#{a}|])
                         (pure "</span>")
                         id
                         (mkSpan "reply")
                         "</span>"
                         "<emph>"
                         "</emph>"
                         "<strong>"
                         "</strong>"
                         htmlPrintSpace

-- | From an Ogma AST, generate a HTML body.
generateHTML :: Ast.Document
             -> Text
generateHTML = generate htmlFrenchConf
