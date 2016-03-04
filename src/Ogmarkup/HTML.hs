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
                         ("<article>", "</article>")
                         ("", "")
                         ("<blockquote>", "</blockquote>")
                         ("<p>", "</p>")
                         ("", "")
                         (\a -> (mkSpan $ [st|dialogue by-#{a}|], "</span>"))
                         (\a -> (mkSpan $ [st|"thought by-#{a}|], "</span>"))
                         (mkSpan "reply", "</span>")
                         ("<emph>", "</emph>")
                         ("<strong>", "</strong>")
                         id
                         htmlPrintSpace

-- | From an Ogma AST, generate a HTML body.
generateHTML :: Ast.Document
             -> Text
generateHTML = generate htmlFrenchConf
