module Ogmarkup.HTML (
  generateHTML,
  ) where

import Ogmarkup.Config
import qualified Ogmarkup.Private.Ast as Ast
import Ogmarkup.Private.Generator

htmlPrintSpace :: Space -> String
htmlPrintSpace None = ""
htmlPrintSpace Normal = " "
htmlPrintSpace Nbsp = "&nbsp;"

mkSpan :: String -> String
mkSpan clss = "<span class=\"" ++ clss ++ "\">"

htmlFrenchConf :: GenConf
htmlFrenchConf = GenConf frenchTypo
                         "<p>"
                         "</p>"
                         (\a -> mkSpan $ "dialogue by-" ++ a)
                         (\_ -> "</span>")
                         (\a -> mkSpan $ "thought by-" ++ a)
                         (\_ -> "</span>")
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
             -> String
generateHTML = generate htmlFrenchConf
