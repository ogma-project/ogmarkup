{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Text.Ogmarkup

import           Data.Text                     (Text)
import qualified Data.Text.IO                  as TIO
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.Shakespeare.Text

main :: IO ()
main = do
  input <- readFile "examples/sample.up"
  case ogmarkup input (htmlConf frenchTypo) of
    Right res -> TIO.putStrLn [st|<html>
  <head>
    <meta charset=utf-8>
    <style>
    body {
      margin:auto;
      width: 80%;
      max-width: 600px;
      text-align: justify;
    }
    p {
      text-indent:25px;
    }
    .reply {
      color:gray;
    }
    .dialogue .by-kahina .reply {
      font-weight: bold;
    }
    .thought .reply {
      font-style: italic;
    }
    </style>
  </head>
  <body>
    #{res}
  </body>
</html>|]
    Left err -> print err

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
