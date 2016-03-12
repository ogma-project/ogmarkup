{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Text.Ogmarkup

import           Data.Text                     (Text)
import qualified Data.Text.IO                  as TIO
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.Shakespeare.Text
import Text.Hamlet
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = do
  input <- readFile "examples/sample.up"
  case ogmarkup input (htmlConf frenchTypo) of
    Right res -> putStrLn $ renderHtml [shamlet|$doctype 5
<html>
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
  <body>
    #{res}|]
    Left err -> print err

htmlPrintSpace :: Space -> Html
htmlPrintSpace None = ""
htmlPrintSpace Normal = " "
htmlPrintSpace Nbsp = [shamlet|&nbsp;|]

htmlConf :: Typography Html
         -> GenConf Html
htmlConf typo =
  GenConf typo
          (\doc -> [shamlet|<article>#{doc}|])
          id
          (\aside -> [shamlet|<blockquote>#{aside}|])
          (\paragraph -> [shamlet|<p>#{paragraph}|])
          id
          (\a dialogue -> [shamlet|$newline never
                                   <span .dialogue .by-#{a}>
                                     #{dialogue}|])
          (\a thought -> [shamlet|$newline never
                                  <span .thought .by-#{a}>
                                    #{thought}|])
          (\reply -> [shamlet|$newline never
                              <span .reply>
                                #{reply}|])
          "</p><p>"
          (\text -> [shamlet|<emph>#{text}|])
          (\text -> [shamlet|<strong>#{text}|])
          id
          htmlPrintSpace
