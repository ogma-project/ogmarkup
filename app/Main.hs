{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Text.Ogmarkup

import           Data.Text                     (Text)
import qualified Data.Text.IO                  as TIO
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.Shakespeare.Text
import Text.Hamlet
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (preEscapedToHtml)

main :: IO ()
main = do
  input <- readFile "examples/sample.up"
  let res = ogmarkup input HtmlConf

  putStrLn $ renderHtml [shamlet|$doctype 5
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
      .error {
        color: red;
        background-color: black;
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

data HtmlConf = HtmlConf

instance GenConf HtmlConf Html where
  typography _ = frenchTypo

  documentTemplate _ doc = [shamlet|<article>#{doc}|]

  asideTemplate _ (Just cls) a = [shamlet|<blockquote .#{cls}>#{a}|]
  asideTemplate _ _ a = [shamlet|<blockquote>#{a}|]

  paragraphTemplate _ paragraph = [shamlet|<p>#{paragraph}|]

  dialogueTemplate _ a dialogue = [shamlet|$newline never
                                           <span .dialogue .#{a}>#{dialogue}|]

  thoughtTemplate _ a thought = [shamlet|$newline never
                                         <span .thought .by-#{a}>#{thought}|]

  replyTemplate _ reply = [shamlet|$newline never
                                   <span .reply>#{reply}|]

  betweenDialogue _ = preEscapedToHtml ("</p><p>" :: Text)

  emphTemplate _ text = [shamlet|$newline never
                                 <em>#{text}|]
  strongEmphTemplate _ text = [shamlet|$newline never
                                       <strong>#{text}|]

  authorNormalize _ Nothing = "by-anonymus"
  authorNormalize _ (Just auth) = [shamlet|by-#{auth}|]

  printSpace _ None = ""
  printSpace _ Normal = " "
  printSpace _ Nbsp = [shamlet|&nbsp;|]
