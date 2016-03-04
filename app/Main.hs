{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Ogmarkup.Typography
import Ogmarkup.HTML
import Ogmarkup.Private.Ast
import Ogmarkup.Private.Parser

import Data.Text (Text)
import Text.Shakespeare.Text
import qualified Data.Text.IO as TIO
import Text.ParserCombinators.Parsec
import System.IO

main :: IO ()
main = do
  input <- readFile "examples/sample.up"
  case generateHTML frenchTypo input of
    Right res -> do
      TIO.putStrLn [st|<html>
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
    Left err -> putStrLn $ show err
