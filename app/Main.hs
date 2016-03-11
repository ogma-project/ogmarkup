{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Text.Ogmarkup.HTML
import           Text.Ogmarkup.Private.Ast
import           Text.Ogmarkup.Private.Parser
import           Text.Ogmarkup.Typography

import           Data.Text                     (Text)
import qualified Data.Text.IO                  as TIO
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.Shakespeare.Text

main :: IO ()
main = do
  input <- readFile "examples/sample.up"
  case generateHTML frenchTypo input of
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
