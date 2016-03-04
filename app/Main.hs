{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

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
  input <- readFile "sample.up"
  case parse document "(stdin)" input of
    Right comps -> do
      TIO.putStrLn [st|<html>
  <head>
    <meta charset=utf-8>
    <style>
      .by-kata .reply {
        color:blue;
      }
    </style>
  </head>
  <body>
    #{generateHTML comps}
    <pre>#{show comps}</pre>
  </body>
</html>|]
    Left err -> putStrLn $ show err
