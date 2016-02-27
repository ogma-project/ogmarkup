import Ogmarkup.HTML
import Ogmarkup.Private.Ast
import Ogmarkup.Private.Parser

import Text.ParserCombinators.Parsec
import System.IO

main :: IO ()
main = do
  input <- readFile "sample.up"
  case parse document "(stdin)" input of
    Right comps -> do putStrLn "<html>\n<head>\n<meta charset=utf-8>\n<style>\n.by-kata .reply {\ncolor:blue;\n}\n</style>\n</head>\n<body>\n"
                      putStrLn $ generateHTML comps
                      putStrLn $ "<pre>"
                      putStrLn $ show comps
                      putStrLn $ "</pre>"
                      putStrLn "</body></html>"
    Left err -> putStrLn $ show err
