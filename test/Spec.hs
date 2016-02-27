import Test.Hspec

import Parser

main :: IO ()
main = do
  hspec specialcharSpec
  hspec wordSpec
  hspec textSpec
  hspec rawSpec
  hspec strongemphSpec
  hspec weakemphSpec
  hspec formatSpec
  hspec dialogSpec
  hspec tellerSpec
  hspec componentSpec
  hspec paragraphSpec
