{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Criterion.Main
import Text.Ogmarkup
import Data.Text (Text)
import Data.FileEmbed

main = defaultMain [ bench "nf ogmarkup text -> text" $ nf (ogmarkup inputText) ConfText
                   , bench "whnf ogmarkup text -> text" $ whnf (ogmarkup inputText) ConfText
                   , bench "nf ogmarkup string -> string" $ nf (ogmarkup inputString) ConfString
                   , bench "whnf ogmarkup string -> string" $ whnf (ogmarkup inputString) ConfString

                   ]

inputString :: String
inputString = $(embedFile "bench/test.up")
inputText :: Text
inputText = $(embedFile "bench/test.up")

data ConfText = ConfText
instance GenConf ConfText Text where
    printSpace _ _ = " "

data ConfString = ConfString
instance GenConf ConfString String where
    printSpace _ _ = " "
