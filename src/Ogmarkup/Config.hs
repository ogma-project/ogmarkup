{-# LANGUAGE OverloadedStrings #-}

module Ogmarkup.Config where

import Ogmarkup.Typography
import Data.Text (Text)

type Tag = Text -> Text

-- | A data type to carry out the generation configuration. In
--   particular, it works well to define a Typography and some
--   marker such as HTML tags.
data GenConf = GenConf { -- | The Typography to use for the generation
                         typography :: Typography,
                         documentTag :: Tag,
                         storyTag :: Tag,
                         asideTag :: Tag,
                         paragraphTag :: Tag,
                         tellerTag :: Tag,
                         dialogueTag :: Text -> Tag,
                         thoughtTag :: Text -> Tag,
                         replyTag :: Tag,
                         betweenDialogue :: Text,
                         emphTag :: Tag,
                         strongEmphTag :: Tag,
                         authorNormalize :: Text -> Text,
                         printSpace :: Space -> Text }
