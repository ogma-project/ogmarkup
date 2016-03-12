{-# LANGUAGE OverloadedStrings #-}

module Text.Ogmarkup.Private.Config where

import           Data.Monoid
import           Text.Ogmarkup.Private.Typography

type Template a = a -> a

-- | A data type to carry out the generation configuration. In
--   particular, it works well to define a Typography and some
--   marker such as HTML tags.
data GenConf a = GenConf { -- | The Typography to use for the generation
                         typography         :: Typography a,
                         documentTemplate   :: Template a,
                         storyTemplate      :: Template a,
                         asideTemplate      :: Template a,
                         paragraphTemplate  :: Template a,
                         tellerTemplate     :: Template a,
                         dialogueTemplate   :: a -> Template a,
                         thoughtTemplate    :: a -> Template a,
                         replyTemplate      :: Template a,
                         betweenDialogue    :: a,
                         emphTemplate       :: Template a,
                         strongEmphTemplate :: Template a,
                         authorNormalize    :: a -> a,
                         printSpace         :: Space -> a }
