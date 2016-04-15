{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Ogmarkup.Private.Config where

import           Data.Monoid
import           Data.String
import           Text.Ogmarkup.Private.Typography

type Template a = a -> a

class (IsString o, Monoid o) => GenConf c o | c -> o where
  typography :: c
             -> Typography o
  typography _ = englishTypo

  documentTemplate :: c
                   -> Template o
  documentTemplate _ = id

  errorTemplate :: c
                -> Template o
  errorTemplate _ = id

  storyTemplate :: c
                -> Template o
  storyTemplate _ = id

  asideTemplate :: c
                -> Maybe o
                -> Template o
  asideTemplate _ _ = id

  paragraphTemplate :: c
                    -> Template o
  paragraphTemplate _ = id

  tellerTemplate :: c
                 -> Template o
  tellerTemplate _ = id

  dialogueTemplate :: c
                   -> o
                   -> Template o
  dialogueTemplate _ _ = id

  thoughtTemplate :: c
                  -> o
                  -> Template o
  thoughtTemplate _ _ = id

  replyTemplate :: c
                -> Template o
  replyTemplate _ = id

  betweenDialogue :: c
                  -> o
  betweenDialogue _ = mempty

  emphTemplate :: c
               -> Template o
  emphTemplate _ = id

  strongEmphTemplate :: c
                     -> Template o
  strongEmphTemplate _ = id

  authorNormalize :: c
                  -> Maybe o
                  -> o
  authorNormalize _ (Just auth) = auth
  authorNormalize _ Nothing = mempty

  printSpace :: c
             -> Space
             -> o
