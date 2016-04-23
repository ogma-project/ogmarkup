{-|
Module      : Text.Ogmarkup.Private.Config
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

This module provides the 'GenConf' typeclass which is used to configure the
'Text.Ogmarkup.Private.Generator's monad.
-}

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Ogmarkup.Private.Config where

import           Data.Monoid
import           Data.String
import           Data.Maybe
import           Text.Ogmarkup.Private.Typography

-- | A 'Template' is just synonym for a template of one argument.
type Template a = a -> a

-- | An instance of the 'GenConf' typeclass can be given as a parameter to
-- a 'Text.Ogmarkup.Private.Generator.Generator'.  In order to prevent GHC
-- to overabstract this typeclass, there can be only one instance of
-- GenConf for one datatype. In other words, one datatype can only handle
-- one return type @c@.
--
-- For each template, we give a prefered layout and some hints about the
-- default implementation (which is almost always the identity function,
-- ignoring all the parameters but the generated output.
--
-- __Warning:__ 'GenConf' is a multiparam typeclass (see
-- @MultiParamTypeClasses@ GHC extension). In order to make new instances,
-- the following extensions need to be enabled:
--
--     * @FlexibleInstances@
--     * @MultiParamTypeClasses@
--
-- Otherwise, GHC will not accept your instance statement.
class (IsString o, Monoid o) => GenConf c o | c -> o where
  -- | Returns a 'Typography' instance to be used by the
  -- 'Text.Ogmarkup.Private.Generator.Generator'.
  --
  --     * __Default implementation:__ returns 'englishTypo'.
  typography :: c
             -> Typography o
  typography _ = englishTypo

  -- | This template is used once the output has been completely generated.
  --
  --     * __Layout:__ block 
  --     * __Default implementation:__ the identity function
  documentTemplate :: c
                   -> Template o
  documentTemplate _ = id

  -- | This template is used on the ill-formed portion of the ogmarkup
  -- document which have been ignored during the best-effort compilation.
  --
  --     * __Layout:__ inline
  --     * __Default implementation:__ the identity function
  errorTemplate :: c
                -> Template o
  errorTemplate _ = id

  -- | This template is used on regular sections which focus on telling the
  -- story.
  --
  --     * __Layout:__ block
  --     * __Default implementation:__ the identity function
  storyTemplate :: c
                -> Template o
  storyTemplate _ = id

  -- | This template is used on aside sections which may comprised
  -- flashbacks, letters or songs, for instance.
  --
  --     * __Layout:__ block
  --     * __Default implementation:__ the identity function (it ignores the
  --       class name)
  asideTemplate :: c
                -> Maybe o      -- ^ An optional class name
                -> Template o
  asideTemplate _ _ = id

  -- | This template is used on paragraphs within a section.
  --
  --     * __Layout:__ block
  --     * __Default implementation:__ the identity function
  paragraphTemplate :: c
                    -> Template o
  paragraphTemplate _ = id

  -- | This template is used on the narrative portion of an ogmarkup
  -- document.
  --
  --     * __Layout:__ inline
  --     * __Default implementation:__ the identity function
  tellerTemplate :: c
                 -> Template o
  tellerTemplate _ = id

  -- | This template is used on audible dialogue. The class name is
  -- mandatory even if the character name is optional for dialogues and
  -- thoughts in the ogmarkup grammar.  The `authorNormalize` function is
  -- used by the generator to get a default value when no character names
  -- are provided.
  --
  --     * __Layout:__ inline
  --     * __Default implementation:__ the identity function
  dialogueTemplate :: c
                   -> o            -- ^ The class name produced by 'authorNormalize'
                   -> Template o
  dialogueTemplate _ _ = id

  -- | This template is used on unaudible dialogue. See 'dialogueTemplate' to
  -- get more information on why the class name is not
  -- optional.
  --
  --     * __Layout:__ inline
  --     * __Default implementation:__ the identity function
  thoughtTemplate :: c
                   -> o            -- ^ The class name produced by 'authorNormalize'
                  -> Template o
  thoughtTemplate _ _ = id

  -- __Default implementation:__ the identity function
  replyTemplate :: c
                -> Template o
  replyTemplate _ = id

  -- | Return a marker to insert between two consecutive dialogues. The
  -- default use case is to return the concatenation of the ending mark and
  -- the opening mark of a paragraph.
  -- a paragrap
  --
  --     * __Layout:__ inline
  --     * __Default implementation:__ `mempty`
  betweenDialogue :: c
                  -> o
  betweenDialogue _ = mempty

  -- | A template to apply emphasis to an piece of text.
  --
  -- __Default implementation:__ the identity function
  emphTemplate :: c
               -> Template o
  emphTemplate _ = id

  -- | A template to apply stong emphasis (often bold) to a piece of text.
  --
  --     * __Layout:__ inline
  --     * __Default implementation:__ `mempty`
  strongEmphTemplate :: c
                     -> Template o
  strongEmphTemplate _ = id

  -- | This function is called by a Generator to derive a class name for an
  -- optional character name.
  --
  --     * __Default implementation:__ simply unwrap the Maybe value, if
  --     Nothing return 'mempty'.
  authorNormalize :: c
                  -> Maybe o
                  -> o
  authorNormalize _ = maybe mempty id

  -- | Generate an output from a 'Space'.
  printSpace :: c
             -> Space
             -> o
