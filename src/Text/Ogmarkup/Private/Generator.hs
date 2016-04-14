{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Text.Ogmarkup.Private.Generator where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Monoid

import qualified Text.Ogmarkup.Private.Ast        as Ast
import           Text.Ogmarkup.Private.Config     as Conf
import           Text.Ogmarkup.Private.Typography

-- * The 'Generator' Monad

-- | The 'Generator' Monad is eventually used to generate an output from a
--   given 'Ast.Document. Internally, it keeps track of the previous processed
--   'Ast.Atom' in order to deal with atom separation.
newtype Generator c a x = Generator { getState :: StateT (a, Maybe (Ast.Atom a)) (Reader c) x }
  deriving (Functor, Applicative, Monad, MonadState (a, Maybe (Ast.Atom a)), MonadReader c)

-- | Run a 'Generator' monad and get the generated output. The output
--   type has to implement the class 'Monoid' because the 'Generator' monad
--   uses the 'mempty' constant as the initial state of the output and then
--   uses 'mappend' to expand the result as it process the generation.
runGenerator :: Monoid a
             => Generator c a x -- ^ The 'Generator' to run
             -> c               -- ^ The configuration to use during the generation
             -> a              -- ^ The output
runGenerator gen conf = fst $ runReader (execStateT (getState gen) (mempty, Nothing)) conf

-- * Low-level 'Generator's

-- | Retreive a configuration parameter. Let the output untouched.
askConf :: (c -> b) -- ^ The function to apply to the 'GenConf' variable
                    --   to retreive the wanted parameter.
        -> Generator c a b
askConf f = f <$> ask

-- | Apply a template to the result of a given 'Generator' before appending it
--   to the previously generated output.
apply :: Monoid a
      => Template a      -- ^ The 'Template' to apply.
      -> Generator c a x   -- ^ The 'Generator' to run.
      -> Generator c a ()
apply app gen = do
  (str, maybe) <- get
  put (mempty, maybe)
  gen
  (str', maybe') <- get
  put (str `mappend` app str', maybe')

-- | Forget about the past and consider the next 'Ast.Atom' is the
--   first to be processed.
reset :: Generator c a ()
reset = do
  (str, _) <- get
  put (str, Nothing)

-- | Append a new sub-output to the generated output
raw :: Monoid a
    => a                -- ^ A sub-output to append
    -> Generator c a ()
raw str' = do
  (str, maybePrev) <- get
  put (str `mappend` str', maybePrev)

-- * AST Processing 'Generator's

-- | Process an 'Ast.Atom' and deal with the space to use to separate it from
--   the paramter of the previous call (that is the previous processed
--   'Ast.Atom').
atom :: (Monoid a, GenConf c a)
     => Ast.Atom a
     -> Generator c a ()
atom text = do
  (str, maybePrev) <- get
  typo <- askConf typography
  ptrSpace <- askConf printSpace

  case maybePrev of
    Just prev ->
      let
        spc =  (ptrSpace $ max (afterAtom typo prev) (beforeAtom typo text))
        str' = spc `mappend` normalizeAtom typo text
      in
        put (str `mappend` str', Just text)
    Nothing -> put (str `mappend` normalizeAtom typo text, Just text)

-- | Call 'atom' if the parameter is not 'Nothing'. Otherwise, do nothing.
maybeAtom :: (Monoid a, GenConf c a)
          => Maybe (Ast.Atom a)
          -> Generator c a ()
maybeAtom (Just text) = atom text
maybeAtom Nothing = return ()

-- | Process a sequence of 'Ast.Atom'.
atoms :: (Monoid a, GenConf c a)
      => [Ast.Atom a]
      -> Generator c a ()
atoms (f:rst) = do
  atom f
  atoms rst
atoms [] = return ()

-- | Process a 'Ast.Format'.
format :: (Monoid a, GenConf c a)
       => Ast.Format a
       -> Generator c a ()

format (Ast.Raw as) = atoms as

format (Ast.Emph fs) = do
  temp <- askConf emphTemplate

  apply temp (formats fs)

format (Ast.StrongEmph fs) = do
  temp <- askConf strongEmphTemplate

  apply temp (formats fs)

format (Ast.Quote fs) = do
  atom $ Ast.Punctuation Ast.OpenQuote
  formats fs
  atom $ Ast.Punctuation Ast.CloseQuote

-- | Process a sequence of 'Ast.Format'.
formats :: (Monoid a, GenConf c a)
           => [Ast.Format a]
           -> Generator c a ()
formats (f:rst) = do
  format f
  formats rst
formats [] = return ()

-- | Process a 'Ast.Reply'.
reply :: (Monoid a, GenConf c a)
      => Maybe (Ast.Atom a)
      -> Maybe (Ast.Atom a)
      -> Ast.Reply a
      -> Generator c a ()
reply begin end (Ast.Simple d) = do
  temp <- askConf replyTemplate

  maybeAtom begin
  apply temp (formats d)
  maybeAtom end
reply begin end (Ast.WithSay d ws d') = do
  temp <- askConf replyTemplate

  maybeAtom begin
  apply temp (formats d)

  case d' of [] -> do
               maybeAtom end
               formats ws
             l -> do
               formats ws
               apply temp (formats d')
               maybeAtom end

-- | Process a 'Ast.Component'.
component :: (Monoid a, GenConf c a)
          => Bool           -- ^ Was the last component an audible dialog?
          -> Bool           -- ^ Will the next component be an audible dialog?
          -> Ast.Component a  -- ^ The current to process.
          -> Generator c a ()
component p n (Ast.Dialogue d a) = do
  typo <- askConf typography
  auth <- askConf authorNormalize
  temp <- askConf dialogueTemplate

  let
    open = openDialogue typo
    close = closeDialogue typo

  apply (temp $ auth a) (reply (Ast.Punctuation <$> open p) (Ast.Punctuation <$> close n) d)

component p n (Ast.Thought d a) = do
  auth <- askConf authorNormalize
  temp <- askConf thoughtTemplate

  apply (temp $ auth a) (reply Nothing Nothing d)
component p n (Ast.Teller fs) = formats fs
component p n (Ast.IllFormed ws) = do
    temp <- askConf errorTemplate
    apply temp (raw ws)

-- | Process a 'Ast.Paragraph' and deal with sequence of 'Ast.Reply'.
paragraph :: (Monoid a, GenConf c a)
          => Ast.Paragraph a
          -> Generator c a ()
paragraph l@(h:r) = do
  temp <- askConf paragraphTemplate
  between <- askConf betweenDialogue

  apply temp (recGen between False (willBeDialogue l) l)

  where
    isDialogue (Ast.Dialogue _ _) = True
    isDialogue _ = False

    willBeDialogue (h:n:r) = isDialogue n
    willBeDialogue _ = False

    recGen :: (Monoid a, GenConf c a)
           => a
           -> Bool
           -> Bool
           -> [Ast.Component a]
           -> Generator c a ()
    recGen between p n (c:rst) = do
      case (p, isDialogue c) of (True, True) -> do raw between
                                                   reset
                                _ -> return ()
      component p n c
      recGen between (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ [] = return ()

-- | Process a sequence of 'Ast.Paragraph'.
paragraphs :: (Monoid a, GenConf c a)
           => [Ast.Paragraph a]
           -> Generator c a ()
paragraphs (h:r) = do paragraph h
                      reset
                      paragraphs r
paragraphs [] = return ()

-- | Process a 'Ast.Section'.
section :: (Monoid a, GenConf c a)
        => Ast.Section a
        -> Generator c a ()
section (Ast.Story ps) = do temp <- askConf storyTemplate

                            apply temp (paragraphs ps)
section (Ast.Aside cls ps) = do temp <- askConf asideTemplate
                                apply (temp cls) (paragraphs ps)

section (Ast.Failing f) = do
    temp <- askConf errorTemplate
    temp2 <- askConf storyTemplate
    apply (temp2 . temp) (raw f)

-- | Process a sequence of 'Ast.Section'.
sections :: (Monoid a, GenConf c a)
         => [Ast.Section a]
         -> Generator c a ()
sections (s:r) = do section s
                    sections r
sections [] = return ()

-- | Process a 'Ast.Document', that is a complete Ogmarkup document
document :: (Monoid a, GenConf c a)
          => Ast.Document a
         -> Generator c a ()
document d = do temp <- askConf documentTemplate

                apply temp (sections d)
