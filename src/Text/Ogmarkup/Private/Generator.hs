{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Text.Ogmarkup.Private.Generator where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Monoid

import qualified Text.Ogmarkup.Private.Ast        as Ast
import           Text.Ogmarkup.Private.Config     as Conf
import           Text.Ogmarkup.Private.Typography

newtype Generator a x = Generator { getState :: StateT (a, Maybe (Ast.Atom a)) (Reader (GenConf a)) x }
  deriving (Functor, Applicative, Monad, MonadState (a, Maybe (Ast.Atom a)), MonadReader (GenConf a))

runGenerator :: Monoid a
             => Generator a x
             -> GenConf a
             -> a
runGenerator gen conf = fst $ runReader (execStateT (getState gen) (mempty, Nothing)) conf

askConf :: (GenConf a -> b) -> Generator a b
askConf f = f <$> ask

apply :: Monoid a
      => (a -> a)
      -> Generator a x
      -> Generator a ()
apply app gen = do
  (str, maybe) <- get
  put (mempty, maybe)
  gen
  (str', maybe') <- get
  put (str `mappend` app str', maybe')

reset :: Generator a ()
reset = do
  (str, _) <- get
  put (str, Nothing)

raw :: Monoid a
    => a
    -> Generator a ()
raw str' = do
  (str, maybePrev) <- get
  put (str `mappend` str', maybePrev)

atom :: Monoid a
     => Ast.Atom a
     -> Generator a ()
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

maybeAtom :: Monoid a
          => Maybe (Ast.Atom a)
          -> Generator a ()
maybeAtom (Just text) = atom text
maybeAtom Nothing = return ()

atoms :: Monoid a
      => [Ast.Atom a]
      -> Generator a ()
atoms (f:rst) = do
  atom f
  atoms rst
atoms [] = return ()

collection :: Monoid a
           => Ast.Collection a
           -> Generator a ()
collection (Ast.Quote as) = do atom (Ast.Punctuation Ast.OpenQuote)
                               atoms as
                               atom (Ast.Punctuation Ast.CloseQuote)

collection (Ast.Text as) = atoms as

collections :: Monoid a
            => [Ast.Collection a]
            -> Generator a ()
collections (f:rst) = do collection f
                         collections rst
collections [] = return ()

format :: Monoid a
       => Ast.Format a
       -> Generator a ()
format (Ast.Raw cs) = collections cs
format (Ast.Emph cs) = do
  temp <- askConf emphTemplate

  apply temp (collections cs)
format (Ast.StrongEmph cs) = do
  temp <- askConf strongEmphTemplate

  apply temp (collections cs)

formats :: Monoid a
           => [Ast.Format a]
           -> Generator a ()
formats (f:rst) = do
  format f
  formats rst
formats [] = return ()

reply :: Monoid a
      => Maybe (Ast.Atom a)
      -> Maybe (Ast.Atom a)
      -> Ast.Reply a
      -> Generator a ()
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

component :: Monoid a
          => Bool           -- ^ Was the last component an audible dialog?
          -> Bool           -- ^ Will the next component be an audible dialog?
          -> Ast.Component a  -- ^ The current to process.
          -> Generator a ()
component p n (Ast.Dialogue d a) = do
  typo <- askConf typography
  auth <- askConf authorNormalize
  temp <- askConf dialogueTemplate

  let
    open = openDialogue typo
    close = closeDialogue typo

  apply (temp $ auth a) (reply (Ast.Punctuation <$> open p) (Ast.Punctuation <$> close p) d)

component p n (Ast.Thought d a) = do
  auth <- askConf authorNormalize
  temp <- askConf dialogueTemplate

  apply (temp $ auth a) (reply Nothing Nothing d)
component p n (Ast.Teller fs) = formats fs

paragraph :: Monoid a
          => [Ast.Component a]
          -> Generator a ()
paragraph l@(h:r) = do
  temp <- askConf paragraphTemplate
  between <- askConf betweenDialogue

  apply temp (recGen between False (willBeDialogue l) (h:r))

  where
    isDialogue (Ast.Dialogue _ _) = True
    isDialogue _ = False

    willBeDialogue (h:n:r) = isDialogue n
    willBeDialogue _ = False

    recGen :: Monoid a
           => a
           -> Bool
           -> Bool
           -> [Ast.Component a]
           -> Generator a ()
    recGen between p n (c:rst) = do
      case (p, isDialogue c) of (True, True) -> do raw between
                                                   reset
                                _ -> return ()
      component p n c
      recGen between (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ [] = return ()

paragraphs :: Monoid a
           => [Ast.Paragraph a]
           -> Generator a ()
paragraphs (h:r) = do paragraph h
                      reset
                      paragraphs r
paragraphs [] = return ()

section :: Monoid a
        => Ast.Section a
        -> Generator a ()
section (Ast.Story ps) = do temp <- askConf storyTemplate

                            apply temp (paragraphs ps)
section (Ast.Aside ps) = do temp <- askConf asideTemplate

                            apply temp (paragraphs ps)

sections :: Monoid a
         => [Ast.Section a]
         -> Generator a ()
sections (s:r) = do section s
                    sections r
sections [] = return ()

document :: Monoid a
          => Ast.Document a
         -> Generator a ()
document d = do temp <- askConf documentTemplate

                apply temp (sections d)
