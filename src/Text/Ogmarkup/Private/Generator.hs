{-# LANGUAGE OverloadedStrings #-}

module Text.Ogmarkup.Private.Generator where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Monoid

import           Text.Ogmarkup.Config
import qualified Text.Ogmarkup.Private.Ast       as Ast
import           Text.Ogmarkup.Typography

type Generator a = StateT (a, Maybe (Ast.Atom a))
                          (Reader (GenConf a))
                          ()

apply :: Monoid a
      => (a -> a)
      -> Generator a
      -> Generator a
apply app gen = do
  (str, maybe) <- get
  put (mempty, maybe)
  gen
  (str', maybe') <- get
  put (str `mappend` app str', maybe')

reset :: Generator a
reset = do
  (str, _) <- get
  put (str, Nothing)

raw :: Monoid a
    => a
    -> Generator a
raw str' = do
  (str, maybePrev) <- get
  put (str `mappend` str', maybePrev)

atom :: Monoid a
     => Ast.Atom a
     -> Generator a
atom text = do
  (str, maybePrev) <- get
  typo <- typography <$> ask
  ptrSpace <- printSpace <$> ask

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
          -> Generator a
maybeAtom (Just text) = atom text
maybeAtom Nothing = return ()

atoms :: Monoid a
      => [Ast.Atom a]
      -> Generator a
atoms (f:rst) = do
  atom f
  atoms rst
atoms [] = return ()

collection :: Monoid a
           => Ast.Collection a
           -> Generator a
collection (Ast.Quote as) = do atom (Ast.Punctuation Ast.OpenQuote)
                               atoms as
                               atom (Ast.Punctuation Ast.CloseQuote)

collection (Ast.Text as) = atoms as

collections :: Monoid a
            => [Ast.Collection a]
            -> Generator a
collections (f:rst) = do collection f
                         collections rst
collections [] = return ()

format :: Monoid a
       => Ast.Format a
       -> Generator a
format (Ast.Raw cs) = collections cs
format (Ast.Emph cs) = do
  temp <- emphTemplate <$> ask

  apply temp (collections cs)
format (Ast.StrongEmph cs) = do
  temp <- strongEmphTemplate <$> ask

  apply temp (collections cs)

formats :: Monoid a
           => [Ast.Format a]
           -> Generator a
formats (f:rst) = do
  format f
  formats rst
formats [] = return ()

reply :: Monoid a
      => Maybe (Ast.Atom a)
      -> Maybe (Ast.Atom a)
      -> Ast.Reply a
      -> Generator a
reply begin end (Ast.Simple d) = do
  temp <- replyTemplate <$> ask

  maybeAtom begin
  apply temp (formats d)
  maybeAtom end
reply begin end (Ast.WithSay d ws d') = do
  temp <- replyTemplate <$> ask

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
          -> Generator a
component p n (Ast.Dialogue d a) = do
  conf <- ask

  let
    open = openDialogue . typography $ conf
    close = closeDialogue . typography $ conf
    auth = authorNormalize conf
    temp = dialogueTemplate conf $ auth a

  apply temp (reply (Ast.Punctuation <$> open p) (Ast.Punctuation <$> close p) d)

component p n (Ast.Thought d a) = do
  conf <- ask

  let
    auth = authorNormalize conf
    temp = dialogueTemplate conf (auth a)

  apply temp (reply Nothing Nothing d)
component p n (Ast.Teller fs) = formats fs

paragraph :: Monoid a
          => [Ast.Component a]
          -> Generator a
paragraph l@(h:r) = do
  temp <- paragraphTemplate <$> ask
  between <- betweenDialogue <$> ask

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
           -> Generator a
    recGen between p n (c:rst) = do
      case (p, isDialogue c) of (True, True) -> do raw between
                                                   reset
                                _ -> return ()
      component p n c
      recGen between (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ [] = return ()

paragraphs :: Monoid a
           => [Ast.Paragraph a]
           -> Generator a
paragraphs (h:r) = do paragraph h
                      reset
                      paragraphs r
paragraphs [] = return ()

section :: Monoid a
        => Ast.Section a
        -> Generator a
section (Ast.Story ps) = do temp <- storyTemplate <$> ask

                            apply temp (paragraphs ps)
section (Ast.Aside ps) = do temp <- asideTemplate <$> ask

                            apply temp (paragraphs ps)

sections :: Monoid a
         => [Ast.Section a]
         -> Generator a
sections (s:r) = do section s
                    sections r
sections [] = return ()

document :: Monoid a
          => Ast.Document a
         -> Generator a
document d = do temp <- documentTemplate <$> ask

                apply temp (sections d)

generate :: Monoid a
         => GenConf a
         -> Ast.Document a
         -> a

generate conf lst = fst $ runReader (execStateT (document lst) (mempty, Nothing)) conf
