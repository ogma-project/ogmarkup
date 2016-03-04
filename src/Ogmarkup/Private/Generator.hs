{-# LANGUAGE OverloadedStrings #-}

module Ogmarkup.Private.Generator where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Text (Text, append)

import qualified Ogmarkup.Private.Ast as Ast
import Ogmarkup.Typography
import Ogmarkup.Config

type Generator = StateT (Text, Maybe Ast.Atom) (Reader GenConf) ()

genApply :: (Text -> Text) -> Generator -> Generator
genApply app gen = do
  (str, maybe) <- get
  put ("", maybe)
  gen
  (str', maybe') <- get
  put (str `append` (app str'), maybe')

genResetPrev :: Generator
genResetPrev = do
  (str, _) <- get
  put (str, Nothing)

genRawText :: Text
           -> Generator
genRawText str' = do
  (str, maybePrev) <- get
  put (str `append` str', maybePrev)

genAtom :: Ast.Atom
        -> Generator
genAtom text = do
  (str, maybePrev) <- get
  typo <- (typography <$> ask)
  ptrSpace <- (printSpace <$> ask)

  case maybePrev of
    Just prev ->
      let str' = (ptrSpace $ max (afterAtom typo prev) (beforeAtom typo text)) `append` (normalizeAtom typo text) in
        put (str `append` str', Just text)
    Nothing -> put (str `append` normalizeAtom typo text, Just text)

genMaybeAtom :: Maybe Ast.Atom
             -> Generator
genMaybeAtom (Just text) = genAtom text
genMaybeAtom Nothing = return ()

genAtoms :: [Ast.Atom]
         -> Generator
genAtoms (f:rst) = do
  genAtom f
  genAtoms rst
genAtoms [] = return ()

genCollection :: Ast.Collection
              -> Generator
genCollection (Ast.Quote atoms) = do genAtom (Ast.Punctuation Ast.OpenQuote)
                                     genAtoms atoms
                                     genAtom (Ast.Punctuation Ast.CloseQuote)
genCollection (Ast.Text atoms) = genAtoms atoms

genCollections :: [Ast.Collection]
               -> Generator
genCollections (f:rst) = do genCollection f
                            genCollections rst
genCollections [] = return ()

genFormat :: Ast.Format
          -> Generator
genFormat (Ast.Raw cs) = genCollections cs
genFormat (Ast.Emph cs) = do
  tag <- emphTag <$> ask

  genApply tag (genCollections cs)
genFormat (Ast.StrongEmph cs) = do
  tag <- strongEmphTag <$> ask

  genApply tag (genCollections cs)

genFormats :: [Ast.Format]
           -> Generator
genFormats (f:rst) = do
  genFormat f
  genFormats rst
genFormats [] = return ()

genReply :: Maybe Ast.Atom
         -> Maybe Ast.Atom
         -> Ast.Reply
         -> Generator
genReply begin end (Ast.Simple d) = do
  tag <- replyTag <$> ask

  genMaybeAtom begin
  genApply tag (genFormats d)
  genMaybeAtom end
genReply begin end (Ast.WithSay d ws d') = do
  tag <- replyTag <$> ask

  genMaybeAtom begin
  genApply tag (genFormats d)

  case d' of [] -> do
               genMaybeAtom end
               genFormats ws
             l -> do
               genFormats ws
               genApply tag (genFormats d')
               genMaybeAtom end

genComponent :: Bool           -- ^ Was the last component an audible dialog?
             -> Bool           -- ^ Will the next component be an audible dialog?
             -> Ast.Component  -- ^ The current to process.
             -> Generator
genComponent p n (Ast.Dialogue d a) = do
  conf <- ask

  let 
    auth = authorNormalize conf
    tag = (dialogueTag conf) (auth a)

  genApply tag (genReply (prevT p) (nextT n) d)
  where
    prevT True = Just (Ast.Punctuation Ast.LongDash)
    prevT False = Just (Ast.Punctuation Ast.OpenQuote)

    nextT True = Nothing
    nextT False = Just (Ast.Punctuation Ast.CloseQuote)
genComponent p n (Ast.Thought d a) = do
  conf <- ask

  let 
    auth = authorNormalize conf
    tag = (dialogueTag conf) (auth a)

  genApply tag (genReply Nothing Nothing d)
genComponent p n (Ast.Teller fs) = do
  genFormats fs

genParagraph :: [Ast.Component]
             -> Generator
genParagraph l@(h:r) = do
  tag <- paragraphTag <$> ask
  between <- betweenDialogue <$> ask

  genApply tag (recGen between False (willBeDialogue l) (h:r))

  where
    isDialogue (Ast.Dialogue _ _) = True
    isDialogue _ = False

    willBeDialogue (h:n:r) = isDialogue n
    willBeDialogue _ = False

    recGen :: Text
           -> Bool
           -> Bool
           -> [Ast.Component]
           -> Generator
    recGen between p n (c:rst) = do
      case (p, isDialogue c) of (True, True) -> do genRawText between
                                                   genResetPrev
                                _ -> return ()
      genComponent p n c
      recGen between (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ [] = return ()

genParagraphs :: [Ast.Paragraph] -> Generator
genParagraphs (h:r) = do genParagraph h
                         genResetPrev
                         genParagraphs r
genParagraphs [] = return ()

genSection :: Ast.Section -> Generator
genSection (Ast.Story ps) = do tag <- storyTag <$> ask

                               genApply tag (genParagraphs ps)
genSection (Ast.Aside ps) = do tag <- asideTag <$> ask

                               genApply tag (genParagraphs ps)

genSections :: [Ast.Section] -> Generator
genSections (s:r) = do genSection s
                       genSections r
genSections [] = return ()

genDocument :: Ast.Document -> Generator
genDocument d = do tag <- documentTag <$> ask

                   genApply tag (genSections d)

generate :: GenConf
         -> Ast.Document
         -> Text

generate conf lst = fst $ runReader (execStateT (genDocument lst) ("", Nothing)) conf
