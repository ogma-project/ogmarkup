{-# LANGUAGE OverloadedStrings #-}

module Ogmarkup.Private.Generator where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Text (Text, append)

import qualified Ogmarkup.Private.Ast as Ast
import Ogmarkup.Config

type Generator = StateT (Text, Maybe Ast.Atom) (Reader GenConf) ()

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

  genRawText $ open tag
  genCollections cs
  genRawText $ close tag
genFormat (Ast.StrongEmph cs) = do
  tag <- strongEmphTag <$> ask

  genRawText $ open tag
  genCollections cs
  genRawText $ close tag

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
  genRawText $ open tag
  genFormats d
  genRawText $ close tag
  genMaybeAtom end
genReply begin end (Ast.WithSay d ws d') = do
  tag <- replyTag <$> ask

  genMaybeAtom begin
  genRawText $ open tag
  genFormats d
  genRawText $ close tag

  case d' of [] -> do
               genMaybeAtom end
               genFormats ws
             l -> do
               genFormats ws
               genRawText $ open tag
               genFormats d'
               genRawText $ close tag
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

  genRawText $ open tag
  genReply (prevT p) (nextT n) d
  genRawText $ close tag
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

  genRawText $ open tag
  genReply Nothing Nothing d
  genRawText $ close tag
genComponent p n (Ast.Teller fs) = do
  genFormats fs

genParagraph :: [Ast.Component]
             -> Generator
genParagraph (h:r) = do
  tag <- paragraphTag <$> ask

  genRawText $ open tag
  recGen (open tag) (close tag) False (willBeDialogue r) (h:r)
  genRawText $ close tag

  where
    isDialogue (Ast.Dialogue _ _) = True
    isDialogue _ = False

    willBeDialogue (h:n:r) = isDialogue n
    willBeDialogue _ = False

    recGen :: Text
           -> Text
           -> Bool
           -> Bool
           -> [Ast.Component]
           -> Generator
    recGen beg end p n (c:rst) = do
      case (p, isDialogue c) of (True, True) -> do genRawText (end `append` beg)
                                                   genResetPrev
                                _ -> return ()
      genComponent p n c
      recGen beg end (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ _ [] = return ()

genParagraphs :: [Ast.Paragraph] -> Generator
genParagraphs (h:r) = do genParagraph h
                         genResetPrev
                         genParagraphs r
genParagraphs [] = return ()

genSection :: Ast.Section -> Generator
genSection (Ast.Story ps) = do tag <- storyTag <$> ask

                               genRawText $ open tag
                               genParagraphs ps
                               genRawText $ close tag
genSection (Ast.Aside ps) = do tag <- asideTag <$> ask

                               genRawText $ open tag
                               genParagraphs ps
                               genRawText $ close tag

genSections :: [Ast.Section] -> Generator
genSections (s:r) = do genSection s
                       genSections r
genSections [] = return ()

genDocument :: Ast.Document -> Generator
genDocument d = do tag <- documentTag <$> ask

                   genRawText $ open tag
                   genSections d
                   genRawText $ close tag

generate :: GenConf
         -> Ast.Document
         -> Text

generate conf lst = fst $ runReader (execStateT (genDocument lst) ("", Nothing)) conf
