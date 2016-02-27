module Ogmarkup.Private.Generator where

import Control.Monad.State.Strict
import Control.Monad.Reader

import qualified Ogmarkup.Private.Ast as Ast
import Ogmarkup.Config

type Generator = StateT (String, Maybe Ast.Atom) (Reader GenConf) ()

genResetPrev :: Generator
genResetPrev = do
  (str, _) <- get
  put (str, Nothing)

genRawString :: String
             -> Generator
genRawString str' = do
  (str, maybePrev) <- get
  put (str ++ str', maybePrev)

genAtom :: Ast.Atom
        -> Generator
genAtom text = do
  (str, maybePrev) <- get
  typo <- (typography <$> ask)
  ptrSpace <- (printSpace <$> ask)

  case maybePrev of
    Just prev ->
      let str' = (ptrSpace $ max (afterAtom typo prev) (beforeAtom typo text)) ++ (normalizeAtom typo text) in
        put (str ++ str', Just text)
    Nothing -> put (str ++ normalizeAtom typo text, Just text)

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
  conf <- ask

  genRawString $ beginWeakEmph conf
  genCollections cs
  genRawString $ endWeakEmph conf
genFormat (Ast.StrongEmph cs) = do
  conf <- ask

  genRawString $ beginStrongEmph conf
  genCollections cs
  genRawString $ endStrongEmph conf

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
  br <- beginReply <$> ask
  er <- endReply <$> ask

  genMaybeAtom begin
  genRawString br
  genFormats d
  genRawString  er
  genMaybeAtom end
genReply begin end (Ast.WithSay d ws d') = do
  br <- beginReply <$> ask
  er <- endReply <$> ask

  genMaybeAtom begin
  genRawString br
  genFormats d
  genRawString er

  case d' of [] -> do
               genMaybeAtom end
               genFormats ws
             l -> do
               genFormats ws
               genRawString br
               genFormats d'
               genRawString er
               genMaybeAtom end

genComponent :: Bool           -- ^ Was the last component an audible dialog?
             -> Bool           -- ^ Will the next component be an audible dialog?
             -> Ast.Component  -- ^ The current to process.
             -> Generator
genComponent p n (Ast.Dialogue d a) = do
  db <- beginDialogue <$> ask
  de <- endDialogue <$> ask
  auth <- authorNormalize <$> ask

  genRawString . db . auth $ a
  genReply (prevT p) (nextT n) d
  genRawString . de . auth $ a
  where
    prevT True = Just (Ast.Punctuation Ast.LongDash)
    prevT False = Just (Ast.Punctuation Ast.OpenQuote)

    nextT True = Nothing
    nextT False = Just (Ast.Punctuation Ast.CloseQuote)
genComponent p n (Ast.Thought d a) = do
  tb <- beginThought <$> ask
  te <- endThought <$> ask
  auth <- authorNormalize <$> ask

  genRawString . tb . auth $ a
  genReply Nothing Nothing d
  genRawString . te . auth $ a
genComponent p n (Ast.Teller fs) = do
  genFormats fs

genParagraph :: [Ast.Component]
             -> Generator
genParagraph (h:r) = do
  begin <- beginParagraph <$> ask
  end <- endParagraph <$> ask

  genRawString begin
  recGen begin end False (willBeDialogue r) (h:r)
  genRawString end

  where
    isDialogue (Ast.Dialogue _ _) = True
    isDialogue _ = False

    willBeDialogue (h:n:r) = isDialogue n
    willBeDialogue _ = False

    recGen :: String
           -> String
           -> Bool
           -> Bool
           -> [Ast.Component]
           -> Generator
    recGen beg end p n (c:rst) = do
      case (p, isDialogue c) of (True, True) -> do genRawString (end ++ beg)
                                                   genResetPrev
                                _ -> return ()
      genComponent p n c
      recGen beg end (isDialogue c) (willBeDialogue rst) rst
    recGen _ _ _ _ [] = return ()

genParagraphs :: [Ast.Paragraph] -> Generator
genParagraphs (h:r) = do genParagraph h
                         genParagraphs r
genParagraphs [] = return ()

genSection :: Ast.Section -> Generator
genSection (Ast.Story ps) = do genRawString "<div>"
                               genParagraphs ps
                               genRawString "</div>"
genSection (Ast.Aside ps) = do genRawString "<blockquote>"
                               genParagraphs ps
                               genRawString "</blockquote>"

genDocument :: [Ast.Section] -> Generator
genDocument (s:r) = do genSection s
                       genDocument r
genDocument [] = return ()

generate :: GenConf
         -> Ast.Document
         -> String

generate conf lst = fst $ runReader (execStateT (genDocument lst) ("", Nothing)) conf
