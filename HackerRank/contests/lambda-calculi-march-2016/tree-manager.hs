{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S

data Op = Print | Change Int | VisitL | VisitR | VisitP | VisitC Int | InsertL Int | InsertR Int | InsertC Int | Delete
data Tree a = Tree a (S.Seq (Tree a)) deriving (Eq, Read, Show)
type Forest a = S.Seq (Tree a)

data Zipper a = Zipper {
  at :: Tree a,
  left :: Forest a,
  right :: Forest a,
  ancestors :: [(Forest a, a, Forest a)]
  } deriving (Eq, Read, Show)

singleton x = Tree x S.empty

parse =  do
  n <- int
  (n,) <$> replicateM n readOp
  where
    int = state $ fromJust . B.readInt . B.dropWhile isSpace
    str = state $ B.span (not . isSpace) . B.dropWhile isSpace
    readOp = do
      op <- str
      case op of
        "change" -> Change <$> int
        "print" -> return Print
        "visit" -> do
          op2 <- str
          case op2 of
            "left" -> return VisitL
            "right" -> return VisitR
            "parent" -> return VisitP
            "child" -> VisitC <$> int
        "insert" -> do
          op2 <- str
          case op2 of
            "left" -> InsertL <$> int
            "right" -> InsertR <$> int
            "child" -> InsertC <$> int
        "delete" -> return Delete

main = do
  (n, ops) <- evalState parse <$> B.getContents
  let initial = Zipper{at = singleton 0, left = S.empty, right = S.empty, ancestors = []}
  flip (`foldM_` initial) ops $ \z@Zipper{..} op ->
    case op of
      Print ->
        case at of
          Tree x _ -> z <$ print x
      Change val ->
        case at of
          Tree _ xs -> return z{at = Tree val xs}
      VisitL ->
        case S.viewr left of
          ls S.:> l -> return z{at = l, left = ls, right = at S.<| right}
      VisitR ->
        case S.viewl right of
          r S.:< rs -> return z{at = r, left = left S.|> at, right = rs}
      VisitP ->
        case ancestors of
          (ls,val,rs):ancs -> return z{at = Tree val ((left S.|> at) S.>< right), left = ls, right = rs, ancestors = ancs}
      VisitC i ->
        case at of
          Tree val xs ->
            let (ls, rs') = S.splitAt (i-1) xs
                y S.:< rs = S.viewl rs' in
            return z{at = y, left = ls, right = rs, ancestors = (left, val, right):ancestors}
      InsertL val ->
        return z{left = left S.|> singleton val}
      InsertR val ->
        return z{right = singleton val S.<| right}
      InsertC val ->
        case at of
          Tree x xs -> return z{at = Tree x (singleton val S.<| xs)}
      Delete ->
        case ancestors of
          (ls,val,rs):ancs ->
            return z{at = Tree val (left S.>< right), left = ls, right = rs, ancestors = ancs}
