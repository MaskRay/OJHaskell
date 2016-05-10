{-# LANGUAGE Safe #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, RecordWildCards, RecursiveDo #-}
import Control.Applicative
import Control.Monad.Identity hiding (forM_)
import Control.Monad.State.Strict hiding (forM_)
import Data.Array.IO.Safe
import Data.Foldable
import Data.Functor
import Data.Int
import Data.List hiding (foldl')
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Text.Printf

-- | Backport

modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do
    s <- get
    put $! f s

a & f = f a
infixl 1 &

-- | Lens

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
_1 f (a, b) = (\a' -> (a', b)) <$> f a
_2 f (a, b) = (\b' -> (a, b')) <$> f b

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)
infixr 4 %~

(.~) :: ASetter s t a b -> b -> s -> t
l .~ b = l %~ const b
infixr 4 .~

(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ a = l %~ (+a)
infixr 4 +~

type Getting r s a = (a -> Const r a) -> s -> Const r s
(^.) :: s -> Getting a s a -> a
x ^. l = getConst $ l Const x
infixl 8 ^.

-- | Suffix automaton (multi-string)

data S = S {_no, _len :: !Int, _link :: Int, _next :: M.Map Int Int} deriving (Show)
no f s = (\a -> s {_no = a}) <$> f (_no s)
len f s = (\a -> s {_len = a}) <$> f (_len s)
link f s = (\a -> s {_link = a}) <$> f (_link s)
next f s = (\a -> s {_next = a}) <$> f (_next s)

emptyS = S {_no = 0, _len = 0, _link = -1, _next = M.empty}

insertS c p s = s {_next = M.insert c p (s ^. next)}

data SuffixAutomaton = SuffixAutomaton {_allo :: !Int, _lastS :: S, _states :: IOArray Int S}
allo f s = (\a -> s {_allo = a}) <$> f (_allo s)
lastS f s = (\a -> s {_lastS = a}) <$> f (_lastS s)
states f s = (\a -> s {_states = a}) <$> f (_states s)

root sam = liftIO $ readArray (sam^.states) 0

type MS = (SuffixAutomaton, Int64)

insertSAM :: (Functor m, MonadIO m, MonadState MS m) => S -> m ()
insertSAM p = do
  sam <- (^. _1) <$> get
  liftIO $ writeArray (sam^.states) (p^.no) p

computeLink :: (Functor m, MonadIO m, MonadState MS m) => S -> Int -> m S
computeLink p c = do
  sam <- (^. _1) <$> get
  let sts = sam^.states
  q <- liftIO $ readArray sts ((p^.next) M.! c)
  if p^.len + 1 == q^.len
     then return q
     else do
       let r = q & no .~ sam^.allo & len .~ p^.len + 1
           go p
             | Just t <- M.lookup c (p^.next) =
               when (t == q^.no) $ do
                 insertSAM $ insertS c (r^.no) p
                 case p^.link of
                      -1 -> return ()
                      p' -> liftIO (readArray sts p') >>= go
             | otherwise = return ()
       modify' . (_1 %~) $ \sam -> sam & allo +~ 1
       insertSAM r
       insertSAM $ q & link .~ r^.no
       go p
       return r

add :: (Functor m, MonadIO m, MonadFix m, MonadState MS m) => Int -> m ()
add c = do
  sam <- (^. _1) <$> get
  let p = sam^.lastS
  if M.member c (p^.next)
    then do
      r <- computeLink p c
      modify' . (_1 %~) $ \sam -> sam & lastS .~ r
    else mdo
      let r = S {_no = sam^.allo, _len = p^.len + 1, _link = rl^.no, _next = M.empty}
      modify' . (_1 %~) $ \sam -> sam {_allo = sam^.allo + 1, _lastS = r}
      insertSAM r
      let go p
            | M.notMember c (p^.next) = do
                insertSAM $ insertS c (r^.no) p
                case p^.link of
                    -1 -> return Nothing
                    p' -> liftIO (readArray (sam^.states) p') >>= go
            | otherwise = return $ Just p
      q <- go p
      rl <- maybe (root sam) (`computeLink` c) q
      modify' $ _2 %~ (+ (fromIntegral $ r^.len - rl^.len))

end :: (Functor m, MonadIO m, MonadState MS m) => m Int
end = do
  sam <- (^. _1) <$> get
  rt <- root sam
  modify' $ (_1 . lastS) .~ rt
  return $ sam^.lastS^.no

main = do
  n <- (fst . fromJust . B.readInt) <$> B.getLine
  a <- B.getLine
  child <- newArray (0, n) (-1) :: IO (IOUArray Int Int)
  sibling <- newArray_ (0, n-1) :: IO (IOUArray Int Int)
  char <- newArray_ (0, n-1) :: IO (IOUArray Int Int)
  sts <- newArray (0, 2*n-2) emptyS :: IO (IOArray Int S)
  let emptySAM = SuffixAutomaton {_allo = 1, _lastS = emptyS, _states = sts}
  let f o i
       | i == n = return o
       | B.index a i == '(' =
           f (o+1) (i+1)
       | otherwise = do
           addSubs o
           e <- end
           when (o > 0) . lift $ do
             pos <- readArray child $ o-1
             writeArray child (o-1) i
             writeArray char i e
             writeArray sibling i pos
           f (max 0 $ o-1) (i+1)
      g pos
       | pos < 0 = return ()
       | otherwise = do
           c <- lift $ readArray char pos
           add c
           pos' <- lift $ readArray sibling pos
           g pos'
      addSubs o = do
        pos <- lift $ readArray child o
        lift $ writeArray child o (-1)
        g pos
  (sam, ans) <- flip execStateT (emptySAM, 0) $ do
    o <- f 0 0
    forM_ [o,o-1..0] $ \o -> do
      addSubs o
      void end
  print ans
