import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Maybe
import qualified Data.Set as S
import Text.Printf
import qualified Data.ByteString.Char8 as B

main = do
  cases <- readLn
  let vowel = S.fromList "aeiou"
  replicateM_ cases $ do
    a <- B.getLine
    let b = B.reverse $ B.filter (`S.notMember` vowel) a
        f i j | i >= B.length a = return ()
        f i j =
          if S.member c vowel
          then putChar c >> f (i+1) j
          else putChar (B.index b j) >> f (i+1) (j+1)
          where
            c = B.index a i
    f 0 0
    putChar '\n'
