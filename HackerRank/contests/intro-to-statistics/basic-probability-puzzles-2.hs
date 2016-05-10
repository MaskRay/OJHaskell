import Control.Monad
import Data.Ratio
import Text.Printf

main =
  let t = (%36) . length $ [1 | x<-[1..6], y<-[1..6], x /= y, x+y == 6] in
  printf "%d/%d\n" (numerator t) (denominator t)
