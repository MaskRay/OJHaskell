import Control.Monad
import Data.Ratio
import Text.Printf

f = fromEnum

main =
  let t = (%(7*9*8)) . length $ [1 | x<-[0..6], y<-[0..8], z<-[0..7], f(x<4)+f(y<5)+f(z<4) == 2] in
  printf "%d/%d\n" (numerator t) (denominator t)
