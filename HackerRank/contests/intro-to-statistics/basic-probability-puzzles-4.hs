import Control.Monad
import Data.Ratio
import Text.Printf

f = fromEnum

main =
  let t = (%(9*10*9`div`2)) . length $ [1 | x<-[0..8], y<-[0..9], z<-[y+1..9], f(x<4)+f(y<3)+f(z<3) == 1] in
  printf "%d/%d\n" (numerator t) (denominator t)
