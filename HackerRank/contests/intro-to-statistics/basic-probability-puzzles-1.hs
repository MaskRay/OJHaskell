import Control.Monad
import Data.Ratio
import Text.Printf

main =
  let t = (%36) . length . filter (<=9) . map sum $ replicateM 2 [1..3] in
  printf "%d/%d\n" (numerator t) (denominator t)
