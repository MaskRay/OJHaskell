import Control.Monad
import Data.List

main = print . length . group . sort . liftM2 (^) [2..100] $ [2..100]

