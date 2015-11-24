{-# LANGUAGE TupleSections #-}

import Control.Monad
import Control.Monad.Instances
import Data.List

check = and . ap (zipWith (<)) tail . map snd . sort . map (liftM2 (,) length sum) . subsequences

p105 = sum . concat . filter check . map (read . ('[':) . (++"]")) . lines

main = readFile "sets.txt" >>= print . p105
