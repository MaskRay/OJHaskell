import Data.Array

gen 0 = [[]]
gen 1 = []
gen 2 = [[2]]
gen l = fmap (l:) $ concat [gen $ l-2, gen $ l-3]

transferable _ [] = True
transferable [] _ = True
transferable (x:xs) (y:ys) = case x `compare` y of
             EQ -> False
             LT -> transferable (x:xs) ys
             GT -> transferable xs (y:ys)

prob215 m n = (\arr -> sum [arr ! i | i <- range $ bounds arr]) $
        (!! (n-1)) $
        iterate (\pre -> listArray (bounds statesA)
        [sum [pre ! j | j <- transferableA ! i] | i <- range $ bounds pre]) $ listArray (bounds statesA) [1,1..]

        where states = fmap tail $ gen m
              statesA = listArray (1,length states) states
              transferableA = listArray (bounds statesA) [[j | j <- range $ bounds statesA, transferable (statesA ! i) (statesA ! j)] | i <- range $ bounds statesA]

main = print $ prob215 32 10