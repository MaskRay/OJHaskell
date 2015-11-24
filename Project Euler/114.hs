l = replicate 3 1 ++ [(+ l!!(n-1)) $ succ $ sum $ take (n-3) l | n <- [3..]]
main = print $ l !! 50