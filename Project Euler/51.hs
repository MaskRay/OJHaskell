import Data.List

isPrime n = n >= 2 && loop primes n
        where loop ps@(p:ps') n
                   | p*p > n = True
                   | n `mod` p == 0 = False
                   | otherwise = loop ps' n
primes = 2 : filter isPrime [3,5..]

f n = maximum . map (\d -> sum [if isPrime (replace n d d') then 1 else 0 | d' <- ['1'..'9']]) $ nub . show $ n
replace n d d' = read . map (\x -> if x == d then d' else x) $ show n

main = print . head . dropWhile (< 100000) . filter (\n -> f n == 8) $ primes