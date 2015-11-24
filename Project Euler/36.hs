import Data.List

isPalindrome x = x == reverse x

showBin = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2))

main = print . sum $ [x | x <- [1..999999], isPalindrome $ show x, isPalindrome $ showBin x]