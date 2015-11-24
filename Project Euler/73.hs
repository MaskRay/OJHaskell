import Data.Ratio

maxn = 12000

traverse a b c d
         | b > maxn || d > maxn || c%d <= 1%3 || 1%2 <= a%b = 0
         | otherwise = traverse a b m n + traverse m n c d + fromEnum (1%3 < m%n && m%n < 1%2 && n <= maxn)
         where m = a+c
               n = b+d

main = print $ traverse 0 1 1 1