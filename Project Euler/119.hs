import Data.Char
import Data.List

main = print $ (!!29) $ nub $ sort [a^b | a <- [2..200], b <- [2..20],
     (==a) $ sum  $ map (fromIntegral . digitToInt) $ show (a^b)]