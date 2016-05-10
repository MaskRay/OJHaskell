import Control.Monad
import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [sum ys * c, sum (map ((pi*) . join (*)) ys) * c]
  where
    l' = fromIntegral l :: Double
    r' = fromIntegral r
    xs = [l',l'+0.001..r']
    c = (r'-l') / fromIntegral (length xs) :: Double
    ys = map (\x -> sum $ zipWith (\a b -> fromIntegral a * x ^^ b) a b) xs :: [Double]


--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
