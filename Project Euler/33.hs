import Data.Ratio

main = print . denominator . product $ [x%z | x <- [1..9], y <- [1..9], z <- [1..9], 9*x*z+y*z == 10*x*y]