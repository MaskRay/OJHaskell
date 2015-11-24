import Data.Char

small = ["", "one", "two", "three", "four", "five", "six", "seven",
      "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

ty = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
     "eighty", "ninety"]

decompose x
        | x < 20 = small !! x
        | x < 100 = ty !! (x `div` 10) ++ small !! (x `mod` 10)
        | x == 1000 = "onethousand"
        | x `mod` 100 == 0 = small !! (x `div` 100) ++ "hundred"
        | otherwise = small !! (x `div` 100) ++ "hundredand" ++ decompose (x `mod` 100)

main = print $ length $ concatMap decompose [1..1000]
