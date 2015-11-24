import Data.Int
import Data.List
import Data.Numbers.Fibonacci

modulus :: Integral a => a
modulus = 10^9

newtype ModP = ModP Int64 deriving (Show, Eq)
instance Num ModP where
    ModP a + ModP b = ModP $ (a + b) `rem` modulus
    ModP a - ModP b = ModP $ (a - b) `rem` modulus
    ModP a * ModP b = ModP $ (a * b) `rem` modulus
    fromInteger = ModP . fromIntegral . (`rem` modulus)

main = print . head $ dropWhile (\x -> not (checkLast (fib x) && checkFirst (fib x))) [0..]
  where
    checkLast (ModP x) = (==['1'..'9']) . sort . show $ x
    checkFirst = (==['1'..'9']) . sort . take 9 . show
