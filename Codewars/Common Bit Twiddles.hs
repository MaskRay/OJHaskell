module BitTwiddles where
import Prelude hiding ((*), (/), (^), (&&), (||), fromIntegral, fromInteger, mod, div, quot, rem, quotRem, divMod, (>), (<), compare, Ordering(..), abs, even, odd)
import Data.Bits hiding (popCount)
import Data.Int (Int32)

isEven :: Bits a => a -> Bool
isEven = not . flip testBit 0

isOdd :: Bits a => a -> Bool
isOdd = flip testBit 0

halfAndFloor :: Bits a => a -> a
halfAndFloor = (`shiftR` 1)

isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo x = fromEnum (x /= 0) + fromEnum (x .&. (x-1) == 0) == 2

nthPowerOfTwo :: (Num a, Bits a) => Int -> a
nthPowerOfTwo = (1 `shiftR`)

abs :: Int32 -> Int32
abs x = (x+y) `xor` y
  where
    y = x `shiftR` 31
