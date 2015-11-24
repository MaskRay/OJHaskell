import Control.Monad
import Data.Char
main=interact$ap((:).toUpper.head)tail
