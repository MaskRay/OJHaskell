import Control.Applicative
import Control.Monad.Reader
import Data.Char
import Data.List
import Data.Ratio
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Text.Printf

-- Polynomial
instance (Eq a, Num a) => Num [a] where
   (f:fs) + (g:gs) = trim $ f+g : fs+gs
   fs + [] = fs
   [] + gs = gs
   (f:fs) * (g:gs) = trim $ f*g : [f]*gs + fs*(g:gs)
   _ * _ = []
   abs = undefined
   signum = map signum
   fromInteger n = [fromInteger n]
   negate = map (\x -> -x)

trim :: (Eq a, Num a) => [a] -> [a]
trim xs =
  let ys = dropWhile (0==) (reverse xs) in
  if null ys
  then [0]
  else reverse ys

data Expr = Var | Con Rational | Bin Binop Expr Expr deriving Show
data Binop = Plus | Minus | Mul | Div | Pow deriving Show

expr = chainl1 pterms plusOp
pterms = char '+' *> terms <|> Bin Mul (Con (-1)) <$ char '-' <*> terms <|> terms
terms = foldl1' (Bin Mul) <$> many1 term
term = chainl1 factor mulOp
factor = chainr1 block (Bin Pow <$ char '^')
block = between (char '(') (char ')') expr <|> (Var <$ char 'x') <|> ((Con . fromIntegral . read) <$> many1 digit)

plusOp = Bin Plus <$ char '+' <|> Bin Minus <$ char '-'
mulOp = Bin Mul <$ char '*' <|> Bin Div <$ char '/'

eval :: Expr -> [Rational]
eval Var = [0, 1]
eval (Con v) = [v]
eval (Bin op x y) = do
  let rx = eval x
      ry = eval y
  case op of
    Plus -> rx + ry
    Minus -> rx - ry
    Mul -> rx * ry
    Div -> map (/ head ry) rx
    Pow -> rx ^ numerator (head ry)

f a n = when (a /= 0) $ do
  if a > 0
      then putStr " + "
      else putStr " - "
  when (abs a /= 1 || n == 0) $ printf "%d" (abs a)
  when (n > 0) $ do
    putChar 'x'
    when (n > 1) $ printf "^%d" n

output (x:xs) = do
  let n = length xs
  if n > 0
    then do
      when (x /= 1) $
         if x == -1
           then putChar '-'
           else printf "%d" x
      putChar 'x'
      when (n > 1) $ printf "^%d" n
    else
      printf "%d" x
  zipWithM_ f xs [n-1,n-2..0]

main = do
  n <- readLn
  replicateM_ n $ do
    line <- filter (not . isSpace) <$> getLine
    case parse expr "" line of
      Left err -> print err
      Right e -> do
        output . map numerator . reverse $ eval e
        putChar '\n'
