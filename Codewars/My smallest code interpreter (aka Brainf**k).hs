{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Brainfuck
    ( executeString
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Data.Word
import Text.Parse

data Token = Plus | Minus | Leftward | Rightward | Input | Output | Loop [Token]
           deriving (Show)
type Zipper = (String,[Word8],[Word8])
type Program = RWST () String Zipper Maybe

char = literal . (:[])

program :: Parser Char [Token]
program = many $ Plus <$ char '+' <|> Minus <$ char '-' <|>
          Leftward <$ char '<' <|> Rightward <$ char '>' <|>
          Input <$ char ',' <|> Output <$ char '.' <|>
          Loop <$> bracket (char '[') (char ']') program

execute :: [Token] -> Program ()
execute = mapM_ eval

eval :: Token -> Program ()
eval Plus = modify $ \(is,xs,x:ys) -> (is,xs,x+1:ys)
eval Minus = modify $ \(is,xs,x:ys) -> (is,xs,x-1:ys)
eval Leftward = modify $ \(is,x:xs,ys) -> (is,xs,x:ys)
eval Rightward = modify $ \(is,xs,x:ys) -> (is,x:xs,ys)
eval Input =
  get >>= \case ([],_,_) -> lift Nothing
                (i:is,ys,_:xs) -> put (is,ys,fromIntegral (fromEnum i):xs)
eval Output = get >>= \(_,_,x:_) -> tell [toEnum (fromIntegral x)]
eval (Loop p) = get >>= \(_,_,c:_) -> when (c /= 0) (execute p >> eval (Loop p))

-- | Interprets the Brainfuck source code from the first argument, while
-- supplying it with input from the second. May fail on insufficient input.
executeString :: String -> String -> Maybe String
executeString source input =
  case readByParse program source of
       [] -> error "parse"
       [(p, _)] -> case evalRWST (execute p) () (input, repeat 0, repeat 0) of
                        Nothing -> Nothing
                        Just ((), w) -> Just w
