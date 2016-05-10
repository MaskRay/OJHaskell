module TitleCase (titleCase) where

import Data.Char
import qualified Data.Set as S

capitalize (c:cs) = toUpper c : map toLower cs

titleCase :: String -> String -> String
titleCase minor title =
  let s = S.fromList . words . map toLower $ minor in
  case unwords $ map (\x -> if S.member (map toLower x) s then map toLower x else capitalize x) $ words title of
       "" -> ""
       c:cs -> toUpper c : cs
