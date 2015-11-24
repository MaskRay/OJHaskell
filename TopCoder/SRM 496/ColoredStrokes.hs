import Text.Regex
import Data.List

getLeast :: [String] -> Int
getLeast s = f (fmap (\x -> subRegex (mkRegex "G|R") x "1") s) +
  f (fmap (\x -> subRegex (mkRegex "G|B") x "1") $ transpose s)
  where f = sum . fmap (length . filter ((=='1') . head) . group)

main = do
  s <- getContents
  print $ getLeast $ tail $ lines s