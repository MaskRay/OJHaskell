import Data.Char

main = do
     getContents >>= return . filter (/= '\n') >>= print . maximum . map product. groupsOf 5 . map digitToInt

groupsOf _ [] = []
groupsOf n xs = 
         take n xs : groupsOf n (tail xs)