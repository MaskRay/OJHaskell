import Data.Char

main = print . sum . map digitToInt . show . snd . foldl (\(p',p) a -> (p,p*a+p')) (1,2) . take 99 $ concat [[1,2*i,1] | i<-[1..]]