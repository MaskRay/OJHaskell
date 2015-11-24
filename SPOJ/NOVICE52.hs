import Data.List

main = getContents >>= mapM_ (putStrLn . (["Akash","Aayush"]!!) . (`mod`2) . sum . map (pred . length) . group) . tail . lines
