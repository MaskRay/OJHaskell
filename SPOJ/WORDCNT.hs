import Data.List

main = mapM_ (const $ getLine >>= print . maximum . map length . groupBy (==) . map length . words) . enumFromTo 1 =<< fmap read getLine