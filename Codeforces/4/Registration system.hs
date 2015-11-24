import Control.Monad
import qualified Data.Map as M

main = do
     getLine
     foldM_ (\mapping name -> do
            let res = M.lookup name mapping
            if res == Nothing
               then putStrLn "OK" >> return (M.insert name 0 mapping)
               else let Just res' = res
                    in putStrLn (name++show (res'+1)) >> return (M.insert name (res'+1) mapping)
            ) M.empty =<< return . lines =<< getContents