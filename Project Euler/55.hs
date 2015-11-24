lychrel n = loop n 1
        where loop n k
                   | k > 50 = True
              loop n k = let n' = n + (read . reverse . show)  n
                         in if isPalindrome n'
                            then False
                            else loop n' (k+1)
              isPalindrome n = show n == reverse (show n)

main = print . length . filter lychrel $ [1..9999]