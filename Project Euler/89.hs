replace "" _ = ""
replace xs (s,s')
        | lxs == s = s'++rxs
        | otherwise = (head xs) : replace (tail xs) (s,s')
        where (lxs,rxs) = splitAt (length s) xs

main = print . diff . words =<< readFile "roman.txt"
     where diff s = sum (map length s) - sum (map (length . reduce) s)
           reduce s = foldl replace s [("DCCCC", "CM"), ("CCCC", "CD"),
                                        ("LXXXX", "XC"), ("XXXX", "XL"),
                                        ("VIIII", "IX"), ("IIII", "IV")]


