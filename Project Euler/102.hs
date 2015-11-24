check [x1,y1,x2,y2,x3,y3] = oab*abc >= 0 && obc*abc >= 0 && oca*abc >= 0
  where
    o = (0,0)
    a = (x1,y1)
    b = (x2,y2)
    c = (x3,y3)
    abc = area a b c
    oab = area o a b
    obc = area o b c
    oca = area o c a
    area (x0,y0) (x1,y1) (x2,y2) = (x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)

p102 = length . filter check . map (\l -> read $ "["++l++"]") . lines

main = readFile "triangles.txt" >>= print . p102
