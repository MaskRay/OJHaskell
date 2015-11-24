f 1 a b = 1 + min a b
f n 0 b = g!!(n-1)!!b!!9
f n a b = g!!n!!(a-1)!!b + g!!(n-1)!!(max (b-a) 0)!!(9-a)

g = [] : [[[f n a b | b <- [0..9]] | a <- [0..9]] | n <- [1..20]]

main = print $ f 20 9 9 - f 20 0 9