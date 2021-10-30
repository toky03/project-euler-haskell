triplet :: Integer
triplet = triplet' 1 2 3
    where triplet' a b c | isValid a b c = a*b*c
                         | c == 1000 = triplet' a (b+1) (b+2)
                         | b == 1000 = triplet' (a+1) (a+2) (a+3)
                         | otherwise = triplet' a b (c+1)

isValid :: (Num a, Eq a) => a -> a -> a -> Bool
isValid a b c = (a+b+c) == 1000 && (a^2+b^2)==(c^2)

main :: IO ()
main = do 
    print triplet