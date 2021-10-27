
ggt:: Integral a => a -> a -> a
ggt 0 _ = 0
ggt _ 0 = 0
ggt a b | a == b = a
        | mod (max a b) (min a b) == 0 = min a b
        | otherwise = ggt (min a b) (rem (max a b) (min a b))



kgv :: Integral a => a -> a -> a
kgv a b = (a * b) `div` ggt a b



kgvMult:: Integral a => a -> [a] -> a
kgvMult = foldl kgv


main = do
    print $ kgvMult 1 [1..20]