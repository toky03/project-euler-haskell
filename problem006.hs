
squaredSum:: Integral a => [a] -> a
squaredSum a = sum a ^ 2


sumSquared:: Integral a => [a] -> a
sumSquared = foldl (\x y-> x + y ^2) 0


main = do
    print $ squaredSum [1..100] - sumSquared [1..100]