fib :: Int -> Int 
fib n = fib' 0 1 n
    where fib' a b n | n <= 1 = b
                     | otherwise = fib' b (a+b) (n-1)

fibSequence = takeWhile (<4000000) [ fib x |x <- [1..]] 
evenFibSequence = [i |i <- fibSequence , even i]

sumSeq = sum evenFibSequence
main = do
    print sumSeq