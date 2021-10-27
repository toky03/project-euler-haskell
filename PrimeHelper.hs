module PrimeHelper(
    isPrime,
    primes,
    primeFactors
) where

isPrime :: Integral t => t -> Bool
isPrime n = isPrime' n 2
    where isPrime' a b | a <= 2 = a == 2
                       | mod a b == 0 = False
                       | b * b > a = True
                       | otherwise = isPrime' a (b+1)

primes = [x | x <- [2..], isPrime x]


primeFactors n = primeFactors' n 0 []
    where primeFactors' x index factors | isPrime x = factors ++ [x]
                              | mod x (primes!!index) == 0 = primeFactors' (reminder x index) index (factors ++ [primes!!index])
                              | otherwise = primeFactors' x (index + 1) factors
                                        where reminder a b = a `div` primes!!b