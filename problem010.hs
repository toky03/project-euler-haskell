import PrimeHelper(primes)

sievePrime :: Integral a => a -> [a]
sievePrime n = [x | x <- [2..n], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]

primesBelowTwoMillion :: [Integer]
primesBelowTwoMillion = [x | x <- sievePrime 2000000]

main :: IO ()
main = do
    print $ sum primesBelowTwoMillion