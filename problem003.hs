import PrimeHelper ( primeFactors )


startNumber :: Integer
startNumber = 600851475143

main :: IO ()
main = do
    print . maximum $ primeFactors startNumber
