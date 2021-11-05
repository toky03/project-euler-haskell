import Data.List(nub)

divisors a = (1:) $ nub $ concat [[x, div a x] | x <- [2..limit], rem a x == 0]
    where limit = (floor.sqrt.fromIntegral) a


triangular x = sum [1..x]

triangulars :: Integral a =>  [a]
triangulars = [triangular x | x <- [1..]]

takeInclusiveWhile:: (a -> Bool) -> [a] -> [a]
takeInclusiveWhile _ [] = []
takeInclusiveWhile p (x:xs) = x: if p x then takeInclusiveWhile p xs else []

highestTriangular :: Integral a => Int -> [a]
highestTriangular div = takeInclusiveWhile (\x -> length (divisors x) < div) triangulars

main = do
    print $ maximum $ highestTriangular 500