
reverseDigits:: Integer -> Integer
reverseDigits a = revDig' a 0
    where revDig' x b | x == 0 = b
                      | otherwise = revDig' ((x - mod x 10) `div` 10) ((b * 10) + mod x 10)

isPalindrome :: Integer -> Bool
isPalindrome a = a == reverseDigits a


products :: [Integer]
products = [x * y  | x <- [100..999], y <- [100..999]]

filteredProducts :: [Integer]
filteredProducts = [x | x <- products, isPalindrome x]

main :: IO ()
main = do
    print $ maximum filteredProducts