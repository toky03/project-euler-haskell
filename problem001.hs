limit :: Int
limit = 1000

infiniteList :: [Int]
infiniteList = [1..]
combinedList = takeWhile (<1000) [ i | i <- infiniteList, mod i 3 == 0 || mod i 5 == 0]

combinedSum = sum combinedList

main = do
    print combinedSum
