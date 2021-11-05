import System.IO  
import Control.Monad
import Data.Text (pack, splitOn, unpack)
import qualified Data.Text (Text)
import Data.Char



splitLines grid = splitOn (pack "\n") (pack grid)

mapToInts :: Data.Text.Text -> [Integer]
mapToInts x = map (read . unpack) (splitOn (pack " ") x)

transformToDigits:: [Data.Text.Text] -> [[Int]]
transformToDigits = map (map digitToInt . unpack)

columnSum:: Integral a => [[a]] -> Int -> a
columnSum g index = sum (map (!! index) g)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]


add :: Integral a => [[a]] -> [a]
add grid = add' grid (length  (head grid) -1) 0 []
    where 
        add' g index reminder result
            | -1 == index = digs reminder ++ result
            | otherwise = add' g (index - 1) newReminder (lastDigit : result)
                where newReminder = (newSum - lastDigit) `div` 10 
                      newSum = columnSum g index + reminder 
                      lastDigit = mod newSum 10

main :: IO ()
main = do  
        handle <- openFile "text_problem013.txt" ReadMode
        contents <- hGetContents handle
        print $ concatMap show $ take 10 $ add (transformToDigits $ splitLines contents)
        hClose handle   
