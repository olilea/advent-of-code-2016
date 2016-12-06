
import Data.List
import Data.Ord

mostFreq :: (Eq a, Ord a) => [a] -> a
mostFreq as = head . maximumBy (comparing length) . group . sort $ as

leastFreq :: (Eq a, Ord a) => [a] -> a
leastFreq as = head . minimumBy (comparing length) . group . sort $ as

main :: IO ()
main = do
    f <- readFile "input.txt"
    let ls = lines f
    print $ map mostFreq $ transpose ls
    print $ map leastFreq $ transpose ls