
import Data.List.Split
import Data.List
import Debug.Trace

type Length = Int

validTriangle :: [Int] -> Bool
validTriangle s = let [a, b, c] = sort s in a + b > c

parseLine :: String -> [Int]
parseLine s = map read $ words s

main :: IO ()
main = do
	f <- readFile "input.txt"
	let parsed = map parseLine $ lines f
	print $ length $ filter validTriangle parsed
	print $ length $ filter validTriangle $ concat $ map transpose $ chunksOf 3 parsed