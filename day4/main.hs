import Data.List as List
import Data.List.Split
import Data.Map as Map
import Data.Ord
import Data.Char
import Debug.Trace

type Room = (String, Int, String)

getName :: String -> String
getName s = takeWhile isAlpha [x | x <- s, x /= '-']

getId :: String -> Int
getId s = read cs :: Int
    where
        cs = takeWhile isDigit $ dropWhile (\c -> not $ isDigit c) s

getChecksum :: String -> String
getChecksum s = takeWhile (\c -> c /= ']') $ tail $ dropWhile (\c -> c /= '[') s

parse :: String -> [Room]
parse s = let ls = lines s in
    List.map (\r -> (getName r, getId r, getChecksum r)) ls

sorter :: (Char, Int) -> (Char, Int) -> Ordering
sorter (c1, no1) (c2, no2)
    | no1 < no2 = GT
    | no1 > no2 = LT
    | no1 == no2 = compare c1 c2

topOccurring :: Int -> String -> [Char]
topOccurring count s = List.map (\p -> fst p) $ take 5 sfreqs
    where
        freqs = toList $ fromListWith (+) [(c, 1) | c <- s]
        sfreqs = List.sortBy sorter freqs
        

validRoom :: Room -> Bool
validRoom (name, idee, checksum) = all (\c -> elem c checksum) top
    where
        top = topOccurring 5 name

countValid :: [Room] -> Int
countValid rs = sum $ List.map (\(_, id, _) -> id) $ List.filter validRoom rs

decrypt :: Room -> Room
decrypt (name, idee, checksum) = (shift name, idee, checksum)
    where
        alpha = cycle ['a'..'z']
        shift = List.map (\c -> alpha !! ((ord c) - (ord 'a') + idee))

main :: IO ()
main = do
    f <- readFile "input.txt"
    let rs = parse f
    print $ countValid rs
    print $ List.filter (\(name, _, _) -> isInfixOf "north" name) $ List.map decrypt rs