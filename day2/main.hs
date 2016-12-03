import Data.List.Split
import qualified Data.Map as Map

type Key = Int
type Coord = (Int, Int)
type Keypad = Map.Map Coord Key
type Keypad' = Map.Map Key Coord
data Direction = U | D | L | R deriving (Eq, Show, Read)

flipMap mp = Map.fromList $ map (\x -> (snd x, fst x)) (Map.assocs mp)

keypad1 :: Keypad
keypad1 = Map.fromList [
        ((-1, 1), 1), ((0, 1), 2), ((1, 1), 3),
        ((-1, 0), 4), ((0, 0), 5), ((1, 0), 6),
        ((-1, -1), 7), ((0, -1), 8), ((1, -1), 9)
    ]
keypad1' :: Keypad'
keypad1' = flipMap keypad1

keypad2 :: Keypad
keypad2 = Map.fromList [
        ((0, 2), 1),
        ((-1, 1), 2), ((0, 1), 3), ((1, 1), 4),
        ((-2, 0), 5), ((-1, 0), 6), ((0, 0), 7), ((1, 0), 8), ((2, 0), 9),
        ((-1, -1), 0xA), ((0, -1), 0xB), ((1, -1), 0xC),
        ((0, -2), 0xD)
    ]
keypad2' :: Keypad'
keypad2' = flipMap keypad2

nextCoord :: Keypad -> Keypad' -> Coord -> Direction -> Coord
nextCoord kp kp' c d = Map.findWithDefault (-999, -999) finalK kp'
	where 
		finalK = Map.findWithDefault (Map.findWithDefault 0 c kp) nextC kp
		nextC = case d of
			U -> (x, succ y)
			D -> (x, pred y)
			L -> (pred x, y)
			R -> (succ x, y)
		(x, y) = c

determineFinalCoord :: Keypad -> Keypad' -> Coord -> [Direction] -> Coord
determineFinalCoord kp kp' starting ds = foldl (nextCoord kp kp') starting ds
	

solve :: Keypad -> Keypad' -> Coord -> [[Direction]] -> [Key]
solve kp kp' starting ds = do
	map (\d -> Map.findWithDefault 0 (determineFinalCoord kp kp' starting d) kp) ds

part1 :: Coord -> [[Direction]] -> [Key]
part1 starting ds = solve keypad1 keypad1' starting ds

part2 :: Coord -> [[Direction]] -> [Key]
part2 starting ds = solve keypad2 keypad2' starting ds

parseInput :: String -> [[Direction]]
parseInput s = map (\l -> map (\d -> read [d]) l) lines
	where
		lines = splitOn "\n" $ (reverse . drop 1 . reverse) s

main :: IO ()
main = do
	f <- readFile "input.txt"
	let p = parseInput f
	print $ part1 (0, 0) p
	print $ part2 (0, 0) p
