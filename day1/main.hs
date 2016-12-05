import Data.List.Split

data Compass = North | South | East | West deriving (Eq, Read, Show)
data Direction = L | R deriving (Eq, Read, Show)

type Coord = (Int, Int)
type Steps = Int
type Move = (Direction, Steps)

parseMove :: String -> Move
parseMove s = (dir, steps)
    where
        dir = if head s == 'L' then L else R
        steps = read (tail s) :: Int

parseInput :: String -> [Move]
parseInput s = map parseMove moves
    where
        moves = splitOn ", " s

turnLeft :: Compass -> Compass
turnLeft c = case c of
    North -> West
    West -> South
    South -> East
    East -> North

turnRight :: Compass -> Compass
turnRight c = case c of
    North -> East
    East -> South
    South -> West
    West -> North

turn :: Compass -> Direction -> Compass
turn c d = case d of
    L -> turnLeft c
    R -> turnRight c

nextLocation :: Compass -> Coord -> Coord
nextLocation facing c = 
    case facing of
        North -> (x, succ y)
        South -> (x, pred y)
        East -> (succ x, y)
        West -> (pred x, y)
    where
        (x, y) = c

execMove :: Compass -> Coord -> [Coord] -> Steps -> [Coord]
execMove facing current locs steps =
    case steps of
        0 -> locs
        otherwise -> execMove facing newLoc ([newLoc] ++ locs) (pred steps)
    where
        newLoc = nextLocation facing current

doMoves :: Compass -> [Coord] -> [Move] -> [Coord]
doMoves facing locs [] = locs
doMoves facing locs (m : ms) = doMoves newFacing (newLocs ++ locs) ms
    where
        currentLoc = head locs
        newFacing = turn facing turnDir
        newLocs = execMove newFacing currentLoc [] steps
        (turnDir, steps) = m

moves :: Compass -> Coord -> [Move] -> [Coord]
moves initialFacing initialLoc moves = doMoves initialFacing [initialLoc] moves

firstDuplicate :: Eq a => [a] -> a
firstDuplicate as = let x : xs = as in
    if elem x xs then x else firstDuplicate xs

p1 :: [Move] -> Int
p1 ms = (abs x + abs y)
    where
        (x, y) = finalLoc
        finalLoc = head $ moves North (0, 0) ms

p2 :: [Move] -> Int
p2 ms = (abs x + abs y)
    where
        (x, y) = finalLoc
        finalLoc = firstDuplicate $ reverse $ moves North (0, 0) ms

main :: IO ()
main = do
    f <- readFile "input.txt"
    let ms = parseInput f
    print $ p1 ms
    print $ p2 ms