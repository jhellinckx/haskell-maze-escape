module Main where

import qualified System.Environment
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do [path] <- System.Environment.getArgs 
          maze <- readFile path
          putStr $ unlines $ escape $ lines maze

data MazeCell = Wall | Pathway | Path | Start | End deriving (Eq, Ord)
type Maze = [[MazeCell]]
type Coord = (Int, Int)
type Path = [Coord]

escape :: [[Char]] -> [[Char]]
escape maze = showPath dmaze (bfs dmaze)
    where dmaze = map (map deserializeCell) maze

(serializeCell, deserializeCell) = bimappers [(Wall, 'X'), (Pathway, ' '), (Path, '.'), (Start, '*'), (End, '@')]

bimappers :: (Ord a, Ord b) => [(a, b)] -> (a -> b, b -> a)
bimappers keys = (mapper $ Map.fromList keys, mapper $ Map.fromList $ map swap keys) where swap (a, b) = (b, a)

mapper :: (Ord a) => Map.Map a b -> a -> b
mapper m k = m Map.! k

showPath :: Maze -> Maybe Path -> [[Char]]
showPath maze Nothing = ["No solution found."]
showPath maze (Just path) = map (map serializeCell) $ map (map (\((x, y), cell) -> if elem (x, y) path then Path else cell)) $ withCoords maze

withCoords :: Maze -> [[(Coord, MazeCell)]]
withCoords maze = map (\(y, row) -> zipWith (\x e -> ((x, y), e)) [0..] row) $ zip [0..] maze

move :: (Int, Int) -> Coord -> Coord
move (a, b) (x, y) = (x + a, y + b)

motions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

adjacents :: Maze -> Coord -> [Coord]
adjacents maze coord = (pathway . inside) candidates
    where candidates = map move motions <*> (pure coord)
          (bx, by) = ((0, length (maze !! 0)), (0, length maze))
          inside = filter (\(x, y) -> inbounds bx x && inbounds by y)
          pathway = filter (\coord -> not (maze `cell` coord == Wall))

inbounds :: (Int, Int) -> Int -> Bool
inbounds (lower, upper) x = lower <= x && x <= upper

cell :: Maze -> Coord -> MazeCell
maze `cell` (x, y) = maze !! y !! x

coord :: Maze -> MazeCell -> Maybe Coord
maze `coord` cell = pure fst <*> (find (\((x, y), c) -> c == cell) $ concat $ withCoords maze)

bfs :: Maze -> Maybe Path
bfs maze = start >>= (bfstart maze)
    where start = maze `coord` Start

bfstart :: Maze -> Coord -> Maybe Path
bfstart maze start = bfstep maze [(start, Start, [])] []

bfstep :: Maze -> [(Coord, MazeCell, Path)] -> [Coord] -> Maybe Path
bfstep _ [] _ = Nothing
bfstep _ ((h, End, p):_) _ = Just (h:p)
bfstep maze ((h, _, p):queue) visited = bfstep maze (queue ++ (map (\a -> (a, maze `cell` a, h:p)) adj)) (h:visited)
    where adj = filter (not . (flip elem visited)) (adjacents maze h)




