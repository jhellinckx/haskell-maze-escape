module Main where

import qualified System.Environment
import Data.List
import qualified Data.Map as Map
import Data.Array

main :: IO ()
main = do [path] <- System.Environment.getArgs 
          maze <- readFile path
          putStr $ unlines $ escape $ lines maze

data MazeCell = Wall | Pathway | Path | Start | End deriving (Eq, Ord)
type Position = (Int, Int)
type Path = [Position]
type Maze = Array Position MazeCell

motions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
cells = [(Wall, 'X'), (Pathway, ' '), (Path, '.'), (Start, '*'), (End, '@')]
(serialize, deserialize) = bimappers cells

escape :: [[Char]] -> [[Char]]
escape maze = case bfs (mazeArray maze) of Nothing -> ["No solution found."]
                                           Just solution -> map (map (markPath solution)) $ withPositions maze

markPath :: Path -> (Position, Char) -> Char
markPath path (pos, c) = if elem pos path then serialize Path else c

mazeArray :: [[Char]] -> Maze
mazeArray maze = array mazeBounds $ (concat . withPositions . map (map deserialize)) maze 
    where mazeBounds = ((0, 0), (length maze - 1, length (head maze) - 1))

withPositions :: [[a]] -> [[(Position, a)]]
withPositions matrix = map (\(i, row) -> zipWith (\j e -> ((i, j), e)) [0..] row) $ zip [0..] matrix

bimappers :: (Ord a, Ord b) => [(a, b)] -> (a -> b, b -> a)
bimappers keys = (mapper $ Map.fromList keys, mapper $ Map.fromList $ map swap keys) where swap (a, b) = (b, a)

mapper :: (Ord a) => Map.Map a b -> a -> b
mapper m k = m Map.! k

move :: (Int, Int) -> Position -> Position
move (a, b) (x, y) = (x + a, y + b)

adjacents :: Maze -> Position -> [Position]
adjacents maze pos = (pathway . inside) candidates
    where candidates = map move motions <*> (pure pos)
          inside = filter (\(i, j) -> inRange (bounds maze) pos)
          pathway = filter (\pos -> not (maze ! pos == Wall))

position :: Maze -> MazeCell -> Maybe Position
maze `position` cell = pure fst <*> (find (\((i, j), e) -> e == cell) $ assocs maze)

bfs :: Maze -> Maybe Path
bfs maze = maze `position` Start >>= (bfstart maze)

bfstart :: Maze -> Position -> Maybe Path
bfstart maze start = bfstep maze [(start, Start, [])] [start]

bfstep :: Maze -> [(Position, MazeCell, Path)] -> [Position] -> Maybe Path
bfstep _ [] _ = Nothing
bfstep _ ((_, End, _:p):_) _ = Just p
bfstep maze ((h, _, p):queue) visited = bfstep maze (queue ++ (map (\pos -> (pos, maze ! pos, pos:p)) adj)) (visited ++ adj)
    where adj = filter (not . (flip elem visited)) (adjacents maze h)




