module Main where

import qualified System.Environment
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do [path] <- System.Environment.getArgs 
          maze <- readFile path
          putStr $ unlines $ escape $ lines maze

escape :: [[Char]] -> [[Char]]
escape maze = (map (map serializeCell)) $ (map (map deserializeCell) maze)

data MazeCell = Wall | Pathway | Path | Start | End deriving(Eq, Ord)

-- No parsing/input validation is done, we consider the input maze to be correctly encoded.
(serializeCell, deserializeCell) = bimappers [(Wall, 'X'), (Pathway, ' '), (Path, '.'), (Start, '*'), (End, '@')]
bimappers :: (Ord a, Ord b) => [(a, b)] -> (a -> b, b -> a)
bimappers keys = (mapper $ Map.fromList keys, mapper $ Map.fromList $ map swap keys) where swap (a, b) = (b, a)
mapper :: (Ord a) => Map.Map a b -> a -> b
mapper m k = m Map.! k




