module Advent.Day8 (test, real) where

import Advent.Grid (Grid, Coord (..))
import Advent.Grid qualified as Grid
import Advent.Parse qualified as Advent
import Data.List (foldl', tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Cell = Cell (Maybe Char)

instance Show Cell where
  show (Cell Nothing) = "."
  show (Cell (Just c)) = [c]

charParser :: Advent.Parser Cell
charParser = Cell <$>
  (try (single '.' *> pure Nothing)
    <|> try (Just <$> alphaNumChar))

parser :: Advent.Parser (Grid Cell)
parser = Grid.parser charParser

test :: IO ()
test = do
  input <- Advent.parseFile parser "Day8.test.txt"
  print $ runProgram1 input
  print $ runProgram2 input

real :: IO ()
real = do
  input <- Advent.parseFile parser "Day8.txt"
  print $ runProgram1 input
  print $ runProgram2 input

runProgram1 :: Grid Cell -> Int
runProgram1 grid =
  length
  . filter (flip Grid.isCoord grid)
  . getAllAntinodes
  . getCharPositions
  $ fmap (\(Cell c) -> c) grid

runProgram2 :: Grid Cell -> Int
runProgram2 grid =
  let unCelledGrid = fmap (\(Cell c) -> c) grid
      charPositions = getCharPositions unCelledGrid
      allAntinodes = getAllResonantAntinodes unCelledGrid charPositions
   in length allAntinodes

getAllAntinodes :: Map Char (Set Coord) -> [Coord]
getAllAntinodes coordMap = Set.toList $ Map.foldl' accum Set.empty coordMap
  where
    accum antinodes coords = antinodes <> getAntinodes (pairs $ Set.toList coords)
    getAntinodes coords = foldl' insertTwoCoords Set.empty (map (uncurry getAntinodePair) coords)
    insertTwoCoords :: Ord a => Set a -> (a, a) -> Set a
    insertTwoCoords s (coord1, coord2) = Set.insert coord2 $ Set.insert coord1 s
    getAntinodePair :: Coord -> Coord -> (Coord, Coord)
    getAntinodePair (Coord row1 col1) (Coord row2 col2) =
      let deltaRow = row2 - row1
          deltaCol = col2 - col1
          forwards = Coord (row2 + deltaRow) (col2 + deltaCol)
          backwards = Coord (row1 - deltaRow) (col1 - deltaCol)
       in (forwards, backwards)
    pairs :: [a] -> [(a, a)]
    pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

getCharPositions :: Grid (Maybe Char) -> Map Char (Set Coord)
getCharPositions grid =
  foldl' accum Map.empty $ catMaybes $ map (\coord -> (coord,) <$> Grid.lookup coord grid) $ Grid.coords grid
  where
    accum m (coord, ch) = Map.alter (addElt coord) ch m
    addElt coord s = case s of
      Nothing -> Just $ Set.singleton coord
      Just s' -> Just $ Set.insert coord s'

getAllResonantAntinodes :: Grid (Maybe Char) -> Map Char (Set Coord) -> [Coord]
getAllResonantAntinodes grid coordMap = Set.toList $ Map.foldl' accum Set.empty coordMap
  where
    pairs :: [a] -> [(a, a)]
    pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
    accum antinodes coords = antinodes <> getAntinodes (pairs $ Set.toList coords)
    getAntinodes coords = foldl' (<>) Set.empty (map (uncurry getLinearAntinodes) coords)
    getLinearAntinodes coord1@(Coord row1 col1) coord2@(Coord row2 col2) =
      let deltaRow = row2 - row1
          deltaCol = col2 - col1
          delta = Coord deltaRow deltaCol
          infiniteForwardses = map (\m -> Grid.addCoord coord2 $ Grid.multCoord m delta) [0..]
          infiniteBackwardses = map (\m -> Grid.addCoord coord1 $ Grid.multCoord (-m) delta) [0..]
          forwardses = takeWhile (flip Grid.isCoord grid) infiniteForwardses
          backwardses = takeWhile (flip Grid.isCoord grid) infiniteBackwardses
       in Set.fromList $ forwardses ++ backwardses
