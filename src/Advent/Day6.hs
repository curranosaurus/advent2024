{-# LANGUAGE DuplicateRecordFields #-}

module Advent.Day6 (test, real) where

import Advent.Parse qualified as Advent
import Control.Monad.Combinators
import Data.Array qualified as Array
import Data.Array (Array)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char

data Dir = DirUp | DirRight | DirDown | DirLeft
  deriving (Show, Eq, Ord, Bounded, Enum)

rot90 :: Dir -> Dir
rot90 DirUp = DirRight
rot90 DirRight = DirDown
rot90 DirDown = DirLeft
rot90 DirLeft = DirUp

data Cell = Obstacle | Open
  deriving Eq

newtype Cells = Cells (Array Int Cell)

getCell :: (Int, Int) -> (Int, Int) -> Cells -> Cell
getCell botRight pairIdx (Cells cells) = 
  let idx = fromPairIdx botRight pairIdx
   in cells Array.! idx

data Input = Input
  { inputCells :: Cells
  , inputGuardIdx :: Int
  }

cellParser :: Advent.Parser (Cell, Bool)
cellParser =
  try (single '.' *> pure (Open, False))
    <|> try (single '#' *> pure (Obstacle, False))
    <|> try (single '^' *> pure (Open, True))

parser :: Advent.Parser Input
parser = do
  cells <- manyTill (cellParser <* many eol) eof
  let guardIdx = fst $ head $ filter (snd . snd) $ zip [0..] cells
  pure
    Input
      { inputCells = mkCells $ map fst cells
      , inputGuardIdx = guardIdx
      }
  where
    mkCells l = Cells $ Array.listArray (0, length l - 1) l

test :: IO ()
test = runAll "Day6.test.txt"

real :: IO ()
real = runAll "Day6.txt"

runAll :: String -> IO ()
runAll file = do
  input <- Advent.parseFile parser file
  rawInput <- T.readFile $ "data/" <> file
  let rightmostIdx = T.length $ T.takeWhile (/= '\n') rawInput
      botmostIdx = T.length $ T.filter (== '\n') rawInput
  print $ runProgram1 (botmostIdx, rightmostIdx) input
  print $ runProgram2 (botmostIdx, rightmostIdx) input

toPairIdx :: (Int, Int) -> Int -> (Int, Int)
toPairIdx (_height, width) idx = (idx `div` width, idx `mod` width)

fromPairIdx :: (Int, Int) -> (Int, Int) -> Int
fromPairIdx (_height, width) (row, col) = row * width + col

runProgram1Iteration :: (Int, Int) -> Input -> MapState
runProgram1Iteration botRight input = iterWhile keepIterating (nextMap botRight input.inputCells) initialState
  where
    initialState = MapState
      { visitedCells = Set.empty
      , guardCell = toPairIdx botRight input.inputGuardIdx
      , guardDirection = DirUp
      , keepIterating = True
      }

runProgram1 :: (Int, Int) -> Input -> Int
runProgram1 botRight input = length . visitedCells $ runProgram1Iteration botRight input

iterWhile :: (a -> Bool) -> (a -> a) -> a -> a
iterWhile p f x =
  if p x
    then iterWhile p f (f x)
    else x

data MapState = MapState
  { visitedCells :: Set (Int, Int)
  , guardCell :: (Int, Int)
  , guardDirection :: Dir
  , keepIterating :: Bool
  }
  deriving Show

setGoal :: (Int, Int) -> Dir -> (Int, Int)
setGoal (curRow, curCol) direction = 
  case direction of
    DirUp -> (curRow - 1, curCol)
    DirRight -> (curRow, curCol + 1)
    DirDown -> (curRow + 1, curCol)
    DirLeft -> (curRow, curCol - 1)

cellInBounds :: (Int, Int) -> (Int, Int) -> Bool
cellInBounds (botmostIdx, rightmostIdx) (cellRow, cellCol) =
  cellRow >= 0 && cellRow < botmostIdx && cellCol >= 0 && cellCol < rightmostIdx

nextMap :: (Int, Int) -> Cells -> MapState -> MapState
nextMap botRight cells s =
  let goalCell = setGoal s.guardCell s.guardDirection
      goalInBounds = cellInBounds botRight goalCell
      goalCellOpen = getCell botRight goalCell cells == Open
   in case (goalInBounds, goalCellOpen) of
        (False, _) ->
          MapState
             { visitedCells = Set.insert s.guardCell s.visitedCells 
             , guardCell = s.guardCell
             , guardDirection = s.guardDirection
             , keepIterating = False
             }
        (True, True) ->
          MapState
             { visitedCells = Set.insert s.guardCell s.visitedCells 
             , guardCell = goalCell
             , guardDirection = s.guardDirection
             , keepIterating = True
             }
        (True, False) ->
          MapState
             { visitedCells = s.visitedCells
             , guardCell = s.guardCell
             , guardDirection = rot90 s.guardDirection
             , keepIterating = True
             }

runProgram2 :: (Int, Int) -> Input -> Int
runProgram2 botRight input = length $ filter (resultsInLoop botRight) mapOptions
  where
    mapOptions = map (\cell -> input { inputCells = addObstacle botRight input.inputCells cell }) validCellsForObstacle
    guardCells = visitedCells $ runProgram1Iteration botRight input
    guardInitialCell = toPairIdx botRight input.inputGuardIdx
    validCellsForObstacle = filter cellCanHaveNewObstacle $ Set.toList guardCells
    cellCanHaveNewObstacle cell =
      cellInBounds botRight cell
        && getCell botRight cell input.inputCells == Open
        && cell /= guardInitialCell
              
addObstacle :: (Int, Int) -> Cells -> (Int, Int) -> Cells
addObstacle botRight (Cells cells) newObstacle =
  let newObstacleIdx = fromPairIdx botRight newObstacle
   in Cells $ cells Array.// [(newObstacleIdx, Obstacle)]

resultsInLoop :: (Int, Int) -> Input -> Bool
resultsInLoop botRight input = checkForLoop botRight input.inputCells initialState
  where
    initialState = LoopState
      { oldGuardStates = Set.empty
      , guardCell = toPairIdx botRight input.inputGuardIdx
      , guardDirection = DirUp
      }

data LoopState = LoopState
  { oldGuardStates :: Set ((Int, Int), Dir)
  , guardCell :: (Int, Int)
  , guardDirection :: Dir
  }

checkForLoop :: (Int, Int) -> Cells -> LoopState -> Bool
checkForLoop botRight cells s
  | Set.member (s.guardCell, s.guardDirection) s.oldGuardStates = True
  | otherwise =
      let goalCell = setGoal s.guardCell s.guardDirection
          goalInBounds = cellInBounds botRight goalCell
          goalCellOpen = getCell botRight goalCell cells == Open
       in case (goalInBounds, goalCellOpen) of
            (False, _) -> False
            (True, True) ->
              let newState = LoopState
                    { oldGuardStates = Set.insert (s.guardCell, s.guardDirection) s.oldGuardStates
                    , guardCell = goalCell
                    , guardDirection = s.guardDirection
                    }
               in checkForLoop botRight cells newState
            (True, False) ->
              let newState = LoopState
                    { oldGuardStates = Set.insert (s.guardCell, s.guardDirection) s.oldGuardStates
                    , guardCell = s.guardCell
                    , guardDirection = rot90 s.guardDirection
                    }
               in checkForLoop botRight cells newState
