module Advent.Day1 (real, test) where

import Advent.Parse qualified as Advent
import Control.Monad
import Control.Monad.Combinators
import Data.List (sort, foldl')
import Data.Map qualified as Map
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec hiding (State)

data Row = Row Int Int
  deriving (Show, Eq)

rowParser :: Advent.Parser Row
rowParser = do
  left <- decimal
  space1
  right <- decimal
  void eol
  pure $ Row left right

parser :: Advent.Parser [Row]
parser = manyTill rowParser eof

real :: IO ()
real = do
  input <- Advent.parseFile parser "Day1.txt"
  print $ runProgram1 input
  print $ runProgram2 input

test :: IO ()
test = do
  input <- Advent.parseFile parser "Day1.test.txt"
  print $ runProgram1 input
  print $ runProgram2 input

runProgram1 :: [Row] -> Int
runProgram1 rows =
  let lefts = map (\(Row l _) -> l) rows
      rights = map (\(Row _ r) -> r) rows
      pairs = zip (sort lefts) (sort rights)
      diffs = map (abs . uncurry (-)) pairs
   in sum diffs

data State = State
  { numberOfRightOccurrences :: Map.Map Int Int
  , currentSum :: Int
  }

initialState :: State
initialState = State
  { numberOfRightOccurrences = Map.empty
  , currentSum = 0
  }

runProgram2 :: [Row] -> Int
runProgram2 rows =
  let lefts = map (\(Row l _) -> l) rows
      rights = map (\(Row _ r) -> r) rows
   in currentSum $ foldl' (iter rights) initialState lefts
  where
    iter :: [Int] -> State -> Int -> State
    iter rights s leftNum = case Map.lookup leftNum s.numberOfRightOccurrences of
      Just occurrences -> s { currentSum = s.currentSum + leftNum * occurrences }
      Nothing ->
        let occurrences = length $ filter (== leftNum) rights
         in State
              { numberOfRightOccurrences = Map.insert leftNum occurrences s.numberOfRightOccurrences
              , currentSum = s.currentSum + leftNum * occurrences
              }
