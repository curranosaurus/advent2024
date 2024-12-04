module Advent.Day4 (test, real) where

import Advent.Parse qualified as Advent
import Control.Monad.Combinators
import Data.List (transpose)
import Text.Megaparsec
import Text.Megaparsec.Char

data Letter = X | M | A | S
  deriving Show

letterParser :: Advent.Parser Letter
letterParser =
  try (single 'X' *> pure X)
    <|> try (single 'M' *> pure M)
    <|> try (single 'A' *> pure A)
    <|> try (single 'S' *> pure S)

parser :: Advent.Parser [[Letter]]
parser = manyTill (manyTill letterParser eol) eof

test :: IO ()
test = do
  input <- Advent.parseFile parser "Day4.test.txt"
  print $ runProgram1 input
  print $ runProgram2 input

real :: IO ()
real = do
  input <- Advent.parseFile parser "Day4.txt"
  print $ runProgram1 input
  print $ runProgram2 input

runProgram1 :: [[Letter]] -> Int
runProgram1 letters =
  let forwards = sum $ map countHorizontal letters
      downwards = sum $ map countHorizontal $ transpose letters
      downwardsForwards = sum $ map countHorizontal $ diagonals letters
      downwardsBackwards = sum $ map countHorizontal $ diagonals $ reverse letters
   in forwards + downwards + downwardsForwards + downwardsBackwards

diagonals :: [[Letter]] -> [[Letter]]
diagonals m =
  let dim = length m
      firstHalf = [[(m !! (startingRow + col)) !! col | col <- [0 .. dim - startingRow - 1]] | startingRow <- [dim - 1, dim - 2 .. 0]]
      secondHalf = [[(m !! row) !! (startingCol + row) | row <- [0 .. dim - startingCol - 1]] | startingCol <- [1 .. dim - 1]]
   in firstHalf ++ secondHalf

countHorizontal :: [Letter] -> Int
countHorizontal letters = go letters 0
  where
    go [] n = n
    go (X : M : A : S : ls) n = go (S : ls) (n + 1)
    go (S : A : M : X : ls) n = go (X : ls) (n + 1)
    go (_ : ls) n = go ls n

type Tuple3 a = (a, a, a)

runProgram2 :: [[Letter]] -> Int
runProgram2 = length . filter isXmas . threeByThreeBlocks

isXmas :: Tuple3 (Tuple3 Letter) -> Bool
isXmas
  ( ( M, _, S )
  , ( _, A, _ )
  , ( M, _, S )
  ) = True
isXmas
  ( ( S, _, S )
  , ( _, A, _ )
  , ( M, _, M )
  ) = True
isXmas
  ( ( S, _, M )
  , ( _, A, _ )
  , ( S, _, M )
  ) = True
isXmas
  ( ( M, _, M )
  , ( _, A, _ )
  , ( S, _, S )
  ) = True
isXmas _ = False

threeByThreeBlocks :: [[Letter]] -> [Tuple3 (Tuple3 Letter)]
threeByThreeBlocks (row1 : row2 : row3 : rest) = go row1 row2 row3 ++ threeByThreeBlocks (row2 : row3 : rest)
  where
    go :: [Letter] -> [Letter] -> [Letter] -> [Tuple3 (Tuple3 Letter)]
    go (e11 : e12 : e13 : e1Rest) (e21 : e22 : e23 : e2Rest) (e31 : e32 : e33 : e3Rest)
      = ((e11, e12, e13), (e21, e22, e23), (e31, e32, e33))
          : go (e12 : e13 : e1Rest) (e22 : e23 : e2Rest) (e32 : e33 : e3Rest)
    go _ _ _ = []
threeByThreeBlocks _ = []
