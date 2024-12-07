module Advent.Day7 (test, real, concatNums) where

import Advent.Parse qualified as Advent
import Control.Monad
import Control.Monad.Combinators
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

parseLine :: Advent.Parser (Int, [Int])
parseLine = do
  first <- decimal
  void $ single ':'
  factors <- many (single ' ' *> decimal)
  pure (first, factors)

parser :: Advent.Parser [(Int, [Int])]
parser = manyTill (parseLine <* eol) eof

test :: IO ()
test = do
  input <- Advent.parseFile parser "Day7.test.txt"
  print $ runProgram1 input
  print $ runProgram2 input

real :: IO ()
real = do
  input <- Advent.parseFile parser "Day7.txt"
  print $ runProgram1 input
  print $ runProgram2 input

runProgram1 :: [(Int, [Int])] -> Int
runProgram1 = sum . map fst . filter (uncurry (isPossible [(+), (*)]))

runProgram2 :: [(Int, [Int])] -> Int
runProgram2 = sum . map fst . filter (uncurry (isPossible [(+), (*), concatNums]))

concatNums :: Int -> Int -> Int
concatNums m n = read $ show m ++ show n

isPossible :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
isPossible _ 0 [] = True
isPossible _ _ [] = False
isPossible ops y (x : xs) =
  let possibleResults = foldM accum x xs
   in any (== y) possibleResults
  where
    accum m n = map (\op -> op m n) ops
