module Advent.Day2 (real, test) where

import Advent.Parse qualified as Advent
import Control.Monad.Combinators
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec

newtype Report = Report [Int]
  deriving Show

reportParser :: Advent.Parser Report
reportParser = fmap Report $ manyTill (decimal <* skipMany (char ' ')) eol

parser :: Advent.Parser [Report]
parser = manyTill reportParser eof

real :: IO ()
real = do
  input <- Advent.parseFile parser "Day2.txt"
  print $ runProgram1 input
  print $ runProgram2 input

test :: IO ()
test = do
  input <- Advent.parseFile parser "Day2.test.txt"
  print $ runProgram1 input
  print $ runProgram2 input

reportIsSafe :: Report -> Bool
reportIsSafe (Report report) =
  let pairs = mkPairs report
   in pairsAreMonotonic pairs && pairsAreEvenlySpaced pairs

mkPairs :: [a] -> [(a, a)]
mkPairs report = drop 1 report `zip` dropLast 1 report
  where
    dropLast n xs = reverse (drop n (reverse xs))

pairsAreMonotonic :: [(Int, Int)] -> Bool
pairsAreMonotonic [] = True
pairsAreMonotonic ((m, n) : ns) =
  let firstComparison = compare m n
   in all (== firstComparison) $ map (uncurry compare) ns

pairsAreEvenlySpaced :: [(Int, Int)] -> Bool
pairsAreEvenlySpaced = all (uncurry diffInRange)

runProgram1 :: [Report] -> Int
runProgram1 = length . filter reportIsSafe

runProgram2 :: [Report] -> Int
runProgram2 = length . filter reportIsNearlySafe

diffInRange :: Int -> Int -> Bool
diffInRange m n =
  let diff = abs (m - n)
   in 1 <= diff && diff <= 3

reportIsNearlySafe :: Report -> Bool
reportIsNearlySafe (Report report) =
  any (reportIsSafe . Report) $ pokeReport report
  where
    pokeReport rep = rep : pokes rep
    pokes as = map (pokeOutNth as) [0..length as - 1]
    pokeOutNth as i = take i as <> drop (i + 1) as
