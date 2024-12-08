module Advent.Day5 (test, real) where

import Advent.Parse qualified as Advent
import Control.Monad
import Control.Monad.Combinators
import Data.Array qualified as Array
import Data.Array (Array, (!), Ix)
import Data.List (foldl', sortBy)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set.NonEmpty qualified as NESet
import Data.Set.NonEmpty (NESet)
import Data.Set qualified as Set
import Data.Set (Set)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- the key is the page on the left, the NESet is all the pages that must feature to its right.
newtype PageOrders = PageOrders (Map Int (NESet Int))
  deriving Show

newtype Update = Update (Array Int Int)
  deriving Show

data Input = Input
  { inputPageOrderingRules :: PageOrders
  , inputUpdates :: [Update]
  }
  deriving Show

parsePageOrder :: Advent.Parser (Int, Int)
parsePageOrder = do
  left <- decimal
  void $ single '|'
  right <- decimal
  void eol
  pure $ (left, right)

parsePageOrders :: Advent.Parser PageOrders
parsePageOrders = toPageOrders <$> many parsePageOrder
  where
    toPageOrders :: [(Int, Int)] -> PageOrders
    toPageOrders pairs = PageOrders $ foldl' iter Map.empty pairs
    iter s (left, right) = Map.alter (addDirectional right) left s
    addDirectional newVal = \case
      Nothing -> Just $ NESet.singleton newVal
      Just directionals -> Just $ NESet.insert newVal directionals

parseUpdate :: Advent.Parser Update
parseUpdate = updatesToListArray <$> manyTill (decimal <* many (single ',')) eol
  where
    updatesToListArray l = Update $ Array.listArray (0, length l - 1) l

parseInput :: Advent.Parser Input
parseInput = do
  pageOrders <- parsePageOrders
  void eol
  updates <- manyTill parseUpdate eof
  pure
    Input
      { inputPageOrderingRules = pageOrders
      , inputUpdates = updates
      }

test :: IO ()
test = do
  input <- Advent.parseFile parseInput "Day5.test.txt"
  print $ runProgram1 input
  print $ runProgram2 input

real :: IO ()
real = do
  input <- Advent.parseFile parseInput "Day5.txt"
  print $ runProgram1 input
  print $ runProgram2 input

runProgram1 :: Input -> Int
runProgram1 input =
  sum . map middleEntry $
    filter (isCorrectUpdate input.inputPageOrderingRules) input.inputUpdates

runProgram2 :: Input -> Int
runProgram2 input =
  sum . map (middleEntry . sortUpdate input.inputPageOrderingRules) $
    filter (not . isCorrectUpdate input.inputPageOrderingRules) input.inputUpdates

sortUpdate :: PageOrders -> Update -> Update
sortUpdate (PageOrders outArrows) (Update update) =
  let sorted = sortBy comp $ Array.elems update
   in Update $ Array.listArray (Array.bounds update) sorted
  where
    hasArrow arrs a b = case Map.lookup a arrs of
      Nothing -> False
      Just s -> NESet.member b s
    comp x y
        | lt = LT
        | gt = GT
        | otherwise = EQ
      where
        lt = hasArrow outArrows x y
        gt = hasArrow outArrows y x

middleEntry :: Update -> Int
middleEntry (Update update) = middleArrEntry update

middleArrEntry :: (Ix i, Integral i) => Array.Array i e -> e
middleArrEntry arr =
  let len = snd (Array.bounds arr) + 1
      middleIndex = len `div` 2
   in arr ! middleIndex

isCorrectUpdate :: PageOrders -> Update -> Bool
isCorrectUpdate pageOrders = not . hasRuleBreakage pageOrders

hasRuleBreakage :: PageOrders -> Update -> Bool
hasRuleBreakage (PageOrders pageOrders) (Update update) = go Set.empty (Array.elems update)
  where
    go :: Set Int -> [Int] -> Bool
    go _seenBefore [] = False
    go seenBefore (pageToUpdate : pagesToUpdate) =
      let bannedPages = Map.lookup pageToUpdate pageOrders
          seenBannedPage =
            case bannedPages of
              Nothing -> False
              Just bannedPages' -> not $ Set.null $ seenBefore `Set.intersection` NESet.toSet bannedPages'
       in seenBannedPage || go (Set.insert pageToUpdate seenBefore) pagesToUpdate
