module Main (main) where

import System.Environment
import Advent.Day1 qualified as Day1
import Advent.Day2 qualified as Day2
import Advent.Day3 qualified as Day3
import Advent.Day4 qualified as Day4
import Advent.Day5 qualified as Day5
import Advent.Day7 qualified as Day7

main :: IO ()
main = do
  args <- getArgs
  case take 2 args of
    ["test", "1"] -> Day1.test
    ["real", "1"] -> Day1.real
    ["test", "2"] -> Day2.test
    ["real", "2"] -> Day2.real
    ["test", "3"] -> Day3.test
    ["real", "3"] -> Day3.real
    ["test", "4"] -> Day4.test
    ["real", "4"] -> Day4.real
    ["test", "5"] -> Day5.test
    ["real", "5"] -> Day5.real
    ["test", "7"] -> Day7.test
    ["real", "7"] -> Day7.real
    _ -> error "day not found"
