module Main (main) where

import System.Environment
import Advent.Day1 qualified as Day1
import Advent.Day2 qualified as Day2

main :: IO ()
main = do
  args <- getArgs
  case take 2 args of
    ["test", "1"] -> Day1.test
    ["real", "1"] -> Day1.real
    ["test", "2"] -> Day2.test
    ["real", "2"] -> Day2.real
    _ -> error "day not found"
