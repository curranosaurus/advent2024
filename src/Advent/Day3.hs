module Advent.Day3 (test, real) where

import Advent.Parse qualified as Advent
import Control.Monad
import Control.Monad.Combinators
import Data.List (foldl')
import Data.Maybe
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec

data Mult = Mult Int Int
  deriving Show

multParser :: Advent.Parser Mult
multParser = do
  void $ chunk "mul("
  left <- decimal
  void $ char ','
  right <- decimal
  void $ char ')'
  pure $ Mult left right

multOrNothingParser :: Advent.Parser (Maybe Mult)
multOrNothingParser = try (fmap Just multParser) <|> fmap (const Nothing) anySingle

parser1 :: Advent.Parser [Mult]
parser1 = fmap catMaybes $ manyTill multOrNothingParser eof

data Instr = Do | Don't
  deriving Show

doParser :: Advent.Parser Instr
doParser = chunk "do()" *> pure Do

don'tParser :: Advent.Parser Instr
don'tParser = chunk "don't()" *> pure Don't

instrParser :: Advent.Parser Instr
instrParser = try doParser <|> try don'tParser

parser2 :: Advent.Parser [Either Instr Mult]
parser2 =
  fmap catMaybes $
    flip manyTill eof $
      fmap (Just . Left) (try instrParser) <|>
        fmap (Just . Right) (try multParser) <|>
          fmap (const Nothing) anySingle
      

test :: IO ()
test = do
  input1 <- Advent.parseFile parser1 "Day3.test1.txt"
  print $ runProgram1 input1
  input2 <- Advent.parseFile parser2 "Day3.test2.txt"
  print $ runProgram2 input2

real :: IO ()
real = do
  input1 <- Advent.parseFile parser1 "Day3.txt"
  print $ runProgram1 input1
  input2 <- Advent.parseFile parser2 "Day3.txt"
  print $ runProgram2 input2

runProgram1 :: [Mult] -> Int
runProgram1 = sum . map (\(Mult l r) -> l * r) 

runProgram2 :: [Either Instr Mult] -> Int
runProgram2 = snd . foldl' iter (True, 0)
  where
    iter (_, n) (Left Do) = (True, n)
    iter (_, n) (Left Don't) = (False, n)
    iter (True, n) (Right (Mult x y)) = (True, x * y + n)
    iter (False, n) (Right _) = (False, n)
