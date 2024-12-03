module Advent.Parse (parseFile, Parser) where

import Control.Exception
import Data.Functor.Identity
import Data.Text
import Data.Text.IO qualified as T
import Data.Void
import Text.Megaparsec qualified as Megaparsec

type Parser = Megaparsec.ParsecT Void Text Identity

data ParseException = ParseException
  deriving (Show, Exception)

runParser :: Parser a -> Text -> IO a
runParser parser input = case Megaparsec.runParser parser "" input of
  Left err -> throwIO err
  Right res -> pure res

parseFile :: Parser a -> String -> IO a
parseFile parser fileName = do
  rawInput <- T.readFile $ "src/Advent/" <> fileName
  runParser parser rawInput
