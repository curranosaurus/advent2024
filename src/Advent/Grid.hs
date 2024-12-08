module Advent.Grid (Grid, fromList, Coord (..), toList, Dir (..), lookup, isCoord, coords, parser, gridWidth, gridHeight) where

import Advent.Parse qualified as Advent
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array qualified as Array
import Data.Array (Array)
import Data.Text (Text)
import Data.Void
import Prelude hiding (lookup)
import Text.Megaparsec
import Text.Megaparsec.Char

data Dir = DirUp | DirRight | DirDown | DirLeft

data Coord = Coord { coordRow :: Int, coordCol :: Int }
  deriving (Show, Eq, Ord)

data Grid a = Grid Int (Array Int a)

instance Show a => Show (Grid a) where
  show grid = concatMap toCell $ zip [0..] (coords grid)
    where
      toCell (i, c) =
        if (i `mod` gridWidth grid) - gridWidth grid == -1
          then show (lookup c grid) ++ "\n"
          else show (lookup c grid)

instance Functor Grid where
  fmap f (Grid width arr) = Grid width (fmap f arr)

isCoord :: Coord -> Grid a -> Bool
isCoord (Coord row col) grid =
  row >= 0
    && col >= 0
    && row < gridHeight grid
    && col < gridWidth grid

toList :: Grid a -> [a]
toList (Grid _ arr) = Array.elems arr

fromList :: Int -> [a] -> Grid a
fromList width as = Grid width $ Array.listArray (0, length as - 1) as

lookup :: Coord -> Grid a -> a
lookup (Coord row col) (Grid width arr) = arr Array.! (row * width + col)

gridHeight :: Grid a -> Int
gridHeight (Grid width arr) = arrLen `div` width
  where
    arrLen =
      let (_init, term) = Array.bounds arr
       in term + 1

gridWidth :: Grid a -> Int
gridWidth (Grid width _) = width

coords :: Grid a -> [Coord]
coords grid = [Coord row col | row <- [0..gridHeight grid - 1], col <- [0..gridWidth grid - 1]]

data EolState = EolState
  { charsEatenBeforeSpace :: Int
  }
  deriving Show

parser :: forall a. Advent.Parser a -> Advent.Parser (Grid a)
parser cellParser = do
  (as, eolState) <- runStateT innerParser (EolState 0)
  pure $ fromList (charsEatenBeforeSpace eolState) as
  where
    innerParser :: StateT EolState (Parsec Void Text) [a]
    innerParser = do
      preEol <- flip loopM [] $ \results -> do
        val <- parseCellPreEol
        case val of
          Nothing -> pure $ Right results
          Just oneMore -> pure $ Left $ oneMore : results
      postEol <- manyTill parseCellPostEol eof
      pure $ (reverse preEol) ++ postEol

    parseCellPostEol :: StateT EolState (Parsec Void Text) a
    parseCellPostEol = lift cellParser <* many eol

    parseCellPreEol :: StateT EolState (Parsec Void Text) (Maybe a)
    parseCellPreEol = do
      res <- try (Just <$> lift cellParser) <|> try (eol *> pure Nothing)
      case res of
        Just _ -> do
          modify $ \s -> s { charsEatenBeforeSpace = s.charsEatenBeforeSpace + 1 }
          pure res
        Nothing -> pure Nothing
