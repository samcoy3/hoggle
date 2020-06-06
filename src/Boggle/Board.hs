module Boggle.Board
 (randomBoard,
  neighbours,
  Point,
  Board) where

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Primitive

import System.Random.MWC as MWC
import System.Random.MWC.Distributions as Dist

import qualified Data.Vector as V
import Data.Vector (Vector)

type Point = (Int, Int)
type Tile = String
type Board = M.Map Point Tile

neighbours :: Point -> [Point]
neighbours (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x, y) /= (x', y')]

tileBag :: Vector Tile
tileBag = V.fromList .
  concat .
  (map (\(l, f) -> replicate f l)) $
  [
    ("A", 9),
    ("B", 2),
    ("C", 2),
    ("D", 4),
    ("E", 12),
    ("F", 2),
    ("G", 3),
    ("H", 2),
    ("I", 9),
    ("J", 1),
    ("K", 1),
    ("L", 4),
    ("M", 2),
    ("N", 6),
    ("O", 8),
    ("P", 2),
    ("QU", 1),
    ("R", 6),
    ("S", 4),
    ("T", 6),
    ("U", 4),
    ("V", 2),
    ("W", 2),
    ("X", 1),
    ("Y", 2),
    ("Z", 1)
  ]

randomBoard :: Int -> Gen RealWorld -> IO Board
randomBoard size rng = do
  tiles <- randomTiles rng
  return . M.fromList . zip [(a, b) | a <- [1..size], b <- [1..size]] $ V.toList tiles

randomTiles :: Gen RealWorld -> IO (Vector Tile)
randomTiles gen = uniformShuffle tileBag gen
