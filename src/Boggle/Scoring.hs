module Boggle.Scoring where

import qualified Boggle.Board as B
import Boggle.Board (Point, Board)

import qualified Data.Map as M
import Data.List
import Data.Maybe

import System.IO.Unsafe

wordList :: [String]
wordList = map (filter (/= '\r')) . lines . unsafePerformIO . readFile $ "resources/words.txt"

isWord :: String -> Bool
isWord = flip elem wordList

isOnBoard :: Board -> String -> Bool
isOnBoard board word = isOnBoard' word [] (M.keys board) where
  isOnBoard' remainingWord touchedTiles validNextSquares
          | remainingWord == "" = True
          | validNextSquares == [] = False
          | otherwise = or
                      . map (\s -> isOnBoard'
                               (fromJust $ stripPrefix (board M.! s) remainingWord)
                               (s : touchedTiles)
                               (B.neighbours s))
                      . filter (\s ->
                                 (s `notElem` touchedTiles)
                                 && fmap (`isPrefixOf` remainingWord) (board M.!? s)== Just True)
                      $ validNextSquares


wordScore :: String -> Int
wordScore s
  -- In this case the word is not valid, frontend JS should prevent submission of this
  | length s <= 2 = -1
  | (length s == 3) || (length s == 4) = 1
  | length s == 5 = 2
  | length s == 6 = 3
  | length s == 7 = 5
  | otherwise = 11

-- Returns a map from the word to the score that the word obtains
scoreWords :: Board -> [String] -> M.Map String Int
scoreWords board = foldr (\k m -> M.alter (scoreWord k) k m) M.empty where
  scoreWord _ (Just x) = if x == -1 then (Just $ -1) else (Just 0)
  scoreWord word Nothing = if (isOnBoard board word && isWord word)
                                   then (Just $ wordScore word)
                                   else (Just $ -1)
