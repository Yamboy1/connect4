module Connect4.Check
  ( checkBoard
  ) where

import Data.List (isInfixOf)
import Data.Matrix (Matrix, toLists, transpose)
import Data.Universe.Helpers (diagonals)

import Connect4.Board (flipBoard)
import Connect4.Player (Player(..), playerInt)

checkList :: [Int] -> Maybe Player
checkList xs
  | hasFourInARow P1 = Just P1
  | hasFourInARow P2 = Just P2
  | otherwise = Nothing
  where hasFourInARow x = replicate 4 (playerInt x) `isInfixOf` xs

checkBoard :: Matrix Int -> Maybe Player 
checkBoard board = foldr f Nothing lists
  where f _ (Just player) = Just player
        f xs _ = checkList xs
        lists = concatMap ($ board) [toLists,
                                     toLists . transpose,
                                     diagonals . toLists,
                                     diagonals . toLists . flipBoard]