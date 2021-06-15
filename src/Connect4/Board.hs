{-# LANGUAGE TupleSections #-}

module Connect4.Board
  ( flipBoard
  , placePiece
  ) where

import Data.Matrix (Matrix, getCol, matrix, setElem)
import Data.Vector (elemIndex)

import Connect4.Player (Player, playerInt)

flipBoard :: Matrix Int -> Matrix Int
flipBoard = (flipper *)
  where flipper = matrix 6 6 (\(i,j) -> fromEnum (i + j == 7))

piecePosition :: Int -> Matrix Int -> Maybe (Int, Int)
piecePosition col board = (, col) <$> row
  where row = succ <$> elemIndex 0 vec
        vec = getCol col board

placePiece :: Player -> Matrix Int -> Int -> Maybe (Matrix Int)
placePiece player board col = setPiece <$> piecePosition col board
  where setPiece pos = setElem (playerInt player) pos board