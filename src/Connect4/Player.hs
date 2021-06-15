module Connect4.Player
  ( Player(..)
  , playerInt
  ) where

data Player = P1 | P2 deriving Show

playerInt :: Player -> Int
playerInt P1 = 1
playerInt P2 = 2