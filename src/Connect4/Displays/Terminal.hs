module Connect4.Displays.Terminal
  ( displayBoard
  , promptInput
  ) where

import Data.Ix (inRange)
import Data.Matrix (Matrix)

import Text.Read (readMaybe)

import Connect4.Board (flipBoard)
import Connect4.Helpers (ensure)
import Connect4.Player (Player)

displayBoard :: Matrix Int -> IO ()
displayBoard = print . flipBoard

prompt :: String -> IO String
prompt text = putStr text >> getLine

promptInput :: Player -> IO Int
promptInput player = do
  line <- prompt $ show player ++ ", please select a column (1-7): "
  case ensure (inRange (1,7)) $ readMaybe line of
    Just col -> return col
    Nothing -> promptInput player