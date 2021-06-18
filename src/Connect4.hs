module Connect4
    ( main
    ) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

import Data.Matrix ( Matrix, zero )

import Connect4.Board (placePiece)
import Connect4.Check (checkBoard)
import Connect4.Displays.Terminal (promptInput, displayBoard)
import Connect4.Player (Player(..))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  loop (zero 6 7) (cycle [P1,P2])

loop :: Matrix Int -> [Player] -> IO ()
loop _ [] = error "You can't run out of players silly..."
loop board (player:rest)  = do
  updated <- placePiece player board <$> promptInput player
  case updated of
    Just updated -> do
      displayBoard updated
      case checkBoard updated of
        Just winner -> putStrLn $ show winner ++ " wins!!"
        Nothing -> loop updated rest
    Nothing -> loop board (player:rest)