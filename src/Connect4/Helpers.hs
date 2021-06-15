module Connect4.Helpers 
  ( ensure
  ) where

ensure :: (a -> Bool) -> Maybe a -> Maybe a
ensure condition m = do
  val <- m
  if condition val
    then return val
    else Nothing