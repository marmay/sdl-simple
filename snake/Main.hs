module Main where

import SDL.Simple

data Snake = Snake

instance SimpleGame Snake where
  draw g = do
    drawCircle (V2 10 10) 20 Red

main :: IO ()
main = runGame $ Snake
