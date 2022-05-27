module Main where

import SDL.Simple
import Control.Monad (forM_)
import System.Random (randoms, mkStdGen)
import qualified Data.Map as M

data SnakeGame = SnakeGame
  { gridSize       :: Pos
  , snake          :: [Pos]
  , snakeLength    :: Int
  , snakeDirection :: Pos
  , apples         :: M.Map Pos Int
  }

gridCellSize :: Int
gridCellSize = 10

toScreen :: Pos -> Pos
toScreen (V2 x y) = (V2 (gridCellSize * x) (gridCellSize * y))

wrapToGrid :: Pos -> Pos -> Pos
wrapToGrid (V2 w h) (V2 x y) = V2 (wrap 0 (w - 1) x) (wrap 0 (h - 1) y)
  where wrap from to val =
          if val < from
          then to
          else if val > to
               then from
               else val

step :: SnakeGame -> SnakeGame
step g = g{ snake = newSnake }
  where newSnake = take (snakeLength g) (newHead : snake g)
        newHead = wrapToGrid (gridSize g) $ (head $ snake g) + (snakeDirection g)

makeApple :: Int -> SnakeGame -> SnakeGame
makeApple t g = g { apples = M.insert pos t (apples g) }
  where pos                = V2 (randomX `mod` gridX) (randomY `mod` gridY)
        [randomX, randomY] = take 2 $ randoms (mkStdGen t)
        (V2 gridX gridY)   = gridSize g

instance SimpleGame SnakeGame where
  tick g t = foldl (\g h -> if t `mod` (fst h) == 0 then (snd h) g else g) g handlers
    where handlers = [(30, step), (600, makeApple t)]

  handleKeyPressed g KeyUp    = g { snakeDirection = (V2 0 (-1)) }
  handleKeyPressed g KeyDown  = g { snakeDirection = (V2 0 1) }
  handleKeyPressed g KeyLeft  = g { snakeDirection = (V2 (-1) 0) }
  handleKeyPressed g KeyRight = g { snakeDirection = (V2 1 0) }
  handleKeyPressed g _        = g

  draw g = do
    forM_ (M.keys $ apples g) $ \cell ->
      fillCircle (toScreen cell + (V2 (gridCellSize `div` 2) (gridCellSize `div` 2))) (gridCellSize `div` 2) Red
    forM_ (snake g) $ \cell ->
      fillRect (toScreen cell) (V2 gridCellSize gridCellSize) Green

main :: IO ()
main = runGame $ SnakeGame { gridSize = V2 30 20
                           , snake = [(V2 14 9)]
                           , snakeLength = 3
                           , snakeDirection = (V2 1 0)
                           , apples = M.empty
                           }
