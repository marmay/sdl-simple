module Main where

import SDL.Simple
import Control.Monad (forM_)
import System.Random (RandomGen, randoms, mkStdGen)
import Data.List.Split (chunksOf)
import Data.Foldable (find)
import qualified Data.Map as M

data SnakeGame = SnakeGame
  { gridSize       :: Pos
  , snake          :: [Pos]
  , snakeLength    :: Int
  , snakeDirection :: Pos
  , apples         :: M.Map Pos Int
  }

initialGame :: SnakeGame
initialGame = SnakeGame { gridSize = V2 32 24
                        , snake = [V2 15 11]
                        , snakeLength = 3
                        , snakeDirection = V2 1 0
                        , apples = M.empty
                        }

rateOf :: SnakeGame -> Int
rateOf g = rateOf' (snakeLength g)
  where rateOf' len = 30 - len

gridCellSize :: Int
gridCellSize = 10

toScreen :: Pos -> Pos
toScreen (V2 x y) = V2 (gridCellSize * x) (gridCellSize * y)

wrapToGrid :: Pos -> Pos -> Pos
wrapToGrid (V2 w h) (V2 x y) = V2 (wrap 0 (w - 1) x) (wrap 0 (h - 1) y)
  where wrap from to val
          | val < from    = to
          | val > to      = from
          | otherwise     = val

die :: SnakeGame -> SnakeGame
die g = if isDeadly g (head (snake g)) then initialGame else g

eat :: SnakeGame -> SnakeGame
eat g =
  if M.member snakeHead (apples g)
  then g{ snakeLength = snakeLength g + 1
        , apples = M.delete snakeHead (apples g)
        }
  else g
  where snakeHead = head $ snake g

step :: SnakeGame -> SnakeGame
step g = eat $ g{ snake = newSnake }
  where newSnake = take (snakeLength g) (newHead : snake g)
        newHead = wrapToGrid (gridSize g) $ head (snake g) + snakeDirection g

isDeadly :: SnakeGame -> Pos -> Bool
isDeadly g pos = pos `notElem` tail (snake g)

isFree :: SnakeGame -> Pos -> Bool
isFree g pos = pos `notElem` snake g &&
               not (pos `M.member` apples g)

randomPositions :: RandomGen g => g -> Pos -> [Pos]
randomPositions gen rng = map (toRandomPosition rng) (chunksOf 2 $ randoms gen)
  where toRandomPosition (V2 w h) [x, y] = V2 (x `mod` w) (y `mod` h)
        toRandomPosition _ _ = error "This can never happen, as we are only dealing with chunks of 2!"

randomFreePos :: Int -> Int -> SnakeGame -> Maybe Pos
randomFreePos maxIt seed g = find (isFree g) $ take maxIt $ randomPositions (mkStdGen seed) (gridSize g)

makeApple :: Int -> SnakeGame -> SnakeGame
makeApple t g = case randomFreePos 3 t g of
                  Just applePos -> g { apples = M.insert applePos t (apples g) }
                  _             -> g

instance SimpleGame SnakeGame where
  tick g t = foldl (\g h -> if t `mod` fst h == 0 then snd h g else g) g handlers
    where handlers = [(rateOf g, step), (600, makeApple t)]

  handleKeyPressed g KeyUp    = g { snakeDirection = V2 0 (-1) }
  handleKeyPressed g KeyDown  = g { snakeDirection = V2 0 1 }
  handleKeyPressed g KeyLeft  = g { snakeDirection = V2 (-1) 0 }
  handleKeyPressed g KeyRight = g { snakeDirection = V2 1 0 }
  handleKeyPressed g _        = g

  draw g = do
    forM_ (M.keys $ apples g) $ \cell ->
      fillCircle (toScreen cell + V2 (gridCellSize `div` 2) (gridCellSize `div` 2)) (gridCellSize `div` 2) Red
    forM_ (snake g) $ \cell ->
      fillRect (toScreen cell) (V2 gridCellSize gridCellSize) Green

main :: IO ()
main = runGame initialGame
