module Main where

import SDL.Simple
import Control.Monad (forM_)
import System.Random (RandomGen, randoms, mkStdGen)
import Data.List.Split (chunksOf)
import Data.Foldable (find)
import qualified Data.Map as M
import qualified Data.Set as S

data PlayerData = PlayerData
  { snake          :: [Pos]
  , snakeColor     :: Color
  , snakeLength    :: Int
  , snakeDirection :: Pos
  , nextDirections :: [Pos]
  }

data SnakeGame = SnakeGame
  { gridSize       :: Pos
  , players        :: M.Map Player PlayerData
  , apples         :: M.Map Pos Int
  , blockers       :: S.Set Pos
  , nextStep       :: Int
  , nextApple      :: Int
  }

initialGame :: SnakeGame
initialGame = SnakeGame { gridSize = V2 32 24
                        , players = M.fromList
                          [ ( Player1
                            , PlayerData
                              { snake = [V2 15 11]
                              , snakeColor = Cyan
                              , snakeLength = 3
                              , snakeDirection = V2 1 0
                              , nextDirections = []
                              }
                            )
                          , ( Player2
                            , PlayerData
                              { snake = [V2 14 11]
                              , snakeColor = Yellow
                              , snakeLength = 3
                              , snakeDirection = V2 (-1) 0
                              , nextDirections = []
                              }
                            )
                          ]
                        , apples = M.empty
                        , blockers = S.fromList $  [V2 x y | x <- [0, 31], y <- [0..9] ++ [14..23]]
                                                ++ [V2 x y | x <- [1..30], y <- [0, 23]]
                                                ++ [V2 x y | x <- [6, 7, 24, 25], y <- [10, 11, 12, 13]]
                        , nextStep = 0
                        , nextApple = 0
                        }

rateOf :: SnakeGame -> Int
rateOf g = rateOf' (maximum $ map (snakeLength . snd) $ M.toList $ players g)
  where rateOf' len = max 1 $ 20 - (len `div` 2)

-- Whenever the snake moves forward
step :: SnakeGame -> Int -> SnakeGame
step g t = (updateNextStep t . die . eat . updatePlayers) g

updatePlayers :: SnakeGame -> SnakeGame
updatePlayers g = g{ players = M.map (updatePlayer g) (players g) }

updatePlayer :: SnakeGame -> PlayerData -> PlayerData
updatePlayer g = move g . turn

updateNextStep :: Int -> SnakeGame -> SnakeGame
updateNextStep t g = g { nextStep = t + rateOf g }

turn :: PlayerData -> PlayerData
turn p = p{ snakeDirection = newDirection, nextDirections = newNextDirections }
  where (newDirection, newNextDirections) = turn' (snakeDirection p) (nextDirections p)
        turn' :: Pos -> [Pos] -> (Pos, [Pos])
        turn' pos (next:more)
          | pos + next /= V2 0 0    = (next, more)
          | otherwise               = turn' pos more
        turn' pos [] = (pos, [])

move :: SnakeGame -> PlayerData -> PlayerData
move g p = p{ snake = newSnake }
  where newSnake = take (snakeLength p) (newHead : snake p)
        newHead = wrapToGrid (gridSize g) $ head (snake p) + snakeDirection p

wrapToGrid :: Pos -> Pos -> Pos
wrapToGrid (V2 w h) (V2 x y) = V2 (wrap 0 (w - 1) x) (wrap 0 (h - 1) y)
  where wrap from to val
          | val < from    = to
          | val > to      = from
          | otherwise     = val

die :: SnakeGame -> SnakeGame
die g = if any (dies g) (M.keys $ players g) then initialGame else g

dies :: SnakeGame -> Player -> Bool
dies g p = pos `elem` tail thisSnake ||
           pos `elem` concat otherSnakes ||
           pos `S.member` blockers g
  where
    (Just thisSnake) = snake <$> M.lookup p (players g)
    otherSnakes = map (snake . snd) $ filter ((/= p) . fst) $ M.toList $ players g
    pos = head thisSnake

eat :: SnakeGame -> SnakeGame
eat g = g{ players = newPlayers, apples = newApples }
  where
    newPlayers = M.map (\p -> if M.member (head (snake p)) (apples g) then p { snakeLength = snakeLength p + 1 } else p) (players g)
    newApples = foldr (M.delete . head . snake . snd) (apples g) (M.toList (players g))

-- Update direction of snake
updateSnakeDirection :: SnakeGame -> Player -> Pos -> SnakeGame
updateSnakeDirection g p dir = g { players = M.adjust (\pd -> pd{ nextDirections = take 1 (nextDirections pd) ++ [dir] }) p (players g) }

-- Generate apples for the snake to eat.
makeApple :: SnakeGame -> Int -> SnakeGame
makeApple g t = updateNextApple t $ case randomFreePos 3 t g of
                  Just applePos -> g { apples = M.insert applePos t (apples g) }
                  _             -> g

updateNextApple :: Int -> SnakeGame -> SnakeGame
updateNextApple t g = g { nextApple = t + 600 }

randomFreePos :: Int -> Int -> SnakeGame -> Maybe Pos
randomFreePos maxIt seed g = find (isFree g) $ take maxIt $ randomPositions (mkStdGen seed) (gridSize g)

isFree :: SnakeGame -> Pos -> Bool
isFree g pos = pos `notElem` concatMap snake (players g) &&
               not (pos `M.member` apples g) &&
               not (pos `S.member` blockers g)

randomPositions :: RandomGen g => g -> Pos -> [Pos]
randomPositions gen rng = map (toRandomPosition rng) (chunksOf 2 $ randoms gen)
  where toRandomPosition (V2 w h) [x, y] = V2 (x `mod` w) (y `mod` h)
        toRandomPosition _ _ = error "This can never happen, as we are only dealing with chunks of 2!"

gridCellSize :: Int
gridCellSize = 10

toScreen :: Pos -> Pos
toScreen (V2 x y) = V2 (gridCellSize * x) (gridCellSize * y)

instance SimpleGame SnakeGame where
  tick g t = foldl (\g h -> if t >= fst h then snd h g t else g) g handlers
    where handlers = [(nextStep g, step), (nextApple g, makeApple)]

  handleAction g (ActionEvent p action Pressed) =
    case directionOf action of
      Just dir -> updateSnakeDirection g p dir
      Nothing  -> g
    where directionOf ActionUp    = Just $ V2 0 (-1)
          directionOf ActionDown  = Just $ V2 0 1
          directionOf ActionLeft  = Just $ V2 (-1) 0
          directionOf ActionRight = Just $ V2 1 0
          directionOf _           = Nothing
  handleAction g _ = g

  draw g = do
    forM_ (M.keys $ apples g) $ \cell ->
      fillCircle (toScreen cell + V2 (gridCellSize `div` 2) (gridCellSize `div` 2)) (gridCellSize `div` 2) Red
    forM_ (players g) $ \player ->
      forM_ (snake player) $ \cell ->
        fillRect (toScreen cell) (V2 gridCellSize gridCellSize) (snakeColor player)
    forM_ (blockers g) $ \cell ->
      fillRect (toScreen cell) (V2 gridCellSize gridCellSize) White

main :: IO ()
main = runGame initialGame
