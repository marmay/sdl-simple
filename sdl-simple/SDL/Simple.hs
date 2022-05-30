{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module SDL.Simple
  ( Color( .. )
  , Action( .. )
  , ActionEvent( .. )
  , ActionState( .. )
  , Player( .. )
  , SimpleGame( .. )
  , V2( .. )
  , Pos( .. )
  , drawCircle
  , fillRect
  , fillCircle
  , runGame
  ) where

import Control.Lens ((^.))
import Control.Monad (void, foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.State as Trans
import qualified SDL
import qualified SDL.Primitive as SDL
import SDL (($=))
import Linear.V2
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Foreign.C.Types
import qualified GHC.Word

type Pos = V2 Int
type Size = Pos

type SDLPos = SDL.V2 Foreign.C.Types.CInt

x :: V2 a -> a
x v = v ^. _x

y :: V2 a -> a
y v = v ^. _y

toSDLPos :: Pos -> SDLPos
toSDLPos (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

data Color
  = White
  | Black
  | Red
  | Green
  | Blue
  | RGB Int Int Int

type SDLColor = SDL.V4 GHC.Word.Word8
toSDLColor :: Color -> SDLColor
toSDLColor White  = SDL.V4 255 255 255 255
toSDLColor Black  = SDL.V4   0   0   0 255
toSDLColor Red    = SDL.V4 255   0   0 255
toSDLColor Green  = SDL.V4   0 255   0 255
toSDLColor Blue   = SDL.V4   0   0 255 255
toSDLColor (RGB r g b) = SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

withWindow :: MonadIO m => T.Text -> Size -> (SDL.Window -> m a) -> m ()
withWindow title size op = do
  SDL.initialize []
  w <- SDL.createWindow title $ SDL.defaultWindow { SDL.windowInitialSize = toSDLPos size }
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w
  SDL.quit

data MainLoopIntent
  = Quit

type PlayerActionMap = M.Map PlayerAction Int

translateEvents :: Int -> [SDL.Event] -> Trans.State PlayerActionMap (Either MainLoopIntent [ActionEvent])
translateEvents tick = updatePlayerActionMap . foldM (\k e -> translateEventPayload k (SDL.eventPayload e)) [] . reverse
  where
    updatePlayerActionMap :: Either MainLoopIntent [ActionEvent] -> Trans.State PlayerActionMap (Either MainLoopIntent [ActionEvent])
    updatePlayerActionMap (Left i) = pure $ Left i
    updatePlayerActionMap (Right evs) = do
      playerActionMap <- Trans.get
      Trans.put $ foldl updatePlayerActionMap' playerActionMap evs
      pure $ Right (evs ++ playerActionMapEvents playerActionMap)
    updatePlayerActionMap' :: PlayerActionMap -> ActionEvent -> PlayerActionMap
    updatePlayerActionMap' playerActionMap ActionEvent{ player, action, state } =
      case state of
        Pressed   -> M.insert (PlayerAction player action) tick playerActionMap
        Released  -> M.delete (PlayerAction player action) playerActionMap
        _         -> playerActionMap
    playerActionMapEvents :: PlayerActionMap -> [ActionEvent]
    playerActionMapEvents playerActionMap = map (\(playerAction, sinceTick) -> ActionEvent { player = player' playerAction, action = action' playerAction, state = Held (tick - sinceTick) }) $ M.toList playerActionMap
    translateEventPayload :: [ActionEvent] -> SDL.EventPayload -> Either MainLoopIntent [ActionEvent]
    translateEventPayload _ SDL.QuitEvent = Left Quit
    translateEventPayload r (SDL.KeyboardEvent k) = translateKeyboardEvent r k
    translateEventPayload r _ = Right r
    translateKeyboardEvent :: [ActionEvent] -> SDL.KeyboardEventData -> Either MainLoopIntent [ActionEvent]
    translateKeyboardEvent r (SDL.KeyboardEventData _ motion repeated keysym) =
      if repeated
         then Right r
         else case SDL.keysymKeycode keysym of
                SDL.KeycodeEscape     -> Left Quit
                SDL.KeycodeLeft       -> process $ PlayerAction Player1 ActionLeft
                SDL.KeycodeRight      -> process $ PlayerAction Player1 ActionRight
                SDL.KeycodeUp         -> process $ PlayerAction Player1 ActionUp
                SDL.KeycodeDown       -> process $ PlayerAction Player1 ActionDown
                SDL.KeycodeKPEnter    -> process $ PlayerAction Player1 ActionA
                SDL.KeycodeBackspace  -> process $ PlayerAction Player1 ActionA
                _ -> Right r
      where actionState = case motion of
                         SDL.Pressed -> Pressed
                         SDL.Released -> Released
            process :: PlayerAction -> Either MainLoopIntent [ActionEvent]
            process playerAction = Right $ process' playerAction : r
            process' :: PlayerAction -> ActionEvent
            process' playerAction = ActionEvent { player = player' playerAction, action = action' playerAction, state = actionState }

runGame :: SimpleGame g => g -> IO ()
runGame g =
  withWindow (windowTitle g) (windowSize g) $ \window -> do
    renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { rendererType = SDL.AcceleratedRenderer, rendererTargetTexture = False }
    SDL.clear renderer
    SDL.present renderer
    Trans.runStateT (mainLoop g window renderer) initialGameState

data GameState = GameState
  { playerActionMap :: PlayerActionMap
  , fullScreen :: Bool
  , tickCount :: Int
  }

initialGameState = GameState { playerActionMap = M.empty, fullScreen = False, tickCount = 0 }

mainLoop :: SimpleGame g => g -> SDL.Window -> SDL.Renderer -> Trans.StateT GameState IO ()
mainLoop g window renderer = do
  SDL.rendererDrawColor renderer $= toSDLColor (clearColor g)
  SDL.clear renderer
  gameState <- Trans.get
  (translatedEvents, newKeyMap) <- flip Trans.runState (playerActionMap gameState) . translateEvents (tickCount gameState) <$> SDL.pollEvents

  case translatedEvents of
    Left Quit -> pure ()
    Right keys -> do
      let g' = update g (tickCount gameState) keys
      liftIO $ Trans.runStateT (draw g') renderer
      SDL.present renderer
      SDL.delay 16
      Trans.put $ gameState { playerActionMap = newKeyMap, tickCount = tickCount gameState + 1 }
      mainLoop g' window renderer

type Draw = Trans.StateT SDL.Renderer IO ()

withRenderer :: (SDL.Renderer -> Draw) -> Draw
withRenderer fn = do
  renderer <- Trans.get
  fn renderer
  pure ()

fillRect :: Pos -> Pos -> Color -> Draw
fillRect topLeft bottomRight color = withRenderer $ \renderer -> do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P $ toSDLPos topLeft) (toSDLPos bottomRight)))

fillCircle :: Pos -> Int -> Color -> Draw
fillCircle center radius color = withRenderer $ \renderer -> do
  SDL.fillCircle renderer (toSDLPos center) (fromIntegral radius) (toSDLColor color)

drawCircle :: Pos -> Int -> Color -> Draw
drawCircle center radius color = withRenderer $ \renderer -> do
  SDL.circle renderer (toSDLPos center) (fromIntegral radius) (toSDLColor color)

data Player
  = Player1
  | Player2
  deriving (Eq, Ord)

data Action
  = ActionUp
  | ActionDown
  | ActionLeft
  | ActionRight
  | ActionA
  | ActionB
  deriving (Eq, Ord)

data PlayerAction = PlayerAction
  { player' :: Player
  , action' :: Action
  }
  deriving (Eq, Ord)

data ActionState
  = Pressed
  | Released
  | Held Int

data ActionEvent = ActionEvent
  { player :: Player
  , action :: Action
  , state :: ActionState
  }

class SimpleGame a where
  -- Diese Funktion wird vor dem Zeichnen aufgerufen. Sie verarbeitet
  -- einerseits Tastatureingaben und aktualisiert andererseits den
  -- Zustand des Spiels.
  --
  -- Später kannst du diese Funktion selbst implementieren. Für's erste
  -- genügt es jedoch, wenn du die Funktionen "tick" und/oder "handleKey"
  -- implementierst.
  update :: a -> Int -> [ActionEvent] -> a
  update g t ev = tick (foldl handleKey g ev) t
    where handleKey g ev = handleAction g ev

  -- Macht einen Zeitschritt in deinem Spiel.
  tick :: a -> Int -> a
  tick g _ = g

  -- Bestimmt, wie sich der Zustand des Spiels ändert, wenn eine Taste
  -- gedrückt wird.
  handleAction :: a -> ActionEvent -> a
  handleAction g _ = g

  -- Diese Funktion zeichnet den aktuellen Zustand des Spiels.
  draw :: a -> Draw
  draw _ = pure ()

  -- windowTitle legt den Namen des Fensters fest.
  windowTitle :: a -> T.Text
  windowTitle _ = "Mein Spiel"

  -- windowSize legt die Größe des Fensters fest. Der erste Wert gibt
  -- die Breite, der zweite die Höhe des Fensters an.
  windowSize :: a -> Size
  windowSize _ = SDL.V2 320 240

  clearColor :: a -> Color
  clearColor _ = Black
