{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module SDL.Simple
  ( Color( .. )
  , Key( .. )
  , KeyState( .. )
  , KeyEvent( .. )
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

type SDLColor = SDL.V4 GHC.Word.Word8
toSDLColor :: Color -> SDLColor
toSDLColor White  = SDL.V4 255 255 255 255
toSDLColor Black  = SDL.V4   0   0   0 255
toSDLColor Red    = SDL.V4 255   0   0 255
toSDLColor Green  = SDL.V4   0 255   0 255
toSDLColor Blue   = SDL.V4   0   0 255 255

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

type KeyMap = M.Map Key Int

translateEvents :: Int -> [SDL.Event] -> Trans.State KeyMap (Either MainLoopIntent [KeyEvent])
translateEvents tick = updateKeyMap . foldM (\k e -> translateEventPayload k (SDL.eventPayload e)) [] . reverse
  where
    updateKeyMap :: Either MainLoopIntent [KeyEvent] -> Trans.State KeyMap (Either MainLoopIntent [KeyEvent])
    updateKeyMap (Left i) = pure $ Left i
    updateKeyMap (Right evs) = do
      keyMap <- Trans.get
      Trans.put $ foldl updateKeyMap' keyMap evs
      pure $ Right (evs ++ keyMapEvents keyMap)
    updateKeyMap' :: KeyMap -> KeyEvent -> KeyMap
    updateKeyMap' keyMap KeyEvent{ key, state } =
      case state of
        Pressed   -> M.insert key tick keyMap
        Released  -> M.delete key keyMap
        _         -> keyMap
    keyMapEvents :: KeyMap -> [KeyEvent]
    keyMapEvents keyMap = map (\(key, sinceTick) -> KeyEvent { key = key, state = Held (tick - sinceTick) }) $ M.toList keyMap
    translateEventPayload :: [KeyEvent] -> SDL.EventPayload -> Either MainLoopIntent [KeyEvent]
    translateEventPayload _ SDL.QuitEvent = Left Quit
    translateEventPayload r (SDL.KeyboardEvent k) = translateKeyboardEvent r k
    translateEventPayload r _ = Right r
    translateKeyboardEvent :: [KeyEvent] -> SDL.KeyboardEventData -> Either MainLoopIntent [KeyEvent]
    translateKeyboardEvent r (SDL.KeyboardEventData _ motion repeated keysym) =
      if repeated
         then Right r
         else case SDL.keysymKeycode keysym of
                SDL.KeycodeEscape   -> Left Quit
                SDL.KeycodeLeft     -> process KeyLeft
                SDL.KeycodeRight    -> process KeyRight
                SDL.KeycodeUp       -> process KeyUp
                SDL.KeycodeDown     -> process KeyDown
                SDL.KeycodeSpace    -> process KeySpace
                _ -> Right r
      where keyState = toKeyState motion
            toKeyState SDL.Pressed = Pressed
            toKeyState SDL.Released = Released
            process :: Key -> Either MainLoopIntent [KeyEvent]
            process key = Right $ process' key : r
            process' :: Key -> KeyEvent
            process' key = KeyEvent { key = key, state = keyState }

runGame :: SimpleGame g => g -> IO ()
runGame g =
  withWindow (windowTitle g) (windowSize g) $ \window -> do
    renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { rendererType = SDL.AcceleratedRenderer, rendererTargetTexture = False }
    SDL.clear renderer
    SDL.present renderer
    Trans.runStateT (mainLoop g window renderer) initialGameState

data GameState = GameState
  { keyMap :: KeyMap
  , tickCount :: Int
  }

initialGameState = GameState { keyMap = M.empty, tickCount = 0 }

mainLoop :: SimpleGame g => g -> SDL.Window -> SDL.Renderer -> Trans.StateT GameState IO ()
mainLoop g window renderer = do
  SDL.rendererDrawColor renderer $= toSDLColor (clearColor g)
  SDL.clear renderer
  gameState <- Trans.get
  (translatedEvents, newKeyMap) <- flip Trans.runState (keyMap gameState) . translateEvents (tickCount gameState) <$> SDL.pollEvents

  case translatedEvents of
    Left Quit -> pure ()
    Right keys -> do
      let g' = update g (tickCount gameState) keys
      liftIO $ Trans.runStateT (draw g') renderer
      SDL.present renderer
      SDL.delay 16
      Trans.put $ GameState { keyMap = newKeyMap, tickCount = tickCount gameState + 1 }
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

data Key
  = KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeySpace
  deriving (Eq, Ord)

data KeyState
  = Pressed
  | Released
  | Held Int

data KeyEvent = KeyEvent
  { key :: Key
  , state :: KeyState
  }

class SimpleGame a where
  -- Diese Funktion wird vor dem Zeichnen aufgerufen. Sie verarbeitet
  -- einerseits Tastatureingaben und aktualisiert andererseits den
  -- Zustand des Spiels.
  --
  -- Später kannst du diese Funktion selbst implementieren. Für's erste
  -- genügt es jedoch, wenn du die Funktionen "tick" und/oder "handleKey"
  -- implementierst.
  update :: a -> Int -> [KeyEvent] -> a
  update g t ev = tick (foldl handleKey g ev) t
    where handleKey g ev = case state ev of
                             Pressed  -> handleKeyPressed g (key ev)
                             Released -> handleKeyReleased g (key ev)
                             Held ticks -> handleKeyHeld g (key ev) ticks

  -- Macht einen Zeitschritt in deinem Spiel.
  tick :: a -> Int -> a
  tick g _ = g

  -- Bestimmt, wie sich der Zustand des Spiels ändert, wenn eine Taste
  -- gedrückt wird.
  handleKeyPressed :: a -> Key -> a
  handleKeyPressed g _ = g

  -- Bestimmt, wie sich der Zustand des Spiels ändert, wenn eine Taste
  -- losgelassen wird.
  handleKeyReleased :: a -> Key -> a
  handleKeyReleased g _ = g

  -- Bestimmt, wie sich der Zustand des Spiels ändert, wenn eine Taste
  -- seit einer bestimmten Zeit gedrückt ist.
  handleKeyHeld :: a -> Key -> Int -> a
  handleKeyHeld g _ _ = g

  -- Diese Funktion zeichnet den aktuellen Zustand des Spiels.
  draw :: a -> Draw

  -- windowTitle legt den Namen des Fensters fest.
  windowTitle :: a -> T.Text
  windowTitle _ = "Mein Spiel"

  -- windowSize legt die Größe des Fensters fest. Der erste Wert gibt
  -- die Breite, der zweite die Höhe des Fensters an.
  windowSize :: a -> Size
  windowSize _ = SDL.V2 320 240

  clearColor :: a -> Color
  clearColor _ = Black
