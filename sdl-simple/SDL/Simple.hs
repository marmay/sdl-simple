{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module SDL.Simple
  ( Color( .. )
  , Action( .. )
  , ActionEvent( .. )
  , ActionState( .. )
  , FontStyle( .. )
  , Player( .. )
  , SimpleGame( .. )
  , V2( .. )
  , Pos( .. )
  , PosF( .. )
  , drawCircle
  , drawText
  , drawText'
  , fillRect
  , fillCircle
  , runGame
  ) where

import Control.Lens ((^.))
import Control.Monad (void, foldM, forM_)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Trans.Either as Trans
import qualified Control.Monad.Trans.State as Trans
import qualified SDL
import qualified SDL.Input.GameController as SDL
import qualified SDL.Primitive as SDL
import qualified SDL.Raw.Event as SDLR
import qualified SDL.Font as SDLT
import qualified SDL.Vect as SDL
import SDL (($=))
import Linear.V2
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Foreign.C.Types
import qualified GHC.Word
import qualified System.Environment
import qualified System.Process

type Pos = V2 Int
type PosF = V2 Float
type Size = Pos

type SDLPos = SDL.V2 Foreign.C.Types.CInt

x :: V2 a -> a
x v = v ^. _x

y :: V2 a -> a
y v = v ^. _y

toSDLPos :: Pos -> SDLPos
toSDLPos (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

verifyFont :: Maybe FilePath -> IO (Maybe FilePath)
verifyFont (Just path) = verifyFont' path
verifyFont Nothing     = pure Nothing

verifyFont' :: FilePath -> IO (Maybe FilePath)
verifyFont' path = do
  pure $ Just path

fontFromEnvironment :: String -> IO (Maybe String)
fontFromEnvironment envName =
  (Just <$> System.Environment.getEnv envName)
    `catchError` const (pure Nothing)
    >>= verifyFont

fontFromFcMatch :: String -> IO (Maybe FilePath)
fontFromFcMatch pattern =
  (Just <$> System.Process.readProcess "fc-match" ["-f", "%{file}", pattern] "")
    `catchError` const (pure Nothing)
    >>= verifyFont

firstFont :: [IO (Maybe FilePath)] -> IO (Maybe FilePath)
firstFont (f:fs) = do
  f' <- f
  case f' of
    Just p    -> pure $ Just p
    Nothing   -> firstFont fs
firstFont [] = pure Nothing

defaultFont :: FontStyle -> IO (Maybe FilePath)
defaultFont RegularFont = firstFont
  [ fontFromEnvironment "SDL_SIMPLE_DEFAULT_FONT"
  , fontFromFcMatch "monospace"
  ]
defaultFont BoldFont = firstFont
  [ fontFromEnvironment "SDL_SIMPLE_DEFAULT_BOLD_FONT"
  , fontFromFcMatch "monospace:bold"
  , defaultFont RegularFont
  ]
defaultFont ItalicFont = firstFont
  [ fontFromEnvironment "SDL_SIMPLE_DEFAULT_ITALIC_FONT"
  , fontFromFcMatch "monospace:italic"
  , defaultFont RegularFont
  ]
defaultFont BoldItalicFont = firstFont
  [ fontFromEnvironment "SDL_SIMPLE_DEFAULT_BOLD_ITALIC_FONT"
  , fontFromFcMatch "monospace:bold:italic"
  , defaultFont RegularFont
  ]

defaultFontMap :: IO (M.Map FontStyle FilePath)
defaultFontMap = do
  let styles = [RegularFont, BoldFont, ItalicFont, BoldItalicFont]
  fonts <- mapM defaultFont styles
  case sequence fonts of
    (Just fonts) -> pure $ M.fromList $ styles `zip` fonts
    Nothing      -> error "Could not find all required default fonts! Please use the environment variable 'SDL_SIMPLE_DEFAULT_FONT' to provide one, as detection using fc-match does not seem to work."

data Color
  = White
  | Black
  | Red
  | Green
  | Blue
  | Cyan
  | Magenta
  | Yellow
  | RGB Int Int Int
  deriving (Eq, Ord)

type SDLColor = SDL.V4 GHC.Word.Word8
toSDLColor :: Color -> SDLColor
toSDLColor White    = SDL.V4 255 255 255 255
toSDLColor Black    = SDL.V4   0   0   0 255
toSDLColor Red      = SDL.V4 255   0   0 255
toSDLColor Green    = SDL.V4   0 255   0 255
toSDLColor Blue     = SDL.V4   0   0 255 255
toSDLColor Cyan     = SDL.V4   0 255 255 255
toSDLColor Magenta  = SDL.V4 255   0 255 255
toSDLColor Yellow   = SDL.V4 255 255   0 255
toSDLColor (RGB r g b) = SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

withWindow :: MonadIO m => T.Text -> Size -> (SDL.Window -> m a) -> m ()
withWindow title size op = do
  SDL.initialize [SDL.InitJoystick, SDL.InitGameController]
  SDLT.initialize
  w <- SDL.createWindow title $ SDL.defaultWindow { SDL.windowInitialSize = toSDLPos size }
  SDL.showWindow w
  void $ op w
  SDLT.quit
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
    translateEventPayload r (SDL.ControllerButtonEvent ev) = translateControllerButtonEvent r ev
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
                SDL.KeycodeA          -> process $ PlayerAction Player2 ActionLeft
                SDL.KeycodeD          -> process $ PlayerAction Player2 ActionRight
                SDL.KeycodeW          -> process $ PlayerAction Player2 ActionUp
                SDL.KeycodeS          -> process $ PlayerAction Player2 ActionDown
                SDL.KeycodeSpace      -> process $ PlayerAction Player2 ActionA
                SDL.KeycodeComma      -> process $ PlayerAction Player2 ActionA
                _ -> Right r
      where actionState = case motion of
                         SDL.Pressed -> Pressed
                         SDL.Released -> Released
            process :: PlayerAction -> Either MainLoopIntent [ActionEvent]
            process playerAction = Right $ process' playerAction : r
            process' :: PlayerAction -> ActionEvent
            process' playerAction = ActionEvent { player = player' playerAction, action = action' playerAction, state = actionState }

    translateControllerButtonEvent r (SDL.ControllerButtonEventData which button state) =
      case event of
        Just event' -> Right $ event' : r
        Nothing     -> Right r
      where
        event = do
          action <- actionOf button
          state' <- stateOf state
          pure $ ActionEvent { player = Player1, action = action, state = state'}
        actionOf SDL.ControllerButtonDpadUp     = Just ActionUp
        actionOf SDL.ControllerButtonDpadDown   = Just ActionDown
        actionOf SDL.ControllerButtonDpadLeft   = Just ActionLeft
        actionOf SDL.ControllerButtonDpadRight  = Just ActionRight
        actionOf SDL.ControllerButtonA          = Just ActionA
        actionOf SDL.ControllerButtonB          = Just ActionB
        actionOf _                              = Nothing
        stateOf SDL.ControllerButtonPressed     = Just Pressed
        stateOf SDL.ControllerButtonReleased    = Just Released
        stateOf _                               = Nothing

runGame :: SimpleGame g => g -> IO ()
runGame g =
  withWindow (windowTitle g) (windowSize g) $ \window -> do
    joysticks <- SDL.availableJoysticks
    forM_ joysticks $ \joystick -> do
      putStrLn "Initializing joystick."
      SDLR.gameControllerOpen 0
    renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { rendererType = SDL.AcceleratedRenderer, rendererTargetTexture = False }
    SDL.clear renderer
    SDL.present renderer
    drawDefaultFonts <- defaultFontMap
    let drawState = DrawState
                    { drawTime = tickCount initialGameState
                    , drawRenderer = renderer
                    , drawTextCache = M.empty
                    , drawFontCache = M.empty
                    , drawDefaultFonts = drawDefaultFonts
                    }
    Trans.runStateT (mainLoop g window drawState) initialGameState

data GameState = GameState
  { playerActionMap :: PlayerActionMap
  , fullScreen :: Bool
  , tickCount :: Int
  }

initialGameState = GameState { playerActionMap = M.empty, fullScreen = False, tickCount = 0 }

mainLoop :: SimpleGame g => g -> SDL.Window -> DrawState -> Trans.StateT GameState IO ()
mainLoop g window drawState = do
  SDL.rendererDrawColor (drawRenderer drawState) $= toSDLColor (clearColor g)
  SDL.clear (drawRenderer drawState)
  gameState <- Trans.get
  (translatedEvents, newKeyMap) <- flip Trans.runState (playerActionMap gameState) . translateEvents (tickCount gameState) <$> SDL.pollEvents

  case translatedEvents of
    Left Quit -> pure ()
    Right keys -> do
      let g' = update g (tickCount gameState) keys
      drawState' <- liftIO $ Trans.execStateT (draw g') drawState{ drawTime = tickCount gameState }
      SDL.present (drawRenderer drawState')
      SDL.delay 16
      Trans.put $ gameState { playerActionMap = newKeyMap, tickCount = tickCount gameState + 1 }
      mainLoop g' window drawState'

data Font = Font
  { fontFilePath          :: T.Text
  , fontPointSize         :: Int
  } deriving (Eq, Ord)

data FontStyle
  = RegularFont
  | BoldFont
  | ItalicFont
  | BoldItalicFont
  deriving (Eq, Ord)

type FontCache = M.Map Font SDLT.Font

data DrawTextParams = DrawTextParams
  { drawTextText          :: T.Text
  , drawTextColor         :: Color
  , drawTextFont          :: Font
  } deriving (Eq, Ord)

data TextCacheEntry = TextCacheEntry
  { textCacheTexture      :: SDL.Texture
  , textCacheLastUse      :: Int
  }

type TextCache = M.Map DrawTextParams TextCacheEntry

data DrawState = DrawState
  { drawTime          :: Int
  , drawRenderer      :: SDL.Renderer
  , drawTextCache     :: TextCache
  , drawFontCache     :: FontCache
  , drawDefaultFonts  :: M.Map FontStyle FilePath
  }

type Draw a = Trans.StateT DrawState IO a

createFont :: Font -> IO SDLT.Font
createFont f = do
  SDLT.load (T.unpack $ fontFilePath f) (fontPointSize f)

getFont :: Font -> Draw SDLT.Font
getFont f = do
  drawState <- Trans.get
  case M.lookup f (drawFontCache drawState) of
    (Just font) -> do
      pure font
    Nothing -> do
      font <- liftIO $ createFont f
      Trans.put drawState { drawFontCache = M.insert f font (drawFontCache drawState) }
      pure font

createTextTexture :: SDL.Renderer -> DrawTextParams -> Draw SDL.Texture
createTextTexture renderer text = do
  font <- getFont (drawTextFont text)
  surface <- SDLT.solid font (toSDLColor $ drawTextColor text) (drawTextText text)
  SDL.createTextureFromSurface renderer surface

getTextTexture :: SDL.Renderer -> DrawTextParams -> Draw SDL.Texture
getTextTexture renderer text = do
  drawState <- Trans.get
  case M.lookup text (drawTextCache drawState) of
    (Just textCacheEntry) -> do
      Trans.put drawState { drawTextCache = M.insert text textCacheEntry{ textCacheLastUse = drawTime drawState } (drawTextCache drawState) }
      pure $ textCacheTexture textCacheEntry
    Nothing -> do
      texture <- createTextTexture renderer text
      Trans.put drawState { drawTextCache = M.insert text TextCacheEntry{ textCacheTexture = texture, textCacheLastUse = drawTime drawState } (drawTextCache drawState) }
      pure texture

withRenderer :: (SDL.Renderer -> Draw a) -> Draw a
withRenderer fn = do
  drawState <- Trans.get
  fn $ drawRenderer drawState

fillRect :: Pos -> Pos -> Color -> Draw ()
fillRect topLeft bottomRight color = withRenderer $ \renderer -> do
  SDL.rendererDrawColor renderer $= toSDLColor color
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P $ toSDLPos topLeft) (toSDLPos bottomRight)))

fillCircle :: Pos -> Int -> Color -> Draw ()
fillCircle center radius color = withRenderer $ \renderer -> do
  SDL.fillCircle renderer (toSDLPos center) (fromIntegral radius) (toSDLColor color)

drawCircle :: Pos -> Int -> Color -> Draw ()
drawCircle center radius color = withRenderer $ \renderer -> do
  SDL.circle renderer (toSDLPos center) (fromIntegral radius) (toSDLColor color)

drawDefaultFont :: FontStyle -> Draw FilePath
drawDefaultFont style = do
  drawState <- Trans.get
  let (Just r) = M.lookup style (drawDefaultFonts drawState)
  pure r

drawText' :: Pos -> Int -> Color -> FontStyle -> T.Text -> Draw ()
drawText' pos pointSize color style text = do
  fontPath <- T.pack <$> drawDefaultFont style
  let params = DrawTextParams
               { drawTextText = text
               , drawTextColor = color
               , drawTextFont = Font{ fontFilePath = fontPath
                                    , fontPointSize = 12
                                    }
               }
  drawText pos params

drawText :: Pos -> DrawTextParams -> Draw ()
drawText pos text = withRenderer $ \renderer -> do
  textTexture <- getTextTexture renderer text
  target <- liftIO $ targetOf textTexture
  SDL.copy renderer textTexture Nothing (Just target)
  pure ()
    where targetOf :: SDL.Texture -> IO (SDL.Rectangle Foreign.C.Types.CInt)
          targetOf texture = do
            info <- SDL.queryTexture texture
            pure $ SDL.Rectangle (SDL.P $ toSDLPos pos) (V2 (SDL.textureWidth info) (SDL.textureHeight info))

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
  draw :: a -> Draw ()
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
