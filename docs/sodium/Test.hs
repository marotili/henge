{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import qualified           Data.Map as Map
import           FRP.Sodium
import qualified Graphics.UI.GLFW as GLFW
import           System.Time
    
import           Control.Concurrent
-- test :: Reactive Int
-- test = do
--   (eTime, pushTime) <- newEvent
  -- return pushTime

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init

    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

getTime :: IO Double
getTime = do
    (TOD sec pico) <- getClockTime
    return $!
        (fromIntegral sec) +
        
        (fromIntegral pico) / 1000000000000
        
keyCallback keyEvents win key sc keyState modifierKeys = do
  when (key == GLFW.Key'Escape) $ GLFW.setWindowShouldClose win True
  unless (keyState == GLFW.KeyState'Repeating) $ do
    let mKey = keyEvents ^. at key
    case mKey of 
      Just pushState -> sync $ pushState keyState
      Nothing -> return ()
    
type KeyMap a = Map.Map GLFW.Key a
data Keyboard = Keyboard
 { _kKeyStates :: KeyMap (Behavior GLFW.KeyState)
 , _kTimeKeyDown :: KeyMap (Behavior Double)
 , _kKeyStatePush :: KeyMap (GLFW.KeyState -> Reactive ())
 }
 
type Time = Behavior Double
type TimeDelta = Event Double

test :: Double -> GLFW.KeyState -> Double
test dt GLFW.KeyState'Pressed = dt
test dt GLFW.KeyState'Repeating = dt
test dt _ = 0

test2 :: Behavior Double -> Behavior GLFW.KeyState -> Behavior Double
test2 time keyState = test <$> time <*> keyState
      
b :: Event Double -> Behavior GLFW.KeyState -> Reactive (Behavior (Behavior Double))
b deltaTime keyState = do
  let e2 = execute $ fmap (s deltaTime) (updates keyState)
  b <- hold (pure 0) e2
  return b
  
s :: Event Double -> GLFW.KeyState -> Reactive (Behavior Double)
s dtE GLFW.KeyState'Released = return $ pure 0
s dtE _ = accum 0 (fmap (+) dtE)
 
-- newKeyboard :: Time -> IO Keyboard
newKeyboard time timeDelta = sync $ do
  (keyEvents, keyStatePush) <- fmap unzip $ mapM (const newEvent) [1..length keys]
  keyStates <- mapM (hold GLFW.KeyState'Released) keyEvents :: Reactive [Behavior GLFW.KeyState]
  
  behs <- mapM (b timeDelta) keyStates
  b' <- mapM switch behs
  --pushB (b')
  let timeKeyDown = map (test2 time) keyStates -- :: Reactive [Behavior Double]
  
  return Keyboard { _kKeyStates = Map.fromList $ zip keys keyStates
                  , _kTimeKeyDown = Map.fromList $ zip keys b'
                  , _kKeyStatePush = Map.fromList $ zip keys keyStatePush
                  }
  where keys = [GLFW.Key'Escape, GLFW.Key'A, GLFW.Key'W, GLFW.Key'S, GLFW.Key'D]
    
main = withWindow 100 100 "test" $ \win -> do
  (deltaTime, pushDeltaTime) <- sync newEvent :: IO (Event Double, Double -> Reactive ())
  t <- sync $ accum 0 (fmap (+) deltaTime)
  keyboard <- newKeyboard t deltaTime
  
  -- _ <- sync $ listen (updates (fromJust $ (_kTimeKeyDown keyboard)^.at GLFW.Key'A)) print
  
  GLFW.setKeyCallback win $ Just (keyCallback (_kKeyStatePush keyboard))
  startTime <- getTime
  loop win pushDeltaTime startTime keyboard
  where loop win pushDeltaTime startTime keyboard = do
          GLFW.swapBuffers win
          GLFW.pollEvents
          current <- getTime
          val <- sync $ do
            pushDeltaTime (current - startTime)
            sample $ fromJust $ (_kTimeKeyDown keyboard)^.at GLFW.Key'A
          print val
          close <- GLFW.windowShouldClose win
          threadDelay $ 1000 * (1000 `div` 120 - 1000 * round (current - startTime))
          unless close $ loop win pushDeltaTime current keyboard
       
