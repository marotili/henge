module Main where

import FRP.Sodium
import System.Time
    
-- test :: Reactive Int
-- test = do
--   (eTime, pushTime) <- newEvent
  -- return pushTime

getTime :: IO Double
getTime = do
    (TOD sec pico) <- getClockTime
    return $!
        (fromIntegral sec) +
        (fromIntegral pico) / 1000000000000

main = do
  (eTime, pushTime) <- sync newEvent
  beh <- sync $ hold 0 eTime
  r <- sync $ sample beh
  sync $ pushTime 1
  r2 <- sync $ sample beh
  print (r, r2)
  return ()


       
