-- |

module D6 where

import Control.Monad
import System.Random

import Control.Concurrent
import Control.Concurrent.STM

thread :: TVar Int -> Int -> IO ()
thread v k = do
  putStrLn $ "Hello from Thread " ++ show k
  d <- randomRIO (1_000_000, 3_000_000)
  threadDelay d
  r <- randomRIO (-1000, 1000)
  atomically $ do
    -- не сработает: randomRIO работает в IO!
    -- r <- randomRIO (-1000, 1000)
    modifyTVar' v (+r)
  putStrLn $ "Bye from Thread " ++ show k

run :: IO ()
run = do
  v <- newTVarIO 0
  forM_ [1..3] $ \k -> do
    forkIO $ thread v k
  x <- atomically $ readTVar v
  print x
  threadDelay 4_000_000
  y <- atomically $ readTVar v
  print y
