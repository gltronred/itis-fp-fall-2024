-- |

module D6 where

import Control.Monad
import System.Random

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

myForkIO :: IO a -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  forkFinally io (\_ -> putMVar mvar ())
  return mvar

thread :: TVar Int -> Int -> IO Int
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
  pure r

run :: IO ()
run = do
  v <- newTVarIO 0
  -- ждём завершения потоков (иначе основной поток завершится раньше)
  mvars <- forM [1..3] $ \k -> do
    myForkIO $ thread v k
  forM_ mvars takeMVar
  x <- atomically $ readTVar v
  print x

  -- более высокоуровневый интерфейс для запуска (см. async)
  s <- newTVarIO 0
  rs <- forConcurrently [1..3] $ thread s
  y <- readTVarIO s
  print (rs, sum rs, y)

  -- запуск с таймаутом
  res <- race (threadDelay 2_000_000) (thread s 5)
  print res
