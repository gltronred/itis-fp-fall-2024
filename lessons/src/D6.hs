-- |

module D6 where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

thread d k = do
  putStrLn $ "Hello from Thread " ++ show k
  threadDelay d
  putStrLn $ "Bye from Thread " ++ show k

run = do
  forM_ [1..3] $ \k -> do
    forkIO $ thread 3000000 k
  thread 4000000 0
