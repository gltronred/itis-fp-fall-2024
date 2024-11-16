module D3_2 where

import Control.Monad.Reader
import Control.Monad.Writer

f3 :: (MonadReader Int m,
       MonadWriter [String] m,
       MonadIO m)
   => m ()
f3 = do
  r <- ask
  tell ["Env: " ++ show r]
  s <- liftIO getLine
  tell ["String 1: " ++ s]
  t <- liftIO getLine
  tell ["String 2: " ++ t]
  liftIO $ print $ r * (length s + length t)
