{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Markov

-- Helper for @transition@
cumulative :: Num a => [(a, t)] -> [(a, t)]
cumulative = scanl1 f
  where f (a, _) (p, x) = (a + p, x)

-- Select a random t from a list of probabilities summing to 1.
transition :: MonadRandom m => [(Double, t)] -> m t
transition ps = do
  -- Random probability
  r <- getRandomR (0, 1)
  -- Get points greater than r. We'll take the first, or @last ps@ if there
  let gt = dropWhile ((<= r) . fst) . cumulative $ ps
  return . snd $ case gt of
                      [] -> last ps
                      x:xs -> x


data S = A | B deriving(Show, Eq)

countTwo :: (a -> Bool) -> [a] -> (Int, Int)
countTwo f xs = foldl g (0, 0) xs
  where g (a', a) x = if f x then (a'+1,a) else (a', a+1)

chain :: (MonadState S m, MonadRandom m) => m S
chain = transition [(0.2, A), (0.8, B)] >>= put >> get >>= return

main = do
  gen <- newStdGen
  let cs = replicateM 1000 chain
  let ((as,gen'), s) = runMarkov cs gen A
  {-print a-}
  let r = countTwo (==A) as
  print r
