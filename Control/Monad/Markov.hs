{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Markov
  ( MarkovT()
  , runMarkovT
  , Markov()
  , runMarkov
  )
  where

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Identity

newtype MarkovT g s m a = MarkovT (RandT g (StateT s m) a)
  deriving(Functor, Monad, MonadRandom, MonadState s)

runMarkovT :: (Monad m, RandomGen g) => MarkovT g s m a -> g -> s -> m ((a, g), s)
runMarkovT (MarkovT m) g s = runStateT (runRandT m g) s

newtype Markov g s a = Markov (MarkovT g s Identity a)
  deriving(Functor, Monad, MonadRandom, MonadState s)

runMarkov :: RandomGen g => Markov g s a -> g -> s -> ((a, g), s)
runMarkov (Markov m) g s = runIdentity $ runMarkovT m g s

class (RandomGen g, Monad m, MonadState s m, MonadRandom m) => MonadMarkov g s m | m -> s

instance (Monad m, RandomGen g) => MonadMarkov g s (MarkovT g s m)
