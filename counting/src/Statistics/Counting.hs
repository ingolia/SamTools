{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Statistics.Counting
       where

import Control.Monad.Primitive

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Mutable.Class as Mu
import Data.Traversable



class MCounter c where
  type CounterMonad c
  type CounterElt c
  type CounterFrozen c
  newCounter    :: (CounterMonad c) c
  incrCounter   :: c -> (CounterElt c) -> (CounterMonad c) ()
  freezeCounter :: c -> (CounterMonad c) (CounterFrozen c)

instance MCounter (IORef Int) where
  type CounterMonad (IORef Int) = IO
  type CounterElt (IORef Int) = ()
  type CounterFrozen (IORef Int) = Int
  newCounter = newIORef 0
  incrCounter ctref _x = modifyIORef' ctref succ
  freezeCounter ctref = readIORef ctref

instance (MCounter c, CounterMonad c ~ IO) => MCounter (IORef (IM.IntMap c)) where
  type CounterMonad (IORef (IM.IntMap c)) = CounterMonad c
  type CounterElt (IORef (IM.IntMap c)) = (Int, CounterElt c)
  type CounterFrozen (IORef (IM.IntMap c)) = IM.IntMap (CounterFrozen c)
  newCounter = newIORef IM.empty
  incrCounter imref (x, y) = do im <- readIORef imref
                                subct <- case IM.lookup x im of
                                  Just c -> return c
                                  Nothing -> do c <- newCounter
                                                writeIORef imref $! IM.insert x c im
                                                return c
                                incrCounter subct y
  freezeCounter imref = readIORef imref >>= traverse freezeCounter

instance (MCounter c, CounterMonad c ~ IO, Hashable k, Eq k) => MCounter (IORef (HM.HashMap k c)) where
  type CounterMonad (IORef (HM.HashMap k c)) = CounterMonad c
  type CounterElt (IORef (HM.HashMap k c)) = (k, CounterElt c)
  type CounterFrozen (IORef (HM.HashMap k c)) = HM.HashMap k (CounterFrozen c)
  newCounter = newIORef HM.empty
  incrCounter hmref (x, y) = do hm <- readIORef hmref
                                subct <- case HM.lookup x hm of
                                  Just c -> return c
                                  Nothing -> do c <- newCounter
                                                writeIORef hmref $! HM.insert x c hm
                                                return c
                                incrCounter subct y
  freezeCounter hmref = readIORef hmref >>= traverse freezeCounter
