{-# LANGUAGE RankNTypes #-}
module Main
       where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.List (sort, sortBy)
import Data.Ord
import Data.Traversable
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import System.IO

import Criterion.Main

main = do
  forM_ [ mvector, foldIntMap, iorefIntMap, foldHashMap, iorefHashMap] $ \counter ->
    verifyCounter (intSourceCyclic littleNum) counter 17
  defaultMain
    [ bgroup "mvector" $ counterBenchGroup mvector
    , bgroup "foldIntMap" $ counterBenchGroup foldIntMap
    , bgroup "iorefIntMap" $ counterBenchGroup iorefIntMap
    , bgroup "foldHashMap" $ counterBenchGroup foldHashMap
    , bgroup "iorefHashMap" $ counterBenchGroup iorefHashMap
    ]

counterBenchGroup :: Counter Int IO -> [Benchmark]
counterBenchGroup counter =
  [ bench "cyclic" $ nfIO $ counter (intSourceCyclic numElts numReps)
  , bench "blocked" $ nfIO $ counter (intSourceBlocked numElts numReps)
  , bench "cyclic-small-many" $ nfIO $ counter (intSourceCyclic littleNum bigNum)
  , bench "cyclic-big-few" $ nfIO $ counter (intSourceCyclic bigNum littleNum)
  ]
--intSourceScatter :: (Monad m) => Int -> Int -> C.Producer m Int
--intSourceScatter rng nrep = C.enumFromTo 1 (rng * nrep) C.=$= C.map (* (rng `div` 2 - 1)) C.=$= C.map (`mod` rng) 

intSourceCyclic :: (Monad m) => Int -> Int -> C.Producer m Int
intSourceCyclic rng nrep = C.enumFromTo 1 (rng * nrep) C.=$= C.map (`mod` rng) 

intSourceBlocked :: (Monad m) => Int -> Int -> C.Producer m Int
intSourceBlocked rng nrep = C.enumFromTo 0 (rng * nrep - 1) C.=$= C.map (`div` nrep)

numElts, numReps :: Int
littleNum = 101
numElts = 997
numReps = 1009
bigNum = 10007
--numElts = 3331
--numReps = 3343

verifyCounter :: (Ord a, Eq a, Show a) => (Int -> C.Producer IO a) -> (Counter a IO) -> Int -> IO ()
verifyCounter source counter nreps = do
  values <- C.runConduit $ source 1 C.=$= C.consume
  let exp = [(v, nreps) | v <- sort values]
  unsorted <- counter $ source nreps
  let sorted = sortBy (comparing fst) unsorted
  when (sorted /= exp) $ do
    hPutStrLn stderr "Count failed!"
    hPutStrLn stderr . show . take 17 $ exp
    hPutStrLn stderr . show . take 17 $ sorted

type Counter a m = C.Producer m a -> m [(a, Int)]
--newtype Counter a m = Counter { countify :: C.Producer m a -> m [(a, Int)] }

foldHashMap :: (Eq a, Hashable a, Monad m) => Counter a m
foldHashMap src = do ctmap <- C.runConduit $ src C.=$= C.fold (\hm x -> HM.insertWith (+) x 1 hm) HM.empty
                     return $! HM.toList ctmap

iorefHashMap :: (Monad m, MonadIO m, Eq a, Hashable a, Functor m) => Counter a m
iorefHashMap src = do ctmapref <- liftIO $ newIORef HM.empty
                      C.runConduit $ src C.=$= C.mapM_ (liftIO . countMapRef ctmapref)
                      HM.toList <$> (liftIO $ readIORef ctmapref >>= traverse readIORef)
  where countMapRef ctmapref x = do ctmap <- readIORef ctmapref
                                    ctref <- case HM.lookup x ctmap of
                                      Just ref -> return ref
                                      Nothing -> do ref <- newIORef 0
                                                    writeIORef ctmapref $! HM.insert x ref ctmap
                                                    return ref
                                    modifyIORef' ctref succ

foldIntMap :: (Monad m) => Counter Int m
foldIntMap src = do ctmap <- C.runConduit $ src C.=$= C.fold (\hm x -> IM.insertWith (+) x 1 hm) IM.empty
                    return $! IM.toList ctmap

iorefIntMap :: (Monad m, MonadIO m, Functor m) => Counter Int m
iorefIntMap src = do ctmapref <- liftIO $ newIORef IM.empty
                     C.runConduit $ src C.=$= C.mapM_ (liftIO . countMapRef ctmapref)
                     IM.toList <$> (liftIO $ readIORef ctmapref >>= traverse readIORef)
  where countMapRef ctmapref x = do ctmap <- readIORef ctmapref
                                    ctref <- case IM.lookup x ctmap of
                                      Just ref -> return ref
                                      Nothing -> do ref <- newIORef 0
                                                    writeIORef ctmapref $! IM.insert x ref ctmap
                                                    return ref
                                    modifyIORef' ctref succ

mvector :: (Monad m, MonadIO m, Functor m) => Counter Int m
mvector src = do mvecref <- liftIO $ UM.replicate len0 0 >>= newIORef
                 C.runConduit $ src C.=$= C.mapM_ (liftIO . countMVecRef mvecref)
                 (filter ((> 0) . snd) . zip [0..] . U.toList) <$> (liftIO $ readIORef mvecref >>= U.freeze)
  where len0 = 10
        countMVecRef mvecref x = do mvec0 <- readIORef mvecref
                                    when (UM.length mvec0 <= x) $ do
                                      let newsize = max (x + 1) (2 * UM.length mvec0)
                                      mvec' <- UM.replicate newsize 0
                                      UM.copy (UM.slice 0 (UM.length mvec0) mvec') mvec0
                                      writeIORef mvecref mvec'
                                    mvec <- readIORef mvecref
                                    ct0 <- UM.read mvec x
                                    UM.write mvec x $! succ ct0
