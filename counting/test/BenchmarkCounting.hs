module Main
       where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Criterion.Main

main = defaultMain [
  bgroup "one" [ bench "foldHM 1009"  $ nfIO $ C.runConduit $ benchmarkFoldHM 1009
               , bench "foldHM 101"   $ nfIO $ C.runConduit $ benchmarkFoldHM 101
               , bench "foldHM 10007" $ nfIO $ C.runConduit $ benchmarkFoldHM 10007                 
               ]
  ]

rawSource :: (Monad m) => C.Producer m Int
rawSource = C.enumFromTo 0 10000000

countModuloFoldHM :: (Monad m) => Int -> C.ConduitM Int a m (HM.HashMap Int Int)
countModuloFoldHM modulus = C.map (`mod` modulus) C.=$= C.fold countOne HM.empty
  where countOne hm0 x = HM.insertWith (+) x 1 hm0

benchmarkFoldHM :: Int -> C.ConduitM () a IO (HM.HashMap Int Int)
benchmarkFoldHM modulus = rawSource C.=$= countModuloFoldHM modulus



modulus :: Int
modulus = 1009
