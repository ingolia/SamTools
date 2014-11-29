{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main
       where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Maybe
import Numeric
import System.Environment
import System.FilePath
import System.IO

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.Conduit as Bam

main :: IO ()
main = getArgs >>= runWithArgs
  where runWithArgs [ inname ] = copy inname
        runWithArgs _ = do hPutStrLn stderr $ "Specify exactly one input BAM file"

copy :: FilePath -> IO ()
copy infile = do Bam.withBamInFile infile $ \hin ->
                   Bam.withBamOutFile (outfile "-1") (Bam.inHeader hin) $ \hout ->
                   runResourceT $ Bam.sourceHandle hin C.$$ Bam.sinkHandle hout
                 runResourceT $ Bam.sourceBamInFile infile C.$= process C.$$ Bam.sinkBamOutFile (outfile "-2")
                 runResourceT $ Bam.sourceBamInFile infile C.$= enumerate C.$$ Bam.sinkBamOutFile (outfile "-3")
  where outfile suffix = (takeBaseName infile) ++ suffix ++ (takeExtension infile)
                 
process :: (MonadIO m) => C.Conduit Bam.Bam1 m Bam.Bam1
process = C.mapM procOne
  where procOne b0 = let qyseq = fromMaybe "" . Bam.querySeq $ b0
                         nt = BS.foldl' (\n ch -> if ch == 'T' then n+1 else n) 0 qyseq
                         nn = BS.length qyseq
                         fstr = showFFloat (Just 3) ((fromIntegral nt / fromIntegral nn) :: Double) ""
                     in liftIO (Bam.addAuxZ b0 "XX" fstr)

enumerate :: (MonadIO m) => C.Conduit Bam.Bam1 m Bam.Bam1
enumerate = C.mapAccumM procOne 0 >> return ()
  where procOne b0 i = do b' <- liftIO $! Bam.addAuxi b0 "XY" i
                          return (i + 1, b')
                          