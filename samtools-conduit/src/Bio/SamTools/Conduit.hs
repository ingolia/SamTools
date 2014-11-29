{-# LANGUAGE RankNTypes #-}
module Bio.SamTools.Conduit
       where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIdx

import System.FilePath

sourceHandle :: (MonadIO m) => Bam.InHandle -> C.Producer m Bam.Bam1
sourceHandle inh = go
  where go = (liftIO $ Bam.get1 inh) >>= maybe (return ()) (\b -> C.yield b >> go)

sourceBamInFile :: (MonadResource m) => FilePath -> C.Producer m Bam.Bam1
sourceBamInFile infile = C.bracketP (Bam.openBamInFile infile) Bam.closeInHandle sourceHandle

sourceTamInFile :: (MonadResource m) => FilePath -> C.Producer m Bam.Bam1
sourceTamInFile infile = C.bracketP (Bam.openTamInFile infile) Bam.closeInHandle sourceHandle

sourceQuery :: (MonadIO m) => BamIdx.Query -> C.Producer m Bam.Bam1
sourceQuery qy = go
  where go = (liftIO $ BamIdx.next qy) >>= maybe (return ()) (\b -> C.yield b >> go)

sinkHandle :: (MonadIO m) => Bam.OutHandle -> C.Consumer Bam.Bam1 m ()
sinkHandle outh = C.mapM_ (liftIO . Bam.put1 outh)

sinkBamOutFileWithHeader :: (MonadResource m) => FilePath -> Bam.Header -> C.Consumer Bam.Bam1 m ()
sinkBamOutFileWithHeader outfile hdr = C.bracketP (Bam.openBamOutFile outfile hdr) Bam.closeOutHandle sinkHandle

sinkBamOutFile :: (MonadResource m) => FilePath -> C.Consumer Bam.Bam1 m ()
sinkBamOutFile outfile = C.await >>= \mb -> case mb of
                                              Nothing -> return ()
                                              Just b -> C.bracketP (openAndPut b) Bam.closeOutHandle sinkHandle
  where openAndPut b = do outh <- Bam.openBamOutFile outfile (Bam.header b)
                          liftIO . Bam.put1 outh $! b
                          return outh

sinkTamOutFileWithHeader :: (MonadResource m) => FilePath -> Bam.Header -> C.Consumer Bam.Bam1 m ()
sinkTamOutFileWithHeader outfile hdr = C.bracketP (Bam.openTamOutFile outfile hdr) Bam.closeOutHandle sinkHandle

sinkTamOutFile :: (MonadResource m) => FilePath -> C.Consumer Bam.Bam1 m ()
sinkTamOutFile outfile = C.await >>= \mb -> case mb of
                                              Nothing -> return ()
                                              Just b -> C.bracketP (openAndPut b) Bam.closeOutHandle sinkHandle
  where openAndPut b = do outh <- Bam.openTamOutFile outfile (Bam.header b)
                          liftIO . Bam.put1 outh $! b
                          return outh

