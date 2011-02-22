-- | This module provides an interface to sorted, indexed BAM
-- alignment files, which allow rapid extraction of alignments that lie
-- within one specific region of one sequence.
module Bio.SamTools.BamIndex
       ( 
         IdxHandle, idxFilename, idxHeader
       , open, close
       , Query, qyHandle
       , query, next
       )          
       where        

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Bio.SamTools.Bam
import Bio.SamTools.Internal
import Bio.SamTools.LowLevel       
       
-- | Handle for fetching alignments by region from a sorted, indexed
-- BAM file.
data IdxHandle = IdxHandle { idxFilename :: !FilePath -- ^ Filename of sorted, indexed BAM file
                           , bamindex :: !(MVar (Ptr BamFileInt, Ptr BamIndexInt))
                           , idxHeader :: !Header -- ^ Target sequences
                           }
                     
-- | Open a sorted, indexed BAM file.              
open :: FilePath -> IO IdxHandle
open filename = do
  f <- bamOpen filename "r"
  when (f == nullPtr) $ ioError . userError 
    $ "Error opening BAM file " ++ show filename
  i <- bamIndexLoad filename
  when (i == nullPtr) $ ioError . userError 
    $ "Error opening index for BAM file " ++ show filename
  mv <- newMVar (f, i)
  bhdr <- bamHeaderRead f
  bamInitHeaderHash bhdr
  addMVarFinalizer mv (finalizeBamIndex mv)
  hdr <- liftM Header . newForeignPtr bamHeaderDestroyPtr $ bhdr
  return $ IdxHandle { idxFilename = filename
                     , bamindex = mv
                     , idxHeader = hdr
                     }
    
close :: IdxHandle -> IO ()
close = finalizeBamIndex . bamindex

finalizeBamIndex :: MVar (Ptr BamFileInt, Ptr BamIndexInt) -> IO ()
finalizeBamIndex mv = modifyMVar mv $ \(f, i) -> do
  unless (f == nullPtr) $ bamClose f >> return ()
  unless (i == nullPtr) $ bamIndexDestroy i
  return ((nullPtr, nullPtr), ())

data Query = Query { qyHandle :: !IdxHandle
                   , iter :: !(MVar (Ptr BamIterInt))
                   }
              
query :: IdxHandle -> Int -> (Int, Int) -> IO Query
query inh tid (start, end) = withMVar (bamindex inh) $ \(_f, idx) -> do
  it <- bamIterQuery idx tid start end
  when (it == nullPtr) $ ioError . userError
    $ "Error starting BAM query: " ++ show (idxFilename inh, (tid, (start, end)))
  mv <- newMVar it
  addMVarFinalizer mv (finalizeBamIter mv)
  return $ Query { qyHandle = inh, iter = mv }
  
finalizeBamIter :: MVar (Ptr BamIterInt) -> IO ()
finalizeBamIter mv = modifyMVar mv $ \it -> do
  unless (it == nullPtr) $ bamIterDestroy it
  return (nullPtr, ())
  
next :: Query -> IO (Maybe Bam1)
next rgn = withMVar (bamindex . qyHandle $ rgn) $ \(f, _idx) -> 
  withMVar (iter rgn) $ \it -> do
    b <- bamInit1
    res <- bamIterRead f it b
    if res > 0
       then do bptr <- newForeignPtr bamDestroy1Ptr b
               return . Just $ Bam1 { ptrBam1 = bptr, header = idxHeader . qyHandle $ rgn }
       else do bamDestroy1 b
               if res < -1
                  then ioError . userError $
                       "Error reading BAM query from " ++ show (idxFilename . qyHandle $ rgn)
                  else return Nothing