-- | This module provides an interface to sorted, indexed BAM
-- alignment files, which allow rapid extraction of alignments that lie
-- within one specific region of one sequence.
module Bio.SamTools.BamIndex
       ( 
         Handle, filename, header
       , open
       , fetch
       )          
       where        

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Foreign.C.Types
import Foreign.Ptr

import Bio.SamTools.Bam
import Bio.SamTools.LowLevel       
       
-- | Handle for fetching alignments by region from a sorted, indexed
-- BAM file.
data Handle = Handle { filename :: !FilePath
                     , bamindex :: !(MVar (Ptr BamFileInt, Ptr BamIndexInt))
                     , header :: !Header
                     }
                     
-- | Open a sorted, indexed BAM file.              
open :: FilePath -> IO Handle
open filename = do
  f <- bamOpen filename "r"
  when (f == nullPtr) $ ioError . userError 
    $ "Error opening BAM file " ++ show filename
  i <- bamIndexLoad filename
  when (i == nullPtr) $ ioError . userError 
    $ "Error opening index for BAM file " ++ show filename
  mv <- newMVar (f, i)
  bhdr <- bamHeaderRead f
  addMVarFinalizer mv (finalizeBamIndex mv)
  hdr <- convertHeader bhdr
  return $ Handle { filename = filename
                  , bamindex = mv
                  , header = hdr
                  }
    
finalizeBamIndex :: MVar (Ptr BamFileInt, Ptr BamIndexInt) -> IO ()
finalizeBamIndex mv = modifyMVar mv $ \(f, i) -> do
  unless (f == nullPtr) $ bamClose f >> return ()
  unless (i == nullPtr) $ bamIndexDestroy i
  return ((nullPtr, nullPtr), ())

-- | Map an 'IO' action across each alignment falling within a
-- specified region.
fetch :: Handle -> Int -- ^ Target ID of the sequence
         -> (Int, Int) -- ^ (Starting, ending) position on the target sequence, 0-based
         -> (Bam1 -> IO ()) -- ^ Action to perform
         -> IO Int
fetch inh tid (start, end) f = withMVar (bamindex inh) $ \(f, i) -> do
  bracket (mkBamFetchFPtr func) freeHaskellFunPtr $ \fptr ->
    bamFetch f i tid start end nullPtr fptr
      where func :: Bam1Ptr -> Ptr () -> IO CInt
            func b _ = new b (header inh) >>= f >> return 0
    
            
