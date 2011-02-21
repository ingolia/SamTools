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
       
data Handle = Handle { filename :: !FilePath
                     , bamindex :: !(MVar (Ptr BamFileInt, Ptr BamIndexInt))
                     , header :: !Header
                     }
                     
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

fetch :: Handle -> Int -> Int -> Int -> (Bam1 -> IO ()) -> IO Int
fetch inh tid start end f = withMVar (bamindex inh) $ \(f, i) -> do
  bracket (mkBamFetchFPtr func) freeHaskellFunPtr $ \fptr ->
    bamFetch f i tid start end nullPtr fptr
      where func :: Bam1Ptr -> Ptr () -> IO CInt
            func b _ = new b (header inh) >>= f >> return 0
    
            
