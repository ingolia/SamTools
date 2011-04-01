-- | Fetch sequences from an indexed fasta file
module Bio.SamTools.FaIdx ( InHandle, filename
                          , open, close, withFastaIndex
                          , fetch
                          , fetchLoc, readLoc
                          )
       where

import Control.Arrow
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Bio.SamTools.LowLevel

import qualified Bio.SeqLoc.Location as Loc
import Bio.SeqLoc.OnSeq
import Bio.SeqLoc.Strand

-- | Input handle for an indexed fasta file
data InHandle = InHandle { filename :: !FilePath -- ^ Name of the fasta file
                         , faidx :: !(MVar (Ptr FaIdxInt))
                         }
                
-- | Open an indexed fasta file
open :: FilePath -> IO InHandle
open name = do 
  f <- faiLoad name
  when (f == nullPtr) $ ioError . userError $ "Error opening indexed Fasta file " ++ show name
  mv <- newMVar f
  addMVarFinalizer mv (finalizeFaIdx mv)
  return $ InHandle { filename = name, faidx = mv }
  
close :: InHandle -> IO ()
close = finalizeFaIdx . faidx

finalizeFaIdx :: MVar (Ptr FaIdxInt) -> IO ()
finalizeFaIdx mv = modifyMVar mv $ \fai -> do
  unless (fai == nullPtr) $ faiDestroy fai
  return (nullPtr, ())

withFastaIndex :: FilePath -> (InHandle -> IO a) -> IO a
withFastaIndex fn = bracket (open fn) close

-- | Fetch a region specified by sequence name and coordinates, or the
-- empty string when the sequence is not found.
fetch :: InHandle -> BS.ByteString -- ^ Sequence name
         -> (Int64, Int64) -- ^ (Starting, ending) position, 0-based 
         -> IO BS.ByteString
fetch inh name (start, end) = withMVar (faidx inh) $ \fai ->
  BS.useAsCString name $ \cname ->
  alloca $ \lp -> 
  faiFetchSeq fai cname (fromIntegral start) (fromIntegral end) lp >>= \s ->
  if (s == nullPtr)
  then return BS.empty
  else do l <- liftM fromIntegral . peek $ lp
          sout <- BS.packCStringLen (s, l)
          free s
          return sout

fetchContig :: InHandle -> OnSeq Loc.ContigLoc -> IO (Maybe BS.ByteString)
fetchContig inh (OnSeq (SeqName name) cl) 
  = liftM result $ fetch inh name intbounds
  where intbounds = (fromIntegral *** fromIntegral) $ Loc.bounds cl
        result sequ | BS.null sequ = Nothing
                    | otherwise = return $! stranded (Loc.strand cl) sequ
    

fetchLoc :: (Loc.Location l) => InHandle -> OnSeq l -> IO (Maybe BS.ByteString)
fetchLoc inh (OnSeq name l)
  = liftM (liftM BS.concat . sequence) $ mapM (fetchContig inh) contigs
  where contigs = map (OnSeq name) . Loc.toContigs $ l
        
readLoc :: (Loc.Location l) => FilePath -> OnSeq l -> IO (Maybe BS.ByteString)
readLoc fn l = withFastaIndex fn $ \idx -> fetchLoc idx l