module Bio.SamTools.Enumerator 
       (
       -- | BAM/SAM enumeration
         enumInHandle
       , enumTam, enumTamWithIndex, enumBam
       -- | Indexed BAM region enumeration
       , enumQuery, enumIndexRegion, enumBamRegion
       -- | BAM/SAM iteration
       , iterHandle
       )
       where

import Control.Exception (bracket, bracket_, finally)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List
import Data.Maybe

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIndex
import qualified Data.Enumerator as E

-- | Enumerate over the contents of a BAM/SAM alignment input handle
enumInHandle :: (MonadIO m) => Bam.InHandle -> E.Enumerator Bam.Bam1 m a
enumInHandle inh = loop
  where loop (E.Continue k) = (liftIO $ Bam.get1 inh) >>= maybe eof next
          where eof = E.returnI (E.Continue k)
                next b = k (E.Chunks [b]) E.>>== loop
        loop step = E.returnI step

-- | Enumerate over the contents of a TAM (tab-delimited text) alignment file
enumTam :: FilePath -> E.Enumerator Bam.Bam1 IO a
enumTam inname step = E.Iteratee $ liftIO $ bracket (Bam.openTamInFile inname) Bam.closeInHandle $ \h ->
  E.runIteratee $ enumInHandle h step

-- | Enumerate over the contents of a TAM file with a separate target sequence index
enumTamWithIndex :: FilePath -> FilePath -> E.Enumerator Bam.Bam1 IO a
enumTamWithIndex inname idxname step
  = E.Iteratee $ liftIO $ bracket (Bam.openTamInFileWithIndex inname idxname) Bam.closeInHandle $ \h ->
  E.runIteratee $ enumInHandle h step

-- | Enumerate over the contents of a BAM (binary) alignment file
enumBam :: FilePath -> E.Enumerator Bam.Bam1 IO a
enumBam inname step = E.Iteratee $ liftIO $ bracket (Bam.openBamInFile inname) Bam.closeInHandle $ \h ->
  E.runIteratee $ enumInHandle h step

-- | Enumerate over the results of a query into a sorted, indexed BAM file
enumQuery :: BamIndex.Query -> E.Enumerator Bam.Bam1 IO a
enumQuery q = loop
  where loop (E.Continue k) = (liftIO $ BamIndex.next q) >>= maybe end next
            where end = E.returnI (E.Continue k)
                  next b = k (E.Chunks [b]) E.>>== loop
        loop step = E.returnI step
        
-- | Enumerate over the reads in a region from an indexed BAM file input handle
enumIndexRegion :: BamIndex.IdxHandle -> Int -> (Int64, Int64) -> E.Enumerator Bam.Bam1 IO a
enumIndexRegion h tid bnds step = E.Iteratee $ do 
  q <- liftIO $ BamIndex.query h tid bnds 
  E.runIteratee $ enumQuery q step

-- | Enumerate over the reads in a region from a sorted, indexed BAM file
enumBamRegion :: FilePath -> BS.ByteString -> (Int64, Int64) -> E.Enumerator Bam.Bam1 IO a
enumBamRegion inname seqname bnds step = E.Iteratee $ liftIO $ bracket (BamIndex.open inname) (BamIndex.close) $ \h -> do
  tid <- lookupTid $ BamIndex.idxHeader h
  E.runIteratee $ enumIndexRegion h tid bnds step
    where lookupTid = maybe noTid return . findIndex ((== seqname) . Bam.name) . Bam.targetSeqList
          noTid = ioError . userError $ "Target " ++ show seqname ++ " not found in " ++ show inname
          
iterHandle :: Bam.OutHandle -> E.Iteratee Bam.Bam1 IO ()
iterHandle h = E.continue step
  where step E.EOF = E.yield () E.EOF
        step (E.Chunks []) = E.continue step
        step (E.Chunks bs) = do liftIO $ forM_ bs $ Bam.put1 h
                                E.continue step