module Bio.SamTools.Iteratee
       (
         enumInHandle
       , enumTam, enumTamWithIndex, enumBam
       , enumQuery, enumIndexRegion, enumBamRegion
       )      
where
  
import Control.Exception (bracket, bracket_, finally)  
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIndex
import qualified Data.Iteratee as Iter
  
enumInHandle :: Bam.InHandle -> Iter.Enumerator [Bam.Bam1] IO a
enumInHandle inh i0 = step i0
  where step iter = Bam.get1 inh >>= maybe eof next
          where eof = return iter
                next b = Iter.runIter iter Iter.idoneM onCont
                  where onCont k Nothing = step . k $ Iter.Chunk [b]
                        onCont k e = return $ Iter.icont k e
                        
enumTam :: FilePath -> Iter.Enumerator [Bam.Bam1] IO a
enumTam inname i0 = bracket (Bam.openTamInFile inname) Bam.closeInHandle $ \h ->
  enumInHandle h i0
  
enumTamWithIndex :: FilePath -> FilePath -> Iter.Enumerator [Bam.Bam1] IO a
enumTamWithIndex inname idxname i0 
  = bracket (Bam.openTamInFileWithIndex inname idxname) Bam.closeInHandle $ \h ->
  enumInHandle h i0
  
enumBam :: FilePath -> Iter.Enumerator [Bam.Bam1] IO a
enumBam inname i0 = bracket (Bam.openBamInFile inname) Bam.closeInHandle $ \h ->
  enumInHandle h i0
  
enumQuery :: BamIndex.Query -> Iter.Enumerator [Bam.Bam1] IO a
enumQuery q i0 = step i0
  where step iter = BamIndex.next q >>= maybe eof next
          where eof = return iter
                next b = Iter.runIter iter Iter.idoneM onCont
                  where onCont k Nothing = step . k $ Iter.Chunk [b]
                        onCont k e       = return $ Iter.icont k e

enumIndexRegion :: BamIndex.IdxHandle -> Int -> (Int, Int) -> Iter.Enumerator [Bam.Bam1] IO a
enumIndexRegion h tid bnds i0 = do
  q <- liftIO $ BamIndex.query h tid bnds
  enumQuery q i0
  
enumBamRegion :: FilePath -> BS.ByteString -> (Int, Int) -> Iter.Enumerator [Bam.Bam1] IO a
enumBamRegion inname seqname bnds i0 = bracket (BamIndex.open inname) BamIndex.close $ \h -> do
  tid <- lookupTid . BamIndex.idxHeader $ h
  enumIndexRegion h tid bnds i0
    where lookupTid h = maybe noTid return $! Bam.lookupTarget h seqname
          noTid = ioError . userError $ "Target " ++ show seqname ++ " not found in " ++ show inname