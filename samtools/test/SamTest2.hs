module Main
       where 

import Control.Monad
import qualified Data.ByteString.Char8 as BS

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIndex
import qualified Bio.SamTools.Cigar as Cigar

main :: IO ()
main = do f <- Bam.openTamInFile "test/test.sam"
          let h = Bam.inHeader f
          b1 <- Bam.get1 f
          b2 <- Bam.get1 f
          maybe (return ()) (BS.putStrLn . Bam.queryName) b1
          Bam.closeInHandle f
          maybe (return ()) (BS.putStrLn . Bam.queryName) b1
          maybe (return ()) (BS.putStrLn . Bam.queryName) b2
          flip (maybe (return ())) b1 $ \b -> do
            BS.putStrLn . Bam.queryName $ b
            print ( Bam.targetID b, Bam.targetName b, Bam.targetLen b )
            print . Bam.position $ b
            print ( Bam.queryName b, Bam.queryLength b, Bam.querySeq b )
            print ( Bam.cigars b )
          o <- Bam.openTamOutFile "test/test2-out-1.sam" h
          maybe (return ()) (Bam.put1 o) b2
          maybe (return ()) (Bam.put1 o) b1
          Bam.closeOutHandle o
          convertSamToBam "test/test.sam" "test/test2-out-2.bam"
          extractActin "test/test.bam" "uc003sot.3" "test/test2-out-3.sam"

convertSamToBam :: FilePath -> FilePath -> IO ()
convertSamToBam inname outname = do
  i <- Bam.openTamInFile inname
  o <- Bam.openBamOutFile outname (Bam.inHeader i)
  loop (Bam.get1 i) (Bam.put1 o)
  Bam.closeOutHandle o
    where loop mi mo = go
            where go = mi >>= maybe (return ()) (\i -> mo i >> go)
                  
extractActin :: FilePath -> String -> FilePath -> IO ()
extractActin inname seqname outname = do
  i <- BamIndex.open inname 
  let (tid, start, end) = lookupSeq (BamIndex.idxHeader i) seqname
  o <- Bam.openTamOutFile outname (BamIndex.idxHeader i)
  q <- BamIndex.query i tid (start, end)
  loop q (Bam.put1 o)
  Bam.closeOutHandle o  
    where loop q m = BamIndex.next q >>= maybe (return ()) (\b -> m b >> loop q m)

lookupSeq :: Bam.Header -> String -> (Int, Int, Int)
lookupSeq h n = maybe noTarget handleTid . Bam.lookupTarget h . BS.pack $ n
  where noTarget = error $ "No target ID for sequence " ++ show n
        handleTid tid = (tid, 0, Bam.targetSeqLen h tid - 1)
