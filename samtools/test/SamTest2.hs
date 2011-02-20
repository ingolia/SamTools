module Main
       where 

import Control.Monad
import qualified Data.ByteString.Char8 as BS

import qualified Bio.SamTools.Bam as Bam
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
          o <- Bam.openTamOutFile "test/test2.sam" h
          maybe (return ()) (Bam.put1 o) b2
          maybe (return ()) (Bam.put1 o) b1
          Bam.closeOutHandle o