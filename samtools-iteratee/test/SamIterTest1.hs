{-# LANGUAGE OverloadedStrings #-}

module Main       
       where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Iteratee as Iter
import System.IO

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIndex
import Bio.SamTools.Iteratee

main :: IO ()
main = do enumTam "../../samtools/test/test-in-gene.sam" (Iter.mapM_ print) >>= Iter.run
          bracket (Bam.openBamInFile "../../samtools/test/test-in.bam") Bam.closeInHandle $ \hin ->
            bracket (Bam.openTamOutFile "test-out.sam" (Bam.inHeader hin)) Bam.closeOutHandle $ \hout ->
            (enumInHandle hin (Iter.mapM_ (Bam.put1 hout)) >>= Iter.run)
          withFile "test-out-region.sam" WriteMode $ \hout ->
            enumBamRegion "../../samtools/test/test-in.bam" "chr1" (14362 - 100, 16764 + 100) (Iter.mapM_ (hPutStrLn hout . show)) >>= Iter.run
            