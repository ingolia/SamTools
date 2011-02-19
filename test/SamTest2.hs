module Main
       where 

import Control.Monad
import qualified Data.ByteString.Char8 as BS

import qualified Bio.SamTools.Bam as Bam

main :: IO ()
main = do f <- Bam.openTamInFile "test/test.sam"
          b1 <- Bam.get1 f
          b2 <- Bam.get1 f
          maybe (return ()) (BS.putStrLn . Bam.queryName) b1
          Bam.closeInHandle f
          maybe (return ()) (BS.putStrLn . Bam.queryName) b1
          maybe (return ()) (BS.putStrLn . Bam.queryName) b2
          