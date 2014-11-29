module Main
       where

import Control.Monad.Trans.Resource
import qualified Data.Conduit as C

import System.Environment
import System.FilePath
import System.IO

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.Conduit as Bam

main :: IO ()
main = getArgs >>= runWithArgs
  where runWithArgs [ inname ] = copy inname
        runWithArgs _ = do hPutStrLn stderr $ "Specify exactly one input BAM file"

copy :: FilePath -> IO ()
copy infile = do Bam.withBamInFile infile $ \hin ->
                   Bam.withBamOutFile (outfile "-1") (Bam.inHeader hin) $ \hout ->
                   runResourceT $ Bam.sourceHandle hin C.$$ Bam.sinkHandle hout
                 runResourceT $ Bam.sourceBamInFile infile C.$$ Bam.sinkBamOutFile (outfile "-2")
  where outfile suffix = (takeBaseName infile) ++ suffix ++ (takeExtension infile)
                 
