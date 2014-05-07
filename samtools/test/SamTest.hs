{-# LANGUAGE OverloadedStrings #-}
module Main
       where 

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Bio.SeqLoc.LocRepr

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIndex
import qualified Bio.SamTools.Cigar as Cigar

geneTargetName = "chr1"
geneTargetBounds = (14362 - 100, 16764 + 100)
geneRegion = BS.concat [ geneTargetName, ":"
                          , BS.pack . show . fst $ geneTargetBounds, "-"
                          , BS.pack . show . snd $ geneTargetBounds
                          ]

main :: IO ()
main = getArgs >>= mainWithArgs
  where mainWithArgs [ bamin ] = doSamTest bamin
        mainWithArgs _ = do prog <- getProgName
                            error . unwords $ [ "USAGE:", prog, "<IN.BAM>" ]

doSamTest :: FilePath -> IO ()
doSamTest bamin = do samout <- bamToSam bamin
                     let samout' = samout ++ "_samtools"
                     rawSystemE "samtools" [ "view", bamin, "-h", "-o", samout' ]
                     rawSystemP "diff" [ samout, samout' ]
                     samout2 <- samToBam samout >>= bamToSam
                     rawSystemP "diff" [ samout, samout2 ]
                     
                     genesam <- extract bamin
                     let genesam' = genesam ++ "_samtools"                     
                     rawSystemE "samtools" [ "view", bamin, "-h", "-o", genesam', BS.unpack geneRegion ]
                     rawSystemP "diff" [ genesam, genesam' ]
                     
                     header <- headerToIndex bamin
                     let header' = header ++ "_samtools"
                     rawSystemE "samtools" [ "view", bamin, "-H", "-o", header' ]
                     rawSystemP "diff" [ header, header' ]

                     bracket (Bam.openBamInFile bamin) Bam.closeInHandle $ \hin ->
                       Bam.get1 hin >>= maybe (return ()) (parseBam (Bam.inHeader hin))

bamToSam :: FilePath -> IO FilePath
bamToSam inname = let outname = dropExtension inname ++ "-test.sam"
                  in bracket (Bam.openBamInFile inname) Bam.closeInHandle $ \hin ->
                  bracket (Bam.openTamOutFile outname (Bam.inHeader hin)) Bam.closeOutHandle $ \hout -> do
                    loop (Bam.get1 hin) (Bam.put1 hout)
                    return outname
    
samToBam :: FilePath -> IO FilePath
samToBam inname = let outname = dropExtension inname ++ "-test.bam"
                  in bracket (Bam.openTamInFile inname) Bam.closeInHandle $ \hin ->
                  bracket (Bam.openBamOutFile outname (Bam.inHeader hin)) Bam.closeOutHandle $ \hout -> do
                    loop (Bam.get1 hin) (Bam.put1 hout)
                    return outname

headerToIndex :: FilePath -> IO FilePath
headerToIndex inname = let outname = dropExtension inname ++ "-index.txt"
                       in bracket (Bam.openBamInFile inname) Bam.closeInHandle $ \hin ->
                       withFile outname WriteMode $ \hout -> 
                       let hseqs = Bam.targetSeqList $ Bam.inHeader hin
                       in do forM_ hseqs $ \hseq -> hPutStrLn hout . concat $ 
                                                    [ "@SQ\tSN:", BS.unpack . Bam.name $ hseq, "\tLN:", show . Bam.len $ hseq ]
                             return outname
                         
extract :: FilePath -> IO FilePath
extract inname = let outname = dropExtension inname ++ "-gene.sam"
                     outname2 = dropExtension inname ++ "-gene-sploc.sam"
                 in bracket (BamIndex.open inname) (BamIndex.close) $ \idxin ->
                 let header = BamIndex.idxHeader idxin
                     tid = fromMaybe (error $ "No sequence " ++ show geneTargetName) $ 
                           Bam.lookupTarget (BamIndex.idxHeader idxin) $ geneTargetName
                 in bracket (Bam.openTamOutFile outname header) Bam.closeOutHandle $ \hout -> 
                 withFile outname2 WriteMode $ \hout2 -> do
                   unless (Bam.targetSeqName header tid == geneTargetName) $ error "Bad target name"
                   q <- BamIndex.query idxin tid geneTargetBounds
                   loop (BamIndex.next q) $ \b -> do
                     Bam.put1 hout b
                     hPutStrLn hout2 . concat $ 
                       [ maybe "n/a" (BS.unpack . repr) . Bam.refSeqLoc $ b
                       , "\t"
                       , show b
                       ]
                   return outname

parseBam :: Bam.Header -> Bam.Bam1 -> IO ()
parseBam hdr b = sequence_ [ verify (Bam.queryName b)  (bamfields !! 0)
                           , verify (Bam.targetName b) (Just $ bamfields !! 2)
                           , verify (liftM (BS.pack . show . succ) . Bam.position $ b) (Just $ bamfields !! 3)
                           , verify (Bam.querySeq b)   (Just $ bamfields !! 9) 
                           ]
  where bamfields = BS.split '\t' . BS.pack . show $ b
        verify s1 s2 | s1 == s2 = return ()
                     | otherwise = error $ "Mismatch: " ++ show (s1, s2)
                          
loop :: (IO (Maybe a)) -> (a -> IO ()) -> IO ()
loop mi mo = go
  where go = mi >>= maybe (return ()) (\i -> mo i >> go)

rawSystemE :: String -> [String] -> IO ()
rawSystemE prog args = rawSystem prog args >>= checkExit
  where checkExit ExitSuccess = return ()
        checkExit (ExitFailure err) = error $ show (prog : args) ++ " => " ++ show err

rawSystem_ :: String -> [String] -> IO ()
rawSystem_ prog args = rawSystem prog args >>= checkExit >> return ()
  where checkExit ExitSuccess = return ()
        checkExit (ExitFailure err) = hPutStrLn stderr $ show (prog : args) ++ " => " ++ show err

rawSystemP :: String -> [String] -> IO ()
rawSystemP prog args = rawSystem prog args >>= checkExit >> return ()
  where checkExit ExitSuccess = hPutStrLn stderr $ show (prog : args) ++ " => 0"
        checkExit (ExitFailure err) = hPutStrLn stderr $ show (prog : args) ++ " => " ++ show err
