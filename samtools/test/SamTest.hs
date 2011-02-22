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

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.BamIndex as BamIndex
import qualified Bio.SamTools.Cigar as Cigar

actinName = "uc003sot.3"
actinLen = 1852

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
                     
                     actinsam <- extract bamin
                     let actinsam' = actinsam ++ "_samtools"                     
                     rawSystemE "samtools" [ "view", bamin, "-h", "-o", actinsam', actinName ++ ":0-" ++ show (actinLen - 1) ]
                     rawSystemP "diff" [ actinsam, actinsam' ]
                     
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
extract inname = let outname = dropExtension inname ++ "-actin.sam"
                 in bracket (BamIndex.open inname) (BamIndex.close) $ \idxin ->
                 let header = BamIndex.idxHeader idxin
                     tid = fromMaybe (error $ "No sequence " ++ show actinName) $ 
                           Bam.lookupTarget (BamIndex.idxHeader idxin) $ BS.pack actinName
                 in bracket (Bam.openTamOutFile outname header) Bam.closeOutHandle $ \hout -> do
                   unless (Bam.targetSeqName header tid == BS.pack actinName) $ error "Bad target name"
                   unless (Bam.targetSeqLen header tid == actinLen) $ error "Bad target length"
                   q <- BamIndex.query idxin tid (0, actinLen - 1)
                   loop (BamIndex.next q) (Bam.put1 hout)
                   return outname

parseBam :: Bam.Header -> Bam.Bam1 -> IO ()
parseBam hdr b = mapM_ verify [ (Bam.queryName b, bamfields !! 0)
                              , (Bam.targetName b, bamfields !! 2)
                              , (BS.pack . show . succ . Bam.position $ b, bamfields !! 3)                              
                              , (Bam.querySeq b, bamfields !! 9) 
                              , (BS.pack . show . queryLength b, BS.pack . show . targetLen hdr . targetID b)
                              ]
  where bamfields = BS.split '\t' . BS.pack . show $ b
        verify (s1, s2) | s1 == s2 = return ()
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
