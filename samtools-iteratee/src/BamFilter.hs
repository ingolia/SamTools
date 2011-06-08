module Main
       where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

import System.Console.GetOpt
import System.Environment
import System.IO

import qualified Data.Iteratee as Iter

import qualified Bio.SamTools.Bam as Bam
import qualified Bio.SamTools.Iteratee as BamIter

main :: IO ()
main = getArgs >>= handleOpt . getOpt RequireOrder optDescrs
    where handleOpt (_,    _,         errs@(_:_)) = usage (unlines errs)
          handleOpt (args, [bam], []) = either usage (doBamFilter bam) $ argsToConf args
          handleOpt (_,    _,     []) = usage "Specify exactly one BAM file"
          usage errs = do prog <- getProgName
                          hPutStr stderr $ usageInfo prog optDescrs
                          hPutStrLn stderr errs

doBamFilter :: FilePath -> Conf -> IO ()
doBamFilter bam conf = Bam.withBamInFile bam $ \hin ->
  Bam.withBamOutFile (confOutput conf) (Bam.inHeader hin) $ \hout ->
  let handleBam bam | confIsWanted conf bam = Bam.put1 hout bam
                    | otherwise = return ()
  in BamIter.enumInHandle hin (Iter.mapM_ handleBam) >>= Iter.run

confIsWanted :: Conf -> Bam.Bam1 -> Bool
confIsWanted conf | confPerfect conf = isPerfect
                  | otherwise        = const True
                                
isPerfect :: Bam.Bam1 -> Bool
isPerfect = maybe False (== 0) . Bam.nMismatch

data Conf = Conf { confOutput :: !FilePath
                 , confPerfect :: !Bool
                 } deriving (Show)

data Arg = ArgOutput { unArgOutput :: !String }
         | ArgPerfect
         deriving (Show, Read, Eq, Ord)

argOutput :: Arg -> Maybe String
argOutput (ArgOutput del) = Just del
argOutput _ = Nothing

optDescrs :: [OptDescr Arg]
optDescrs = [ Option ['o'] ["output"]        (ReqArg ArgOutput "OUTFILE") "Output filename"
            , Option []    ["perfect"]       (NoArg ArgPerfect)           "Pass only perfect alignments"
            ]

argsToConf :: [Arg] -> Either String Conf
argsToConf = runReaderT conf
    where conf = Conf <$> 
                 findOutput <*>
                 (ReaderT $ return . elem ArgPerfect)
          findOutput = ReaderT $ maybe (Left "No output filename") return . listToMaybe . mapMaybe argOutput
