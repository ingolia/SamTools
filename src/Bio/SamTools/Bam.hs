{-# LANGUAGE ForeignFunctionInterface #-}

module Bio.SamTools.Bam ( Header
                        , Bam1
                        , targetID, targetName, targetLen, position
                        , isPaired, isProperPair, isUnmap, isMateUnmap, isReverse, isMateReverse
                        , isRead1, isRead2, isSecondary, isQCFail, isDup
                        , queryName, queryLength, querySeq
                        , mateTargetID, mateTargetName, matePosition, insertSize
                        , InHandle, inHeader
                        , openTamInFile, openTamInFileWithIndex, openBamInFile
                        , closeInHandle
                        , get1
                        )
       where

import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import qualified Data.Vector as V

import Bio.SamTools.Cigar
import Bio.SamTools.LowLevel

data HeaderSeq = HeaderSeq { name :: !BS.ByteString, len :: !Int } deriving (Eq, Show, Ord)

newtype Header = Header { unHeader :: V.Vector HeaderSeq } deriving (Eq, Show)

data Bam1 = Bam1 { ptrBam1 :: !(ForeignPtr Bam1Int)
                 , header :: !Header
                 }
            
targetID :: Bam1 -> Int
targetID b = unsafePerformIO $ withForeignPtr (ptrBam1 b) getTID

targetName :: Bam1 -> BS.ByteString
targetName b = name $ (unHeader . header $ b) V.! (targetID b)

targetLen :: Bam1 -> Int
targetLen b = len $ (unHeader . header $ b) V.! (targetID b)

position :: Bam1 -> Int
position b = unsafePerformIO $ withForeignPtr (ptrBam1 b) getPos

isFlagSet :: BamFlag -> Bam1 -> Bool
isFlagSet f b = unsafePerformIO $ withForeignPtr (ptrBam1 b) $ liftM isfset . getFlag
  where isfset = (== f) . (.&. f)

isPaired :: Bam1 -> Bool
isPaired = isFlagSet flagPaired

isProperPair :: Bam1 -> Bool
isProperPair = isFlagSet flagProperPair

isUnmap :: Bam1 -> Bool
isUnmap = isFlagSet flagUnmap

isMateUnmap :: Bam1 -> Bool
isMateUnmap = isFlagSet flagMUnmap

isReverse :: Bam1 -> Bool
isReverse = isFlagSet flagReverse

isMateReverse :: Bam1 -> Bool
isMateReverse = isFlagSet flagMReverse

isRead1 :: Bam1 -> Bool
isRead1 = isFlagSet flagRead1

isRead2 :: Bam1 -> Bool
isRead2 = isFlagSet flagRead2

isSecondary :: Bam1 -> Bool
isSecondary = isFlagSet flagSecondary

isQCFail :: Bam1 -> Bool
isQCFail = isFlagSet flagQCFail

isDup :: Bam1 -> Bool
isDup = isFlagSet flagDup

cigars :: Bam1 -> [Cigar]
cigars b = unsafePerformIO $ withForeignPtr (ptrBam1 b) $ \p -> do
  nc <- getNCigar p
  liftM (map toCigar) $! peekArray nc . bam1Cigar $ p

queryName :: Bam1 -> BS.ByteString
queryName b = unsafePerformIO $ withForeignPtr (ptrBam1 b) (return . bam1QName)

queryLength :: Bam1 -> Int
queryLength b = unsafePerformIO $ withForeignPtr (ptrBam1 b) getLQSeq

querySeq :: Bam1 -> BS.ByteString
querySeq b = unsafePerformIO $ withForeignPtr (ptrBam1 b) $ \p -> do
  l <- getLQSeq p
  let seqarr = bam1Seq p
  return $! BS.pack [ seqiToChar . bam1Seqi seqarr $ i | i <- [0..((fromIntegral l)-1)] ]

seqiToChar :: CUChar -> Char
seqiToChar = (chars V.!) . fromIntegral
  where chars = emptyChars V.// [(1, 'A'), (2, 'C'), (4, 'G'), (8, 'T'), (15, 'N')]
        emptyChars = V.generate 16 (\idx -> error $ "Unknown char " ++ show idx)

mateTargetID :: Bam1 -> Int
mateTargetID b = unsafePerformIO $ withForeignPtr (ptrBam1 b) getMTID

mateTargetName :: Bam1 -> BS.ByteString
mateTargetName b = name $ (unHeader . header $ b) V.! (mateTargetID b)

mateTargetLen :: Bam1 -> Int
mateTargetLen b = len $ (unHeader . header $ b) V.! (mateTargetID b)

matePosition :: Bam1 -> Int
matePosition b = unsafePerformIO $ withForeignPtr (ptrBam1 b) getMPos

insertSize :: Bam1 -> Int
insertSize b = unsafePerformIO $ withForeignPtr (ptrBam1 b) getISize

data InHandle = InHandle { inFilename :: !FilePath
                         , samfile :: !(MVar (Ptr SamFileInt))
                         , inHeader :: !Header
                         }
               
newInHandle :: FilePath -> Ptr SamFileInt -> IO InHandle
newInHandle filename fsam = do
  when (fsam == nullPtr) $ ioError . userError $ "Error opening BAM file " ++ show filename
  mv <- newMVar fsam
  addMVarFinalizer mv (finalizeSamFile mv)
  bhdr <- getSbamHeader fsam
  hdr <- convertHeader bhdr
  return $ InHandle { inFilename = filename, samfile = mv, inHeader = hdr }  

openTamInFile :: FilePath -> IO InHandle
openTamInFile filename = sbamOpen filename "r" nullPtr >>= newInHandle filename
  
openTamInFileWithIndex :: FilePath -> FilePath -> IO InHandle
openTamInFileWithIndex filename indexname 
  = withCString indexname (sbamOpen filename "r" . castPtr) >>= newInHandle filename

openBamInFile :: FilePath -> IO InHandle
openBamInFile filename = sbamOpen filename "rb" nullPtr >>= newInHandle filename

finalizeSamFile :: MVar (Ptr SamFileInt) -> IO ()
finalizeSamFile mv = modifyMVar mv $ \fsam -> do
  unless (fsam == nullPtr) $ sbamClose fsam
  return (nullPtr, ())

closeInHandle :: InHandle -> IO ()
closeInHandle = finalizeSamFile . samfile

convertHeader :: BamHeaderPtr -> IO Header
convertHeader bhdr = do
  ntarg <- getNTargets bhdr
  names <- getTargetName bhdr
  lens <- getTargetLen bhdr
  hseqs <- forM [0..((fromIntegral ntarg)-1)] $ \idx -> do
    h <- peek (advancePtr names idx) >>= BS.packCString
    l <- peek (advancePtr lens idx)
    return $ HeaderSeq h (fromIntegral l)
  return . Header $! V.fromList hseqs
  
get1 :: InHandle -> IO (Maybe Bam1)
get1 inh = withMVar (samfile inh) $ \fsam -> do
  b <- bamInit1
  res <- sbamRead fsam b 
  if res < 0
     then do bamDestroy1 b
             if res < -1
                then ioError . userError $ "Error reading from BAM file " ++ show (inFilename inh)
                else return Nothing
    else do bptr <- newForeignPtr bamDestroy1Ptr b
            return . Just $ Bam1 { ptrBam1 = bptr, header = inHeader inh }
