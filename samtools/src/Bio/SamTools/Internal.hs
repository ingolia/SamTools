module Bio.SamTools.Internal
       where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Foreign
import Foreign.C.String

import Bio.SamTools.LowLevel

-- | Information about one target sequence in a SAM alignment set
data HeaderSeq = HeaderSeq { -- | Target sequence name 
                             name :: !BS.ByteString
                             -- | Target sequence lengh
                           , len :: !Int64
                           } deriving (Eq, Show, Ord)

-- | Target sequences from a SAM alignment set
newtype Header = Header { unHeader :: (ForeignPtr BamHeaderInt) }

-- The Header is a copy of the C data structure, pulled into Haskell memory management

newHeader :: BamHeaderPtr -> IO Header
newHeader bhp0 = do 
  ntarg <- liftM fromIntegral . getNTargets $ bhp0
  len' <- mallocArray ntarg
  getTargetLen bhp0 >>= \len0 -> copyArray len' len0 ntarg
  name' <- mallocArray ntarg
  getTargetName bhp0 >>= \name0 -> forM_ [0..(ntarg-1)] $ \idx ->
    peekElemOff name0 idx >>= peekCString >>= newCString >>= pokeElemOff name' idx
  bhp' <- bamHeaderInit
  setTargetName bhp' name'
  setTargetLen bhp' len'
  setNTargets bhp' $ fromIntegral ntarg
  bamInitHeaderHash bhp'
  hdr' <- newForeignPtr bamHeaderDestroyPtr bhp'
  return $ Header hdr'
  
-- | Number of target sequences
nTargets :: Header -> Int
nTargets h = fromIntegral . unsafePerformIO $ withForeignPtr (unHeader h) getNTargets

-- | Returns the list of target sequences
targetSeqList :: Header -> [HeaderSeq]
targetSeqList h = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- getNTargets bhdr
  names <- getTargetName bhdr
  lens <- getTargetLen bhdr
  forM [0..((fromIntegral ntarg)-1)] $ \idx -> do
    n <- peek (advancePtr names idx) >>= BS.packCString
    l <- peek (advancePtr lens idx)
    return $ HeaderSeq n (fromIntegral l)

-- | Returns a target sequence by ID, which is a 0-based index
targetSeq :: Header -> Int -> HeaderSeq
targetSeq h idx = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- liftM fromIntegral . getNTargets $ bhdr
  when (idx < 0 || idx >= ntarg) $ ioError . userError $
    "Target id " ++ show idx ++ " > " ++ show (ntarg-1)
  names <- getTargetName bhdr
  lens <- getTargetLen bhdr
  n <- peek (advancePtr names idx) >>= BS.packCString
  l <- peek (advancePtr lens idx)
  return $ HeaderSeq n (fromIntegral l)

-- | Returns a target sequence name by ID
targetSeqName :: Header -> Int -> BS.ByteString
targetSeqName h idx = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- liftM fromIntegral . getNTargets $ bhdr
  when (idx < 0 || idx >= ntarg) $ ioError . userError $
    "Target id " ++ show idx ++ " > " ++ show (ntarg - 1)
  names <- getTargetName bhdr  
  peek (advancePtr names idx) >>= BS.packCString

targetSeqLen :: Header -> Int -> Int64
targetSeqLen h idx = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- liftM fromIntegral . getNTargets $ bhdr
  when (idx < 0 || idx >= ntarg) $ ioError . userError $
    "Target id " ++ show idx ++ " > " ++ show (ntarg-1)
  lens <- getTargetLen bhdr
  liftM fromIntegral . peek $ advancePtr lens idx

lookupTarget :: Header -> BS.ByteString -> Maybe Int
lookupTarget h n = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr ->
  liftM handleResult . bamGetTid bhdr $ n
    where handleResult res | res < 0 = Nothing    
                           | otherwise = Just $! fromIntegral res

-- | SAM/BAM format alignment
data Bam1 = Bam1 { ptrBam1 :: !(ForeignPtr Bam1Int)
                 , header :: !Header
                 }

instance Show Bam1 where
  show b = unsafePerformIO $ 
           withForeignPtr (ptrBam1 b) $ \bp ->
           withForeignPtr (unHeader . header $ b) $ \hp -> do
             n <- bamFormat1 hp bp
             n' <- peekCString n
             free n
             return n'