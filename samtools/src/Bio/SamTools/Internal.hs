module Bio.SamTools.Internal
       where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Foreign
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Bio.SamTools.LowLevel

-- | Information about one target sequence in a SAM alignment set
data HeaderSeq = HeaderSeq { -- | Target sequence name 
                             name :: !BS.ByteString
                             -- | Target sequence lengh
                           , len :: !Int 
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
    h <- peek (advancePtr names idx) >>= BS.packCString
    l <- peek (advancePtr lens idx)
    return $ HeaderSeq h (fromIntegral l)

-- | Returns a target sequence by ID, which is a 0-based index
targetSeq :: Header -> Int -> HeaderSeq
targetSeq h idx = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- liftM fromIntegral . getNTargets $ bhdr
  when (idx < 0 || idx >= ntarg) $ ioError . userError $
    "Target id " ++ show idx ++ " out of bounds " ++ show (0, ntarg-1)
  names <- getTargetName bhdr
  lens <- getTargetLen bhdr
  h <- peek (advancePtr names idx) >>= BS.packCString
  l <- peek (advancePtr lens idx)
  return $ HeaderSeq h (fromIntegral l)

-- | Returns a target sequence name by ID
targetSeqName :: Header -> Int -> BS.ByteString
targetSeqName h idx = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- liftM fromIntegral . getNTargets $ bhdr
  when (idx < 0 || idx >= ntarg) $ ioError . userError $
    "Target id " ++ show idx ++ " out of bounds " ++ show (0, ntarg-1)
  names <- getTargetName bhdr  
  peek (advancePtr names idx) >>= BS.packCString

targetSeqLen :: Header -> Int -> Int
targetSeqLen h idx = unsafePerformIO $ withForeignPtr (unHeader h) $ \bhdr -> do
  ntarg <- liftM fromIntegral . getNTargets $ bhdr
  when (idx < 0 || idx >= ntarg) $ ioError . userError $
    "Target id " ++ show idx ++ " out of bounds " ++ show (0, ntarg-1)
  lens <- getTargetLen bhdr
  liftM fromIntegral . peek $ advancePtr lens idx

-- withHeader :: Header -> (BamHeaderPtr -> IO a) -> IO a
-- withHeader (Header hdr) m = bracket bamHeaderInit bamHeaderDestroy $ \bhdr -> 
--   withMany BS.useAsCString (V.toList . V.map name $ hdr) $ \namelist ->
--   withArray namelist $ \names ->
--   withArray (V.toList . V.map (fromIntegral . len) $ hdr) $ \lens -> 
--   bracket_ (setNTargets bhdr . fromIntegral . V.length $ hdr) (setNTargets bhdr 0) $ 
--   bracket_ (setTargetName bhdr names) (setTargetName bhdr nullPtr) $
--   bracket_ (setTargetLen bhdr lens) (setTargetLen bhdr nullPtr) $
--   m bhdr

-- -- | Internal utility to copy and convert a raw 'BamHeaderInt' to a 'Header'
-- convertHeader :: BamHeaderPtr -> IO Header
-- convertHeader bhdr = do
--   ntarg <- getNTargets bhdr
--   names <- getTargetName bhdr
--   lens <- getTargetLen bhdr
--   hseqs <- forM [0..((fromIntegral ntarg)-1)] $ \idx -> do
--     h <- peek (advancePtr names idx) >>= BS.packCString
--     l <- peek (advancePtr lens idx)
--     return $ HeaderSeq h (fromIntegral l)
--   return . Header $! V.fromList hseqs

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