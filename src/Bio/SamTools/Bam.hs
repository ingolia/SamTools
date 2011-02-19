{-# LANGUAGE ForeignFunctionInterface #-}

module Bio.SamTools.Bam ( Header
                        , Bam1
                        , queryName
                        , InHandle
                        , openTamInFile, openTamInFileWithIndex
                        , closeInHandle
                        , get1
                        )
       where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Foreign
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import qualified Data.Vector as V

import Bio.SamTools.LowLevel

data HeaderSeq = HeaderSeq { name :: !BS.ByteString, len :: !Int } deriving (Eq, Show, Ord)

newtype Header = Header { unHeader :: V.Vector HeaderSeq } deriving (Eq, Show)

data Bam1 = Bam1 { ptrBam1 :: !(ForeignPtr Bam1Int)
                 , header :: !Header
                 }
            
queryName :: Bam1 -> BS.ByteString
queryName b = unsafePerformIO $ withForeignPtr (ptrBam1 b) (return . bam1QName)

data InHandle = InHandle { inFilename :: !FilePath
                         , samfile :: !(MVar (Ptr SamFileInt))
                         , fileHeader :: !Header
                         }
               
newInHandle :: FilePath -> Ptr SamFileInt -> IO InHandle
newInHandle filename fsam = do
  when (fsam == nullPtr) $ ioError . userError $ "Error opening BAM file " ++ show filename
  mv <- newMVar fsam
  addMVarFinalizer mv (finalizeSamFile mv)
  bhdr <- getSbamHeader fsam
  hdr <- convertHeader bhdr
  return $ InHandle { inFilename = filename, samfile = mv, fileHeader = hdr }  

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
            return . Just $ Bam1 { ptrBam1 = bptr, header = fileHeader inh }
