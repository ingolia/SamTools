module Bio.SamTools.Internal
       where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import Foreign.ForeignPtr
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
newtype Header = Header { unHeader :: V.Vector HeaderSeq } 
                 deriving (Eq, Show)

-- | Internal utility to copy and convert a raw 'BamHeaderInt' to a 'Header'
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

-- | SAM/BAM format alignment
data Bam1 = Bam1 { ptrBam1 :: !(ForeignPtr Bam1Int)
                 , header :: !Header
                 }
