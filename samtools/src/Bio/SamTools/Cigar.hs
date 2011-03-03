-- | Handling of the extended CIGAR pair-wise alignment descriptors

module Bio.SamTools.Cigar ( CigarType(..)
                          , Cigar(..)
                          , toCigar
                          )
       where 

import Prelude hiding (length)

import Foreign.C

import qualified Data.Vector as V

import Bio.SamTools.LowLevel

-- | Cigar entry types
data CigarType = Match    -- ^ Aligned nucleotide, may be a match or mismatch
               | Ins      -- ^ Insertion in read relative to reference
               | Del      -- ^ Deletion from read relative to reference
               | RefSkip  -- ^ Skipped reference bases, i.e., splice
               | SoftClip -- ^ Trimmed nucleotides, still present in read
               | HardClip -- ^ Trimmed nucleotides, removed from read
               | Pad      -- ^ Deletion from padded reference
               deriving (Show, Ord, Eq, Enum, Bounded)
                        
cigarTypes :: V.Vector CigarType                        
cigarTypes = emptyCigarTypes V.// typeAssocs
  where typeAssocs = [ (fromIntegral . unBamCigar $ cigarMatch,    Match)
                     , (fromIntegral . unBamCigar $ cigarIns,      Ins)
                     , (fromIntegral . unBamCigar $ cigarDel,      Del)
                     , (fromIntegral . unBamCigar $ cigarRefSkip,  RefSkip)
                     , (fromIntegral . unBamCigar $ cigarSoftClip, SoftClip)
                     , (fromIntegral . unBamCigar $ cigarHardClip, HardClip)
                     , (fromIntegral . unBamCigar $ cigarPad,      Pad)
                     ]
        maxtype = maximum . map fst $ typeAssocs
        emptyCigarTypes = V.generate (maxtype + 1) (\idx -> error $ "Unknown cigar op " ++ show idx)
                        
data Cigar = Cigar { cigar :: !CigarType, length :: !Int } deriving (Show, Ord, Eq)

toCigarType :: BamCigar -> CigarType
toCigarType = (cigarTypes V.!) . fromIntegral . unBamCigar

-- | Convert a @BAM@ binary cigar integer to a 'Cigar'
toCigar :: CUInt -> Cigar
toCigar cuint = Cigar { cigar = toCigarType . cigarOp $ cuint
                      , length = fromIntegral . cigarLength $ cuint
                      }