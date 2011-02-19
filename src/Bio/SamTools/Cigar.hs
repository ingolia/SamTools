module Bio.SamTools.Cigar ( CigarType(..)
                          , Cigar(..)
                          , toCigar
                          )
       where 

import Prelude hiding (length)

import Foreign.C

import qualified Data.Vector as V

import Bio.SamTools.LowLevel

data CigarType = Match | Ins | Del | RefSkip | SoftClip | HardClip | Pad
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

toCigar :: CUInt -> Cigar
toCigar cuint = Cigar { cigar = toCigarType . cigarOp $ cuint
                      , length = fromIntegral . cigarLength $ cuint
                      }