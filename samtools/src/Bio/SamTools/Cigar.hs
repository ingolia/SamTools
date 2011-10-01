-- | Handling of the extended CIGAR pair-wise alignment descriptors

module Bio.SamTools.Cigar ( CigarType(..)
                          , Cigar(..)
                          , toCigar
                          , cigarToSpLoc
                          , cigarToAlignment
                          )
       where 

import Prelude hiding (length)

import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (mapAccumL) -- hiding (length)
import Data.Maybe
import Foreign.C

import qualified Data.Vector as V

import Bio.SamTools.LowLevel

import Bio.SeqLoc.LocRepr
import qualified Bio.SeqLoc.Location as Loc
import qualified Bio.SeqLoc.Position as Pos
import qualified Bio.SeqLoc.SpliceLocation as SpLoc
import Bio.SeqLoc.Strand

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
                        
-- | Cigar entry including length
data Cigar = Cigar { cigar :: !CigarType, length :: !Int64 } deriving (Show, Ord, Eq)

toCigarType :: BamCigar -> CigarType
toCigarType = (cigarTypes V.!) . fromIntegral . unBamCigar

-- | Convert a @BAM@ binary cigar integer to a 'Cigar'
toCigar :: CUInt -> Cigar
toCigar cuint = Cigar { cigar = toCigarType . cigarOp $ cuint
                      , length = fromIntegral . cigarLength $ cuint
                      }
                
-- | 
cigarToSpLoc :: Int64 -> [Cigar] -> SpLoc.SpliceLoc
cigarToSpLoc pos5 = fromContigs . foldr mergeAdj [] . catMaybes . snd . mapAccumL entry pos5
  where fromContigs ctgs = fromMaybe badContigs $! SpLoc.fromContigs ctgs
          where badContigs = error . unwords $ 
                             [ "cigarToSpLoc: bad contigs " ]
                             ++ map (BS.unpack . repr) ctgs
        entry start (Cigar Match     len) = (start + len, contig start len)
        entry start (Cigar Ins      _len) = (start,       Nothing)
        entry start (Cigar Del       len) = (start + len, contig start len) -- INCLUDE aligned but deleted region
        entry start (Cigar RefSkip   len) = (start + len, Nothing)
        entry start (Cigar SoftClip _len) = (start,       Nothing)
        entry start (Cigar HardClip _len) = (start,       Nothing)
        entry start (Cigar Pad      _len) = (start,       Nothing)
        contig start len = Just $! Loc.fromBoundsStrand (fromIntegral start) (fromIntegral $ start + len - 1) Plus

mergeAdj :: Loc.ContigLoc -> [Loc.ContigLoc] -> [Loc.ContigLoc]
mergeAdj cprev [] = [cprev]
mergeAdj cprev cs@(ccurr:crest) 
  | adjacent  = Loc.fromPosLen (Loc.startPos cprev) (Loc.length cprev + Loc.length ccurr) : crest
  | otherwise = cprev : cs
    where adjacent = (snd . Loc.bounds $ cprev) + 1 == (fst . Loc.bounds $ ccurr)
          
cigarToAlignment :: Int64 -> [Cigar] -> [(Maybe Pos.Pos, Maybe Pos.Pos)]
cigarToAlignment pos5 cigars = concat . snd . mapAccumL cigarStep (0, pos5) $ cigars
  where cigarStep (read0, ref0) (Cigar Match     len) = ((read0 + len, ref0 + len)
                                                        , zip (poses read0 len) (poses ref0 len))
        cigarStep (read0, ref0) (Cigar Ins       len) = ((read0 + len, ref0)
                                                        , zip (poses read0 len) (repeat Nothing))
        cigarStep (read0, ref0) (Cigar Del       len) = ((read0, ref0 + len)
                                                        , zip (repeat Nothing) (poses ref0 len))
        cigarStep (read0, ref0) (Cigar RefSkip   len) = ((read0, ref0 + len), [])
        cigarStep (read0, ref0) (Cigar SoftClip  len) = ((read0 + len, ref0)
                                                        , zip (poses read0 len) (repeat Nothing))
        cigarStep (read0, ref0) (Cigar HardClip _len) = ((read0, ref0), [])
        cigarStep (read0, ref0) (Cigar Pad      _len) = ((read0, ref0), [])
        poses start len = [ Just $! Pos.Pos (fromIntegral $ start + i) Plus | i <- [0..(len-1)] ]