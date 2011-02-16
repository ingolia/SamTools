-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Bio.SamTools.LowLevel ( TamFilePtr
                             , samOpen, samClose
                             , BamFilePtr
                             , bamOpen, bamClose
                             , BamHeaderPtr
                             , getNTargets, getTargetName, getTargetLen, bamGetTid
                             , samHeaderRead, samHeaderRead2                             
                             , samRead1
                             , bamHeaderRead, bamHeaderWrite
                             , bamRead1, bamWrite1
                             , Bam1CorePtr
                             , Bam1Ptr
                             , bamInit1, bamDestroy1
                             , sbamOpen, sbamClose, getSbamHeader, sbamRead, sbamWrite
                             )
where

import C2HS
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr

#include "sam.h"
#include "samtools.h"

data TamFileInt
{#pointer tamFile as TamFilePtr -> TamFileInt#}

data BamFileInt
{#pointer bamFile as BamFilePtr -> BamFileInt#}

{#fun unsafe bam_open_ as bamOpen
  { `String'
  , `String' } -> `BamFilePtr' id#}

{#fun unsafe bam_close_ as bamClose
  {id `BamFilePtr'} -> `CInt' id#}

data BamHeaderInt
{#pointer *bam_header_t as BamHeaderPtr -> BamHeaderInt#}

getNTargets :: BamHeaderPtr -> IO CInt
getNTargets = {#get bam_header_t->n_targets#}

getTargetName :: BamHeaderPtr -> IO (Ptr CString)
getTargetName = {#get bam_header_t->target_name#}

getTargetLen :: BamHeaderPtr -> IO (Ptr CUInt)
getTargetLen = {#get bam_header_t->target_len#}

newtype BamFlag = BamFlag { unBamFlag :: CInt }
                deriving (Eq, Show, Ord)

bamFPaired :: BamFlag
bamFPaired = BamFlag {#call pure bam_fpaired#}

bamFProperPair :: BamFlag
bamFProperPair = BamFlag {#call pure bam_fproper_pair#}

bamFUnmap :: BamFlag
bamFUnmap = BamFlag {#call pure bam_funmap#}

bamFMUnmap :: BamFlag
bamFMUnmap = BamFlag {#call pure bam_fmunmap#}

bamFReverse :: BamFlag
bamFReverse = BamFlag {#call pure bam_freverse#}

bamFMReverse :: BamFlag
bamFMReverse = BamFlag {#call pure bam_fmreverse#}

bamFRead1 :: BamFlag
bamFRead1 = BamFlag {#call pure bam_fread1#}

bamFRead2 :: BamFlag
bamFRead2 = BamFlag {#call pure bam_fread2#}

bamFSecondary :: BamFlag
bamFSecondary = BamFlag {#call pure bam_fsecondary#}

bamFQCFail :: BamFlag
bamFQCFail = BamFlag {#call pure bam_fqcfail#}

bamFDup :: BamFlag
bamFDup = BamFlag {#call pure bam_fdup#}

newtype BamCigar = BamCigar { unBamCigar :: CUInt }
                   deriving (Eq, Show, Ord)
                            
bamCMatch :: BamCigar
bamCMatch = BamCigar {#call pure bam_cmatch#}

bamCIns :: BamCigar
bamCIns = BamCigar {#call pure bam_cins#}

bamCDel :: BamCigar
bamCDel = BamCigar {#call pure bam_cdel#}

bamCRefSkip :: BamCigar
bamCRefSkip = BamCigar {#call pure bam_cref_skip#}

bamCSoftClip :: BamCigar
bamCSoftClip = BamCigar {#call pure bam_csoft_clip#}

bamCHardClip :: BamCigar
bamCHardClip = BamCigar {#call pure bam_chard_clip#}

bamCPad :: BamCigar
bamCPad = BamCigar {#call pure bam_cpad#}

data Bam1CoreInt
{#pointer *bam1_core_t as Bam1CorePtr -> Bam1CoreInt#}

data Bam1Int
{#pointer *bam1_t as Bam1Ptr -> Bam1Int#}

{#fun pure unsafe bam1_strand_ as bam1Strand 
  {id `Bam1Ptr' } -> `Bool'#}

{#fun pure unsafe bam1_mstrand_ as bam1MStrand 
  {id `Bam1Ptr' } -> `Bool'#}

{#fun pure unsafe bam1_cigar_ as bam1Cigar
  {id `Bam1Ptr' } -> `Ptr CUInt' id#}

{#fun pure unsafe bam1_qname_ as bam1QName
  {id `Bam1Ptr' } -> `BS.ByteString' packCString*#}

{#fun pure unsafe bam1_seq_ as bam1Seq
  {id `Bam1Ptr' } -> `Ptr CUChar' id#}

{#fun pure unsafe bam1_qual_ as bam1Qual
  {id `Bam1Ptr' } -> `Ptr CUChar' id#}

{#fun pure unsafe bam1_seqi_ as bam1Seqi
  { id `Ptr CUChar' 
  , id `CInt' } -> `CUChar' id#}

-- Low-level SAM I/O

{#fun unsafe sam_open as samOpen
  {`String'} -> `TamFilePtr' id#}

{#fun unsafe sam_close as samClose
  {id `TamFilePtr'} -> `()'#}

{#fun unsafe sam_read1 as samRead1
  { id `TamFilePtr'
  , id `BamHeaderPtr'
  , id `Bam1Ptr' } -> `Int' #}

{#fun unsafe sam_header_read2 as samHeaderRead2
  {`String'} -> `BamHeaderPtr' id#}

{#fun unsafe sam_header_read as samHeaderRead
  {id `TamFilePtr'} -> `BamHeaderPtr' id#}

{#fun unsafe bam_get_tid as bamGetTid
  { id `BamHeaderPtr'
  , useAsCString* `BS.ByteString'} -> `Int'#}

-- Low-level BAM I/O
{#fun unsafe bam_header_init as bamHeaderInit
  { } -> `BamHeaderPtr' id#}

{#fun unsafe bam_header_destroy as bamHeaderDestroy
  {id `BamHeaderPtr' } -> `()'#}

{#fun unsafe bam_header_read as bamHeaderRead
  {id `BamFilePtr'} -> `BamHeaderPtr' id#}

{#fun unsafe bam_header_write as bamHeaderWrite
  { id `BamFilePtr'
  , id `BamHeaderPtr' } -> `CInt' id#}

{#fun unsafe bam_read1 as bamRead1
  { id `BamFilePtr' 
  , id `Bam1Ptr' } -> `CInt' id#}

{#fun unsafe bam_write1 as bamWrite1
  { id `BamFilePtr' 
  , id `Bam1Ptr' } -> `CInt' id#}

{#fun unsafe bam_init1_ as bamInit1
  { } -> `Bam1Ptr' id#}

{#fun unsafe bam_destroy1_ as bamDestroy1
  { id `Bam1Ptr' } -> `()'#}

-- Unified SAM/BAM I/O

data SamFileInt
{#pointer *samfile_t as SamFilePtr -> SamFileInt#}

getSbamHeader :: SamFilePtr -> IO BamHeaderPtr
getSbamHeader = {#get samfile_t->header#}

{#fun unsafe samopen as sbamOpen
  { `String'
  , `String'
  , id `Ptr ()' } -> `SamFilePtr' id#}
    
{#fun unsafe samclose as sbamClose
  { id `SamFilePtr' } -> `()'#}

{#fun unsafe samread as sbamRead
  { id `SamFilePtr'
  , id `Bam1Ptr' } -> `CInt' id#}

{#fun unsafe samwrite as sbamWrite
  { id `SamFilePtr'
  , id `Bam1Ptr' } -> `CInt' id#}

-- Helpers

packCString = BS.packCString
useAsCString = BS.useAsCString