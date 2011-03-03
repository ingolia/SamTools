-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Bio.SamTools.LowLevel ( TamFilePtr
                             , samOpen, samClose
                             , BamFilePtr, BamFileInt
                             , bamOpen, bamClose
                             
                             , BamHeaderPtr, BamHeaderInt
                             , getNTargets, getTargetName, getTargetLen, bamGetTid
                             , bamHeaderInit, bamHeaderDestroy, bamHeaderDestroyPtr, bamInitHeaderHash
                             , setNTargets, setTargetName, setTargetLen
                             
                             , samHeaderRead, samHeaderRead2                             
                             , samRead1
                             
                             , bamHeaderRead, bamHeaderWrite
                             , bamRead1, bamWrite1
                             , BamCigar(..)                             
                             , cigarMatch, cigarIns, cigarDel, cigarRefSkip, cigarSoftClip, cigarHardClip, cigarPad
                             , cigarOp, cigarLength
                             , BamFlag(..)
                             , flagPaired, flagProperPair, flagUnmap, flagMUnmap, flagReverse, flagMReverse
                             , flagRead1, flagRead2, flagSecondary, flagQCFail, flagDup
                             , Bam1Ptr, Bam1Int
                             , getTID, getPos, getFlag, getNCigar, getLQSeq, getMTID, getMPos, getISize
                             , bam1Strand, bam1MStrand, bam1Cigar, bam1QName, bam1Seq, bam1Qual, bam1Seqi
                                                                                                                             
                             , bamAuxGet, bamAux2Z, bamAux2i --, bamAux2f, bamAux2d, bamAux2A
                                                                                                 
                             , bamInit1, bamDestroy1, bamDestroy1Ptr, bamDup1, bamFormat1
                             
                             , BamIndexInt, BamIndexPtr
                             , bamIndexLoad, bamIndexDestroy
                             , BamIterInt, BamIterPtr
                             , bamIterQuery, bamIterRead, bamIterDestroy
                             , BamFetchFPtr, mkBamFetchFPtr, bamFetch
                             , SamFilePtr, SamFileInt
                             , sbamOpen, sbamClose, getSbamHeader, sbamRead, sbamWrite                             
                             , FaIdxPtr, FaIdxInt
                             , faiLoad, faiDestroy, faiFetchSeq
                             )
where

import C2HS
import Control.Monad
import qualified Data.ByteString.Char8 as BS

#include "faidx.h"
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

setNTargets :: BamHeaderPtr -> CInt -> IO ()
setNTargets = {#set bam_header_t->n_targets#}

getTargetName :: BamHeaderPtr -> IO (Ptr CString)
getTargetName = {#get bam_header_t->target_name#}

setTargetName :: BamHeaderPtr -> Ptr CString -> IO ()
setTargetName = {#set bam_header_t->target_name#}

getTargetLen :: BamHeaderPtr -> IO (Ptr CUInt)
getTargetLen = {#get bam_header_t->target_len#}

setTargetLen :: BamHeaderPtr -> Ptr CUInt -> IO ()
setTargetLen = {#set bam_header_t->target_len#}

newtype BamFlag = BamFlag { unBamFlag :: CUInt }
                deriving (Eq, Show, Ord, Num, Bits)

flagPaired :: BamFlag
flagPaired = BamFlag {#call pure unsafe bam_fpaired#}

flagProperPair :: BamFlag
flagProperPair = BamFlag {#call pure unsafe bam_fproper_pair#}

flagUnmap :: BamFlag
flagUnmap = BamFlag {#call pure unsafe bam_funmap#}

flagMUnmap :: BamFlag
flagMUnmap = BamFlag {#call pure unsafe bam_fmunmap#}

flagReverse :: BamFlag
flagReverse = BamFlag {#call pure unsafe bam_freverse#}

flagMReverse :: BamFlag
flagMReverse = BamFlag {#call pure unsafe bam_fmreverse#}

flagRead1 :: BamFlag
flagRead1 = BamFlag {#call pure unsafe bam_fread1#}

flagRead2 :: BamFlag
flagRead2 = BamFlag {#call pure unsafe bam_fread2#}

flagSecondary :: BamFlag
flagSecondary = BamFlag {#call pure unsafe bam_fsecondary#}

flagQCFail :: BamFlag
flagQCFail = BamFlag {#call pure unsafe bam_fqcfail#}

flagDup :: BamFlag
flagDup = BamFlag {#call pure unsafe bam_fdup#}

newtype BamCigar = BamCigar { unBamCigar :: CUInt }
                   deriving (Eq, Show, Ord)
                            
cigarMatch :: BamCigar
cigarMatch = BamCigar {#call pure unsafe bam_cmatch#}

cigarIns :: BamCigar
cigarIns = BamCigar {#call pure unsafe bam_cins#}

cigarDel :: BamCigar
cigarDel = BamCigar {#call pure unsafe bam_cdel#}

cigarRefSkip :: BamCigar
cigarRefSkip = BamCigar {#call pure unsafe bam_cref_skip#}

cigarSoftClip :: BamCigar
cigarSoftClip = BamCigar {#call pure unsafe bam_csoft_clip#}

cigarHardClip :: BamCigar
cigarHardClip = BamCigar {#call pure unsafe bam_chard_clip#}

cigarPad :: BamCigar
cigarPad = BamCigar {#call pure unsafe bam_cpad#}

cigarOp :: CUInt -> BamCigar
cigarOp = BamCigar . {#call pure unsafe bam_cigar_op#}

cigarLength :: CUInt -> CUInt
cigarLength = {#call pure unsafe bam_cigar_length#}

data Bam1Int
{#pointer *bam1_t as Bam1Ptr -> Bam1Int#}

getTID :: Bam1Ptr -> IO CInt
getTID = {#get bam1_t->core.tid#}

getPos :: Bam1Ptr -> IO CInt
getPos = {#get bam1_t->core.pos#}

getFlag :: Bam1Ptr -> IO BamFlag
getFlag = liftM BamFlag . {#get bam1_t->core.flag#}

getNCigar :: Bam1Ptr -> IO Int
getNCigar = liftM fromIntegral . {#get bam1_t->core.n_cigar#}

getLQSeq :: Bam1Ptr -> IO CInt
getLQSeq = {#get bam1_t->core.l_qseq#}

getMTID :: Bam1Ptr -> IO CInt
getMTID = {#get bam1_t->core.mtid#}

getMPos :: Bam1Ptr -> IO CInt
getMPos =  {#get bam1_t->core.mpos#}

getISize :: Bam1Ptr -> IO CInt
getISize = {#get bam1_t->core.isize#}

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

foreign import ccall unsafe "bam.h &bam_header_destroy" bamHeaderDestroyPtr :: FunPtr (Ptr BamHeaderInt -> IO ())

{#fun unsafe bam_header_read as bamHeaderRead
  {id `BamFilePtr'} -> `BamHeaderPtr' id#}

{#fun unsafe bam_header_write as bamHeaderWrite
  { id `BamFilePtr'
  , id `BamHeaderPtr' } -> `CInt' id#}

{#fun unsafe bam_init_header_hash as bamInitHeaderHash
  { id `BamHeaderPtr' } -> `()' id#}

{#fun unsafe bam_read1 as bamRead1
  { id `BamFilePtr' 
  , id `Bam1Ptr' } -> `CInt' id#}

{#fun unsafe bam_write1 as bamWrite1
  { id `BamFilePtr' 
  , id `Bam1Ptr' } -> `CInt' id#}

{#fun unsafe bam_aux_get as bamAuxGet
  { id `Bam1Ptr'
  , id `CString' } -> `Ptr CUChar' id#}

{#fun unsafe bam_aux2Z as bamAux2Z
  { id `Ptr CUChar' } -> `CString' id#}

{#fun unsafe bam_aux2i as bamAux2i
  { id `Ptr CUChar' } -> `Int'#}

{#fun unsafe bam_init1_ as bamInit1
  { } -> `Bam1Ptr' id#}

{#fun unsafe bam_destroy1_ as bamDestroy1
  { id `Bam1Ptr' } -> `()'#}

foreign import ccall unsafe "samtools.h &bam_destroy1_" bamDestroy1Ptr :: FunPtr (Ptr Bam1Int -> IO ())

{#fun unsafe bam_dup1_ as bamDup1
 { id `Bam1Ptr' } -> `Bam1Ptr' id#}

{#fun unsafe bam_format1 as bamFormat1
 { id `BamHeaderPtr', id `Bam1Ptr' } -> `CString' id#}

-- BAM indexing

data BamIndexInt
{#pointer *bam_index_t as BamIndexPtr -> BamIndexInt#}

{#fun unsafe bam_index_load as bamIndexLoad
  { `String' } -> `BamIndexPtr' id#}

{#fun unsafe bam_index_destroy as bamIndexDestroy
  {id `BamIndexPtr' } -> `()'#}

{#pointer bam_fetch_f as BamFetchFPtr#}

foreign import ccall "wrapper"
  mkBamFetchFPtr :: (Bam1Ptr -> Ptr () -> IO CInt) -> IO (FunPtr (Bam1Ptr ->Ptr () -> IO CInt))

{#fun bam_fetch as bamFetch
  { id `BamFilePtr', id `BamIndexPtr'
  , `Int', `Int', `Int'
  , id `Ptr ()', id `BamFetchFPtr' } -> `Int'#}

data BamIterInt
{#pointer bam_iter_t as BamIterPtr -> BamIterInt#}

{#fun unsafe bam_iter_query as bamIterQuery
  {id `BamIndexPtr', `Int', `Int', `Int'} -> `BamIterPtr' id#}

{#fun unsafe bam_iter_read as bamIterRead
  {id `BamFilePtr', id `BamIterPtr', id `Bam1Ptr'} -> `CInt' id#}

{#fun unsafe bam_iter_destroy as bamIterDestroy
  {id `BamIterPtr'} -> `()' id#}

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

-- FASTA Indexing

data FaIdxInt
{#pointer *faidx_t as FaIdxPtr -> FaIdxInt#}

{#fun unsafe fai_load as faiLoad
 { `String' } -> `FaIdxPtr' id#}

{#fun unsafe fai_destroy as faiDestroy
 {id `FaIdxPtr' } -> `()'#}

{#fun unsafe faidx_fetch_seq as faiFetchSeq
 {id `FaIdxPtr'
 ,id `CString' , `Int', `Int'
 , id `Ptr CInt' } -> `CString' id#}

-- Helpers

packCString :: CString -> IO BS.ByteString
packCString = BS.packCString

useAsCString :: BS.ByteString -> (CString -> IO a) -> IO a
useAsCString = BS.useAsCString

