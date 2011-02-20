#include "sam.h"

#include "samtools.h"

bamFile bam_open_(const char *fn, const char *mode) { return bam_open(fn, mode); }
int bam_close_(bamFile fp) { return bam_close(fp); }

uint32_t bam_fpaired(void) { return BAM_FPAIRED; }
uint32_t bam_fproper_pair(void) { return BAM_FPROPER_PAIR; }
uint32_t bam_funmap(void) { return BAM_FUNMAP; }
uint32_t bam_fmunmap(void) { return BAM_FMUNMAP; }
uint32_t bam_freverse(void) { return BAM_FREVERSE; }
uint32_t bam_fmreverse(void) { return BAM_FMREVERSE; }
uint32_t bam_fread1(void) { return BAM_FREAD1; }
uint32_t bam_fread2(void) { return BAM_FREAD2; }
uint32_t bam_fsecondary(void) { return BAM_FSECONDARY; }
uint32_t bam_fqcfail(void) { return BAM_FQCFAIL; }
uint32_t bam_fdup(void) { return BAM_FDUP; }

uint32_t bam_cigar_mask(void) { return BAM_CIGAR_MASK; }
uint32_t bam_cigar_shift(void) { return BAM_CIGAR_SHIFT; }
uint32_t bam_cigar_op(uint32_t cigar) { return (cigar & BAM_CIGAR_MASK); }
uint32_t bam_cigar_length(uint32_t cigar) { return (cigar >> BAM_CIGAR_SHIFT); }

uint32_t bam_cmatch(void) { return BAM_CMATCH; }
uint32_t bam_cins(void) { return BAM_CINS; }
uint32_t bam_cdel(void) { return BAM_CDEL; }
uint32_t bam_cref_skip(void) { return BAM_CREF_SKIP; }
uint32_t bam_csoft_clip(void) { return BAM_CSOFT_CLIP; }
uint32_t bam_chard_clip(void) { return BAM_CHARD_CLIP; }
uint32_t bam_cpad(void) { return BAM_CPAD; }

int bam1_strand_(bam1_t *b) { return bam1_strand(b); }
int bam1_mstrand_(bam1_t *b) { return bam1_mstrand(b); }
uint32_t* bam1_cigar_(bam1_t *b) { return bam1_cigar(b); }
char *bam1_qname_(bam1_t *b) { return bam1_qname(b); }
uint8_t *bam1_seq_(bam1_t *b) { return bam1_seq(b); }
uint8_t *bam1_qual_(bam1_t *b) { return bam1_qual(b); }
uint8_t bam1_seqi_(uint8_t *s, int i) { return bam1_seqi(s, i); }

bam1_t *bam_init1_(void) { return bam_init1(); }
void bam_destroy1_(bam1_t *b) { bam_destroy1(b); }
