#include "sam.h"

#include "samtools.h"

int bam_fpaired(void) { return BAM_FPAIRED; }
int bam_fproper_pair(void) { return BAM_FPROPER_PAIR; }
int bam_funmap(void) { return BAM_FUNMAP; }
int bam_fmunmap(void) { return BAM_FMUNMAP; }
int bam_freverse(void) { return BAM_FREVERSE; }
int bam_fmreverse(void) { return BAM_FMREVERSE; }
int bam_fread1(void) { return BAM_FREAD1; }
int bam_fread2(void) { return BAM_FREAD2; }
int bam_fsecondary(void) { return BAM_FSECONDARY; }
int bam_fqcfail(void) { return BAM_FQCFAIL; }
int bam_fdup(void) { return BAM_FDUP; }

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
