#ifndef _samtools_h
#define _samtools_h 1

#include "sam.h"

int bam_fpaired(void);
int bam_fproper_pair(void);
int bam_funmap(void);
int bam_fmunmap(void);
int bam_freverse(void);
int bam_fmreverse(void);
int bam_fread1(void);
int bam_fread2(void);
int bam_fsecondary(void);
int bam_fqcfail(void);
int bam_fdup(void);

uint32_t bam_cmatch(void);
uint32_t bam_cins(void);
uint32_t bam_cdel(void);
uint32_t bam_cref_skip(void);
uint32_t bam_csoft_clip(void);
uint32_t bam_chard_clip(void);
uint32_t bam_cpad(void);

int bam1_strand_(bam1_t *b);
int bam1_mstrand_(bam1_t *b);
uint32_t* bam1_cigar_(bam1_t *b);
char *bam1_qname_(bam1_t *b);
uint8_t *bam1_seq_(bam1_t *b);
uint8_t *bam1_qual_(bam1_t *b);
uint8_t bam1_seqi_(uint8_t *s, int i);

#endif /* _samtools_h */
