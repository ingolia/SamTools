#ifndef _samtools_h
#define _samtools_h 1

#include "sam.h"

bamFile bam_open_(const char *, const char *);
int bam_close_(bamFile bf);

uint32_t bam_fpaired(void);
uint32_t bam_fproper_pair(void);
uint32_t bam_funmap(void);
uint32_t bam_fmunmap(void);
uint32_t bam_freverse(void);
uint32_t bam_fmreverse(void);
uint32_t bam_fread1(void);
uint32_t bam_fread2(void);
uint32_t bam_fsecondary(void);
uint32_t bam_fqcfail(void);
uint32_t bam_fdup(void);

uint32_t bam_cigar_mask(void);
uint32_t bam_cigar_shift(void);
uint32_t bam_cigar_op(uint32_t cigar);
uint32_t bam_cigar_length(uint32_t cigar);
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

bam1_t *bam_init1_(void);
void bam_destroy1_(bam1_t *b);
bam1_t *bam_dup1_(const bam1_t *b);

void bam_init_header_hash(bam_header_t *header);

/* Prototype for bam_aux.c function */
void bam_aux_append(bam1_t *b, const char tag[2], char type, int len, uint8_t *data);

#endif /* _samtools_h */
