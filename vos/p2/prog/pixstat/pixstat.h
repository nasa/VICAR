/* pixstat.h - include file for pixstat.c */
/*              and its modules         */
/* Apr 14, 2011 - Ray Bambery           */
/*     for 64-bit linux                 */
#ifndef _pixstat_h
#define  _pixstat_h 1

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>             //64-bit def of NULL


/* prototypes */
void compute_mean(float *ibuf,float *mean,float *vs);
void compute_sigma(int opcode,float *ibuf,float *mean,double *moment,
        float *vs,double *vs2);
void unit_filter(float *vs,float *mean,int ns,int nsw,double scale);
void unit_filter2(int opcode,float *vs,double *vs2,float *mean,double *moment,
    int ns,int nlw,int nsw);
void output(int ounit,float *mean,int nso,double mindn,double maxdn);
void output2(int ounit,float *mean,double *moment,
    int nso,double mindn,double maxdn,double scale);

#endif /* _pixstat_h */

