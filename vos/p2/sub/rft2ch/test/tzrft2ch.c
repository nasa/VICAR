#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TRFT2CH.F */
/************************************************************************/

void FTN_NAME(tzrft2ch) (buffer,nlines,nsampl,tmode)
void  *buffer;   /* buffer containing the data to be transformed (i/o) */
int   *nlines;   /* number of lines in the matrix (M)        (input) */
int   *nsampl;   /* number of samples in the matrix (N)      (input) */
int   *tmode;     /* flag to do forward/reverse transform (input) */

{
       zrft2ch(buffer,*nlines,*nsampl,*tmode);
}

