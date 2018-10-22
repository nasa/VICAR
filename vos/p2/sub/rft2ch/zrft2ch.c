#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of RFT2CH                                         */
/************************************************************************/

void zrft2ch(buffer,nlines,nsampl,tmode)
void  *buffer;   /* buffer containing the data to be transformed (i/o) */
int   nlines;   /* number of lines in the matrix (M)        (input) */
int   nsampl;   /* number of samples in the matrix (N)      (input) */
int   tmode;     /* flag to do forward/reverse transform (input) */

{
FTN_NAME(rft2ch)(buffer,&nlines,&nsampl,&tmode);
}

