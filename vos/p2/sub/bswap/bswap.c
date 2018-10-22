#include "xvmaininc.h"
#include "ftnbridge.h"

void zbswap(unsigned char *in, int n);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/
void FTN_NAME2(bswap, BSWAP) ( in , n )
unsigned char *in;
int *n;
{
     zbswap(in,*n);
     return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zbswap(unsigned char *in, int n)
{
  register int i, indx;
  register unsigned char temp;

  for (i=0; i<n;i++) {
    indx = i * 2;
    temp = in[indx];
    in[indx] = in[indx+1];
    in[indx+1] = temp;
  }
}
    
