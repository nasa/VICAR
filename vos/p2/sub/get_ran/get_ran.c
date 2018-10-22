/************************************************************************
 * get_ran.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* Fortran-Callable Version (no C-version needed -- use memset)         */
/************************************************************************/

void FTN_NAME2_(get_ran, GET_RAN) (seed1,randout)
int *seed1;
float *randout;
{
  int i;
  srand((unsigned) *seed1);  
  i = ((unsigned int) rand());
  if (i >= 32767) *randout = (float) (i%32768)/32767.0;
  else *randout = (float) (i/32767.0);
  *seed1 = ((*seed1)*1103515245+12345);
  *seed1 = (unsigned int) (*seed1/65536)%32768;
  return;
}
