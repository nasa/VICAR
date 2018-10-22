#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

/************************************************************************
 * MVB (move_a_byte_array)
 *     Fortran-Callable Version (no C-version needed -- use memcpy) 
 ************************************************************************/
void FTN_NAME(mvb)(from_bbuf,to_bbuf,nb)
char *from_bbuf, *to_bbuf;
int *nb;
{
  memcpy(to_bbuf,from_bbuf,*nb);
  return;
}

/************************************************************************
 * ZBA (zero_a_byte_array)
 *     Fortran-Callable Version (no C-version needed -- use memset) 
 ************************************************************************/
void FTN_NAME(zba)(bbuf,nb)
char *bbuf;
int *nb;
{
  memset(bbuf,0,*nb);
  return;
}
