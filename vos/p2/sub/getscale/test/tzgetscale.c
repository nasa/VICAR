#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzgetscale)(itype,labuf,maxdn,iscale,oscale,ind)
void *labuf;
int *itype, *maxdn, *ind;
float *iscale, *oscale;
{
/*  ============================================  */
      zgetscale(*itype,labuf,*maxdn,iscale,oscale,ind);
}
