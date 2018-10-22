#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzhsub)(dcode,nso,buf,hist,ilo,ihi) 
int *dcode, *nso, *ilo, *ihi;
int *hist;
void *buf;
{
/*  ============================================  */

      zhsub(*dcode,*nso,buf,hist,*ilo,*ihi );
}
